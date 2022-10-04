package org.renci.cam

import org.geneontology.jena.OWLtoRules
import org.geneontology.rules.engine
import org.geneontology.rules.engine.{RuleEngine, URI}
import org.geneontology.rules.util.Bridge
import org.phenoscape.sparql.SPARQLInterpolation._
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.QueryService._
import org.renci.cam.Util.IterableSPARQLOps
import org.renci.cam.domain.{BiolinkPredicate, IRI}
import org.semanticweb.owlapi
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import zio._
import zio.config.{getConfig, ZConfig}

import java.io.File

object ExplanationService {

  type Rules = Has[RuleEngine]

  final private case class CategorizedNode(node: IRI, category: IRI)

  final private case class CategorizedQuad(s: CategorizedNode, p: CategorizedNode, o: CategorizedNode, g: IRI)

  final private case class Quad(s: IRI, p: IRI, o: IRI, g: IRI)

  // Within each triple, the predicate is expected to be a Biolink slot.
  // The subject and object should be term IRIs (not Biolink classes).
  def explainEdges(edges: Set[Triple]): RIO[ZConfig[AppConfig] with HttpClient with Rules, Nothing] =
    for {
      ruleEngine <- ZIO.service[RuleEngine]
      quads <- mapTriplesToRDFNodesAndGraphs(edges)
      graphs = quads.map(_.g)
      graphsWithTriples <- retrieveGraphTriples(graphs)
      graphsToRuleEngines <- ZIO
        .foreachPar(graphsWithTriples.toSeq) { case (g, triples) =>
          ZIO.succeed(g -> ruleEngine.processTriples(triples.map(toArachne)))
        }
        .map(_.toMap)
      quadsAndExplanations = quads.flatMap { case cq @ CategorizedQuad(s, p, o, g) =>
        graphsToRuleEngines.get(g).to(Set).map { ruleEngine =>
          val explanations = ruleEngine.explain(engine.Triple(engine.URI(s.node.value), engine.URI(p.node.value), engine.URI(o.node.value)))
          cq -> explanations.map { e =>
            e.facts.collect { case engine.Triple(URI(sID), URI(pID), URI(oID)) =>
              Triple(IRI(sID), IRI(pID), IRI(oID))
            }
          }
        }
      }
      explanationTriples = quadsAndExplanations.flatMap(_._2.flatten)
      explanationIndividuals = explanationTriples.flatMap(t => Set(t.subj, t.obj))
      individualsToTypesToLabelAndCategories <- getNodeMetadata(explanationIndividuals)
      explanationProperties = explanationTriples.map(_.pred)
      propertiesToPredicates <- getPropertyMetadata(explanationProperties)
    } yield ???

  private def mapTriplesToRDFNodesAndGraphs(edges: Set[Triple]): RIO[ZConfig[AppConfig] with HttpClient, Set[CategorizedQuad]] = {
    val spoValues = edges
      .map { case Triple(s, p, o) =>
        sparql" ($s $p $o) "
      }
      .reduceOption(_ + _)
      .getOrElse(sparql"")
    val queryString =
      sparql"""SELECT DISTINCT ?s_node ?s_category ?p_node ?p_category ?o_node ?o_category ?g
               WHERE {
                 VALUES (?s_category ?p_category ?o_category) { $spoValues }
                 ?s_node $RDFType ?s_category .
                 ?p_node $SlotMapping ?p_category .
                 ?o_node $RDFType ?o_category .
                 GRAPH ?g1 { ?s_node ?p_node ?o_node }
                 OPTIONAL { ?g1 $ProvWasDerivedFrom ?g2 }
                 BIND(COALESCE(?g2, ?g1) AS ?g)
               }
           """
    for {
      query <- ZIO.effect(queryString.toQuery)
      results <- SPARQLQueryExecutor.runSelectQueryAs[CategorizedQuad](query)
    } yield results.toSet
  }

  def retrieveGraphTriples(graphs: Set[IRI]): RIO[ZConfig[AppConfig] with HttpClient, Map[IRI, Set[Triple]]] = {
    val queryString =
      sparql"""SELECT DISTINCT ?s ?p ?o ?g
               WHERE {
                 VALUES ?g { ${graphs.asValues} }
                 GRAPH ?g { ?s ?p ?o }
               }
           """
    for {
      query <- ZIO.effect(queryString.toQuery)
      results <- SPARQLQueryExecutor.runSelectQueryAs[Quad](query)
    } yield results.to(Set).groupMap(_.g)(quad => Triple(quad.s, quad.p, quad.o))
  }

  def getNodeMetadata(nodes: Set[IRI]): RIO[ZConfig[AppConfig] with HttpClient, Map[IRI, Map[IRI, (String, Set[IRI])]]] = {
    final case class Result(node: IRI, `type`: IRI, label: String, category: IRI)
    val queryString =
      sparql"""SELECT DISTINCT ?node ?type ?label ?category
               WHERE {
                 VALUES ?node { ${nodes.asValues} }
                 ?node $SesameDirectType ?type .
                 ?type $RDFSLabel ?label .
                 OPTIONAL {
                   ?type $RDFSSubClassOf ?biolink .                 
                   ?biolink a $BiolinkMLClassDefinition .
                 }
                 BIND(COALESCE(?biolink, ${BiolinkNamedThing.iri}) AS ?category)
               }
           """
    for {
      query <- ZIO.effect(queryString.toQuery)
      results <- SPARQLQueryExecutor.runSelectQueryAs[Result](query)
    } yield results
      .to(Set)
      .groupMap(_.node)(r => (r.`type`, r.label, r.category))
      .view
      .mapValues(vs => vs.groupMapReduce(_._1)(v => (v._2, Set(v._3)))((x, y) => (x._1, x._2 ++ y._2)))
      .toMap
  }

  def getPropertyMetadata(properties: Set[IRI]): RIO[ZConfig[AppConfig] with HttpClient, Map[IRI, IRI]] = {
    val RelatedTo = BiolinkPredicate("related_to").iri
    final case class Result(property: IRI, predicate: IRI)
    val queryString =
      sparql"""SELECT DISTINCT ?property ?predicate
               WHERE {
                 VALUES ?property { ${properties.asValues} }
                 OPTIONAL {
                   ?property $SlotMapping ?slot .
                   ?slot a $BiolinkMLSlotDefinition .
                 }
                 BIND(COALESCE(?slot, $RelatedTo) AS ?predicate)
                 FILTER NOT EXISTS {
                   ?property $SlotMapping ?other .
                   ?other $BiolinkMLIsA+/$BiolinkMLMixins* ?predicate .
                 }
               }
           """
    for {
      query <- ZIO.effect(queryString.toQuery)
      results <- SPARQLQueryExecutor.runSelectQueryAs[Result](query)
    } yield results.map(r => r.property -> r.predicate).toMap
  }

  private def toArachne(triple: Triple) =
    engine.Triple(engine.URI(triple.subj.value), engine.URI(triple.pred.value), engine.URI(triple.obj.value))

  def createArachneReasoner(ontologyPath: String): Task[RuleEngine] =
    for {
      ontology <- ZIO.effect(OWLManager.createOWLOntologyManager().loadOntology(owlapi.model.IRI.create(new File(ontologyPath))))
      jenaRules = OWLtoRules.translate(ontology, Imports.INCLUDED, true, true, true, true)
      rules = Bridge.rulesFromJena(jenaRules)
    } yield new RuleEngine(rules, true)

  val rulesLayer: RLayer[ZConfig[AppConfig], Rules] =
    getConfig[AppConfig].flatMap(conf => createArachneReasoner(conf.ontologyPath)).toLayer

}
