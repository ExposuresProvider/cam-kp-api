package org.renci.cam

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.jena.ext.com.google.common.base.CaseFormat
import org.apache.jena.query.{QueryFactory, ResultSet}
import org.renci.cam.domain._
import zio.config.ZConfig
import zio.{RIO, Task, ZIO, config => _}

import scala.collection.JavaConverters._
import scala.collection.mutable

object QueryService extends LazyLogging {

  val PREFIXES: Map[String, String] = Map(
    "BFO" -> "http://purl.obolibrary.org/obo/BFO_",
    "BIOGRID" -> "http://thebiogrid.org/",
    "BioSample" -> "http://example.org/UNKNOWN/BioSample/",
    "CAID" -> "http://example.org/UNKNOWN/CAID/",
    "CHEBI" -> "http://purl.obolibrary.org/obo/CHEBI_",
    "CHEMBL.COMPOUND" -> "http://identifiers.org/chembl.compound/",
    "CHEMBL.TARGET" -> "http://identifiers.org/chembl.target/",
    "CIO" -> "http://purl.obolibrary.org/obo/CIO_",
    "CIViC" -> "http://example.org/UNKNOWN/CIViC/",
    "CL" -> "http://purl.obolibrary.org/obo/CL_",
    "CLO" -> "http://purl.obolibrary.org/obo/CLO_",
    "ClinVar" -> "http://www.ncbi.nlm.nih.gov/clinvar/",
    "DBSNP" -> "http://identifiers.org/dbsnp/",
    "DOID" -> "http://purl.obolibrary.org/obo/DOID_",
    "DRUGBANK" -> "http://identifiers.org/drugbank/",
    "ECO" -> "http://purl.obolibrary.org/obo/ECO_",
    "ECTO" -> "http://example.org/UNKNOWN/ECTO/",
    "EFO" -> "http://purl.obolibrary.org/obo/EFO_",
    "ENSEMBL" -> "http://ensembl.org/id/",
    "ExO" -> "http://example.org/UNKNOWN/ExO/",
    "FAO" -> "http://purl.obolibrary.org/obo/FAO_",
    "GENO" -> "http://purl.obolibrary.org/obo/GENO_",
    "GO" -> "http://purl.obolibrary.org/obo/GO_",
    "GOLD.META" -> "http://identifiers.org/gold.meta/",
    "GTOPDB" -> "http://example.org/UNKNOWN/GTOPDB/",
    "HANCESTRO" -> "http://example.org/UNKNOWN/HANCESTRO/",
    "HGNC" -> "http://www.genenames.org/cgi-bin/gene_symbol_report?hgnc_id=",
    "HGVS" -> "http://example.org/UNKNOWN/HGVS/",
    "HMDB" -> "http://www.hmdb.ca/metabolites/",
    "HP" -> "http://purl.obolibrary.org/obo/HP_",
    "IAO" -> "http://purl.obolibrary.org/obo/IAO_",
    "INCHI" -> "http://identifiers.org/inchi/",
    "INCHIKEY" -> "http://identifiers.org/inchikey/",
    "IUPHAR" -> "http://example.org/UNKNOWN/IUPHAR/",
    "IntAct" -> "http://example.org/UNKNOWN/IntAct/",
    "KEGG" -> "http://identifiers.org/kegg/",
    "MEDDRA" -> "http://purl.bioontology.org/ontology/MEDDRA/",
    "MGI" -> "http://www.informatics.jax.org/accession/MGI:",
    "MIR" -> "http://identifiers.org/mir/",
    "MONDO" -> "http://purl.obolibrary.org/obo/MONDO_",
    "MYVARIANT_HG19" -> "http://example.org/UNKNOWN/MYVARIANT_HG19/",
    "MYVARIANT_HG38" -> "http://example.org/UNKNOWN/MYVARIANT_HG38/",
    "NCBIGene" -> "http://www.ncbi.nlm.nih.gov/gene/",
    "NCIT" -> "http://purl.obolibrary.org/obo/NCIT_",
    "OBAN" -> "http://purl.org/oban/",
    "OBI" -> "http://purl.obolibrary.org/obo/OBI_",
    "OGMS" -> "http://purl.obolibrary.org/obo/OGMS_",
    "OIO" -> "http://www.geneontology.org/formats/oboInOwl#",
    "OMIM" -> "http://purl.obolibrary.org/obo/OMIM_",
    "ORPHANET" -> "http://identifiers.org/orphanet/",
    "PANTHER" -> "http://www.pantherdb.org/panther/family.do?clsAccession=",
    "PMID" -> "http://www.ncbi.nlm.nih.gov/pubmed/",
    "PO" -> "http://purl.obolibrary.org/obo/PO_",
    "PR" -> "http://purl.obolibrary.org/obo/PR_",
    "PW" -> "http://purl.obolibrary.org/obo/PW_",
    "PomBase" -> "https://www.pombase.org/spombe/result/",
    "RHEA" -> "http://identifiers.org/rhea/",
    "RO" -> "http://purl.obolibrary.org/obo/RO_",
    "SGD" -> "https://www.yeastgenome.org/locus/",
    "SIO" -> "http://semanticscience.org/resource/SIO_",
    "SMPDB" -> "http://smpdb.ca/view/",
    "SO" -> "http://purl.obolibrary.org/obo/SO_",
    "UBERON" -> "http://purl.obolibrary.org/obo/UBERON_",
    "UMLS" -> "http://linkedlifedata.com/resource/umls/id/",
    "UMLSSC" -> "https://uts-ws.nlm.nih.gov/rest/semantic-network/semantic-network/current/TUI/",
    "UMLSSG" -> "https://uts-ws.nlm.nih.gov/rest/semantic-network/semantic-network/current/GROUP/",
    "UMLSST" -> "https://uts-ws.nlm.nih.gov/rest/semantic-network/semantic-network/current/STY/",
    "UNII" -> "http://fdasis.nlm.nih.gov/srs/unii/",
    "UPHENO" -> "http://purl.obolibrary.org/obo/UPHENO_",
    "UniProtKB" -> "http://identifiers.org/uniprot/",
    "VMC" -> "http://example.org/UNKNOWN/VMC/",
    "WB" -> "http://identifiers.org/wb/",
    "WD" -> "http://example.org/UNKNOWN/WD/",
    "WIKIPATHWAYS" -> "http://identifiers.org/wikipathways/",
    "ZFIN" -> "http://zfin.org/",
    "biolinkml" -> "https://w3id.org/biolink/biolinkml/",
    "dct" -> "http://example.org/UNKNOWN/dct/",
    "dcterms" -> "http://purl.org/dc/terms/",
    "dictyBase" -> "http://dictybase.org/gene/",
    "faldo" -> "http://biohackathon.org/resource/faldo#",
    "metatype" -> "https://w3id.org/biolink/biolinkml/type/",
    "owl" -> "http://www.w3.org/2002/07/owl#",
    "pav" -> "http://purl.org/pav/",
    "qud" -> "http://qudt.org/1.1/schema/qudt#",
    "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
    "shex" -> "http://www.w3.org/ns/shex#",
    "skos" -> "https://www.w3.org/TR/skos-reference/#",
    "void" -> "http://rdfs.org/ns/void#",
    "wgs" -> "http://www.w3.org/2003/01/geo/wgs84_pos",
    "xsd" -> "http://www.w3.org/2001/XMLSchema#",
    "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
    "go" -> "http://www.geneontology.org/formats/oboInOwl#",
    "blml" -> "https://w3id.org/biolink/biolinkml/meta/",
    "bl" -> "https://w3id.org/biolink/vocab/",
    "MONDO" -> "http://purl.obolibrary.org/obo/MONDO_",
    "EMAPA" -> "http://purl.obolibrary.org/obo/EMAPA_",
    "SO" -> "http://purl.obolibrary.org/obo/SO_",
    "RO" -> "http://purl.obolibrary.org/obo/RO_",
    "GO" -> "http://purl.obolibrary.org/obo/GO_",
    "CHEBI" -> "http://purl.obolibrary.org/obo/CHEBI_",
    "BFO" -> "http://purl.obolibrary.org/obo/BFO_",
    "obo" -> "http://purl.obolibrary.org/obo/",
    "NCBIGENE" -> "http://identifiers.org/ncbigene:",
    "sesame" -> "http://www.openrdf.org/schema/sesame#",
    "owl" -> "http://www.w3.org/2002/07/owl#",
    "prov" -> "http://www.w3.org/ns/prov#",
    "MESH" -> "http://id.nlm.nih.gov/mesh/"
  )

  case class NewTranslatorEdge(`type`: String, source_id: String, target_id: String)

  def getNodeTypes(nodes: List[TranslatorQueryNode]): Map[String, String] = {
    val nodeTypes = nodes.collect {
      case node if node.`type`.nonEmpty => (node.id, "bl:" + CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, node.`type`))
    }.toMap
    val newNodeTypes = nodeTypes ++ nodes.flatMap(node => node.curie.map(node.id -> _)).toMap
    newNodeTypes
  }

  def prefixes = PREFIXES.map(entry => s"PREFIX ${entry._1}: <${entry._2}>").mkString("\n")

  def applyPrefix(value: String): String =
    PREFIXES
      .filter(entry => value.startsWith(entry._2))
      .map(entry => s"${entry._1}:" + value.substring(entry._2.length, value.length))
      .headOption
      .getOrElse(value);

  def run(limit: Int, queryGraph: TranslatorQueryGraph): RIO[ZConfig[AppConfig], ResultSet] = {
    val nodeTypes = QueryService.getNodeTypes(queryGraph.nodes)

    val getPredicates = ZIO.foreach(queryGraph.edges.filter(_.`type`.nonEmpty)) { edge =>
      for {
        queryText <- Task.effect(
          s"""PREFIX bl: <https://w3id.org/biolink/vocab/>
                |SELECT DISTINCT ?predicate WHERE {
                |bl:${edge.`type`} <http://reasoner.renci.org/vocab/slot_mapping> ?predicate .
                |}""".stripMargin
        )
        query <- Task.effect(QueryFactory.create(queryText))
        resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
        predicates = (for {
            solution <- resultSet.asScala
            v <- solution.varNames.asScala
            node = solution.get(v)
          } yield s"<$node>").mkString(" ")
        predicateValuesBlock = s"VALUES ?${edge.id} { $predicates }"
        triple = s"  ?${edge.source_id} ?${edge.id} ?${edge.target_id} ."
      } yield (Set(edge.source_id, edge.target_id),
               Set(edge.source_id -> edge.source_id, edge.target_id -> edge.target_id),
               s"$predicateValuesBlock\n$triple")
    }
    for {
      predicates <- getPredicates
      whereClauseParts =
        queryGraph.nodes
          .map(node => String.format("  ?%1$s sesame:directType ?%1$s_type .", node.id))
          .mkString("\n")
      whereClause = s"WHERE { \n$whereClauseParts"
      (instanceVars, instanceVarsToTypes, sparqlLines) = predicates.unzip3
      ids =
        instanceVars.toSet.flatten.map(a => s"?$a").toList :::
          queryGraph.nodes.map(a => s"?${a.id}_type") :::
          queryGraph.edges.map(a => s"?${a.id}")
      selectClause = s"SELECT DISTINCT ${ids.mkString(" ")} "
      valuesClause = {
        var bindings = mutable.ListBuffer[String]()
        bindings += sparqlLines.mkString("")
        instanceVarsToTypes.foreach(a =>
          a.foreach(b =>
            nodeTypes.get(b._2) match {
              case Some(v) => bindings += s"?${b._1} rdf:type $v ."
              case None => bindings += ""
            }))
        s"${bindings.mkString("\n")}"
      }
      limitSparql = if (limit > 0) s" LIMIT $limit" else ""
      queryString = s"$prefixes\n$selectClause\n$whereClause\n$valuesClause \n } $limitSparql"
      query <- Task.effect(QueryFactory.create(queryString))
      response <- SPARQLQueryExecutor.runSelectQuery(query)
    } yield response
  }

  def parseResultSet(queryGraph: TranslatorQueryGraph, resultSet: ResultSet): RIO[ZConfig[AppConfig], TranslatorMessage] =
    for {
      kgNodes <- Task.effect(mutable.ListBuffer[TranslatorNode]())
      kgEdges <- Task.effect(mutable.ListBuffer[TranslatorEdge]())
      results <- ZIO.foreach(resultSet.asScala.toList) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
          nodeBindings <- ZIO.foreach(queryGraph.nodes) { n =>
            for {
              abbreviatedNodeType <- Task.effect(applyPrefix(nodeMap.get(n.id).get))
              nodeDetails <- getKnowledgeGraphNodeDetails(String.format("<%s>", nodeMap.get(n.id).get))
              nodeDetailsHead <- Task.effect(nodeDetails.head)
              _ = kgNodes += TranslatorNode(abbreviatedNodeType, Some(nodeDetailsHead._1), nodeDetailsHead._2, List[TranslatorNodeAttribute]())
              nodeBinding <- Task.effect(TranslatorNodeBinding(n.id, abbreviatedNodeType))
            } yield nodeBinding
          }

          edgeBindings <- ZIO.foreach(queryGraph.edges) { e =>
            for {
              predicateRDFNode <- Task.effect(querySolution.get(e.id).toString)
              sourceRDFNode <- Task.effect(querySolution.get(e.source_id).toString)
              targetRDFNode <- Task.effect(querySolution.get(e.target_id).toString)
              knowledgeGraphId = {
                val newTranslatorEdge =
                  NewTranslatorEdge(e.`type`, e.source_id, e.target_id).asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
                val messageDigest = MessageDigest.getInstance("SHA-256")
                String.format("%064x", new BigInteger(1, messageDigest.digest(newTranslatorEdge)))
              }
              _ = kgEdges += TranslatorEdge(knowledgeGraphId,
                                            Some(e.`type`),
                                            applyPrefix(nodeMap.get(e.source_id).get),
                                            applyPrefix(nodeMap.get(e.target_id).get))
              prov <- getProvenance(sourceRDFNode, predicateRDFNode, targetRDFNode)
            } yield TranslatorEdgeBinding(e.id, knowledgeGraphId, Some(prov.toString))
          }

        } yield TranslatorResult(nodeBindings, edgeBindings)
      }
    } yield TranslatorMessage(Some(queryGraph), Some(TranslatorKnowledgeGraph(kgNodes.toList, kgEdges.toList)), results)

  def getProvenance(source: String, predicate: String, target: String): RIO[ZConfig[AppConfig], String] =
    for {
      queryText <- Task.effect(
        s"""$prefixes
          |SELECT ?g ?other WHERE {
          |GRAPH ?g { <$source> <$predicate> <$target> } OPTIONAL { ?g prov:wasDerivedFrom ?other . }
          |}""".stripMargin
      )
      query <- Task.effect(QueryFactory.create(queryText))
      bindings <- SPARQLQueryExecutor.runSelectQuery(query)
      nextSolution = bindings.nextSolution()
      prov <- Task.effect(nextSolution.get("other").toString).orElse(Task.effect(nextSolution.get("g").toString))
    } yield prov

  def getKnowledgeGraphNodeDetails(nodeIds: String): RIO[ZConfig[AppConfig], Map[String, List[String]]] =
    for {
      queryText <- Task.effect(
        s"""$prefixes
            |SELECT DISTINCT ?kid ?blclass ?label WHERE {
            |VALUES ?kid { $nodeIds }
            |?kid rdfs:subClassOf ?blclass .
            |?blclass blml:is_a* bl:NamedThing .
            |OPTIONAL { ?kid rdfs:label ?label . }
            |}""".stripMargin
      )
      query <- Task.effect(QueryFactory.create(queryText))
      resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
      map <- Task.effect(
        resultSet.asScala.toList.map(qs => (qs.get("label").toString, qs.get("blclass").toString)).groupBy(_._1).map {
          case (k, v) =>
            (k, v.map(a => CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, a._2.substring(a._2.lastIndexOf("/") + 1, a._2.length))))
        }
      )
    } yield map

  def getCAMStuffQuery(prov: String): RIO[ZConfig[AppConfig], String] =
    for {
      queryText <- Task.effect(
        s"""$prefixes
           |SELECT ?s_type ?p ?o_type WHERE {
           |GRAPH <{prov}> { {
           |?s ?p ?o .
           |?s rdf:type owl:NamedIndividual .
           |?o rdf:type owl:NamedIndividual .
           |}
           |?o sesame:directType ?o_type .
           |?s sesame:directType ?s_type .
           |}""".stripMargin
      )
      query <- Task.effect(QueryFactory.create(queryText))
      resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
      response <- ZIO.succeed("")
    } yield response

}
