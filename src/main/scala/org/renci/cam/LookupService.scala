package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import sttp.tapir.generic.auto._
import org.phenoscape.sparql.SPARQLInterpolation._
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.{RDFNode, Resource}
import org.http4s.{HttpRoutes, InvalidBodyException}
import org.renci.cam.Biolink.{biolinkData, BiolinkData}
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.Server.EndpointEnv
import org.renci.cam.Server.LocalTapirJsonCirce.jsonBody
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, LogEntry, TRAPIMessage, TRAPINode, TRAPIQuery, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode, TRAPIResponse}
import sttp.tapir.Endpoint
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.ztapir.{endpoint, query, stringBody}
import zio.{Has, RIO, URIO, ZIO}
import org.http4s.implicits._

/** The LookupService can be used to look up concepts within CAM-KP-API without needing to use either high-level TRAPI queries or low-level
  * SPARQL queries. It is intended to provide a middle path to interrogate the database and to identify cases where the TRAPI interface
  * (QueryService) doesn't provide the correct results.
  */
object LookupService extends LazyLogging {

  /* DATA STRUCTURES */
  case class ResultTriplesIRIs(
    s: String,
    p: String,
    o: String,
    g: String
  )

  case class Result(
    biolinkPredicates: Set[BiolinkPredicate],
    snaks: Seq[Snak],
    subjectTriples: Seq[ResultTriplesIRIs],
    objectTriples: Seq[ResultTriplesIRIs]
  )

  case class Error(
    code: String,
    message: String
  )

  case class LabeledIRI(
    iri: String,
    label: Option[String]
  )

  case class Snak(
    p: LabeledIRI,
    biolinkPredicate: Option[LabeledIRI],
    obj: LabeledIRI,
    g: String
  )

  /* CONTROLLER */
  type LookupEndpointParams = (String, Option[String], Option[String])
  type LookupEndpoint = Endpoint[LookupEndpointParams, Error, Result, Any]

  def convertSPOToResultTriplesIRIs(result: QuerySolution): ResultTriplesIRIs =
    // Note that either ?s or ?o may be bnodes!
    ResultTriplesIRIs(
      result.get("s").toString,
      result.get("p").toString,
      result.get("o").toString,
      result.get("g").toString
    )

  def lookup(subject: String, predicateOpt: Option[String], objOpt: Option[String]): ZIO[EndpointEnv, Throwable, Result] = for {
    biolinkData <- biolinkData

    subjectIRI = {
      implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)

      // TODO: throw an error here.
      iriKeyDecoder(subject).getOrElse(IRI("http://identifiers.org/ncbigene/478"))
    }

    // Get every triple that has the subject as a subject.
    subjectTriplesQueryString = sparql"""SELECT DISTINCT ($subjectIRI AS ?s) ?p ?o ?g { GRAPH ?g { $subjectIRI ?p ?o }}"""
    subjectTriples <- SPARQLQueryExecutor.runSelectQuery(subjectTriplesQueryString.toQuery)
    _ = logger.debug(s"SPARQL query for subjectTriples: ${subjectTriplesQueryString.toQuery}")
    _ = logger.debug(s"Results for subjectTriples: ${subjectTriples}")

    // Get every triple that has the subject as an object.
    objectTriplesQueryString = sparql"""SELECT DISTINCT ?s ?p ($subjectIRI AS ?o) ?g { GRAPH ?g { ?s ?p $subjectIRI }}"""
    objectTriples <- SPARQLQueryExecutor.runSelectQuery(objectTriplesQueryString.toQuery)
    _ = logger.debug(s"SPARQL query for objectTriples: ${objectTriplesQueryString.toQuery}")
    _ = logger.debug(s"Results for objectTriples: ${objectTriples}")

    // Get every biolink predicate that has the subject as a subject.
    snaksQuery =
      sparql"""PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
               PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
               
               SELECT DISTINCT ?p ?pLabel ?obj ?objLabel ?g {
                  ?s rdf:type $subjectIRI .
                  ?o rdf:type ?obj .
                  
                  GRAPH ?g { 
                    ?s ?p ?o
                  }
                  
                  OPTIONAL {
                    ?p rdfs:label ?pLabel
                  }
               }"""
    snakResults <- SPARQLQueryExecutor.runSelectQuery(snaksQuery.toQuery)
    preds = snakResults.map(_.getResource("p").getURI).map(IRI(_))
    biolinkRelationMap <- QueryService.mapRelationsToLabelAndBiolink(preds.toSet)
    snaks = snakResults.map { res =>
      val predIRI = res.getResource("p").getURI
      val pred = res.getLiteral("pLabel") match {
        case null => LabeledIRI(predIRI, None)
        case lit  => LabeledIRI(predIRI, Some(lit.getString))
      }
      val biolinkRes = biolinkRelationMap.get(IRI(predIRI))
      val biolink = biolinkRes match {
        case None                     => None
        case Some((None, iri))        => Some(LabeledIRI(iri.value, None))
        case Some((Some(label), iri)) => Some(LabeledIRI(iri.value, Some(label)))
      }

      val objIRI = res.getResource("obj").getURI
      val obj = res.getLiteral("objLabel") match {
        case null => LabeledIRI(objIRI, None)
        case lit  => LabeledIRI(objIRI, Some(lit.getString))
      }

      Snak(
        pred,
        biolink,
        obj,
        res.getResource("g").getURI
      )
    }
  } yield Result(
    Set(),
    snaks,
    subjectTriples.map(convertSPOToResultTriplesIRIs),
    objectTriples.map(convertSPOToResultTriplesIRIs)
  )

  /* HTTP4S INTERFACE */
  val lookupEndpointZ: URIO[Has[BiolinkData], LookupEndpoint] =
    for {
      biolinkData <- biolinkData
    } yield {
      implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
      implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)

      implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
      implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)

      implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
      implicit val biolinkClassDecoder: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)

      implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
      implicit val biolinkPredicateDecoder: Decoder[List[BiolinkPredicate]] =
        Implicits.predicateOrPredicateListDecoder(biolinkData.predicates)

      // The encoded example asks what biological process or activities positively regulate GO:0004707
      // (MAP kinase activity, see http://purl.obolibrary.org/obo/GO_0004707)
      endpoint.get
        .in("lookup")
        .in(
          query[String]("subject")
            .default("NCBIGene:478")
        )
        .in(
          query[Option[String]]("predicate")
            .default(None)
            .example(None)
        )
        .in(
          query[Option[String]]("object")
            .default(None)
            .example(None)
        )
        .out(jsonBody[Result])
        .errorOut(jsonBody[Error])
        .summary("Look up relationships within CAM-KP-API by identifier.")
    }

  def lookupRouteR(lookupEndpoint: LookupEndpoint): HttpRoutes[RIO[EndpointEnv, *]] =
    ZHttp4sServerInterpreter[EndpointEnv]()
      .from(lookupEndpoint) { case (s, p, o) => lookup(s, p, o).mapError(ex => Error("interp_error", ex.getMessage)) }
      .toRoutes

}
