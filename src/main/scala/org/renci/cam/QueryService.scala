package org.renci.cam

import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import org.apache.commons.text.CaseUtils
import org.apache.jena.query.{ResultSet, ResultSetFactory}
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam.domain._
import zio._
import zio.interop.catz._
import zio.ZIO.ZIOAutoCloseableOps
import zio.config.{Config, _}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object QueryService extends LazyLogging {

  implicit val runtime: Runtime[ZEnv] = Runtime.default

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

  def makeHttpClient: UIO[TaskManaged[Client[Task]]] =
    ZIO.runtime[Any].map { implicit rts =>
      BlazeClientBuilder[Task](rts.platform.executor.asEC).withConnectTimeout(Duration(3, MINUTES)).resource.toManaged
    }

  implicit val sparqlJsonDecoder: EntityDecoder[Task, ResultSet] = EntityDecoder[Task, String].flatMapR { jsonText =>
    Try(ResultSetFactory.fromJSON(IOUtils.toInputStream(jsonText, StandardCharsets.UTF_8))) match {
      //IntelliJ shows false compile errors here; check with SBT
      //Requires import of ZIO-Cats interop typeclasses to compile
      case Success(resultSet) => DecodeResult.success(resultSet)
      case Failure(e) => DecodeResult.failure(MalformedMessageBodyFailure("Invalid JSON for SPARQL results", Some(e)))
    }
  }

  def getNodeTypes(nodes: List[KGSNode]): Map[String, String] = {
    val nodeTypes = nodes.collect {
      case (node) if node.`type`.nonEmpty => (node.id, "bl:" + CaseUtils.toCamelCase(node.`type`, true, '_'))
    }.toMap
    nodeTypes ++ nodes.collect { case (node) if node.curie.nonEmpty => (node.id, node.curie.get) }.toMap
    nodeTypes
  }

  def runSPARQLSelectQuery(query: String, appConfig: AppConfig): Task[ResultSet] =
    for {
      clientManaged <- makeHttpClient
      //building a uri from config shouldn't be this verbose...any better way to do this?
      uri = Uri(
        scheme = Some(Uri.Scheme.http),
        authority =
          Some(Uri.Authority(host = Uri.RegName(appConfig.`sparql-host`), port = Some(appConfig.`sparql-port`))),
        path = "/blazegraph/sparql"
      ).withQueryParam("query", query).withQueryParam("format", "json")
      request = Request[Task](Method.POST, uri)
        .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
      response <- clientManaged.use(_.expect[ResultSet](request))
    } yield response

  def run(limit: Int, queryGraph: KGSQueryGraph, appConfig: AppConfig): Task[ResultSet] = {
    val nodeTypes = QueryService.getNodeTypes(queryGraph.nodes)
    logger.info("appConfig: {}", appConfig)
    val query = StringBuilder.newBuilder
    queryGraph.nodes.foreach(node =>
      query.append(String.format("  ?%1$s sesame:directType ?%1$s_type .%n", node.`type`)))

    var instanceVars: Set[String] = Set()
    var instanceVarsToTypes: Map[String, String] = Map()

    for ((edge, idx) <- queryGraph.edges.view.zipWithIndex)
      if (edge.`type`.nonEmpty) {
        val getPredicates = for {
          resultSet <- runSPARQLSelectQuery(
            s"""PREFIX bl: <https://w3id.org/biolink/vocab/>
              SELECT DISTINCT ?predicate WHERE { bl:${edge.`type`} <http://reasoner.renci.org/vocab/slot_mapping> ?predicate . }""",
            appConfig
          )
          bindings = (for {
              solution <- resultSet.asScala
              v <- solution.varNames.asScala
              node = solution.get(v)
            } yield s"<$node>").mkString(" ")
        } yield bindings

        val predicates = runtime.unsafeRun(getPredicates)
        query.append(s"VALUES ?${edge.`type`} { $predicates }\n")
        query.append(s"  ?${edge.source_id} ?${edge.id} ?${edge.target_id} .\n")

        instanceVars += (edge.source_id, edge.target_id)
        instanceVarsToTypes += (edge.source_id -> edge.source_id, edge.target_id -> edge.target_id)

      }

    for ((key, value) <- instanceVarsToTypes)
      nodeTypes.get(value) match {
        case Some(v) => query.append(s"?$key rdf:type $v .\n")
        case None => query.append("") // how to do nothing here?
      }

    query.append("}")
    logger.debug("query: {}", query)
    if (limit > 0) query.append(s" LIMIT $limit")

    val prequel = StringBuilder.newBuilder
    for ((key, value) <- PREFIXES)
      prequel.append(s"PREFIX $key: <$value>\n")

    val ids = instanceVars.map(a => s"?$a").toList :::
      queryGraph.nodes.map(a => s"?${a.id}_type") :::
      queryGraph.edges.map(a => s"?${a.id}")

    prequel.append(s"\nSELECT DISTINCT ${ids.mkString(" ")} WHERE {\n")

    val full_query = prequel.toString() + query.toString()
    logger.debug("full_query: {}", full_query)
    val queryResponse = runSPARQLSelectQuery(full_query, appConfig)
    queryResponse
  }

}
