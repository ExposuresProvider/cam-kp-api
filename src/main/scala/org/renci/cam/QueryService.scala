package org.renci.cam

import java.nio.charset.StandardCharsets

import org.apache.commons.io.IOUtils
import org.apache.jena.query.{ResultSet, ResultSetFactory}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.{DecodeResult, EntityDecoder, MalformedMessageBodyFailure}
import zio._
import zio.interop.catz._

import scala.util.{Failure, Success, Try}

object QueryService {

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
      BlazeClientBuilder[Task](rts.platform.executor.asEC).resource.toManaged
    }

  implicit val sparqlJsonDecoder: EntityDecoder[Task, ResultSet] = EntityDecoder[Task, String].flatMapR { jsonText =>
    Try(ResultSetFactory.fromJSON(IOUtils.toInputStream(jsonText, StandardCharsets.UTF_8))) match {
      //IntelliJ shows false compile errors here; check with SBT
      //Requires import of ZIO-Cats interop typeclasses to compile
      case Success(resultSet) => DecodeResult.success(resultSet)
      case Failure(e) => DecodeResult.failure(MalformedMessageBodyFailure("Invalid JSON for SPARQL results", Some(e)))
    }
  }

}
