package aml.cim.model

import amf.core.vocabulary.Namespace
import aml.cim.CIM
import org.apache.jena.rdf.model.Model

case class FunctionalArea(id: String, version: String, name: String, description: Option[String], classes: Seq[String], properties: Seq[String])
case class RdfsClass(id: String, name: String, displayName: Option[String], description: Option[String], superClasses: Seq[String])
case class RdfProperty(id: String, name: String, displayName: Option[String], description: Option[String], domains: Seq[String], ranges: Seq[String])

class ConceptualModel(val jsonld: Model) extends ModelHelper {

  protected val RDFS_CLASS: String = (Namespace.Rdfs + "Class").iri()
  protected  val RDFS_LABEL: String = (Namespace.Rdfs + "label").iri()
  protected val RDFS_COMMENT: String = (Namespace.Rdfs + "comment").iri()
  protected  val RDFS_SUBCLASS_OF: String = (Namespace.Rdfs + "subClassOf").iri()

  protected val RDF_PROPERTY: String = (Namespace.Rdf + "Property").iri()
  protected  val RDFS_DOMAIN: String = (Namespace.Rdfs + "domain").iri()
  protected val RDFS_RANGE: String = (Namespace.Rdfs + "range").iri()

  /**
   * Find all the RDFS classes defined in the model
   * @return
   */
  lazy val rdfsClasses: Seq[RdfsClass] = {
    findInstancesOf(RDFS_CLASS) map { rdfsClass =>
      val id = rdfsClass.getURI
      val name = id.split("/").last
      val displayName = findProperty(id, RDFS_LABEL).map(_.getString)
      val description = findProperty(id, RDFS_COMMENT).map(_.getString)
      val superClasses = findRelatedResources(id, RDFS_SUBCLASS_OF).map(_.getURI)
      RdfsClass(
        id,
        name,
        displayName,
        description,
        superClasses
      )
    }
  }

  /**
   * All the RDF properties in the model
   */
  lazy val rdfProperties: Seq[RdfProperty] = {
    findInstancesOf(RDF_PROPERTY) map { rdfProperty =>
      val id = rdfProperty.getURI
      val name = id.split("/").last
      val displayName = findProperty(id, RDFS_LABEL).map(_.getString)
      val description = findProperty(id, RDFS_COMMENT).map(_.getString)
      val domains = findRelatedResources(id, RDFS_DOMAIN).map(_.getURI)
      val ranges = findRelatedResources(id, RDFS_RANGE).map(_.getURI)

      RdfProperty(
        id,
        name,
        displayName,
        description,
        domains,
        ranges
      )
    }
  }


  lazy val functionalAreas: Seq[FunctionalArea] = {
    findInstancesOf(CIM.FUNCTIONAL_AREA) map { fa =>
      val id = fa.getURI
      val name = findProperty(id, RDFS_LABEL).map(_.getString).getOrElse(id.split("/").last)
      val description = findProperty(id, RDFS_COMMENT).map(_.getString)
      val version = findProperty(id, CIM.VERSION).map(_.getString).getOrElse(throw new Exception(s"Missing mandatory version for functional area $id"))
      val classes = findRelatedResources(id, CIM.CLASSES).map(_.getURI)
      val properties = findRelatedResources(id, CIM.PROPERTIES).map(_.getURI)
      FunctionalArea(
        id,
        version,
        name,
        description,
        classes,
        properties
      )
    }
  }

}
