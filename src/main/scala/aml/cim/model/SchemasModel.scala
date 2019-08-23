package aml.cim.model

import amf.core.vocabulary.Namespace
import org.apache.jena.rdf.model.Model

case class ShaclShape(id: String, name: String, properties: Seq[ShaclProperty])
case class ShaclProperty(path: String, mandatory: Boolean, objectRange: Option[String], scalarRange: Option[String])

class SchemasModel(val jsonld: Model) extends ModelHelper {

  protected val SH_SHAPE: String = (Namespace.Shacl + "Shape").iri()
  protected val SH_PROPERTY: String = (Namespace.Shacl + "property").iri()
  protected val SH_PATH: String = (Namespace.Shacl + "path").iri()
  protected val SH_MIN_COUNT: String = (Namespace.Shacl + "minCount").iri()
  protected val SH_DATATYPE: String = (Namespace.Shacl + "datatype").iri()
  protected val SH_NODE: String = (Namespace.Shacl + "node").iri()

  lazy val shapes: Seq[ShaclShape] = {
    findInstancesOf(SH_SHAPE) map { shaclShape =>
      val id = shaclShape.getURI
      val name = id.split("/").last

      val properties = shapeProperties(id)

      ShaclShape(
        id,
        name,
        properties
      )
    }
  }

  def shapeProperties(id: String): Seq[ShaclProperty] = {
    findRelatedResources(id, SH_PROPERTY) map { propertyShape =>
      val propertyId = propertyShape.getId.getLabelString
      val path = findRelatedResource(propertyId, SH_PATH).map(_.getURI).getOrElse(throw new Exception(s"Missing 'sh:path' for property constraint of shape for '$id'"))
      val mandatory = findProperty(propertyId, SH_MIN_COUNT).exists(_.getInt > 0)

      val datatypeRange = findRelatedResource(propertyId, SH_DATATYPE).map(_.getURI)
      val objectRange = findRelatedResource(propertyId, SH_NODE).map(_.getURI)

      if (datatypeRange.nonEmpty && objectRange.nonEmpty) {
        throw new Exception(s"Shape '$id' with both datatype and node ranges defined")
      }

      if (datatypeRange.isEmpty && objectRange.isEmpty) {
        throw new Exception(s"Shape '$id' without datatype or node ranges defined")
      }

      ShaclProperty(
        path,
        mandatory,
        objectRange,
        datatypeRange
      )
    }
  }
}
