package aml.cim.model

import amf.core.vocabulary.Namespace
import aml.cim.CIM
import aml.cim.model.entities.{FunctionalArea, ShaclProperty, ShaclShape}
import org.apache.jena.rdf.model.Model

class SchemasModel(val jsonld: Model) extends ModelHelper {

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
      val name = path.split("/").last
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
        name,
        path,
        mandatory,
        objectRange,
        datatypeRange
      )
    }
  }

  lazy val functionalAreas: Seq[FunctionalArea] = {
    findInstancesOf(CIM.FUNCTIONAL_AREA) map { fa =>
      val id = fa.getURI
      val name = findProperty(id, RDFS_LABEL).map(_.getString).getOrElse(id.split("/").last)
      val description = findProperty(id, RDFS_COMMENT).map(_.getString)
      val version = findProperty(id, CIM.VERSION).map(_.getString).getOrElse(throw new Exception(s"Missing mandatory version for functional area $id"))
      val shapes = findRelatedResources(id, CIM.SCHEMAS).map(_.getURI)
      FunctionalArea(
        id,
        version,
        name,
        description,
        Nil,
        Nil,
        shapes
      )
    }
  }

  def findShapeById(id: String): Option[ShaclShape] = shapes.find(_.id == id)
}
