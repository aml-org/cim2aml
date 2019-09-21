package aml.cim.model

import amf.core.vocabulary.Namespace
import aml.cim.CIM
import aml.cim.model.entities.{ConceptualGroup, PathDependency, ShaclProperty, ShaclShape, ShapeDependency}
import org.apache.jena.rdf.model.{Model, RDFList, Resource}

class SchemasModel(val jsonld: Model, ontology: Seq[ConceptualGroup]) extends ModelHelper {

  lazy val shapes: Seq[ShaclShape] = {
    findInstancesOf(SH_SHAPE) map { shaclShape =>
      val id = shaclShape.getURI
      val name = id.split("/").last

      shapeExtensions(id) match {
        case Some((baseShape, properties)) =>
          ShaclShape(
            id,
            name,
            properties,
            Seq(baseShape)
          )
        case None                        =>
          val properties = shapeProperties(id)
          ShaclShape(
            id,
            name,
            properties,
            Nil
          )
      }
    }
  }

  def shapeExtensions(id: String): Option[(String, Seq[ShaclProperty])] = {
    val composed = findRelatedResources(id, SH_AND)
    if (composed.nonEmpty) {
      composed.map { resource: Resource =>
        val list = resource.as(classOf[RDFList])
        val baseShape = list.get(0).asResource().getURI
        val properties = shapeProperties(list.get(1).asResource().getId.toString)
        (baseShape, properties)
      } headOption
    } else {
      None
    }
  }

  def shapeProperties(id: String): Seq[ShaclProperty] = {
    findRelatedResources(id, SH_PROPERTY) map { propertyShape =>
      val propertyId = propertyShape.getId.getLabelString
      val path = findRelatedResource(propertyId, SH_PATH).map(_.getURI).getOrElse(throw new Exception(s"Missing 'sh:path' for property constraint of shape for '$id'"))
      val name = path.split("/").last
      val mandatory = findProperty(propertyId, SH_MIN_COUNT).exists(_.getInt > 0)
      val oneToOne = findProperty(propertyId, SH_MAX_COUNT).exists(_.getInt == 1)
      val enum = findRelatedProperties(propertyId, SH_IN).map(_.getString)

      var datatypeRange = findRelatedResource(propertyId, SH_DATATYPE).map(_.getURI)
      val objectRange = findRelatedResource(propertyId, SH_NODE).map(_.getURI)

      if(findProperty(propertyId, SH_IN).isDefined) {
        datatypeRange = Some((Namespace.Xsd + "string").iri())
      }

      if (datatypeRange.nonEmpty && objectRange.nonEmpty) {
        throw new Exception(s"Shape '$id' with both datatype and node ranges defined for property path '${path}'")
      }

      if (datatypeRange.isEmpty && objectRange.isEmpty) {
        throw new Exception(s"Shape '$id' without datatype or node ranges defined for property path '${path}'")
      }

      ShaclProperty(
        name,
        path,
        mandatory,
        !oneToOne,
        objectRange,
        datatypeRange,
        enum
      )
    }
  }

  lazy val entityGroups: Seq[ConceptualGroup] = {
    findInstancesOf(CIM.ENTITY_GROUP) map { fa =>
      val id = fa.getURI
      val name = findProperty(id, RDFS_LABEL).map(_.getString).getOrElse(id.split("/").last)
      val description = findProperty(id, RDFS_COMMENT).map(_.getString)
      val location = findProperty(id, CIM.LOCATED).map(_.getString)
      // val version = findProperty(id, CIM.VERSION).map(_.getString).getOrElse(throw new Exception(s"Missing mandatory version for functional area $id"))
      val shapes = findRelatedResources(id, CIM.SCHEMAS).map(_.getURI)
      ConceptualGroup(
        id,
        name,
        description,
        Nil,
        Nil,
        shapes,
        location
      )
    }
  }

  lazy val attributeGroups: Seq[ConceptualGroup] = ontology.filter(cg => cg.properties.nonEmpty)

  def linkEntityGroups = {
    entityGroups.foreach { entityGroup =>
      entityGroup.shapes.foreach { shapeId =>
        val shape = shapes.find(_.id == shapeId).getOrElse(throw new Exception(s"Cannot find shape with ID ${shapeId} for entity group ${entityGroup.id}"))
        // check object properties
        val objectProperties = shape.properties.toList.filter(_.objectRange.nonEmpty)
        objectProperties.foreach { objectProperty =>
          val range = objectProperty.objectRange.get
          findEntityGroupForShape(range) match {
            case Some(rangeEntityGroup) if rangeEntityGroup.id == entityGroup.id => // ignore
            case Some(rangeEntityGroup) if rangeEntityGroup.id != entityGroup.id => // dependency
              val rangeShape = shapes.find(_.id == range).getOrElse(throw new Exception(s"Cannot find entity by ID ${range}"))
              entityGroup.dependencies += ShapeDependency(rangeEntityGroup, rangeShape) // = entityGroup.dependencies ++ Seq(ShapeDependency(rangeEntityGroup, rangeShape))
            case _ =>
              println(s"Cannot find entity group for shape ID ${range} in entity group ${entityGroup.name}")
              // throw new Exception(s"Cannot find entity group for shape ID ${range}")
          }
        }

        shape.properties.foreach { property =>
          val path = property.path
          findAttributeGroupForShape(path) match {
            case Some(rangeEntityGroup) if rangeEntityGroup.id == entityGroup.id => // ignore
            case Some(rangeEntityGroup) if rangeEntityGroup.id != entityGroup.id => // dependency
              entityGroup.pathDependencies += PathDependency(rangeEntityGroup, path) // = entityGroup.dependencies ++ Seq(ShapeDependency(rangeEntityGroup, rangeShape))
            case _ =>
              println(s"Cannot find entity group for property ID ${path} in entity group ${entityGroup.name}")
            // throw new Exception(s"Cannot find entity group for shape ID ${range}")
          }
        }
      }
    }
  }

  def findEntityGroupForShape(id: String) = {
    entityGroups.find { entityGroup =>
      entityGroup.shapes.contains(id)
    }
  }

  def findAttributeGroupForShape(id: String) = {
    attributeGroups.find { attributeGroup =>
      attributeGroup.properties.contains(id)
    }
  }

  def entityGroupId: Option[String] = findInstancesOf(CIM.ENTITY_GROUP).headOption.map(_.getURI)

  def findShapeById(id: String): Option[ShaclShape] = shapes.find(_.id == id)
}
