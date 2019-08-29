package aml.cim.generators

import amf.core.vocabulary.Namespace
import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.vocabularies.model.domain.{DocumentMapping, DocumentsModel, External, NodeMapping, PropertyMapping, PublicNodeMapping}
import aml.cim.CIM
import aml.cim.model.SchemasModel
import aml.cim.model.entities.EntityGroup

class DialectGenerator(schemaModel: SchemasModel, entityGroup: EntityGroup, version: String) {

  def generate(): Dialect = {
    val dialect = Dialect().withId(entityGroup.id).withName(entityGroup.name).withVersion(version)
    val nodes = nodeMappings

    dialect.withDeclares(nodes)
    dialect.withExternals(Seq(External().withAlias("cim").withBase(CIM.NS.base)))
    val docModel = DocumentsModel()
    val publicNodeMappings = nodeMappings.map { nodeMapping =>
      PublicNodeMapping().withMappedNode(nodeMapping.id.split("/").last).withName(nodeMapping.name + "Schemas")
    }
    docModel.withRoot(DocumentMapping().withDeclaredNodes(publicNodeMappings))
    val fragmentMapings = nodeMappings.map { nodeMapping =>
      DocumentMapping().withEncoded(nodeMapping.id.split("/").last).withDocumentName(nodeMapping.id.split("/").last)
    }
    docModel.withFragments(fragmentMapings)
    dialect.withDocuments(docModel)
  }

  lazy protected val nodeMappings: Seq[NodeMapping] = {
    entityGroup.shapes.map { shapeId =>
      val shape = schemaModel.findShapeById(shapeId).getOrElse(throw new Exception(s"Cannot find shape '$shapeId' in Entity Group ${entityGroup.id}"))
      val nodeMapping = NodeMapping().withId(nodeMappingId(shape.id)).withName(shape.name).withNodeTypeMapping(shape.id)
      val propertyMappings = shape.properties.map { property =>
        val propertyMapping = PropertyMapping().withName(property.name).withNodePropertyMapping(property.path)
        if (property.mandatory) {
          propertyMapping.withMinCount(1)
        }
        if (property.allowMultiple) {
          propertyMapping.withAllowMultiple(true)
        }
        property.objectRange.foreach { objectRange =>
          propertyMapping.withObjectRange(Seq(nodeMappingId(objectRange)))
        }
        property.scalarRange.foreach { scalarRange =>
          if (scalarRange == CIM.cim("id")) {
            propertyMapping.withLiteralRange((Namespace.Xsd + "string").iri())
          } else {
            propertyMapping.withLiteralRange(scalarRange)
          }
        }

        propertyMapping
      }
      nodeMapping.withPropertiesMapping(propertyMappings)
    }
  }

  def nodeMappingId(id: String): String = id.replace(CIM.NS.base, entityGroup.id + "/declarations/")
}
