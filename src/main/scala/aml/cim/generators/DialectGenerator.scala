package aml.cim.generators

import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.vocabularies.model.domain.{DocumentMapping, DocumentsModel, External, NodeMapping, PropertyMapping, PublicNodeMapping}
import aml.cim.CIM
import aml.cim.model.SchemasModel
import aml.cim.model.entities.FunctionalArea

class DialectGenerator(conceptualModel: SchemasModel, functionalArea: FunctionalArea) {

  def generate(): Dialect = {
    val dialect = Dialect().withId(functionalArea.id).withName(functionalArea.name).withVersion(functionalArea.version)
    val nodes = nodeMappings

    dialect.withDeclares(nodes)
    dialect.withExternals(Seq(External().withAlias(functionalArea.name).withBase(CIM.NS.base)))
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
    functionalArea.shapes.map { shapeId =>
      val shape = conceptualModel.findShapeById(shapeId).getOrElse(throw new Exception(s"Cannot find shape '$shapeId' in functional area ${functionalArea.id}"))
      val nodeMapping = NodeMapping().withId(nodeMappingId(shape.id)).withName(shape.name).withNodeTypeMapping(shape.id)
      val propertyMappings = shape.properties.map { property =>
        val propertyMapping = PropertyMapping().withName(property.name).withNodePropertyMapping(property.path)
        if (property.mandatory) {
          propertyMapping.withMinCount(1)
        }
        property.objectRange.foreach { objectRange =>
          propertyMapping.withObjectRange(Seq(nodeMappingId(objectRange)))
        }
        property.scalarRange.foreach { scalarRange =>
          propertyMapping.withLiteralRange(scalarRange)
        }

        propertyMapping
      }
      nodeMapping.withPropertiesMapping(propertyMappings)
    }
  }

  def nodeMappingId(id: String): String = id.replace(CIM.NS.base, functionalArea.id + "/declarations/")
}
