package aml.cim.generators

import java.nio.file.Paths

import amf.core.annotations.Aliases
import amf.core.model.document.BaseUnit
import amf.core.model.domain.{DomainElement, Linkable}
import amf.core.vocabulary.Namespace
import amf.plugins.document.vocabularies.model.document.{Dialect, DialectLibrary}
import amf.plugins.document.vocabularies.model.domain._
import aml.cim.CIM
import aml.cim.model.SchemasModel
import aml.cim.model.entities.{EntityGroup, ShaclShape, ShapeDependency}

import scala.collection.mutable

class DialectGenerator(schemaModel: SchemasModel, entityGroup: EntityGroup, version: String) {

  def generate(): Dialect = {
    val dialect = Dialect().withId(entityGroup.id).withName(entityGroup.name).withVersion(version)
    val nodes = nodeMappings

    val aliases = computeDependencies(dialect, entityGroup)
    dialect.withDeclares(nodes)

    computeExtensions(aliases, dialect)

    dialect.withExternals(Seq(External().withAlias("cim").withBase(CIM.NS.base)))
    val docModel = DocumentsModel()
    val publicNodeMappings = nodeMappings.map { nodeMapping =>
      PublicNodeMapping().withMappedNode(nodeMapping.id.split("/").last).withName(nodeMapping.name + "Schemas")
    }
    docModel.withRoot(DocumentMapping().withDeclaredNodes(publicNodeMappings))
    val fragmentMappings = nodeMappings.map { nodeMapping =>
      DocumentMapping().withEncoded(nodeMapping.id.split("/").last).withDocumentName(nodeMapping.id.split("/").last)
    }
    docModel.withFragments(fragmentMappings)
    dialect.withDocuments(docModel)
  }

  protected def computeDependencies(dialect: Dialect, sourceEntityGroup: EntityGroup) = {
    val dependentEntityGroups: mutable.Seq[ShapeDependency] = sourceEntityGroup.dependencies
    val acc = mutable.Map[String,Seq[ShaclShape]]()
    val entityGroupsMap = mutable.Map[String, EntityGroup]()

    dependentEntityGroups.foreach { shapeDependency =>
      entityGroupsMap.put(shapeDependency.entityGroup.id, shapeDependency.entityGroup)
      val shapes = acc.getOrElse(shapeDependency.entityGroup.id, Seq[ShaclShape]())
      acc.put(shapeDependency.entityGroup.id, shapes ++ Seq(shapeDependency.shape))
    }

    val deps = entityGroupsMap.map { case (egId, entityGroup) =>
      val nodeMappings = acc(egId).map { shape => NodeMapping().withId(nodeMappingId(entityGroup,shape.id)).withName(shape.name) }
      val path = relativePaths(sourceEntityGroup.location.get, entityGroup.location.get).replace(".jsonld", ".yaml")
      (entityGroup, DialectLibrary().withId(entityGroup.id).withLocation(path).withDeclares(nodeMappings))
    }.toSeq

    dialect.withReferences(deps.map(_._2))

    computeAliases(dialect, deps)
  }

  protected def computeAliases(dialect: Dialect, deps: Seq[(EntityGroup, BaseUnit)]) = {
    var acc = mutable.Map[String,String]()

    if (deps.nonEmpty) {
      val aliasesValues = deps.map { case (eg: EntityGroup, dep: DialectLibrary) =>
        val alias = eg.name.split(" ").map(_.toLowerCase).mkString("_").replace("entitygroup", "_eg")
        acc += (dep.id -> alias)
        (alias, (dep.id, dep.location().get))
      }.toSet
      dialect.annotations += Aliases(aliasesValues)
      dialect.withReferences(deps.map(_._2))
    }

    acc
  }


  lazy protected val nodeMappings: Seq[NodeMapping] = {
    entityGroup.shapes.map { shapeId =>
      val shape = schemaModel.findShapeById(shapeId).getOrElse(throw new Exception(s"Cannot find shape '$shapeId' in Entity Group ${entityGroup.id}"))
      val nodeMapping = NodeMapping().withId(nodeMappingId(entityGroup, shape.id)).withName(shape.name).withNodeTypeMapping(shape.id)
      shape.baseSchema.headOption match {
        case Some(baseSchema) =>
          val alias = entityGroup.dependencies.find(_.shape.id == baseSchema) match {
            // external reference
            case Some(dep) =>
              nodeMappingId(dep.entityGroup, baseSchema)
            // local reference
            case _         =>
              nodeMappingId(entityGroup, baseSchema)
          }
          nodeMapping.withExtends(Seq(NodeMapping().withId(baseSchema).link(alias).asInstanceOf[NodeMapping]))
        case None => // ignore
      }

      val propertyMappings = shape.properties.map { property =>
        val propertyMapping = PropertyMapping().withName(property.name).withNodePropertyMapping(property.path)
        if (property.mandatory) {
          propertyMapping.withMinCount(1)
        }
        if (property.allowMultiple) {
          propertyMapping.withAllowMultiple(true)
        }
        property.objectRange.foreach { objectRange =>
          entityGroup.dependencies.find(_.shape.id == objectRange) match {
              // external reference
            case Some(dep) => propertyMapping.withObjectRange(Seq(nodeMappingId(dep.entityGroup, objectRange)))
              // local reference
            case _         => propertyMapping.withObjectRange(Seq(nodeMappingId(entityGroup, objectRange)))
          }

        }
        property.scalarRange.foreach { scalarRange =>
          if (scalarRange == CIM.cim("id")) {
            propertyMapping.withLiteralRange((Namespace.Xsd + "string").iri())
          } else if (scalarRange == (Namespace.Xsd + "decimal").iri()) {
            propertyMapping.withLiteralRange((Namespace.Xsd + "integer").iri())
          } else {
            propertyMapping.withLiteralRange(scalarRange)
          }
        }

        if (property.enum.nonEmpty) propertyMapping.withEnum(property.enum)

        propertyMapping
      }
      nodeMapping.withPropertiesMapping(propertyMappings)
    }
  }

  protected def nodeMappingId(entityGroup: EntityGroup, id: String): String = {
    val after = id.replace(CIM.NS.base, entityGroup.id + "/declarations/")
    after
  }

  protected def relativePaths(from: String, to: String): String = {
    Paths.get(from).getParent.relativize(Paths.get(to)).toString
  }

  protected def computeExtensions(aliases: mutable.Map[String, String], dialect: Dialect) = {
    dialect.declares.foreach { case nodeMapping: NodeMapping =>
      if (nodeMapping.extend.nonEmpty) {
        val b = nodeMapping.extend.head
        val base = b.asInstanceOf[Linkable]
        val label = base.linkLabel.value()
        val foundAlias = aliases.find { case (id, alias) =>
          label.contains(id)
        }
        foundAlias match {
          case Some((_, alias)) =>
            val finalLabel = alias + "." + label.split("/declarations/").last
            nodeMapping.withExtends(Seq(base.withLinkLabel(finalLabel).asInstanceOf[DomainElement]))
          case _          =>
            val finalLabel = label.split("/declarations/").last
            nodeMapping.withExtends(Seq(base.withLinkLabel(finalLabel).asInstanceOf[DomainElement]))
        }
      }
    }
  }
}
