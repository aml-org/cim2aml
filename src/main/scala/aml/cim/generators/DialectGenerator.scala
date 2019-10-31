package aml.cim.generators

import java.nio.file.Paths

import amf.core.annotations.Aliases
import amf.core.model.document.BaseUnit
import amf.core.model.domain.{DomainElement, Linkable}
import amf.core.vocabulary.Namespace
import amf.plugins.document.vocabularies.model.document.{Dialect, DialectLibrary, Vocabulary}
import amf.plugins.document.vocabularies.model.domain._
import aml.cim.CIM
import aml.cim.model.SchemasModel
import aml.cim.model.entities.{ConceptualGroup, PathDependency, ShaclShape, ShapeDependency}

import scala.collection.mutable

class DialectGenerator(schemaModel: SchemasModel, conceptualGroup: ConceptualGroup, version: String, ontology: Seq[ConceptualGroup]) {

  def generate(): Dialect = {
    val dialect = Dialect().withLocation(conceptualGroup.location.get.replace("schema.jsonld", "schema.yaml")).withId(conceptualGroup.id).withName(conceptualGroup.name).withVersion(version)
    val nodes = nodeMappings

    val aliases = computeDependencies(dialect, conceptualGroup)
    dialect.withDeclares(nodes)

    computeExtensions(aliases, dialect)

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

  protected def computeSemanticDependencies(entityGroup: ConceptualGroup) = {
    ontology.find(_.id == entityGroup.id).get
  }

  protected def computeDependencies(dialect: Dialect, sourceEntityGroup: ConceptualGroup): mutable.Map[String, String] = {
    val dependentEntityGroups: mutable.Seq[ShapeDependency] = sourceEntityGroup.dependencies
    // val pahtDependentyEntityGrups: mutable.Seq[PathDependency] = sourceEntityGroup.pathDependencies
    val acc = mutable.Map[String,Seq[ShaclShape]]()
    val propAcc = mutable.Map[String,Seq[String]]()
    val entityGroupsMap = mutable.Map[String, ConceptualGroup]()

    dependentEntityGroups.foreach { shapeDependency =>
      entityGroupsMap.put(shapeDependency.entityGroup.id, shapeDependency.entityGroup)
      val shapes = acc.getOrElse(shapeDependency.entityGroup.id, Seq[ShaclShape]())
      acc.put(shapeDependency.entityGroup.id, shapes ++ Seq(shapeDependency.shape))
    }

    /*
    pahtDependentyEntityGrups.foreach { pathDependency =>
      entityGroupsMap.put(pathDependency.entityGroup.id, pathDependency.entityGroup)
      val properties = propAcc.getOrElse(pathDependency.entityGroup.id, Seq[String]())
      propAcc.put(pathDependency.entityGroup.id, properties ++ Seq(pathDependency.propertyId))
    }
    */

    val deps: Seq[(ConceptualGroup, BaseUnit)] = entityGroupsMap.map { case (egId, entityGroup) =>
      if (acc.contains(egId)) {
        val nodeMappings = acc(egId).map { shape => NodeMapping().withId(nodeMappingId(entityGroup, shape.id)).withName(shape.name) }
        val path = relativePaths(sourceEntityGroup.location.get, entityGroup.location.get).replace(".jsonld", ".yaml")
        (entityGroup, DialectLibrary().withId(entityGroup.id).withLocation(path).withDeclares(nodeMappings))
      } else {
        val props = propAcc(egId).map { propId => DatatypePropertyTerm().withId(propId) }
        val path = relativePaths(sourceEntityGroup.location.get, entityGroup.location.get).replace(".jsonld", ".yaml")
        (entityGroup, Vocabulary().withBase(CIM.NS.base).withId(entityGroup.id).withLocation(path).withDeclares(props))
      }
    }.toSeq

    dialect.withExternals(Seq(External().withBase(CIM.NS.base) withAlias("cim")))
    dialect.withReferences(deps.map(_._2))

    computeAliases(dialect, deps)
  }

  protected def computeAliases(dialect: Dialect, deps: Seq[(ConceptualGroup, BaseUnit)]): mutable.Map[String, String] = {
    var acc = mutable.Map[String,String]()

    val vocabId: String = dialect.id
    // val vocabLocation = dialect.location().get.replace("schema.yaml", "concepts.yaml")
    // val vocab = Vocabulary().withId(vocabId).withLocation(vocabLocation).withBase(CIM.NS.base)

    if (deps.nonEmpty) {
      val dialectAliasesValues = deps.map {
        case (eg: ConceptualGroup, dep: DialectLibrary) =>
          val alias = eg.name.split(" ").map(_.toLowerCase).mkString("_").replace("entitygroup", "_eg")
          acc += (dep.id -> alias)
          Some((alias, (dep.id, dep.location().get)))
        case (eg: ConceptualGroup, vocab: Vocabulary) =>
          None
          /*
          val alias = eg.name.split(" ").map(_.toLowerCase).mkString("_").replace("attribute_group", "props")
          acc += (vocab.id -> alias)
          (alias, (vocab.id, vocab.location().get))
           */
      }. collect {case Some(x) => x} .toSet

      val aliasesValues: Set[(String, (String, String))]  = dialectAliasesValues  // ++ Set(("vocab", (CIM.NS.base, "./concepts.jsonld")))
      dialect.annotations += Aliases(aliasesValues)
      dialect.withReferences(deps.map(_._2)) //  ++ Seq(vocab))
    } else {
      /*
      val aliasesValues = Set(("vocab", (CIM.NS.base, "./concepts.jsonld")))
      dialect.annotations += Aliases(aliasesValues)
      dialect.withReferences(Seq(vocab))
       */
    }



    acc
  }


  lazy protected val nodeMappings: Seq[NodeMapping] = {
    conceptualGroup.shapes.map { shapeId =>
      val shape = schemaModel.findShapeById(shapeId).getOrElse(throw new Exception(s"Cannot find shape '$shapeId' in Entity Group ${conceptualGroup.id}"))
      val nodeMapping = NodeMapping().withId(nodeMappingId(conceptualGroup, shape.id)).withName(shape.name).withNodeTypeMapping(shape.id)
      shape.baseSchema.headOption match {
        case Some(baseSchema) =>
          val alias = conceptualGroup.dependencies.find(_.shape.id == baseSchema) match {
            // external reference
            case Some(dep) =>
              nodeMappingId(dep.entityGroup, baseSchema)
            // local reference
            case _         =>
              nodeMappingId(conceptualGroup, baseSchema)
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
          conceptualGroup.dependencies.find(_.shape.id == objectRange) match {
              // external reference
            case Some(dep) => propertyMapping.withObjectRange(Seq(nodeMappingId(dep.entityGroup, objectRange)))
              // local reference
            case _         => propertyMapping.withObjectRange(Seq(nodeMappingId(conceptualGroup, objectRange)))
          }

        }
        property.scalarRange.foreach { scalarRange =>
          if (scalarRange == CIM.cim("id")) {
            propertyMapping.withLiteralRange((Namespace.Shapes + "guid").iri())
            propertyMapping.withUnique(true)
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

  protected def nodeMappingId(entityGroup: ConceptualGroup, id: String): String = {
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
