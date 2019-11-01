package aml.cim

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}

import amf.client.AMF
import amf.client.model.document.{Dialect, Vocabulary}
import amf.client.render.Aml10Renderer
import amf.core.unsafe.PlatformSecrets
import aml.cim.generators.{DialectGenerator, VocabularyGenerator}
import aml.cim.model.entities.ConceptualGroup
import aml.cim.model.{ConceptualModel, SchemasModel}
import org.apache.jena.rdf.model.{Model, ModelFactory}

import scala.collection.JavaConverters._

object RepositoryLoader extends PlatformSecrets {

  var globalConceptualModel = ModelFactory.createDefaultModel()
  var globalSchemaModel = ModelFactory.createDefaultModel()

  def generateGlobalFiles(path: String) = {
    // vocabulary
    var targetPath = new File(path + File.separator + "concepts.yaml").getAbsolutePath
    val conceptualModel = new ConceptualModel(globalConceptualModel)
    val globalConceptualGroup = conceptualModel.globalConceptualGroup(targetPath)
    val vocabulary = new VocabularyGenerator(conceptualModel, globalConceptualGroup).generate()
    var generated = new Aml10Renderer("application/yaml").generateString(new Vocabulary(vocabulary)).get().trim
    writeFile(targetPath, generated)

    // schema
    val ontology = Seq(globalConceptualGroup)
    targetPath = new File(path + File.separator + "schema.yaml").getAbsolutePath
    val schemaModel = new SchemasModel(globalSchemaModel, ontology)
    val dialect = new DialectGenerator(schemaModel, schemaModel.globalEntityGroup(targetPath), "0.1", ontology).generate()
    generated = new Aml10Renderer("application/yaml").generateString(new Dialect(dialect)).get().trim
    writeFile(targetPath, generated)
  }

  def fromDirectory(path: String, contextPath: String) = {
    globalConceptualModel = ModelFactory.createDefaultModel()
    globalSchemaModel = ModelFactory.createDefaultModel()

    var files = Files.walk(Paths.get(path)).iterator().asScala
    val conceptFiles = files.filter(f => f.endsWith("concepts.jsonld"))
    files = Files.walk(Paths.get(path)).iterator().asScala
    val schemaFiles = files.filter(f => f.endsWith("schema.jsonld"))
    val ontology = conceptFiles.toList.map { f =>
      println(s"*** Loading vocabulary file JSON ${f.toFile.getAbsolutePath}")
      processConceptFile(f, contextPath)
    }

    schemaFiles.foreach { f =>
      println(s"*** Loading schema file JSON ${f.toFile.getAbsolutePath}")
      val jsonld = loadSchemaRDFModel(f.toFile.getAbsolutePath, contextPath,ontology)
      globalSchemaModel = globalSchemaModel.union(jsonld)
    }
    processSchemaFiles(globalSchemaModel, ontology)

    generateGlobalFiles(path)
  }

  protected def processConceptFile(f: Path, contextPath: String) = {
    println(s"*** Loading concept file ${f.toFile.getAbsolutePath}")
    val fullPath = f.toFile.getAbsolutePath
    val jsonld = JsonldLoader.fromFile(fullPath, contextPath)
    val conceptualModel = new ConceptualModel(jsonld)
    globalConceptualModel = globalConceptualModel.union(jsonld)
    val entityGroup = conceptualModel.conceptualGroups.head.copy(location = Some(fullPath))
    val vocabulary = new VocabularyGenerator(conceptualModel, entityGroup).generate()

    val generated = new Aml10Renderer("application/yaml").generateString(new Vocabulary(vocabulary)).get().trim
    val targetPath = f.toFile.getAbsolutePath.replace("concepts.jsonld", "concepts.yaml")

    writeFile(targetPath, generated)

    entityGroup
  }

  protected def processSchemaFiles(unionModel: Model, ontology: Seq[ConceptualGroup]) = {
    val schemaModel = new SchemasModel(unionModel, ontology)
    schemaModel.linkEntityGroups
    schemaModel.entityGroups.foreach { entityGroup =>
      val location = entityGroup.location.getOrElse(throw new Exception("Cannot generate AML dialect without location for the CIM schema file"))
      println(s"*** Loading schema file $location")

      val dialect = new DialectGenerator(schemaModel, entityGroup, "0.1", ontology).generate()

      val generated = new Aml10Renderer("application/yaml").generateString(new Dialect(dialect)).get().trim
      val targetPath = location.replace("schema.jsonld", "schema.yaml")

      writeFile(targetPath, generated)
    }
  }

  protected def writeFile(filename: String, text: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }

  protected def loadSchemaRDFModel(path: String, contextPath: String, ontology: Seq[ConceptualGroup]): Model = {
    val jsonld = JsonldLoader.fromFile(path, contextPath)
    new SchemasModel(jsonld, ontology).entityGroupId map { entityGroupId =>
      jsonld.add(
        jsonld.createResource(entityGroupId),
        jsonld.createProperty(CIM.LOCATED),
        jsonld.createLiteral(path)
      )
    }
    jsonld
  }


  def main(args: Array[String]) = {
    AMF.init().get()
    if (args.length != 2) {
      println("Path to a directory containing CIM files and to the context.jsonld file must be provided as an argument")
      System.exit(1)
    }
    val path = args(0)
    val context = args(1)
    println(s"\n\nProcessing directory $path\n\n")
    println(s"\n\nProcessing context at $context\n\n")
    fromDirectory(path, context)
  }

}
