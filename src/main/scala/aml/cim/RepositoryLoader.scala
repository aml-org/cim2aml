package aml.cim

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}

import amf.client.AMF
import amf.client.model.document.{Dialect, Vocabulary}
import amf.client.render.Aml10Renderer
import amf.core.unsafe.PlatformSecrets
import aml.cim.generators.{DialectGenerator, VocabularyGenerator}
import aml.cim.model.{ConceptualModel, SchemasModel}
import org.apache.jena.rdf.model.{Model, ModelFactory}

import scala.collection.JavaConverters._

object RepositoryLoader extends PlatformSecrets {

  def fromDirectory(path: String, contextPath: String) = {
    var files = Files.walk(Paths.get(path)).iterator().asScala
    val conceptFiles = files.filter(f => f.endsWith("concepts.jsonld"))
    files = Files.walk(Paths.get(path)).iterator().asScala
    val schemaFiles = files.filter(f => f.endsWith("schema.jsonld"))
    conceptFiles.foreach { f =>
      processConceptFile(f, contextPath)
    }

    var model = ModelFactory.createDefaultModel()
    schemaFiles.foreach { f =>
      println(s"*** Loading schema file JSON ${f.toFile.getAbsolutePath}")
      val jsonld = loadSchemaRDFModel(f.toFile.getAbsolutePath, contextPath)
      model = model.union(jsonld)
    }
    processSchemaFiles(model)
  }

  protected def processConceptFile(f: Path, contextPath: String) = {
    println(s"*** Loading concept file ${f.toFile.getAbsolutePath}")
    val jsonld = JsonldLoader.fromFile(f.toFile.getAbsolutePath, contextPath)
    val conceptualModel = new ConceptualModel(jsonld)
    val entityGroup = conceptualModel.entityGroups.head
    val vocabulary = new VocabularyGenerator(conceptualModel, entityGroup).generate()

    val generated = new Aml10Renderer("application/yaml").generateString(new Vocabulary(vocabulary)).get().trim
    val targetPath = f.toFile.getAbsolutePath.replace("concepts.jsonld", "concepts.yaml")

    writeFile(targetPath, generated)
  }

  protected def processSchemaFiles(model: Model) = {
    val schemaModel = new SchemasModel(model)
    schemaModel.linkEntityGroups
    schemaModel.entityGroups.foreach { entityGroup =>
      val location = entityGroup.location.getOrElse(throw new Exception("Cannot generate AML dialect without location for the CIM schema file"))
      println(s"*** Loading schema file $location")

      val dialect = new DialectGenerator(schemaModel, entityGroup, "1.0").generate()

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

  protected def loadSchemaRDFModel(path: String, contextPath: String): Model = {
    val jsonld = JsonldLoader.fromFile(path, contextPath)
    new SchemasModel(jsonld).entityGroupId map { entityGroupId =>
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
    if (args.length != 1) {
      println("Path to a directory containing CIM files must be provided as an argument")
      System.exit(1)
    }
    val path = args(0)
    println(s"\n\nProcessing directory $path\n\n")
    fromDirectory(path, "src/test/resources/context.jsonld")
  }

}
