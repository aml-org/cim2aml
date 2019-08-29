package aml.cim

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}

import amf.client.AMF
import amf.client.model.document.{Dialect, Vocabulary}
import amf.client.render.Aml10Renderer

import scala.collection.JavaConverters._
import amf.core.unsafe.PlatformSecrets
import aml.cim.generators.{DialectGenerator, VocabularyGenerator}
import aml.cim.model.{ConceptualModel, SchemasModel}

import scala.concurrent.Await

object RepositoryLoader extends PlatformSecrets {

  def fromDirectory(path: String, contextPath: String) = {
    var files = Files.walk(Paths.get(path)).iterator().asScala
    val conceptFiles = files.filter(f => f.endsWith("concepts.jsonld"))
    files = Files.walk(Paths.get(path)).iterator().asScala
    val schemaFiles = files.filter(f => f.endsWith("schema.jsonld"))
    conceptFiles.foreach { f =>
      processConceptFile(f, contextPath)
    }
    schemaFiles.foreach { f =>
      processSchemaFile(f, contextPath)
    }
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

  protected def processSchemaFile(f: Path, contextPath: String) = {
    println(s"*** Loading schema file ${f.toFile.getAbsolutePath}")
    val jsonld = JsonldLoader.fromFile(f.toFile.getAbsolutePath, contextPath)
    val schemaModel = new SchemasModel(jsonld)
    val entityGroup = schemaModel.entityGroups.head
    val dialect = new DialectGenerator(schemaModel, entityGroup, "1.0").generate()

    val generated = new Aml10Renderer("application/yaml").generateString(new Dialect(dialect)).get().trim
    val targetPath = f.toFile.getAbsolutePath.replace("schema.jsonld", "schema.yaml")

    writeFile(targetPath, generated)
  }

  protected def writeFile(filename: String, text: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
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
