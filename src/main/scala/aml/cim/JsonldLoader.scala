package aml.cim

import java.nio.charset.Charset

import amf.core.unsafe.PlatformSecrets
import io.circe._
import io.circe.syntax._
import org.apache.commons.io.IOUtils
import org.apache.jena.rdf.model.{Model, ModelFactory}

/**
 * Utility to load raw JSON-LD models
 */
object JsonldLoader extends PlatformSecrets {

  /**
   * Loads a JSON-LD model from a file in the provided path
   * @param path
   * @return
   */
  def fromFile(path: String): Model = {
    val cs = platform.fs.syncFile(path).read()
    val text = cs.subSequence(0, cs.length()).toString
    loadModel(text)
  }

  /**
   * Loads a JSON-LD model from a file in a path, with the context provided.
   * It overwrites the context in the source file.
   * It is expecting a top-level JSON object.
   * @param path
   * @param contextPath
   * @return
   */
  def fromFile(path: String, contextPath: String): Model = {
    val cs = platform.fs.syncFile(path).read()
    val text = cs.subSequence(0, cs.length()).toString

    val csContext = platform.fs.syncFile(contextPath).read()
    val textContext = csContext.subSequence(0, csContext.length()).toString

    val finalJson = for {
      json <- io.circe.parser.parse(text)
      jsonContext <- io.circe.parser.parse(textContext)
    } yield {
      replaceContext(json, jsonContext)
    }

    finalJson match {
      case Left(e)     => throw(new Exception(s"""Error parsing JSON from file $path and context $contextPath""", e))
      case Right(json) => loadModel(json)
    }
  }

  protected def replaceContext(json: Json, jsonContext: Json): String = {
    val replaced: Option[Json] = for {
      obj <- json.asObject
    } yield {
      obj.add("@context", jsonContext).asJson
    }
    replaced.map(_.toString()).getOrElse(json.toString())
  }

  protected def loadModel(data: String, base: String = "http://cim2aml.org/"): Model = {
    val dataModel = ModelFactory.createDefaultModel()
    dataModel.read(IOUtils.toInputStream(data, Charset.defaultCharset()), base, "JSONLD")
    dataModel
  }
}
