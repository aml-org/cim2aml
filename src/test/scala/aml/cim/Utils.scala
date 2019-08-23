package aml.cim

import org.apache.jena.rdf.model.Model

trait Utils {

  val CONTEXT_PATH = "src/test/resources/context.jsonld"

  val examples: Map[String,String] = Map(
    "concepts" -> "src/test/resources/concepts_example/concepts.jsonld",
    "schemas"  -> "src/test/resources/schemas_example/schemas.jsonld"
  )

  protected def loadExample(exampleName: String): Model = {
    JsonldLoader.fromFile(
      examples(exampleName),
      CONTEXT_PATH
    )
  }
}
