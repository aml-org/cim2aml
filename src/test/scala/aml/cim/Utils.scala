package aml.cim

import org.apache.jena.rdf.model.Model

trait Utils {

  val CONTEXT_PATH = "src/test/resources/context.jsonld"

  protected def loadExample: Model = {
    JsonldLoader.fromFile(
      "src/test/resources/concepts_example/concepts.jsonld",
      CONTEXT_PATH
    )
  }
}
