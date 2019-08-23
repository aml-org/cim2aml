package aml.cim

import org.apache.jena.rdf.model.Model

trait Utils {

  protected def loadExample: Model = {
    JsonldLoader.fromFile(
      "src/test/resources/concepts_example/concepts.jsonld",
      "src/test/resources/concepts_example/context.jsonld"
    )
  }
}
