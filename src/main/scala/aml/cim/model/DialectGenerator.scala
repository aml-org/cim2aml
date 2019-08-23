package aml.cim.model

import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.vocabularies.model.domain.NodeMapping

class DialectGenerator(conceptualModel: ConceptualModel, functionalArea: FunctionalArea) {

  def generate(): Dialect = ???
  lazy protected val nodeMappings: Seq[NodeMapping] = ???

}
