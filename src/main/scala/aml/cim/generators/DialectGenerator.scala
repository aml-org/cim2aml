package aml.cim.generators

import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.vocabularies.model.domain.NodeMapping
import aml.cim.model.ConceptualModel
import aml.cim.model.entities.FunctionalArea

class DialectGenerator(conceptualModel: ConceptualModel, functionalArea: FunctionalArea) {

  def generate(): Dialect = ???
  lazy protected val nodeMappings: Seq[NodeMapping] = ???

}
