package aml.cim

import amf.core.vocabulary.Namespace

object CIM {

  val NS: Namespace = Namespace("http://cim.org/model/")
  val DATATYPE: String = cim("DataType")
  val FUNCTIONAL_AREA: String = cim("FunctionalArea")
  val VERSION: String = cim("version")
  val CLASSES: String = cim("classes")
  val PROPERTIES: String = cim("properties")
  val SCHEMAS: String = cim("schemas")

  def cim(property: String): String = (NS + property).iri()

}
