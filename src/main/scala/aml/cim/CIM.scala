package aml.cim

import amf.core.vocabulary.Namespace

object CIM {

  val NS: Namespace = Namespace("http://cim.org/model/")
  val DATATYPE: String = cim("DataType")
  val ENTITY_GROUP: String = cim("EntityGroup")
  val VERSION: String = cim("version")
  val CLASSES: String = cim("classes")
  val PROPERTIES: String = cim("properties")
  val SCHEMAS: String = cim("schemas")

  val LOCATED: String = cim("filePath")
  
  def cim(property: String): String = (NS + property).iri()

}
