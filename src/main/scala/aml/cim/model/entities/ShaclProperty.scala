package aml.cim.model.entities

case class ShaclProperty(name: String, path: String, mandatory: Boolean, allowMultiple: Boolean, objectRange: Option[String], scalarRange: Option[String], enum: Seq[String])
