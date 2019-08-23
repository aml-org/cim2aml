package aml.cim.model.entities

case class ShaclProperty(path: String, mandatory: Boolean, objectRange: Option[String], scalarRange: Option[String])
