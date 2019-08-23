package aml.cim.model.entities

case class RdfsClass(id: String, name: String, displayName: Option[String], description: Option[String], superClasses: Seq[String])
