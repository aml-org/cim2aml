package aml.cim.model.entities

case class FunctionalArea(id: String, version: String, name: String, description: Option[String], classes: Seq[String], properties: Seq[String], shapes: Seq[String])
