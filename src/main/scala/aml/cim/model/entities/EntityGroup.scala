package aml.cim.model.entities

case class EntityGroup(id: String, name: String, description: Option[String], classes: Seq[String], properties: Seq[String], shapes: Seq[String])
