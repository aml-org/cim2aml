package aml.cim.model.entities

import scala.collection.mutable

case class EntityGroup(id: String,
                       name: String,
                       description: Option[String],
                       classes: Seq[String],
                       properties: Seq[String],
                       shapes: Seq[String],
                       location: Option[String] = None,
                       dependencies: mutable.ListBuffer[ShapeDependency] = mutable.ListBuffer())