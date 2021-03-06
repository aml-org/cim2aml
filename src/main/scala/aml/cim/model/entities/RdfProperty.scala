package aml.cim.model.entities

import amf.core.vocabulary.Namespace
import aml.cim.CIM

case class RdfProperty(id: String, name: String, displayName: Option[String], description: Option[String], domains: Seq[String], ranges: Seq[String]) {
  def isDataProperty: Boolean = ranges.contains(CIM.DATATYPE) || ranges.exists(_.startsWith(Namespace.Xsd.base))
}
