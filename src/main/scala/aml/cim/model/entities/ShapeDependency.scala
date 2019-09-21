package aml.cim.model.entities

import java.util.Objects

case class ShapeDependency(entityGroup: ConceptualGroup, shape: ShaclShape) {
  override def hashCode(): Int = {
    Objects.hash(entityGroup.id, shape.id)
  }
}

case class PathDependency(entityGroup: ConceptualGroup, propertyId: String) {
  override def hashCode(): Int = {
    Objects.hash(entityGroup.id, propertyId)
  }
}
