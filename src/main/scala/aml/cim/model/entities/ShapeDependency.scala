package aml.cim.model.entities

import java.util.Objects

case class ShapeDependency(entityGroup: EntityGroup, shape: ShaclShape) {
  override def hashCode(): Int = {
    Objects.hash(entityGroup.id, shape.id)
  }
}
