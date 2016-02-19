package io.buoyant.linkerd

import com.fasterxml.jackson.databind.ObjectMapper

trait ConfigInitializer {
  def registerSubtypes(mapper: ObjectMapper): Unit
}
