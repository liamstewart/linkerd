package io.l5d.experimental

import org.scalatest.FunSuite
import scala.io.Source

class K8sTest extends FunSuite {

  test("sanity") {
    // ensure it doesn't totally blowup
    new k8s().newNamer()
  }
}
