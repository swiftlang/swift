// Checks that we don't crash
// RUN: %sourcekitd-test -req=cursor -pos=8:19 %s -- %s | %FileCheck %s
// CHECK: source.lang.swift.ref.class

class ImageSet {
  class StandImageSet {}
  func foo() {
    /*here:*/StandImageSet()
  }
}
