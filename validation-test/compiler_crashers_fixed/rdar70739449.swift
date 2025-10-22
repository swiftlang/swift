// RUN: %target-swift-frontend -emit-module %s -experimental-skip-non-inlinable-function-bodies

struct Foo {
  var fieldWithDidSet : Int {
    didSet {
      let local = oldValue
    }
  }
}
