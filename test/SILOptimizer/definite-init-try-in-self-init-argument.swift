// RUN: %target-swift-frontend -emit-sil -verify %s

class Y: X {
  required init(_: Z) throws {
    try super.init(Z())
  }
}
class Z { init() throws {} }

class X {
  required init(_: Z) throws {}
}
