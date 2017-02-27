// RUN: %target-swift-frontend %s -emit-ir

class Base {
  init<S: Sequence>(_ s: S) where S.Iterator.Element == UInt8 { }
}

class Sub: Base {
  init(_ b: [UInt8]) { super.init(b) }
}