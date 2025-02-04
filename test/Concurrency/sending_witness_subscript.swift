// RUN: %target-swift-frontend -emit-sil -swift-version 6 %s -o /dev/null -verify

class NonSendableKlass {}

protocol P {
  subscript(_: sending NonSendableKlass) -> sending NonSendableKlass { get }
}

struct S: P {
  subscript(_: sending NonSendableKlass) -> sending NonSendableKlass { NonSendableKlass() }
}
