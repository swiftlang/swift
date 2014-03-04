// RUN: %target-run-simple-swift | FileCheck %s

class C {}

struct Foo: _ObjCBridgeable {
  func _bridge() -> AnyObject {
    return C()
  }
}

// CHECK: ok
if _bridge(Foo()) as C {
  println("ok")
}
