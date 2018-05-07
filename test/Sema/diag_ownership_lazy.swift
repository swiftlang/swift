// RUN: %target-typecheck-verify-swift

class LazyReferenceClass {
  lazy unowned(unsafe) var unsafeSelf = { self }() // expected-error{{lazy properties must not have reference attributes}}
  lazy weak var weakSelf = self // expected-error{{lazy properties must not have reference attributes}}
  lazy unowned var unownedSelf = self // expected-error{{lazy properties must not have reference attributes}}
}
