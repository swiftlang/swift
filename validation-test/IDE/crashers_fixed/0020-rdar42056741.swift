// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -code-completion-token=COMPLETE -source-filename=%s
// REQUIRES: objc_interop

import Foundation

class A {
  static var `default` = A()

  func foo(arg: String) -> Bool {
    return false
  }

  func foo(arg: String, _ flag: UnsafeMutablePointer<ObjCBool>?) -> Bool {
    return true
  }
}

class B {
  var bar: Bool = false
  func baz() {
    bar = A.default.foo(arg: self.#^COMPLETE^#)
  }
}
