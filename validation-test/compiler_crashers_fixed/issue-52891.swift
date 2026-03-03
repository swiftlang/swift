// RUN: %target-swift-frontend -emit-ir %s
// REQUIRES: objc_interop

import Foundation

class C {
  @objc func foo() -> String! { return nil }
}

func bar(_ x: AnyObject) {
  let y: String = x.foo!()
}
