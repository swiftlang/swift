// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

class Obj: NSObject {
}

class Container {
  var objects: [Obj]
  init(objects: [Obj]) {}
}

func test(other: Container) {
  _ = Container(objects: other)
  // expected-error@-1 {{cannot convert value of type 'Container' to expected argument type '[Obj]'}}
}
