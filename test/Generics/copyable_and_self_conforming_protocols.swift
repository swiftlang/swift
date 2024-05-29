// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -enable-experimental-feature NonescapableTypes %s -verify

// REQUIRES: objc_interop
// REQUIRES: asserts

import Foundation

@objc protocol MyResult {
}

class Request<T : MyResult> {
}

struct Test {
  let closure: (Request<MyResult>) -> Void

  func test<R>(_ request: Request<R>) {
    self.closure(request as! Request<MyResult>)
  }
}
