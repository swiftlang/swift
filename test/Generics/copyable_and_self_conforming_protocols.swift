// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck  %s -verify

// REQUIRES: objc_interop

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
