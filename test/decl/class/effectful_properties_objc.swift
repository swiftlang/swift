// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

enum E : Error {
  case NotAvailable
}

class ProblematicObjC {
  @objc var par : Int { // expected-error{{property with 'throws' or 'async' is not representable in Objective-C}}
    get async throws { 60 }
  }

  @objc subscript(_ i : Int) -> Int { // expected-error{{subscript with 'throws' or 'async' is not representable in Objective-C}}
    get throws { throw E.NotAvailable }
  }
}