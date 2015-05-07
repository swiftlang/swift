// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify

// REQUIRES: objc_interop

import Foundation

enum FooError: HairyErrorType, Runcible {
  case A

  var hairiness: Int { return 0 }

  func runce() {}
}

protocol HairyErrorType: ErrorType {
  var hairiness: Int { get }
}

protocol Runcible {
  func runce()
}

let foo = FooError.A
let error: ErrorType = foo
let subError: HairyErrorType = foo
let compo: protocol<HairyErrorType, Runcible> = foo

// ErrorType-conforming concrete or existential types can be coerced explicitly
// to NSError.
let ns1 = foo as NSError
let ns2 = error as NSError
let ns3 = subError as NSError
var ns4 = compo as NSError

// NSError conversion must be explicit.
// TODO: fixit to insert 'as NSError'
ns4 = compo // expected-error{{cannot assign a value of type 'protocol<HairyErrorType, Runcible>' to a value of type 'NSError'}}

let e1 = ns1 as? FooError
let e1fix = ns1 as FooError // expected-error{{did you mean to use 'as!'}}

let esub = ns1 as ErrorType
let esub2 = ns1 as? ErrorType // expected-warning{{conditional cast from 'NSError' to 'ErrorType' always succeeds}}
