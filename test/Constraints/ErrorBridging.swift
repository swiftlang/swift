// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify

// REQUIRES: objc_interop

import Foundation

enum FooError: HairyError, Runcible {
  case A

  var hairiness: Int { return 0 }

  func runce() {}
}

protocol HairyError : Error {
  var hairiness: Int { get }
}

protocol Runcible {
  func runce()
}

let foo = FooError.A
let error: Error = foo
let subError: HairyError = foo
let compo: protocol<HairyError, Runcible> = foo

// Error-conforming concrete or existential types can be coerced explicitly
// to NSError.
let ns1 = foo as NSError
let ns2 = error as NSError
let ns3 = subError as NSError
var ns4 = compo as NSError

// NSError conversion must be explicit.
// TODO: fixit to insert 'as NSError'
ns4 = compo // expected-error{{cannot assign value of type 'protocol<HairyError, Runcible>' to type 'NSError'}}

let e1 = ns1 as? FooError
let e1fix = ns1 as FooError // expected-error{{did you mean to use 'as!'}} {{17-19=as!}}

let esub = ns1 as Error
let esub2 = ns1 as? Error // expected-warning{{conditional cast from 'NSError' to 'Error' always succeeds}}
