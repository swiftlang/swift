// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify

// REQUIRES: objc_interop

import Foundation

enum FooError: HairyErrorProtocol, Runcible {
  case A

  var hairiness: Int { return 0 }

  func runce() {}
}

protocol HairyErrorProtocol : ErrorProtocol {
  var hairiness: Int { get }
}

protocol Runcible {
  func runce()
}

let foo = FooError.A
let error: ErrorProtocol = foo
let subError: HairyErrorProtocol = foo
let compo: protocol<HairyErrorProtocol, Runcible> = foo

// ErrorProtocol-conforming concrete or existential types can be coerced explicitly
// to NSError.
let ns1 = foo as NSError
let ns2 = error as NSError
let ns3 = subError as NSError
var ns4 = compo as NSError

// NSError conversion must be explicit.
// TODO: fixit to insert 'as NSError'
ns4 = compo // expected-error{{cannot assign value of type 'protocol<HairyErrorProtocol, Runcible>' to type 'NSError'}}

let e1 = ns1 as? FooError
let e1fix = ns1 as FooError // expected-error{{did you mean to use 'as!'}} {{17-19=as!}}

let esub = ns1 as ErrorProtocol
let esub2 = ns1 as? ErrorProtocol // expected-warning{{conditional cast from 'NSError' to 'ErrorProtocol' always succeeds}}
