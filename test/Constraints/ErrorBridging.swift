// RUN: %target-swift-frontend %clang-importer-sdk -typecheck %s -verify

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
let compo: HairyError & Runcible = foo

// Error-conforming concrete or existential types can be coerced explicitly
// to NSError.
let ns1 = foo as NSError
let ns2 = error as NSError
let ns3 = subError as NSError
var ns4 = compo as NSError

// NSError conversion must be explicit.
// TODO: fixit to insert 'as NSError'
ns4 = compo // expected-error{{cannot assign value of type 'HairyError & Runcible' to type 'NSError'}}

let e1 = ns1 as? FooError
let e1fix = ns1 as FooError // expected-error{{did you mean to use 'as!'}} {{17-19=as!}}

let esub = ns1 as Error
let esub2 = ns1 as? Error // expected-warning{{conditional cast from 'NSError' to 'Error' always succeeds}}

// SR-1562 / rdar://problem/26370984
enum MyError : Error {
  case failed
}

func concrete1(myError: MyError) -> NSError {
  return myError as NSError
}

func concrete2(myError: MyError) -> NSError {
  return myError // expected-error{{cannot convert return expression of type 'MyError' to return type 'NSError'}}
}

func generic<T : Error>(error: T) -> NSError {
  return error as NSError
}

extension Error {
  var asNSError: NSError {
    return self as NSError
  }

  var asNSError2: NSError {
    return self // expected-error{{cannot convert return expression of type 'Self' to return type 'NSError'}}
  }
}

// rdar://problem/27543121
func throwErrorCode() throws {
  throw FictionalServerError.meltedDown // expected-error{{thrown error code type 'FictionalServerError.Code' does not conform to 'Error'; construct an 'FictionalServerError' instance}}{{29-29=(}}{{40-40=)}}
}
