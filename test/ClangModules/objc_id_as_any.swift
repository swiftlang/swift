// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -enable-id-as-any %s
// REQUIRES: objc_interop

import Foundation

func assertTypeIsAny(_: Any.Protocol) {}
func staticType<T>(_: T) -> T.Type { return T.self }

let idLover = IdLover()

let t1 = staticType(idLover.makesId())
assertTypeIsAny(t1)

struct ArbitraryThing {}
idLover.takesId(ArbitraryThing())

var x: AnyObject = NSObject()
idLover.takesArray(ofId: &x) // expected-error{{cannot pass immutable value as inout argument: implicit conversion from 'AnyObject' to 'id' requires a temporary}}
var xAsAny = x as Any
idLover.takesArray(ofId: &xAsAny)

var y: Any = NSObject()
idLover.takesArray(ofId: &y)

idLover.takesId(x)
idLover.takesId(y)
