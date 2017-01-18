// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

func assertTypeIsAny(_: Any.Protocol) {}
func staticType<T>(_: T) -> T.Type { return T.self }

let idLover = NSIdLover()

let t1 = staticType(idLover.makesId())
assertTypeIsAny(t1)

struct ArbitraryThing {}
idLover.takesId(ArbitraryThing())

var x: AnyObject = NSObject()
idLover.takesArray(ofId: &x)
var xAsAny = x as Any
idLover.takesArray(ofId: &xAsAny)  // expected-error{{argument type 'Any' does not conform to expected type 'AnyObject'}}

var y: Any = NSObject()
idLover.takesArray(ofId: &y) // expected-error{{argument type 'Any' does not conform to expected type 'AnyObject'}}

idLover.takesId(x)
idLover.takesId(y)
