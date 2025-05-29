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
idLover.takesArray(ofId: &xAsAny)  // expected-error{{cannot convert value of type 'UnsafePointer<Any>' to expected argument type 'UnsafePointer<AnyObject>'}}
// expected-note@-1 {{arguments to generic parameter 'Pointee' ('Any' and 'AnyObject') are expected to be equal}}

var y: Any = NSObject()
idLover.takesArray(ofId: &y) // expected-error{{cannot convert value of type 'UnsafePointer<Any>' to expected argument type 'UnsafePointer<AnyObject>'}}
// expected-note@-1 {{arguments to generic parameter 'Pointee' ('Any' and 'AnyObject') are expected to be equal}}

idLover.takesId(x)
idLover.takesId(y)

install_global_event_handler(idLover) // expected-error {{cannot convert value of type 'NSIdLover' to expected argument type 'event_handler' (aka '@convention(c) (Any) -> ()')}}

// FIXME: this should not type-check!
// Function conversions are not legal when converting to a thin function type.
let handler: @convention(c) (Any) -> () = { object in () }
install_global_event_handler(handler)
