// RUN: %swift -parse-stdlib -parse -verify %s

import Swift

// A type that is convertible from an inout parameter.
struct ArgPointer<T>: BuiltinInOutAddressConvertible {
  typealias _InOutType = T
  static func _convertFromInOutAddress(addr: Builtin.RawPointer) -> ArgPointer {
    return ArgPointer()
  }
}

func takeIntArgPointer(x: ArgPointer<Int>) {}
func takeStringArgPointer(x: ArgPointer<String>) {}

var mutInt = Int()
var mutStr = String()

let immInt = Int()
let immStr = String()

takeIntArgPointer(&mutInt) // expected-error{{not implemented}}
takeIntArgPointer(&immInt) // expected-error{{not a subtype of '@lvalue}}
takeIntArgPointer(&mutStr) // expected-error{{does not type-check}}
takeIntArgPointer(&immStr) // expected-error{{not a subtype of '@lvalue}}

takeStringArgPointer(&mutInt) // expected-error{{does not type-check}}
takeStringArgPointer(&immInt) // expected-error{{not a subtype of '@lvalue}}
takeStringArgPointer(&mutStr) // expected-error{{not implemented}}
takeStringArgPointer(&immStr) // expected-error{{not a subtype of '@lvalue}}

