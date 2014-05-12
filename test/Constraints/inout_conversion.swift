// RUN: %swift -parse-stdlib -parse -verify %s

import Swift

// A type that is convertible from an inout parameter.
struct ArgPointer<T> {
  static func __inout_conversion(inout _: T)
  -> ArgPointer {
    return ArgPointer()
  }

  static func __inout_conversion(inout _: Float)
  -> ArgPointer {
    return ArgPointer()
  }
}

// A type that is convertible from an inout parameter through a writeback
// temporary.
struct WritebackPointer<T> {
  static func __writeback_conversion_get(x: T) -> Int {
    return reinterpretCast(x)
  }
  static func __writeback_conversion_set(x: Int) -> T {
    return reinterpretCast(x)
  }
  static func __writeback_conversion(inout _: Int)
  -> WritebackPointer {
    return WritebackPointer()
  }
}

func takeIntArgPointer(x: ArgPointer<Int>) {}
func takeStringArgPointer(x: ArgPointer<String>) {}
func takeIntWritebackPointer(x: WritebackPointer<Int>) {}
func takeStringWritebackPointer(x: WritebackPointer<String>) {}

var mutInt = Int()
var mutFlo = Float()
var mutStr = String()

let immInt = Int()
let immFlo = Float()
let immStr = String()

takeIntArgPointer(&mutInt)
takeIntArgPointer(&immInt) // expected-error{{not a subtype of '@lvalue}}
takeIntArgPointer(&mutStr) // expected-error{{cannot convert the expression's type '()' to type 'inout String'}}
takeIntArgPointer(&immStr) // expected-error{{not a subtype of '@lvalue}}
takeIntArgPointer(&mutFlo)
takeIntArgPointer(&immFlo) // expected-error{{not a subtype of '@lvalue}}

takeStringArgPointer(&mutInt) // expected-error{{cannot convert the expression's type '()' to type 'inout Int'}}
takeStringArgPointer(&immInt) // expected-error{{not a subtype of '@lvalue}}
takeStringArgPointer(&mutStr)
takeStringArgPointer(&immStr) // expected-error{{not a subtype of '@lvalue}}
takeStringArgPointer(&mutFlo)
takeStringArgPointer(&immFlo) // expected-error{{not a subtype of '@lvalue}}

takeIntWritebackPointer(&mutInt)
takeIntWritebackPointer(&mutStr) // expected-error{{cannot convert the expression's type '()' to type 'inout String'}}
takeIntWritebackPointer(&immInt) // expected-error{{not a subtype of '@lvalue}}
takeIntWritebackPointer(&immStr) // expected-error{{not a subtype of '@lvalue}}

takeStringWritebackPointer(&mutInt) // expected-error{{cannot convert the expression's type '()' to type 'inout Int'}}
takeStringWritebackPointer(&mutStr)
takeStringWritebackPointer(&immInt) // expected-error{{not a subtype of '@lvalue}}
takeStringWritebackPointer(&immStr) // expected-error{{not a subtype of '@lvalue}}
