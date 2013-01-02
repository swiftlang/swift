// RUN: %swift %s -verify -parse-as-library

//===----------------------------------------------------------------------===//
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

import Builtin

//===----------------------------------------------------------------------===//
// Standardized aliases
//===----------------------------------------------------------------------===//
typealias Void = ()
typealias Int  = Int64
typealias UInt = UInt64

// FIXME/TBD: Consider adding "int", "double", etc as aliases for Int/Double.
// They violate the naming convention but lower the barrier to entry.

//===----------------------------------------------------------------------===//
// Default types for unconstrained number literals
//===----------------------------------------------------------------------===//
typealias IntegerLiteralType = Int
typealias FloatLiteralType = Double
typealias CharacterLiteralType = Char

//===----------------------------------------------------------------------===//
// Objective-C interactions
//===----------------------------------------------------------------------===//

// This violates the naming convention but looks really wrong as Id.
struct id {
  var value : Builtin.ObjCPointer
}

//===----------------------------------------------------------------------===//
// Standard protocols
//===----------------------------------------------------------------------===//

typealias Any = protocol<>

protocol Identifiable {
  func [infix=160] ===(lhs : This, rhs : This) -> Bool
  func [infix=160] !==(lhs : This, rhs : This) -> Bool
}

protocol Equatable {
  func [infix=160] ==(lhs : This, rhs : This) -> Bool
  func [infix=160] !=(lhs : This, rhs : This) -> Bool
}

protocol Comparable : Equatable {
  func [infix=170] <(lhs : This, rhs : This) -> Bool
  func [infix=170] <=(lhs : This, rhs : This) -> Bool
  func [infix=170] >=(lhs : This, rhs : This) -> Bool
  func [infix=170] >(lhs : This, rhs : This) -> Bool
}

protocol Hashable : Equatable {
  func hash() -> UInt
}
