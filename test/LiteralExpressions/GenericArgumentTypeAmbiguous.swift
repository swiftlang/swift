// REQUIRES: swift_feature_LiteralExpressions

// A parenthesized generic argument that names a type ambiguously is diagnosed
// as an ambiguous type, once, as the unparenthesized spelling is.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name MA -o %t/MA.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name MB -o %t/MB.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t %t/Client.swift -target %target-swift-6.2-abi-triple -enable-experimental-feature LiteralExpressions

//--- Lib.swift
public struct Dup {}

//--- Client.swift
import MA
import MB

struct Box<T> {}
struct Vec<let N: Int, T> {}

typealias Bare = Box<Dup>
// expected-error@-1 {{'Dup' is ambiguous for type lookup in this context}}
typealias Paren = Box<(Dup)>
// expected-error@-1 {{'Dup' is ambiguous for type lookup in this context}}
typealias ArrayOf = Box<([Dup])>
// expected-error@-1 {{'Dup' is ambiguous for type lookup in this context}}
typealias Specialization = Box<(Box<Dup>)>
// expected-error@-1 {{'Dup' is ambiguous for type lookup in this context}}

// The generic arguments of a specialized base are shared with the value path,
// so trying the type interpretation must not leave them silently invalid.
enum Limits { static let count = 4 }
typealias GenericAlias<T> = Limits
typealias MemberOfSpecializedBase = Vec<(GenericAlias<Dup>.count), Int>
// expected-error@-1 {{'Dup' is ambiguous for type lookup in this context}}
// expected-error@-2 {{generic value must be an integer literal expression}}

// A local value does not hide the type name (SE-0531), so this is the
// ambiguous type, not the value 3.
func shadowedByLocal() {
  let Dup = 3
  _ = Dup
  let _: Vec<(Dup), Int>? = nil
  // expected-error@-1 {{'Dup' is ambiguous for type lookup in this context}}
}
