// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/constrained_existentials -emit-module -Xfrontend -disable-availability-checking
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/constrained_existentials -type-from-mangled=%t/input | %FileCheck %s --match-full-lines

func blackHole(_: Any...) {}
func blackHole_noncopyable(_: consuming any ~Copyable) {}

protocol BaseProto<A, B> {
  associatedtype A
  associatedtype B
}

protocol DerivedProto<A, B>: BaseProto {}

protocol OtherProto {}

struct S<A, B>: DerivedProto, OtherProto {}

// We should lift the artificial ban on compositions involving constrained protocol types
typealias BaseProtoIntStringAndOtherProto = BaseProto<Int, String> & OtherProto

do {
  let e0: any BaseProto<Int, String> = S<Int, String>()
  let e1: any BaseProto<Int, String>.Type = S<Int, String>.self
  let e2: (any BaseProto<Int, String>).Type = (any BaseProto<Int, String>).self

  blackHole(e0, e1, e2)
}

// DEMANGLE: $s24constrained_existentials9BaseProto_pSi1AAaBPRts_SS1BADRtsXPD
// DEMANGLE: $s24constrained_existentials9BaseProto_pSi1AAaBPRts_SS1BADRtsXPXmTD
// DEMANGLE: $s24constrained_existentials9BaseProto_pSi1AAaBPRts_SS1BADRtsXPXMtD

// CHECK: any BaseProto<Int, String>
// CHECK: @thick any BaseProto<Int, String>.Type
// CHECK: @thin (any BaseProto<Int, String>).Type

do {
  let e0: any DerivedProto<Int, String> = S<Int, String>()
  let e1: any DerivedProto<Int, String>.Type = S<Int, String>.self
  let e2: (any DerivedProto<Int, String>).Type = (any DerivedProto<Int, String>).self

  blackHole(e0, e1, e2)
}

protocol NCProto: ~Copyable {}
struct NC: ~Copyable {}
struct GenNC<T: ~Copyable>: ~Copyable, NCProto {}

do {
  let e0: any NCProto & ~Copyable = GenNC<NC>()
  let e1: any NCProto & ~Copyable = GenNC<String>()

  // FIXME: breaks the MoveChecker (rdar://129885532)
//   blackHole_noncopyable(consume e0)
//   blackHole_noncopyable(consume e1)
}

// Constrained existential compositions where an inherited protocol is listed
// explicitly alongside a constraint on one of its primary associated types.
// The mangled base type canonicalizes away the redundant inherited member,
// but the same-type constraint on its primary associated type survives in
// the requirement list, so reconstruction must recover the inherited member
// to claim it. (https://github.com/swiftlang/swift/issues/87358)
//
// The mangled names below are compiler-generated via...
//
//   swiftc -emit-ir -g -module-name constrained_existentials \
//     -Xfrontend -disable-availability-checking \
//     test/TypeDecoder/constrained_existentials.swift

protocol Base<T> {
  associatedtype T
}

protocol Derived: Base {}

struct SDerived<T>: Derived {}

do {
  let e0: any Derived & Base<Int> = SDerived<Int>()
  let e1: any (Derived & Base<Int>).Type = SDerived<Int>.self
  let e2: (any Derived & Base<Int>).Type = (any Derived & Base<Int>).self

  blackHole(e0, e1, e2)
}

// DEMANGLE: _$s24constrained_existentials7Derived_pSi1TAA4BasePRts_XPD
// DEMANGLE: _$s24constrained_existentials7Derived_pSi1TAA4BasePRts_XPXmTD
// DEMANGLE: _$s24constrained_existentials7Derived_pSi1TAA4BasePRts_XPXMtD

// CHECK: any Derived & Base<Int>
// CHECK: @thick any (Derived & Base<Int>).Type
// CHECK: @thin (any Derived & Base<Int>).Type

// Same shape, but the inherited base being recovered is one of two siblings
// that both refine a common ancestor with two primary associated types; only
// one sibling's associated type is actually constrained.
protocol BaseProtocolUT {
  associatedtype U
  associatedtype T
}

protocol ProtocolGenericU<U>: BaseProtocolUT {}
protocol ProtocolGenericT<T>: BaseProtocolUT {}
protocol FullProtocol<U, T>: ProtocolGenericU, ProtocolGenericT {}

struct SFull<U, T>: FullProtocol {}

do {
  let e0: any FullProtocol & ProtocolGenericU<Int> = SFull<Int, String>()
  let e1: any (FullProtocol & ProtocolGenericU<Int>).Type = SFull<Int, String>.self
  let e2: (any FullProtocol & ProtocolGenericU<Int>).Type =
    (any FullProtocol & ProtocolGenericU<Int>).self

  blackHole(e0, e1, e2)
}

// DEMANGLE: _$s24constrained_existentials12FullProtocol_pSi1UAA04BaseD2UTPRts_XPD
// DEMANGLE: _$s24constrained_existentials12FullProtocol_pSi1UAA04BaseD2UTPRts_XPXmTD
// DEMANGLE: _$s24constrained_existentials12FullProtocol_pSi1UAA04BaseD2UTPRts_XPXMtD

// CHECK: any FullProtocol & ProtocolGenericU<Int>
// CHECK: @thick any (FullProtocol & ProtocolGenericU<Int>).Type
// CHECK: @thin (any FullProtocol & ProtocolGenericU<Int>).Type

do {
  let e0: any FullProtocol & ProtocolGenericT<String> = SFull<Int, String>()
  let e1: any (FullProtocol & ProtocolGenericT<String>).Type = SFull<Int, String>.self
  let e2: (any FullProtocol & ProtocolGenericT<String>).Type =
    (any FullProtocol & ProtocolGenericT<String>).self

  blackHole(e0, e1, e2)
}

// DEMANGLE: _$s24constrained_existentials12FullProtocol_pSS1TAA04BaseD2UTPRts_XPD
// DEMANGLE: _$s24constrained_existentials12FullProtocol_pSS1TAA04BaseD2UTPRts_XPXmTD
// DEMANGLE: _$s24constrained_existentials12FullProtocol_pSS1TAA04BaseD2UTPRts_XPXMtD

// CHECK: any FullProtocol & ProtocolGenericT<String>
// CHECK: @thick any (FullProtocol & ProtocolGenericT<String>).Type
// CHECK: @thin (any FullProtocol & ProtocolGenericT<String>).Type

// Multiple inherited protocols get canonicalized away at once; reconstruction
// must recover all of them and re-add them in their original source order.
protocol HopBase {
  associatedtype Parent
  associatedtype Successor
  associatedtype Destination
}

protocol HopGenericParent<Parent>: HopBase {}
protocol HopGenericSuccessor<Successor>: HopBase {}
protocol HopGenericDestination<Destination>: HopBase {}
protocol Hop<Parent, Successor>:
  HopGenericParent, HopGenericSuccessor, HopGenericDestination {}

struct SHop<Parent, Successor, Destination>: Hop {}

do {
  let e0: any Hop & HopGenericSuccessor<Int> & HopGenericDestination<String> =
    SHop<Bool, Int, String>()
  let e1: any (Hop & HopGenericSuccessor<Int> & HopGenericDestination<String>).Type =
    SHop<Bool, Int, String>.self
  let e2: (any Hop & HopGenericSuccessor<Int> & HopGenericDestination<String>).Type =
    (any Hop & HopGenericSuccessor<Int> & HopGenericDestination<String>).self

  blackHole(e0, e1, e2)
}

// DEMANGLE: _$s24constrained_existentials3Hop_pSS11DestinationAA0C4BasePRts_Si9SuccessorAERtsXPD
// DEMANGLE: _$s24constrained_existentials3Hop_pSS11DestinationAA0C4BasePRts_Si9SuccessorAERtsXPXmTD
// DEMANGLE: _$s24constrained_existentials3Hop_pSS11DestinationAA0C4BasePRts_Si9SuccessorAERtsXPXMtD

// CHECK: any Hop & HopGenericSuccessor<Int> & HopGenericDestination<String>
// CHECK: @thick any (Hop & HopGenericSuccessor<Int> & HopGenericDestination<String>).Type
// CHECK: @thin (any Hop & HopGenericSuccessor<Int> & HopGenericDestination<String>).Type

// The most-specific recoverable inherited protocol ("Full") needs both Root
// and Value to parameterize, but only Root is actually constrained here, so
// reconstruction must fall back to the less-specific "GenericRoot" instead.
protocol TwoFieldBase {
  associatedtype Root
  associatedtype Value
}

protocol GenericRoot<Root>: TwoFieldBase {}
protocol GenericValue<Value>: TwoFieldBase {}
protocol Full<Root, Value>: GenericRoot, GenericValue {}
protocol Writable<Root, Value>: Full {}

struct SWritable<Root, Value>: Writable {}

do {
  let e0: any Writable & GenericRoot<Int> = SWritable<Int, String>()
  let e1: any (Writable & GenericRoot<Int>).Type = SWritable<Int, String>.self
  let e2: (any Writable & GenericRoot<Int>).Type = (any Writable & GenericRoot<Int>).self

  blackHole(e0, e1, e2)
}

// DEMANGLE: _$s24constrained_existentials8Writable_pSi4RootAA12TwoFieldBasePRts_XPD
// DEMANGLE: _$s24constrained_existentials8Writable_pSi4RootAA12TwoFieldBasePRts_XPXmTD
// DEMANGLE: _$s24constrained_existentials8Writable_pSi4RootAA12TwoFieldBasePRts_XPXMtD

// CHECK: any Writable & GenericRoot<Int>
// CHECK: @thick any (Writable & GenericRoot<Int>).Type
// CHECK: @thin (any Writable & GenericRoot<Int>).Type
