// RUN: %target-swift-frontend -typecheck %s -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -enable-experimental-feature CxxConcreteTemplateTypes -verify -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}concrete-template-types%{fs-sep}types.h
// REQUIRES: swift_feature_CxxConcreteTemplateTypes

import ConcreteTemplateTypes

// The pointer and void specializations have no C++ aliases.
func pointer(_ value: concrete.Result<UnsafeMutableRawPointer>) -> concrete.Result<UnsafeMutableRawPointer> { value }
func empty(_ value: concrete.Result<Void>) -> concrete.Result<Void> { value }
func immutable(_ value: concrete.Result<UnsafeRawPointer>) -> concrete.Result<UnsafeRawPointer> { value }
func indirect(_ value: concrete.Result<UnsafeMutablePointer<UnsafeMutableRawPointer>>) {}
func integers(_ value: concrete.Result<UnsafePointer<CInt>>) {}
func nested(_ value: concrete.Result<concrete.Result<UnsafeMutableRawPointer>>) {}
func redeclared(_ value: concrete.Result<CShort>) {}
func record(_ value: concrete.Result<concrete.Token>) {}
func lazy(_ value: concrete.Lazy<CInt>) {}
func partial(_ value: concrete.Partial<UnsafeMutablePointer<CInt>>) {}
func defaulted(_ value: concrete.Pair<UnsafeMutableRawPointer, CInt>) {}

typealias PointerResult = concrete.Result<UnsafeMutableRawPointer>
typealias EmptyResult = concrete.Result<Void>
typealias RawPointerAlias = UnsafeMutableRawPointer
func aliasedArgument(_ value: concrete.Result<RawPointerAlias>) -> PointerResult { value }
func cxxAliases(_ value: concrete.IntResult) -> concrete.Result<CInt> { value }
func otherAlias(_ value: concrete.Result<CInt>) -> concrete.OtherIntResult { value }
func same<T>(_ x: T, _ y: T) {}

func checkInferredValues() {
  _ = pointer(concrete.makePointer())
  _ = empty(concrete.makeVoid())
  _ = immutable(concrete.makeConstPointer())
  indirect(concrete.makePointerPointer())
  integers(concrete.makeIntPointer())
  nested(concrete.makeNested())
  redeclared(concrete.makeShort())
  record(concrete.makeToken())
  lazy(concrete.makeLazy())
  partial(concrete.makePartial())
  same(cxxAliases(concrete.makeInt()), otherAlias(concrete.makeInt()))
}

func unavailable(_ value: concrete.Result<CFloat>) {} // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Result' matches these type arguments}}
func absent(_ value: concrete.Result<CBool>) {} // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Result' matches these type arguments}}
func hidden(_ value: concrete.Result<CDouble>) {} // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Result' matches these type arguments}}
func incomplete(_ value: concrete.Poison<CInt>) {} // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Poison' matches these type arguments}}
func unmentioned(_ value: concrete.Poison<CShort>) {} // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Poison' matches these type arguments}}
func generic<T>(_ value: concrete.Result<T>) {} // expected-error {{type 'T' cannot be used to name a C++ class template specialization}}
func optional(_ value: concrete.Result<UnsafeMutableRawPointer?>) {} // expected-error {{cannot be used to name a C++ class template specialization}}
struct Native {}
func native(_ value: concrete.Result<Native>) {} // expected-error {{type 'Native' cannot be used to name a C++ class template specialization}}
enum Shadow {
  struct UnsafeMutableRawPointer {}
  func shadow(_ value: concrete.Result<UnsafeMutableRawPointer>) {} // expected-error {{cannot be used to name a C++ class template specialization}}
}
func nonType(_ value: concrete.NonType<CInt>) {} // expected-error {{no complete, visible, explicitly declared C++ specialization of 'NonType' matches these type arguments}}
func pack(_ value: concrete.Pack<CInt>) {} // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Pack' matches these type arguments}}
func missingDefault(_ value: concrete.Pair<UnsafeMutableRawPointer>) {} // expected-error {{specialized with too few type parameters}}
typealias Unbound = concrete.Result // expected-error {{requires explicit concrete type arguments}}
extension concrete.Result {} // expected-error {{requires explicit concrete type arguments}}
func differentNamespace(_ value: other.Result<UnsafeMutableRawPointer>) {
  _ = pointer(value) // expected-error {{cannot convert value of type}}
}
func differentConstness(_ value: concrete.Result<UnsafeRawPointer>) {
  _ = pointer(value) // expected-error {{cannot convert value of type}}
}

// Generic syntax must not start working because Swift used an inferred value.
func inferredBeforeName() {
  _ = concrete.makeImplicit()
  let _: concrete.Implicit<CInt>? = nil // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Implicit' matches these type arguments}}
}
func nameBeforeInferred(_ value: concrete.Implicit<CInt>) { // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Implicit' matches these type arguments}}
  _ = concrete.makeImplicit()
}

extension concrete.Result<CInt> {} // expected-error {{cannot extend generic struct}}

typealias IntegerResult = concrete.Result<CInt>
extension IntegerResult {} // expected-error {{cannot extend struct}}

extension concrete.IntResult {
  var doubled: CInt { value * 2 }
}
func useAliasExtension(_ value: concrete.IntResult) -> CInt { value.doubled }
func construct() -> concrete.IntResult { concrete.Result<CInt>(value: 5) }
struct GenericOwner<T> {
  var result: concrete.Result<UnsafeMutableRawPointer>
}
func absentExpression() {
  _ = concrete.Result<CBool>() // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Result' matches these type arguments}}
}
