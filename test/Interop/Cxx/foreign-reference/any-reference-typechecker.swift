// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -target %target-swift-5.8-abi-triple -cxx-interoperability-mode=default -enable-experimental-feature AnyReference

// REQUIRES: swift_feature_AnyReference

import AnyReferenceFRTs

class SwiftClass {}
struct SwiftStruct {}

struct Box<T: _AnyReference> {} // expected-note 3 {{requirement specified as 'T' : '_AnyReference'}}

// Swift classes and C++ foreign reference types satisfy '_AnyReference'.
let _: Box<SwiftClass>
let _: Box<RefCountedType>
let _: Box<DerivedRefCountedType>
let _: Box<ImmortalRefType>

// Value types don't.
let _: Box<SwiftStruct> // expected-error {{'Box' requires that 'SwiftStruct' be a class or foreign reference type}}
let _: Box<ValueType> // expected-error {{'Box' requires that 'ValueType' be a class or foreign reference type}}
let _: Box<Int> // expected-error {{'Box' requires that 'Int' be a class or foreign reference type}}

// An archetype constrained to AnyObject or _AnyReference satisfies it.
func fromAnyObject<T: AnyObject>(_: T) { let _: Box<T> }
func fromAnyReference<T: _AnyReference>(_: T) { let _: Box<T> }

// An AnyReference archetype doesn't satisfy AnyObject: it may be a foreign
// reference type.
struct ClassBox<T: AnyObject> {} // expected-note {{requirement specified as 'T' : 'AnyObject' [with T = T]}}
func toAnyObject<T: _AnyReference>(_: T) { let _: ClassBox<T> } // expected-error {{'ClassBox' requires that 'T' be a class type}}

struct Generic<T> {}
extension Generic where T == ImmortalRefType, T: _AnyReference {}
extension Generic where T == RefCountedType, T: _AnyReference {}
extension Generic where T == SwiftClass, T: _AnyReference {}
extension Generic where T == ValueType, T: _AnyReference {} // expected-error {{no type for 'T' can satisfy both 'T : _AnyReference' and 'T == ValueType'}}
extension Generic where T == Int, T: _AnyReference {} // expected-error {{no type for 'T' can satisfy both 'T : _AnyReference' and 'T == Int'}}
