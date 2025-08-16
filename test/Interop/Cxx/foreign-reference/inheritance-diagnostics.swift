// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck -verify -I %S%{fs-sep}Inputs  %s -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature WarnUnannotatedReturnOfCxxFrt -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}inheritance.h -Xcc -Wno-return-type -Xcc -Wno-inaccessible-base

// REQUIRES: swift_feature_WarnUnannotatedReturnOfCxxFrt

import Inheritance

let _ = ImmortalRefereceExample.returnImmortalRefType()
let _ = ImmortalRefereceExample.returnDerivedFromImmortalRefType() // expected-note {{'returnDerivedFromImmortalRefType()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = ExplicitAnnotationHasPrecedence1.returnValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnRefType() // expected-note {{'returnRefType()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueTypeAndAnnotated() // expected-note {{'returnDerivedFromValueTypeAndAnnotated()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefType() // expected-note {{'returnDerivedFromRefType()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefTypeAndAnnotated() // expected-note {{'returnDerivedFromRefTypeAndAnnotated()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndB()
let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndBAnnotated() // expected-note {{'returnDerivedFromRefTypeAAndBAnnotated()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = BasicInheritanceExample.returnValueType()
let _ = BasicInheritanceExample.returnRefType() // expected-note {{'returnRefType()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = BasicInheritanceExample.returnDerivedFromRefType() // expected-note {{'returnDerivedFromRefType()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef1AndBaseRef2()
let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef3() // expected-note {{'returnDerivedFromBaseRef3()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = MultipleInheritanceExample2.returnD()

let _ = MultipleInheritanceExample3.returnD()

let _ = OverloadedRetainRelease.returnD() // expected-note {{'returnD()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = RefTypeDiamondInheritance.returnDiamond()
let _ = RefTypeDiamondInheritance.returnVirtualDiamond() // expected-note {{'returnVirtualDiamond()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = NonRefTypeDiamondInheritance.returnDiamond() // expected-note {{'returnDiamond()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = InheritingTemplatedRefType.returnForest() // expected-note {{'returnForest()' returns a SWIFT_SHARED_REFERENCE type, but isn't annotated with SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
