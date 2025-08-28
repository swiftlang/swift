// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck -verify -I %S%{fs-sep}Inputs  %s -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature WarnUnannotatedReturnOfCxxFrt -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}inheritance.h -Xcc -Wno-return-type -Xcc -Wno-inaccessible-base

// REQUIRES: swift_feature_WarnUnannotatedReturnOfCxxFrt

import Inheritance

let _ = ImmortalRefereceExample.returnImmortalRefType()
let _ = ImmortalRefereceExample.returnDerivedFromImmortalRefType() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnDerivedFromImmortalRefType()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = ExplicitAnnotationHasPrecedence1.returnValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnRefType() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnRefType()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueTypeAndAnnotated() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnDerivedFromValueTypeAndAnnotated()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefType() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnDerivedFromRefType()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefTypeAndAnnotated() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnDerivedFromRefTypeAndAnnotated()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndB()
let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndBAnnotated() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnDerivedFromRefTypeAAndBAnnotated()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = BasicInheritanceExample.returnValueType()
let _ = BasicInheritanceExample.returnRefType() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnRefType()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = BasicInheritanceExample.returnDerivedFromRefType() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnDerivedFromRefType()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef1AndBaseRef2()
let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef3() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnDerivedFromBaseRef3()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = MultipleInheritanceExample2.returnD()

let _ = MultipleInheritanceExample3.returnD()

let _ = OverloadedRetainRelease.returnD() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnD()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = RefTypeDiamondInheritance.returnDiamond()
let _ = RefTypeDiamondInheritance.returnVirtualDiamond() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnVirtualDiamond()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = NonRefTypeDiamondInheritance.returnDiamond() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnDiamond()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

let _ = InheritingTemplatedRefType.returnForest() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnForest()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
