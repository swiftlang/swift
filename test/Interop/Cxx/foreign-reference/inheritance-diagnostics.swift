// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck -verify -I %S%{fs-sep}Inputs  %s -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature WarnUnannotatedReturnOfCxxFrt -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}inheritance.h -Xcc -Wno-return-type -Xcc -Wno-inaccessible-base

// REQUIRES: swift_feature_WarnUnannotatedReturnOfCxxFrt

import Inheritance

let _ = ImmortalRefereceExample.returnImmortalRefType()
let _ = ImmortalRefereceExample.returnDerivedFromImmortalRefType() // expected-warning {{'returnDerivedFromImmortalRefType' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}

let _ = ExplicitAnnotationHasPrecedence1.returnValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnRefType() // expected-warning {{'returnRefType' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueTypeAndAnnotated() // expected-warning {{'returnDerivedFromValueTypeAndAnnotated' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefType() // expected-warning {{'returnDerivedFromRefType' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefTypeAndAnnotated() // expected-warning {{'returnDerivedFromRefTypeAndAnnotated' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}

let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndB()
let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndBAnnotated() // expected-warning {{'returnDerivedFromRefTypeAAndBAnnotated' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}

let _ = BasicInheritanceExample.returnValueType()
let _ = BasicInheritanceExample.returnRefType() // expected-warning {{'returnRefType' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let _ = BasicInheritanceExample.returnDerivedFromRefType() // expected-warning {{'returnDerivedFromRefType' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}

let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef1AndBaseRef2()
let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef3() // expected-warning {{'returnDerivedFromBaseRef3' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}

let _ = MultipleInheritanceExample2.returnD()

let _ = MultipleInheritanceExample3.returnD()

let _ = OverloadedRetainRelease.returnD() // expected-warning {{'returnD' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}

let _ = RefTypeDiamondInheritance.returnDiamond()
let _ = RefTypeDiamondInheritance.returnVirtualDiamond() // expected-warning {{'returnVirtualDiamond' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}

let _ = NonRefTypeDiamondInheritance.returnDiamond() // expected-warning {{'returnDiamond' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}

let _ = InheritingTemplatedRefType.returnForest() // expected-warning {{'returnForest' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
