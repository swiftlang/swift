// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck -verify -I %S%{fs-sep}Inputs  %s -cxx-interoperability-mode=upcoming-swift -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}inheritance.h -Xcc -Wno-return-type -Xcc -Wno-inaccessible-base

import Inheritance

let _ = ImmortalRefereceExample.returnImmortalRefType()
let _ = ImmortalRefereceExample.returnDerivedFromImmortalRefType()

let _ = ExplicitAnnotationHasPrecedence1.returnValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnRefType()
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueTypeAndAnnotated()
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefType()
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefTypeAndAnnotated()

let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndB()
let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndBAnnotated()

let _ = BasicInheritanceExample.returnValueType()
let _ = BasicInheritanceExample.returnRefType()
let _ = BasicInheritanceExample.returnDerivedFromRefType()

let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef1AndBaseRef2()
let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef3()

let _ = MultipleInheritanceExample2.returnD()

let _ = MultipleInheritanceExample3.returnD()

let _ = OverloadedRetainRelease.returnD()

let _ = RefTypeDiamondInheritance.returnDiamond()
let _ = RefTypeDiamondInheritance.returnVirtualDiamond()

let _ = NonRefTypeDiamondInheritance.returnDiamond()

let _ = InheritingTemplatedRefType.returnForest()
