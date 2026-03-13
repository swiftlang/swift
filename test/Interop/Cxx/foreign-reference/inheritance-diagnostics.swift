// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -Xcc -Wno-return-type -Xcc -Wno-inaccessible-base \
// RUN:   -I %S%{fs-sep}Inputs %s \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}inheritance.h

import Inheritance

let _ = ImmortalRefereceExample.returnImmortalRefType()
let _ = ImmortalRefereceExample.returnDerivedFromImmortalRefType()

let _ = ExplicitAnnotationHasPrecedence1.returnValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnRefType() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnRefType()'}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueType()
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromValueTypeAndAnnotated() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnDerivedFromValueTypeAndAnnotated()'}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefType() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnDerivedFromRefType()'}}
let _ = ExplicitAnnotationHasPrecedence1.returnDerivedFromRefTypeAndAnnotated() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnDerivedFromRefTypeAndAnnotated()'}}

let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndB()
let _ = ExplicitAnnotationHasPrecedence2.returnDerivedFromRefTypeAAndBAnnotated() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnDerivedFromRefTypeAAndBAnnotated()'}}

let _ = BasicInheritanceExample.returnValueType()
let _ = BasicInheritanceExample.returnRefType() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnRefType()'}}
let _ = BasicInheritanceExample.returnDerivedFromRefType() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnDerivedFromRefType()'}}

let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef1AndBaseRef2()
let _ = MultipleInheritanceExample1.returnDerivedFromBaseRef3() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnDerivedFromBaseRef3()'}}

let _ = MultipleInheritanceExample2.returnD()

let _ = MultipleInheritanceExample3.returnD()

let _ = OverloadedRetainRelease.returnD() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnD()'}}

let _ = RefTypeDiamondInheritance.returnDiamond()
let _ = RefTypeDiamondInheritance.returnVirtualDiamond() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnVirtualDiamond()'}}

let _ = NonRefTypeDiamondInheritance.returnDiamond() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnDiamond()'}}

let _ = InheritingTemplatedRefType.returnForest() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnForest()'}}
