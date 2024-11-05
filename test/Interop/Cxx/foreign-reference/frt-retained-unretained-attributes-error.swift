// RUN: rm -rf %t
// RUN: not %target-swift-frontend -typecheck -I %S/Inputs  %s -cxx-interoperability-mode=upcoming-swift 2>&1 | %FileCheck %s

import FunctionsAndMethodsReturningFRT
import CxxStdlib

let frtLocalVar1 = global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained()
// CHECK: error: 'global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained' cannot be annotated with both SWIFT_RETURNS_RETAINED and SWIFT_RETURNS_UNRETAINED

let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithBothAttributesReturnsRetainedAndReturnsUnretained.StaticMethodReturningFRT()
// CHECK: error: 'StaticMethodReturningFRT' cannot be annotated with both SWIFT_RETURNS_RETAINED and SWIFT_RETURNS_UNRETAINED
let frtLocalVar3 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrt()
// CHECK: warning: 'StaticMethodReturningCxxFrt' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE
let frtLocalVar4 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrtWithAnnotation()

let frtLocalVar5 = global_function_returning_cxx_frt()
// CHECK: warning: 'global_function_returning_cxx_frt' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE
let frtLocalVar6 = global_function_returning_cxx_frt_with_annotations()

let frtLocalVar7 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrt()
let frtLocalVar8 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrtWithAnnotation()
// CHECK: error: 'StaticMethodReturningNonCxxFrtWithAnnotation' cannot be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED because it is not returning a SWIFT_SHARED_REFERENCE type

let frtLocalVar9 = global_function_returning_non_cxx_frt()
let frtLocalVar10 = global_function_returning_non_cxx_frt_with_annotations()
// CHECK: error: 'global_function_returning_non_cxx_frt_with_annotations' cannot be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED because it is not returning a SWIFT_SHARED_REFERENCE type

let frtLocalVar11 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReference()
let frtLocalVar12 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReferenceWithAnnotation()
// CHECK: error: 'StaticMethodReturningImmortalReferenceWithAnnotation' cannot be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED because it is not returning a SWIFT_SHARED_REFERENCE type

let frtLocalVar13 = global_function_returning_immortal_reference()
let frtLocalVar14 = global_function_returning_immortal_reference_with_annotations()
// CHECK: error: 'global_function_returning_immortal_reference_with_annotations' cannot be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED because it is not returning a SWIFT_SHARED_REFERENCE type

let frtLocalVar15 = StructWithAPIsReturningUnsafeReference.StaticMethodReturningUnsafeReference()
let frtLocalVar16 = StructWithAPIsReturningUnsafeReference.StaticMethodReturningUnsafeReferenceWithAnnotation()
// CHECK: error: 'StaticMethodReturningUnsafeReferenceWithAnnotation' cannot be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED because it is not returning a SWIFT_SHARED_REFERENCE type

let frtLocalVar17 = global_function_returning_unsafe_reference()
let frtLocalVar18 = global_function_returning_unsafe_reference_with_annotations()
// CHECK: error: 'global_function_returning_unsafe_reference_with_annotations' cannot be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED because it is not returning a SWIFT_SHARED_REFERENCE type

let x = returnFRTOverloadedOperators()
// CHECK: warning: 'returnFRTOverloadedOperators' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE
let y = returnFRTOverloadedOperators()
// CHECK: warning: 'returnFRTOverloadedOperators' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE
let z = x + y
// CHECK: warning: SWIFT_RETURNS_RETAINED and SWIFT_RETURNS_UNRETAINED is not supported yet for overloaded C++ 'operator+'. Overloaded C++ operators always return SWIFT_SHARED_REFERENCE types as owned
let w = x - y
// CHECK-NOT: warning: 'operator-' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE

let f = FunctionVoidToFRTStruct()
let frt = f()
// CHECK-NOT: warning: 'operator()' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE
