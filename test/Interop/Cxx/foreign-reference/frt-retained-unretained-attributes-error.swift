// RUN: rm -rf %t
// RUN: not %target-swift-frontend -typecheck -I %S/Inputs  %s -cxx-interoperability-mode=upcoming-swift 2>&1 | %FileCheck %s --check-prefix=CHECK-UPCOMING-SWIFT
// RUN: not %target-swift-frontend -typecheck -I %S/Inputs  %s -cxx-interoperability-mode=default 2>&1 | %FileCheck %s --check-prefix=CHECK-DEFAULT

import FunctionsAndMethodsReturningFRT

let frtLocalVar1 = global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained()
// CHECK-DEFAULT: error: 'global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained' cannot be annotated with both 'SWIFT_RETURNS_RETAINED' and 'SWIFT_RETURNS_UNRETAINED'

let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithBothAttributesReturnsRetainedAndReturnsUnretained.StaticMethodReturningFRT()
// CHECK-DEFAULT: error: 'StaticMethodReturningFRT' cannot be annotated with both 'SWIFT_RETURNS_RETAINED' and 'SWIFT_RETURNS_UNRETAINED'
let frtLocalVar3 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrt()
// CHECK-DEFAULT: warning: 'StaticMethodReturningCxxFrt' would not be imported in a later version of Swift because it is returning a 'SWIFT_SHARED_REFERENCE' type but it is not annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED'
// CHECK-UPCOMING-SWIFT: error: 'StaticMethodReturningCxxFrt' is not imported in Swift because it is returning a 'SWIFT_SHARED_REFERENCE' type but it is not annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED'
let frtLocalVar4 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrtWithAnnotation()

let frtLocalVar5 = global_function_returning_cxx_frt()
// CHECK-DEFAULT: warning: 'global_function_returning_cxx_frt' would not be imported in a later version of Swift because it is returning a 'SWIFT_SHARED_REFERENCE' type but it is not annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED'
// CHECK-UPCOMING-SWIFT: error: 'global_function_returning_cxx_frt' is not imported in Swift because it is returning a 'SWIFT_SHARED_REFERENCE' type but it is not annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED'
let frtLocalVar6 = global_function_returning_cxx_frt_with_annotations()

let frtLocalVar7 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrt()
let frtLocalVar8 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrtWithAnnotation()
// CHECK-DEFAULT: error: 'StaticMethodReturningNonCxxFrtWithAnnotation' cannot be annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED' because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar9 = global_function_returning_non_cxx_frt()
let frtLocalVar10 = global_function_returning_non_cxx_frt_with_annotations()
// CHECK-DEFAULT: error: 'global_function_returning_non_cxx_frt_with_annotations' cannot be annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED' because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar11 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReference()
let frtLocalVar12 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReferenceWithAnnotation()
// CHECK-DEFAULT: error: 'StaticMethodReturningImmortalReferenceWithAnnotation' cannot be annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED' because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar13 = global_function_returning_immortal_reference()
let frtLocalVar14 = global_function_returning_immortal_reference_with_annotations()
// CHECK-DEFAULT: error: 'global_function_returning_immortal_reference_with_annotations' cannot be annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED' because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar15 = StructWithAPIsReturningUnsafeReference.StaticMethodReturningUnsafeReference()
let frtLocalVar16 = StructWithAPIsReturningUnsafeReference.StaticMethodReturningUnsafeReferenceWithAnnotation()
// CHECK-DEFAULT: error: 'StaticMethodReturningUnsafeReferenceWithAnnotation' cannot be annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED' because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar17 = global_function_returning_unsafe_reference()
let frtLocalVar18 = global_function_returning_unsafe_reference_with_annotations()
// CHECK-DEFAULT: error: 'global_function_returning_unsafe_reference_with_annotations' cannot be annotated with either 'SWIFT_RETURNS_RETAINED' or 'SWIFT_RETURNS_UNRETAINED' because it is not returning a 'SWIFT_SHARED_REFERENCE' type
 