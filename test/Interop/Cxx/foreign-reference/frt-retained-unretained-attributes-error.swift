// RUN: rm -rf %t
// RUN: not %target-swift-frontend -typecheck -I %S/Inputs  %s -cxx-interoperability-mode=upcoming-swift 2>&1 | %FileCheck %s

import FunctionsAndMethodsReturningFRT

let frtLocalVar1 = global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained()
// CHECK: error: 'global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained' cannot be annotated with both swift_attr('returns_retained') and swift_attr('returns_unretained') attributes

let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithBothAttributesReturnsRetainedAndReturnsUnretained.StaticMethodReturningFRT()
// CHECK: error: 'StaticMethodReturningFRT' cannot be annotated with both swift_attr('returns_retained') and swift_attr('returns_unretained') attributes

let frtLocalVar3 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrt()
// CHECK: warning: 'StaticMethodReturningCxxFrt' is returning a 'SWIFT_SHARED_REFERENCE' type but is not annotated with either swift_attr('returns_retained') or swift_attr('returns_unretained') attributes
let frtLocalVar4 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrtWithAnnotation()

let frtLocalVar5 = global_function_returning_cxx_frt()
// CHECK: warning: 'global_function_returning_cxx_frt' is returning a 'SWIFT_SHARED_REFERENCE' type but is not annotated with either swift_attr('returns_retained') or swift_attr('returns_unretained') attributes
let frtLocalVar6 = global_function_returning_cxx_frt_with_annotations()

let frtLocalVar7 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrt()
let frtLocalVar8 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrtWithAnnotation()
// CHECK: error: 'StaticMethodReturningNonCxxFrtWithAnnotation' cannot be annotated with either swift_attr('returns_retained') or swift_attr('returns_unretained') attribute because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar9 = global_function_returning_non_cxx_frt()
let frtLocalVar10 = global_function_returning_non_cxx_frt_with_annotations()
// CHECK: error: 'global_function_returning_non_cxx_frt_with_annotations' cannot be annotated with either swift_attr('returns_retained') or swift_attr('returns_unretained') attribute because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar11 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReference()
let frtLocalVar12 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReferenceWithAnnotation()
// CHECK: error: 'StaticMethodReturningImmortalReferenceWithAnnotation' cannot be annotated with either swift_attr('returns_retained') or swift_attr('returns_unretained') attribute because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar13 = global_function_returning_immortal_reference()
let frtLocalVar14 = global_function_returning_immortal_reference_with_annotations()
// CHECK: error: 'global_function_returning_immortal_reference_with_annotations' cannot be annotated with either swift_attr('returns_retained') or swift_attr('returns_unretained') attribute because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar15 = StructWithAPIsReturningUnsafeReference.StaticMethodReturningUnsafeReference()
let frtLocalVar16 = StructWithAPIsReturningUnsafeReference.StaticMethodReturningUnsafeReferenceWithAnnotation()
// CHECK: error: 'StaticMethodReturningUnsafeReferenceWithAnnotation' cannot be annotated with either swift_attr('returns_retained') or swift_attr('returns_unretained') attribute because it is not returning a 'SWIFT_SHARED_REFERENCE' type

let frtLocalVar17 = global_function_returning_unsafe_reference()
let frtLocalVar18 = global_function_returning_unsafe_reference_with_annotations()
// CHECK: error: 'global_function_returning_unsafe_reference_with_annotations' cannot be annotated with either swift_attr('returns_retained') or swift_attr('returns_unretained') attribute because it is not returning a 'SWIFT_SHARED_REFERENCE' type
 