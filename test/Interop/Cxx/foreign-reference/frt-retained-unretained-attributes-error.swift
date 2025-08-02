// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs  %s -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature WarnUnannotatedReturnOfCxxFrt -verify-additional-file %S/Inputs/cxx-functions-and-methods-returning-frt.h -Xcc -Wno-return-type -Xcc -Wno-nullability-completeness

// XFAIL: OS=windows-msvc
// TODO: Enable this on windows when -verify-additional-file issue on Windows Swift CI is resolved 

// REQUIRES: swift_feature_WarnUnannotatedReturnOfCxxFrt

import FunctionsAndMethodsReturningFRT
import CxxStdlib

let frtLocalVar1 = global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained()
let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithBothAttributesReturnsRetainedAndReturnsUnretained.StaticMethodReturningFRT()
let frtLocalVar3 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrt() // expected-warning {{'StaticMethodReturningCxxFrt' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let frtLocalVar4 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrtWithAnnotation()
let frtLocalVar5 = global_function_returning_cxx_frt() // expected-warning {{'global_function_returning_cxx_frt' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let frtLocalVar6 = global_function_returning_cxx_frt_with_annotations()
let frtLocalVar7 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrt()
let frtLocalVar8 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrtWithAnnotation()
let frtLocalVar9 = global_function_returning_non_cxx_frt()
let frtLocalVar10 = global_function_returning_non_cxx_frt_with_annotations()
let frtLocalVar11 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReference()
let frtLocalVar12 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReferenceWithAnnotation()
let frtLocalVar13 = global_function_returning_immortal_reference()
let frtLocalVar14 = global_function_returning_immortal_reference_with_annotations()
let frtLocalVar16 = StructWithAPIsReturningUnsafeReference.StaticMethodReturningUnsafeReferenceWithAnnotation()
let frtLocalVar17 = global_function_returning_unsafe_reference()
let frtLocalVar18 = global_function_returning_unsafe_reference_with_annotations()
let x = returnFRTOverloadedOperators() // expected-warning {{'returnFRTOverloadedOperators' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let y = returnFRTOverloadedOperators() // expected-warning {{'returnFRTOverloadedOperators' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let z = x + y
let w = x - y
let f = FunctionVoidToFRTStruct()
let frt = f()
let nonFrt = NonFRTStruct()
let _ = DefaultOwnershipConventionOnCXXForeignRefType.returnRefTyDefUnretained()
let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretained()
let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretainedAnnotatedRetained()
let _ = DefaultOwnershipSuppressUnannotatedAPIWarning.returnRefType() // expected-warning {{'returnRefType' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let _ = DefaultOwnershipSuppressUnannotatedAPIWarning.returnRefTyDefUnretainedd()
let _ = DefaultOwnershipInheritance.createBaseType()
let _ = DefaultOwnershipInheritance.createDerivedType()
let _ = DefaultOwnershipInheritance.createDerivedType2()
let _ = DefaultOwnershipInheritance.createBaseTypeNonDefault() // expected-warning {{'createBaseTypeNonDefault' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefault() // expected-warning {{'createDerivedTypeNonDefault' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefaultUnretained()
let  _ = SourceLocationCaching.FactoryA.make() // expected-warning {{'make' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
let  _ = SourceLocationCaching.FactoryB.make() // expected-warning {{'make' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it returns a SWIFT_SHARED_REFERENCE}}
