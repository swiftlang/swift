// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs  %s -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature WarnUnannotatedReturnOfCxxFrt -strict-memory-safety -verify-additional-file %S/Inputs/cxx-functions-and-methods-returning-frt.h -Xcc -Wno-return-type -Xcc -Wno-nullability-completeness

// XFAIL: OS=windows-msvc
// TODO: Enable this on windows when -verify-additional-file issue on Windows Swift CI is resolved 

// REQUIRES: swift_feature_WarnUnannotatedReturnOfCxxFrt

import FunctionsAndMethodsReturningFRT
import CxxStdlib

let frtLocalVar1 = global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained()
let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithBothAttributesReturnsRetainedAndReturnsUnretained.StaticMethodReturningFRT()
let frtLocalVar3 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrt() // expected-warning {{cannot infer the ownership of the returned value, annotate 'StaticMethodReturningCxxFrt()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let frtLocalVar4 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrtWithAnnotation()
let frtLocalVar5 = global_function_returning_cxx_frt() // expected-warning {{cannot infer the ownership of the returned value, annotate 'global_function_returning_cxx_frt()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let frtLocalVar6 = global_function_returning_cxx_frt_with_annotations()
let frtLocalVar7 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrt() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{involves unsafe type}}
let frtLocalVar8 = StructWithAPIsReturningNonCxxFrt.StaticMethodReturningNonCxxFrtWithAnnotation() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{involves unsafe type}}
let frtLocalVar9 = global_function_returning_non_cxx_frt() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{involves unsafe type}}
let frtLocalVar10 = global_function_returning_non_cxx_frt_with_annotations() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{involves unsafe type}}
let frtLocalVar11 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReference()
let frtLocalVar12 = StructWithAPIsReturningImmortalReference.StaticMethodReturningImmortalReferenceWithAnnotation()
let frtLocalVar13 = global_function_returning_immortal_reference()
let frtLocalVar14 = global_function_returning_immortal_reference_with_annotations()
let frtLocalVar16 = StructWithAPIsReturningUnsafeReference.StaticMethodReturningUnsafeReferenceWithAnnotation() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{involves unsafe type}}
let frtLocalVar17 = global_function_returning_unsafe_reference() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{involves unsafe type}}
let frtLocalVar18 = global_function_returning_unsafe_reference_with_annotations() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{involves unsafe type}}
let x = returnFRTOverloadedOperators() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnFRTOverloadedOperators()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let y = returnFRTOverloadedOperators() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnFRTOverloadedOperators()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let z = x + y
let w = x - y
let f = FunctionVoidToFRTStruct() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{argument 'self' in call to initializer 'init' has unsafe type}} expected-note {{reference to unsafe type}} expected-note {{initializer 'init()' involves unsafe type}}
let frt = f() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{argument 'self' in call to instance method 'callAsFunction' has unsafe type}} expected-note {{reference to let 'f' involves unsafe type}}
let nonFrt = NonFRTStruct()
let nonFrtLocalVar1 = global_function_returning_templated_retrun_frt_owned(nonFrt)
let _ = DefaultOwnershipConventionOnCXXForeignRefType.returnRefTyDefUnretained()
let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretained()
let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretainedAnnotatedRetained()
let _ = DefaultOwnershipSuppressUnannotatedAPIWarning.returnRefType() // expected-warning {{cannot infer the ownership of the returned value, annotate 'returnRefType()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = DefaultOwnershipSuppressUnannotatedAPIWarning.returnRefTyDefUnretainedd()
let _ = DefaultOwnershipInheritance.createBaseType()
let _ = DefaultOwnershipInheritance.createDerivedType()
let _ = DefaultOwnershipInheritance.createDerivedType2()
let _ = DefaultOwnershipInheritance.createBaseTypeNonDefault() // expected-warning {{cannot infer the ownership of the returned value, annotate 'createBaseTypeNonDefault()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefault() // expected-warning {{cannot infer the ownership of the returned value, annotate 'createDerivedTypeNonDefault()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefaultUnretained()
let  _ = SourceLocationCaching.FactoryA.make() // expected-warning {{cannot infer the ownership of the returned value, annotate 'make()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
let  _ = SourceLocationCaching.FactoryB.make() // expected-warning {{cannot infer the ownership of the returned value, annotate 'make()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
