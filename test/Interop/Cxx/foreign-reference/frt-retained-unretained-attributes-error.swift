// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -Xcc -Wno-return-type -Xcc -Wno-nullability-completeness \
// RUN:   -I %S%{fs-sep}Inputs %s \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}cxx-functions-and-methods-returning-frt.h

import FunctionsAndMethodsReturningFRT
import CxxStdlib

let frtLocalVar1 = global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained()
let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithBothAttributesReturnsRetainedAndReturnsUnretained.StaticMethodReturningFRT()
let frtLocalVar3 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrt() // expected-warning {{cannot infer ownership of foreign reference value returned by 'StaticMethodReturningCxxFrt()'}}
let frtLocalVar4 = StructWithAPIsReturningCxxFrt.StaticMethodReturningCxxFrtWithAnnotation()
let frtLocalVar5 = global_function_returning_cxx_frt() // expected-warning {{cannot infer ownership of foreign reference value returned by 'global_function_returning_cxx_frt()'}}
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
let x = returnFRTOverloadedOperators() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnFRTOverloadedOperators()'}}
let y = returnFRTOverloadedOperators() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnFRTOverloadedOperators()'}}
let z = x + y
let w = x - y
let f = FunctionVoidToFRTStruct()
let frt = f()
let nonFrt = NonFRTStruct()
let nonFrtLocalVar1 = global_function_returning_templated_retrun_frt_owned(nonFrt)
let _ = DefaultOwnershipConventionOnCXXForeignRefType.returnRefTyDefUnretained()
let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretained()
let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretainedAnnotatedRetained()
let _ = DefaultOwnershipSuppressUnannotatedAPIWarning.returnRefType() // expected-warning {{cannot infer ownership of foreign reference value returned by 'returnRefType()'}}
let _ = DefaultOwnershipSuppressUnannotatedAPIWarning.returnRefTyDefUnretainedd()
let _ = DefaultOwnershipInheritance.createBaseType()
let _ = DefaultOwnershipInheritance.createDerivedType()
let _ = DefaultOwnershipInheritance.createDerivedType2()
let _ = DefaultOwnershipInheritance.createBaseTypeNonDefault() // expected-warning {{cannot infer ownership of foreign reference value returned by 'createBaseTypeNonDefault()'}}
let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefault() // expected-warning {{cannot infer ownership of foreign reference value returned by 'createDerivedTypeNonDefault()'}}
let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefaultUnretained()
let  _ = SourceLocationCaching.FactoryA.make() // expected-warning {{cannot infer ownership of foreign reference value returned by 'make()'}}
let  _ = SourceLocationCaching.FactoryB.make() // expected-warning {{cannot infer ownership of foreign reference value returned by 'make()'}}

let refTemplate = FRTStructRef()
let templatePtr = refTemplate.ptr()
let templateGet = refTemplate.get()
let templateValue = refTemplate.value()  // expected-warning {{cannot infer ownership of foreign reference value returned by 'value()'}}
