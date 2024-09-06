// RUN: rm -rf %t
// RUN: not %target-swift-frontend -typecheck -I %S/Inputs  %s -cxx-interoperability-mode=upcoming-swift 2>&1 | %FileCheck %s

import FunctionsAndMethodsReturningFRT

let frtLocalVar1 = global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained()
// CHECK: error: 'global_function_returning_FRT_with_both_attrs_returns_retained_returns_unretained' cannot be annotated with both swift_attr('returns_retained') and swift_attr('returns_unretained') attributes

let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithBothAttributesReturnsRetainedAndReturnsUnretained.StaticMethodReturningFRT()
// CHECK: error: 'StaticMethodReturningFRT' cannot be annotated with both swift_attr('returns_retained') and swift_attr('returns_unretained') attributes
