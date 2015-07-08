// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_1 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_3 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_4 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_5 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_NO_DOT_1 | FileCheck %s -check-prefix=FOO_OBJECT_NO_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_NO_DOT_2 | FileCheck %s -check-prefix=FOO_OBJECT_NO_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_STRUCT_DOT_1 | FileCheck %s -check-prefix=FOO_STRUCT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_STRUCT_NO_DOT_1 | FileCheck %s -check-prefix=FOO_STRUCT_NO_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CF1 | FileCheck %s -check-prefix=CF1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CF2 | FileCheck %s -check-prefix=CF2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CF3 | FileCheck %s -check-prefix=CF3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CF4 | FileCheck %s -check-prefix=CF4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_FUNC_0 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_FUNC_0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_FUNC_1 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_FUNC_2 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_FUNC_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_VARARG_FUNC_0 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_VARARG_FUNC_0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_VARARG_FUNC_1 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_VARARG_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_VARARG_FUNC_2 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_VARARG_FUNC_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_OVERLOADED_FUNC_1 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_OVERLOADED_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_OVERLOADED_FUNC_2 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_OVERLOADED_FUNC_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_CURRIED_FUNC_1 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_CURRIED_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_CURRIED_FUNC_2 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_CURRIED_FUNC_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_CURRIED_FUNC_3 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_CURRIED_FUNC_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_CURRIED_FUNC_4 | FileCheck %s -check-prefix=IMPLICITLY_CURRIED_CURRIED_FUNC_4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_SWITCH_CASE_1 | FileCheck %s -check-prefix=IN_SWITCH_CASE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_SWITCH_CASE_2 | FileCheck %s -check-prefix=IN_SWITCH_CASE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_SWITCH_CASE_3 | FileCheck %s -check-prefix=IN_SWITCH_CASE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_SWITCH_CASE_4 | FileCheck %s -check-prefix=IN_SWITCH_CASE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VF1 | FileCheck %s -check-prefix=VF1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VF2 | FileCheck %s -check-prefix=VF2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BASE_MEMBERS | FileCheck %s -check-prefix=BASE_MEMBERS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BASE_MEMBERS_STATIC | FileCheck %s -check-prefix=BASE_MEMBERS_STATIC

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_NO_DOT_1 | FileCheck %s -check-prefix=PROTO_MEMBERS_NO_DOT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_NO_DOT_2 | FileCheck %s -check-prefix=PROTO_MEMBERS_NO_DOT_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_NO_DOT_3 | FileCheck %s -check-prefix=PROTO_MEMBERS_NO_DOT_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_1 | FileCheck %s -check-prefix=PROTO_MEMBERS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_2 | FileCheck %s -check-prefix=PROTO_MEMBERS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_3 | FileCheck %s -check-prefix=PROTO_MEMBERS_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_4 | FileCheck %s -check-prefix=PROTO_MEMBERS_4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_0 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_1 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_2 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_3 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_4 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_5 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_6 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_7 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_7

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_VARARG_FUNCTION_CALL_1 | FileCheck %s -check-prefix=INSIDE_VARARG_FUNCTION_CALL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_VARARG_FUNCTION_CALL_2 | FileCheck %s -check-prefix=INSIDE_VARARG_FUNCTION_CALL_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_VARARG_FUNCTION_CALL_3 | FileCheck %s -check-prefix=INSIDE_VARARG_FUNCTION_CALL_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_OVERLOADED_FUNCTION_CALL_1 | FileCheck %s -check-prefix=INSIDE_OVERLOADED_FUNCTION_CALL_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_CURRIED_FUNCTION_CALL_1 | FileCheck %s -check-prefix=INSIDE_CURRIED_FUNCTION_CALL_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1 | FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_1 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_3 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_4 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_5 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_6 | FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_6

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CONSTRUCTOR_PARAM_1 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CONSTRUCTOR_PARAM_2 | FileCheck %s -check-prefix=RESOLVE_CONSTRUCTOR_PARAM_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CONSTRUCTOR_PARAM_3 | FileCheck %s -check-prefix=RESOLVE_CONSTRUCTOR_PARAM_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_PAREN_PATTERN_1 | FileCheck %s -check-prefix=FUNC_PAREN_PATTERN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_PAREN_PATTERN_2 | FileCheck %s -check-prefix=FUNC_PAREN_PATTERN_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_PAREN_PATTERN_3 | FileCheck %s -check-prefix=FUNC_PAREN_PATTERN_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAINED_CALLS_1 | FileCheck %s -check-prefix=CHAINED_CALLS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAINED_CALLS_2 | FileCheck %s -check-prefix=CHAINED_CALLS_2

// Disabled because we aren't handling failures well.
// FIXME: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAINED_CALLS_3 | FileCheck %s -check-prefix=CHAINED_CALLS_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_1 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_2 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_3 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_4 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_5 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_ERROR_1 | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_ERROR_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_1_STATIC | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_1_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_2_STATIC | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_2_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_3_STATIC | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_3_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_4_STATIC | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_4_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_5_STATIC | FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_5_STATIC

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_UNSOLVED_VARIABLES_1 | FileCheck %s -check-prefix=TC_UNSOLVED_VARIABLES_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_UNSOLVED_VARIABLES_2 | FileCheck %s -check-prefix=TC_UNSOLVED_VARIABLES_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_UNSOLVED_VARIABLES_3 | FileCheck %s -check-prefix=TC_UNSOLVED_VARIABLES_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_UNSOLVED_VARIABLES_4 | FileCheck %s -check-prefix=TC_UNSOLVED_VARIABLES_4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_MODULES_1 | FileCheck %s -check-prefix=RESOLVE_MODULES_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INTERPOLATED_STRING_1 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P1 | FileCheck %s -check-prefix=PROTOCOL_EXT_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P2 | FileCheck %s -check-prefix=PROTOCOL_EXT_P2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P3 | FileCheck %s -check-prefix=PROTOCOL_EXT_P3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_WILLCONFORMP1 | FileCheck %s -check-prefix=PROTOCOL_EXT_WILLCONFORMP1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_DIDCONFORMP2 | FileCheck %s -check-prefix=PROTOCOL_EXT_DIDCONFORMP2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_DIDCONFORMP3 | FileCheck %s -check-prefix=PROTOCOL_EXT_DIDCONFORMP3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_GENERICP1 | FileCheck %s -check-prefix=PROTOCOL_EXT_GENERICP1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_GENERICP2 | FileCheck %s -check-prefix=PROTOCOL_EXT_GENERICP2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_GENERICP3 | FileCheck %s -check-prefix=PROTOCOL_EXT_GENERICP3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P4 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE1 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE2 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONSTRAINED_GENERIC_1 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONSTRAINED_GENERIC_2 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INSIDE_CONCRETE1_1 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INSIDE_CONCRETE1_2 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE3 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE3_SUB | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME_SUB
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE4 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONSTRAINED_GENERIC_3 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONSTRAINED_GENERIC_3_SUB | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME_SUB
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INSIDE_CONCRETE2_1 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INSIDE_CONCRETE2_2 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_TA_1 | FileCheck %s -check-prefix=PROTOCOL_EXT_TA
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_TA_2 | FileCheck %s -check-prefix=PROTOCOL_EXT_TA
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INIT_1 | FileCheck %s -check-prefix=PROTOCOL_EXT_INIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INIT_2 | FileCheck %s -check-prefix=PROTOCOL_EXT_INIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P4_DOT_1 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P4_DOT_2 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P4_T_DOT_1 | FileCheck %s -check-prefix=PROTOCOL_EXT_P4_T_DOT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_UNUSABLE_EXISTENTIAL | FileCheck %s -check-prefix=PROTOCOL_EXT_UNUSABLE_EXISTENTIAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=THROWS1 | FileCheck %s -check-prefix=THROWS1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=THROWS2 | FileCheck %s -check-prefix=THROWS2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_THROWS1 | FileCheck %s -check-prefix=MEMBER_THROWS1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_THROWS2 | FileCheck %s -check-prefix=MEMBER_THROWS2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_THROWS3 | FileCheck %s -check-prefix=MEMBER_THROWS3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_THROWS1 | FileCheck %s -check-prefix=INIT_THROWS1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE1 > %t.autoclosure1
// RUN: FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE2 > %t.autoclosure2
// RUN: FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE3 > %t.autoclosure3
// RUN: FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE4 > %t.autoclosure4
// RUN: FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE5 > %t.autoclosure5
// RUN: FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure5

// Test code completion of expressions that produce a value.

struct FooStruct {
  lazy var lazyInstanceVar = 0
  var instanceVar = 0

  mutating
  func instanceFunc0() {}
  mutating
  func instanceFunc1(a: Int) {}
  mutating
  func instanceFunc2(a: Int, inout b: Double) {}
  mutating
  func instanceFunc3(a: Int, _: (Float, Double)) {}
  mutating
  func instanceFunc4(a: Int?, b: Int!, inout c: Int?, inout d: Int!) {}
  mutating
  func instanceFunc5() -> Int? {}
  mutating
  func instanceFunc6() -> Int! {}
  mutating
  func instanceFunc7(#a: Int) {}
  mutating
  func instanceFunc8(a: (Int, Int)) {}
  mutating
  func instanceFunc9(@autoclosure a: () -> Int) {}

  mutating
  func varargInstanceFunc0(v: Int...) {}
  mutating
  func varargInstanceFunc1(a: Float, v: Int...) {}
  mutating
  func varargInstanceFunc2(a: Float, b: Double, v: Int...) {}

  mutating
  func overloadedInstanceFunc1() -> Int {}
  mutating
  func overloadedInstanceFunc1() -> Double {}

  mutating
  func overloadedInstanceFunc2(x: Int) -> Int {}
  mutating
  func overloadedInstanceFunc2(x: Double) -> Int {}

  mutating
  func builderFunc1(a: Int) -> FooStruct { return self }

  subscript(i: Int) -> Double {
    get {
      return Double(i)
    }
    set(v) {
      instanceVar = i
    }
  }

  subscript(i: Int, j: Int) -> Double {
    get {
      return Double(i + j)
    }
    set(v) {
      instanceVar = i + j
    }
  }

  mutating
  func curriedVoidFunc1()() {}
  mutating
  func curriedVoidFunc2()(a: Int) {}
  mutating
  func curriedVoidFunc3(a: Int)() {}
  mutating
  func curriedVoidFunc4(a: Int)(b: Int) {}
  mutating
  func curriedVoidFunc5(a: Int)(b: Int, _: (Float, Double)) {}

  mutating
  func curriedStringFunc1()() -> String {}
  mutating
  func curriedStringFunc2()(a: Int) -> String {}
  mutating
  func curriedStringFunc3(a: Int)() -> String {}
  mutating
  func curriedStringFunc4(a: Int)(b: Int) -> String {}
  mutating
  func curriedStringFunc5(a: Int)(b: Int, _: (Float, Double)) -> String {}

  mutating
  func selectorVoidFunc1(_ a: Int, b x: Float) {}
  mutating
  func selectorVoidFunc2(_ a: Int, b x: Float, c y: Double) {}
  mutating
  func selectorVoidFunc3(_ a: Int, b _: (Float, Double)) {}

  mutating
  func selectorStringFunc1(_ a: Int, b x: Float) -> String {}
  mutating
  func selectorStringFunc2(_ a: Int,  b x: Float, c y: Double) -> String {}
  mutating
  func selectorStringFunc3(_ a: Int, b _: (Float, Double)) -> String{}

  struct NestedStruct {}
  class NestedClass {}
  enum NestedEnum {}
  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  static var staticVar: Int = 4

  static func staticFunc0() {}
  static func staticFunc1(a: Int) {}

  static func overloadedStaticFunc1() -> Int {}
  static func overloadedStaticFunc1() -> Double {}

  static func overloadedStaticFunc2(x: Int) -> Int {}
  static func overloadedStaticFunc2(x: Double) -> Int {}
}

extension FooStruct {
  var extProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  mutating
  func extFunc0() {}

  static var extStaticProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  static func extStaticFunc0() {}

  struct ExtNestedStruct {}
  class ExtNestedClass {}
  enum ExtNestedEnum {
    case ExtEnumX(Int)
  }

  typealias ExtNestedTypealias = Int
}

var fooObject: FooStruct

// FOO_OBJECT_DOT: Begin completions
// FOO_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    lazyInstanceVar[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc2({#(a): Int#}, {#b: &Double#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc3({#(a): Int#}, {#(Float, Double)#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc4({#(a): Int?#}, {#b: Int!#}, {#c: &Int?#}, {#d: &Int!#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc5()[#Int?#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc6()[#Int!#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc7({#a: Int#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc8({#(a): (Int, Int)#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc9({#(a): Int#})[#Void#]{{; name=.+$}}
//
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: varargInstanceFunc0({#(v): Int...#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: varargInstanceFunc1({#(a): Float#}, {#v: Int...#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: varargInstanceFunc2({#(a): Float#}, {#b: Double#}, {#v: Int...#})[#Void#]{{; name=.+$}}
//
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc1()[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc1()[#Double#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc2({#(x): Int#})[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc2({#(x): Double#})[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: builderFunc1({#(a): Int#})[#FooStruct#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc1()[#() -> Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc2()[#(a: Int) -> Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc3({#(a): Int#})[#() -> Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc4({#(a): Int#})[#(b: Int) -> Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc5({#(a): Int#})[#(b: Int, (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc1()[#() -> String#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc2()[#(a: Int) -> String#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc3({#(a): Int#})[#() -> String#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc4({#(a): Int#})[#(b: Int) -> String#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc5({#(a): Int#})[#(b: Int, (Float, Double)) -> String#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorVoidFunc1({#(a): Int#}, {#b: Float#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorVoidFunc2({#(a): Int#}, {#b: Float#}, {#c: Double#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorVoidFunc3({#(a): Int#}, {#b: (Float, Double)#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorStringFunc1({#(a): Int#}, {#b: Float#})[#String#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorStringFunc2({#(a): Int#}, {#b: Float#}, {#c: Double#})[#String#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorStringFunc3({#(a): Int#}, {#b: (Float, Double)#})[#String#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    extProp[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: extFunc0()[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: End completions

// FOO_OBJECT_NO_DOT: Begin completions
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    .lazyInstanceVar[#Int#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    .instanceVar[#Int#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc0()[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc2({#(a): Int#}, {#b: &Double#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc3({#(a): Int#}, {#(Float, Double)#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc4({#(a): Int?#}, {#b: Int!#}, {#c: &Int?#}, {#d: &Int!#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc5()[#Int?#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc6()[#Int!#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc7({#a: Int#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc8({#(a): (Int, Int)#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc9({#(a): Int#})[#Void#]{{; name=.+$}}
//
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .varargInstanceFunc0({#(v): Int...#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .varargInstanceFunc1({#(a): Float#}, {#v: Int...#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .varargInstanceFunc2({#(a): Float#}, {#b: Double#}, {#v: Int...#})[#Void#]{{; name=.+$}}
//
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc1()[#Int#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc1()[#Double#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc2({#(x): Int#})[#Int#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc2({#(x): Double#})[#Int#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .builderFunc1({#(a): Int#})[#FooStruct#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[Subscript]/CurrNominal:      [{#Int#}][#Double#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[Subscript]/CurrNominal:      [{#Int#}, {#Int#}][#Double#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc1()[#() -> Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc2()[#(a: Int) -> Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc3({#(a): Int#})[#() -> Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc4({#(a): Int#})[#(b: Int) -> Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc5({#(a): Int#})[#(b: Int, (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc1()[#() -> String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc2()[#(a: Int) -> String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc3({#(a): Int#})[#() -> String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc4({#(a): Int#})[#(b: Int) -> String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc5({#(a): Int#})[#(b: Int, (Float, Double)) -> String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorVoidFunc1({#(a): Int#}, {#b: Float#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorVoidFunc2({#(a): Int#}, {#b: Float#}, {#c: Double#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorVoidFunc3({#(a): Int#}, {#b: (Float, Double)#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorStringFunc1({#(a): Int#}, {#b: Float#})[#String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorStringFunc2({#(a): Int#}, {#b: Float#}, {#c: Double#})[#String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorStringFunc3({#(a): Int#}, {#b: (Float, Double)#})[#String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    .extProp[#Int#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .extFunc0()[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: End completions

// FOO_STRUCT_DOT: Begin completions
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc0({#self: &FooStruct#})[#() -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#self: &FooStruct#})[#(Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc2({#self: &FooStruct#})[#(Int, inout b: Double) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc3({#self: &FooStruct#})[#(Int, (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc4({#self: &FooStruct#})[#(Int?, b: Int!, inout c: Int?, inout d: Int!) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc5({#self: &FooStruct#})[#() -> Int?#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc6({#self: &FooStruct#})[#() -> Int!#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc7({#self: &FooStruct#})[#(a: Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc8({#self: &FooStruct#})[#((Int, Int)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc9({#self: &FooStruct#})[#(@autoclosure () -> Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: varargInstanceFunc0({#self: &FooStruct#})[#(Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: varargInstanceFunc1({#self: &FooStruct#})[#(Float, v: Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: varargInstanceFunc2({#self: &FooStruct#})[#(Float, b: Double, v: Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc1({#self: &FooStruct#})[#() -> Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc1({#self: &FooStruct#})[#() -> Double#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc2({#self: &FooStruct#})[#(Int) -> Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc2({#self: &FooStruct#})[#(Double) -> Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: builderFunc1({#self: &FooStruct#})[#(Int) -> FooStruct#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc1({#self: &FooStruct#})[#() -> () -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc2({#self: &FooStruct#})[#() -> (a: Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc3({#self: &FooStruct#})[#(Int) -> () -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc4({#self: &FooStruct#})[#(Int) -> (b: Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedVoidFunc5({#self: &FooStruct#})[#(Int) -> (b: Int, (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc1({#self: &FooStruct#})[#() -> () -> String#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc2({#self: &FooStruct#})[#() -> (a: Int) -> String#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc3({#self: &FooStruct#})[#(Int) -> () -> String#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc4({#self: &FooStruct#})[#(Int) -> (b: Int) -> String#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: curriedStringFunc5({#self: &FooStruct#})[#(Int) -> (b: Int, (Float, Double)) -> String#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorVoidFunc1({#self: &FooStruct#})[#(Int, b: Float) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorVoidFunc2({#self: &FooStruct#})[#(Int, b: Float, c: Double) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorVoidFunc3({#self: &FooStruct#})[#(Int, b: (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorStringFunc1({#self: &FooStruct#})[#(Int, b: Float) -> String#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorStringFunc2({#self: &FooStruct#})[#(Int, b: Float, c: Double) -> String#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: selectorStringFunc3({#self: &FooStruct#})[#(Int, b: (Float, Double)) -> String#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[Struct]/CurrNominal:         NestedStruct[#FooStruct.NestedStruct#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[Class]/CurrNominal:          NestedClass[#FooStruct.NestedClass#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[Enum]/CurrNominal:           NestedEnum[#FooStruct.NestedEnum#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[StaticVar]/CurrNominal:      staticVar[#Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   staticFunc0()[#Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   staticFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   overloadedStaticFunc1()[#Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   overloadedStaticFunc1()[#Double#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   overloadedStaticFunc2({#(x): Int#})[#Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   overloadedStaticFunc2({#(x): Double#})[#Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: extFunc0({#self: &FooStruct#})[#() -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[StaticVar]/CurrNominal:      extStaticProp[#Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   extStaticFunc0()[#Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[Struct]/CurrNominal:         ExtNestedStruct[#FooStruct.ExtNestedStruct#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[Class]/CurrNominal:          ExtNestedClass[#FooStruct.ExtNestedClass#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[Enum]/CurrNominal:           ExtNestedEnum[#FooStruct.ExtNestedEnum#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[TypeAlias]/CurrNominal:      ExtNestedTypealias[#Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: End completions

// FOO_STRUCT_NO_DOT: Begin completions
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc0({#self: &FooStruct#})[#() -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc1({#self: &FooStruct#})[#(Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc2({#self: &FooStruct#})[#(Int, inout b: Double) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc3({#self: &FooStruct#})[#(Int, (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc4({#self: &FooStruct#})[#(Int?, b: Int!, inout c: Int?, inout d: Int!) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc5({#self: &FooStruct#})[#() -> Int?#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc6({#self: &FooStruct#})[#() -> Int!#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc7({#self: &FooStruct#})[#(a: Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc8({#self: &FooStruct#})[#((Int, Int)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc9({#self: &FooStruct#})[#(@autoclosure () -> Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .varargInstanceFunc0({#self: &FooStruct#})[#(Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .varargInstanceFunc1({#self: &FooStruct#})[#(Float, v: Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .varargInstanceFunc2({#self: &FooStruct#})[#(Float, b: Double, v: Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc1({#self: &FooStruct#})[#() -> Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc1({#self: &FooStruct#})[#() -> Double#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc2({#self: &FooStruct#})[#(Int) -> Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc2({#self: &FooStruct#})[#(Double) -> Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .builderFunc1({#self: &FooStruct#})[#(Int) -> FooStruct#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc1({#self: &FooStruct#})[#() -> () -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc2({#self: &FooStruct#})[#() -> (a: Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc3({#self: &FooStruct#})[#(Int) -> () -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc4({#self: &FooStruct#})[#(Int) -> (b: Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedVoidFunc5({#self: &FooStruct#})[#(Int) -> (b: Int, (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc1({#self: &FooStruct#})[#() -> () -> String#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc2({#self: &FooStruct#})[#() -> (a: Int) -> String#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc3({#self: &FooStruct#})[#(Int) -> () -> String#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc4({#self: &FooStruct#})[#(Int) -> (b: Int) -> String#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .curriedStringFunc5({#self: &FooStruct#})[#(Int) -> (b: Int, (Float, Double)) -> String#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorVoidFunc1({#self: &FooStruct#})[#(Int, b: Float) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorVoidFunc2({#self: &FooStruct#})[#(Int, b: Float, c: Double) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorVoidFunc3({#self: &FooStruct#})[#(Int, b: (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorStringFunc1({#self: &FooStruct#})[#(Int, b: Float) -> String#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorStringFunc2({#self: &FooStruct#})[#(Int, b: Float, c: Double) -> String#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorStringFunc3({#self: &FooStruct#})[#(Int, b: (Float, Double)) -> String#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[Struct]/CurrNominal:         .NestedStruct[#FooStruct.NestedStruct#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[Class]/CurrNominal:          .NestedClass[#FooStruct.NestedClass#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[Enum]/CurrNominal:           .NestedEnum[#FooStruct.NestedEnum#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[TypeAlias]/CurrNominal:      .NestedTypealias[#Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[StaticVar]/CurrNominal:      .staticVar[#Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   .staticFunc0()[#Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   .staticFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   .overloadedStaticFunc1()[#Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   .overloadedStaticFunc1()[#Double#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   .overloadedStaticFunc2({#(x): Int#})[#Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   .overloadedStaticFunc2({#(x): Double#})[#Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[Constructor]/CurrNominal:    ({#lazyInstanceVar: Int?#}, {#instanceVar: Int#})[#FooStruct#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[Constructor]/CurrNominal:    ()[#FooStruct#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .extFunc0({#self: &FooStruct#})[#() -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[StaticVar]/CurrNominal:      .extStaticProp[#Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[StaticMethod]/CurrNominal:   .extStaticFunc0()[#Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[Struct]/CurrNominal:         .ExtNestedStruct[#FooStruct.ExtNestedStruct#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[Class]/CurrNominal:          .ExtNestedClass[#FooStruct.ExtNestedClass#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[Enum]/CurrNominal:           .ExtNestedEnum[#FooStruct.ExtNestedEnum#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[TypeAlias]/CurrNominal:      .ExtNestedTypealias[#Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: End completions

func testObjectExpr() {
  fooObject.#^FOO_OBJECT_DOT_1^#
}

func testDotDotTokenSplitWithCodeCompletion() {
  fooObject.#^FOO_OBJECT_DOT_2^#.bar
}

func testObjectExprBuilderStyle1() {
  fooObject
    .#^FOO_OBJECT_DOT_3^#
}

func testObjectExprBuilderStyle2() {
  fooObject
    .builderFunc1(42).#^FOO_OBJECT_DOT_4^#
}

func testObjectExprBuilderStyle3() {
  fooObject
    .builderFunc1(42)
    .#^FOO_OBJECT_DOT_5^#
}

func testObjectExprWithoutDot() {
  fooObject#^FOO_OBJECT_NO_DOT_1^#
}

func testObjectExprWithoutSpaceAfterCodeCompletion() {
  fooObject#^FOO_OBJECT_NO_DOT_2^#.bar
}

func testMetatypeExpr() {
  FooStruct.#^FOO_STRUCT_DOT_1^#
}

func testMetatypeExprWithoutDot() {
  FooStruct#^FOO_STRUCT_NO_DOT_1^#
}

func testCurriedFunc() {
  fooObject.curriedVoidFunc1()#^CF1^#
// CF1: Begin completions
// CF1-NEXT: Pattern/ExprSpecific: ()[#Void#]{{; name=.+$}}
// CF1-NEXT: End completions

  fooObject.curriedVoidFunc2()#^CF2^#
// CF2: Begin completions
// CF2-NEXT: Pattern/ExprSpecific: ({#a: Int#})[#Void#]{{; name=.+$}}
// CF2-NEXT: End completions

  fooObject.curriedVoidFunc3(42)#^CF3^#
// CF3: Begin completions
// CF3-NEXT: Pattern/ExprSpecific: ()[#Void#]{{; name=.+$}}
// CF3-NEXT: End completions

  fooObject.curriedVoidFunc4(42)#^CF4^#
// CF4: Begin completions
// CF4-NEXT: Pattern/ExprSpecific: ({#b: Int#})[#Void#]{{; name=.+$}}
// CF4-NEXT: End completions
}

func testImplicitlyCurriedFunc(fs: FooStruct) {
  FooStruct.instanceFunc0(&fs)#^IMPLICITLY_CURRIED_FUNC_0^#
// IMPLICITLY_CURRIED_FUNC_0: Begin completions
// IMPLICITLY_CURRIED_FUNC_0-NEXT: Pattern/ExprSpecific: ()[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_FUNC_0-NEXT: End completions

  FooStruct.instanceFunc1(&fs)#^IMPLICITLY_CURRIED_FUNC_1^#
// IMPLICITLY_CURRIED_FUNC_1: Begin completions
// IMPLICITLY_CURRIED_FUNC_1-NEXT: Pattern/ExprSpecific: ({#Int#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_FUNC_1-NEXT: End completions

  FooStruct.instanceFunc2(&fs)#^IMPLICITLY_CURRIED_FUNC_2^#
// IMPLICITLY_CURRIED_FUNC_2: Begin completions
// IMPLICITLY_CURRIED_FUNC_2-NEXT: Pattern/ExprSpecific: ({#Int#}, {#b: &Double#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_FUNC_2-NEXT: End completions

  FooStruct.varargInstanceFunc0(&fs)#^IMPLICITLY_CURRIED_VARARG_FUNC_0^#
// IMPLICITLY_CURRIED_VARARG_FUNC_0: Begin completions
// IMPLICITLY_CURRIED_VARARG_FUNC_0-NEXT: Pattern/ExprSpecific: ({#Int...#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_0-NEXT: End completions

  FooStruct.varargInstanceFunc1(&fs)#^IMPLICITLY_CURRIED_VARARG_FUNC_1^#
// IMPLICITLY_CURRIED_VARARG_FUNC_1: Begin completions
// IMPLICITLY_CURRIED_VARARG_FUNC_1-NEXT: Pattern/ExprSpecific: ({#Float#}, {#v: Int...#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_1-NEXT: End completions

  FooStruct.varargInstanceFunc2(&fs)#^IMPLICITLY_CURRIED_VARARG_FUNC_2^#
// IMPLICITLY_CURRIED_VARARG_FUNC_2: Begin completions
// IMPLICITLY_CURRIED_VARARG_FUNC_2-NEXT: Pattern/ExprSpecific: ({#Float#}, {#b: Double#}, {#v: Int...#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_2-NEXT: End completions

  // This call is ambiguous, and the expression is invalid.
  // Ensure that we don't suggest to call the result.
  FooStruct.overloadedInstanceFunc1(&fs)#^IMPLICITLY_CURRIED_OVERLOADED_FUNC_1^#
// IMPLICITLY_CURRIED_OVERLOADED_FUNC_1: found code completion token
// IMPLICITLY_CURRIED_OVERLOADED_FUNC_1-NOT: Begin completions

  // This call is ambiguous, and the expression is invalid.
  // Ensure that we don't suggest to call the result.
  FooStruct.overloadedInstanceFunc2(&fs)#^IMPLICITLY_CURRIED_OVERLOADED_FUNC_2^#
// IMPLICITLY_CURRIED_OVERLOADED_FUNC_2: found code completion token
// IMPLICITLY_CURRIED_OVERLOADED_FUNC_2-NOT: Begin completions

  FooStruct.curriedVoidFunc1(&fs)#^IMPLICITLY_CURRIED_CURRIED_FUNC_1^#
// IMPLICITLY_CURRIED_CURRIED_FUNC_1: Begin completions
// IMPLICITLY_CURRIED_CURRIED_FUNC_1-NEXT: Pattern/ExprSpecific: ()[#() -> ()#]{{; name=.+$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_1-NEXT: End completions

  FooStruct.curriedVoidFunc2(&fs)#^IMPLICITLY_CURRIED_CURRIED_FUNC_2^#
// IMPLICITLY_CURRIED_CURRIED_FUNC_2: Begin completions
// IMPLICITLY_CURRIED_CURRIED_FUNC_2-NEXT: Pattern/ExprSpecific: ()[#(a: Int) -> ()#]{{; name=.+$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_2-NEXT: End completions

  FooStruct.curriedVoidFunc3(&fs)#^IMPLICITLY_CURRIED_CURRIED_FUNC_3^#
// IMPLICITLY_CURRIED_CURRIED_FUNC_3: Begin completions
// IMPLICITLY_CURRIED_CURRIED_FUNC_3-NEXT: Pattern/ExprSpecific: ({#Int#})[#() -> ()#]{{; name=.+$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_3-NEXT: End completions

  FooStruct.curriedVoidFunc4(&fs)#^IMPLICITLY_CURRIED_CURRIED_FUNC_4^#
// IMPLICITLY_CURRIED_CURRIED_FUNC_4: Begin completions
// IMPLICITLY_CURRIED_CURRIED_FUNC_4-NEXT: Pattern/ExprSpecific: ({#Int#})[#(b: Int) -> ()#]{{; name=.+$}}
// IMPLICITLY_CURRIED_CURRIED_FUNC_4-NEXT: End completions
}

//===---
//===--- Test that we can complete inside 'case'.
//===---

func testSwitch1() {
  switch fooObject {
    case #^IN_SWITCH_CASE_1^#
  }

  switch fooObject {
    case 1, #^IN_SWITCH_CASE_2^#
  }

  switch unknown_var {
    case #^IN_SWITCH_CASE_3^#
  }

  switch {
    case #^IN_SWITCH_CASE_4^#
  }
}

// IN_SWITCH_CASE: Begin completions
// IN_SWITCH_CASE-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// IN_SWITCH_CASE-DAG: Decl[Struct]/CurrModule:    FooStruct[#FooStruct#]{{; name=.+$}}
// IN_SWITCH_CASE: End completions

//===--- Helper types that are used in this test

struct FooGenericStruct<T> {
  init(t: T) { fooInstanceVarT = t }

  var fooInstanceVarT: T
  var fooInstanceVarTBrackets: [T]
  mutating
  func fooVoidInstanceFunc1(a: T) {}
  mutating
  func fooTInstanceFunc1(a: T) -> T { return a }
  mutating
  func fooUInstanceFunc1<U>(a: U) -> U { return a }

  static var fooStaticVarT: Int = 0
  static var fooStaticVarTBrackets: [Int] = [0]
  static func fooVoidStaticFunc1(a: T) {}
  static func fooTStaticFunc1(a: T) -> T { return a }
  static func fooUInstanceFunc1<U>(a: U) -> U { return a }
}

class FooClass {
  var fooClassInstanceVar = 0
  func fooClassInstanceFunc0() {}
  func fooClassInstanceFunc1(a: Int) {}
}

enum FooEnum {
}

protocol FooProtocol {
  var fooInstanceVar1: Int { get set }
  var fooInstanceVar2: Int { get }
  typealias FooTypeAlias1
  func fooInstanceFunc0() -> Double
  func fooInstanceFunc1(a: Int) -> Double
  subscript(i: Int) -> Double { get set }
}

class FooProtocolImpl : FooProtocol {
  var fooInstanceVar1 = 0
  val fooInstanceVar2 = 0
  typealias FooTypeAlias1 = Float
  init() {}
  func fooInstanceFunc0() -> Double {
    return 0.0
  }
  func fooInstanceFunc1(a: Int) -> Double {
    return Double(a)
  }
  subscript(i: Int) -> Double {
    return 0.0
  }
}

protocol FooExProtocol : FooProtocol {
  func fooExInstanceFunc0() -> Double
}

protocol BarProtocol {
  var barInstanceVar: Int { get set }
  typealias BarTypeAlias1
  func barInstanceFunc0() -> Double
  func barInstanceFunc1(a: Int) -> Double
}

protocol BarExProtocol : BarProtocol {
  func barExInstanceFunc0() -> Double
}

protocol BazProtocol {
  func bazInstanceFunc0() -> Double
}

typealias BarBazProtocolComposition = protocol<BarProtocol, BazProtocol>

var fooProtocolInstance: FooProtocol = FooProtocolImpl()
var fooBarProtocolInstance: protocol<FooProtocol, BarProtocol>
var fooExBarExProtocolInstance: protocol<FooExProtocol, BarExProtocol>

typealias FooTypealias = Int

//===--- Test that we can code complete inside function calls.

func testInsideFunctionCall0() {
  ERROR(#^INSIDE_FUNCTION_CALL_0^#
// INSIDE_FUNCTION_CALL_0: Begin completions
// INSIDE_FUNCTION_CALL_0-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_0: End completions
}

func testInsideFunctionCall1() {
  var a = FooStruct()
  a.instanceFunc0(#^INSIDE_FUNCTION_CALL_1^#
// There should be no other results here because the function call
// unambigously resolves to overload that takes 0 arguments.
// INSIDE_FUNCTION_CALL_1: Begin completions
// INSIDE_FUNCTION_CALL_1-NEXT: Pattern/ExprSpecific: ['('])[#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_1-NEXT: End completions
}

func testInsideFunctionCall2() {
  var a = FooStruct()
  a.instanceFunc1(#^INSIDE_FUNCTION_CALL_2^#
// INSIDE_FUNCTION_CALL_2: Begin completions
// FIXME: we should print the non-API param name rdar://20962472
// INSIDE_FUNCTION_CALL_2-DAG: Pattern/ExprSpecific:       ['(']{#Int#})[#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_2-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_2: End completions
}

func testInsideFunctionCall3() {
  FooStruct().instanceFunc1(42, #^INSIDE_FUNCTION_CALL_3^#
// INSIDE_FUNCTION_CALL_3: Begin completions
// FIXME: There should be no results here because the function call
// unambigously resolves to overload that takes 1 argument.
// INSIDE_FUNCTION_CALL_3-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_3: End completions
}

func testInsideFunctionCall4() {
  var a = FooStruct()
  a.instanceFunc2(#^INSIDE_FUNCTION_CALL_4^#
// INSIDE_FUNCTION_CALL_4: Begin completions
// FIXME: we should print the non-API param name rdar://20962472
// INSIDE_FUNCTION_CALL_4-DAG: Pattern/ExprSpecific:       ['(']{#Int#}, {#b: &Double#})[#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_4-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_4: End completions
}

func testInsideFunctionCall5() {
  FooStruct().instanceFunc2(42, #^INSIDE_FUNCTION_CALL_5^#
// INSIDE_FUNCTION_CALL_5: Begin completions
// INSIDE_FUNCTION_CALL_5-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_5: End completions
}

func testInsideFunctionCall6() {
  var a = FooStruct()
  a.instanceFunc7(#^INSIDE_FUNCTION_CALL_6^#
// INSIDE_FUNCTION_CALL_6: Begin completions
// INSIDE_FUNCTION_CALL_6-NEXT: Pattern/ExprSpecific: ['(']{#a: Int#})[#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_6-NEXT: End completions
}

func testInsideFunctionCall7() {
  var a = FooStruct()
  a.instanceFunc8(#^INSIDE_FUNCTION_CALL_7^#
// INSIDE_FUNCTION_CALL_7: Begin completions
// FIXME: we should print the non-API param name rdar://20962472
// INSIDE_FUNCTION_CALL_7: Pattern/ExprSpecific: ['(']{#(Int, Int)#})[#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_7: End completions
}

func testInsideVarargFunctionCall1() {
  var a = FooStruct()
  a.varargInstanceFunc0(#^INSIDE_VARARG_FUNCTION_CALL_1^#
// INSIDE_VARARG_FUNCTION_CALL_1: Begin completions
// FIXME: we should print the non-API param name rdar://20962472
// INSIDE_VARARG_FUNCTION_CALL_1-DAG: Pattern/ExprSpecific:       ['(']{#Int...#})[#Void#]{{; name=.+$}}
// INSIDE_VARARG_FUNCTION_CALL_1-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_VARARG_FUNCTION_CALL_1: End completions
}

func testInsideVarargFunctionCall2() {
  FooStruct().varargInstanceFunc0(42, #^INSIDE_VARARG_FUNCTION_CALL_2^#
// INSIDE_VARARG_FUNCTION_CALL_2: Begin completions
// INSIDE_VARARG_FUNCTION_CALL_2-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_VARARG_FUNCTION_CALL_2: End completions
}

func testInsideVarargFunctionCall3() {
  FooStruct().varargInstanceFunc0(42, 4242, #^INSIDE_VARARG_FUNCTION_CALL_3^#
// INSIDE_VARARG_FUNCTION_CALL_3: Begin completions
// INSIDE_VARARG_FUNCTION_CALL_3-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_VARARG_FUNCTION_CALL_3: End completions
}

func testInsideOverloadedFunctionCall1() {
  var a = FooStruct()
  a.overloadedInstanceFunc2(#^INSIDE_OVERLOADED_FUNCTION_CALL_1^#
// INSIDE_OVERLOADED_FUNCTION_CALL_1: Begin completions
// FIXME: produce call patterns here.
// INSIDE_OVERLOADED_FUNCTION_CALL_1-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_OVERLOADED_FUNCTION_CALL_1: End completions
}

func testInsideCurriedFunctionCall1() {
  var a = FooStruct()
  a.curriedVoidFunc4(42)(#^INSIDE_CURRIED_FUNCTION_CALL_1^#
// INSIDE_CURRIED_FUNCTION_CALL_1: Begin completions
// INSIDE_CURRIED_FUNCTION_CALL_1-DAG: Pattern/ExprSpecific: ['(']{#b: Int#})[#Void#]{{; name=.+$}}
// INSIDE_CURRIED_FUNCTION_CALL_1: End completions
}

func testInsideFunctionCallOnClassInstance1(a: FooClass) {
  a.fooClassInstanceFunc1(#^INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1^#
// INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1: Begin completions
// FIXME: we should print the non-API param name rdar://20962472
// INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1-DAG: Pattern/ExprSpecific:       ['(']{#Int#})[#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1: End completions
}

//===--- Variables that have function types.

class FuncTypeVars {
  var funcVar1: () -> Double
  var funcVar2: (a: Int) -> Double
}

var funcTypeVarsObject: FuncTypeVars
func testFuncTypeVars() {
  funcTypeVarsObject.funcVar1#^VF1^#
// VF1: Begin completions
// VF1-NEXT: Pattern/ExprSpecific: ()[#Double#]{{; name=.+$}}
// VF1-NEXT: End completions

  funcTypeVarsObject.funcVar2#^VF2^#
// VF2: Begin completions
// VF2-NEXT: Pattern/ExprSpecific: ({#a: Int#})[#Double#]{{; name=.+$}}
// VF2-NEXT: End completions
}

//===--- Check that we look into base classes.

class MembersBase {
  var baseVar = 0
  func baseInstanceFunc() {}
  class func baseStaticFunc() {}
}

class MembersDerived : MembersBase {
  var derivedVar = 0
  func derivedInstanceFunc() {}
  class func derivedStaticFunc() {}
}

var membersDerived: MembersDerived
func testLookInBase() {
  membersDerived.#^BASE_MEMBERS^#
// BASE_MEMBERS: Begin completions
// BASE_MEMBERS-NEXT: Decl[InstanceVar]/CurrNominal:    derivedVar[#Int#]{{; name=.+$}}
// BASE_MEMBERS-NEXT: Decl[InstanceMethod]/CurrNominal: derivedInstanceFunc()[#Void#]{{; name=.+$}}
// BASE_MEMBERS-NEXT: Decl[InstanceVar]/Super:          baseVar[#Int#]{{; name=.+$}}
// BASE_MEMBERS-NEXT: Decl[InstanceMethod]/Super:       baseInstanceFunc()[#Void#]{{; name=.+$}}
// BASE_MEMBERS-NEXT: End completions
}

func testLookInBaseStatic() {
  MembersDerived.#^BASE_MEMBERS_STATIC^#
// BASE_MEMBERS_STATIC: Begin completions
// BASE_MEMBERS_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: derivedInstanceFunc({#self: MembersDerived#})[#() -> Void#]{{; name=.+$}}
// BASE_MEMBERS_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   derivedStaticFunc()[#Void#]{{; name=.+$}}
// BASE_MEMBERS_STATIC-NEXT: Decl[InstanceMethod]/Super:       baseInstanceFunc({#self: MembersBase#})[#() -> Void#]{{; name=.+$}}
// BASE_MEMBERS_STATIC-NEXT: Decl[StaticMethod]/Super:         baseStaticFunc()[#Void#]{{; name=.+$}}
// BASE_MEMBERS_STATIC-NEXT: End completions
}

//===--- Check that we can look into protocols.

func testLookInProtoNoDot1() {
  fooProtocolInstance#^PROTO_MEMBERS_NO_DOT_1^#
// PROTO_MEMBERS_NO_DOT_1: Begin completions
// PROTO_MEMBERS_NO_DOT_1-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVar1[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_1-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVar2[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_1-NEXT: Decl[InstanceMethod]/CurrNominal: .fooInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_1-NEXT: Decl[InstanceMethod]/CurrNominal: .fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_1-NEXT: Decl[Subscript]/CurrNominal:      [{#Int#}][#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_1-NEXT: End completions
}

func testLookInProtoNoDot2() {
  fooBarProtocolInstance#^PROTO_MEMBERS_NO_DOT_2^#
// PROTO_MEMBERS_NO_DOT_2: Begin completions
// PROTO_MEMBERS_NO_DOT_2-NEXT: Decl[InstanceVar]/CurrNominal:    .barInstanceVar[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_2-NEXT: Decl[InstanceMethod]/CurrNominal: .barInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_2-NEXT: Decl[InstanceMethod]/CurrNominal: .barInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_2-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVar1[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_2-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVar2[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_2-NEXT: Decl[InstanceMethod]/CurrNominal: .fooInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_2-NEXT: Decl[InstanceMethod]/CurrNominal: .fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_2-NEXT: Decl[Subscript]/CurrNominal:      [{#Int#}][#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_2-NEXT: End completions
}

func testLookInProtoNoDot3() {
  fooExBarExProtocolInstance#^PROTO_MEMBERS_NO_DOT_3^#
// PROTO_MEMBERS_NO_DOT_3: Begin completions
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[InstanceVar]/Super:          .barInstanceVar[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[InstanceMethod]/Super:       .barInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[InstanceMethod]/Super:       .barInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[InstanceMethod]/CurrNominal: .barExInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[InstanceVar]/Super:          .fooInstanceVar1[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[InstanceVar]/Super:          .fooInstanceVar2[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[InstanceMethod]/Super:       .fooInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[InstanceMethod]/Super:       .fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[Subscript]/Super:            [{#Int#}][#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: Decl[InstanceMethod]/CurrNominal: .fooExInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_NO_DOT_3-NEXT: End completions
}

func testLookInProto1() {
  fooProtocolInstance.#^PROTO_MEMBERS_1^#
// PROTO_MEMBERS_1: Begin completions
// PROTO_MEMBERS_1-NEXT: Decl[InstanceVar]/CurrNominal:    fooInstanceVar1[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_1-NEXT: Decl[InstanceVar]/CurrNominal:    fooInstanceVar2[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_1-NEXT: End completions
}

func testLookInProto2() {
  fooBarProtocolInstance.#^PROTO_MEMBERS_2^#
// PROTO_MEMBERS_2: Begin completions
// PROTO_MEMBERS_2-NEXT: Decl[InstanceVar]/CurrNominal:    barInstanceVar[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal: barInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal: barInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_2-NEXT: Decl[InstanceVar]/CurrNominal:    fooInstanceVar1[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_2-NEXT: Decl[InstanceVar]/CurrNominal:    fooInstanceVar2[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_2-NEXT: End completions
}

func testLookInProto3() {
  fooExBarExProtocolInstance.#^PROTO_MEMBERS_3^#
// PROTO_MEMBERS_3: Begin completions
// PROTO_MEMBERS_3-NEXT: Decl[InstanceVar]/Super:          barInstanceVar[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_3-NEXT: Decl[InstanceMethod]/Super:       barInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_3-NEXT: Decl[InstanceMethod]/Super:       barInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_3-NEXT: Decl[InstanceMethod]/CurrNominal: barExInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_3-NEXT: Decl[InstanceVar]/Super:          fooInstanceVar1[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_3-NEXT: Decl[InstanceVar]/Super:          fooInstanceVar2[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_3-NEXT: Decl[InstanceMethod]/Super:       fooInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_3-NEXT: Decl[InstanceMethod]/Super:       fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_3-NEXT: Decl[InstanceMethod]/CurrNominal: fooExInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_3-NEXT: End completions
}

func testLookInProto4(a: protocol<FooProtocol, BarBazProtocolComposition>) {
  a.#^PROTO_MEMBERS_4^#
// PROTO_MEMBERS_4: Begin completions
// PROTO_MEMBERS_4-DAG: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_4-DAG: Decl[InstanceMethod]/CurrNominal: barInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_4-DAG: Decl[InstanceMethod]/CurrNominal: bazInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_4: End completions
}

//===--- Check that we can resolve function parameters.

func testResolveFuncParam1(fs: FooStruct) {
  fs.#^RESOLVE_FUNC_PARAM_1^#
}

class TestResolveFuncParam2 {
  func testResolveFuncParam2a(fs: FooStruct) {
    fs.#^RESOLVE_FUNC_PARAM_2^#
  }
}

func testResolveFuncParam3<Foo : FooProtocol>(foo: Foo) {
  foo.#^RESOLVE_FUNC_PARAM_3^#
// RESOLVE_FUNC_PARAM_3: Begin completions
// RESOLVE_FUNC_PARAM_3-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar1[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_3-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar2[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_3-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_3-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_3-NEXT: End completions
}

func testResolveFuncParam4<FooBar : protocol<FooProtocol, BarProtocol>>(fooBar: FooBar) {
  fooBar.#^RESOLVE_FUNC_PARAM_4^#
// RESOLVE_FUNC_PARAM_4: Begin completions
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceVar]/Super:    barInstanceVar[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceMethod]/Super: barInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceMethod]/Super: barInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar1[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar2[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: End completions
}

func testResolveFuncParam5<FooExBarEx : protocol<FooExProtocol, BarExProtocol>>(a: FooExBarEx) {
  a.#^RESOLVE_FUNC_PARAM_5^#
// RESOLVE_FUNC_PARAM_5: Begin completions
// RESOLVE_FUNC_PARAM_5-NEXT: Decl[InstanceVar]/Super:    barInstanceVar[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_5-NEXT: Decl[InstanceMethod]/Super: barInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_5-NEXT: Decl[InstanceMethod]/Super: barInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_5-NEXT: Decl[InstanceMethod]/Super: barExInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_5-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar1[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_5-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar2[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_5-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_5-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_5-NEXT: Decl[InstanceMethod]/Super: fooExInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_5-NEXT: End completions
}

func testResolveFuncParam6<Foo : FooProtocol where Foo : FooClass>(foo: Foo) {
  foo.#^RESOLVE_FUNC_PARAM_6^#
// RESOLVE_FUNC_PARAM_6: Begin completions
// RESOLVE_FUNC_PARAM_6-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar1[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_6-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar2[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_6-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_6-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_6-NEXT: Decl[InstanceVar]/Super:    fooClassInstanceVar[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_6-NEXT: Decl[InstanceMethod]/Super: fooClassInstanceFunc0()[#Void#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_6-NEXT: Decl[InstanceMethod]/Super: fooClassInstanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_6-NEXT: End completions
}

class TestResolveConstructorParam1 {
  init(fs: FooStruct) {
    fs.#^RESOLVE_CONSTRUCTOR_PARAM_1^#
  }
}

class TestResolveConstructorParam2 {
  init<Foo : FooProtocol>(foo: Foo) {
    foo.#^RESOLVE_CONSTRUCTOR_PARAM_2^#
// RESOLVE_CONSTRUCTOR_PARAM_2: Begin completions
// RESOLVE_CONSTRUCTOR_PARAM_2-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar1[#Int#]{{; name=.+$}}
// RESOLVE_CONSTRUCTOR_PARAM_2-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar2[#Int#]{{; name=.+$}}
// RESOLVE_CONSTRUCTOR_PARAM_2-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_CONSTRUCTOR_PARAM_2-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_CONSTRUCTOR_PARAM_2-NEXT: End completions
  }
}

class TestResolveConstructorParam3<Foo : FooProtocol> {
  init(foo: Foo) {
    foo.#^RESOLVE_CONSTRUCTOR_PARAM_3^#
// RESOLVE_CONSTRUCTOR_PARAM_3: Begin completions
// RESOLVE_CONSTRUCTOR_PARAM_3-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar1[#Int#]{{; name=.+$}}
// RESOLVE_CONSTRUCTOR_PARAM_3-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar2[#Int#]{{; name=.+$}}
// RESOLVE_CONSTRUCTOR_PARAM_3-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_CONSTRUCTOR_PARAM_3-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_CONSTRUCTOR_PARAM_3-NEXT: End completions
  }
}

//===--- Check that we can handle ParenPattern in function arguments.

struct FuncParenPattern {
  init(_: Int) {}
  init(_: (Int, Int)) {}

  mutating
  func instanceFunc(_: Int) {}

  subscript(_: Int) -> Int {
    get {
      return 0
    }
  }
}

func testFuncParenPattern1(fpp: FuncParenPattern) {
  fpp#^FUNC_PAREN_PATTERN_1^#
// FUNC_PAREN_PATTERN_1: Begin completions
// FUNC_PAREN_PATTERN_1-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#Int#})[#Void#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_1-NEXT: Decl[Subscript]/CurrNominal: [{#Int#}][#Int#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_1-NEXT: End completions
}

func testFuncParenPattern2(fpp: FuncParenPattern) {
  FuncParenPattern#^FUNC_PAREN_PATTERN_2^#
// FUNC_PAREN_PATTERN_2: Begin completions
// FUNC_PAREN_PATTERN_2-NEXT: Decl[Constructor]/CurrNominal: ({#Int#})[#FuncParenPattern#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_2-NEXT: Decl[Constructor]/CurrNominal: ({#(Int, Int)#})[#FuncParenPattern#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_2-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#self: &FuncParenPattern#})[#(Int) -> Void#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_2-NEXT: End completions
}

func testFuncParenPattern3(var fpp: FuncParenPattern) {
  fpp.instanceFunc#^FUNC_PAREN_PATTERN_3^#
// FUNC_PAREN_PATTERN_3: Begin completions
// FUNC_PAREN_PATTERN_3-NEXT: Pattern/ExprSpecific: ({#Int#})[#Void#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_3-NEXT: End completions
}

//===--- Check that we can code complete after function calls

struct SomeBuilder {
  init(a: Int) {}
  func doFoo() -> SomeBuilder { return self }
  func doBar() -> SomeBuilder { return self }
  func doBaz(z: Double) -> SomeBuilder { return self }
}

func testChainedCalls1() {
  SomeBuilder(42)#^CHAINED_CALLS_1^#
// CHAINED_CALLS_1: Begin completions
// CHAINED_CALLS_1-DAG: Decl[InstanceMethod]/CurrNominal: .doFoo()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_1-DAG: Decl[InstanceMethod]/CurrNominal: .doBar()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_1-DAG: Decl[InstanceMethod]/CurrNominal: .doBaz({#(z): Double#})[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_1: End completions
}

func testChainedCalls2() {
  SomeBuilder(42).doFoo()#^CHAINED_CALLS_2^#
// CHAINED_CALLS_2: Begin completions
// CHAINED_CALLS_2-DAG: Decl[InstanceMethod]/CurrNominal: .doFoo()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_2-DAG: Decl[InstanceMethod]/CurrNominal: .doBar()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_2-DAG: Decl[InstanceMethod]/CurrNominal: .doBaz({#(z): Double#})[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_2: End completions
}

func testChainedCalls3() {
  // doBaz() takes a Double.  Check that we can recover.
  SomeBuilder(42).doFoo().doBaz(SomeBuilder(24))#^CHAINED_CALLS_3^#
// CHAINED_CALLS_3: Begin completions
// CHAINED_CALLS_3-DAG: Decl[InstanceMethod]/CurrNominal: .doFoo()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_3-DAG: Decl[InstanceMethod]/CurrNominal: .doBar()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_3-DAG: Decl[InstanceMethod]/CurrNominal: .doBaz({#z: Double#})[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_3: End completions
}

//===--- Check that we can code complete expressions that have generic parameters

func testResolveGenericParams1() {
  FooGenericStruct<FooStruct>()#^RESOLVE_GENERIC_PARAMS_1^#
// RESOLVE_GENERIC_PARAMS_1: Begin completions
// RESOLVE_GENERIC_PARAMS_1-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarT[#FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarTBrackets[#[FooStruct]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#(a): FooStruct#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#(a): FooStruct#})[#FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1-NEXT: End completions

  FooGenericStruct<FooStruct>#^RESOLVE_GENERIC_PARAMS_1_STATIC^#
// RESOLVE_GENERIC_PARAMS_1_STATIC: Begin completions
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: FooStruct#})[#FooGenericStruct<FooStruct>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#FooStruct -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#FooStruct -> FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): FooStruct#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): FooStruct#})[#FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: End completions
}

func testResolveGenericParams2<Foo : FooProtocol>(foo: Foo) {
  FooGenericStruct<Foo>()#^RESOLVE_GENERIC_PARAMS_2^#
// RESOLVE_GENERIC_PARAMS_2: Begin completions
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarT[#Foo#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarTBrackets[#[Foo]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#(a): Foo#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#(a): Foo#})[#Foo#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: End completions

  FooGenericStruct<Foo>#^RESOLVE_GENERIC_PARAMS_2_STATIC^#
// RESOLVE_GENERIC_PARAMS_2_STATIC: Begin completions
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: Foo#})[#FooGenericStruct<Foo>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<Foo>#})[#Foo -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<Foo>#})[#Foo -> Foo#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<Foo>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): Foo#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): Foo#})[#Foo#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: End completions
}

struct TestResolveGenericParams3_4<T> {
  func testResolveGenericParams3() {
    FooGenericStruct<FooStruct>()#^RESOLVE_GENERIC_PARAMS_3^#
// RESOLVE_GENERIC_PARAMS_3: Begin completions
// RESOLVE_GENERIC_PARAMS_3-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarT[#FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarTBrackets[#[FooStruct]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#(a): FooStruct#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#(a): FooStruct#})[#FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3-NEXT: End completions

    FooGenericStruct<FooStruct>#^RESOLVE_GENERIC_PARAMS_3_STATIC^#
// RESOLVE_GENERIC_PARAMS_3_STATIC: Begin completions, 9 items
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: FooStruct#})[#FooGenericStruct<FooStruct>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#FooStruct -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#FooStruct -> FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): FooStruct#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): FooStruct#})[#FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: End completions
  }

  func testResolveGenericParams4(t: T) {
    FooGenericStruct<T>(t)#^RESOLVE_GENERIC_PARAMS_4^#
// RESOLVE_GENERIC_PARAMS_4: Begin completions
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarT[#T#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarTBrackets[#[T]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#(a): T#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#(a): T#})[#T#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: End completions

    FooGenericStruct<T>#^RESOLVE_GENERIC_PARAMS_4_STATIC^#
// RESOLVE_GENERIC_PARAMS_4_STATIC: Begin completions
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: T#})[#FooGenericStruct<T>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<T>#})[#T -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<T>#})[#T -> T#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<T>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): T#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): T#})[#T#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: End completions
  }

  func testResolveGenericParams5<U>(u: U) {
    FooGenericStruct<U>(u)#^RESOLVE_GENERIC_PARAMS_5^#
// RESOLVE_GENERIC_PARAMS_5: Begin completions
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarT[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarTBrackets[#[U]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#(a): U#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: End completions

    FooGenericStruct<U>#^RESOLVE_GENERIC_PARAMS_5_STATIC^#
// RESOLVE_GENERIC_PARAMS_5_STATIC: Begin completions
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: U#})[#FooGenericStruct<U>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<U>#})[#U -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<U>#})[#U -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<U>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): U#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: End completions
  }
}

func testResolveGenericParamsError1() {
  // There is no type 'Foo'.  Check that we don't crash.
  // FIXME: we could also display correct completion results here, because
  // swift does not have specialization, and the set of completion results does
  // not depend on the generic type argument.
  FooGenericStruct<NotDefinedType>()#^RESOLVE_GENERIC_PARAMS_ERROR_1^#
// RESOLVE_GENERIC_PARAMS_ERROR_1: found code completion token
// RESOLVE_GENERIC_PARAMS_ERROR_1-NOT: Begin completions
}

//===--- Check that we can code complete expressions that have unsolved type variables.

class BuilderStyle<T> {
  var count = 0
  func addString(s: String) -> BuilderStyle<T> {
    count++
    return self
  }
  func add(t: T) -> BuilderStyle<T> {
    count++
    return self
  }
  func get() -> Int {
    return count
  }
}

func testTypeCheckWithUnsolvedVariables1() {
  BuilderStyle().#^TC_UNSOLVED_VARIABLES_1^#
}
// TC_UNSOLVED_VARIABLES_1: Begin completions
// TC_UNSOLVED_VARIABLES_1-NEXT: Decl[InstanceVar]/CurrNominal: count[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_1-NEXT: Decl[InstanceMethod]/CurrNominal: addString({#(s): String#})[#BuilderStyle<T>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_1-NEXT: Decl[InstanceMethod]/CurrNominal: add({#(t): T#})[#BuilderStyle<T>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_1-NEXT: Decl[InstanceMethod]/CurrNominal: get()[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_1-NEXT: End completions

func testTypeCheckWithUnsolvedVariables2() {
  BuilderStyle().addString("abc").#^TC_UNSOLVED_VARIABLES_2^#
}
// TC_UNSOLVED_VARIABLES_2: Begin completions
// TC_UNSOLVED_VARIABLES_2-NEXT: Decl[InstanceVar]/CurrNominal:    count[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_2-NEXT: Decl[InstanceMethod]/CurrNominal: addString({#(s): String#})[#BuilderStyle<T>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_2-NEXT: Decl[InstanceMethod]/CurrNominal: add({#(t): T#})[#BuilderStyle<T>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_2-NEXT: Decl[InstanceMethod]/CurrNominal: get()[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_2-NEXT: End completions

func testTypeCheckWithUnsolvedVariables3() {
  BuilderStyle().addString("abc").add(42).#^TC_UNSOLVED_VARIABLES_3^#
}
// TC_UNSOLVED_VARIABLES_3: Begin completions
// TC_UNSOLVED_VARIABLES_3-NEXT: Decl[InstanceVar]/CurrNominal:    count[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_3-NEXT: Decl[InstanceMethod]/CurrNominal: addString({#(s): String#})[#BuilderStyle<Int>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_3-NEXT: Decl[InstanceMethod]/CurrNominal: add({#(t): Int#})[#BuilderStyle<Int>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_3-NEXT: Decl[InstanceMethod]/CurrNominal: get()[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_3-NEXT: End completions

func testTypeCheckNil() {
  nil#^TC_UNSOLVED_VARIABLES_4^#
}
// TC_UNSOLVED_VARIABLES_4-NOT: Begin completions

//===--- Check that we can look up into modules

func testResolveModules1() {
  Swift#^RESOLVE_MODULES_1^#
// RESOLVE_MODULES_1: Begin completions
// RESOLVE_MODULES_1-DAG: Decl[Struct]/OtherModule[Swift]:    .Int8[#Int8#]{{; name=.+$}}
// RESOLVE_MODULES_1-DAG: Decl[Struct]/OtherModule[Swift]:    .Int16[#Int16#]{{; name=.+$}}
// RESOLVE_MODULES_1-DAG: Decl[Struct]/OtherModule[Swift]:    .Int32[#Int32#]{{; name=.+$}}
// RESOLVE_MODULES_1-DAG: Decl[Struct]/OtherModule[Swift]:    .Int64[#Int64#]{{; name=.+$}}
// RESOLVE_MODULES_1-DAG: Decl[Struct]/OtherModule[Swift]:    .Bool[#Bool#]{{; name=.+$}}
// RESOLVE_MODULES_1-DAG: Decl[TypeAlias]/OtherModule[Swift]: .Float32[#Float#]{{; name=.+$}}
// RESOLVE_MODULES_1: End completions
}

//===--- Check that we can complete inside interpolated string literals

func testInterpolatedString1() {
  "\(fooObject.#^INTERPOLATED_STRING_1^#)"
}

//===--- Check protocol extensions

struct WillConformP1 {
}

protocol P1 {
  func reqP1()
}

protocol P2 : P1 {
  func reqP2()
}

protocol P3 : P1, P2 {
}

extension P1 {
  final func extP1() {}
}

extension P2 {
  final func extP2() {}
}

extension P3 {
  final func reqP1() {}
  final func reqP2() {}
  final func extP3() {}
}

extension WillConformP1 : P1 {
  func reqP1() {}
}

struct DidConformP2 : P2 {
  func reqP1() {}
  func reqP2() {}
}

struct DidConformP3 : P3 {
}

func testProtocol1(x: P1) {
  x.#^PROTOCOL_EXT_P1^#
}
// PROTOCOL_EXT_P1: Begin completions
// PROTOCOL_EXT_P1-DAG: Decl[InstanceMethod]/CurrNominal:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P1-DAG: Decl[InstanceMethod]/CurrNominal:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P1: End completions


func testProtocol2(x: P2) {
  x.#^PROTOCOL_EXT_P2^#
}
// PROTOCOL_EXT_P2: Begin completions
// PROTOCOL_EXT_P2-DAG: Decl[InstanceMethod]/Super:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P2-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P2-DAG: Decl[InstanceMethod]/CurrNominal:   reqP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P2-DAG: Decl[InstanceMethod]/CurrNominal:   extP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P2: End completions

func testProtocol3(x: P3) {
  x.#^PROTOCOL_EXT_P3^#
}
// PROTOCOL_EXT_P3: Begin completions

// FIXME: the next two should both be "CurrentNominal"
// PROTOCOL_EXT_P3-DAG: Decl[InstanceMethod]/Super:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P3-DAG: Decl[InstanceMethod]/Super:   reqP2()[#Void#]{{; name=.+$}}

// PROTOCOL_EXT_P3-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P3-DAG: Decl[InstanceMethod]/Super:   extP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P3-DAG: Decl[InstanceMethod]/CurrNominal:   extP3()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P3: End completions

func testConformingConcrete1(x: WillConformP1) {
  x.#^PROTOCOL_EXT_WILLCONFORMP1^#
}
// PROTOCOL_EXT_WILLCONFORMP1: Begin completions
// PROTOCOL_EXT_WILLCONFORMP1-DAG: Decl[InstanceMethod]/CurrNominal:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_WILLCONFORMP1-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_WILLCONFORMP1: End completions

func testConformingConcrete2(x: DidConformP2) {
  x.#^PROTOCOL_EXT_DIDCONFORMP2^#
}
// PROTOCOL_EXT_DIDCONFORMP2: Begin completions
// PROTOCOL_EXT_DIDCONFORMP2-DAG: Decl[InstanceMethod]/CurrNominal:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP2-DAG: Decl[InstanceMethod]/CurrNominal:   reqP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP2-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP2-DAG: Decl[InstanceMethod]/Super:   extP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP2: End completions

func testConformingConcrete3(x: DidConformP3) {
  x.#^PROTOCOL_EXT_DIDCONFORMP3^#
}
// PROTOCOL_EXT_DIDCONFORMP3: Begin completions
// FIXME: the next two should both be "CurrentNominal"
// PROTOCOL_EXT_DIDCONFORMP3-DAG: Decl[InstanceMethod]/Super:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP3-DAG: Decl[InstanceMethod]/Super:   reqP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP3-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP3-DAG: Decl[InstanceMethod]/Super:   extP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP3-DAG: Decl[InstanceMethod]/Super:   extP3()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP3: End completions

func testGenericConforming1<T: P1>(x: T) {
  x.#^PROTOCOL_EXT_GENERICP1^#
}
// PROTOCOL_EXT_GENERICP1: Begin completions
// PROTOCOL_EXT_GENERICP1-DAG: Decl[InstanceMethod]/Super:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP1-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP1: End completions

func testGenericConforming2<T: P2>(x: T) {
  x.#^PROTOCOL_EXT_GENERICP2^#
}
// PROTOCOL_EXT_GENERICP2: Begin completions
// PROTOCOL_EXT_GENERICP2-DAG: Decl[InstanceMethod]/Super:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP2-DAG: Decl[InstanceMethod]/Super:   reqP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP2-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP2-DAG: Decl[InstanceMethod]/Super:   extP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP2: End completions

func testGenericConforming3<T: P3>(x: T) {
  x.#^PROTOCOL_EXT_GENERICP3^#
}
// PROTOCOL_EXT_GENERICP3: Begin completions
// PROTOCOL_EXT_GENERICP3-DAG: Decl[InstanceMethod]/Super:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP3-DAG: Decl[InstanceMethod]/Super:   reqP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP3-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP3-DAG: Decl[InstanceMethod]/Super:   extP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP3-DAG: Decl[InstanceMethod]/Super:   extP3()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_GENERICP3: End completions

struct OnlyMe {}
protocol P4 {
  typealias T
}
extension P4 where Self.T : P1 {
  final func extP4WhenP1() {}
  final var x: Int { return 1 }
  init() {}
}
extension P4 where Self.T : P1 {
  init(x: Int) {}
}
extension P4 where Self.T == OnlyMe {
  final func extP4OnlyMe() {}
  final subscript(x: Int) -> Int { return 2 }
}
struct Concrete1 : P4 {
  typealias T = WillConformP1
}
struct Generic1<S: P1> : P4 {
  typealias T = S
}
struct Concrete2 : P4 {
  typealias T = OnlyMe
}
struct Generic2<S> : P4 {
  typealias T = S
}

func testConstrainedP4(x: P4) {
  x.#^PROTOCOL_EXT_P4^#
}
// PROTOCOL_EXT_P4-NOT: extP4

func testConstrainedConcrete1(x: Concrete1) {
  x.#^PROTOCOL_EXT_CONCRETE1^#
}
func testConstrainedConcrete2(x: Generic1<WillConformP1>) {
  x.#^PROTOCOL_EXT_CONCRETE2^#
}
func testConstrainedGeneric1<S: P1>(x: Generic1<S>) {
  x.#^PROTOCOL_EXT_CONSTRAINED_GENERIC_1^#
}
func testConstrainedGeneric2<S: P4 where S.T : P1>(x: S) {
  x.#^PROTOCOL_EXT_CONSTRAINED_GENERIC_2^#
}
extension Concrete1 {
  func testInsideConstrainedConcrete1_1() {
    #^PROTOCOL_EXT_INSIDE_CONCRETE1_1^#
  }
  func testInsideConstrainedConcrete1_2() {
    self.#^PROTOCOL_EXT_INSIDE_CONCRETE1_2^#
  }
}
// PROTOCOL_EXT_P4_P1: Begin completions
// PROTOCOL_EXT_P4_P1-NOT: extP4OnlyMe()
// PROTOCOL_EXT_P4_P1-DAG: Decl[InstanceMethod]/Super:         extP4WhenP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P4_P1-DAG: Decl[InstanceVar]/Super:         x[#Int#]{{; name=.+$}}
// PROTOCOL_EXT_P4_P1-NOT: extP4OnlyMe()
// PROTOCOL_EXT_P4_P1: End completions

func testConstrainedConcrete3(x: Concrete2) {
  x.#^PROTOCOL_EXT_CONCRETE3^#
}
func testConstrainedConcrete3_sub(x: Concrete2) {
  x#^PROTOCOL_EXT_CONCRETE3_SUB^#
}
func testConstrainedConcrete4(x: Generic2<OnlyMe>) {
  x.#^PROTOCOL_EXT_CONCRETE4^#
}
func testConstrainedGeneric1<S: P4 where S.T == OnlyMe>(x: S) {
  x.#^PROTOCOL_EXT_CONSTRAINED_GENERIC_3^#
}
func testConstrainedGeneric1_sub<S: P4 where S.T == OnlyMe>(x: S) {
  x#^PROTOCOL_EXT_CONSTRAINED_GENERIC_3_SUB^#
}
extension Concrete2 {
  func testInsideConstrainedConcrete2_1() {
    #^PROTOCOL_EXT_INSIDE_CONCRETE2_1^#
  }
  func testInsideConstrainedConcrete2_2() {
    self.#^PROTOCOL_EXT_INSIDE_CONCRETE2_2^#
  }
}
// PROTOCOL_EXT_P4_ONLYME: Begin completions
// PROTOCOL_EXT_P4_ONLYME-NOT: extP4WhenP1()
// PROTOCOL_EXT_P4_ONLYME-NOT: x[#Int#]
// PROTOCOL_EXT_P4_ONLYME-DAG: Decl[InstanceMethod]/Super:    extP4OnlyMe()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P4_ONLYME-NOT: extP4WhenP1()
// PROTOCOL_EXT_P4_ONLYME-NOT: x[#Int#]
// PROTOCOL_EXT_P4_ONLYME: End completions

// PROTOCOL_EXT_P4_ONLYME_SUB: Begin completions
// PROTOCOL_EXT_P4_ONLYME_SUB: Decl[Subscript]/Super:              [{#Int#}][#Int#]{{; name=.+$}}
// PROTOCOL_EXT_P4_ONLYME_SUB: End completions

func testTypealias1() {
  Concrete1.#^PROTOCOL_EXT_TA_1^#
}
func testTypealias1<S: P4 where S.T == WillConformP1>() {
  S.#^PROTOCOL_EXT_TA_2^#
}
// PROTOCOL_EXT_TA: Begin completions
// PROTOCOL_EXT_TA-DAG: Decl[TypeAlias]/{{Super|CurrNominal}}: T
// PROTOCOL_EXT_TA: End completions

func testProtExtInit1() {
  Concrete1(#^PROTOCOL_EXT_INIT_1^#
}
func testProtExtInit2<S: P4 where S.T : P1>() {
  S(#^PROTOCOL_EXT_INIT_2^#
}

// PROTOCOL_EXT_INIT: Begin completions
// PROTOCOL_EXT_INIT: Decl[Constructor]/Super:            ['('])[#Self#]{{; name=.+$}}
// PROTOCOL_EXT_INIT: Decl[Constructor]/Super:            ['(']{#x: Int#})[#Self#]{{; name=.+$}}
// PROTOCOL_EXT_INIT: End completions

extension P4 where Self.T == OnlyMe {
  final func test1() {
    self.#^PROTOCOL_EXT_P4_DOT_1^#
  }
  final func test2() {
    #^PROTOCOL_EXT_P4_DOT_2^#
  }
}
// PROTOCOL_EXT_P4_DOT: Begin completions
// PROTOCOL_EXT_P4_DOT-NOT: extP4WhenP1()
// PROTOCOL_EXT_P4_DOT-DAG: Decl[InstanceMethod]/Super:         extP4OnlyMe()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P4_DOT-NOT: extP4WhenP1()
// PROTOCOL_EXT_P4_DOT: End completions

extension P4 where Self.T == WillConformP1 {
  final func test() {
    T.#^PROTOCOL_EXT_P4_T_DOT_1^#
  }
}
// PROTOCOL_EXT_P4_T_DOT_1: Begin completions
// PROTOCOL_EXT_P4_T_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   reqP1({#self: WillConformP1#})[#() -> Void#]{{; name=.+$}}
// PROTOCOL_EXT_P4_T_DOT_1-DAG: Decl[InstanceMethod]/Super:   extP1({#self: Self#})[#() -> Void#]{{; name=.+$}}
// PROTOCOL_EXT_P4_T_DOT_1: End completions

protocol PWithT {
  typealias T
  func foo(x: T) -> T
}

extension PWithT {
  final func bar(x: T) -> T {
    return x
  }
}

// Note: PWithT cannot actually be used as an existential type because it has
// an associated type.  But we should still be able to give code completions.
func testUnusableProtExt(x: PWithT) {
  x.#^PROTOCOL_EXT_UNUSABLE_EXISTENTIAL^#
}
// PROTOCOL_EXT_UNUSABLE_EXISTENTIAL: Begin completions
// PROTOCOL_EXT_UNUSABLE_EXISTENTIAL: Decl[InstanceMethod]/CurrNominal:   foo({#(x): `Self`.T#})[#`Self`.T#]{{; name=.+}}
// PROTOCOL_EXT_UNUSABLE_EXISTENTIAL: Decl[InstanceMethod]/CurrNominal:   bar({#(x): Self.T#})[#Self.T#]{{; name=.+}}
// PROTOCOL_EXT_UNUSABLE_EXISTENTIAL: End completions


//===--- Check calls that may throw

func globalFuncThrows() throws {}
func globalFuncRethrows(x: () throws -> ()) rethrows {}
struct HasThrowingMembers {
  func memberThrows() throws {}
  func memberRethrows(x: () throws -> ()) rethrows {}
  init() throws {}
  init(x: () throws -> ()) rethrows {}
}

func testThrows001() {
  globalFuncThrows#^THROWS1^#

// THROWS1: Begin completions
// THROWS1: Pattern/ExprSpecific:               ()[' throws'][#Void#]; name=() throws
// THROWS1: End completions
}
func testThrows002() {
  globalFuncRethrows#^THROWS2^#

// THROWS2: Begin completions
// FIXME: <rdar://problem/21010193> Fix throws => rethrows in call patterns
// THROWS2: Pattern/ExprSpecific:               ({#() throws -> ()##() throws -> ()#})[' throws'][#Void#]; name=(() throws -> ()) throws
// THROWS2: End completions
}
func testThrows003(x: HasThrowingMembers) {
  x.#^MEMBER_THROWS1^#
// MEMBER_THROWS1: Begin completions
// MEMBER_THROWS1-DAG: Decl[InstanceMethod]/CurrNominal:   memberThrows()[' throws'][#Void#]
// MEMBER_THROWS1-DAG: Decl[InstanceMethod]/CurrNominal:   memberRethrows({#(x): () throws -> ()##() throws -> ()#})[' rethrows'][#Void#]
// MEMBER_THROWS1: End completions
}
func testThrows004(x: HasThrowingMembers) {
  x.memberThrows#^MEMBER_THROWS2^#
// MEMBER_THROWS2: Begin completions
// MEMBER_THROWS2: Pattern/ExprSpecific:               ()[' throws'][#Void#]; name=() throws
// MEMBER_THROWS2: End completions
}
func testThrows005(x: HasThrowingMembers) {
  x.memberRethrows#^MEMBER_THROWS3^#
// MEMBER_THROWS3: Begin completions
// FIXME: <rdar://problem/21010193> Fix throws => rethrows in call patterns
// MEMBER_THROWS3: Pattern/ExprSpecific:               ({#() throws -> ()##() throws -> ()#})[' throws'][#Void#]; name=(() throws -> ()) throws
// MEMBER_THROWS3: End completions
}
func testThrows006() {
  HasThrowingMembers(#^INIT_THROWS1^#
// INIT_THROWS1: Begin completions
// INIT_THROWS1: Decl[Constructor]/CurrNominal:      ['('])[' throws'][#HasThrowingMembers#]
// INIT_THROWS1: Decl[Constructor]/CurrNominal:      ['(']{#x: () throws -> ()##() throws -> ()#})[' rethrows'][#HasThrowingMembers#]
// INIT_THROWS1: End completions
}


// rdar://21346928
// Just sample some String API to sanity check.
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      characters[#String.CharacterView#]
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      utf16[#String.UTF16View#]
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      utf8[#String.UTF8View#]
func testWithAutoClosure1(x: String?) {
  (x ?? "autoclosure").#^AUTOCLOSURE1^#
}
func testWithAutoClosure2(x: String?) {
  let y = (x ?? "autoclosure").#^AUTOCLOSURE2^#
}
func testWithAutoClosure3(x: String?) {
  let y = (x ?? "autoclosure".#^AUTOCLOSURE3^#)
}
func testWithAutoClosure4(x: String?) {
  let y = { let z = (x ?? "autoclosure").#^AUTOCLOSURE4^# }
}
func testWithAutoClosure5(x: String?) {
  if let y = (x ?? "autoclosure").#^AUTOCLOSURE5^# {
  }
}
