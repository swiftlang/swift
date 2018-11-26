// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_1 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_2 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_3 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_4 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_DOT_5 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_NO_DOT_1 | %FileCheck %s -check-prefix=FOO_OBJECT_NO_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_OBJECT_NO_DOT_2 | %FileCheck %s -check-prefix=FOO_OBJECT_NO_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_STRUCT_DOT_1 | %FileCheck %s -check-prefix=FOO_STRUCT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_STRUCT_NO_DOT_1 | %FileCheck %s -check-prefix=FOO_STRUCT_NO_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_STRUCT_META_1 | %FileCheck %s -check-prefix=FOO_STRUCT_META
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOO_STRUCT_META_2 | %FileCheck %s -check-prefix=FOO_STRUCT_META

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_FUNC_0 | %FileCheck %s -check-prefix=IMPLICITLY_CURRIED_FUNC_0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_FUNC_1 | %FileCheck %s -check-prefix=IMPLICITLY_CURRIED_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_FUNC_2 | %FileCheck %s -check-prefix=IMPLICITLY_CURRIED_FUNC_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_VARARG_FUNC_0 | %FileCheck %s -check-prefix=IMPLICITLY_CURRIED_VARARG_FUNC_0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_VARARG_FUNC_1 | %FileCheck %s -check-prefix=IMPLICITLY_CURRIED_VARARG_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_VARARG_FUNC_2 | %FileCheck %s -check-prefix=IMPLICITLY_CURRIED_VARARG_FUNC_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_OVERLOADED_FUNC_1 | %FileCheck %s -check-prefix=IMPLICITLY_CURRIED_OVERLOADED_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICITLY_CURRIED_OVERLOADED_FUNC_2 | %FileCheck %s -check-prefix=IMPLICITLY_CURRIED_OVERLOADED_FUNC_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_SWITCH_CASE_1 | %FileCheck %s -check-prefix=IN_SWITCH_CASE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_SWITCH_CASE_2 | %FileCheck %s -check-prefix=IN_SWITCH_CASE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_SWITCH_CASE_3 | %FileCheck %s -check-prefix=IN_SWITCH_CASE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_SWITCH_CASE_4 | %FileCheck %s -check-prefix=IN_SWITCH_CASE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VF1 | %FileCheck %s -check-prefix=VF1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VF2 | %FileCheck %s -check-prefix=VF2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BASE_MEMBERS | %FileCheck %s -check-prefix=BASE_MEMBERS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BASE_MEMBERS_STATIC | %FileCheck %s -check-prefix=BASE_MEMBERS_STATIC

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_NO_DOT_1 | %FileCheck %s -check-prefix=PROTO_MEMBERS_NO_DOT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_NO_DOT_2 | %FileCheck %s -check-prefix=PROTO_MEMBERS_NO_DOT_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_NO_DOT_3 | %FileCheck %s -check-prefix=PROTO_MEMBERS_NO_DOT_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_1 | %FileCheck %s -check-prefix=PROTO_MEMBERS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_2 | %FileCheck %s -check-prefix=PROTO_MEMBERS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_3 | %FileCheck %s -check-prefix=PROTO_MEMBERS_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTO_MEMBERS_4 | %FileCheck %s -check-prefix=PROTO_MEMBERS_4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_0 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_1 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_2 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_3 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_4 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_5 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_6 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_7 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_7
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_8 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_8
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_9 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_9
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_10 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_10
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_11 -code-complete-call-pattern-heuristics | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_11
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_11 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_12 -code-complete-call-pattern-heuristics | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_12
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_12 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_VARARG_FUNCTION_CALL_1 | %FileCheck %s -check-prefix=INSIDE_VARARG_FUNCTION_CALL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_VARARG_FUNCTION_CALL_2 | %FileCheck %s -check-prefix=INSIDE_VARARG_FUNCTION_CALL_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_VARARG_FUNCTION_CALL_3 | %FileCheck %s -check-prefix=INSIDE_VARARG_FUNCTION_CALL_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_OVERLOADED_FUNCTION_CALL_1 | %FileCheck %s -check-prefix=INSIDE_OVERLOADED_FUNCTION_CALL_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1 | %FileCheck %s -check-prefix=INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_1 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_2 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_3 | %FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_4 | %FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_5 | %FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_FUNC_PARAM_6 | %FileCheck %s -check-prefix=RESOLVE_FUNC_PARAM_6

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CONSTRUCTOR_PARAM_1 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CONSTRUCTOR_PARAM_2 | %FileCheck %s -check-prefix=RESOLVE_CONSTRUCTOR_PARAM_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CONSTRUCTOR_PARAM_3 | %FileCheck %s -check-prefix=RESOLVE_CONSTRUCTOR_PARAM_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_PAREN_PATTERN_1 | %FileCheck %s -check-prefix=FUNC_PAREN_PATTERN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_PAREN_PATTERN_2 | %FileCheck %s -check-prefix=FUNC_PAREN_PATTERN_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_PAREN_PATTERN_3 | %FileCheck %s -check-prefix=FUNC_PAREN_PATTERN_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAINED_CALLS_1 | %FileCheck %s -check-prefix=CHAINED_CALLS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAINED_CALLS_2 | %FileCheck %s -check-prefix=CHAINED_CALLS_2

// Disabled because we aren't handling failures well.
// FIXME: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAINED_CALLS_3 | %FileCheck %s -check-prefix=CHAINED_CALLS_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_1 | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_2 | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_3 | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_4 | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_5 | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_ERROR_1 | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_ERROR_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_1_STATIC | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_1_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_2_STATIC | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_2_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_3_STATIC | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_3_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_4_STATIC | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_4_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_GENERIC_PARAMS_5_STATIC | %FileCheck %s -check-prefix=RESOLVE_GENERIC_PARAMS_5_STATIC

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_UNSOLVED_VARIABLES_1 | %FileCheck %s -check-prefix=TC_UNSOLVED_VARIABLES_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_UNSOLVED_VARIABLES_2 | %FileCheck %s -check-prefix=TC_UNSOLVED_VARIABLES_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_UNSOLVED_VARIABLES_3 | %FileCheck %s -check-prefix=TC_UNSOLVED_VARIABLES_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_UNSOLVED_VARIABLES_4 | %FileCheck %s -check-prefix=TC_UNSOLVED_VARIABLES_4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_MODULES_1 | %FileCheck %s -check-prefix=RESOLVE_MODULES_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INTERPOLATED_STRING_1 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P3 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_WILLCONFORMP1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_WILLCONFORMP1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_DIDCONFORMP2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_DIDCONFORMP2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_DIDCONFORMP3 | %FileCheck %s -check-prefix=PROTOCOL_EXT_DIDCONFORMP3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_GENERICP1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_GENERICP1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_GENERICP2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_GENERICP2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_GENERICP3 | %FileCheck %s -check-prefix=PROTOCOL_EXT_GENERICP3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ1 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ2 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ3 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ4 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ5 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ6 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ_TYPE1 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ_TYPE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ_TYPE2 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ_TYPE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ_TYPE3 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ_TYPE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ_NODOT1 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ_NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ_NODOT2 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ_NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NODUP_RESTATED_REQ_NODOT3 | %FileCheck %s -check-prefix=CHECK_NODUP_RESTATED_REQ_NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHECK_PROT_OVERRIDES1 | %FileCheck %s -check-prefix=CHECK_PROT_OVERRIDES
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHECK_PROT_OVERRIDES2 | %FileCheck %s -check-prefix=CHECK_PROT_OVERRIDES
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P4 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONSTRAINED_GENERIC_1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONSTRAINED_GENERIC_2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INSIDE_CONCRETE1_1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INSIDE_CONCRETE1_2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_P1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE3 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE3_SUB | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME_SUB
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONCRETE4 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONSTRAINED_GENERIC_3 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_CONSTRAINED_GENERIC_3_SUB | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME_SUB
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INSIDE_CONCRETE2_1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INSIDE_CONCRETE2_2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_ONLYME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_TA_1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_TA
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_TA_2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_TA
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INIT_1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_INIT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INIT_2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_INIT_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INIT_3 | %FileCheck %s -check-prefix=PROTOCOL_EXT_INIT_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_INIT_4 | %FileCheck %s -check-prefix=PROTOCOL_EXT_INIT_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P4_DOT_1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P4_DOT_2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_P4_T_DOT_1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_P4_T_DOT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_UNUSABLE_EXISTENTIAL | %FileCheck %s -check-prefix=PROTOCOL_EXT_UNUSABLE_EXISTENTIAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_DEDUP_1 | %FileCheck %s -check-prefix=PROTOCOL_EXT_DEDUP_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_DEDUP_2 | %FileCheck %s -check-prefix=PROTOCOL_EXT_DEDUP_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_DEDUP_3 | %FileCheck %s -check-prefix=PROTOCOL_EXT_DEDUP_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=THROWS1 | %FileCheck %s -check-prefix=THROWS1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=THROWS2 | %FileCheck %s -check-prefix=THROWS2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_THROWS1 | %FileCheck %s -check-prefix=MEMBER_THROWS1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_THROWS2 | %FileCheck %s -check-prefix=MEMBER_THROWS2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_THROWS3 | %FileCheck %s -check-prefix=MEMBER_THROWS3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_THROWS1 | %FileCheck %s -check-prefix=INIT_THROWS1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE1 > %t.autoclosure1
// RUN: %FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE2 > %t.autoclosure2
// RUN: %FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE3 > %t.autoclosure3
// RUN: %FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE4 > %t.autoclosure4
// RUN: %FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE5 > %t.autoclosure5
// RUN: %FileCheck %s -check-prefix=AUTOCLOSURE_STRING < %t.autoclosure5

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_TYPEALIAS_1 | %FileCheck %s -check-prefix=GENERIC_TYPEALIAS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_TYPEALIAS_2 | %FileCheck %s -check-prefix=GENERIC_TYPEALIAS_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEPRECATED_1 | %FileCheck %s -check-prefix=DEPRECATED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DOT_EXPR_NON_NOMINAL_1 | %FileCheck %s -check-prefix=DOT_EXPR_NON_NOMINAL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DOT_EXPR_NON_NOMINAL_2 | %FileCheck %s -check-prefix=DOT_EXPR_NON_NOMINAL_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD_1 | %FileCheck %s -check-prefix=KEYWORD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD_2 | %FileCheck %s -check-prefix=KEYWORD_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD_3 | %FileCheck %s -check-prefix=KEYWORD_3

// Test code completion of expressions that produce a value.

struct FooStruct {
  lazy var lazyInstanceVar = 0
  var instanceVar = 0

  mutating
  func instanceFunc0() {}
  mutating
  func instanceFunc1(_ a: Int) {}
  mutating
  func instanceFunc2(_ a: Int, b: inout Double) {}
  mutating
  func instanceFunc3(_ a: Int, _: (Float, Double)) {}
  mutating
  func instanceFunc4(_ a: Int?, b: Int!, c: inout Int?, d: inout Int!) {}
  mutating
  func instanceFunc5() -> Int? {}
  mutating
  func instanceFunc6() -> Int! {}
  mutating
  func instanceFunc7(a a: Int) {}
  mutating
  func instanceFunc8(_ a: (Int, Int)) {}
  mutating
  func instanceFunc9(a: @autoclosure () -> Int) {}

  mutating
  func varargInstanceFunc0(_ v: Int...) {}
  mutating
  func varargInstanceFunc1(_ a: Float, v: Int...) {}
  mutating
  func varargInstanceFunc2(_ a: Float, b: Double, v: Int...) {}

  mutating
  func overloadedInstanceFunc1() -> Int {}
  mutating
  func overloadedInstanceFunc1() -> Double {}

  mutating
  func overloadedInstanceFunc2(_ x: Int) -> Int {}
  mutating
  func overloadedInstanceFunc2(_ x: Double) -> Int {}

  mutating
  func builderFunc1(_ a: Int) -> FooStruct { return self }

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
  func selectorVoidFunc1(_ a: Int, b x: Float) {}
  mutating
  func selectorVoidFunc2(_ a: Int, b x: Float, c y: Double) {}
  mutating
  func selectorVoidFunc3(_ a: Int, b _: (Float, Double)) {}

  mutating
  func selectorStringFunc1(_ a: Int, b x: Float) -> String {}
  mutating
  func selectorStringFunc2(_ a: Int, b x: Float, c y: Double) -> String {}
  mutating
  func selectorStringFunc3(_ a: Int, b _: (Float, Double)) -> String{}

  struct NestedStruct {}
  class NestedClass {}
  enum NestedEnum {}
  // Cannot declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  static var staticVar: Int = 4

  static func staticFunc0() {}
  static func staticFunc1(_ a: Int) {}

  static func overloadedStaticFunc1() -> Int {}
  static func overloadedStaticFunc1() -> Double {}

  static func overloadedStaticFunc2(_ x: Int) -> Int {}
  static func overloadedStaticFunc2(_ x: Double) -> Int {}
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
// FOO_OBJECT_DOT-NEXT: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
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
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc9({#a: Int#})[#Void#]{{; name=.+$}}
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
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc9({#a: Int#})[#Void#]{{; name=.+$}}
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
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorVoidFunc1({#(a): Int#}, {#b: Float#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorVoidFunc2({#(a): Int#}, {#b: Float#}, {#c: Double#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorVoidFunc3({#(a): Int#}, {#b: (Float, Double)#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorStringFunc1({#(a): Int#}, {#b: Float#})[#String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorStringFunc2({#(a): Int#}, {#b: Float#}, {#c: Double#})[#String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .selectorStringFunc3({#(a): Int#}, {#b: (Float, Double)#})[#String#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    .extProp[#Int#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .extFunc0()[#Void#]{{; name=.+$}}
// FOO_OBJECT_NO_DOT-NEXT: BuiltinOperator/None:                     = {#Foo
// FOO_OBJECT_NO_DOT-NEXT: Keyword[self]/CurrNominal: .self[#FooStruct#]; name=self
// FOO_OBJECT_NO_DOT-NEXT: End completions

// FOO_STRUCT_DOT: Begin completions
// FOO_STRUCT_DOT-NEXT: Keyword[self]/CurrNominal: self[#FooStruct.Type#]; name=self
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc0({#self: &FooStruct#})[#() -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#self: &FooStruct#})[#(Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc2({#self: &FooStruct#})[#(Int, b: inout Double) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc3({#self: &FooStruct#})[#(Int, (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc4({#self: &FooStruct#})[#(Int?, b: Int?, c: inout Int?, d: inout Int?) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc5({#self: &FooStruct#})[#() -> Int?#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc6({#self: &FooStruct#})[#() -> Int?#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc7({#self: &FooStruct#})[#(a: Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc8({#self: &FooStruct#})[#((Int, Int)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc9({#self: &FooStruct#})[#(a: @autoclosure () -> Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: varargInstanceFunc0({#self: &FooStruct#})[#(Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: varargInstanceFunc1({#self: &FooStruct#})[#(Float, v: Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: varargInstanceFunc2({#self: &FooStruct#})[#(Float, b: Double, v: Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc1({#self: &FooStruct#})[#() -> Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc1({#self: &FooStruct#})[#() -> Double#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc2({#self: &FooStruct#})[#(Int) -> Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: overloadedInstanceFunc2({#self: &FooStruct#})[#(Double) -> Int#]{{; name=.+$}}
// FOO_STRUCT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: builderFunc1({#self: &FooStruct#})[#(Int) -> FooStruct#]{{; name=.+$}}
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
// FOO_STRUCT_DOT-NEXT: Decl[Constructor]/CurrNominal:    init({#lazyInstanceVar: Int?#}, {#instanceVar: Int#})[#FooStruct#]; name=init(lazyInstanceVar: Int?, instanceVar: Int){{$}}
// FOO_STRUCT_DOT-NEXT: Decl[Constructor]/CurrNominal:    init()[#FooStruct#]; name=init(){{$}}
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
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc2({#self: &FooStruct#})[#(Int, b: inout Double) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc3({#self: &FooStruct#})[#(Int, (Float, Double)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc4({#self: &FooStruct#})[#(Int?, b: Int?, c: inout Int?, d: inout Int?) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc5({#self: &FooStruct#})[#() -> Int?#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc6({#self: &FooStruct#})[#() -> Int?#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc7({#self: &FooStruct#})[#(a: Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc8({#self: &FooStruct#})[#((Int, Int)) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc9({#self: &FooStruct#})[#(a: @autoclosure () -> Int) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .varargInstanceFunc0({#self: &FooStruct#})[#(Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .varargInstanceFunc1({#self: &FooStruct#})[#(Float, v: Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .varargInstanceFunc2({#self: &FooStruct#})[#(Float, b: Double, v: Int...) -> Void#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc1({#self: &FooStruct#})[#() -> Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc1({#self: &FooStruct#})[#() -> Double#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc2({#self: &FooStruct#})[#(Int) -> Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .overloadedInstanceFunc2({#self: &FooStruct#})[#(Double) -> Int#]{{; name=.+$}}
// FOO_STRUCT_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .builderFunc1({#self: &FooStruct#})[#(Int) -> FooStruct#]{{; name=.+$}}
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
// FOO_STRUCT_NO_DOT-NEXT: Keyword[self]/CurrNominal:        .self[#FooStruct.Type#]; name=self
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

struct NoMetaCompletions {
  static var foo: Int = 0
  class func bar() {}
  typealias Foo = Int
}
func testMetatypeCompletions() {
  NoMetaCompletions.Type.#^FOO_STRUCT_META_1^#
}
func testMetatypeCompletionsWithoutDot() {
  NoMetaCompletions.Type#^FOO_STRUCT_META_2^#
}
// FOO_STRUCT_META-NOT: Decl
// FOO_STRUCT_META: Keyword[self]/CurrNominal: {{self|.self}}[#NoMetaCompletions.Type.Type#]; name=self
// FOO_STRUCT_META-NOT: Decl

func testImplicitlyCurriedFunc(_ fs: inout FooStruct) {
  FooStruct.instanceFunc0(&fs)#^IMPLICITLY_CURRIED_FUNC_0^#
// IMPLICITLY_CURRIED_FUNC_0: Begin completions
// IMPLICITLY_CURRIED_FUNC_0-NEXT: Pattern/CurrModule: ()[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_FUNC_0-NEXT: Keyword[self]/CurrNominal: .self[#() -> ()#]; name=self
// IMPLICITLY_CURRIED_FUNC_0-NEXT: End completions

  FooStruct.instanceFunc1(&fs)#^IMPLICITLY_CURRIED_FUNC_1^#
// IMPLICITLY_CURRIED_FUNC_1: Begin completions
// IMPLICITLY_CURRIED_FUNC_1-NEXT: Pattern/CurrModule: ({#(a): Int#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_FUNC_1-NEXT: Keyword[self]/CurrNominal: .self[#(Int) -> ()#]; name=self
// IMPLICITLY_CURRIED_FUNC_1-NEXT: End completions

  FooStruct.instanceFunc2(&fs)#^IMPLICITLY_CURRIED_FUNC_2^#
// IMPLICITLY_CURRIED_FUNC_2: Begin completions
// IMPLICITLY_CURRIED_FUNC_2-NEXT: Pattern/CurrModule: ({#(a): Int#}, {#b: &Double#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_FUNC_2-NEXT: Keyword[self]/CurrNominal: .self[#(Int, inout Double) -> ()#]; name=self
// IMPLICITLY_CURRIED_FUNC_2-NEXT: End completions

  FooStruct.varargInstanceFunc0(&fs)#^IMPLICITLY_CURRIED_VARARG_FUNC_0^#
// IMPLICITLY_CURRIED_VARARG_FUNC_0: Begin completions
// IMPLICITLY_CURRIED_VARARG_FUNC_0-NEXT: Pattern/CurrModule: ({#(v): Int...#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_0-NEXT: Keyword[self]/CurrNominal: .self[#(Int...) -> ()#]; name=self
// IMPLICITLY_CURRIED_VARARG_FUNC_0-NEXT: End completions

  FooStruct.varargInstanceFunc1(&fs)#^IMPLICITLY_CURRIED_VARARG_FUNC_1^#
// IMPLICITLY_CURRIED_VARARG_FUNC_1: Begin completions
// IMPLICITLY_CURRIED_VARARG_FUNC_1-NEXT: Pattern/CurrModule: ({#(a): Float#}, {#v: Int...#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_1-NEXT: Keyword[self]/CurrNominal: .self[#(Float, Int...) -> ()#]; name=self
// IMPLICITLY_CURRIED_VARARG_FUNC_1-NEXT: End completions

  FooStruct.varargInstanceFunc2(&fs)#^IMPLICITLY_CURRIED_VARARG_FUNC_2^#
// IMPLICITLY_CURRIED_VARARG_FUNC_2: Begin completions
// IMPLICITLY_CURRIED_VARARG_FUNC_2-NEXT: Pattern/CurrModule: ({#(a): Float#}, {#b: Double#}, {#v: Int...#})[#Void#]{{; name=.+$}}
// IMPLICITLY_CURRIED_VARARG_FUNC_2-NEXT: Keyword[self]/CurrNominal: .self[#(Float, Double, Int...) -> ()#]; name=self
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
  init() {}
  init(t: T) { fooInstanceVarT = t }

  var fooInstanceVarT: T
  var fooInstanceVarTBrackets: [T]
  mutating
  func fooVoidInstanceFunc1(_ a: T) {}
  mutating
  func fooTInstanceFunc1(_ a: T) -> T { return a }
  mutating
  func fooUInstanceFunc1<U>(_ a: U) -> U { return a }

  static var fooStaticVarT: Int = 0
  static var fooStaticVarTBrackets: [Int] = [0]
  static func fooVoidStaticFunc1(_ a: T) {}
  static func fooTStaticFunc1(_ a: T) -> T { return a }
  static func fooUInstanceFunc1<U>(_ a: U) -> U { return a }
}

class FooClass {
  var fooClassInstanceVar = 0
  func fooClassInstanceFunc0() {}
  func fooClassInstanceFunc1(_ a: Int) {}
}

enum FooEnum {
}

protocol FooProtocol {
  var fooInstanceVar1: Int { get set }
  var fooInstanceVar2: Int { get }
  typealias FooTypeAlias1
  func fooInstanceFunc0() -> Double
  func fooInstanceFunc1(_ a: Int) -> Double
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
  func fooInstanceFunc1(_ a: Int) -> Double {
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
  func barInstanceFunc1(_ a: Int) -> Double
}

protocol BarExProtocol : BarProtocol {
  func barExInstanceFunc0() -> Double
}

protocol BazProtocol {
  func bazInstanceFunc0() -> Double
}

typealias BarBazProtocolComposition = BarProtocol & BazProtocol

let fooProtocolInstance: FooProtocol = FooProtocolImpl()
let fooBarProtocolInstance: FooProtocol & BarProtocol
let fooExBarExProtocolInstance: FooExProtocol & BarExProtocol

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
// There should be no results here because the function call
// unambiguously resolves to overload that takes 0 arguments.
// INSIDE_FUNCTION_CALL_1-NOT: Begin completions
}

func testInsideFunctionCall2() {
  var a = FooStruct()
  a.instanceFunc1(#^INSIDE_FUNCTION_CALL_2^#
// INSIDE_FUNCTION_CALL_2: Begin completions
// INSIDE_FUNCTION_CALL_2-DAG: Pattern/CurrModule:       ['(']{#(a): Int#}[')'][#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_2-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_2: End completions
}

func testInsideFunctionCall3() {
  FooStruct().instanceFunc1(42, #^INSIDE_FUNCTION_CALL_3^#
// INSIDE_FUNCTION_CALL_3-NOT: Begin Completions
}

func testInsideFunctionCall4() {
  var a = FooStruct()
  a.instanceFunc2(#^INSIDE_FUNCTION_CALL_4^#
// INSIDE_FUNCTION_CALL_4: Begin completions
// INSIDE_FUNCTION_CALL_4-DAG: Pattern/CurrModule:       ['(']{#(a): Int#}, {#b: &Double#}[')'][#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_4-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_4: End completions
}

func testInsideFunctionCall5() {
  FooStruct().instanceFunc2(42, #^INSIDE_FUNCTION_CALL_5^#
// INSIDE_FUNCTION_CALL_5: Begin completions
// INSIDE_FUNCTION_CALL_5-DAG: Keyword/ExprSpecific:               b: [#Argument name#]; name=b: 
// INSIDE_FUNCTION_CALL_5: End completions
}

func testInsideFunctionCall6() {
  var a = FooStruct()
  a.instanceFunc7(#^INSIDE_FUNCTION_CALL_6^#
// INSIDE_FUNCTION_CALL_6: Begin completions
// INSIDE_FUNCTION_CALL_6-NEXT: Pattern/CurrModule: ['(']{#a: Int#}[')'][#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_6-NEXT: End completions
}

func testInsideFunctionCall7() {
  var a = FooStruct()
  a.instanceFunc8(#^INSIDE_FUNCTION_CALL_7^#
// INSIDE_FUNCTION_CALL_7: Begin completions
// INSIDE_FUNCTION_CALL_7: Pattern/CurrModule: ['(']{#(a): (Int, Int)#}[')'][#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_7: End completions
}

func testInsideFunctionCall8(_ x: inout FooStruct) {
  x.instanceFunc0(#^INSIDE_FUNCTION_CALL_8^#)
// Since we already have '()', there is no pattern to complete.
// INSIDE_FUNCTION_CALL_8-NOT: Pattern/{{.*}}:
}
func testInsideFunctionCall9(_ x: inout FooStruct) {
  x.instanceFunc1(#^INSIDE_FUNCTION_CALL_9^#)
// Annotated ')'
// INSIDE_FUNCTION_CALL_9: Begin completions
// INSIDE_FUNCTION_CALL_9-DAG: Pattern/CurrModule: ['(']{#(a): Int#}[')'][#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_9-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_9: End completions
}
func testInsideFunctionCall10(_ x: inout FooStruct) {
  x.instanceFunc2(#^INSIDE_FUNCTION_CALL_10^#)
// Annotated ')'
// INSIDE_FUNCTION_CALL_10: Begin completions
// INSIDE_FUNCTION_CALL_10-DAG: Pattern/CurrModule: ['(']{#(a): Int#}, {#b: &Double#}[')'][#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_10-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_10: End completions
}
func testInsideFunctionCall11(_ x: inout FooStruct) {
  x.instanceFunc2(#^INSIDE_FUNCTION_CALL_11^#,
// INSIDE_FUNCTION_CALL_11-NOT: Pattern/{{.*}}:{{.*}}({{.*}}{#Int#}
// INSIDE_FUNCTION_CALL_11B: Pattern/CurrModule: ['(']{#Int#}, {#b: &Double#}[')'][#Void#];
}
func testInsideFunctionCall12(_ x: inout FooStruct) {
  x.instanceFunc2(#^INSIDE_FUNCTION_CALL_12^#<#placeholder#>
// INSIDE_FUNCTION_CALL_12-NOT: Pattern/{{.*}}:{{.*}}({{.*}}{#Int#}
}

func testInsideVarargFunctionCall1() {
  var a = FooStruct()
  a.varargInstanceFunc0(#^INSIDE_VARARG_FUNCTION_CALL_1^#
// INSIDE_VARARG_FUNCTION_CALL_1: Begin completions
// INSIDE_VARARG_FUNCTION_CALL_1-DAG: Pattern/CurrModule:       ['(']{#(v): Int...#}[')'][#Void#]{{; name=.+$}}
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

func testInsideFunctionCallOnClassInstance1(_ a: FooClass) {
  a.fooClassInstanceFunc1(#^INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1^#
// INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1: Begin completions
// INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1-DAG: Pattern/CurrModule:       ['(']{#(a): Int#}[')'][#Void#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// INSIDE_FUNCTION_CALL_ON_CLASS_INSTANCE_1: End completions
}

//===--- Variables that have function types.

class FuncTypeVars {
  var funcVar1: () -> Double
  var funcVar2: (a: Int) -> Double // adding the label is erroneous.
}

var funcTypeVarsObject: FuncTypeVars
func testFuncTypeVars() {
  funcTypeVarsObject.funcVar1#^VF1^#
// VF1: Begin completions
// VF1-NEXT: Pattern/CurrModule: ()[#Double#]{{; name=.+$}}
// VF1-NEXT: BuiltinOperator/None:         = {#() -> Double##() -> Double#}[#Void#]
// VF1-NEXT: Keyword[self]/CurrNominal:    .self[#() -> Double#]; name=self
// VF1-NEXT: End completions

  funcTypeVarsObject.funcVar2#^VF2^#
// VF2: Begin completions
// VF2-NEXT: Pattern/CurrModule: ({#Int#})[#Double#]{{; name=.+$}}
// VF2-NEXT: BuiltinOperator/None:         = {#(Int) -> Double##(Int) -> Double#}[#Void#]
// VF2-NEXT: Keyword[self]/CurrNominal:    .self[#(Int) -> Double#]; name=self
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
// BASE_MEMBERS-DAG: Keyword[self]/CurrNominal: self[#MembersDerived#]; name=self
// BASE_MEMBERS-NEXT: Decl[InstanceVar]/CurrNominal:    derivedVar[#Int#]{{; name=.+$}}
// BASE_MEMBERS-NEXT: Decl[InstanceMethod]/CurrNominal: derivedInstanceFunc()[#Void#]{{; name=.+$}}
// BASE_MEMBERS-NEXT: Decl[InstanceVar]/Super:          baseVar[#Int#]{{; name=.+$}}
// BASE_MEMBERS-NEXT: Decl[InstanceMethod]/Super:       baseInstanceFunc()[#Void#]{{; name=.+$}}
// BASE_MEMBERS-NEXT: End completions
}

func testLookInBaseStatic() {
  MembersDerived.#^BASE_MEMBERS_STATIC^#
// BASE_MEMBERS_STATIC: Begin completions
// BASE_MEMBERS_STATIC-NEXT: Keyword[self]/CurrNominal: self[#MembersDerived.Type#]; name=self
// BASE_MEMBERS_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: derivedInstanceFunc({#self: MembersDerived#})[#() -> Void#]{{; name=.+$}}
// BASE_MEMBERS_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   derivedStaticFunc()[#Void#]{{; name=.+$}}
// BASE_MEMBERS_STATIC-NEXT: Decl[Constructor]/CurrNominal:    init()[#MembersDerived#]; name=init(){{$}}
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
// PROTO_MEMBERS_NO_DOT_1-NEXT: Keyword[self]/CurrNominal:        .self[#FooProtocol#]; name=self
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
// PROTO_MEMBERS_NO_DOT_2-NEXT: Keyword[self]/CurrNominal: .self[#BarProtocol & FooProtocol#]; name=self
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
// PROTO_MEMBERS_NO_DOT_3-NEXT: Keyword[self]/CurrNominal: .self[#BarExProtocol & FooExProtocol#]; name=self
// PROTO_MEMBERS_NO_DOT_3-NEXT: End completions
}

func testLookInProto1() {
  fooProtocolInstance.#^PROTO_MEMBERS_1^#
// PROTO_MEMBERS_1: Begin completions
// PROTO_MEMBERS_1-NEXT: Keyword[self]/CurrNominal: self[#FooProtocol#]; name=self
// PROTO_MEMBERS_1-NEXT: Decl[InstanceVar]/CurrNominal:    fooInstanceVar1[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_1-NEXT: Decl[InstanceVar]/CurrNominal:    fooInstanceVar2[#Int#]{{; name=.+$}}
// PROTO_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_1-NEXT: End completions
}

func testLookInProto2() {
  fooBarProtocolInstance.#^PROTO_MEMBERS_2^#
// PROTO_MEMBERS_2: Begin completions
// PROTO_MEMBERS_2-NEXT: Keyword[self]/CurrNominal: self[#BarProtocol & FooProtocol#]; name=self
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
// PROTO_MEMBERS_3-NEXT: Keyword[self]/CurrNominal: self[#BarExProtocol & FooExProtocol#]; name=self
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

func testLookInProto4(_ a: FooProtocol & BarBazProtocolComposition) {
  a.#^PROTO_MEMBERS_4^#
// PROTO_MEMBERS_4: Begin completions
// PROTO_MEMBERS_4-DAG: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_4-DAG: Decl[InstanceMethod]/CurrNominal: barInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_4-DAG: Decl[InstanceMethod]/CurrNominal: bazInstanceFunc0()[#Double#]{{; name=.+$}}
// PROTO_MEMBERS_4: End completions
}

//===--- Check that we can resolve function parameters.

func testResolveFuncParam1(_ fs: FooStruct) {
  fs.#^RESOLVE_FUNC_PARAM_1^#
}

class TestResolveFuncParam2 {
  func testResolveFuncParam2a(_ fs: FooStruct) {
    fs.#^RESOLVE_FUNC_PARAM_2^#
  }
}

func testResolveFuncParam3<Foo : FooProtocol>(_ foo: Foo) {
  foo.#^RESOLVE_FUNC_PARAM_3^#
// RESOLVE_FUNC_PARAM_3: Begin completions
// RESOLVE_FUNC_PARAM_3-NEXT: Keyword[self]/CurrNominal: self[#Foo#]; name=self
// RESOLVE_FUNC_PARAM_3-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar1[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_3-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar2[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_3-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_3-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_3-NEXT: End completions
}

func testResolveFuncParam4<FooBar : FooProtocol & BarProtocol>(_ fooBar: FooBar) {
  fooBar.#^RESOLVE_FUNC_PARAM_4^#
// RESOLVE_FUNC_PARAM_4: Begin completions
// RESOLVE_FUNC_PARAM_4-NEXT: Keyword[self]/CurrNominal: self[#FooBar#]; name=self
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceVar]/Super:    barInstanceVar[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceMethod]/Super: barInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceMethod]/Super: barInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar1[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceVar]/Super:    fooInstanceVar2[#Int#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc0()[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: Decl[InstanceMethod]/Super: fooInstanceFunc1({#(a): Int#})[#Double#]{{; name=.+$}}
// RESOLVE_FUNC_PARAM_4-NEXT: End completions
}

func testResolveFuncParam5<FooExBarEx : FooExProtocol & BarExProtocol>(_ a: FooExBarEx) {
  a.#^RESOLVE_FUNC_PARAM_5^#
// RESOLVE_FUNC_PARAM_5: Begin completions
// RESOLVE_FUNC_PARAM_5-NEXT: Keyword[self]/CurrNominal: self[#FooExBarEx#]; name=self
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

func testResolveFuncParam6<Foo : FooProtocol where Foo : FooClass>(_ foo: Foo) {
  foo.#^RESOLVE_FUNC_PARAM_6^#
// RESOLVE_FUNC_PARAM_6: Begin completions
// RESOLVE_FUNC_PARAM_6-NEXT: Keyword[self]/CurrNominal: self[#Foo#]; name=self
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
// RESOLVE_CONSTRUCTOR_PARAM_2-NEXT: Keyword[self]/CurrNominal: self[#Foo#]; name=self
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
// RESOLVE_CONSTRUCTOR_PARAM_3-NEXT: Keyword[self]/CurrNominal: self[#Foo#]; name=self
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

func testFuncParenPattern1(_ fpp: FuncParenPattern) {
  fpp#^FUNC_PAREN_PATTERN_1^#
// FUNC_PAREN_PATTERN_1: Begin completions
// FUNC_PAREN_PATTERN_1-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#Int#})[#Void#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_1-NEXT: Decl[Subscript]/CurrNominal: [{#Int#}][#Int#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_1-NEXT: Keyword[self]/CurrNominal: .self[#FuncParenPattern#]; name=self
// FUNC_PAREN_PATTERN_1-NEXT: End completions
}

func testFuncParenPattern2(_ fpp: FuncParenPattern) {
  FuncParenPattern#^FUNC_PAREN_PATTERN_2^#
// FUNC_PAREN_PATTERN_2: Begin completions
// FUNC_PAREN_PATTERN_2-NEXT: Decl[Constructor]/CurrNominal: ({#Int#})[#FuncParenPattern#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_2-NEXT: Decl[Constructor]/CurrNominal: ({#(Int, Int)#})[#FuncParenPattern#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_2-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#self: &FuncParenPattern#})[#(Int) -> Void#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_2-NEXT: Keyword[self]/CurrNominal: .self[#FuncParenPattern.Type#]; name=self
// FUNC_PAREN_PATTERN_2-NEXT: End completions
}

func testFuncParenPattern3(_ fpp: inout FuncParenPattern) {
  fpp.instanceFunc#^FUNC_PAREN_PATTERN_3^#
// FUNC_PAREN_PATTERN_3: Begin completions
// FUNC_PAREN_PATTERN_3-NEXT: Pattern/CurrModule: ({#Int#})[#Void#]{{; name=.+$}}
// FUNC_PAREN_PATTERN_3-NEXT: Keyword[self]/CurrNominal: .self[#(Int) -> ()#]; name=self
// FUNC_PAREN_PATTERN_3-NEXT: End completions
}

//===--- Check that we can code complete after function calls

struct SomeBuilder {
  init(_ a: Int) {}
  func doFoo() -> SomeBuilder { return self }
  func doBar() -> SomeBuilder { return self }
  func doBaz(_ z: Double) -> SomeBuilder { return self }
}

func testChainedCalls1() {
  SomeBuilder(42)#^CHAINED_CALLS_1^#
// CHAINED_CALLS_1: Begin completions
// CHAINED_CALLS_1-DAG: Decl[InstanceMethod]/CurrNominal: .doFoo()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_1-DAG: Decl[InstanceMethod]/CurrNominal: .doBar()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_1-DAG: Decl[InstanceMethod]/CurrNominal: .doBaz({#(z): Double#})[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_1-DAG: Keyword[self]/CurrNominal: .self[#SomeBuilder#]; name=self
// CHAINED_CALLS_1: End completions
}

func testChainedCalls2() {
  SomeBuilder(42).doFoo()#^CHAINED_CALLS_2^#
// CHAINED_CALLS_2: Begin completions
// CHAINED_CALLS_2-DAG: Decl[InstanceMethod]/CurrNominal: .doFoo()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_2-DAG: Decl[InstanceMethod]/CurrNominal: .doBar()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_2-DAG: Decl[InstanceMethod]/CurrNominal: .doBaz({#(z): Double#})[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_2-DAG: Keyword[self]/CurrNominal: .self[#SomeBuilder#]; name=self
// CHAINED_CALLS_2: End completions
}

func testChainedCalls3() {
  // doBaz() takes a Double.  Check that we can recover.
  SomeBuilder(42).doFoo().doBaz(SomeBuilder(24))#^CHAINED_CALLS_3^#
// CHAINED_CALLS_3: Begin completions
// CHAINED_CALLS_3-DAG: Decl[InstanceMethod]/CurrNominal: .doFoo()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_3-DAG: Decl[InstanceMethod]/CurrNominal: .doBar()[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_3-DAG: Decl[InstanceMethod]/CurrNominal: .doBaz({#z: Double#})[#SomeBuilder#]{{; name=.+$}}
// CHAINED_CALLS_3-DAG: Keyword[self]/CurrNominal: .self[#SomeBuilder#]; name=self
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
// RESOLVE_GENERIC_PARAMS_1-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<FooStruct>#]; name=self
// RESOLVE_GENERIC_PARAMS_1-NEXT: End completions

  FooGenericStruct<FooStruct>#^RESOLVE_GENERIC_PARAMS_1_STATIC^#
// RESOLVE_GENERIC_PARAMS_1_STATIC: Begin completions
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ()[#FooGenericStruct<FooStruct>#]; name=()
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: FooStruct#})[#FooGenericStruct<FooStruct>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#(FooStruct) -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#(FooStruct) -> FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): FooStruct#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): FooStruct#})[#FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<FooStruct>.Type#]; name=self
// RESOLVE_GENERIC_PARAMS_1_STATIC-NEXT: End completions
}

func testResolveGenericParams2<Foo : FooProtocol>(_ foo: Foo) {
  FooGenericStruct<Foo>()#^RESOLVE_GENERIC_PARAMS_2^#
// RESOLVE_GENERIC_PARAMS_2: Begin completions
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarT[#FooProtocol#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarTBrackets[#[FooProtocol]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#(a): FooProtocol#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#(a): FooProtocol#})[#FooProtocol#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<Foo>#]; name=self
// RESOLVE_GENERIC_PARAMS_2-NEXT: End completions

  FooGenericStruct<Foo>#^RESOLVE_GENERIC_PARAMS_2_STATIC^#
// RESOLVE_GENERIC_PARAMS_2_STATIC: Begin completions
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ()[#FooGenericStruct<FooProtocol>#]; name=()
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: FooProtocol#})[#FooGenericStruct<FooProtocol>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<FooProtocol>#})[#(FooProtocol) -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<FooProtocol>#})[#(FooProtocol) -> FooProtocol#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<FooProtocol>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): FooProtocol#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): FooProtocol#})[#FooProtocol#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_2_STATIC-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<Foo>.Type#]; name=self
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
// RESOLVE_GENERIC_PARAMS_3-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<FooStruct>#]; name=self
// RESOLVE_GENERIC_PARAMS_3-NEXT: End completions

    FooGenericStruct<FooStruct>#^RESOLVE_GENERIC_PARAMS_3_STATIC^#
// RESOLVE_GENERIC_PARAMS_3_STATIC: Begin completions, 11 items
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ()[#FooGenericStruct<FooStruct>#]; name=()
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: FooStruct#})[#FooGenericStruct<FooStruct>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#(FooStruct) -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#(FooStruct) -> FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<FooStruct>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): FooStruct#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): FooStruct#})[#FooStruct#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<FooStruct>.Type#]; name=self
// RESOLVE_GENERIC_PARAMS_3_STATIC-NEXT: End completions
  }

  func testResolveGenericParams4(_ t: T) {
    FooGenericStruct<T>(t: t)#^RESOLVE_GENERIC_PARAMS_4^#
// RESOLVE_GENERIC_PARAMS_4: Begin completions
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarT[#T#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarTBrackets[#[T]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#(a): T#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#(a): T#})[#T#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<T>#]; name=self
// RESOLVE_GENERIC_PARAMS_4-NEXT: End completions

    FooGenericStruct<T>#^RESOLVE_GENERIC_PARAMS_4_STATIC^#
// RESOLVE_GENERIC_PARAMS_4_STATIC: Begin completions
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ()[#FooGenericStruct<T>#]; name=()
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: T#})[#FooGenericStruct<T>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<T>#})[#(T) -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<T>#})[#(T) -> T#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<T>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): T#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): T#})[#T#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<T>.Type#]; name=self
// RESOLVE_GENERIC_PARAMS_4_STATIC-NEXT: End completions
  }

  func testResolveGenericParams5<U>(_ u: U) {
    FooGenericStruct<U>(t: u)#^RESOLVE_GENERIC_PARAMS_5^#
// RESOLVE_GENERIC_PARAMS_5: Begin completions
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarT[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceVar]/CurrNominal:    .fooInstanceVarTBrackets[#[U]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#(a): U#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<U>#]; name=self
// RESOLVE_GENERIC_PARAMS_5-NEXT: End completions

    FooGenericStruct<U>#^RESOLVE_GENERIC_PARAMS_5_STATIC^#
// RESOLVE_GENERIC_PARAMS_5_STATIC: Begin completions
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ()[#FooGenericStruct<U>#]; name=()
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[Constructor]/CurrNominal:    ({#t: U#})[#FooGenericStruct<U>#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooVoidInstanceFunc1({#self: &FooGenericStruct<U>#})[#(U) -> Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooTInstanceFunc1({#self: &FooGenericStruct<U>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[InstanceMethod]/CurrNominal: .fooUInstanceFunc1({#self: &FooGenericStruct<U>#})[#(U) -> U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarT[#Int#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticVar]/CurrNominal:      .fooStaticVarTBrackets[#[Int]#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooVoidStaticFunc1({#(a): U#})[#Void#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooTStaticFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Decl[StaticMethod]/CurrNominal:   .fooUInstanceFunc1({#(a): U#})[#U#]{{; name=.+$}}
// RESOLVE_GENERIC_PARAMS_5_STATIC-NEXT: Keyword[self]/CurrNominal:        .self[#FooGenericStruct<U>.Type#]; name=self
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
  func addString(_ s: String) -> BuilderStyle<T> {
    count += 1
    return self
  }
  func add(_ t: T) -> BuilderStyle<T> {
    count += 1
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
// TC_UNSOLVED_VARIABLES_1-NEXT: Keyword[self]/CurrNominal: self[#BuilderStyle<_>#]; name=self
// TC_UNSOLVED_VARIABLES_1-NEXT: Decl[InstanceVar]/CurrNominal: count[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_1-NEXT: Decl[InstanceMethod]/CurrNominal: addString({#(s): String#})[#BuilderStyle<_>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_1-NEXT: Decl[InstanceMethod]/CurrNominal: add({#(t): _#})[#BuilderStyle<_>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_1-NEXT: Decl[InstanceMethod]/CurrNominal: get()[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_1-NEXT: End completions

func testTypeCheckWithUnsolvedVariables2() {
  BuilderStyle().addString("abc").#^TC_UNSOLVED_VARIABLES_2^#
}
// TC_UNSOLVED_VARIABLES_2: Begin completions
// TC_UNSOLVED_VARIABLES_2-NEXT: Keyword[self]/CurrNominal: self[#BuilderStyle<_>#]; name=self
// TC_UNSOLVED_VARIABLES_2-NEXT: Decl[InstanceVar]/CurrNominal:    count[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_2-NEXT: Decl[InstanceMethod]/CurrNominal: addString({#(s): String#})[#BuilderStyle<_>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_2-NEXT: Decl[InstanceMethod]/CurrNominal: add({#(t): _#})[#BuilderStyle<_>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_2-NEXT: Decl[InstanceMethod]/CurrNominal: get()[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_2-NEXT: End completions

func testTypeCheckWithUnsolvedVariables3() {
  BuilderStyle().addString("abc").add(42).#^TC_UNSOLVED_VARIABLES_3^#
}
// TC_UNSOLVED_VARIABLES_3: Begin completions
// TC_UNSOLVED_VARIABLES_3-NEXT: Keyword[self]/CurrNominal: self[#BuilderStyle<Int>#]; name=self
// TC_UNSOLVED_VARIABLES_3-NEXT: Decl[InstanceVar]/CurrNominal:    count[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_3-NEXT: Decl[InstanceMethod]/CurrNominal: addString({#(s): String#})[#BuilderStyle<Int>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_3-NEXT: Decl[InstanceMethod]/CurrNominal: add({#(t): Int#})[#BuilderStyle<Int>#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_3-NEXT: Decl[InstanceMethod]/CurrNominal: get()[#Int#]{{; name=.+$}}
// TC_UNSOLVED_VARIABLES_3-NEXT: End completions

func testTypeCheckNil() {
  nil#^TC_UNSOLVED_VARIABLES_4^#
}
// TC_UNSOLVED_VARIABLES_4-NOT: Decl{{.*}}: .{{[a-zA-Z]}}

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

// FOO_OBJECT_DOT1: Begin completions
// FOO_OBJECT_DOT1-DAG: Decl[InstanceVar]/CurrNominal:      lazyInstanceVar[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT1-DAG: Decl[InstanceVar]/CurrNominal:      instanceVar[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT1-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: instanceFunc0()[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT1-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT1-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: instanceFunc2({#(a): Int#}, {#b: &Double#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT1-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: instanceFunc3({#(a): Int#}, {#(Float, Double)#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT1-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: instanceFunc4({#(a): Int?#}, {#b: Int!#}, {#c: &Int?#}, {#d: &Int!#})[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT1-DAG: Decl[InstanceMethod]/CurrNominal:   instanceFunc5()[#Int?#]{{; name=.+$}}
// FOO_OBJECT_DOT1-DAG: Decl[InstanceMethod]/CurrNominal:   instanceFunc6()[#Int!#]{{; name=.+$}}

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

func testProtocol1(_ x: P1) {
  x.#^PROTOCOL_EXT_P1^#
}
// PROTOCOL_EXT_P1: Begin completions
// PROTOCOL_EXT_P1-DAG: Decl[InstanceMethod]/CurrNominal:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P1-DAG: Decl[InstanceMethod]/CurrNominal:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P1: End completions


func testProtocol2(_ x: P2) {
  x.#^PROTOCOL_EXT_P2^#
}
// PROTOCOL_EXT_P2: Begin completions
// PROTOCOL_EXT_P2-DAG: Decl[InstanceMethod]/Super:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P2-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P2-DAG: Decl[InstanceMethod]/CurrNominal:   reqP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P2-DAG: Decl[InstanceMethod]/CurrNominal:   extP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_P2: End completions

func testProtocol3(_ x: P3) {
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

func testConformingConcrete1(_ x: WillConformP1) {
  x.#^PROTOCOL_EXT_WILLCONFORMP1^#
}
// PROTOCOL_EXT_WILLCONFORMP1: Begin completions
// PROTOCOL_EXT_WILLCONFORMP1-DAG: Decl[InstanceMethod]/CurrNominal:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_WILLCONFORMP1-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_WILLCONFORMP1: End completions

func testConformingConcrete2(_ x: DidConformP2) {
  x.#^PROTOCOL_EXT_DIDCONFORMP2^#
}
// PROTOCOL_EXT_DIDCONFORMP2: Begin completions
// PROTOCOL_EXT_DIDCONFORMP2-DAG: Decl[InstanceMethod]/CurrNominal:   reqP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP2-DAG: Decl[InstanceMethod]/CurrNominal:   reqP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP2-DAG: Decl[InstanceMethod]/Super:   extP1()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP2-DAG: Decl[InstanceMethod]/Super:   extP2()[#Void#]{{; name=.+$}}
// PROTOCOL_EXT_DIDCONFORMP2: End completions

func testConformingConcrete3(_ x: DidConformP3) {
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

protocol NoDupReq1 {
  func foo()
  func roo(arg1: Int)
  subscript(arg: Bool) -> Bool {get}
  var doo: Int {get}
  associatedtype E
}
protocol NoDupReq2 {
  func foo()
  subscript(arg: Bool) -> Bool {get}
  var doo: Int {get}
  associatedtype E
}
protocol NoDupReq3 {
  func foo()
  func roo(arg2: Int)
  subscript(arg: Bool) -> Bool {get}
  var doo: Int {get}
  associatedtype E
}

protocol NoDupReq4 {
  func foo()
  func roo(arg1: Int)
  subscript(arg: Bool) -> Bool {get}
  var doo: Int {get}
  associatedtype E
}
protocol NoDupReq5: NoDupReq4 {
  func foo()
  subscript(arg: Bool) -> Bool {get}
  var doo: Int {get}
  associatedtype E
}
protocol NoDupReq6: NoDupReq5 {
  func foo()
  func roo(arg2: Int)
  subscript(arg: Bool) -> Bool {get}
  var doo: Int {get}
  associatedtype E
}

typealias NoDupReq23 = NoDupReq2 & NoDupReq3

protocol Override {
  func foo<T: NoDupReq1>(_ arg: T)
  func foo<T: NoDupReq2>(_ arg: T)
}
protocol Override2 {
  func foo<T: NoDupReq1>(_ arg: T)
}
protocol Override3: Override2 {
  func foo<T: NoDupReq2>(_ arg: T)
}

func checkRestatementNoDup1(_ arg: NoDupReq1 & NoDupReq2 & NoDupReq3) {
  arg.#^NODUP_RESTATED_REQ1^#
  arg#^NODUP_RESTATED_REQ_NODOT1^#
}
func checkRestatementNoDup2(_ arg: NoDupReq6) {
  arg.#^NODUP_RESTATED_REQ2^#
}
func checkRestatementNoDup3<T: NoDupReq6>(_ arg: T) {
  arg.#^NODUP_RESTATED_REQ3^#
  T.#^NODUP_RESTATED_REQ_TYPE1^#
  arg#^NODUP_RESTATED_REQ_NODOT2^#
}
func checkRestatementNoDup4<T: NoDupReq1 & NoDupReq2 & NoDupReq3>(_ arg: T) {
  arg.#^NODUP_RESTATED_REQ4^#
  T.#^NODUP_RESTATED_REQ_TYPE2^#
}
func checkRestatementNoDup5<T: NoDupReq1 & NoDupReq23>(_ arg: T) {
  arg.#^NODUP_RESTATED_REQ5^#
  T.#^NODUP_RESTATED_REQ_TYPE3^#
}
func checkRestatementNoDup6(_ arg: NoDupReq1 & NoDupReq23) {
  arg.#^NODUP_RESTATED_REQ6^#
  arg#^NODUP_RESTATED_REQ_NODOT3^#
}
func checkOverrideInclusion1(_ arg: Override) {
  arg.#^CHECK_PROT_OVERRIDES1^#
}
func checkOverrideInclusion2(_ arg: Override3) {
  arg.#^CHECK_PROT_OVERRIDES2^#
}

// CHECK_NODUP_RESTATED_REQ: Begin completions
// CHECK_NODUP_RESTATED_REQ-DAG: Decl[InstanceMethod]/{{Super|CurrNominal}}: foo()[#Void#]; name=foo()
// CHECK_NODUP_RESTATED_REQ-DAG: Decl[InstanceMethod]/{{Super|CurrNominal}}: roo({#arg1: Int#})[#Void#]
// CHECK_NODUP_RESTATED_REQ-DAG: Decl[InstanceVar]/{{Super|CurrNominal}}:    doo[#Int#]; name=doo
// CHECK_NODUP_RESTATED_REQ-DAG: Decl[InstanceMethod]/{{Super|CurrNominal}}: roo({#arg2: Int#})[#Void#]
// CHECK_NODUP_RESTATED_REQ-NOT: Decl[InstanceMethod]/{{Super|CurrNominal}}: foo()[#Void#]; name=foo()
// CHECK_NODUP_RESTATED_REQ-NOT: Decl[InstanceVar]/{{Super|CurrNominal}}:    doo[#Int#]; name=doo
// CHECK_NODUP_RESTATED_REQ: End completions

// CHECK_NODUP_RESTATED_REQ_NODOT: Begin completions
// CHECK_NODUP_RESTATED_REQ_NODOT: Decl[InstanceMethod]/{{Super|CurrNominal}}: .foo()[#Void#]; name=foo()
// CHECK_NODUP_RESTATED_REQ_NODOT: Decl[InstanceMethod]/{{Super|CurrNominal}}: .roo({#arg1: Int#})[#Void#];
// CHECK_NODUP_RESTATED_REQ_NODOT: Decl[Subscript]/{{Super|CurrNominal}}:      [{#Bool#}][#Bool#]; name=[Bool]
// CHECK_NODUP_RESTATED_REQ_NODOT: Decl[InstanceVar]/{{Super|CurrNominal}}:    .doo[#Int#]; name=doo
// CHECK_NODUP_RESTATED_REQ_NODOT: Decl[InstanceMethod]/{{Super|CurrNominal}}: .roo({#arg2: Int#})[#Void#];
// CHECK_NODUP_RESTATED_REQ_NODOT-NOT: Decl[InstanceMethod]/{{Super|CurrNominal}}: .foo()[#Void#]; name=foo()
// CHECK_NODUP_RESTATED_REQ_NODOT-NOT: Decl[Subscript]/{{Super|CurrNominal}}:      [{#Bool#}][#Bool#]; name=[Bool]
// CHECK_NODUP_RESTATED_REQ_NODOT-NOT: Decl[InstanceVar]/{{Super|CurrNominal}}:    .doo[#Int#]; name=doo
// CHECK_NODUP_RESTATED_REQ_NODOT: End completions

// CHECK_NODUP_RESTATED_REQ_TYPE: Begin completions
// CHECK_NODUP_RESTATED_REQ_TYPE-DAG: Decl[InstanceMethod]/Super: foo({#self: [[ARG:.+]]#})[#() -> Void#]; name=foo([[ARG]])
// CHECK_NODUP_RESTATED_REQ_TYPE-DAG: Decl[InstanceMethod]/Super: roo({#self: [[ARG]]#})[#(arg1: Int) -> Void#]; name=roo([[ARG]])
// CHECK_NODUP_RESTATED_REQ_TYPE-DAG: Decl[AssociatedType]/Super: E; name=E
// CHECK_NODUP_RESTATED_REQ_TYPE-DAG: Decl[InstanceMethod]/Super: roo({#self: [[ARG]]#})[#(arg2: Int) -> Void#]; name=roo([[ARG]])
// CHECK_NODUP_RESTATED_REQ_TYPE-NOT: Decl[InstanceMethod]/Super: foo({#self: [[ARG:.+]]#})[#() -> Void#]; name=foo([[ARG]])
// CHECK_NODUP_RESTATED_REQ_TYPE-NOT: Decl[AssociatedType]/Super: E; name=E
// CHECK_NODUP_RESTATED_REQ_TYPE: End completions

// CHECK_PROT_OVERRIDES: Decl[InstanceMethod]/{{Super|CurrNominal}}: foo({#(arg): NoDupReq1#})[#Void#]; name=foo(arg: NoDupReq1)
// CHECK_PROT_OVERRIDES: Decl[InstanceMethod]/{{Super|CurrNominal}}: foo({#(arg): NoDupReq2#})[#Void#]; name=foo(arg: NoDupReq2)

struct OnlyMe {}
protocol P4 {
  associatedtype T
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

func testConstrainedP4(_ x: P4) {
  x.#^PROTOCOL_EXT_P4^#
}
// PROTOCOL_EXT_P4-NOT: extP4

func testConstrainedConcrete1(_ x: Concrete1) {
  x.#^PROTOCOL_EXT_CONCRETE1^#
}
func testConstrainedConcrete2(_ x: Generic1<WillConformP1>) {
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

func testConstrainedConcrete3(_ x: Concrete2) {
  x.#^PROTOCOL_EXT_CONCRETE3^#
}
func testConstrainedConcrete3_sub(_ x: Concrete2) {
  x#^PROTOCOL_EXT_CONCRETE3_SUB^#
}
func testConstrainedConcrete4(_ x: Generic2<OnlyMe>) {
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
// PROTOCOL_EXT_TA_2-DAG: Decl[AssociatedType]/{{Super|CurrNominal}}: T
// PROTOCOL_EXT_TA: End completions

func testProtExtInit1() {
  Concrete1(#^PROTOCOL_EXT_INIT_1^#
}

// PROTOCOL_EXT_INIT_1: Begin completions
// PROTOCOL_EXT_INIT_1: Decl[Constructor]/Super:            ['(']{#x: Int#}[')'][#Concrete1#]{{; name=.+$}}
// PROTOCOL_EXT_INIT_1: End completions

func testProtExtInit2<S: P4 where S.T : P1>() {
  S(#^PROTOCOL_EXT_INIT_2^#
  S.#^PROTOCOL_EXT_INIT_3^#
  S#^PROTOCOL_EXT_INIT_4^#
}

// PROTOCOL_EXT_INIT_2: Decl[Constructor]/Super: ['(']{#x: Int#}[')'][#P4#]{{; name=.+$}}
// PROTOCOL_EXT_INIT_3: Decl[Constructor]/Super: init({#x: Int#})[#P4#]{{; name=.+$}}
// PROTOCOL_EXT_INIT_4: Decl[Constructor]/Super: ({#x: Int#})[#P4#]{{; name=.+$}}

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
// PROTOCOL_EXT_P4_T_DOT_1-DAG: Decl[InstanceMethod]/Super:   extP1({#self: WillConformP1#})[#() -> Void#]{{; name=.+$}}
// PROTOCOL_EXT_P4_T_DOT_1: End completions

protocol PWithT {
  associatedtype T
  func foo(_ x: T) -> T
}

extension PWithT {
  final func bar(_ x: T) -> T {
    return x
  }
}

// Note: PWithT cannot actually be used as an existential type because it has
// an associated type.  But we should still be able to give code completions.
func testUnusableProtExt(_ x: PWithT) {
  x.#^PROTOCOL_EXT_UNUSABLE_EXISTENTIAL^#
}
// PROTOCOL_EXT_UNUSABLE_EXISTENTIAL: Begin completions
// PROTOCOL_EXT_UNUSABLE_EXISTENTIAL: Decl[InstanceMethod]/CurrNominal:   foo({#(x): PWithT.T#})[#PWithT.T#]{{; name=.+}}
// PROTOCOL_EXT_UNUSABLE_EXISTENTIAL: Decl[InstanceMethod]/CurrNominal:   bar({#(x): PWithT.T#})[#PWithT.T#]{{; name=.+}}
// PROTOCOL_EXT_UNUSABLE_EXISTENTIAL: End completions

protocol dedupP {
  associatedtype T
  func foo() -> T
  var bar: T {get}
  subscript(x: T) -> T {get}
}

extension dedupP {
  func foo() -> T { return T() }
  var bar: T { return T() }
  subscript(x: T) -> T { return T() }
}

struct dedupS : dedupP {
  func foo() -> Int { return T() }
  var bar: Int = 5
  subscript(x: Int) -> Int { return 10 }
}

func testDeDuped(_ x: dedupS) {
  x#^PROTOCOL_EXT_DEDUP_1^#
// FIXME: Should produce 3 items (?)
// PROTOCOL_EXT_DEDUP_1: Begin completions, 7 items
// PROTOCOL_EXT_DEDUP_1: Decl[InstanceMethod]/CurrNominal:   .foo()[#Int#]; name=foo()
// PROTOCOL_EXT_DEDUP_1: Decl[InstanceVar]/CurrNominal:      .bar[#Int#]; name=bar
// PROTOCOL_EXT_DEDUP_1: Decl[Subscript]/CurrNominal:        [{#Int#}][#Int#]; name=[Int]
// PROTOCOL_EXT_DEDUP_1: Keyword[self]/CurrNominal:          .self[#dedupS#]; name=self
// PROTOCOL_EXT_DEDUP_1: End completions
}
func testDeDuped2(_ x: dedupP) {
  x#^PROTOCOL_EXT_DEDUP_2^#
// PROTOCOL_EXT_DEDUP_2: Begin completions, 5 items
// PROTOCOL_EXT_DEDUP_2: Decl[InstanceMethod]/CurrNominal:   .foo()[#dedupP.T#]; name=foo()
// PROTOCOL_EXT_DEDUP_2: Decl[InstanceVar]/CurrNominal:      .bar[#dedupP.T#]; name=bar
// PROTOCOL_EXT_DEDUP_2: Decl[Subscript]/CurrNominal:        [{#Self.T#}][#Self.T#]; name=[Self.T]
// PROTOCOL_EXT_DEDUP_2: Keyword[self]/CurrNominal:          .self[#dedupP#]; name=self
// PROTOCOL_EXT_DEDUP_2: End completions
}
func testDeDuped3<T : dedupP where T.T == Int>(_ x: T) {
  x#^PROTOCOL_EXT_DEDUP_3^#
// PROTOCOL_EXT_DEDUP_3: Begin completions, 5 items
// PROTOCOL_EXT_DEDUP_3: Decl[InstanceMethod]/Super:   .foo()[#Int#]; name=foo()
// PROTOCOL_EXT_DEDUP_3: Decl[InstanceVar]/Super:      .bar[#Int#]; name=bar
// PROTOCOL_EXT_DEDUP_3: Decl[Subscript]/Super:        [{#Self.T#}][#Self.T#]; name=[Self.T]
// PROTOCOL_EXT_DEDUP_3: Keyword[self]/CurrNominal:    .self[#T#]; name=self
// PROTOCOL_EXT_DEDUP_3: End completions
}

//===--- Check calls that may throw

func globalFuncThrows() throws {}
func globalFuncRethrows(_ x: () throws -> ()) rethrows {}
struct HasThrowingMembers {
  func memberThrows() throws {}
  func memberRethrows(_ x: () throws -> ()) rethrows {}
  init() throws {}
  init(x: () throws -> ()) rethrows {}
}

func testThrows001() {
  globalFuncThrows#^THROWS1^#

// THROWS1: Begin completions
// THROWS1: Pattern/CurrModule:               ()[' throws'][#Void#]; name=() throws
// THROWS1: End completions
}
func testThrows002() {
  globalFuncRethrows#^THROWS2^#

// THROWS2: Begin completions
// THROWS2: Pattern/CurrModule:               ({#(x): () throws -> ()##() throws -> ()#})[' rethrows'][#Void#]; name=(x: () throws -> ()) rethrows
// THROWS2: End completions
}
func testThrows003(_ x: HasThrowingMembers) {
  x.#^MEMBER_THROWS1^#
// MEMBER_THROWS1: Begin completions
// MEMBER_THROWS1-DAG: Decl[InstanceMethod]/CurrNominal:   memberThrows()[' throws'][#Void#]
// MEMBER_THROWS1-DAG: Decl[InstanceMethod]/CurrNominal:   memberRethrows({#(x): () throws -> ()##() throws -> ()#})[' rethrows'][#Void#]
// MEMBER_THROWS1: End completions
}
func testThrows004(_ x: HasThrowingMembers) {
  x.memberThrows#^MEMBER_THROWS2^#
// MEMBER_THROWS2: Begin completions
// MEMBER_THROWS2: Pattern/CurrModule:               ()[' throws'][#Void#]; name=() throws
// MEMBER_THROWS2: End completions
}
func testThrows005(_ x: HasThrowingMembers) {
  x.memberRethrows#^MEMBER_THROWS3^#
// MEMBER_THROWS3: Begin completions
// MEMBER_THROWS3: Pattern/CurrModule: ({#(x): () throws -> ()##() throws -> ()#})[' rethrows'][#Void#]; name=(x: () throws -> ()) rethrows
// MEMBER_THROWS3: End completions
}
func testThrows006() {
  HasThrowingMembers(#^INIT_THROWS1^#
// INIT_THROWS1: Begin completions
// INIT_THROWS1: Decl[Constructor]/CurrNominal:      ['(']{#x: () throws -> ()##() throws -> ()#}[')'][' rethrows'][#HasThrowingMembers#]
// INIT_THROWS1: End completions
}


// rdar://21346928
// Just sample some String API to sanity check.
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      unicodeScalars[#String.UnicodeScalarView#]
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      utf16[#String.UTF16View#]
func testWithAutoClosure1(_ x: String?) {
  (x ?? "autoclosure").#^AUTOCLOSURE1^#
}
func testWithAutoClosure2(_ x: String?) {
  let y = (x ?? "autoclosure").#^AUTOCLOSURE2^#
}
func testWithAutoClosure3(_ x: String?) {
  let y = (x ?? "autoclosure".#^AUTOCLOSURE3^#)
}
func testWithAutoClosure4(_ x: String?) {
  let y = { let z = (x ?? "autoclosure").#^AUTOCLOSURE4^# }
}
func testWithAutoClosure5(_ x: String?) {
  if let y = (x ?? "autoclosure").#^AUTOCLOSURE5^# {
  }
}

func testGenericTypealias1() {
  typealias MyPair<T> = (T, T)
  var x: MyPair<Int>
  x.#^GENERIC_TYPEALIAS_1^#
}
// GENERIC_TYPEALIAS_1: Pattern/CurrNominal:                0[#Int#];
// GENERIC_TYPEALIAS_1: Pattern/CurrNominal:                1[#Int#];

func testGenericTypealias2() {
  struct Enclose {
    typealias MyPair<T> = (T, T)
  }
  Enclose.#^GENERIC_TYPEALIAS_2^#
}
// GENERIC_TYPEALIAS_2: Decl[TypeAlias]/CurrNominal:        MyPair[#(T, T)#];

struct Deprecated {
  @available(*, deprecated)
  func deprecated(x: Deprecated) {
    x.#^DEPRECATED_1^#
  }
}
// DEPRECATED_1: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecated({#x: Deprecated#})[#Void#];

struct Person {
  var firstName: String
}
class Other { var nameFromOther: Int = 1 }
class TestDotExprWithNonNominal {
  var otherField: Other

  func test1() {
    let person = Person(firstName: otherField.#^DOT_EXPR_NON_NOMINAL_1^#)
// DOT_EXPR_NON_NOMINAL_1-NOT: Instance
// DOT_EXPR_NON_NOMINAL_1: Keyword[self]/CurrNominal:          self[#Other#]; name=self
// DOT_EXPR_NON_NOMINAL_1: Decl[InstanceVar]/CurrNominal:      nameFromOther[#Int#];
// DOT_EXPR_NON_NOMINAL_1-NOT: Instance
  }
  func test2() {
    let person = Person(firstName: 1.#^DOT_EXPR_NON_NOMINAL_2^#)
// DOT_EXPR_NON_NOMINAL_2-NOT: otherField
// DOT_EXPR_NON_NOMINAL_2-NOT: firstName
// DOT_EXPR_NON_NOMINAL_2: Keyword[self]/CurrNominal:          self[#Int#]; name=self
// DOT_EXPR_NON_NOMINAL_2: Decl[InstanceVar]/CurrNominal:      hashValue[#Int#];
// DOT_EXPR_NON_NOMINAL_2-NOT: otherField
// DOT_EXPR_NON_NOMINAL_2-NOT: firstName
  }
}

class Cat {
  struct Inner {
    var prop1: String
    var prop2: String
  }
  var `class`: Inner
}
func testKeyword(cat: Cat) {
  let _ = cat.#^KEYWORD_1^#
// KEYWORD_1: Begin completions
// KEYWORD_1-DAG: Keyword[self]/CurrNominal:          self[#Cat#]; name=self
// KEYWORD_1-DAG: Decl[InstanceVar]/CurrNominal:      class[#Cat.Inner#]; name=class
// KEYWORD_1: End completions

  let _ = cat.class#^KEYWORD_2^#
// KEYWORD_2: Begin completions
// KEYWORD_2-DAG: Decl[InstanceVar]/CurrNominal:      .prop1[#String#]; name=prop1
// KEYWORD_2-DAG: Decl[InstanceVar]/CurrNominal:      .prop2[#String#]; name=prop2
// KEYWORD_2-DAG: BuiltinOperator/None:                = {#Cat.Inner#}[#Void#]; name== Cat.Inner
// KEYWORD_2: End completions

  let _ = cat.class.#^KEYWORD_3^#
// KEYWORD_3: Begin completions
// KEYWORD_3-DAG: Decl[InstanceVar]/CurrNominal:      prop1[#String#]; name=prop1
// KEYWORD_3-DAG: Decl[InstanceVar]/CurrNominal:      prop2[#String#]; name=prop2
// KEYWORD_3: End completions
}
