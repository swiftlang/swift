// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PROTOCOL_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_PROTOCOL < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_GENERIC_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_FUNC_PARAM_GENERIC_1 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_GENERIC_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_FUNC_PARAM_GENERIC_2 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_GENERIC_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_FUNC_PARAM_GENERIC_3 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_GENERIC_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_FUNC_PARAM_GENERIC_4 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_GENERIC_5 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_FUNC_PARAM_GENERIC_5 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_1 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_2 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_2 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_LOCAL_VAR_IN_CONSTRUCTOR_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_LOCAL_VAR_IN_MEMBER_FUNC_1 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_LOCAL_VAR_IN_DESTRUCTOR_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_LOCAL_VAR_IN_MEMBER_FUNC_1 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_LOCAL_VAR_IN_INSTANCE_FUNC_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_LOCAL_VAR_IN_MEMBER_FUNC_1 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_GLOBAL_VAR_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_TYPEALIAS_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_TYPEALIAS_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_TYPEALIAS_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_ASSOC_TYPE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_ASSOC_TYPE_INHERITANCE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_EXTENSION_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_EXTENSION_INHERITANCE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_EXTENSION_INHERITANCE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_EXTENSION_INHERITANCE_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_STRUCT_INHERITANCE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_STRUCT_INHERITANCE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_STRUCT_INHERITANCE_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_STRUCT_INHERITANCE_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_STRUCT_INHERITANCE_5 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_STRUCT_INHERITANCE_6 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_STRUCT_INHERITANCE_7 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_STRUCT_INHERITANCE_8 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CLASS_INHERITANCE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CLASS_INHERITANCE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_ENUM_INHERITANCE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_ENUM_INHERITANCE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PROTOCOL_INHERITANCE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PROTOCOL_INHERITANCE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_TUPLE_TYPE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_TUPLE_TYPE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_TUPLE_TYPE_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_TUPLE_TYPE_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_TUPLE_TYPE_5 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_TUPLE_TYPE_6 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNCTION_TYPE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNCTION_TYPE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNCTION_TYPE_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNCTION_TYPE_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNCTION_TYPE_5 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNCTION_TYPE_6 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PROTOCOL_COMPOSITION_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PROTOCOL_COMPOSITION_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PROTOCOL_COMPOSITION_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_NESTED_TYPES_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_NESTED_TYPES_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_NESTED_TYPES_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_DERIVED_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_NESTED_TYPES_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_DERIVED_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_INSTANCE_VAR_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_INSTANCE_VAR_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_INSTANCE_VAR_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_DERIVED_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_INSTANCE_VAR_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_DERIVED_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_LOCAL_VAR_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_LOCAL_VAR_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_LOCAL_VAR_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_DERIVED_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_LOCAL_VAR_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_DERIVED_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_BASE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_BASE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_BASE_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_BASE_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_BASE_NO_DOT_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_NO_DOT_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_BASE_NO_DOT_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_NO_DOT_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_BASE_NO_DOT_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_BASE_1_NO_DOT_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_DERIVED_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_DERIVED_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_DERIVED_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_DERIVED_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_DERIVED_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=VAR_DERIVED_1_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// FIXME: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_GENERIC_1 > %t.types.txt
// FIXME: FileCheck %s -check-prefix=TYPE_IDENTIFIER_GENERIC_1 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_GENERIC_2 > %t.types.txt
// FIXME: FileCheck %s -check-prefix=TYPE_IDENTIFIER_GENERIC_2 < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITHOUT_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_GENERIC_3 | FileCheck %s -check-prefix=TYPE_IDENTIFIER_GENERIC_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IDENTIFIER_IRRELEVANT_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IDENTIFIER_IRRELEVANT_1 < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSIDE_AS_CAST_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

//===--- Helper types that are used in this test

struct FooStruct {
}

var fooObject: FooStruct

func fooFunc() -> FooStruct {
  return fooObject
}

enum FooEnum {
}

class FooClass {
}

protocol FooProtocol {
  var fooInstanceVar: Int
  typealias FooTypeAlias1
  func fooInstanceFunc0() -> Double
  func fooInstanceFunc1(a: Int) -> Double
  subscript(i: Int) -> Double
}

protocol BarProtocol {
  var barInstanceVar: Int
  typealias BarTypeAlias1
  func barInstanceFunc0() -> Double
  func barInstanceFunc1(a: Int) -> Double
}

typealias FooTypealias = Int

// WITH_GLOBAL_TYPES: Begin completions
// Global completions
// WITH_GLOBAL_TYPES-DAG: Decl[Struct]/CurrModule:    FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Enum]/CurrModule:      FooEnum[#FooEnum#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Class]/CurrModule:     FooClass[#FooClass#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Protocol]/CurrModule:  FooProtocol[#FooProtocol#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[TypeAlias]/CurrModule: FooTypealias[#Int#]{{; name=.+$}}
// WITH_GLOBAL_TYPES: End completions

// GLOBAL_NEGATIVE-NOT: fooObject
// GLOBAL_NEGATIVE-NOT: fooFunc

// WITHOUT_GLOBAL_TYPES-NOT: FooStruct
// WITHOUT_GLOBAL_TYPES-NOT: FooEnum
// WITHOUT_GLOBAL_TYPES-NOT: FooClass
// WITHOUT_GLOBAL_TYPES-NOT: FooProtocol
// WITHOUT_GLOBAL_TYPES-NOT: FooTypealias

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

//===---
//===--- Test that we include 'Self' type while completing inside a protocol.
//===---

// TYPE_IN_PROTOCOL: Begin completions
// TYPE_IN_PROTOCOL-DAG: Decl[GenericTypeParam]/CurrNominal: Self[#`Self`#]{{; name=.+$}}
// TYPE_IN_PROTOCOL: End completions

protocol TestSelf1 {
  func instanceFunc() -> #^TYPE_IN_PROTOCOL_1^#
}

//===---
//===--- Test that we include types from generic parameter lists.
//===---
// FIXME: tests for constructors and destructors.

func testTypeInParamGeneric1<
    GenericFoo : FooProtocol,
    GenericBar : protocol<FooProtocol, BarProtocol>,
    GenericBaz>(a: #^TYPE_IN_FUNC_PARAM_GENERIC_1^#

// TYPE_IN_FUNC_PARAM_GENERIC_1: Begin completions
// Generic parameters of the function.
// TYPE_IN_FUNC_PARAM_GENERIC_1-DAG: Decl[GenericTypeParam]/Local: GenericFoo[#GenericFoo#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_1-DAG: Decl[GenericTypeParam]/Local: GenericBar[#GenericBar#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_1-DAG: Decl[GenericTypeParam]/Local: GenericBaz[#GenericBaz#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_1: End completions

struct TestTypeInParamGeneric2<
    StructGenericFoo : FooProtocol,
    StructGenericBar : protocol<FooProtocol, BarProtocol>,
    StructGenericBaz> {
  func testTypeInParamGeneric2(a: #^TYPE_IN_FUNC_PARAM_GENERIC_2^#
}

// TYPE_IN_FUNC_PARAM_GENERIC_2: Begin completions
// TYPE_IN_FUNC_PARAM_GENERIC_2-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericFoo[#StructGenericFoo#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_2-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBar[#StructGenericBar#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_2-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBaz[#StructGenericBaz#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_2: End completions

struct TestTypeInParamGeneric3 {
  func testTypeInParamGeneric3<
      GenericFoo : FooProtocol,
      GenericBar : protocol<FooProtocol, BarProtocol>,
      GenericBaz>(a: #^TYPE_IN_FUNC_PARAM_GENERIC_3^#
}

// TYPE_IN_FUNC_PARAM_GENERIC_3: Begin completions
// TYPE_IN_FUNC_PARAM_GENERIC_3-DAG: Decl[GenericTypeParam]/Local: GenericFoo[#GenericFoo#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_3-DAG: Decl[GenericTypeParam]/Local: GenericBar[#GenericBar#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_3-DAG: Decl[GenericTypeParam]/Local: GenericBaz[#GenericBaz#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_3: End completions

struct TestTypeInParamGeneric4<
    StructGenericFoo : FooProtocol,
    StructGenericBar : protocol<FooProtocol, BarProtocol>,
    StructGenericBaz> {
  func testTypeInParamGeneric4<
      GenericFoo : FooProtocol,
      GenericBar : protocol<FooProtocol, BarProtocol>,
      GenericBaz>(a: #^TYPE_IN_FUNC_PARAM_GENERIC_4^#
}

// TYPE_IN_FUNC_PARAM_GENERIC_4: Begin completions
// Generic parameters of the struct.
// TYPE_IN_FUNC_PARAM_GENERIC_4-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericFoo[#StructGenericFoo#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_4-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBar[#StructGenericBar#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_4-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBaz[#StructGenericBaz#]{{; name=.+$}}
// Generic parameters of the function.
// TYPE_IN_FUNC_PARAM_GENERIC_4-DAG: Decl[GenericTypeParam]/Local: GenericFoo[#GenericFoo#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_4-DAG: Decl[GenericTypeParam]/Local: GenericBar[#GenericBar#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_4-DAG: Decl[GenericTypeParam]/Local: GenericBaz[#GenericBaz#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_4: End completions

struct TestTypeInParamGeneric5<StructGenericFoo> {
  struct TestTypeInParamGeneric5a<StructGenericBar> {
    struct TestTypeInParamGeneric5b<StructGenericBaz> {
      func testTypeInParamGeneric5<GenericFoo>(a: #^TYPE_IN_FUNC_PARAM_GENERIC_5^#
    }
  }
}

// TYPE_IN_FUNC_PARAM_GENERIC_5: Begin completions
// Generic parameters of the containing structs.
// TYPE_IN_FUNC_PARAM_GENERIC_5-DAG: Decl[GenericTypeParam]/OutNominal: StructGenericFoo[#StructGenericFoo#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_5-DAG: Decl[GenericTypeParam]/OutNominal: StructGenericBar[#StructGenericBar#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_5-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBaz[#StructGenericBaz#]{{; name=.+$}}
// Generic parameters of the function.
// TYPE_IN_FUNC_PARAM_GENERIC_5-DAG: Decl[GenericTypeParam]/Local: GenericFoo[#GenericFoo#]{{; name=.+$}}
// TYPE_IN_FUNC_PARAM_GENERIC_5: End completions

struct TestTypeInConstructorParamGeneric1<
    StructGenericFoo : FooProtocol,
    StructGenericBar : protocol<FooProtocol, BarProtocol>,
    StructGenericBaz> {
  init(a: #^TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_1^#
}

// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_1: Begin completions
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_1-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericFoo[#StructGenericFoo#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_1-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBar[#StructGenericBar#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_1-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBaz[#StructGenericBaz#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_1: End completions

struct TestTypeInConstructorParamGeneric2 {
  init<GenericFoo : FooProtocol,
       GenericBar : protocol<FooProtocol, BarProtocol>,
       GenericBaz>(a: #^TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_2^#
}

// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_2: Begin completions
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_2-DAG: Decl[GenericTypeParam]/Local: GenericFoo[#GenericFoo#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_2-DAG: Decl[GenericTypeParam]/Local: GenericBar[#GenericBar#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_2-DAG: Decl[GenericTypeParam]/Local: GenericBaz[#GenericBaz#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_2: End completions

struct TestTypeInConstructorParamGeneric3<
    StructGenericFoo : FooProtocol,
    StructGenericBar : protocol<FooProtocol, BarProtocol>,
    StructGenericBaz> {
  init<GenericFoo : FooProtocol,
       GenericBar : protocol<FooProtocol, BarProtocol>,
       GenericBaz>(a: #^TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3^#
}

// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3: Begin completions
// Generic parameters of the struct.
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericFoo[#StructGenericFoo#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBar[#StructGenericBar#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBaz[#StructGenericBaz#]{{; name=.+$}}
// Generic parameters of the constructor.
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3-DAG: Decl[GenericTypeParam]/Local: GenericFoo[#GenericFoo#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3-DAG: Decl[GenericTypeParam]/Local: GenericBar[#GenericBar#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3-DAG: Decl[GenericTypeParam]/Local: GenericBaz[#GenericBaz#]{{; name=.+$}}
// TYPE_IN_CONSTRUCTOR_PARAM_GENERIC_3: End completions

// No tests for destructors: destructors don't have parameters.

//===---
//===--- Test that we can complete types in variable declarations.
//===---

func testTypeInLocalVarInFreeFunc1() {
  var localVar: #^TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_1^#
}

func testTypeInLocalVarInFreeFunc2() {
  struct NestedStruct {}
  class NestedClass {}
  enum NestedEnum {
    case NestedEnumX(Int)
  }

  typealias NestedTypealias = Int

  var localVar: #^TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_2^#
}
// TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_2: Begin completions
// TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_2-DAG: Decl[Struct]/Local:    NestedStruct[#NestedStruct#]{{; name=.+$}}
// TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_2-DAG: Decl[Class]/Local:     NestedClass[#NestedClass#]{{; name=.+$}}
// TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_2-DAG: Decl[Enum]/Local:      NestedEnum[#NestedEnum#]{{; name=.+$}}
// TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_2-DAG: Decl[TypeAlias]/Local: NestedTypealias[#Int#]{{; name=.+$}}
// TYPE_IN_LOCAL_VAR_IN_FREE_FUNC_2: End completions

class TestTypeInLocalVarInMemberFunc1 {
  struct NestedStruct {}
  class NestedClass {}
  enum NestedEnum {
    case NestedEnumX(Int)
  }

  typealias NestedTypealias = Int

  init() {
    var localVar: #^TYPE_IN_LOCAL_VAR_IN_CONSTRUCTOR_1^#
  }

  deinit {
    var localVar: #^TYPE_IN_LOCAL_VAR_IN_DESTRUCTOR_1^#
  }

  func test() {
    var localVar: #^TYPE_IN_LOCAL_VAR_IN_INSTANCE_FUNC_1^#
  }
}
// TYPE_IN_LOCAL_VAR_IN_MEMBER_FUNC_1: Begin completions
// TYPE_IN_LOCAL_VAR_IN_MEMBER_FUNC_1-DAG: Decl[Struct]/CurrNominal:    NestedStruct[#TestTypeInLocalVarInMemberFunc1.NestedStruct#]{{; name=.+$}}
// TYPE_IN_LOCAL_VAR_IN_MEMBER_FUNC_1-DAG: Decl[Class]/CurrNominal:     NestedClass[#TestTypeInLocalVarInMemberFunc1.NestedClass#]{{; name=.+$}}
// TYPE_IN_LOCAL_VAR_IN_MEMBER_FUNC_1-DAG: Decl[Enum]/CurrNominal:      NestedEnum[#TestTypeInLocalVarInMemberFunc1.NestedEnum#]{{; name=.+$}}
// TYPE_IN_LOCAL_VAR_IN_MEMBER_FUNC_1-DAG: Decl[TypeAlias]/CurrNominal: NestedTypealias[#Int#]{{; name=.+$}}
// TYPE_IN_LOCAL_VAR_IN_MEMBER_FUNC_1: End completions

var TypeInGlobalVar1: #^TYPE_IN_GLOBAL_VAR_1^#

//===---
//===--- Test that we can complete types in typealias declarations.
//===---

typealias TypeInTypealias1 = #^TYPE_IN_TYPEALIAS_1^#

typealias TypeInTypealias2 = (#^TYPE_IN_TYPEALIAS_2^#

typealias TypeInTypealias3 = ((#^TYPE_IN_TYPEALIAS_3^#

func resyncParser1() {}

//===---
//===--- Test that we can complete types in associated type declarations.
//===---

protocol AssocType1 {
  typealias AssocType = #^TYPE_IN_ASSOC_TYPE_1^#
}

//===---
//===--- Test that we can complete types in inheritance clause of associated type declarations.
//===---

protocol AssocType1 {
  typealias AssocType : #^TYPE_IN_ASSOC_TYPE_INHERITANCE_1^#
}

//===---
//===--- Test that we can complete types in extension declarations.
//===---

extension #^TYPE_IN_EXTENSION_1^#

//===---
//===--- Test that we can complete types in the extension inheritance clause.
//===---

extension TypeInExtensionInheritance1 : #^TYPE_IN_EXTENSION_INHERITANCE_1^#

extension TypeInExtensionInheritance2 : #^TYPE_IN_EXTENSION_INHERITANCE_2^# {
}

extension TypeInExtensionInheritance3 : FooProtocol, #^TYPE_IN_EXTENSION_INHERITANCE_3^# {
}

//===---
//===--- Test that we can complete types in the struct inheritance clause.
//===---

struct TypeInStructInheritance1 : #^TYPE_IN_STRUCT_INHERITANCE_1^#

struct TypeInStructInheritance2 : , #^TYPE_IN_STRUCT_INHERITANCE_2^#

struct TypeInStructInheritance3 : FooProtocol, #^TYPE_IN_STRUCT_INHERITANCE_3^#

struct TypeInStructInheritance4 : FooProtocol., #^TYPE_IN_STRUCT_INHERITANCE_4^#

struct TypeInStructInheritance5 : #^TYPE_IN_STRUCT_INHERITANCE_5^# {
}

struct TypeInStructInheritance6 : , #^TYPE_IN_STRUCT_INHERITANCE_6^# {
}

struct TypeInStructInheritance7 : FooProtocol, #^TYPE_IN_STRUCT_INHERITANCE_7^# {
}

struct TypeInStructInheritance8 : FooProtocol., #^TYPE_IN_STRUCT_INHERITANCE_8^# {
}

//===---
//===--- Test that we can complete types in the class inheritance clause.
//===---

class TypeInClassInheritance1 : #^TYPE_IN_CLASS_INHERITANCE_1^# {
}

class TypeInClassInheritance2 : #^TYPE_IN_CLASS_INHERITANCE_2^#

//===---
//===--- Test that we can complete types in the enum inheritance clause.
//===---

enum TypeInEnumInheritance1 : #^TYPE_IN_ENUM_INHERITANCE_1^#

enum TypeInEnumInheritance2 : #^TYPE_IN_ENUM_INHERITANCE_2^# {
}

//===---
//===--- Test that we can complete types in the protocol inheritance clause.
//===---

protocol TypeInProtocolInheritance1 : #^TYPE_IN_PROTOCOL_INHERITANCE_1^#

protocol TypeInProtocolInheritance2 : #^TYPE_IN_PROTOCOL_INHERITANCE_2^# {
}

//===---
//===--- Test that we can complete types in tuple types.
//===---

func testTypeInTupleType1() {
  var localVar: (#^TYPE_IN_TUPLE_TYPE_1^#
}

func testTypeInTupleType2() {
  var localVar: (a: #^TYPE_IN_TUPLE_TYPE_2^#
}

func testTypeInTupleType3() {
  var localVar: (Int, #^TYPE_IN_TUPLE_TYPE_3^#
}

func testTypeInTupleType4() {
  var localVar: (a: Int, #^TYPE_IN_TUPLE_TYPE_4^#
}

func testTypeInTupleType5() {
  var localVar: (Int, a: #^TYPE_IN_TUPLE_TYPE_5^#
}

func testTypeInTupleType6() {
  var localVar: (a:, #^TYPE_IN_TUPLE_TYPE_6^#
}

func testTypeInTupleType7() {
  var localVar: (a: b: #^TYPE_IN_TUPLE_TYPE_7^#
}

//===---
//===--- Test that we can complete types in function types.
//===---

func testTypeInFunctionType1() {
  var localVar: #^TYPE_IN_FUNCTION_TYPE_1^# ->
}

func testTypeInFunctionType2() {
  var localVar: (#^TYPE_IN_FUNCTION_TYPE_2^#) -> ()
}

func testTypeInFunctionType3() {
  var localVar: () -> #^TYPE_IN_FUNCTION_TYPE_3^#
}

func testTypeInFunctionType4() {
  var localVar: (Int) -> #^TYPE_IN_FUNCTION_TYPE_4^#
}

func testTypeInFunctionType5() {
  var localVar: (a: Int) -> #^TYPE_IN_FUNCTION_TYPE_5^#
}

func testTypeInFunctionType6() {
  var localVar: (a: Int, ) -> #^TYPE_IN_FUNCTION_TYPE_6^#
}

//===---
//===--- Test that we can complete types in protocol compositions.
//===---

func testTypeInProtocolComposition1() {
  var localVar: protocol<#^TYPE_IN_PROTOCOL_COMPOSITION_1^#
}

func testTypeInProtocolComposition2() {
  var localVar: protocol<, #^TYPE_IN_PROTOCOL_COMPOSITION_2^#
}

func testTypeInProtocolComposition3() {
  var localVar: protocol<FooProtocol, #^TYPE_IN_PROTOCOL_COMPOSITION_3^#
}

//===---
//===--- Test that we can complete types from extensions and base classes.
//===---

class VarBase1 {
  var instanceVarBase1: #^TYPE_IN_INSTANCE_VAR_1^#

  func paramNestedTypesBase1(a: #^TYPE_IN_FUNC_PARAM_NESTED_TYPES_1^#

  func localVarBaseTest1() {
    var localVar: #^TYPE_IN_LOCAL_VAR_1^#
  }

  // Define types after all tests to test delayed parsing of decls.

  struct BaseNestedStruct {}
  class BaseNestedClass {}
  enum BaseNestedEnum {
    case BaseEnumX(Int)
  }

  typealias BaseNestedTypealias = Int
}

extension VarBase1 {
  var instanceVarBaseExt1: #^TYPE_IN_INSTANCE_VAR_2^#

  func paramNestedTypesBaseExt1(a: #^TYPE_IN_FUNC_PARAM_NESTED_TYPES_2^#

  func localVarBaseExtTest1() {
    var localVar: #^TYPE_IN_LOCAL_VAR_2^#
  }

  // Define types after all tests to test delayed parsing of decls.

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  enum BaseExtNestedEnum {
    case BaseExtEnumX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

// VAR_BASE_1_TYPES: Begin completions
// From VarBase1
// VAR_BASE_1_TYPES-DAG: Decl[Struct]/CurrNominal:    BaseNestedStruct[#VarBase1.BaseNestedStruct#]{{; name=.+$}}
// VAR_BASE_1_TYPES-DAG: Decl[Class]/CurrNominal:     BaseNestedClass[#VarBase1.BaseNestedClass#]{{; name=.+$}}
// VAR_BASE_1_TYPES-DAG: Decl[Enum]/CurrNominal:      BaseNestedEnum[#VarBase1.BaseNestedEnum#]{{; name=.+$}}
// VAR_BASE_1_TYPES-DAG: Decl[TypeAlias]/CurrNominal: BaseNestedTypealias[#Int#]{{; name=.+$}}
// From VarBase1 extension
// VAR_BASE_1_TYPES-DAG: Decl[Struct]/CurrNominal:    BaseExtNestedStruct[#VarBase1.BaseExtNestedStruct#]{{; name=.+$}}
// VAR_BASE_1_TYPES-DAG: Decl[Class]/CurrNominal:     BaseExtNestedClass[#VarBase1.BaseExtNestedClass#]{{; name=.+$}}
// VAR_BASE_1_TYPES-DAG: Decl[Enum]/CurrNominal:      BaseExtNestedEnum[#VarBase1.BaseExtNestedEnum#]{{; name=.+$}}
// VAR_BASE_1_TYPES-DAG: Decl[TypeAlias]/CurrNominal: BaseExtNestedTypealias[#Int#]{{; name=.+$}}
// VAR_BASE_1_TYPES: End completions

// VAR_BASE_1_NO_DOT_TYPES: Begin completions
// From VarBase1
// VAR_BASE_1_NO_DOT_TYPES-DAG: Decl[Struct]/CurrNominal:    .BaseNestedStruct[#VarBase1.BaseNestedStruct#]{{; name=.+$}}
// VAR_BASE_1_NO_DOT_TYPES-DAG: Decl[Class]/CurrNominal:     .BaseNestedClass[#VarBase1.BaseNestedClass#]{{; name=.+$}}
// VAR_BASE_1_NO_DOT_TYPES-DAG: Decl[Enum]/CurrNominal:      .BaseNestedEnum[#VarBase1.BaseNestedEnum#]{{; name=.+$}}
// VAR_BASE_1_NO_DOT_TYPES-DAG: Decl[TypeAlias]/CurrNominal: .BaseNestedTypealias[#Int#]{{; name=.+$}}
// From VarBase1 extension
// VAR_BASE_1_NO_DOT_TYPES-DAG: Decl[Struct]/CurrNominal:    .BaseExtNestedStruct[#VarBase1.BaseExtNestedStruct#]{{; name=.+$}}
// VAR_BASE_1_NO_DOT_TYPES-DAG: Decl[Class]/CurrNominal:     .BaseExtNestedClass[#VarBase1.BaseExtNestedClass#]{{; name=.+$}}
// VAR_BASE_1_NO_DOT_TYPES-DAG: Decl[Enum]/CurrNominal:      .BaseExtNestedEnum[#VarBase1.BaseExtNestedEnum#]{{; name=.+$}}
// VAR_BASE_1_NO_DOT_TYPES-DAG: Decl[TypeAlias]/CurrNominal: .BaseExtNestedTypealias[#Int#]{{; name=.+$}}
// VAR_BASE_1_NO_DOT_TYPES: End completions

class VarDerived1 : VarBase1 {
  var instanceVarDerived1 : #^TYPE_IN_INSTANCE_VAR_3^#

  func paramNestedTypesDerived1(a: #^TYPE_IN_FUNC_PARAM_NESTED_TYPES_3^#

  func localVarDerivedTest1() {
    var localVar : #^TYPE_IN_LOCAL_VAR_3^#
  }

  // Define types after all tests to test delayed parsing of decls.

  struct DerivedNestedStruct {}
  class DerivedNestedClass {}
  enum DerivedNestedEnum {
    case DerivedEnumX(Int)
  }

  typealias DerivedNestedTypealias = Int
}

extension VarDerived1 {
  var instanceVarDerivedExt1 : #^TYPE_IN_INSTANCE_VAR_4^#

  func paramNestedTypesDerivedExt1(a: #^TYPE_IN_FUNC_PARAM_NESTED_TYPES_4^#

  func localVarDerivedExtTest1() {
    var localVar : #^TYPE_IN_LOCAL_VAR_4^#
  }

  // Define types after all tests to test delayed parsing of decls.

  struct DerivedExtNestedStruct {}
  class DerivedExtNestedClass {}
  enum DerivedExtNestedEnum {
    case DerivedExtEnumX(Int)
  }

  typealias DerivedExtNestedTypealias = Int
}

// VAR_DERIVED_1_TYPES: Begin completions
// From VarBase1
// VAR_DERIVED_1_TYPES-DAG: Decl[Struct]/Super:          BaseNestedStruct[#VarBase1.BaseNestedStruct#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[Class]/Super:           BaseNestedClass[#VarBase1.BaseNestedClass#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[Enum]/Super:            BaseNestedEnum[#VarBase1.BaseNestedEnum#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[TypeAlias]/Super:       BaseNestedTypealias[#Int#]{{; name=.+$}}
// From VarBase1 extension
// VAR_DERIVED_1_TYPES-DAG: Decl[Struct]/Super:          BaseExtNestedStruct[#VarBase1.BaseExtNestedStruct#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[Class]/Super:           BaseExtNestedClass[#VarBase1.BaseExtNestedClass#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[Enum]/Super:            BaseExtNestedEnum[#VarBase1.BaseExtNestedEnum#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[TypeAlias]/Super:       BaseExtNestedTypealias[#Int#]{{; name=.+$}}
// From VarDerived1
// VAR_DERIVED_1_TYPES-DAG: Decl[Struct]/CurrNominal:    DerivedNestedStruct[#VarDerived1.DerivedNestedStruct#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[Class]/CurrNominal:     DerivedNestedClass[#VarDerived1.DerivedNestedClass#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[Enum]/CurrNominal:      DerivedNestedEnum[#VarDerived1.DerivedNestedEnum#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[TypeAlias]/CurrNominal: DerivedNestedTypealias[#Int#]{{; name=.+$}}
// From VarDerived1 extension
// VAR_DERIVED_1_TYPES-DAG: Decl[Struct]/CurrNominal:    DerivedExtNestedStruct[#VarDerived1.DerivedExtNestedStruct#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[Class]/CurrNominal:     DerivedExtNestedClass[#VarDerived1.DerivedExtNestedClass#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[Enum]/CurrNominal:      DerivedExtNestedEnum[#VarDerived1.DerivedExtNestedEnum#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES-DAG: Decl[TypeAlias]/CurrNominal: DerivedExtNestedTypealias[#Int#]{{; name=.+$}}
// VAR_DERIVED_1_TYPES: End completions

//===---
//===--- Test that we can complete based on user-provided type-identifier.
//===---

func testTypeIdentifierBase1(a: VarBase1.#^TYPE_IDENTIFIER_BASE_1^#
func testTypeIdentifierBase2(a: Int, b: VarBase1.#^TYPE_IDENTIFIER_BASE_2^#
func testTypeIdentifierBase3(a: unknown_type, b: VarBase1.#^TYPE_IDENTIFIER_BASE_3^#
func testTypeIdentifierBase4(a: , b: VarBase1.#^TYPE_IDENTIFIER_BASE_4^#

func testTypeIdentifierBaseNoDot1(a: VarBase1#^TYPE_IDENTIFIER_BASE_NO_DOT_1^#

func testTypeIdentifierBaseNoDot2() {
  var localVar : protocol<VarBase1#^TYPE_IDENTIFIER_BASE_NO_DOT_2^#
}

typealias testTypeIdentifierBaseNoDot3 = VarBase1#^TYPE_IDENTIFIER_BASE_NO_DOT_3^#

func testTypeIdentifierDerived1(a: VarDerived1.#^TYPE_IDENTIFIER_DERIVED_1^#

func testTypeIdentifierDerived2() {
  var localVar : protocol<VarDerived1.#^TYPE_IDENTIFIER_DERIVED_2^#
}

typealias testTypeIdentifierDerived3 = VarDerived1.#^TYPE_IDENTIFIER_DERIVED_3^#

func testTypeIdentifierGeneric1<
    GenericFoo : FooProtocol
    >(a: GenericFoo.#^TYPE_IDENTIFIER_GENERIC_1^#

// TYPE_IDENTIFIER_GENERIC_1: Begin completions
// TYPE_IDENTIFIER_GENERIC_1-NEXT: Decl[TypeAlias]/Super: FooTypeAlias1{{; name=.+$}}
// TYPE_IDENTIFIER_GENERIC_1-NEXT: Keyword/None:          Type[#GenericFoo.Type#]
// TYPE_IDENTIFIER_GENERIC_1-NEXT: Keyword/None:          self[#GenericFoo#]
// TYPE_IDENTIFIER_GENERIC_1-NEXT: End completions

func testTypeIdentifierGeneric2<
    GenericFoo : protocol<FooProtocol, BarProtocol>
    >(a: GenericFoo.#^TYPE_IDENTIFIER_GENERIC_2^#

// TYPE_IDENTIFIER_GENERIC_2: Begin completions
// TYPE_IDENTIFIER_GENERIC_2-NEXT: Decl[TypeAlias]/Super: BarTypeAlias1{{; name=.+$}}
// TYPE_IDENTIFIER_GENERIC_2-NEXT: Decl[TypeAlias]/Super: FooTypeAlias1{{; name=.+$}}
// TYPE_IDENTIFIER_GENERIC_2-NEXT: Keyword/None:          Type[#GenericFoo.Type#]
// TYPE_IDENTIFIER_GENERIC_2-NEXT: Keyword/None:          self[#GenericFoo#]
// TYPE_IDENTIFIER_GENERIC_2-NEXT: End completions

func testTypeIdentifierGeneric3<
    GenericFoo>(a: GenericFoo.#^TYPE_IDENTIFIER_GENERIC_3^#

// TYPE_IDENTIFIER_GENERIC_3: Begin completions
// TYPE_IDENTIFIER_GENERIC_3-NEXT: Keyword/None:          Type[#GenericFoo.Type#]
// TYPE_IDENTIFIER_GENERIC_3-NEXT: Keyword/None:          self[#GenericFoo#]
// TYPE_IDENTIFIER_GENERIC_3-NEXT: End completions

func testTypeIdentifierIrrelevant1() {
  var a: Int
  #^TYPE_IDENTIFIER_IRRELEVANT_1^#
}
// TYPE_IDENTIFIER_IRRELEVANT_1: Begin completions
// TYPE_IDENTIFIER_IRRELEVANT_1-DAG: Decl[LocalVar]/Local: a[#Int#]{{; name=.+$}}
// TYPE_IDENTIFIER_IRRELEVANT_1-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// TYPE_IDENTIFIER_IRRELEVANT_1: End completions

//===---
//===--- Test that we can complete types in 'as' cast.
//===---

func testAsCast1(a: Int) {
  a as #^INSIDE_AS_CAST_1^#
}
