// Failing the test as 'Boolean' to 'Boolean' rename introduced problems
// with converting bool literals to DarwinBoolean

// XFAIL: *

// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s -import-objc-header %S/Inputs/BoolBridgingTests.h | %FileCheck %s --check-prefix=CHECK $(test '%target-os' = 'macosx' -o '(' '%target-os' = 'ios' -a '%target-ptrsize' = '32' ')' && echo '--check-prefix=CHECK-OBJCBOOL')

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil @_TF18objc_bool_bridging13testFunctionsFT_T_
public func testFunctions() {
  // CHECK-NOT: convert
  // CHECK: [[FUNC_1:%.+]] = function_ref @testCBool
  // CHECK-NOT: convert
  // CHECK: = apply [[FUNC_1]]({{%.+}}) : $@convention(c) (Bool) -> Bool
  // CHECK-NOT: convert
  _ = testCBool(true)

  // CHECK-OBJCBOOL: [[FUNC_2:%.+]] = function_ref @testObjCBool
  // CHECK-OBJCBOOL: [[BOOL_TO_OBJCBOOL:%.+]] = function_ref @_TF10ObjectiveC22_convertBoolToObjCBoolFSbVS_8ObjCBool
  // CHECK-OBJCBOOL: [[INPUT_2:%.+]] = apply [[BOOL_TO_OBJCBOOL]]({{%.+}})
  // CHECK-OBJCBOOL: [[RESULT_2:%.+]] = apply [[FUNC_2]]({{%.+}}) : $@convention(c) (ObjCBool) -> ObjCBool
  // CHECK-OBJCBOOL: [[OBJCBOOL_TO_BOOL:%.+]] = function_ref @_TF10ObjectiveC22_convertObjCBoolToBoolFVS_8ObjCBoolSb
  // CHECK-OBJCBOOL: = apply [[OBJCBOOL_TO_BOOL]]([[RESULT_2]])
  _ = testObjCBool(true)

  // CHECK: [[FUNC_3:%.+]] = function_ref @testDarwinBoolean
  // CHECK: [[BOOL_TO_DARWIN:%.+]] = function_ref @_TF6Darwin27_convertBoolToDarwinBooleanFSbVS_13DarwinBoolean
  // CHECK: [[INPUT_3:%.+]] = apply [[BOOL_TO_DARWIN]]({{%.+}})
  // CHECK: [[RESULT_3:%.+]] = apply [[FUNC_3]]({{%.+}}) : $@convention(c) (DarwinBoolean) -> DarwinBoolean
  // CHECK: [[DARWIN_TO_BOOL:%.+]] = function_ref @_TF6Darwin27_convertDarwinBooleanToBoolFVS_13DarwinBooleanSb
  // CHECK: = apply [[DARWIN_TO_BOOL]]([[RESULT_3]])
  _ = testDarwinBoolean(true)
}

// CHECK-LABEL: sil @_TF18objc_bool_bridging11testMethodsFCSo4TestT_
public func testMethods(x: Test) {
  // CHECK-NOT: convert
  // CHECK: [[METHOD_1:%.+]] = objc_method %0 : $Test, #Test.testCBool!1.foreign
  // CHECK-NOT: convert
  // CHECK: = apply [[METHOD_1]]({{%.+}}, %0) : $@convention(objc_method) (Bool, Test) -> Bool
  // CHECK-NOT: convert
  _ = x.testCBool(true)

  // CHECK-OBJCBOOL: [[METHOD_2:%.+]] = objc_method %0 : $Test, #Test.testObjCBool!1.foreign
  // CHECK-OBJCBOOL: [[BOOL_TO_OBJCBOOL:%.+]] = function_ref @_TF10ObjectiveC22_convertBoolToObjCBoolFSbVS_8ObjCBool
  // CHECK-OBJCBOOL: [[INPUT_2:%.+]] = apply [[BOOL_TO_OBJCBOOL]]({{%.+}})
  // CHECK-OBJCBOOL: [[RESULT_2:%.+]] = apply [[METHOD_2]]([[INPUT_2]], %0) : $@convention(objc_method) (ObjCBool, Test) -> ObjCBool
  // CHECK-OBJCBOOL: [[OBJCBOOL_TO_BOOL:%.+]] = function_ref @_TF10ObjectiveC22_convertObjCBoolToBoolFVS_8ObjCBoolSb
  // CHECK-OBJCBOOL: = apply [[OBJCBOOL_TO_BOOL]]([[RESULT_2]])
  _ = x.testObjCBool(true)

  // CHECK: [[METHOD_3:%.+]] = objc_method %0 : $Test, #Test.testDarwinBoolean!1.foreign
  // CHECK: [[BOOL_TO_DARWIN:%.+]] = function_ref @_TF6Darwin27_convertBoolToDarwinBooleanFSbVS_13DarwinBoolean
  // CHECK: [[INPUT_3:%.+]] = apply [[BOOL_TO_DARWIN]]({{%.+}})
  // CHECK: [[RESULT_3:%.+]] = apply [[METHOD_3]]([[INPUT_3]], %0) : $@convention(objc_method) (DarwinBoolean, Test) -> DarwinBoolean
  // CHECK: [[DARWIN_TO_BOOL:%.+]] = function_ref @_TF6Darwin27_convertDarwinBooleanToBoolFVS_13DarwinBooleanSb
  // CHECK: = apply [[DARWIN_TO_BOOL]]([[RESULT_3]])
  _ = x.testDarwinBoolean(true)
}

// CHECK-LABEL: sil @_TF18objc_bool_bridging14testPropertiesFCSo4TestT_
public func testProperties(x: Test) {
  // CHECK-NOT: convert
  // CHECK: [[SETTER_1:%.+]] = objc_method %0 : $Test, #Test.propCBool!setter.1.foreign
  // CHECK-NOT: convert
  // CHECK: apply [[SETTER_1]]({{%.+}}, %0) : $@convention(objc_method) (Bool, Test) -> ()
  x.propCBool = true

  // CHECK-OBJCBOOL: [[SETTER_2:%.+]] = objc_method %0 : $Test, #Test.propObjCBool!setter.1.foreign
  // CHECK-OBJCBOOL: [[BOOL_TO_OBJCBOOL:%.+]] = function_ref @_TF10ObjectiveC22_convertBoolToObjCBoolFSbVS_8ObjCBool
  // CHECK-OBJCBOOL: [[INPUT_2:%.+]] = apply [[BOOL_TO_OBJCBOOL]]({{%.+}})
  // CHECK-OBJCBOOL: apply [[SETTER_2]]([[INPUT_2]], %0) : $@convention(objc_method) (ObjCBool, Test) -> ()
  x.propObjCBool = true

  // CHECK: [[SETTER_3:%.+]] = objc_method %0 : $Test, #Test.propDarwinBoolean!setter.1.foreign
  // CHECK: [[BOOL_TO_DARWIN:%.+]] = function_ref @_TF6Darwin27_convertBoolToDarwinBooleanFSbVS_13DarwinBoolean
  // CHECK: [[INPUT_3:%.+]] = apply [[BOOL_TO_DARWIN]]({{%.+}})
  // CHECK: apply [[SETTER_3]]([[INPUT_3]], %0) : $@convention(objc_method) (DarwinBoolean, Test) -> ()
  x.propDarwinBoolean = true
}

// CHECK-LABEL: sil @_TF18objc_bool_bridging20testFunctionPointersFT_T_
public func testFunctionPointers() {
  // CHECK: = function_ref @_TToFF18objc_bool_bridging20testFunctionPointersFT_T_U_FSbSb : $@convention(c) (Bool) -> Bool
  let x: CBoolFn = { $0 }
  _ = x(true)
  
  // CHECK-OBJCBOOL: = function_ref @_TToFF18objc_bool_bridging20testFunctionPointersFT_T_U0_FV10ObjectiveC8ObjCBoolS1_ : $@convention(c) (ObjCBool) -> ObjCBool
  let y: ObjCBoolFn = { $0 }
  _ = y(true)
  
  // CHECK: = function_ref @_TToFF18objc_bool_bridging20testFunctionPointersFT_T_U1_FV6Darwin13DarwinBooleanS1_ : $@convention(c) (DarwinBoolean) -> DarwinBoolean
  let z: DarwinBooleanFn = { $0 }
  _ = z(true)
}

// CHECK-LABEL: sil shared @_TFF18objc_bool_bridging20testFunctionPointersFT_T_U_FSbSb
// CHECK: return %0 : $Bool

// CHECK-LABEL: sil shared @_TToFF18objc_bool_bridging20testFunctionPointersFT_T_U_FSbSb
// CHECK: [[NATIVE:%.+]] = function_ref @_TFF18objc_bool_bridging20testFunctionPointersFT_T_U_FSbSb
// CHECK: [[RESULT:%.+]] = apply [[NATIVE]](%0) : $@convention(thin) (Bool) -> Bool
// CHECK: return [[RESULT]] : $Bool

// CHECK-OBJCBOOL-LABEL: sil shared @_TFF18objc_bool_bridging20testFunctionPointersFT_T_U0_FV10ObjectiveC8ObjCBoolS1_
// CHECK-OBJCBOOL: return %0 : $ObjCBool

// CHECK-OBJCBOOL-LABEL: sil shared @_TToFF18objc_bool_bridging20testFunctionPointersFT_T_U0_FV10ObjectiveC8ObjCBoolS1_
// CHECK-OBJCBOOL: [[NATIVE:%.+]] = function_ref @_TFF18objc_bool_bridging20testFunctionPointersFT_T_U0_FV10ObjectiveC8ObjCBoolS1_
// CHECK-OBJCBOOL: [[RESULT:%.+]] = apply [[NATIVE]](%0) : $@convention(thin) (ObjCBool) -> ObjCBool
// CHECK-OBJCBOOL: return [[RESULT]] : $ObjCBool

// CHECK-LABEL: sil shared @_TFF18objc_bool_bridging20testFunctionPointersFT_T_U1_FV6Darwin13DarwinBooleanS1_
// CHECK: return %0 : $DarwinBoolean

// CHECK-LABEL: sil shared @_TToFF18objc_bool_bridging20testFunctionPointersFT_T_U1_FV6Darwin13DarwinBooleanS1_
// CHECK: [[NATIVE:%.+]] = function_ref @_TFF18objc_bool_bridging20testFunctionPointersFT_T_U1_FV6Darwin13DarwinBooleanS1_
// CHECK: [[RESULT:%.+]] = apply [[NATIVE]](%0) : $@convention(thin) (DarwinBoolean) -> DarwinBoolean
// CHECK: return [[RESULT]] : $DarwinBoolean

// CHECK-LABEL: sil @_TF18objc_bool_bridging10testBlocksFT_T_
public func testBlocks() {
  // CHECK: = function_ref @_TFF18objc_bool_bridging10testBlocksFT_T_U_FSbSb : $@convention(thin) (Bool) -> Bool
  let x: @convention(block) (Bool) -> Bool = { $0 }
  _ = x(true)
  
  // CHECK-OBJCBOOL: = function_ref @_TFF18objc_bool_bridging10testBlocksFT_T_U0_FV10ObjectiveC8ObjCBoolS1_ : $@convention(thin) (ObjCBool) -> ObjCBool
  let y: @convention(block) (ObjCBool) -> ObjCBool = { $0 }
  _ = y(true)
  
  // CHECK: = function_ref @_TFF18objc_bool_bridging10testBlocksFT_T_U1_FV6Darwin13DarwinBooleanS1_ : $@convention(thin) (DarwinBoolean) -> DarwinBoolean
  let z: @convention(block) (DarwinBoolean) -> DarwinBoolean = { $0 }
  _ = z(true)
}

// CHECK-LABEL: sil @_TF18objc_bool_bridging14testBlockPropsFCSo4TestT_
public func testBlockProps(x: Test) {
  // CHECK: = function_ref @_TFF18objc_bool_bridging14testBlockPropsFCSo4TestT_U_FSbSb : $@convention(thin) (Bool) -> Bool
  // CHECK: [[SETTER_1:%.+]] = objc_method %0 : $Test, #Test.propCBoolBlock!setter.1.foreign
  // CHECK: = apply [[SETTER_1]]({{%.+}}, %0) : $@convention(objc_method) (@convention(block) (Bool) -> Bool, Test) -> ()
  x.propCBoolBlock = { $0 }
  
  // CHECK-OBJCBOOL: = function_ref @_TFF18objc_bool_bridging14testBlockPropsFCSo4TestT_U0_FSbSb : $@convention(thin) (Bool) -> Bool
  // CHECK-OBJCBOOL: [[SETTER_2:%.+]] = objc_method %0 : $Test, #Test.propObjCBoolBlock!setter.1.foreign
  // CHECK-OBJCBOOL: = apply [[SETTER_2]]({{%.+}}, %0) : $@convention(objc_method) (@convention(block) (ObjCBool) -> ObjCBool, Test) -> ()
  x.propObjCBoolBlock = { $0 }
  
  // CHECK: = function_ref @_TFF18objc_bool_bridging14testBlockPropsFCSo4TestT_U1_FSbSb : $@convention(thin) (Bool) -> Bool
  // CHECK: [[SETTER_3:%.+]] = objc_method %0 : $Test, #Test.propDarwinBooleanBlock!setter.1.foreign
  // CHECK: = apply [[SETTER_3]]({{%.+}}, %0) : $@convention(objc_method) (@convention(block) (DarwinBoolean) -> DarwinBoolean, Test) -> ()
  x.propDarwinBooleanBlock = { $0 }
}

// CHECK-LABEL: sil @_TF18objc_bool_bridging26testFunctionPointerMethodsFCSo4TestT_
public func testFunctionPointerMethods(x: Test) {
  // CHECK: [[METHOD_1:%.+]] = objc_method %0 : $Test, #Test.testCBoolFnToBlock!1.foreign
  // CHECK: [[CLOSURE_1:%.+]] = function_ref @_TToFF18objc_bool_bridging26testFunctionPointerMethodsFCSo4TestT_U_FSbSb : $@convention(c) (Bool) -> Bool
  // CHECK: [[RESULT_1:%.+]] = apply [[METHOD_1]]([[CLOSURE_1]], %0)
  // CHECK: function_ref @_TTRXFdCb_dSb_dSb_XFo_dSb_dSb_
  _ = x.testCBoolFnToBlock { $0 }
  
  // CHECK-OBJCBOOL: [[METHOD_2:%.+]] = objc_method %0 : $Test, #Test.testObjCBoolFnToBlock!1.foreign
  // CHECK-OBJCBOOL: [[CLOSURE_2:%.+]] = function_ref @_TToFF18objc_bool_bridging26testFunctionPointerMethodsFCSo4TestT_U0_FV10ObjectiveC8ObjCBoolS2_ : $@convention(c) (ObjCBool) -> ObjCBool
  // CHECK-OBJCBOOL: [[RESULT_2:%.+]] = apply [[METHOD_2]]([[CLOSURE_2]], %0)
  // CHECK-OBJCBOOL: function_ref @_TTRXFdCb_dV10ObjectiveC8ObjCBool_dS0__XFo_dSb_dSb_
  _ = x.testObjCBoolFnToBlock { $0 }

  // CHECK: [[METHOD_3:%.+]] = objc_method %0 : $Test, #Test.testDarwinBooleanFnToBlock!1.foreign
  // CHECK: [[CLOSURE_3:%.+]] = function_ref @_TToFF18objc_bool_bridging26testFunctionPointerMethodsFCSo4TestT_U1_FV6Darwin13DarwinBooleanS2_ : $@convention(c) (DarwinBoolean) -> DarwinBoolean
  // CHECK: [[RESULT_3:%.+]] = apply [[METHOD_3]]([[CLOSURE_3]], %0)
  // CHECK: function_ref @_TTRXFdCb_dV6Darwin13DarwinBoolean_dS0__XFo_dSb_dSb_
  _ = x.testDarwinBooleanFnToBlock { $0 }
}

class NewClass : NSObject {
  // CHECK-LABEL: sil hidden @_TFC18objc_bool_bridging8NewClass12takesClosure
  // CHECK-OBJCBOOL-LABEL: sil hidden [thunk] @_TToFC18objc_bool_bridging8NewClass12takesClosure
  func takesClosure(_: (Bool) -> Bool) {}

  // CHECK-LABEL: sil hidden @_TFC18objc_bool_bridging8NewClass15takesCBoolBlock
  // CHECK-LABEL: sil hidden [thunk] @_TToFC18objc_bool_bridging8NewClass15takesCBoolBlock
  func takesCBoolBlock(_: @convention(block) (Bool) -> Bool) {}

  // CHECK-OBJCBOOL-LABEL: sil hidden @_TFC18objc_bool_bridging8NewClass18takesObjCBoolBlock
  // CHECK-OBJCBOOL-LABEL: sil hidden [thunk] @_TToFC18objc_bool_bridging8NewClass18takesObjCBoolBlock
  func takesObjCBoolBlock(_: @convention(block) (ObjCBool) -> ObjCBool) {}

  // CHECK-LABEL: sil hidden @_TFC18objc_bool_bridging8NewClass23takesDarwinBooleanBlock
  // CHECK-LABEL: sil hidden [thunk] @_TToFC18objc_bool_bridging8NewClass23takesDarwinBooleanBlock
  func takesDarwinBooleanBlock(_: @convention(block) (DarwinBoolean) -> DarwinBoolean) {}
}
