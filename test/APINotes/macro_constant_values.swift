// RUN: %target-swift-frontend -I %S/Inputs/custom-modules -emit-sil -Xllvm -sil-disable-pass=Simplification -Xllvm -sil-print-types -Xllvm -sil-print-debuginfo %s | %FileCheck %s

import APINotesTest

// CHECK-LABEL: // typedMacro()
func typedMacro() -> APINotesUnsigned {
  // CHECK: integer_literal $Builtin.Int32, 42
  // CHECK: struct $UInt32
  APINOTES_TYPED_MACRO
}

// CHECK-LABEL: // typedMacroAlias()
func typedMacroAlias() -> APINotesUnsigned {
  // CHECK: integer_literal $Builtin.Int32, 314
  // CHECK: struct $UInt32
  APINOTES_TYPED_MACRO_ALIAS
}

// CHECK-LABEL: // renamedMacro()
func renamedMacro() -> CInt {
  // CHECK: integer_literal $Builtin.Int32, 17
  // CHECK: struct $Int32
  renamedMacroConstant
}

// CHECK-LABEL: // retypedRenamedMacro()
func retypedRenamedMacro() -> APINotesUnsigned {
  // CHECK: integer_literal $Builtin.Int32, 19
  // CHECK: struct $UInt32
  retypedRenamedMacroConstant
}
