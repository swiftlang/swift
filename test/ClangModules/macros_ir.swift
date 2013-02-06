// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -emit-llvm -o - %s | FileCheck %s
// RUN: %swift -constraint-checker -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -emit-llvm -o - %s | FileCheck %s
import macros

// FIXME: Decide the type macros should map to.

// CHECK: define double @_T9macros_ir11circle_areaFT6radiusSd_Sd
func circle_area(radius:CDouble) -> CDouble {
  // CHECK: call double @_T4M_PISdg
  return M_PI * radius * radius
}

// CHECK: define linkonce_odr hidden double @_T4M_PISdg
// CHECK: store double 0x400921FB54442D11, double*
// CHECK: ret double

