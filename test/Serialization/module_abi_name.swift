// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_module_abi_name.swift -module-abi-name Goodbye
// RUN: %target-swift-frontend -module-name ModuleABIName -emit-sil -I %t %s | %FileCheck %s

import def_module_abi_name

func callFunction() {
  // CHECK: function_ref @$s7Goodbye8functionyyF
  def_module_abi_name.function()
}

// CHECK: sil @$s7Goodbye8functionyyF : $@convention(thin) () -> ()
