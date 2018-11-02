
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/default_arguments_other.swiftmodule -emit-module -swift-version 4 -primary-file %S/Inputs/default_arguments_other.swift

// RUN: %target-swift-frontend -module-name default_arguments_serialized -Xllvm -sil-full-demangle -enable-sil-ownership -emit-silgen -swift-version 3 -I %t %s | %FileCheck %s --check-prefix=SWIFT3 --check-prefix=CHECK
// RUN: %target-swift-frontend -module-name default_arguments_serialized -Xllvm -sil-full-demangle -enable-sil-ownership -emit-silgen -swift-version 4 -I %t %s | %FileCheck %s --check-prefix=SWIFT4 --check-prefix=CHECK

// RUN: %target-swift-frontend -module-name default_arguments_serialized -Xllvm -sil-full-demangle -enable-sil-ownership -emit-sil -O -swift-version 3 -I %t %s | %FileCheck %s --check-prefix=OPT
// RUN: %target-swift-frontend -module-name default_arguments_serialized -Xllvm -sil-full-demangle -enable-sil-ownership -emit-sil -O -swift-version 4 -I %t %s | %FileCheck %s --check-prefix=OPT

// Check that default arguments are serialized in Swift 4 mode.

import default_arguments_other

// CHECK-LABEL: sil @$S28default_arguments_serialized0A6StringSSyF : $@convention(thin) () -> @owned String
public func defaultString() -> String { return "hi" }

// SWIFT3-LABEL: sil @$S28default_arguments_serialized19hasDefaultArguments1x1yySi_SStFfA_ : $@convention(thin) () -> Int
// SWIFT4-LABEL: sil non_abi [serialized] @$S28default_arguments_serialized19hasDefaultArguments1x1yySi_SStFfA_ : $@convention(thin) () -> Int

// SWIFT3-LABEL: sil @$S28default_arguments_serialized19hasDefaultArguments1x1yySi_SStFfA0_ : $@convention(thin) () -> @owned String
// SWIFT4-LABEL: sil non_abi [serialized] @$S28default_arguments_serialized19hasDefaultArguments1x1yySi_SStFfA0_ : $@convention(thin) () -> @owned String

public func hasDefaultArguments(x: Int = 0, y: String = defaultString()) {}

// CHECK-LABEL: sil @$S28default_arguments_serialized21callsDefaultArgumentsyyF : $@convention(thin) () -> ()
// CHECK: function_ref @$S28default_arguments_serialized19hasDefaultArguments1x1yySi_SStFfA_ : $@convention(thin) () -> Int
// CHECK: function_ref @$S28default_arguments_serialized19hasDefaultArguments1x1yySi_SStFfA0_ : $@convention(thin) () -> @owned String
// CHECK: function_ref @$S28default_arguments_serialized19hasDefaultArguments1x1yySi_SStF : $@convention(thin) (Int, @guaranteed String) -> ()
// CHECK: apply
// CHECK: return
public func callsDefaultArguments() {
  hasDefaultArguments()
}

// When calling a default argument generator for a function in another module
// that was built in Swift 4 mode, we should always treat it as serialized,
// even if *this* module is built in Swift 3 mode.

// CHECK-LABEL: sil @$S28default_arguments_serialized26callsOtherDefaultArgumentsyyF : $@convention(thin) () -> ()
// CHECK: function_ref @$S23default_arguments_other0C16DefaultArguments1xySi_tFfA_ : $@convention(thin) () -> Int
// CHECK: function_ref @$S23default_arguments_other0C16DefaultArguments1xySi_tF : $@convention(thin) (Int) -> ()
// CHECK: apply
// CHECK: return

// Make sure the optimizer inlines the default argument generator from the
// other module.

// OPT-LABEL: sil @$S28default_arguments_serialized26callsOtherDefaultArgumentsyyF : $@convention(thin) () -> ()
// OPT: [[INT_VAL:%.*]] = integer_literal [[INT_TYPE:\$Builtin.Int(32|64)]], 0
// OPT: [[INT:%.*]] = struct $Int ([[INT_VAL]] : [[INT_TYPE]]
// OPT: [[FN:%.*]] = function_ref @$S23default_arguments_other0C16DefaultArguments1xySi_tF : $@convention(thin) (Int) -> ()
// OPT: apply [[FN]]([[INT]]) : $@convention(thin) (Int) -> ()
// OPT: return
public func callsOtherDefaultArguments() {
  otherDefaultArguments()
}

// CHECK-LABEL: sil hidden_external [serialized] @$S23default_arguments_other0C16DefaultArguments1xySi_tFfA_ : $@convention(thin) () -> Int

// CHECK-LABEL: sil @$S23default_arguments_other0C16DefaultArguments1xySi_tF : $@convention(thin) (Int) -> ()

