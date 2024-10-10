// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_debug.swift -experimental-serialize-debug-info
// RUN: llvm-bcanalyzer %t/def_debug.swiftmodule | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -module-name debug -g -emit-sil -I %t %s | %FileCheck %s

import def_debug

let _ = sum(x: 1)
// SIL-NOT: UnknownCode

// CHECK: sil_scope [[SCOPE_ONE:[0-9]+]] { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":2:13 parent @$s9def_debug3sum1xs6UInt64VAE_tF : $@convention(thin) (UInt64) -> UInt64 }
// CHECK: sil_scope [[SCOPE_TWO:[0-9]+]] { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":3:5 parent [[SCOPE_ONE]] }
// CHECK: sil_scope {{[0-9]+}} { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":3:18 parent [[SCOPE_TWO]] }
