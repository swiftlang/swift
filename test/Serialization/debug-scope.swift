// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_debug.swift -experimental-serialize-debug-info -O -g
// RUN: llvm-bcanalyzer %t/def_debug.swiftmodule | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -module-name debug -g -emit-sil -I %t %s -O | %FileCheck %s

import def_debug

// SIL-NOT: UnknownCode

// CHECK: sil_scope [[SCOPE_ONE:[0-9]+]] { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":2:13 parent @$s9def_debug3foo1xs6UInt64VAE_tF : $@convention(thin) (UInt64) -> UInt64 inlined_at [[INLINE_SITE_ONE:[0-9]+]] }
// CHECK: sil_scope [[SCOPE_TWO:[0-9]+]] { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":3:5 parent [[SCOPE_ONE]] inlined_at [[INLINE_SITE_ONE]] }
// CHECK: sil_scope {{[0-9]+}} { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":3:18 parent [[SCOPE_TWO]] inlined_at [[INLINE_SITE_ONE]] }

let simpleFunc = foo(x: 1)

// CHECK: sil_scope [[SCOPE_GENERIC_ONE:[0-9]+]] { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":19:13 parent @$s9def_debug25specializedGenericInlinedSiyF : $@convention(thin) () -> Int inlined_at [[INLINE_SITE_TWO:[0-9]+]] }
// CHECK: sil_scope [[SCOPE_GENERIC_TWO:[0-9]+]] { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":20:9 parent [[SCOPE_GENERIC_ONE]] inlined_at [[INLINE_SITE_TWO]] }
// CHECK: sil_scope {{[0-9]+}} { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":20:18 parent [[SCOPE_GENERIC_ONE]] inlined_at [[INLINE_SITE_TWO]] }
// CHECK: sil_scope [[SCOPE_GENERIC_THREE:[0-9]+]] { loc "SOURCE_DIR/test/Serialization/Inputs/def_debug.swift":21:12 parent [[SCOPE_GENERIC_TWO]] inlined_at [[INLINE_SITE_TWO]] }
// CHECK: sil_scope [[SCOPE_GENERIC_FOUR:[0-9]+]] { loc "SOURCE_DIR/stdlib/public/core/Hasher.swift":{{[0-9]+}}:{{[0-9]+}} parent @$ss6HasherV7combineyyxSHRzlFSi_Tgq5 : $@convention(method) (Int, @inout Hasher) -> () inlined_at [[SCOPE_GENERIC_THREE]] }
// CHECK: sil_scope [[SCOPE_GENERIC_FIVE:[0-9]+]] {  parent [[SCOPE_GENERIC_FOUR]] inlined_at [[SCOPE_GENERIC_THREE]] }
// CHECK: sil_scope [[SCOPE_GENERIC_SIX:[0-9]+]] { loc "BUILD_DIR/stdlib/public/core/8/IntegerTypes.swift":[[ROW_GENERIC:[0-9]+]]:[[COL_GENERIC:[0-9]+]] parent @$sSiSHsSH4hash4intoys6HasherVz_tFTW : $@convention(witness_method: Hashable) (@inout Hasher, @in_guaranteed Int) -> () inlined_at [[SCOPE_GENERIC_FIVE]] }
// CHECK: sil_scope [[SCOPE_GENERIC_SEVEN:[0-9]+]] { loc "BUILD_DIR/stdlib/public/core/8/IntegerTypes.swift":[[ROW_GENERIC]]:[[COL_GENERIC]] parent [[SCOPE_GENERIC_SIX]] inlined_at [[SCOPE_GENERIC_FIVE]] }
// CHECK: sil_scope {{[0-9]+}} { loc "BUILD_DIR/stdlib/public/core/8/IntegerTypes.swift":[[ROW_GENERIC]]:[[COL_GENERIC]] parent @$sSi4hash4intoys6HasherVz_tF : $@convention(method) (@inout Hasher, Int) -> () inlined_at [[SCOPE_GENERIC_SEVEN]] }

var funcReferringInlinedGeneric = specializedGenericInlined()
