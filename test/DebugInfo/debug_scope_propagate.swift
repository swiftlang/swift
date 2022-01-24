// RUN: %target-swift-frontend -primary-file %s -g -O -emit-sil | %FileCheck %s

public enum BenchmarkCategory {
  case validation
  case api, Array, String, Dictionary, Codable, Set, Data, IndexPath, SIMD
}

// Similar to `DebugInfo/verifier_debug_scope.sil`, in this case the cloner
// should correctly map the scope of debug variable.
// CHECK-LABEL: sil {{.*}} @{{.*}}__derived_enum_equalsySbAC_ACtFZTf4nnd_n
// CHECK: debug_value %{{.*}} : $Builtin.Int{{[0-9]+}}, var, (name "index_a"
// CHECK-SAME:                                        , scope [[VAR_SCOPE:[0-9]+]]),
// CHECK-SAME:                                        , scope [[VAR_SCOPE]]
