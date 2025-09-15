// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -Xllvm -sil-print-types -emit-sil -module-name coverage_lazy %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// Test that the crash from https://github.com/apple/swift/issues/50953 is
// avoided, and that we generate the correct coverage.
class C {
  // CHECK-LABEL: sil hidden [lazy_getter] [noinline] @$s13coverage_lazy1CC6offsetSivg : $@convention(method) (@guaranteed C) -> Int
  // CHECK:       switch_enum {{%[0-9]+}} : $Optional<Int>, case #Optional.some!enumelt: {{bb[0-9]}}, case #Optional.none!enumelt: [[INITBB:bb[0-9]]]
  // CHECK:       [[INITBB]]
  // CHECK-NEXT:  increment_profiler_counter 2
  // CHECK:       function_ref @$sSb6randomSbyFZ : $@convention(method) (@thin Bool.Type) -> Bool
  // CHECK:       cond_br {{%[0-9]+}}, [[TRUEBB:bb[0-9]]], {{bb[0-9]}}
  // CHECK:       [[TRUEBB]]
  // CHECK-NEXT:  increment_profiler_counter 3

  // CHECK-LABEL: sil_coverage_map {{.*}} // coverage_lazy.C.offset.getter : Swift.Int
  // CHECK-NEXT: [[@LINE+4]]:38 -> [[@LINE+4]]:40 : 3
  // CHECK-NEXT: [[@LINE+3]]:43 -> [[@LINE+3]]:45 : (2 - 3)
  // CHECK-NEXT: [[@LINE+2]]:26 -> [[@LINE+2]]:45 : 2
  // CHECK-NEXT: }
  lazy var offset: Int = .random() ? 30 : 55
}
