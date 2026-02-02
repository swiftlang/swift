// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_pattern -o %t/main

// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main
// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata

// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_pattern -g | %FileCheck %s

// REQUIRES: profile_runtime
// REQUIRES: executable_test

// CHECK-LABEL: sil hidden @$s11pgo_pattern3foo1xys5Int32V_tF : $@convention(thin) (Int32) -> () !function_entry_count(10)
func foo(x: Int32) {
  if case 0 ..< 1 = x {} else {}
  // CHECK: cond_br {{.*}} !true_count(1) !false_count(9), loc {{.*}}:[[@LINE-1]]:3

  let y: Int32? = x < 2 ? x : nil
  // CHECK: cond_br {{.*}} !true_count(2) !false_count(8), loc {{.*}}:[[@LINE-1]]:21

  if let y {} else { _ = y }
  // CHECK: switch_enum {{.*}}, case #Optional.some!enumelt: {{.*}} !case_count(2), case #Optional.none!enumelt: {{.*}} !case_count(8), loc {{.*}}:[[@LINE-1]]:3

  let ty: Any = x < 3 ? "" : 0
  // CHECK: cond_br {{.*}} !true_count(3) !false_count(7), loc {{.*}}:[[@LINE-1]]:19

  if case is String = ty {} else {}
  // CHECK: switch_enum {{.*}}, case #Optional.some!enumelt: {{.*}} !case_count(3), case #Optional.none!enumelt: {{.*}} !case_count(7), loc {{.*}}:[[@LINE-1]]:3

  if case true = x < 4 {} else {}
  // CHECK: cond_br {{.*}} !true_count(4) !false_count(6), loc {{.*}}:[[@LINE-1]]:20

  enum E {
    case x, y
  }
  if case E.x = x < 6 ? .x : .y {} else {}
  // CHECK: cond_br {{.*}} !true_count(6) !false_count(4), loc {{.*}}:[[@LINE-1]]:19
  // CHECK: switch_enum {{.*}}, case #<abstract function>E.x!enumelt: {{.*}} !case_count(6), case #<abstract function>E.y!enumelt: {{.*}} !case_count(4), loc {{.*}}:[[@LINE-2]]:3

  let xs = [0, 1, 2, 3]
  var iter = xs.makeIterator()
  while let next = iter.next() { _ = next }
  // CHECK: switch_enum {{.*}}, case #Optional.some!enumelt: {{.*}} !case_count(40), case #Optional.none!enumelt: {{.*}} !case_count(10), loc {{.*}}:[[@LINE-1]]:25
}

for i: Int32 in 0 ..< 10 {
  foo(x: i)
}
