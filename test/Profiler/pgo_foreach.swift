// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_foreach -o %t/main

// This unusual use of 'sh' allows the path of the profraw file to be
// substituted by %target-run.
// RUN: %target-run sh -c 'env LLVM_PROFILE_FILE=$1 $2' -- %t/default.profraw %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// need to move counts attached to expr for this
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_foreach -o - | %FileCheck %s --check-prefix=SIL
// need to lower switch_enum(addr) into IR for this
// %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-ir -module-name pgo_foreach -o - | %FileCheck %s --check-prefix=IR
// need to check Opt support
// %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-sorted-sil -emit-sil -module-name pgo_foreach -o - | %FileCheck %s --check-prefix=SIL-OPT
// need to lower switch_enum(addr) into IR for this
// %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-ir -module-name pgo_foreach -o - | %FileCheck %s --check-prefix=IR-OPT

// REQUIRES: profile_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx

// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

// SIL-LABEL: // pgo_foreach.guessForEach1
// SIL-LABEL: sil @$s11pgo_foreach13guessForEach11xs5Int32VAE_tF : $@convention(thin) (Int32) -> Int32 !function_entry_count(42) {
// IR-LABEL: define swiftcc i32 @$s9pgo_foreach10guessWhiles5Int32VAD1x_tF
// IR-OPT-LABEL: define swiftcc i32 @$s9pgo_foreach10guessWhiles5Int32VAD1x_tF

public func guessForEach1(x: Int32) -> Int32 {
  // SIL: switch_enum {{.*}} : $Optional<Int32>, case #Optional.some!enumelt.1: {{.*}} !case_count(798), case #Optional.none!enumelt: {{.*}} !case_count(42)

  var ret : Int32 = 0
  for currVal in stride(from: 5, to: x, by: 5) {
    ret += currVal
  }
  return ret
}

// SIL-LABEL: // pgo_foreach.guessForEach2
// SIL-LABEL: sil @$s11pgo_foreach13guessForEach21xs5Int32VAE_tF : $@convention(thin) (Int32) -> Int32 !function_entry_count(42) {
// IR-LABEL: define swiftcc i32 @$s9pgo_foreach10guessWhiles5Int32VAD1x_tF
// IR-OPT-LABEL: define swiftcc i32 @$s9pgo_foreach10guessWhiles5Int32VAD1x_tF

public func guessForEach2(x: Int32) -> Int32 {
  // SIL: switch_enum {{.*}} : $Optional<(String, Int32)>, case #Optional.some!enumelt.1: {{.*}} !case_count(168), case #Optional.none!enumelt: {{.*}} !case_count(42)

  var ret : Int32 = 0
  let names = ["John" : Int32(1), "Paul" : Int32(2), "George" : Int32(3), "Ringo" : Int32(x)]
  for (name, number) in names {
    ret += Int32(name.count)
    ret += number
  }
  return ret
}

// SIL-LABEL: // pgo_foreach.main()
// IR-LABEL: define swiftcc i32 @$s9pgo_foreach10guessWhiles5Int32VAD1x_tF
// IR-OPT-LABEL: define swiftcc i32 @$s9pgo_foreach10guessWhiles5Int32VAD1x_tF

func main() {
  // SIL: switch_enum {{.*}} : $Optional<Int>, case #Optional.some!enumelt.1: {{.*}} !case_count(42), case #Optional.none!enumelt: {{.*}} !case_count(1)
  var guesses : Int32 = 0;

  for _ in 1...42 {
    guesses += guessForEach1(x: 100)
    guesses += guessForEach2(x: 100)
  }
}

main()

// IR: !{!"branch_weights", i32 421, i32 43}
// IR: !{!"branch_weights", i32 176401, i32 421}
// IR-OPT: !{!"branch_weights", i32 421, i32 43}
// IR-OPT: !{!"branch_weights", i32 176401, i32 421}
