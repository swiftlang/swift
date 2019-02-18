
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Xfrontend -enable-sil-ownership -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_switchenum -o %t/main

// This unusual use of 'sh' allows the path of the profraw file to be
// substituted by %target-run.
// RUN: %target-run sh -c 'env LLVM_PROFILE_FILE=$1 $2' -- %t/default.profraw %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// need to move counts attached to expr for this
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -enable-sil-ownership -emit-sorted-sil -emit-sil -module-name pgo_switchenum -o - | %FileCheck %s --check-prefix=SIL
// need to lower switch_enum(addr) into IR for this
// %target-swift-frontend %s -enable-sil-ownership -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-ir -module-name pgo_switchenum -o - | %FileCheck %s --check-prefix=IR
// need to check Opt support
// %target-swift-frontend %s -enable-sil-ownership -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-sorted-sil -emit-sil -module-name pgo_switchenum -o - | %FileCheck %s --check-prefix=SIL-OPT
// need to lower switch_enum(addr) into IR for this
// %target-swift-frontend -enable-sil-ownership %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-ir -module-name pgo_switchenum -o - | %FileCheck %s --check-prefix=IR-OPT

// REQUIRES: profile_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx

public enum MaybePair {
  case Neither
  case Left(Int32)
  case Right(String)
  case Both(Int32, String)
}

// SIL-LABEL: // pgo_switchenum.guess1
// SIL-LABEL: sil @$s14pgo_switchenum6guess11xs5Int32VAA9MaybePairO_tF : $@convention(thin) (@guaranteed MaybePair) -> Int32 !function_entry_count(5011) {
// IR-LABEL: define swiftcc i32 @$s9pgo_switchenum6guess1s5Int32VAD1x_tF
// IR-OPT-LABEL: define swiftcc i32 @$s9pgo_switchenum6guess1s5Int32VAD1x_tF

public func guess1(x: MaybePair) -> Int32 {
  // SIL: switch_enum {{.*}} : $MaybePair, case #MaybePair.Neither!enumelt: {{.*}} !case_count(2), case #MaybePair.Left!enumelt.1: {{.*}} !case_count(5001), case #MaybePair.Right!enumelt.1: {{.*}} !case_count(3), case #MaybePair.Both!enumelt.1: {{.*}} !case_count(5)
  // SIL-OPT: switch_enum {{.*}} : $MaybePair, case #MaybePair.Neither!enumelt: {{.*}} !case_count(2), case #MaybePair.Left!enumelt.1: {{.*}} !case_count(5001), case #MaybePair.Right!enumelt.1: {{.*}} !case_count(3), case #MaybePair.Both!enumelt.1: {{.*}} !case_count(5)
  switch x {
  case .Neither:
    return 1
  case let .Left(val):
    return val*2
  case let .Right(val):
    return Int32(val.count)
  case let .Both(valNum, valStr):
    return valNum + Int32(valStr.count)
  }
}

// SIL-LABEL: // pgo_switchenum.guess2
// SIL-LABEL: sil @$s14pgo_switchenum6guess21xs5Int32VAA9MaybePairO_tF : $@convention(thin) (@guaranteed MaybePair) -> Int32 !function_entry_count(5011) {
public func guess2(x: MaybePair) -> Int32 {
  // SIL: switch_enum {{.*}} : $MaybePair, case #MaybePair.Neither!enumelt: {{.*}} !case_count(2), case #MaybePair.Left!enumelt.1: {{.*}} !case_count(5001), default {{.*}} !default_count(8)
  // SIL-OPT: switch_enum {{.*}} : $MaybePair, case #MaybePair.Neither!enumelt: {{.*}} !case_count(2), case #MaybePair.Left!enumelt.1: {{.*}} !case_count(5001), default {{.*}} !default_count(8)
  switch x {
  case .Neither:
    return 1
  case let .Left(val):
    return val*2
  default:
    return 42;
  }
}


func main() {
  var guesses : Int32 = 0;

  guesses += guess1(x: MaybePair.Neither)
  guesses += guess1(x: MaybePair.Neither)
  guesses += guess1(x: MaybePair.Left(42))
  guesses += guess1(x: MaybePair.Right("The Answer"))
  guesses += guess1(x: MaybePair.Right("The Answer"))
  guesses += guess1(x: MaybePair.Right("The Answer"))
  guesses += guess1(x: MaybePair.Both(42, "The Answer"))
  guesses += guess1(x: MaybePair.Both(42, "The Answer"))
  guesses += guess1(x: MaybePair.Both(42, "The Answer"))
  guesses += guess1(x: MaybePair.Both(42, "The Answer"))
  guesses += guess1(x: MaybePair.Both(42, "The Answer"))

  for _ in 1...5000 {
    guesses += guess1(x: MaybePair.Left(10))
  }
  
  guesses += guess2(x: MaybePair.Neither)
  guesses += guess2(x: MaybePair.Neither)
  guesses += guess2(x: MaybePair.Left(42))
  guesses += guess2(x: MaybePair.Right("The Answer"))
  guesses += guess2(x: MaybePair.Right("The Answer"))
  guesses += guess2(x: MaybePair.Right("The Answer"))
  guesses += guess2(x: MaybePair.Both(42, "The Answer"))
  guesses += guess2(x: MaybePair.Both(42, "The Answer"))
  guesses += guess2(x: MaybePair.Both(42, "The Answer"))
  guesses += guess2(x: MaybePair.Both(42, "The Answer"))
  guesses += guess2(x: MaybePair.Both(42, "The Answer"))

  for _ in 1...5000 {
    guesses += guess2(x: MaybePair.Left(10))
  }
}

main()

// IR: !{!"branch_weights", i32 5001, i32 3}
// IR-OPT: !{!"branch_weights", i32 5001, i32 3}
