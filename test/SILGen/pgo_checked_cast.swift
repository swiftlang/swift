// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_checked_cast -o %t/main
// RUN: env LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main
// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_checked_cast -o - | %FileCheck %s --check-prefix=SIL
// need to lower checked_cast_addr_br(addr) into IR for this
// %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-ir -module-name pgo_checked_cast -o - | %FileCheck %s --check-prefix=IR
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-sorted-sil -emit-sil -module-name pgo_checked_cast -o - | %FileCheck %s --check-prefix=SIL-OPT
// need to lower checked_cast_addr_br(addr) into IR for this
// %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-ir -module-name pgo_checked_cast -o - | %FileCheck %s --check-prefix=IR-OPT

// REQUIRES: profile_runtime
// REQUIRES: OS=macosx

// SIL-LABEL: // pgo_checked_cast.check1<A>(Any, A) -> A
// SIL-LABEL: sil @_T016pgo_checked_cast6check1xyp_xtlF : $@convention(thin) <T> (@in Any, @in T) -> @out T !function_entry_count(5001) {
// IR-LABEL: define swiftcc i32 @_T06pgo_checked_cast6guess1s5Int32VAD1x_tF
// IR-OPT-LABEL: define swiftcc i32 @_T06pgo_checked_cast6guess1s5Int32VAD1x_tF
public func check1<T>(_ a : Any, _ t : T) -> T {
  // SIL: checked_cast_addr_br take_always Any in {{.*}} : $*Any to T in {{.*}} : $*T, {{.*}}, {{.*}} !true_count(5000) !false_count(1)
  // SIL-OPT: checked_cast_addr_br take_always Any in {{.*}} : $*Any to T in {{.*}} : $*T, {{.*}}, {{.*}} !true_count(5000) !false_count(1)
  if let x = a as? T {
    return x
  } else {
    return t
  }
}

func main() {
  let answer : Int32 = 42
  var sum : Int32 = 0

  sum += check1("The answer to the life, the universe, and everything", answer)

  for i : Int32 in 1...5000 {
    sum += check1(i, answer)
  }
}

main()

// IR: !{!"branch_weights", i32 5001, i32 2}
// IR-OPT: !{!"branch_weights", i32 5001, i32 2}
