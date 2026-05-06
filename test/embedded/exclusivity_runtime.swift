// RUN: %empty-directory(%t)

// Single-threaded exclusivity checking implementation.

// Build without optimization.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -emit-ir -o %t/yesopt.ll -enforce-exclusivity=checked -enable-experimental-feature EmbeddedDynamicExclusivity
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o -enforce-exclusivity=checked -enable-experimental-feature EmbeddedDynamicExclusivity

// Ensure that the detailed printing strings are there in the resulting binary.
// RUN: %FileCheck -check-prefix YESOPT %s < %t/yesopt.ll

// Run without optimization.
// RUN: %target-clang %t/a.o %target-embedded-posix-shim -o %t/noopt.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip -lswiftExclusivitySingleThreaded
// RUN: %target-run not --crash %t/noopt.out

// Build with optimization.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -emit-ir -o %t/noopt.ll -enforce-exclusivity=checked -enable-experimental-feature EmbeddedDynamicExclusivity -O
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o -enforce-exclusivity=checked -enable-experimental-feature EmbeddedDynamicExclusivity -O

// Ensure that the detailed printing strings are not there in the resulting
// binary.
// RUN: %FileCheck -check-prefix NOOPT %s < %t/noopt.ll

// Run with optimization.
// RUN: %target-clang %t/a.o %target-embedded-posix-shim -o %t/opt.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip -lswiftExclusivitySingleThreaded
// RUN: %target-run not --crash %t/opt.out

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDynamicExclusivity

// UNSUPPORTED: OS=wasip1

struct NC: ~Copyable {
  var i: Int = 1

  mutating func add(_ other: borrowing Self) {
    i += other.i
    i += other.i
    print(self.i)
    print(other.i)
  }
}

class C1 {
  var nc = NC()

  @inline(never)
  func foo() {
    nc.add(nc)
  }
}

struct S {
  var i: Int = 1

  @inline(never)
  mutating func add(_ c: C2) {
    let other = c.getS()
    i += other.i
    i += other.i
    print(self.i)
    print(other.i)
  }
}

final class C2 {
  var s = S()

  @inline(never)
  func getS() -> S { s }

  @inline(never)
  func foo() {
    s.add(self)
  }
}

@main
struct Main {
  static func main() {
    // FIXME: These are actually disabled, because we don't flush standard
    // output before crashing.
    // CHECK: Simultaneous access to 0x{{.*}}, but modification requires exclusive access
    // CHECK: Previous access (a modify) started at 0x{{.*}}
    // CHECK: Current access (a read) started at 0x{{.*}}
    C1().foo()
  }
}

// YESOPT: [[SIMULT_ACCESS_STR:@.*]] = private unnamed_addr constant [26 x i8] c"Simultaneous access to 0x\00"
// YESOPT: define{{.*}}void @"$es35_embeddedReportExclusivityViolation9oldAction0E2PC03newF00hG07pointerys6AccessV0F0O_SVSgAjKSVtF"
// YESOPT-NOT: unreachable
// YESOPT: [[SIMULT_ACCESS_STR]]

// NOOOPT-NO: Simultaneous access to 0x
// NOOPT: define{{.*}}void @swift_beginAccess
// NOOPT: tail call swiftcc void [[TRAPFN:@.*]]()
// NOOPT: define{{.*}}void [[TRAPFN]]()
// NOOPT: call void @llvm.trap()
// NOOPT: unreachable
