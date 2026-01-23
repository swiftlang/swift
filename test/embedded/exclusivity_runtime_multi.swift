// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o -enforce-exclusivity=checked -enable-experimental-feature EmbeddedDynamicExclusivity

// Multi-threaded exclusivity checking implementation (that uses C11 thread_local).
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip -lswiftExclusivityC11ThreadLocal
// RUN: %target-run %t/a.out 2>&1| %FileCheck %s

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

  func foo() {
    nc.add(nc)
  }
}

struct S {
  var i: Int = 1

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

  func foo() {
    s.add(self)
  }
}

@main
struct Main {
  static func main() {
    // CHECK: Simultaneous access to 0x{{.*}}, but modification requires exclusive access
    // CHECK: Previous access (a modify) started at 0x{{.*}}
    // CHECK: Current access (a read) started at 0x{{.*}}
    C1().foo()
  }
}
