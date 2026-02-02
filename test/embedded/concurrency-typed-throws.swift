// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

enum MyError: Error {
  case badN(n: Int)
}

func foo(_ n: Int) async throws(MyError) -> Int {
  let t = Task {
    return n
  }

  if n >= 0 { return n }
  else { throw .badN(n: n) }
}

@main struct Main {
  static func main() async {
    do {
      let n = try await foo(10)
      print(n)
    } catch {
      print("FAIL")
    }
    // CHECK-NOT: FAIL
    // CHECK: 10

    do {
      let n = try await foo(-10)
      print("FAIL")
    } catch {
      guard case .badN(let n) = error else { print("FAIL") ; return }
      print("badN(\(n))")
    }
    // CHECK-NOT: FAIL
    // CHECK: badN(-10)

    // CHECK-NOT: FAIL
  }
}
