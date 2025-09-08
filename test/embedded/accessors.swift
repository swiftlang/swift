// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -Onone -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public class C {
  public var x: Int {
    _read {
      yield(y)
    }
    _modify {
      yield(&y)
    }
  }

  var y: Int = 27
}

@main
struct Main {
  static func main() {
    print("1") // CHECK: 1
    let c = C() // CHECK: 27
    print(c.y)
    c.y = 28
    print(c.y) // CHECK: 28
    print("")

    print("2") // CHECK: 2
    print("")
  }
}
