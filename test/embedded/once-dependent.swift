// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public struct MyStructA {
  static var singleton = MyStructA()

  init() {
    print("MyStructA.init")
    _ = MyStructB.singleton
    print("MyStructA.init done")
  }
}

public struct MyStructB {
  static var singleton = MyStructB()

  init() {
    print("MyStructB.init")
    _ = MyStructC.singleton
    print("MyStructB.init done")
  }
}

public struct MyStructC {
  static var singleton = MyStructC()

  init() {
    print("MyStructC.init")
    print("MyStructC.init done")
  }
}

@main
struct Main {
  static func main() {
    print("Start")
    _ = MyStructA.singleton

    // CHECK: Start
    // CHECK-NEXT: MyStructA.init
    // CHECK-NEXT: MyStructB.init
    // CHECK-NEXT: MyStructC.init
    // CHECK-NEXT: MyStructC.init done
    // CHECK-NEXT: MyStructB.init done
    // CHECK-NEXT: MyStructA.init done
  }
}
