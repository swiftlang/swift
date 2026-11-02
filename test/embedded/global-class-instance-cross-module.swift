// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -O -c -emit-module -o %t/MyModule.o %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -O -c -o %t/Main.o %t/Main.swift -I %t -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -O -emit-ir %t/Main.swift -I %t -enable-experimental-feature Embedded -parse-as-library | %FileCheck %s --check-prefix=CHECK-IR
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Main.o %t/MyModule.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

// A class instance in a global is not outlined before serialization. So a client, which specializes
// a function of the module, doesn't reference the module's object.
// CHECK-IR-NOT: counter_WZTv_

// BEGIN MyModule.swift

final class Counter {
  var value = 0
}

let counter = Counter()

public func increment<T>(_ t: T) -> Int {
  counter.value += 1
  return counter.value
}

// BEGIN Main.swift

import MyModule

@main
struct Main {
  static func main() {
    // CHECK: 1
    print(increment(1))
    // CHECK: 2
    print(increment(true))
  }
}
