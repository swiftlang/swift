// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -mergeable-symbols -num-threads 2 -O -c -emit-module -o %t/MyModule.o             %t/MyModule.swift                   -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -mergeable-symbols -num-threads 2 -O -c              -o %t/MainA.o -o %t/MainB.o  %t/MainA.swift %t/MainB.swift -I %t -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-clang %target-clang-resource-dir-opt %t/MainA.o %t/MainB.o %t/MyModule.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public func module_func(n: Int) {
  var a: [Int] = [1, 2, 3]
  print("module_func: \(a[0])")
}

// BEGIN MainA.swift

import MyModule

// BEGIN MainB.swift

import MyModule

@main
struct Main {
  static func main() {
    module_func(n: 5)
  }
}

// CHECK: module_func: 1
