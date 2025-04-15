// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -mergeable-symbols -num-threads 2 -O -c -emit-module -o %t/MyModule.o             %t/MyModule.swift                   -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -mergeable-symbols -num-threads 2 -O -c              -o %t/MainA.o -o %t/MainB.o  %t/MainA.swift %t/MainB.swift -I %t -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-embedded-link %t/MainA.o %t/MainB.o %t/MyModule.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

var dict: [Int:Int] = [1: 2, 3: 4]

public func module_func() {
  print("module_func: \(dict[1]!)")
}

// BEGIN MainA.swift

import MyModule

fileprivate var dict: [Int:Int] = [5: 6, 7: 8]

func mainA_func() {
  print("main_func: \(dict[5]!)")
}

// BEGIN MainB.swift

import MyModule

fileprivate var dict: [Int:Int] = [5: 6, 7: 8]

func mainB_func() {
  print("main_func: \(dict[5]!)")
}

@main
struct Main {
  static func main() {
    module_func()
    mainA_func()
    mainB_func()
  }
}

// CHECK: module_func: 2
// CHECK: main_func: 6
