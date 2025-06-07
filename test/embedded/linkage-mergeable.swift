// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -mergeable-symbols -c -emit-module -o %t/MyModule.o %t/MyModule.swift   -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -mergeable-symbols -c              -o %t/a.o        %t/Main.swift -I %t -enable-experimental-feature Embedded
// RUN: %target-clang %t/a.o %t/MyModule.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public func module_func() {
  print("module_func")
}

public func module_generic_func<T: CustomStringConvertible>(t: T) {
  print("module_generic_func: \(t)")
}

// BEGIN Main.swift

import MyModule

func test() {
  module_func()
  module_generic_func(t: 42)
}

test()

// CHECK: module_func
// CHECK: module_generic_func: 42
