// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -parse-as-library -o %t/a.o
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang %t/a.o %t/print.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public protocol Foo: AnyObject {
  func run()
}

public class Hello: Foo {
  public init() {}
  public func run() {
    print("Hello from MyLibrary!")
  }
}

// BEGIN Main.swift

import MyModule

@main
struct MyApp {
  static func main() {
    let ex: Foo = Hello()
    print("Hello from main!")
    ex.run()
  }
}

// CHECK: Hello from main!
