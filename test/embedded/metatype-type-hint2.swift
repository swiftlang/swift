// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -o %t/Main.o -I %t %t/Main.swift -enable-experimental-feature Embedded -parse-as-library

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public struct MyStruct<T> {
  var x: Int
}

extension MyStruct {
  public static func makeStruct(of: T.Type) -> MyStruct<T> {
    return MyStruct<T>()
  }

  public init(_: T.Type = T.self) {
    self.x = 42
  }
}

// BEGIN Main.swift

import MyModule

@main
struct Main {
  static func main() {
    _ = MyStruct.makeStruct(of: String.self)
  }
}
