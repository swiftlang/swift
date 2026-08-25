// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -O -c -parse-as-library %t/MyModule.swift -o %t/MyModule.o -emit-module -emit-module-path %t/MyModule.swiftmodule
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -O -c -I%t -parse-as-library %t/Main.swift -o %t/Main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Main.o %t/MyModule.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

//--- MyModule.swift

struct Match {
  let route: Int
}

public struct Tree<T> {
  public init() {}

  public func resolve() -> Int {
    let matches = [Match(route: 5), Match(route: 37)]
    let routes = matches.map(\.route)
    return routes[0] + routes[1]
  }
}

//--- Main.swift

import MyModule

@main
struct Main {
  static func main() {
    // CHECK: 42
    print(Tree<Int>().resolve())
  }
}
