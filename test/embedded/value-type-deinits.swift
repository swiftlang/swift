// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -Osize -wmo -parse-as-library
// RUN: %target-swift-frontend -emit-sil -I %t %t/Main.swift -enable-experimental-feature Embedded -parse-as-library | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public func createFoo(x: some FixedWidthInteger) {
  let _ = Foo<UInt>(i: 0)
}

public struct Foo<Element> : ~Copyable {
  public let i: Int

  public init(i: Int) {
      self.i = i
  }

  deinit { }
}

// BEGIN Main.swift

import MyModule

public func test() {
  createFoo(x: 1)
}

// CHECK-LABEL: sil @$e8MyModule9createFoo1xyx_ts17FixedWidthIntegerRzlFSi_Ttg5 :
// CHECK-NOT:     release
// CHECK:       } // end sil function '$e8MyModule9createFoo1xyx_ts17FixedWidthIntegerRzlFSi_Ttg5'
