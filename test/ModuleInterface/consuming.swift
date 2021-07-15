// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/missing)
// RUN: %empty-directory(%t/inputs)
// RUN: %target-swift-frontend -emit-module-path %t/missing/Foo.swiftmodule -enable-library-evolution -emit-module-interface-path %t/inputs/Foo.swiftinterface -enable-objc-interop -disable-objc-attr-requires-foundation-module -module-name Foo %s
// RUN: %FileCheck --input-file %t/inputs/Foo.swiftinterface %s

// RUN: touch %t/Bar.swift
// RUN: echo "import Foo" > %t/Bar.swift
// RUN: echo "let f = Field()" >> %t/Bar.swift

// RUN: %target-swift-frontend -emit-module-path %t/Bar.swiftmodule -enable-library-evolution -enable-objc-interop -disable-objc-attr-requires-foundation-module -module-name Bar %t/Bar.swift -I %t/inputs


import Swift

public struct Field {
  public init() {}
  public var area: Int {
    __consuming get { return 1 }
    _modify {
      var a = 1
      yield &a
    }
  }
}

// CHECK: __consuming get
