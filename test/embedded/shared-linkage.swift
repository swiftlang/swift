// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/Module.swiftmodule %t/Module.swift -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-ir %t/Main.swift -I%t -enable-experimental-feature Embedded

// RUN: %target-swift-frontend -O -emit-module -o %t/Module.swiftmodule %t/Module.swift -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -O -emit-ir %t/Main.swift -I%t -enable-experimental-feature Embedded

// RUN: %target-swift-frontend -Osize -emit-module -o %t/Module.swiftmodule %t/Module.swift -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -Osize -emit-ir %t/Main.swift -I%t -enable-experimental-feature Embedded

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// BEGIN Module.swift

public func bar<R>(count: Int, _ body: (UnsafeMutableBufferPointer<Int>) -> R) -> R {
  let pointer = UnsafeMutablePointer<Int>(bitPattern: 42)!
  let inoutBufferPointer = UnsafeMutableBufferPointer(start: pointer, count: count)
  return body(inoutBufferPointer)
}

public func foo<R>(_ body: () -> R) -> R {
  bar(count: 10) { p in
    body()
  }
}

public func module_func() {
  foo {
    return 0
  }
}

// BEGIN Main.swift

import Module

public func test() {
  module_func()
}

public func client_func() {
  foo {
    return 0
  }
}
