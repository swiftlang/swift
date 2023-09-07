// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// both modules are embedded - ok
// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -parse-stdlib -enable-experimental-feature Embedded

// MyModule is not embedded - error
// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib
// RUN: not %target-swift-frontend -emit-ir -I %t %t/Main.swift -parse-stdlib -enable-experimental-feature Embedded 2>&1 | %FileCheck %s --check-prefix CHECK-A

// main module is not embedded - error
// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib -enable-experimental-feature Embedded
// RUN: not %target-swift-frontend -emit-ir -I %t %t/Main.swift -parse-stdlib 2>&1 | %FileCheck %s --check-prefix CHECK-B

// BEGIN MyModule.swift

public func foo() { }

// BEGIN Main.swift

import MyModule

public func main() {
  foo()
}

// CHECK-A: error: module 'MyModule' cannot be imported in embedded Swift mode
// CHECK-B: error: module 'MyModule' cannot be imported because it was built with embedded Swift
