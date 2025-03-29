// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// both modules are embedded - ok
// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib -enable-experimental-feature Embedded -wmo
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -parse-stdlib -enable-experimental-feature Embedded -wmo

// MyModule is not embedded - error
// RUN: %target-swift-frontend -emit-module -enable-ossa-modules -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib -wmo
// RUN: not %target-swift-frontend -emit-ir -I %t %t/Main.swift -parse-stdlib -enable-experimental-feature Embedded -wmo 2>&1 | %FileCheck %s --check-prefix CHECK-A

// main module is not embedded - error
// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib -enable-experimental-feature Embedded -wmo
// RUN: not %target-swift-frontend -emit-ir -I %t %t/Main.swift -parse-stdlib -wmo 2>&1 | %FileCheck %s --check-prefix CHECK-B

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public func foo() { }

// BEGIN Main.swift

import MyModule

public func main() {
  foo()
}

// CHECK-A: error: module 'MyModule' cannot be imported in embedded Swift mode
// CHECK-B: error: module 'MyModule' cannot be imported because it was built with embedded Swift
