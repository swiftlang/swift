// REQUIRES: executable_test
// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/clang-module-cache)
// RUN: %empty-directory(%t/Foo.swiftmodule)
// RUN: %empty-directory(%t/Bar.swiftmodule)
// RUN: split-file %s %t

// Step 1: build Bar swift interface and swift module side by side
// RUN: %target-swift-frontend -emit-module %t/Bar.swift -emit-module-path %t/Bar.swiftmodule/%target-swiftmodule-name -module-name Bar -emit-module-interface-path %t/Bar.swiftmodule/%target-swiftinterface-name -I %S/Inputs/CHeaders -I %S/Inputs/Swift

// Step 2: build Foo swift interface and swift module side by side belonging to package 'Test'
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -emit-module-path %t/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo -emit-module-interface-path %t/Foo.swiftmodule/%target-swiftinterface-name -I %S/Inputs/CHeaders -I %S/Inputs/Swift -I %t -package-name Test

// Step 3: scan dependencies with '-no-scanner-module-validation' and '-package-name Test'
// RUN: %target-swift-frontend -scan-dependencies %t/Client.swift -o %t/deps.json -no-scanner-module-validation -I %t -sdk %t -prebuilt-module-cache-path %t/clang-module-cache -I %S/Inputs/CHeaders -I %S/Inputs/Swift -package-name Test
// Step 4: Ensure that same-package scan can see package-only dependencies
// RUN: %FileCheck %s --input-file=%t/deps.json --check-prefix CHECK-SAME-PACKAGE

// Step 5: scan dependencies with '-no-scanner-module-validation' and no package name
// RUN: %target-swift-frontend -scan-dependencies %t/Client.swift -o %t/deps_no_package.json -no-scanner-module-validation -I %t -sdk %t -prebuilt-module-cache-path %t/clang-module-cache -I %S/Inputs/CHeaders -I %S/Inputs/Swift
// Step 6: Ensure that non-same-package scan can not see package-only dependencies
// RUN: %FileCheck %s --input-file=%t/deps_no_package.json --check-prefix CHECK-NO-PACKAGE


// CHECK-SAME-PACKAGE: "swift": "Bar"
// CHECK-NO-PACKAGE-NOT: "swift": "Bar"

//--- Bar.swift
enum PubEnum {
  case red, green
}

//--- Foo.swift
package import Bar

//--- Client.swift
import Foo


