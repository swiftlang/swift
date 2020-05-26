// RUN: %empty-directory(%t)

// Step 1. Create a module with the name ModuleType
// RUN: echo 'public struct MyStruct {}' | %target-swift-frontend -emit-module -o %t/ModuleType.swiftmodule -

// Step 2. Create a second module with a type named ModuleType
// RUN: echo 'public struct ModuleType {}' | %target-swift-frontend -emit-module -o %t/SecondModule.swiftmodule -

// Step 3. Import both modules and try to use `ModuleType.MyStruct`
//         Make sure we emitted a note saying that we failed while trying to
//         look up into `SecondModule.ModuleType`
// RUN: not %target-swift-frontend -typecheck %s -I %t 2>&1 | %FileCheck %s

import ModuleType
import SecondModule

public func f(_ x: ModuleType.MyStruct) {}

// CHECK: error: 'MyStruct' is not a member type of 'ModuleType'
// CHECK: SecondModule.ModuleType:1:15: note: 'ModuleType' declared here
// CHECK: public struct ModuleType {
// CHECK:               ^
