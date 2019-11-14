// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/main~partial.swiftmodule -primary-file %s %S/Inputs/other.swift -module-name main
// RUN: %target-swift-frontend -emit-module -o %t/other~partial.swiftmodule %s -primary-file %S/Inputs/other.swift -module-name main
// RUN: %target-swift-frontend -merge-modules -emit-module -o /dev/null -emit-module-interface-path - %t/main~partial.swiftmodule -module-name main %t/other~partial.swiftmodule | %FileCheck %s

// CHECK: {{^}}public func verySimpleFunction(){{$}}
public func verySimpleFunction() {}

// CHECK: {{^}}public func otherFileFunction(){{$}}
