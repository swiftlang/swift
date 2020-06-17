// RUN: %empty-directory(%t)
// RUN: mkdir %t/sub_module %t/main_module
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/sub_module/SubModule.swiftmodule %S/Inputs/SubModule.swift -I %S/Inputs
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/main_module/MainModule.swiftmodule -emit-module-interface-path %t/main_module/MainModule.swiftinterface -I %t/sub_module %S/Inputs/MainModule.swift -I %S/Inputs
// RUN: %target-swift-frontend -typecheck -swift-version 5 %t/main_module/MainModule.swiftinterface -I %t/sub_module -I %S/Inputs
