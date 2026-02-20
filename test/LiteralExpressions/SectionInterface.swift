// Enum case raw value expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -emit-module -module-name SectionLiteralExprInterface -emit-module-interface-path %t/SectionLiteralExprInterface.swiftinterface -enable-library-evolution -swift-version 5 %s -enable-experimental-feature LiteralExpressions
// RUN: %FileCheck %s < %t/SectionLiteralExprInterface.swiftinterface

// Simple arithmetic operators on integers
@section("mysection") public let intBinaryArithOp1 = 1 + 1

// As of today, the values do not get printed in the textual interface
// CHECK: @section("mysection") public let intBinaryArithOp1: Swift::Int
