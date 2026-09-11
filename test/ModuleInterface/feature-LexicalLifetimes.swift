// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/FeatureTest.swiftinterface) %s -module-name FeatureTest
// RUN: %target-swift-typecheck-module-from-interface(%t/FeatureTest.swiftinterface) -module-name FeatureTest
// RUN: %FileCheck %s < %t/FeatureTest.swiftinterface

// CHECK: @_noEagerMove public struct Permanent {
// CHECK-NEXT: }
@_noEagerMove
public struct Permanent {}

// CHECK: @_hasMissingDesignatedInitializers @_eagerMove public class Transient {
// CHECK-NEXT:   deinit
// CHECK-NEXT: }
@_eagerMove
public class Transient {}

// CHECK: @_lexicalLifetimes public func lexicalInAModuleWithoutLexicalLifetimes(_ t: FeatureTest::Transient)
@_lexicalLifetimes
public func lexicalInAModuleWithoutLexicalLifetimes(_ t: Transient) {}
