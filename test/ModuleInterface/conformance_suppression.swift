// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Mojuel
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface -module-name Mojuel)

// CHECK:     public enum RecollectionOrganization<T> : ~Swift.BitwiseCopyable, Swift.Copyable where T : ~Copyable {
// CHECK-NEXT: }
public enum RecollectionOrganization<T : ~Copyable> : ~BitwiseCopyable, Copyable {}
