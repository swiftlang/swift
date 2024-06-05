// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Mojuel
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface -module-name Mojuel)

// CHECK:      #if compiler(>=5.3) && $ConformanceSuppression
// CHECK-NEXT: public enum RecollectionOrganization<T> : ~Swift.BitwiseCopyable, Swift.Copyable where T : ~Copyable {
// CHECK-NEXT: }
// CHECK-NEXT: #elseif compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: public enum RecollectionOrganization<T> : Swift.Copyable where T : ~Copyable {
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public enum RecollectionOrganization<T> {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
public enum RecollectionOrganization<T : ~Copyable> : ~BitwiseCopyable, Copyable {}
