// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: #if compiler(>=5.3) && $RetroactiveAttribute
// CHECK: extension Swift.Int : @retroactive Swift.Identifiable {
// CHECK:   public var id: Swift.Int {
// CHECK:     get
// CHECK:   }
// CHECK:   public typealias ID = Swift.Int
// CHECK: }
// CHECK: #else
// CHECK: extension Swift.Int : Swift.Identifiable {
// CHECK:   public var id: Swift.Int {
// CHECK:     get
// CHECK:   }
// CHECK:   public typealias ID = Swift.Int
// CHECK: }
// CHECK: #endif
extension Int: @retroactive Identifiable {
    public var id: Int { self }
}
