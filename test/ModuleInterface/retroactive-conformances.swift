// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: extension Swift.Int : @retroactive Swift.Identifiable {
// CHECK:   public var id: Swift.Int {
// CHECK:     get
// CHECK:   }
// CHECK:   public typealias ID = Swift.Int
// CHECK: }
extension Int: @retroactive Identifiable {
    public var id: Int { self }
}
