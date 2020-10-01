// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -emit-module-interface-path %t/main.swiftinterface -enable-library-evolution
// RUN: %FileCheck %s < %t/main.swiftinterface

// RUN: %target-swift-frontend -emit-module -module-name main -primary-file %s -emit-module-path %t/main~partial.swiftmodule -enable-library-evolution

// RUN: %target-swift-frontend -merge-modules %t/main~partial.swiftmodule -emit-module-path %t/main.swiftmodule -emit-module-interface-path %t/main.swiftinterface -enable-library-evolution
// RUN: %FileCheck %s < %t/main.swiftinterface

// CHECK: import Swift

// CHECK: public struct Holder<Value> {
public struct Holder<Value> {
    var value: Value

    // CHECK-NEXT: public init(value: Value){{$}}
    public init(value: Value) {
        self.value = value
    }

    // CHECK-NEXT: public init<T>(_ value: T) where Value == Swift.AnyHashable, T : Swift.Hashable{{$}}
    public init<T : Hashable>(_ value: T) where Value == AnyHashable {
        self.value = value
    }

    // CHECK-NEXT: public struct Transform<Result> {
    public struct Transform<Result> {
        var fn: (Value) -> Result

        // CHECK-NEXT: public init(fn: @escaping (Value) -> Result){{$}}
        public init(fn: @escaping (Value) -> Result) {
            self.fn = fn
        }

        // CHECK-NEXT: func transform(_ holder: main.Holder<Value>) -> Result{{$}}
        public func transform(_ holder: Holder<Value>) -> Result {
            return fn(holder.value)
        }
    
    // CHECK-NEXT: }
    }

// CHECK-NEXT: }
}

// CHECK-NEXT: extension Holder.Transform where Value == Swift.Int {
extension Holder.Transform where Value == Int {
    // CHECK-NEXT: public func negate(_ holder: main.Holder<Value>) -> Result{{$}}
    public func negate(_ holder: Holder<Value>) -> Result {
        return transform(Holder(value: -holder.value))
    }
}