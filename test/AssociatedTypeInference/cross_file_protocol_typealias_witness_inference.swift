// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -module-name Library %t/Library.swift -emit-module-path %t/Library.swiftmodule
// RUN: %target-swift-frontend -emit-module -module-name Client %t/Use.swift %t/Conformer.swift -I %t -emit-module-path %t/Client.swiftmodule

// Keep Use.swift before Conformer.swift. Resolving ConcreteView.Value from the
// earlier file must not invalidate the imported typealias in the later
// conformance witness.

//--- Library.swift

public protocol View {
  associatedtype Value

  func withValue<Result>(_ body: Binding<Result>) -> Result
}

extension View {
  public typealias Binding<Result> = (Value) -> Result
}

public struct RequiresValue<V: View, Expected> where V.Value == Expected {}

public protocol InferredView {
  associatedtype Value

  func withValue<Result>(_ body: InferredBinding<Result>) -> Result
  func accept(_: Value)
}

extension InferredView {
  public typealias InferredBinding<Result> = (Value) -> Result
}

public struct RequiresInferredValue<V: InferredView, Expected>
where V.Value == Expected {}

//--- Use.swift

import Library

struct Model {}

func use(_: RequiresValue<ConcreteView<Model>, Model>) {}
func use(_: RequiresValue<QualifiedConcreteView<Model>, Model>) {}
func use(_: RequiresInferredValue<ConcreteInferredView<String>, Int>) {}

//--- Conformer.swift

import Library

struct ConcreteView<Value>: View {
  func withValue<Result>(_ body: Binding<Result>) -> Result {
    fatalError()
  }
}

struct QualifiedConcreteView<Value>: View {
  func withValue<Result>(_ body: Self.Binding<Result>) -> Result {
    fatalError()
  }
}

struct ConcreteInferredView<Unused>: InferredView {
  func withValue<Result>(_ body: InferredBinding<Result>) -> Result {
    fatalError()
  }

  func accept(_: Int) {}
}
