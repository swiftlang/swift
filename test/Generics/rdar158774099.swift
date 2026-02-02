// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify \
// RUN:     %t/View.swift %t/Modifier.swift %t/MyStorage.swift %t/Storage.swift %t/WrappedStorage.swift

//--- Storage.swift

protocol Representable {
  associatedtype Outputs
  typealias Context = RepresentableContext<Self>
}

struct RepresentableContext<R: Representable> {}

struct Inputs<Outputs> {
}

protocol Storage<Value> {
  associatedtype Value
  associatedtype R: Representable

  func body(content: Content) -> Value

  typealias Content = Inputs<R.Outputs>
}

//--- WrappedStorage.swift
private struct WrappedStorage<Base: Storage>: Storage where Base.Value == MyStorage.Value {
  typealias R = Base.R

  init(base: Base, body: @escaping (Base.Value) -> Void) {
  }

  func body(content: Content) -> Base.Value { fatalError() }
}

extension Storage where Value == MyStorage.Value {
  func withStorage(body: @escaping (Value) -> Void) -> some Storage<Value> {
    WrappedStorage(base: self, body: body)
  }
}

//--- MyStorage.swift
struct MyRepresentable: Representable {
  struct Outputs {}
}

struct MyStorage: Storage {
  typealias R = MyRepresentable

  struct Value: Equatable {}

  func body(content: Content) -> Value {
    fatalError()
  }
}

//--- Modifier.swift
extension P {
  func storage(_: some Storage) -> some P { Test() }
}

//--- View.swift
protocol P {
}

struct Test: P {
}

func test() -> some P {
  let storage = MyStorage().withStorage { value in }
  return Test().storage(storage)
}
