// RUN: %target-swift-emit-silgen -enable-sil-ownership %s -verify

// We just want to SILGen this and ensure it doesn't crash. Don't particularly
// care about the generated SIL.

// Inheritance of unnamed parameters.
class SuperUnnamed {
  init(int _: Int) { }
  init(_ : Double) { }

  init(string _: String) { }
  init(_ : Float) { }
}

class SubUnnamed : SuperUnnamed { }

func testSubUnnamed(_ i: Int, d: Double, s: String, f: Float) {
  _ = SubUnnamed(int: i)
  _ = SubUnnamed(d)
  _ = SubUnnamed(string: s)
  _ = SubUnnamed(f)
}

// rdar://problem/17960407 - Inheritance of initializers for generic types
class ConcreteBase {
  required init(i: Int) {}
}

class GenericDerived<T> : ConcreteBase {}

class GenericBase<T> {
  required init(t: T) {}
}

class GenericDerived2<U> : GenericBase<(U, U)> {}

class ConcreteDerived : GenericBase<Int> {}

func testGenericInheritance() {
  _ = GenericDerived<Int>(i: 10)
  _ = GenericDerived2<Int>(t: (10, 100))
  _ = ConcreteDerived(t: 1000)
}

// rdar://problem/34789779 - Inheritance of initializers with inout parameters
public class Node {
  var data : Data

  public struct Data {
    var index: Int32 = 0// for helpers
  }

 init(data: inout Data/*, context: Context*/) {
   self.data = data
 }

 public required init(node: Node) {
   data = node.data
 }
}

class SubNode : Node {
  var a: Int

  required init(node: Node) {
    a = 1
    super.init(node: node)
  }

  init(data: inout Data, additionalParam: Int) {
    a = additionalParam
    super.init(data: &data)
  }
}

class GenericSubNode<T> : SubNode {
  required init(node: Node) {
    super.init(node: node)
  }

  init(data: inout Data, value: T) {
    super.init(data: &data, additionalParam: 1)
  }
}

protocol HasValue {
  associatedtype Value
  func getValue() -> Value
}

class GenericWrapperNode<T : HasValue> : GenericSubNode<T.Value> {
  required init(node: Node) {
    super.init(node: node)
  }

  init(data: inout Data, otherValue: T) {
    super.init(data: &data, value: otherValue.getValue())
  }
}

// https://bugs.swift.org/browse/SR-3848 - Initializer with generic parameter
protocol P {
  associatedtype T
}

struct S<T> : P {}

class Outer<T> {
  class Inner<U> where U : P {
    init<V>(_: V) where V : P, V.T == U {}
  }
}

class Derived<X> : Outer<X>.Inner<S<X>> {}

protocol Q {
  associatedtype A
  associatedtype B
}

class Twice<X, Y> {
  init<Z>(_: Z) where Z : Q, Z.A == X, Z.B == Y, X == Y {}
}

class Pair<T, U> : Twice<T, U> {}

class Once<T> : Twice<T, T> {}
