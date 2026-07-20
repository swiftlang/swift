// RUN: %target-typecheck-verify-swift -solver-disable-enumerate-supertypes
// RUN: %target-swift-frontend -typecheck %s -solver-enable-enumerate-supertypes

// These examples were held up by the enumerateDirectSupertypes() hack
// which would attempt superclasses of a supertype binding.

do {
  class Base {
    var next: Base?
  }

  class Derived: Base {}

  func g<T>(_: T, _: (T) -> T?) {}

  func f1(d: Derived) {
    g(d, { $0.next })
    // expected-error@-1 {{cannot convert value of type 'Base?' to closure result type 'Derived?'}}
  }

  // Upcasting the base of the member reference expression in one of various
  // ways is sufficient to make this type check.
  func f2(d: Derived) {
    g(d as Base, { $0.next })
  }

  func f4(d: Derived) {
    g(d, { ($0 as Base).next })
  }

  func f3(d: Derived) {
    g(d, { (x: Base) in x.next })
  }
}

// This is a similar setup to the above.
do {
  class NSObject: Hashable {
    func hash(into: inout Hasher) {}
    static func ==(lhs: NSObject, rhs: NSObject) -> Bool { return false }
  }

  class TreeNode: NSObject {
    let childrenNodes: [TreeNode] = []
    var parent: TreeNode? = nil
  }

  class FirstNode: TreeNode {}

  class OtherNode: TreeNode {}

  func f<Node>(_: Node, _: (Node) -> Node?, _: ((Node) -> Bool)) -> Node? {
    return nil
  }

  func g<Node>(_: Node, _: (Node) -> [Node]) -> Set<Node> {
    return []
  }

  func test1(node: FirstNode) -> (TreeNode?, Set<TreeNode>) {
    let x = f(node, \.parent) { $0 is OtherNode }
    // expected-error@-1 {{failed to produce diagnostic for expression}}
    let y = g(node, \.childrenNodes)
    // expected-error@-1 {{failed to produce diagnostic for expression}}
    return (x, y)
  }

  func test2(node: FirstNode) -> (TreeNode?, Set<TreeNode>) {
    let x = f(node, \TreeNode.parent) { $0 is OtherNode }
    let y = g(node, \TreeNode.childrenNodes)
    return (x, y)
  }

  func test3(node: FirstNode) -> (TreeNode?, Set<TreeNode>) {
    let x = f(node, \FirstNode.parent) { $0 is OtherNode }
    // expected-error@-1 {{failed to produce diagnostic for expression}}
    let y = g(node, \FirstNode.childrenNodes)
    // expected-error@-1 {{failed to produce diagnostic for expression}}
    return (x, y)
  }

  func test4(node: FirstNode) -> (TreeNode?, Set<TreeNode>) {
    let x = f(node as TreeNode, \.parent) { $0 is OtherNode }
    let y = g(node as TreeNode, \.childrenNodes)
    return (x, y)
  }
}

// This one might be solvable without supertype enumeration, but that remains TBD.
do {
  class Base {
      static func updateAll() -> First { fatalError() }
  }

  class First: Base {}

  class Second: Base {
      init(_: Int) {}
  }

  func f0(array: [Int], b: Bool) -> [Base] {
      return [[Second(0)], [.updateAll()]].flatMap { $0 }
  }


  func f1(array: [Int], b: Bool) -> [Base] {
      return [[.updateAll()], [Second(0)]].flatMap { $0 }
  }

  func f2(array: [Int], b: Bool) -> [Base] {
      return [[.updateAll()], array.map { Second($0) }].flatMap { $0 }
      // expected-error@-1 {{member 'updateAll()' in 'Second' produces result of type 'First', but context expects 'Second'}}
  }

  func f3(array: [Int], b: Bool) -> [Base] {
      return [array.map { Second($0) }, [.updateAll()]].flatMap { $0 }
      // expected-error@-1 {{member 'updateAll()' in 'Second' produces result of type 'First', but context expects 'Second'}}
  }

  func f4(array: [Int], b: Bool) -> [Base] {
      return [b ? [.updateAll()] : [], array.map { Second($0) }].flatMap { $0 }
      // expected-error@-1 {{member 'updateAll()' in 'Second' produces result of type 'First', but context expects 'Second'}}
  }

  func f5(array: [Int], b: Bool) -> [Base] {
      return [b ? [] : [.updateAll()], array.map { Second($0) }].flatMap { $0 }
      // expected-error@-1 {{member 'updateAll()' in 'Second' produces result of type 'First', but context expects 'Second'}}
  }

  func f6(array: [Int], b: Bool) -> [Base] {
      return [array.map { Second($0) }, b ? [.updateAll()] : []].flatMap { $0 }
      // expected-error@-1 {{member 'updateAll()' in 'Second' produces result of type 'First', but context expects 'Second'}}
  }

  func f7(array: [Int], b: Bool) -> [Base] {
      return [array.map { Second($0) }, b ? [] : [.updateAll()]].flatMap { $0 }
      // expected-error@-1 {{member 'updateAll()' in 'Second' produces result of type 'First', but context expects 'Second'}}
  }
}

// This example is actually invalid, but it used to type check.
do {
  func f1(_ property: First) {
    switch property {
    case .property1: break
    case .property2: break // expected-error {{member 'property2' in 'First' produces result of type 'Second<Int>', but context expects 'First'}}
    case .property3: break // expected-error {{member 'property3' in 'First' produces result of type 'Second<Bool>', but context expects 'First'}}
    case .property4: break
    default: break
    }
  }

  // Now, upcasting the switch expression to the base class allows the code to
  // type check as before; the two middle cases are unreachable.
  func f2(_ property: First) {
    switch property as Base {
    case .property1: break
    case .property2: break
    case .property3: break
    case .property4: break
    default: break
    }
  }

  class Base: Equatable {
    static func ==(_: Base, _: Base) -> Bool {
      fatalError()
    }

    static let property1 = First()
    static let property2 = Second<Int>()
    static let property3 = Second<Bool>()
    static let property4 = First()
  }

  class First: Base {}

  class Second<Value>: Base {}
}

// Another invalid case we used to accept.
do {
  func f<T: AnyObject, U>(_: T, _: ReferenceWritableKeyPath<T, U>, _: U) {}
  func f<T, U>(_: inout T, _: WritableKeyPath<T, U>, _: U) {}

  class D {}

  class E: C {}

  class C {
    var d: AnyObject?

    func g(d: D) {
      f(self, \E.d, d)
      // expected-error@-1 {{cannot convert value of type 'C' to expected argument type 'E'}}

      f(self, \Self.d, d)
      // expected-error@-1 {{cannot convert value of type 'C' to expected argument type 'Self'}}
    }
  }
}

// Another invalid keypath root.
do {
  struct SortDescriptor<Compared> {
    init(_: KeyPath<Compared, String>) {}
    init(_: KeyPath<Compared, String?>) {}
    init(_: KeyPath<Compared, Int>) {}
    init(_: KeyPath<Compared, Int?>) {}
    init(_: KeyPath<Compared, Int8>) {}
    init(_: KeyPath<Compared, Int8?>) {}
  }

  class Base {
    var x: Int? = nil
  }
  class Derived: Base {}

  struct G<T: Base> {
    init(_: [GG<T>], _: [SortDescriptor<T>]) {}
  }

  struct GG<T: Base> {
    static func f() -> GG<Base> { fatalError() }
  }

  func test() {
    let gg = GG.f()
    let _ = G([gg], [SortDescriptor(\Derived.x)])
    // expected-error@-1 {{cannot convert value of type 'KeyPath<Derived, Int?>' to expected argument type 'KeyPath<Base, Int?>'}}
    // expected-note@-2 {{arguments to generic parameter 'Root' ('Derived' and 'Base') are expected to be equal}}
  }
}
