// RUN: %target-swift-emit-silgen %s -module-name test | %FileCheck %s

protocol NoncopyableProto: ~Copyable {}

protocol CopyableProto {}

protocol CopyableProto2 {}

// <T: ~Copyable>
struct A<T: ~Copyable> {
  // Members in an extenion with the same generic signature as the nominal
  // should mangle the same, so make sure we mangle the following as if it were
  // in a constrained extension.

  // (extension in test):test.A<A where A: ~Swift.Copyable>.foo() -> ()
  // CHECK: $s4test1AVAARizlE3fooyyF
  func foo() {}

  // test.A.weird() -> ()
  // CHECK: $s4test1AV5weirdyyF
  func weird() where T: Copyable {}

  // variable initialization expression of (extension in test):test.A<A where A: ~Swift.Copyable>.property : Swift.Int
  // CHECK: $s4test1AVAARizlE8propertySivpfi
  // (extension in test):test.A<A where A: ~Swift.Copyable>.property.getter : Swift.Int
  // CHECK: $s4test1AVAARizlE8propertySivg
  // (extension in test):test.A<A where A: ~Swift.Copyable>.property.setter : Swift.Int
  // CHECK: $s4test1AVAARizlE8propertySivs
  // (extension in test):test.A<A where A: ~Swift.Copyable>.property.modify : Swift.Int
  // CHECK: $s4test1AVAARizlE8propertySivM
  // default argument 0 of (extension in test):test.A<A where A: ~Swift.Copyable>.init(property: Swift.Int) -> test.A<A>
  // CHECK: $s4test1AVAARizlE8propertyACyxGSi_tcfcfA_
  var property: Int = 0

  // (extension in test):test.A<A where A: ~Swift.Copyable>.computed.getter : Swift.String
  // CHECK: $s4test1AVAARizlE8computedSSvg
  var computed: String {
    ""
  }

  // (extension in test):test.A<A where A: ~Swift.Copyable>.init() -> test.A<A>
  // CHECK: $s4test1AVAARizlEACyxGycfC
  // init(property:)
}

// <T: ~Copyable>
extension A where T: ~Copyable {
  // (extension in test):test.A<A where A: ~Swift.Copyable>.bar() -> ()
  // CHECK: $s4test1AVAARizlE3baryyF
  func bar() {}

  // test.A.weird2() -> ()
  // CHECK: $s4test1AV6weird2yyF
  func weird2() where T: Copyable {}
}

// <T: ~Copyable & NoncopyableProto>
extension A where T: ~Copyable & NoncopyableProto {
  // (extension in test):test.A<A where A: test.NoncopyableProto, A: ~Swift.Copyable>.dumb() -> ()
  // CHECK: $s4test1AVA2A16NoncopyableProtoRzRizlE4dumbyyF
  func dumb() {}
}

// <T: Copyable>
extension A {
  // However, an extension that has all of the positive requirements to inverse
  // requirements must be mangled as if there were no inverse generics.

  // test.A.baz() -> ()
  // CHECK: $s4test1AV3bazyyF
  func baz() {}

  // test.A.computedAgain.getter : Swift.Int
  // CHECK: $s4test1AV13computedAgainSivg
  var computedAgain: Int {
    123
  }
}

// <T: CopyableProto> (implies T: Copyable)
extension A where T: CopyableProto {
  // An extension where T: Copyable, but has extra constraints must be mangle
  // the extra constraints and not any inverses.

  // (extension in test):test.A<A where A: test.CopyableProto>.something() -> ()
  // CHECK: $s4test1AVA2A13CopyableProtoRzlE9somethingyyF
  func something() {}
}

// <T == Int>
extension A where T == Int {
  // (extension in test):test.A<A where A == Swift.Int>.int() -> ()
  // CHECK: $s4test1AVAASiRszlE3intyyF
  func int() {}
}

// <T: ~Copyable & NoncopyableProto>
struct B<T: ~Copyable & NoncopyableProto> {}

// <T: Copyable & NoncopyableProto>
extension B {
  // Members of this extension should be treated as if they were just in 'B' and
  // not mangle any requirements about 'NoncopyableProto' because that's implied
  // by the context.

  // test.B.foo() -> ()
  // CHECK: $s4test1BV3fooyyF
  func foo() {}
}

// <T: Copyable>
struct C<T> {
  // test.C.foo() -> ()
  // CHECK: $s4test1CV3fooyyF
  func foo() {}

  // test.C.something<A where A1: ~Swift.Copyable>() -> A1
  // CHECK: $s4test1CV9somethingqd__yRid__lF
  func something<U: ~Copyable>() -> U {
    fatalError()
  }
}

// <T: Copyable>
extension C {
  // test.C.bar() -> ()
  // CHECK: $s4test1CV3baryyF
  func bar() {}
}

// <T: CopyableProto> (implies T: Copyable)
extension C where T: CopyableProto {
  // (extension in test):test.C<A where A: test.CopyableProto>.baz() -> ()
  // CHECK: $s4test1CVA2A13CopyableProtoRzlE3bazyyF
  func baz() {}
}

// <T: CopyableProto> (implies T: Copyable)
struct D<T: CopyableProto> {
  // test.D.foo() -> ()
  // CHECK: $s4test1DV3fooyyF
  func foo() {}
}

// <T: CopyableProto> (implies T: Copyable)
extension D {
  // test.D.bar() -> ()
  // CHECK: $s4test1DV3baryyF
  func bar() {}
}

// <T: CopyableProto & CopyableProto2> (implies T: Copyable)
extension D where T: CopyableProto2 {
  // (extension in test):test.D< where A: test.CopyableProto2>.baz() -> ()
  // CHECK: $s4test1DVA2A14CopyableProto2RzrlE3bazyyF
  func baz() {}
}

//===----------------------------------------------------------------------===//
// @_preInverseGenerics
//===----------------------------------------------------------------------===//

// Members with @_preInverseGenerics should completely ignore inverse
// requirements.

// <T: ~Copyable>
struct E<T: ~Copyable> {
  // test.E.foo() -> ()
  // CHECK: $s4test1EV3fooyyF
  @_preInverseGenerics
  func foo() {}

  // test.E.something<A>() -> A1
  // CHECK: $s4test1EV9somethingqd__ylF
  @_preInverseGenerics
  func something<U: ~Copyable>() -> U {
    fatalError()
  }

  // variable initialization expression of test.E.property : Swift.Int
  // CHECK: $s4test1EV8propertySivpfi
  // test.E.property.getter : Swift.Int
  // CHECK: $s4test1EV8propertySivg
  // test.E.property.setter : Swift.Int
  // CHECK: $s4test1EV8propertySivs
  // test.E.property.modify : Swift.Int
  // CHECK: $s4test1EV8propertySivM

  // Note: The default argument function will include inverse information because
  // it's coming from the implicit init that doesn't have @_preInverseGenerics
  // default argument 0 of (extension in test):test.E<A where A: ~Swift.Copyable>.init(property: Swift.Int) -> test.E<A>
  // CHECK: $s4test1EVAARizlE8propertyACyxGSi_tcfcfA_
  @_preInverseGenerics
  var property: Int = 123

  // Implicit inits are still mangled as if there was an inverse generic req.
  // (extension in test):test.E<A where A: ~Swift.Copyable>.init(property: Swift.Int) -> test.E<A>
  // CHECK: $s4test1EVAARizlE8propertyACyxGSi_tcfC
}

// <T: ~Copyable>
extension E where T: ~Copyable {
  // test.E.bar() -> ()
  // CHECK: $s4test1EV3baryyF
  @_preInverseGenerics
  func bar() {}
}

// <T: ~Copyable & NoncopyableProto>
extension E where T: ~Copyable & NoncopyableProto {
  // (extension in test):test.E<A where A: test.NoncopyableProto>.dumb() -> ()
  // CHECK: $s4test1EVA2A16NoncopyableProtoRzlE4dumbyyF
  @_preInverseGenerics
  func dumb() {}
}


