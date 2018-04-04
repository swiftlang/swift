// RUN: %target-typecheck-verify-swift -module-name ModuleName

protocol ClassProtocol : class {
  init()
  init?(failable: Void)
  init(throwing: Void) throws
}

class C : ClassProtocol {
  required init() {}
  required init?(failable: Void) {}
  required init(throwing: Void) throws {}
}

class D : C {}

func testWeakVariableBindingDiag() throws {
  weak var c1 = C() // expected-warning {{instance will be immediately deallocated as 'c1' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c1' declared here}}
  // expected-note@-3 {{'c1' declared here}}

  c1 = C() // expected-warning {{instance will be immediately deallocated as 'c1' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c2: C? = ModuleName.C() // expected-warning {{instance will be immediately deallocated as 'c2' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c2' declared here}}
  // expected-note@-3 {{'c2' declared here}}

  c2 = C() // expected-warning {{instance will be immediately deallocated as 'c2' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c3: C? = D() // expected-warning {{instance will be immediately deallocated as 'c3' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c3' declared here}}
  // expected-note@-3 {{'c3' declared here}}

  c3 = D() // expected-warning {{instance will be immediately deallocated as 'c3' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c4 = C(failable: ()) // expected-warning {{instance will be immediately deallocated as 'c4' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c4' declared here}}
  // expected-note@-3 {{'c4' declared here}}

  c4 = C(failable: ()) // expected-warning {{instance will be immediately deallocated as 'c4' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c5: C? = C(failable: ()) // expected-warning {{instance will be immediately deallocated as 'c5' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c5' declared here}}
  // expected-note@-3 {{'c5' declared here}}

  c5 = C(failable: ()) // expected-warning {{instance will be immediately deallocated as 'c5' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c6: C? = D(failable: ()) // expected-warning {{instance will be immediately deallocated as 'c6' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c6' declared here}}
  // expected-note@-3 {{'c6' declared here}}

  c6 = D(failable: ()) // expected-warning {{instance will be immediately deallocated as 'c6' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c7 = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c7' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c7' declared here}}
  // expected-note@-3 {{'c7' declared here}}

  c7 = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c7' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c8: C? = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c8' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c8' declared here}}
  // expected-note@-3 {{'c8' declared here}}

  c8 = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c8' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c9: C? = try D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c9' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c9' declared here}}
  // expected-note@-3 {{'c9' declared here}}

  c9 = try D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c9' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c10 = try! C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c10' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c10' declared here}}
  // expected-note@-3 {{'c10' declared here}}

  c10 = try! C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c10' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c11: C? = try! C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c11' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c11' declared here}}
  // expected-note@-3 {{'c11' declared here}}

  c11 = try! C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c11' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c12: C? = try! D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c12' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c12' declared here}}
  // expected-note@-3 {{'c12' declared here}}

  c12 = try! D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c12' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c13 = try? C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c13' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c13' declared here}}
  // expected-note@-3 {{'c13' declared here}}

  c13 = try? C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c13' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c14: C? = try? C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c14' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c14' declared here}}
  // expected-note@-3 {{'c14' declared here}}

  c14 = try? C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c14' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  weak var c15: C? = try? D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c15' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c15' declared here}}
  // expected-note@-3 {{'c15' declared here}}

  c15 = try? D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c15' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  _ = c1; _ = c2; _ = c3; _ = c4; _ = c5; _ = c6; _ = c7; _ = c8; _ = c9; _ = c10; _ = c11; _ = c12; _ = c13; _ = c14; _ = c15
}

func testUnownedVariableBindingDiag() throws {
  unowned(unsafe) var c = C() // expected-warning {{instance will be immediately deallocated as 'c' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c' declared here}}
  // expected-note@-3 {{'c' declared here}}

  c = C() // expected-warning {{instance will be immediately deallocated as 'c' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c1 = C() // expected-warning {{instance will be immediately deallocated as 'c1' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c1' declared here}}
  // expected-note@-3 {{'c1' declared here}}

  c1 = C() // expected-warning {{instance will be immediately deallocated as 'c1' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c2: C = ModuleName.C() // expected-warning {{instance will be immediately deallocated as 'c2' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c2' declared here}}
  // expected-note@-3 {{'c2' declared here}}

  c2 = C() // expected-warning {{instance will be immediately deallocated as 'c2' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c3: C = D() // expected-warning {{instance will be immediately deallocated as 'c3' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c3' declared here}}
  // expected-note@-3 {{'c3' declared here}}

  c3 = D() // expected-warning {{instance will be immediately deallocated as 'c3' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c4 = C(failable: ())! // expected-warning {{instance will be immediately deallocated as 'c4' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c4' declared here}}
  // expected-note@-3 {{'c4' declared here}}

  c4 = C(failable: ())! // expected-warning {{instance will be immediately deallocated as 'c4' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c5: C = C(failable: ())! // expected-warning {{instance will be immediately deallocated as 'c5' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c5' declared here}}
  // expected-note@-3 {{'c5' declared here}}

  c5 = C(failable: ())! // expected-warning {{instance will be immediately deallocated as 'c5' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c6: C = D(failable: ())! // expected-warning {{instance will be immediately deallocated as 'c6' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c6' declared here}}
  // expected-note@-3 {{'c6' declared here}}

  c6 = D(failable: ())! // expected-warning {{instance will be immediately deallocated as 'c6' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c7 = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c7' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c7' declared here}}
  // expected-note@-3 {{'c7' declared here}}

  c7 = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c7' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c8: C = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c8' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c8' declared here}}
  // expected-note@-3 {{'c8' declared here}}

  c8 = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c8' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c9: C = try D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c9' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c9' declared here}}
  // expected-note@-3 {{'c9' declared here}}

  c9 = try D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c9' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c10 = try! C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c10' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c10' declared here}}
  // expected-note@-3 {{'c10' declared here}}

  c10 = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c10' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c11: C = try! C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c11' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c11' declared here}}
  // expected-note@-3 {{'c11' declared here}}

  c11 = try C(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c11' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c12: C = try! D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c12' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c12' declared here}}
  // expected-note@-3 {{'c12' declared here}}

  c12 = try D(throwing: ()) // expected-warning {{instance will be immediately deallocated as 'c12' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c13 = (try? C(throwing: ()))! // expected-warning {{instance will be immediately deallocated as 'c13' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c13' declared here}}
  // expected-note@-3 {{'c13' declared here}}

  c13 = (try? C(throwing: ()))! // expected-warning {{instance will be immediately deallocated as 'c13' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c14: C = (try? C(throwing: ()))! // expected-warning {{instance will be immediately deallocated as 'c14' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c14' declared here}}
  // expected-note@-3 {{'c14' declared here}}

  c14 = (try? C(throwing: ()))! // expected-warning {{instance will be immediately deallocated as 'c14' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned var c15: C = (try? D(throwing: ()))! // expected-warning {{instance will be immediately deallocated as 'c15' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c15' declared here}}
  // expected-note@-3 {{'c15' declared here}}

  c15 = (try? D(throwing: ()))! // expected-warning {{instance will be immediately deallocated as 'c15' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  _ = c; _ = c1; _ = c2; _ = c3; _ = c4; _ = c5; _ = c6; _ = c7; _ = c8; _ = c9; _ = c10; _ = c11; _ = c12; _ = c13; _ = c14; _ = c15
}

func testMultipleBindingDiag() {
  weak var c1 = C(), c2: C? = C(), c3: C? = D()
  // expected-warning@-1 {{instance will be immediately deallocated as 'c1' is a 'weak' variable}}
  // expected-note@-2 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-3 {{'c1' declared here}}
  // expected-warning@-4 {{instance will be immediately deallocated as 'c2' is a 'weak' variable}}
  // expected-note@-5 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-6 {{'c2' declared here}}
  // expected-warning@-7 {{instance will be immediately deallocated as 'c3' is a 'weak' variable}}
  // expected-note@-8 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-9 {{'c3' declared here}}

  unowned let c4 = C(), c5: C = C(), c6: C = D()
  // expected-warning@-1 {{instance will be immediately deallocated as 'c4' is an 'unowned' variable}}
  // expected-note@-2 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-3 {{'c4' declared here}}
  // expected-warning@-4 {{instance will be immediately deallocated as 'c5' is an 'unowned' variable}}
  // expected-note@-5 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-6 {{'c5' declared here}}
  // expected-warning@-7 {{instance will be immediately deallocated as 'c6' is an 'unowned' variable}}
  // expected-note@-8 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-9 {{'c6' declared here}}

  _ = c1; _ = c2; _ = c3; _ = c4; _ = c5; _ = c6
}

func testTupleAndParenBinding() throws {
  weak var ((c1), c2, c3): (C?, C?, C?) = (C() as C, (D()), try D(throwing: ()))
  // expected-warning@-1 {{instance will be immediately deallocated as 'c1' is a 'weak' variable}}
  // expected-note@-2 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-3 {{'c1' declared here}}
  // expected-warning@-4 {{instance will be immediately deallocated as 'c2' is a 'weak' variable}}
  // expected-note@-5 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-6 {{'c2' declared here}}
  // expected-warning@-7 {{instance will be immediately deallocated as 'c3' is a 'weak' variable}}
  // expected-note@-8 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-9 {{'c3' declared here}}

  unowned let ((c4), c5, c6): (C, C, C) = (C() as C, (D()), try D(throwing: ()))
  // expected-warning@-1 {{instance will be immediately deallocated as 'c4' is an 'unowned' variable}}
  // expected-note@-2 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-3 {{'c4' declared here}}
  // expected-warning@-4 {{instance will be immediately deallocated as 'c5' is an 'unowned' variable}}
  // expected-note@-5 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-6 {{'c5' declared here}}
  // expected-warning@-7 {{instance will be immediately deallocated as 'c6' is an 'unowned' variable}}
  // expected-note@-8 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-9 {{'c6' declared here}}

  _ = c1; _ = c2; _ = c3; _ = c4; _ = c5; _ = c6
}

func testInitializationThroughClassArchetypeDiag<T : ClassProtocol>(_ t: T, _ p: ClassProtocol) throws {
  weak var t1: T? = T() // expected-warning {{instance will be immediately deallocated as 't1' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'t1' declared here}}

  weak var t2: ClassProtocol? = T(failable: ()) // expected-warning {{instance will be immediately deallocated as 't2' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'t2' declared here}}

  unowned let t3 = try type(of: t).init(throwing: ()) // expected-warning {{instance will be immediately deallocated as 't3' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'t3' declared here}}

  unowned(unsafe) let t4 = type(of: p).init() // expected-warning {{instance will be immediately deallocated as 't4' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'t4' declared here}}

  let optionalTType: T.Type? = T.self
  let optionalPType: ClassProtocol.Type? = type(of: p)

  weak var t5 = optionalTType?.init(failable: ()) // expected-warning {{instance will be immediately deallocated as 't5' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'t5' declared here}}

  unowned(unsafe) let t6 = try (optionalPType?.init(throwing: ()))! // expected-warning {{instance will be immediately deallocated as 't6' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'t6' declared here}}

  _ = t1; _ = t2; _ = t3; _ = t4; _ = t5; _ = t6
}

weak var topLevelC = C() // expected-warning {{instance will be immediately deallocated as 'topLevelC' is a 'weak' variable}}
// expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
// expected-note@-2 {{'topLevelC' declared here}}
// expected-note@-3 {{'topLevelC' declared here}}

topLevelC = C() // expected-warning {{instance will be immediately deallocated as 'topLevelC' is a 'weak' variable}}
// expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

unowned var topLevelC1 = C() // expected-warning {{instance will be immediately deallocated as 'topLevelC1' is an 'unowned' variable}}
// expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
// expected-note@-2 {{'topLevelC1' declared here}}
// expected-note@-3 {{'topLevelC1' declared here}}

topLevelC1 = C() // expected-warning {{instance will be immediately deallocated as 'topLevelC1' is an 'unowned' variable}}
// expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

struct S {
  weak var c: C? // expected-note {{'c' declared here}}

  unowned var c1 = C() // expected-warning {{instance will be immediately deallocated as 'c1' is an 'unowned' property}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c1' declared here}}
  // expected-note@-3 {{'c1' declared here}}

  mutating func foo() {
    c = D() // expected-warning {{instance will be immediately deallocated as 'c' is a 'weak' property}}
    // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

    c1 = D() // expected-warning {{instance will be immediately deallocated as 'c1' is an 'unowned' property}}
    // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  }
}

class C1 {
  weak var c: C? // expected-note {{'c' declared here}}

  unowned var c1 = C() // expected-warning {{instance will be immediately deallocated as 'c1' is an 'unowned' property}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c1' declared here}}
  // expected-note@-3 {{'c1' declared here}}

  func foo() {
    c = D() // expected-warning {{instance will be immediately deallocated as 'c' is a 'weak' property}}
    // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

    c1 = D() // expected-warning {{instance will be immediately deallocated as 'c1' is an 'unowned' property}}
    // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  }
}

func testInitializationThroughMetaclassDiag(_ t: C.Type) {
  weak var c1: C? = t.init() // expected-warning {{instance will be immediately deallocated as 'c1' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c1' declared here}}

  let optionaCType: C.Type? = t
  weak var c2 = optionaCType?.init(failable: ()) // expected-warning {{instance will be immediately deallocated as 'c2' is a 'weak' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c2' declared here}}

  _ = c1; _ = c2
}

func testInitializationThroughTupleElementDiag() {
  unowned var c1 = ((C() as C, C() as C) as (C, C)).0 // expected-warning {{instance will be immediately deallocated as 'c1' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-2 {{'c1' declared here}}
  // expected-note@-3 {{'c1' declared here}}

  c1 = ((C() as C, C() as C) as (C, C)).0 // expected-warning {{instance will be immediately deallocated as 'c1' is an 'unowned' variable}}
  // expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}

  unowned let (c2, c3) = ((C() as C, C()) as (C, C), 5).0
  // expected-warning@-1 {{instance will be immediately deallocated as 'c2' is an 'unowned' variable}}
  // expected-note@-2 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-3 {{'c2' declared here}}
  // expected-warning@-4 {{instance will be immediately deallocated as 'c3' is an 'unowned' variable}}
  // expected-note@-5 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-6 {{'c3' declared here}}

  _ = c1; _ = c2; _ = c3
}

class E<T> {}

func testGenericWeakClassDiag() {
  weak var e = E<String>()
  // expected-warning@-1 {{instance will be immediately deallocated as 'e' is a 'weak' variable}}
  // expected-note@-2 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-3 {{'e' declared here}}

  _ = e
}

// The diagnostic doesn't currently support tuple shuffles.
func testDontDiagnoseThroughTupleShuffles() {
  unowned let (c1, (c2, c3)): (c: C, (b: C, a: C)) = ((a: D(), b: C()), c: D())
  unowned let c4 = ((a: C(), b: C()) as (b: C, a: C)).0

  _ = c1; _ = c2; _ = c3; _ = c4
}

extension Optional {
  init(dontDiagnoseOnThis: Void) {
    self = nil
  }
}

func testDontDiagnoseOnUnrelatedInitializer() {
  weak var c = C?(dontDiagnoseOnThis: ())
  unowned let c1 = C?(dontDiagnoseOnThis: ())!
  _ = c; _ = c1
}

class F {
  var c: C?
  func makeC() -> C { return C() }
}

func testDontDiagnoseThroughMembers() {
  weak var c1 = F().c
  weak var c2 = F().makeC()
  _ = c1; _ = c2
}

func testDontDiagnoseOnStrongVariable() {
  var c1 = C()
  c1 = C()
  _ = c1
}

func testDontDiagnoseThroughImmediatelyEvaluatedClosure() {
  weak var c1 = { C() }()
  unowned let c2 = { C() }()
  _ = c1; _ = c2
}


