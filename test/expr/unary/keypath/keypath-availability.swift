// RUN: %target-swift-frontend -typecheck -verify %s
// REQUIRES: OS=macosx

struct Butt {
  var setter_conditionally_available: Int {
    get { fatalError() }

    @available(macOS 99, *)
    set { fatalError() }
  }

  var setter_unavailable_on_macos: Int {
    get { fatalError() }

    @available(macOS, unavailable)
    set { fatalError() }
  }

  var setter_universally_unavailable: Int {
    get { fatalError() }

    @available(*, unavailable)
    set { fatalError() }
  }
}

@dynamicMemberLookup
struct Lens<T> {
  var obj: T

  init(_ obj: T) {
    self.obj = obj
  }

  subscript<U>(dynamicMember member: KeyPath<T, U>) -> Lens<U> {
    get { return Lens<U>(obj[keyPath: member]) }
  }

  subscript<U>(dynamicMember member: WritableKeyPath<T, U>) -> Lens<U> {
    get { return Lens<U>(obj[keyPath: member]) }
    set { obj[keyPath: member] = newValue.obj }
  }
}

func assertExactType<T>(of _: inout T, is _: T.Type) {}

@available(macOS 10.9, *)
public func availableOnMacOS_10_9() {
  var kp = \Butt.setter_conditionally_available
  var lens = Lens(Butt())
  assertExactType(of: &kp, is: KeyPath<Butt, Int>.self)
  _ = lens.setter_conditionally_available
  lens.setter_conditionally_available = Lens(1) // expected-error {{cannot assign to property: 'lens' is immutable}}
}

@available(macOS 99, *)
public func availableOnMacOS_99() {
  var kp = \Butt.setter_conditionally_available
  var lens = Lens(Butt())
  assertExactType(of: &kp, is: WritableKeyPath<Butt, Int>.self)
  _ = lens.setter_conditionally_available
  lens.setter_conditionally_available = Lens(1)
}

public func alwaysAvailableOnMacOS() {
  var lens = Lens(Butt())

  if #available(macOS 99, *) {
    var kp = \Butt.setter_conditionally_available
    assertExactType(of: &kp, is: WritableKeyPath<Butt, Int>.self)
    _ = lens.setter_conditionally_available
    // FIXME: [availability] setter_conditionally_available should be writable in this branch
    lens.setter_conditionally_available = Lens(1) // expected-error {{cannot assign to property: 'lens' is immutable}}
  } else {
    var kp = \Butt.setter_conditionally_available
    assertExactType(of: &kp, is: KeyPath<Butt, Int>.self)
    _ = lens.setter_conditionally_available
    lens.setter_conditionally_available = Lens(1) // expected-error {{cannot assign to property: 'lens' is immutable}}
  }

  var kp2 = \Butt.setter_unavailable_on_macos
  assertExactType(of: &kp2, is: KeyPath<Butt, Int>.self)
  _ = lens.setter_unavailable_on_macos
  lens.setter_unavailable_on_macos = Lens(1) // expected-error {{cannot assign to property: 'lens' is immutable}}

  var kp3 = \Butt.setter_universally_unavailable
  assertExactType(of: &kp3, is: KeyPath<Butt, Int>.self)
  _ = lens.setter_universally_unavailable
  lens.setter_universally_unavailable = Lens(1) // expected-error {{cannot assign to property: 'lens' is immutable}}
}

@available(macOS, unavailable)
public func unvailableOnMacOS() {
  var lens = Lens(Butt())
  var kp = \Butt.setter_conditionally_available
  assertExactType(of: &kp, is: WritableKeyPath<Butt, Int>.self)
  _ = lens.setter_conditionally_available
  lens.setter_conditionally_available = Lens(1)

  var kp2 = \Butt.setter_unavailable_on_macos
  assertExactType(of: &kp2, is: WritableKeyPath<Butt, Int>.self)
  _ = lens.setter_unavailable_on_macos
  lens.setter_unavailable_on_macos = Lens(1)

  var kp3 = \Butt.setter_universally_unavailable
  assertExactType(of: &kp3, is: KeyPath<Butt, Int>.self)
  _ = lens.setter_universally_unavailable
  lens.setter_universally_unavailable = Lens(1) // expected-error {{cannot assign to property: 'lens' is immutable}}
}

@available(*, unavailable)
public func universallyUnavailable() {
  var lens = Lens(Butt())
  var kp = \Butt.setter_conditionally_available
  assertExactType(of: &kp, is: WritableKeyPath<Butt, Int>.self)
  _ = lens.setter_conditionally_available
  lens.setter_conditionally_available = Lens(1)

  var kp2 = \Butt.setter_unavailable_on_macos
  assertExactType(of: &kp2, is: WritableKeyPath<Butt, Int>.self)
  _ = lens.setter_unavailable_on_macos
  lens.setter_unavailable_on_macos = Lens(1)

  var kp3 = \Butt.setter_universally_unavailable
  assertExactType(of: &kp3, is: KeyPath<Butt, Int>.self)
  _ = lens.setter_universally_unavailable
  lens.setter_universally_unavailable = Lens(1)
}
