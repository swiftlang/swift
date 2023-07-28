// RUN: %target-swift-frontend -enable-experimental-feature NonCopyableOptional -typecheck -verify %s

struct Butt: ~Copyable {
    var value: Int
}

extension Optional {
    func stillRequiresCopyable() {}
}

func stillRequiresCopyableGeneric<T>(_: Optional<T>) {} // expected-note* {{}}

extension Optional where Wrapped == Butt {
    func buttSpecific() {}
}
extension Optional<Butt> {
    func buttSpecific2() {}
}

func canPassToNongenericOptional(_: borrowing Optional<Butt>) {}

func requiresCopyable<T>(_: T) {} // expected-note* {{}}
func requiresAny(_: Any) {}

func forgotModifierOnOptionalNoncopyable(
  x: Butt?, // expected-error{{noncopyable parameter must specify its ownership}} expected-note * {{}}
  y: Optional<Butt>, // expected-error{{noncopyable parameter must specify its ownership}} expected-note * {{}}
  z: Butt!, // expected-error{{noncopyable parameter must specify its ownership}} expected-note * {{}}
  w: Butt?? // expected-error{{noncopyable parameter must specify its ownership}} expected-note * {{}}
)
{}

func foo(x: consuming Optional<Butt>,
         y: consuming Butt?,
         z: consuming Optional<Butt>,
         w: consuming Butt!) { // OK to instantiate Optional with a noncopyable type
    _ = x! // OK to use optional operators
    _ = x!.value
    _ = x?.value

    // TODO: special ?? operator
    //_ = x ?? y

    // OK to match with `if let`
    if let d = consume y { }
    guard let e = consume z else { }

    // OK to match with `switch`
    switch consume w {
    case .some(let f):
        break

    case .none:
        break
    }

    // Can't call generic methods on Optional or functions that take
    // generic Optional
    x.stillRequiresCopyable() // expected-error{{noncopyable type 'Butt' cannot be substituted}}
    stillRequiresCopyableGeneric(x) // expected-error{{noncopyable type 'Butt' cannot be substituted}}

    // Can call functions constrained to specific type
    canPassToNongenericOptional(x)

    // TODO: can call extension methods applied specifically to Optional<Noncopyable>
    //x.buttSpecific()
    //x.buttSpecific2()

    // TODO: Can use the blessed `take()` method
    // let g = x.take()

    // Can't pass Optional<MoveOnly> as a generic parameter or existential
    requiresCopyable(x) // expected-error{{noncopyable type 'Optional<Butt>' cannot be substituted}}
    requiresAny(x) // expected-error{{noncopyable type 'Optional<Butt>' cannot be converted}}

    // TODO: Can infer a noncopyable type for Optional's generic parameter
    //let g: Optional = x
    //let h: Optional = y!
}
