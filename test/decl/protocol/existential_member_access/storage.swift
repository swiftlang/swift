// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

// rdar://121214563
// https://github.com/apple/swift/issues/62219

// nonmutating get mutating set

do {
  protocol P {
    associatedtype A: P

    var getOnlyProp: A { get set }
    subscript(getOnlySubscript _: Void) -> A { get set }

    var setOnlyProp: (A) -> Void { get set }
    subscript(setOnlySubscript _: Void) -> (A) -> Void { get set }
  }

  let p: any P
  let pClosure: (any P) -> Void

  do {
    let rvalueP: any P // expected-note 2 {{change 'let' to 'var' to make it mutable}}

    let _ = rvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'rvalueP' is a 'let' constant}}
    rvalueP.getOnlyProp = p
    let _ = rvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'rvalueP' is a 'let' constant}}
    rvalueP[getOnlySubscript: ()] = p

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = rvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    rvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = rvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    rvalueP[setOnlySubscript: ()] = pClosure
  }

  func takesInout(_: inout any P) {}

  do {
    var lvalueP: any P

    let _ = lvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'lvalueP' is immutable}}
    lvalueP.getOnlyProp = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP.getOnlyProp)
    let _ = lvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'lvalueP' is immutable}}
    lvalueP[getOnlySubscript: ()] = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP[getOnlySubscript: ()])

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = lvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    lvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = lvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    lvalueP[setOnlySubscript: ()] = pClosure
  }
}

// static

do {
  protocol P {
    associatedtype A: P

    static var getOnlyProp: A { get set }
    static subscript(getOnlySubscript _: Void) -> A { get set }

    static var setOnlyProp: (A) -> Void { get set }
    static subscript(setOnlySubscript _: Void) -> (A) -> Void { get set }
  }

  let p: any P
  let pClosure: (any P) -> Void

  do {
    let rvalueP: any P.Type // expected-note 2 {{change 'let' to 'var' to make it mutable}}

    let _ = rvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'rvalueP' is a 'let' constant}}
    rvalueP.getOnlyProp = p
    let _ = rvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'rvalueP' is a 'let' constant}}
    rvalueP[getOnlySubscript: ()] = p

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}}
    let _ = rvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}}
    rvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}}
    let _ = rvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}}
    rvalueP[setOnlySubscript: ()] = pClosure
  }

  func takesInout(_: inout any P) {}

  do {
    var lvalueP: any P.Type

    let _ = lvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'lvalueP' is immutable}}
    lvalueP.getOnlyProp = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP.getOnlyProp)
    let _ = lvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'lvalueP' is immutable}}
    lvalueP[getOnlySubscript: ()] = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP[getOnlySubscript: ()])

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}}
    let _ = lvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}}
    lvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}}
    let _ = lvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}}
    lvalueP[setOnlySubscript: ()] = pClosure
  }
}

// mutating get mutating set

do {
  protocol P {
    associatedtype A: P

    var getOnlyProp: A { mutating get set }
    subscript(getOnlySubscript _: Void) -> A { mutating get set }

    var setOnlyProp: (A) -> Void { mutating get set }
    subscript(setOnlySubscript _: Void) -> (A) -> Void { mutating get set }
  }

  let p: any P
  let pClosure: (any P) -> Void

  do {
    let rvalueP: any P // expected-note 4 {{change 'let' to 'var' to make it mutable}}

    // expected-error@+1 {{cannot use mutating getter on immutable value: 'rvalueP' is a 'let' constant}}
    let _ = rvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'rvalueP' is a 'let' constant}}
    rvalueP.getOnlyProp = p
    // expected-error@+1 {{cannot use mutating getter on immutable value: 'rvalueP' is a 'let' constant}}
    let _ = rvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'rvalueP' is a 'let' constant}}
    rvalueP[getOnlySubscript: ()] = p

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = rvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    rvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = rvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    rvalueP[setOnlySubscript: ()] = pClosure
  }

  func takesInout(_: inout any P) {}

  do {
    var lvalueP: any P

    let _ = lvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'lvalueP' is immutable}}
    lvalueP.getOnlyProp = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP.getOnlyProp)
    let _ = lvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'lvalueP' is immutable}}
    lvalueP[getOnlySubscript: ()] = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP[getOnlySubscript: ()])

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = lvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    lvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = lvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    lvalueP[setOnlySubscript: ()] = pClosure
  }
}

// nonmutating get nonmutating set

do {
  protocol P {
    associatedtype A: P

    var getOnlyProp: A { get nonmutating set }
    subscript(getOnlySubscript _: Void) -> A { get nonmutating set }

    var setOnlyProp: (A) -> Void { get nonmutating set }
    subscript(setOnlySubscript _: Void) -> (A) -> Void { get nonmutating set }
  }

  let p: any P
  let pClosure: (any P) -> Void

  do {
    let rvalueP: any P // expected-note 2 {{change 'let' to 'var' to make it mutable}}

    let _ = rvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'rvalueP' is a 'let' constant}}
    rvalueP.getOnlyProp = p
    let _ = rvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'rvalueP' is a 'let' constant}}
    rvalueP[getOnlySubscript: ()] = p

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = rvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    rvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = rvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    rvalueP[setOnlySubscript: ()] = pClosure
  }

  func takesInout(_: inout any P) {}

  do {
    var lvalueP: any P

    let _ = lvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'lvalueP' is immutable}}
    lvalueP.getOnlyProp = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP.getOnlyProp)
    let _ = lvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'lvalueP' is immutable}}
    lvalueP[getOnlySubscript: ()] = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP[getOnlySubscript: ()])

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = lvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    lvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = lvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    lvalueP[setOnlySubscript: ()] = pClosure
  }
}

// mutating get nonmutating set

do {
  protocol P {
    associatedtype A: P

    var getOnlyProp: A { mutating get nonmutating set }
    subscript(getOnlySubscript _: Void) -> A { mutating get nonmutating set }

    var setOnlyProp: (A) -> Void { mutating get nonmutating set }
    subscript(setOnlySubscript _: Void) -> (A) -> Void { mutating get nonmutating set }
  }

  let p: any P
  let pClosure: (any P) -> Void

  do {
    let rvalueP: any P // expected-note 4 {{change 'let' to 'var' to make it mutable}}

    // expected-error@+1 {{cannot use mutating getter on immutable value: 'rvalueP' is a 'let' constant}}
    let _ = rvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'rvalueP' is a 'let' constant}}
    rvalueP.getOnlyProp = p
    // expected-error@+1 {{cannot use mutating getter on immutable value: 'rvalueP' is a 'let' constant}}
    let _ = rvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'rvalueP' is a 'let' constant}}
    rvalueP[getOnlySubscript: ()] = p

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = rvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    rvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = rvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    rvalueP[setOnlySubscript: ()] = pClosure
  }

  func takesInout(_: inout any P) {}

  do {
    var lvalueP: any P

    let _ = lvalueP.getOnlyProp
    // expected-error@+1 {{cannot assign to property: 'lvalueP' is immutable}}
    lvalueP.getOnlyProp = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP.getOnlyProp)
    let _ = lvalueP[getOnlySubscript: ()]
    // expected-error@+1 {{cannot assign through subscript: 'lvalueP' is immutable}}
    lvalueP[getOnlySubscript: ()] = p
    // expected-error@+1 {{cannot pass immutable value as inout argument: 'lvalueP' is immutable}}
    takesInout(&lvalueP[getOnlySubscript: ()])

    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = lvalueP.setOnlyProp
    // expected-error@+1 {{member 'setOnlyProp' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    lvalueP.setOnlyProp = pClosure
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    let _ = lvalueP[setOnlySubscript: ()]
    // expected-error@+1 {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
    lvalueP[setOnlySubscript: ()] = pClosure
  }
}
