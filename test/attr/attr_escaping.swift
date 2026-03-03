// RUN: %target-typecheck-verify-swift -swift-version 4

@escaping var fn : () -> Int = { 4 }  // expected-error {{attribute can only be applied to types, not declarations}}
func paramDeclEscaping(@escaping fn: (Int) -> Void) {} // expected-error {{attribute can only be applied to types, not declarations}}

func wrongParamType(a: @escaping Int) {} // expected-error {{'@escaping' only applies to function types}}

func conflictingAttrs(_ fn: @noescape @escaping () -> Int) {} // expected-error {{unknown attribute 'noescape'}}

func takesEscaping(_ fn: @escaping () -> Int) {} // ok

func callEscapingWithNoEscape(_ fn: () -> Int) {
  // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{37-37=@escaping }}

  takesEscaping(fn) // expected-error{{passing non-escaping parameter 'fn' to function expecting an '@escaping' closure}}

  // This is a non-escaping use:
  let _ = fn
}

typealias IntSugar = Int
func callSugared(_ fn: () -> IntSugar) {
  // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{24-24=@escaping }}

  takesEscaping(fn) // expected-error{{passing non-escaping parameter 'fn' to function expecting an '@escaping' closure}}
}

struct StoresClosure {
  var closure : () -> Int
  init(_ fn: () -> Int) {
    // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{14-14=@escaping }}

    closure = fn // expected-error{{assigning non-escaping parameter 'fn' to an '@escaping' closure}}
  }

  func arrayPack(_ fn: () -> Int) -> [() -> Int] {
    // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{24-24=@escaping }}

    return [fn] // expected-error{{using non-escaping parameter 'fn' in a context expecting an '@escaping' closure}}
  }

  func dictPack(_ fn: () -> Int) -> [String: () -> Int] {
    // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{23-23=@escaping }}
    return ["ultimate answer": fn] // expected-error{{using non-escaping parameter 'fn' in a context expecting an '@escaping' closure}}
  }

  func autoclosureDictPack(_ fn: @autoclosure () -> Int) -> [String: () -> Int] {
    // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{46-46= @escaping}}
    return ["ultimate answer": fn] // expected-error{{using non-escaping parameter 'fn' in a context expecting an '@escaping' closure}}
  }

  func arrayPack(_ fn: @escaping () -> Int, _ fn2 : () -> Int) -> [() -> Int] {
    // expected-note@-1{{parameter 'fn2' is implicitly non-escaping}} {{53-53=@escaping }}

    return [fn, fn2] // expected-error{{using non-escaping parameter 'fn2' in a context expecting an '@escaping' closure}}
  }
}

func takesEscapingBlock(_ fn: @escaping @convention(block) () -> Void) {
  fn()
}
func callEscapingWithNoEscapeBlock(_ fn: () -> Void) {
  // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{42-42=@escaping }}

  takesEscapingBlock(fn) // expected-error{{passing non-escaping parameter 'fn' to function expecting an '@escaping' closure}}
}

func takesEscapingAutoclosure(_ fn: @autoclosure @escaping () -> Int) {}

func callEscapingAutoclosureWithNoEscape(_ fn: () -> Int) {
  takesEscapingAutoclosure(1+1)
}

let foo: @escaping (Int) -> Int // expected-error{{'@escaping' may only be used in function parameter position}} {{10-20=}}

struct GenericStruct<T> {}

func misuseEscaping(_ a: @escaping Int) {} // expected-error{{'@escaping' only applies to function types}} {{26-36=}}
func misuseEscaping(_ a: (@escaping Int)?) {} // expected-error{{'@escaping' only applies to function types}} {{27-36=}}
func misuseEscaping(opt a: @escaping ((Int) -> Int)?) {} // expected-error{{closure is already escaping in optional type argument}} {{28-38=}}

func misuseEscaping(_ a: (@escaping (Int) -> Int)?) {} // expected-error{{closure is already escaping in optional type argument}} {{27-36=}}
func misuseEscaping(nest a: (((@escaping (Int) -> Int))?)) {} // expected-error{{closure is already escaping in optional type argument}} {{32-41=}}
func misuseEscaping(iuo a: (@escaping (Int) -> Int)!) {} // expected-error{{closure is already escaping in optional type argument}} {{29-38=}}

func misuseEscaping(_ a: Optional<@escaping (Int) -> Int>, _ b: Int) {} // expected-error{{'@escaping' may only be used in function parameter position}} {{35-44=}}
func misuseEscaping(_ a: (@escaping (Int) -> Int, Int)) {} // expected-error{{'@escaping' may only be used in function parameter position}} {{27-36=}}
func misuseEscaping(_ a: [@escaping (Int) -> Int]) {} // expected-error{{'@escaping' may only be used in function parameter position}} {{27-36=}}
func misuseEscaping(_ a: [@escaping (Int) -> Int]?) {} // expected-error{{'@escaping' may only be used in function parameter position}} {{27-36=}}
func misuseEscaping(_ a: [Int : @escaping (Int) -> Int]) {} // expected-error{{'@escaping' may only be used in function parameter position}} {{33-43=}}
func misuseEscaping(_ a: GenericStruct<@escaping (Int) -> Int>) {} // expected-error{{'@escaping' may only be used in function parameter position}} {{40-49=}}

func takesEscapingGeneric<T>(_ fn: @escaping () -> T) {}
func callEscapingGeneric<T>(_ fn: () -> T) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{35-35=@escaping }}
  takesEscapingGeneric(fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an '@escaping' closure}}
}


class Super {}
class Sub: Super {}

func takesEscapingSuper(_ fn: @escaping () -> Super) {}
func callEscapingSuper(_ fn: () -> Sub) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{30-30=@escaping }}
  takesEscapingSuper(fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an '@escaping' closure}}
}

func takesEscapingSuperGeneric<T: Super>(_ fn: @escaping () -> T) {}
func callEscapingSuperGeneric(_ fn: () -> Sub) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{37-37=@escaping }}
  takesEscapingSuperGeneric(fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an '@escaping' closure}}
}
func callEscapingSuperGeneric<T: Sub>(_ fn: () -> T) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{45-45=@escaping }}
  takesEscapingSuperGeneric(fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an '@escaping' closure}}
}

func testModuloOptionalness() {
  var iuoClosure: (() -> Void)! = nil
  func setIUOClosure(_ fn: () -> Void) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{28-28=@escaping }}
    iuoClosure = fn // expected-error{{assigning non-escaping parameter 'fn' to an '@escaping' closure}}
  }
  var iuoClosureExplicit: (() -> Void)!
  func setExplicitIUOClosure(_ fn: () -> Void) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{36-36=@escaping }}
    iuoClosureExplicit = fn // expected-error{{assigning non-escaping parameter 'fn' to an '@escaping' closure}}
  }
  var deepOptionalClosure: (() -> Void)???
  func setDeepOptionalClosure(_ fn: () -> Void) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{37-37=@escaping }}
    deepOptionalClosure = fn // expected-error{{assigning non-escaping parameter 'fn' to an '@escaping' closure}}
  }
}

// Check that functions in vararg position are @escaping
func takesEscapingFunction(fn: @escaping () -> ()) {}
func takesArrayOfFunctions(array: [() -> ()]) {}

func takesVarargsOfFunctions(fns: () -> ()...) {
  takesArrayOfFunctions(array: fns)
  for fn in fns {
    takesEscapingFunction(fn: fn)
  }
}

func takesVarargsOfFunctionsExplicitEscaping(fns: @escaping () -> ()...) {} // expected-error{{'@escaping' may only be used in function parameter position}}

func takesNoEscapeFunction(fn: () -> ()) { // expected-note {{parameter 'fn' is implicitly non-escaping}}
  takesVarargsOfFunctions(fns: fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an '@escaping' closure}}
}


class FooClass {
  var stored : Optional<(()->Int)->Void> = nil  // expected-note {{add explicit '@escaping' to function parameter #0}} {{26-26=@escaping }}
  var computed : (()->Int)->Void {
    get { return stored! }
    set(newValue) { stored = newValue } // ok
  }
  var computedEscaping : (@escaping ()->Int)->Void {
    get { return stored! }
    set(newValue) { stored = newValue } // expected-error{{cannot assign value of type '(@escaping () -> Int) -> Void' to type '(() -> Int) -> Void'}}
    // expected-note@-1{{parameter #0 expects escaping value of type '() -> Int'}}
  }
}

// A call of a closure literal should be non-escaping
func takesInOut(y: inout Int) {
  _ = {
    y += 1 // no-error
  }()

  _ = ({
    y += 1 // no-error
  })()

  _ = { () in
    y += 1 // no-error
  }()

  _ = ({ () in
    y += 1 // no-error
  })()

  _ = { () -> () in
    y += 1 // no-error
  }()

  _ = ({ () -> () in
    y += 1 // no-error
  })()
}

class HasIVarCaptures {
  var x: Int = 0

  func method() {
    _ = {
      x += 1 // no-error
    }()

    _ = ({
      x += 1 // no-error
    })()

    _ = { () in
      x += 1 // no-error
    }()

    _ = ({ () in
      x += 1 // no-error
    })()

    _ = { () -> () in
      x += 1 // no-error
    }()

    _ = ({ () -> () in
      x += 1 // no-error
    })()
  }
}

// https://github.com/apple/swift/issues/52188

protocol P_52188 {
  typealias F = () -> Void
  typealias G<T> = (T) -> Void
  func foo<T>(_: T, _: @escaping F) // Ok
  func bar<T>(_: @escaping G<T>) // Ok
}

extension P_52188 {
  func fiz<T>(_: T, _: @escaping F) {} // Ok
  func baz<T>(_: @escaping G<T>) {} // Ok
}

// https://github.com/apple/swift/issues/51669

func f_51669<T>(_ x: @escaping T) {} // expected-error 1{{'@escaping' only applies to function types}}

// https://github.com/apple/swift/issues/57070

var global: ((() -> Void) -> Void)? = nil
// expected-note@-1 {{add explicit '@escaping' to function parameter #0}} {{15-15=@escaping }}

class C_57070 {
  let ok: (@escaping () -> Void) -> Void // OK
  let callback: (() -> Void) -> Void // expected-note {{add explicit '@escaping' to function parameter #0}} {{18-18=@escaping }}
  let callback1: (() -> Void, () -> Void) -> Void // expected-note {{add explicit '@escaping' to function parameter #1}} {{31-31=@escaping }} 
  let callbackAuto: (@autoclosure () -> Void) -> Void // expected-note {{add explicit '@escaping' to function parameter #0}} {{34-34= @escaping}}
  let callbackOpt: ((() -> Void) -> Void)? // expected-note{{add explicit '@escaping' to function parameter #0}} {{22-22=@escaping }}

  init(f: @escaping (@escaping() -> Void) -> Void) {
    self.callback = f // expected-error{{cannot assign value of type '(@escaping () -> Void) -> Void' to type '(() -> Void) -> Void'}}
    // expected-note@-1{{parameter #0 expects escaping value of type '() -> Void'}}
    self.ok = f // Ok
  }

  init(af: @escaping (@escaping() -> Void) -> Void) {
    self.callbackOpt = af // expected-error{{cannot assign value of type '(@escaping () -> Void) -> Void' to type '(() -> Void) -> Void'}}
    // expected-note@-1{{parameter #0 expects escaping value of type '() -> Void'}}  
  }

  init(ag: @escaping (@escaping() -> Void) -> Void) {
    global = ag // expected-error{{cannot assign value of type '(@escaping () -> Void) -> Void' to type '(() -> Void) -> Void'}}
    // expected-note@-1{{parameter #0 expects escaping value of type '() -> Void'}}
  }

  init(a: @escaping (@escaping () -> Void) -> Void) {
    self.callbackAuto = a // expected-error{{cannot assign value of type '(@escaping () -> Void) -> Void' to type '(@autoclosure () -> Void) -> Void'}}
    // expected-note@-1{{parameter #0 expects escaping value of type '() -> Void'}}
  }

  init(f: @escaping (() -> Void, @escaping() -> Void) -> Void) {
    self.callback1 = f // expected-error{{cannot assign value of type '(() -> Void, @escaping () -> Void) -> Void' to type '(() -> Void, () -> Void) -> Void'}}
    // expected-note@-1{{parameter #1 expects escaping value of type '() -> Void'}}
  }
}
