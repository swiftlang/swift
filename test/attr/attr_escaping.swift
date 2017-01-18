// RUN: %target-typecheck-verify-swift -swift-version 4

@escaping var fn : () -> Int = { 4 }  // expected-error {{attribute can only be applied to types, not declarations}}
func paramDeclEscaping(@escaping fn: (Int) -> Void) {} // expected-error {{attribute can only be applied to types, not declarations}}

func wrongParamType(a: @escaping Int) {} // expected-error {{@escaping attribute only applies to function types}}

func conflictingAttrs(_ fn: @noescape @escaping () -> Int) {} // expected-error {{@escaping conflicts with @noescape}}
 // expected-warning@-1{{@noescape is the default and is deprecated}} {{29-39=}}

func takesEscaping(_ fn: @escaping () -> Int) {} // ok

func callEscapingWithNoEscape(_ fn: () -> Int) {
  // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{37-37=@escaping }}
  // expected-note@-2{{parameter 'fn' is implicitly non-escaping}} {{37-37=@escaping }}

  takesEscaping(fn) // expected-error{{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
  let _ = fn // expected-error{{non-escaping parameter 'fn' may only be called}}
}

typealias IntSugar = Int
func callSugared(_ fn: () -> IntSugar) {
  // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{24-24=@escaping }}

  takesEscaping(fn) // expected-error{{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
}

struct StoresClosure {
  var closure : () -> Int
  init(_ fn: () -> Int) {
    // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{14-14=@escaping }}

    closure = fn // expected-error{{assigning non-escaping parameter 'fn' to an @escaping closure}}
  }

  func arrayPack(_ fn: () -> Int) -> [() -> Int] {
    // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{24-24=@escaping }}

    return [fn] // expected-error{{using non-escaping parameter 'fn' in a context expecting an @escaping closure}}
  }

  func arrayPack(_ fn: @escaping () -> Int, _ fn2 : () -> Int) -> [() -> Int] {
    // expected-note@-1{{parameter 'fn2' is implicitly non-escaping}} {{53-53=@escaping }}

    return [fn, fn2] // expected-error{{using non-escaping parameter 'fn2' in a context expecting an @escaping closure}}
  }
}

func takesEscapingBlock(_ fn: @escaping @convention(block) () -> Void) {
  fn()
}
func callEscapingWithNoEscapeBlock(_ fn: () -> Void) {
  // expected-note@-1{{parameter 'fn' is implicitly non-escaping}} {{42-42=@escaping }}

  takesEscapingBlock(fn) // expected-error{{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
}

func takesEscapingAutoclosure(_ fn: @autoclosure @escaping () -> Int) {}

func callEscapingAutoclosureWithNoEscape(_ fn: () -> Int) {
  takesEscapingAutoclosure(1+1)
}
func callEscapingAutoclosureWithNoEscape_2(_ fn: () -> Int) {
  // expected-note@-1{{parameter 'fn' is implicitly non-escaping}}

  takesEscapingAutoclosure(fn()) // expected-error{{closure use of non-escaping parameter 'fn' may allow it to escape}}
}
func callEscapingAutoclosureWithNoEscape_3(_ fn: @autoclosure () -> Int) {
  // expected-note@-1{{parameter 'fn' is implicitly non-escaping}}

  takesEscapingAutoclosure(fn()) // expected-error{{closure use of non-escaping parameter 'fn' may allow it to escape}}
}

let foo: @escaping (Int) -> Int // expected-error{{@escaping attribute may only be used in function parameter position}} {{10-20=}}

struct GenericStruct<T> {}

func misuseEscaping(_ a: @escaping Int) {} // expected-error{{@escaping attribute only applies to function types}}
func misuseEscaping(_ a: (@escaping Int)?) {} // expected-error{{@escaping attribute only applies to function types}}

func misuseEscaping(_ a: (@escaping (Int) -> Int)?) {} // expected-error{{@escaping attribute may only be used in function parameter position}} {{27-37=}}
  // expected-note@-1{{closure is already escaping in optional type argument}}
func misuseEscaping(nest a: (((@escaping (Int) -> Int))?)) {} // expected-error{{@escaping attribute may only be used in function parameter position}} {{32-42=}}
  // expected-note@-1{{closure is already escaping in optional type argument}}
func misuseEscaping(iuo a: (@escaping (Int) -> Int)!) {} // expected-error{{@escaping attribute may only be used in function parameter position}} {{29-39=}}
  // expected-note@-1{{closure is already escaping in optional type argument}}

func misuseEscaping(_ a: Optional<@escaping (Int) -> Int>, _ b: Int) {} // expected-error{{@escaping attribute may only be used in function parameter position}} {{35-45=}}
func misuseEscaping(_ a: (@escaping (Int) -> Int, Int)) {} // expected-error{{@escaping attribute may only be used in function parameter position}} {{27-37=}}
func misuseEscaping(_ a: [@escaping (Int) -> Int]) {} // expected-error{{@escaping attribute may only be used in function parameter position}} {{27-37=}}
func misuseEscaping(_ a: [@escaping (Int) -> Int]?) {} // expected-error{{@escaping attribute may only be used in function parameter position}} {{27-37=}}
func misuseEscaping(_ a: [Int : @escaping (Int) -> Int]) {} // expected-error{{@escaping attribute may only be used in function parameter position}} {{33-43=}}
func misuseEscaping(_ a: GenericStruct<@escaping (Int) -> Int>) {} // expected-error{{@escaping attribute may only be used in function parameter position}} {{40-50=}}

func takesEscapingGeneric<T>(_ fn: @escaping () -> T) {}
func callEscapingGeneric<T>(_ fn: () -> T) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{35-35=@escaping }}
  takesEscapingGeneric(fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
}


class Super {}
class Sub: Super {}

func takesEscapingSuper(_ fn: @escaping () -> Super) {}
func callEscapingSuper(_ fn: () -> Sub) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{30-30=@escaping }}
  takesEscapingSuper(fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
}

func takesEscapingSuperGeneric<T: Super>(_ fn: @escaping () -> T) {}
func callEscapingSuperGeneric(_ fn: () -> Sub) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{37-37=@escaping }}
  takesEscapingSuperGeneric(fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
}
func callEscapingSuperGeneric<T: Sub>(_ fn: () -> T) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{45-45=@escaping }}
  takesEscapingSuperGeneric(fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
}

func testModuloOptionalness() {
  var iuoClosure: (() -> Void)! = nil
  func setIUOClosure(_ fn: () -> Void) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{28-28=@escaping }}
    iuoClosure = fn // expected-error{{assigning non-escaping parameter 'fn' to an @escaping closure}}
  }
  var iuoClosureExplicit: ImplicitlyUnwrappedOptional<() -> Void>
  func setExplicitIUOClosure(_ fn: () -> Void) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{36-36=@escaping }}
    iuoClosureExplicit = fn // expected-error{{assigning non-escaping parameter 'fn' to an @escaping closure}}
  }
  var deepOptionalClosure: (() -> Void)???
  func setDeepOptionalClosure(_ fn: () -> Void) { // expected-note {{parameter 'fn' is implicitly non-escaping}} {{37-37=@escaping }}
    deepOptionalClosure = fn // expected-error{{assigning non-escaping parameter 'fn' to an @escaping closure}}
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

func takesVarargsOfFunctionsExplicitEscaping(fns: @escaping () -> ()...) {} // expected-error{{@escaping attribute may only be used in function parameter position}}

func takesNoEscapeFunction(fn: () -> ()) { // expected-note {{parameter 'fn' is implicitly non-escaping}}
  takesVarargsOfFunctions(fns: fn) // expected-error {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
}


class FooClass {
  var stored : Optional<(()->Int)->Void> = nil
  var computed : (()->Int)->Void {
    get { return stored! }
    set(newValue) { stored = newValue } // ok
  }
  var computedEscaping : (@escaping ()->Int)->Void {
    get { return stored! }
    set(newValue) { stored = newValue } // expected-error{{cannot assign value of type '(@escaping () -> Int) -> Void' to type 'Optional<(() -> Int) -> Void>' (aka 'Optional<(() -> Int) -> ()>')}}
  }
}
