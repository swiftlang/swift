// RUN: %target-swift-frontend -typecheck -verify -module-name main %s
func global() -> Int { return 0 }

struct S {
  static func staticMethod() -> Int { return 0 }
}

class C {
  static func staticMethod() -> Int { return 0 }
  class func classMethod() -> Int { return 0 }
}

if true {
  func local() -> Int { return 0 }

  let a: @convention(c) () -> Int = global
  let _: @convention(c) () -> Int = main.global
  let _: @convention(c) () -> Int = { 0 }
  let _: @convention(c) () -> Int = local

  // Can't convert a closure value to a C function pointer
  let global2 = global
  let _: @convention(c) () -> Int = global2 // expected-error{{can only be formed from a reference to a 'func' or a literal closure}}
  let globalBlock: @convention(block) () -> Int = global
  let _: @convention(c) () -> Int = globalBlock // expected-error{{can only be formed from a reference to a 'func' or a literal closure}}

  // Can convert a function pointer to a block or closure, or assign to another
  // C function pointer
  let _: @convention(c) () -> Int = a
  let _: @convention(block) () -> Int = a
  let _: () -> Int = a

  // Can't convert a C function pointer from a method.
  // TODO: Could handle static methods.
  let _: @convention(c) () -> Int = S.staticMethod // expected-error{{}}
  let _: @convention(c) () -> Int = C.staticMethod // expected-error{{}}
  let _: @convention(c) () -> Int = C.classMethod // expected-error{{}}

  // <rdar://problem/22181714> Crash when typing "signal"
  let iuo_global: (() -> Int)! = global
  let _: (@convention(c) () -> Int)! = iuo_global // expected-error{{a C function pointer can only be formed from a reference to a 'func' or a literal closure}}

  func handler(_ callback: (@convention(c) () -> Int)!) {}
  handler(iuo_global) // expected-error{{a C function pointer can only be formed from a reference to a 'func' or a literal closure}}
}

func genericFunc<T>(_ t: T) -> T { return t }

let f: @convention(c) (Int) -> Int = genericFunc // expected-error{{cannot be formed from a reference to a generic function}}

func ct1() -> () { print("") }

let ct1ref0 : @convention(c, cType: "void *(void)") () -> () = ct1
let ct1ref1 : @convention(c, cType: "void *(void)") = ct1 // expected-error{{expected type}}
let ct1ref2 : @convention(c, ) () -> () = ct1 // expected-error{{expected 'cType' label in 'convention' attribute}}
let ct1ref3 : @convention(c, cType) () -> () = ct1 // expected-error{{expected ':' after 'cType' for 'convention' attribute}}
let ct1ref4 : @convention(c, cType: ) () -> () = ct1 // expected-error{{expected string literal containing clang type for 'cType' in 'convention' attribute}}
