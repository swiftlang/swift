// RUN: %target-swift-frontend -parse -verify -module-name main -enable-c-function-pointers %s
func global() -> Int { return 0 }

struct S {
  static func staticMethod() -> Int { return 0 }
}

class C {
  static func staticMethod() -> Int { return 0 }
  class func classMethod() -> Int { return 0 }
}

if true {
  var x = 0
  func local() -> Int { return 0 }
  func localWithContext() -> Int { return x }

  let a: @convention(c) () -> Int = global
  let a2: @convention(c) () -> Int = main.global
  let b: @convention(c) () -> Int = { 0 }
  let c: @convention(c) () -> Int = local

  // Can't convert a closure with context to a C function pointer
  let d: @convention(c) () -> Int = { x } // expected-error{{cannot be formed from a closure that captures context}}
  let e: @convention(c) () -> Int = localWithContext // expected-error{{cannot be formed from a local function that captures context}}

  // Can't convert a closure value to a C function pointer
  let global2 = global
  let f: @convention(c) () -> Int = global2 // expected-error{{can only be formed from a reference to a 'func' or a literal closure}}
  let globalBlock: @objc_block () -> Int = global
  let g: @convention(c) () -> Int = globalBlock // expected-error{{can only be formed from a reference to a 'func' or a literal closure}}

  // Can convert a function pointer to a block or closure, or assign to another
  // C function pointer
  let h: @convention(c) () -> Int = a
  let i: @objc_block () -> Int = a
  let j: () -> Int = a

  // Can't convert a C function pointer from a method.
  // TODO: Could handle static methods.
  let k: @convention(c) () -> Int = S.staticMethod // expected-error{{}}
  let m: @convention(c) () -> Int = C.staticMethod // expected-error{{}}
  let n: @convention(c) () -> Int = C.classMethod // expected-error{{}}
}
