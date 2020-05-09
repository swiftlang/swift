// RUN: %target-typecheck-verify-swift -swift-version 4

let x: Int = 1
let y: Int = x.self
let int: Int.Type = Int.self


// SE-0071 - Allow (most) keywords in member references
// https://github.com/apple/swift-evolution/blob/master/proposals/0071-member-keywords.md

struct SE0071Struct {
  var `default` : Int
}

func f1(a : SE0071Struct) -> Int {
  return a.default
}

func f2(a : SE0071Struct) -> Int {
  return a.`default`
}

  
enum SE0071Enum {
  case `case`
}

func f2() -> SE0071Enum {
  return .case
}

class SE0071Base {
  func `default`() {}
}

class SE0071Derived : SE0071Base {
  func zonk() {
    super.default()
  }
}

// SR-3043: Diagnostics when accessing deinit

class SR3043Base {
}

class SR3043Derived: SR3043Base {
  deinit {
    super.deinit() // expected-error {{deinitializers cannot be accessed}}
  }
}

let sr3043 = SR3043Derived()
sr3043.deinit() // expected-error {{deinitializers cannot be accessed}}
sr3043.deinit // expected-error {{deinitializers cannot be accessed}}
SR3043Derived.deinit() // expected-error {{deinitializers cannot be accessed}}

// Allow deinit functions in classes

class ClassWithDeinitFunc {
  func `deinit`() {
  }

  func `deinit`(a: SR3043Base) {
  }
}

let instanceWithDeinitFunc = ClassWithDeinitFunc()
instanceWithDeinitFunc.deinit()
_ = instanceWithDeinitFunc.deinit(a:)
_ = instanceWithDeinitFunc.deinit as () -> Void
SR3043Derived.deinit() // expected-error {{deinitializers cannot be accessed}}

class ClassWithDeinitMember {
  var `deinit`: SR3043Base?
}

let instanceWithDeinitMember = ClassWithDeinitMember()
_ = instanceWithDeinitMember.deinit


// SR-5715 : Fix variable name in nested static value
struct SR5715 {
  struct A {
    struct B {}
  }
}

extension SR5715.A.B {
  private static let x: Int = 5
    
  func f() -> Int {
    return x  // expected-error {{static member 'x' cannot be used on instance of type 'SR5715.A.B'}} {{12-12=SR5715.A.B.}}
  }
}

// Static function in protocol should have `Self.` instead of its protocol name
protocol P {}

extension P {
  static func f() {}

  func g() {
    f() // expected-error {{static member 'f' cannot be used on instance of type 'Self'}} {{5-5=Self.}}
  }
}

