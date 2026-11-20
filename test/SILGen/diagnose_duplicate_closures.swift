// RUN: %target-swift-emit-silgen -verify -disable-ast-verifier -debug-duplicate-closure-discriminators %s > /dev/null

_ = {} // expected-note {{other definition here}}
_ = {} // expected-error {{multiple definitions of symbol '$s27diagnose_duplicate_closuresyycfU_'; please submit a bug report}}

func foo() {
  _ = {} // expected-note {{other definition here}}
  _ = {} // expected-error {{multiple definitions of symbol '$s27diagnose_duplicate_closures3fooyyFyycfU_'; please submit a bug report}}
}

func bar() {
  _ = {
    _ = {} // expected-note {{other definition here}}
    _ = {} // expected-error {{multiple definitions of symbol '$s27diagnose_duplicate_closures3baryyFyycfU_yycfU_'; please submit a bug report}}
  }
}

func captures(_ x: Int) {
  _ = {} // expected-note {{other definition here}}
  _ = { _ = x } // expected-error {{multiple definitions of symbol '$s27diagnose_duplicate_closures8capturesyySiFyycfU_'; please submit a bug report}}
}

func capturesGeneric<T>(_ t: T) {
  _ = {} // expected-note {{other definition here}}
  _ = { _ = t } // expected-error {{multiple definitions of symbol '$s27diagnose_duplicate_closures15capturesGenericyyxlFyycfU_'; please submit a bug report}}
}

func defaultArgs() {
  _ = { // expected-note {{other definition here}}
    struct S {
      @_silgen_name("s1")
      func m(x: Int = 0) {} // expected-note {{other definition here}}
    }
  }
  _ = { // expected-error {{multiple definitions of symbol '$s27diagnose_duplicate_closures11defaultArgsyyFyycfU_'; please submit a bug report}}
    struct S {
      @_silgen_name("s2")
      func m(x: Int = 0) {} // expected-error {{multiple definitions of symbol '$s27diagnose_duplicate_closures11defaultArgsyyFyycfU_1SL_V1m1xySi_tFfA_'; please submit a bug report}}
    }
  }
}

func immediatelyApplied(_ x: Int) {
  _ = {}() // expected-note {{other definition here}}
  _ = { _ = x }() // expected-error {{multiple definitions of symbol '$s27diagnose_duplicate_closures18immediatelyAppliedyySiFyyXEfU_'; please submit a bug report}}
}

func takesCFunctionPointer(_ fn: @convention(c) () -> ()) {}

func cFunctionPointer() {
  takesCFunctionPointer({}) // expected-note {{other definition here}}
  takesCFunctionPointer({}) // expected-error {{multiple definitions of symbol '$s27diagnose_duplicate_closures16cFunctionPointeryyFyycfU_'; please submit a bug report}}
}
