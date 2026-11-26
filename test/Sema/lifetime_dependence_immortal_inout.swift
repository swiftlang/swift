// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature ClosureLifetimes

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_ClosureLifetimes

// rdar://189340151 ([lifedep] Disable @_lifetime(inoutParam: immortal))

struct NE: ~Escapable {
  @_lifetime(immortal)
  init() {}
}

@_lifetime(dest: immortal) // expected-error{{invalid 'immortal' lifetime dependence for parameter 'dest'}}
func immortalInout(dest: inout NE) {}

@_lifetime(dest: immortal, copy source) // expected-error{{invalid 'immortal' lifetime dependence for parameter 'dest'}}
func immortalInoutCopy(dest: inout NE, source: NE) {}

@_lifetime(dest: copy source, immortal) // expected-error{{invalid 'immortal' lifetime dependence for parameter 'dest'}}
func copyImmortalInout(dest: inout NE, source: NE) {}

@_lifetime(dest: immortal, immortal) // expected-error{{invalid 'immortal' lifetime dependence for parameter 'dest'}}
func immortalImmortalInout(dest: inout NE) {}

@_lifetime(dest: immortal) // expected-error{{invalid 'immortal' lifetime dependence for parameter 'dest'}}
func immortalInoutConflict(dest: inout NE, immortal: NE) {} // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}

struct S: ~Escapable {
  @_lifetime(self: immortal) // expected-error{{invalid 'immortal' lifetime dependence for parameter 'self'}}
  mutating func mutateImmortal() {}

  @_lifetime(self: copy source, immortal) // expected-error{{invalid 'immortal' lifetime dependence for parameter 'self'}}
  mutating func mutateCopyImmortal(source: NE) {}
}

@_lifetime(immortal, copy source) // expected-error{{cannot specify any other dependence source along with immortal}}
func immortalCopyResult(source: NE) -> NE { NE() }

@_lifetime(copy source, immortal) // expected-error{{cannot specify any other dependence source along with immortal}}
func copyImmortalResult(source: NE) -> NE { NE() }

@_lifetime(immortal, borrow source) // expected-error{{cannot specify any other dependence source along with immortal}}
func immortalBorrowResult(source: borrowing NE) -> NE { NE() }

@_lifetime(immortal, immortal) // expected-error{{cannot specify any other dependence source along with immortal}}
func immortalImmortalResult() -> NE { NE() }

func takeClosure(
  _ f: @_lifetime(dest: immortal) (_ dest: inout NE) -> () // expected-error{{invalid 'immortal' lifetime dependence for parameter 'dest'}}
) {}

func takeClosureImmortalCaptures(
  _ f: @_lifetime(immortal, captures) () -> NE // expected-error{{cannot specify any other dependence source along with immortal}}
) {}

// OK: immortal is allowed for the result.
@_lifetime(immortal)
func immortalResult() -> NE { NE() }

// OK: immortal result alongside a parameter dependence.
@_lifetime(dest: copy source)
@_lifetime(immortal)
func immortalResultWithInout(dest: inout NE, source: NE) -> NE { NE() }
