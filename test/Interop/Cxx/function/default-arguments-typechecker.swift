// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import DefaultArguments

let _ = isZero()
let _ = isZero(1)

let _ = isNil()
let _ = isStrNil()
let _ = isGlobalNonNil()

let _ = sum(1)
let _ = sum(1, 2)
let _ = subtract()
let _ = subtract(2)
let _ = subtract(2, 1)

let _ = isArgZero()
let _ = isArgNonZero()
let _ = isArgZeroOutOfLine()
let _ = isArgZeroConstRef() // expected-error {{missing argument for parameter #1 in call}}
let _ = isArgNonZeroConstRef() // expected-error {{missing argument for parameter #1 in call}}
let _ = isArgNonPODNonZeroConstRef() // expected-error {{missing argument for parameter #1 in call}}

let _ = isArgViewNull() // expected-error {{missing argument for parameter #1 in call}}
let _ = isArgViewNullAnd() // expected-error {{missing argument for parameter #1 in call}}
let _ = isArgViewNullAndReversed() // expected-error {{missing argument for parameter #2 in call}}
let _ = isArgViewNullUnsafeParam()
let _ = isArgViewNullUnsafeFunc() // expected-error {{missing argument for parameter #1 in call}}
let _ = isArgOwnedPtrNull()

let _ = isArgFRTNull()
let _ = getArgFRTValue()
let _ = getArgRefCountedValue()

let _ = HasMethodWithDefaultArg().isZero()
let _ = HasMethodWithDefaultArg().isZero(1)
let _ = HasMethodWithDefaultArg().isNonZero()
let _ = HasMethodWithDefaultArg().isNonZero(1)
let _ = HasMethodWithDefaultArg().isNilPtr()
let _ = HasMethodWithDefaultArg().isNilConstPtr()
let _ = HasMethodWithDefaultArg().isZeroConstRef() // expected-error {{missing argument for parameter #1 in call}}

let _ = DerivedFromHasMethodWithDefaultArg().isZero()
let _ = DerivedFromDerivedFromHasMethodWithDefaultArg().isZero()

let _ = HasStaticMethodWithDefaultArg.isNonZero()
let _ = HasStaticMethodWithDefaultArg.isNonZero(1)
let _ = HasStaticMethodWithDefaultArg.isNonZeroPrivateCounter()
let _ = HasStaticMethodWithDefaultArg.isNonZeroPrivateCounter(1)
let _ = HasStaticMethodWithDefaultArg.isArgZeroRef() // expected-error {{missing argument for parameter #1 in call}}

let _ = HasCtorWithDefaultArg(1, 2, 3)
// TODO: support default arguments of constructors (https://github.com/apple/swift/issues/70124)
//let _ = HasCtorWithDefaultArg(1, 2)
//let _ = HasCtorWithDefaultArg(1)

let _ = TemplatedHasMethodWithDefaultArgFloat().isZero()
let _ = TemplatedHasMethodWithDefaultArgFloat().isNonZero()
let _ = TemplatedHasMethodWithDefaultArgInt().isZero()
let _ = TemplatedHasMethodWithDefaultArgInt().isNonZero()

let _ = DerivedFromTemplatedHasMethodWithDefaultArgFloat().isZero()
let _ = DerivedFromTemplatedHasMethodWithDefaultArgFloat().isNonZero()

let _ = ambiguous(1)
let _ = ambiguous(1, 2)

let _ = nonTrailing()
let _ = nonTrailing(1)
let _ = nonTrailing(1, 2)
