// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-experimental-cxx-interop

import Constructors

func takesCopyable<T: Copyable>(_ x: T.Type) {}

let explicit = ExplicitDefaultConstructor()

let implicit = ImplicitDefaultConstructor()

let deletedImplicitly = ConstructorWithParam() // expected-warning {{'init()' is deprecated}}
let onlyCopyAndMove = CopyAndMoveConstructor() // expected-warning {{'init()' is deprecated}}

let deletedExplicitly = DefaultConstructorDeleted() // expected-error {{missing argument for parameter 'a' in call}}

let withArg = ConstructorWithParam(42)

let _ = TemplatedCopyConstructor(123)
let _ = TemplatedCopyConstructorWithExtraArg(123)
takesCopyable(TemplatedCopyConstructor.self)
takesCopyable(TemplatedCopyConstructorWithExtraArg.self)
