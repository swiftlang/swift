// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import ConvertibleToBool

let _ = Bool(fromCxx: BoolBox())
let _ = Bool(fromCxx: NonConstBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'NonConstBoolBox' conform to 'CxxConvertibleToBool'}}
let _ = Bool(fromCxx: DualOverloadBoolBox())
