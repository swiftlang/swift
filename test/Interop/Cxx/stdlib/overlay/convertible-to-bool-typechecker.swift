// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %S/Inputs -enable-experimental-cxx-interop
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %S/Inputs -cxx-interoperability-mode=swift-6
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import ConvertibleToBool

let _ = Bool(fromCxx: BoolBox())
let _ = Bool(fromCxx: NonConstBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'NonConstBoolBox' conform to 'CxxConvertibleToBool'}}
let _ = Bool(fromCxx: DualOverloadBoolBox())
let _ = Bool(fromCxx: ExplicitBoolBox())
let _ = Bool(fromCxx: BoolBoxWithOtherConversions())
let _ = Bool(fromCxx: DeletedBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'DeletedBoolBox' conform to 'CxxConvertibleToBool'}}

let _ = Bool(fromCxx: InheritedBoolBox())
let _ = Bool(fromCxx: OverriddenBoolBox())
let _ = Bool(fromCxx: AmbiguousBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'AmbiguousBoolBox' conform to 'CxxConvertibleToBool'}}
let _ = Bool(fromCxx: VirtualDiamondBoolBox())
let _ = Bool(fromCxx: DiamondBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'DiamondBoolBox' conform to 'CxxConvertibleToBool'}}

let _ = Bool(fromCxx: PrivateBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'PrivateBoolBox' conform to 'CxxConvertibleToBool'}}
let _ = Bool(fromCxx: ProtectedBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'ProtectedBoolBox' conform to 'CxxConvertibleToBool'}}
let _ = Bool(fromCxx: PrivateInheritedBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'PrivateInheritedBoolBox' conform to 'CxxConvertibleToBool'}}
let _ = Bool(fromCxx: ProtectedInheritedBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'ProtectedInheritedBoolBox' conform to 'CxxConvertibleToBool'}}
let _ = Bool(fromCxx: PublicUsingBoolBox())
let _ = Bool(fromCxx: ProtectedUsingBoolBox()) // expected-error {{initializer 'init(fromCxx:)' requires that 'ProtectedUsingBoolBox' conform to 'CxxConvertibleToBool'}}

