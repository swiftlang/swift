// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -F %S/Inputs/frameworks -DOLD -verify %s -Xcc -w
// RUN: %target-swift-frontend -typecheck -F %S/Inputs/frameworks -DNEW -verify %s -Xcc -w

import PrivateAsSubmodule.Private

#if OLD
import PrivateAsParallel.Private
#elseif NEW
import PrivateAsParallel_Private
#else
#error("OLD or NEW must be defined")
#endif

let _: Bool = PSGlobal // expected-error {{type 'Int32' cannot be used as a boolean}}
let _: Bool = PSPrivateGlobal // expected-error {{type 'Int32' cannot be used as a boolean}}
let _: Bool = PPGlobal // expected-error {{type 'Int32' cannot be used as a boolean}}
let _: Bool = PPPrivateGlobal // expected-error {{type 'Int32' cannot be used as a boolean}}
