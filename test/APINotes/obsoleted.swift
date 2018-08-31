// RUN: not %target-swift-frontend -typecheck -verify -I %S/Inputs/custom-modules -F %S/Inputs/custom-frameworks -swift-version 4 %s
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs/custom-modules -F %S/Inputs/custom-frameworks -swift-version 4.2 %s
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs/custom-modules -F %S/Inputs/custom-frameworks -swift-version 5 %s
// REQUIRES: objc_interop
import ObsoletedAPINotesTest

let _: FooID // expected-error{{'FooID' has been renamed to 'Foo_ID'}}
let _: Foo_ID

let _: BarContainerOld.Sub // expected-error{{'Sub' has been renamed to 'BarContainerCanonical.Sub'}}
let _: BarContainerCanonical.Sub
