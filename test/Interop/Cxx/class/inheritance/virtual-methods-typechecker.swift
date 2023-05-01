// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-experimental-cxx-interop

import VirtualMethods

VirtualNonAbstractBase().nonAbstractMethod() // expected-error {{'nonAbstractMethod()' is unavailable: virtual functions are not yet available in Swift}}
