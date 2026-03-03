// RUN: %target-swift-frontend %s -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default

import StdEnableSharedFromThis

func s(_ _: SharableFromThis) {}
func f(_ _: MalformedFoo) {} // expected-error {{cannot find type 'MalformedFoo' in scope}}
func i(_ _: MalformedInt) {} // expected-error {{cannot find type 'MalformedInt' in scope}}
