// RUN: %target-swift-frontend %s -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -verify-ignore-unrelated

import StdEnableSharedFromThis

func s(_ _: SharableFromThis) {}
// The notes explaining why these are not importable land in the C++ standard
// library headers, so they are ignored above rather than matched here.
func f(_ _: MalformedFoo) {} // expected-error {{cannot find type 'MalformedFoo' in scope}}
func i(_ _: MalformedInt) {} // expected-error {{cannot find type 'MalformedInt' in scope}}
