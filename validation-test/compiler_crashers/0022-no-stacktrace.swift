// RUN: not --crash %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Based on the stack trace this bug is actually triggering a different code path
// compared to case #021. so these appears not to be dupes(!).

class A<T where T : A> { // expected-error {{reference to generic type 'A' requires arguments in <...>}} expected-note {{generic type 'A' declared here}}
}
