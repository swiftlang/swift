// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing
// RUN: not %swift %s -parse

struct c<e> {
    let d: [(  h // expected-note 4{{to match this opening}}
      // expected-note @-1{{undeclared type}}
}
func b(g: f) -> <e>(()-> e) -> i
