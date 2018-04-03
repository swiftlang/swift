// RUN: %target-typecheck-verify-swift -parse-as-library

class C {
  init() {}
}

weak var globalC = C() // expected-warning {{instance will be immediately deallocated as 'globalC' is a 'weak' variable}}
// expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
// expected-note@-2 {{'globalC' declared here}}

unowned var globalC1 = C() // expected-warning {{instance will be immediately deallocated as 'globalC1' is an 'unowned' variable}}
// expected-note@-1 {{a strong reference is required to prevent the instance from being deallocated}}
// expected-note@-2 {{'globalC1' declared here}}

