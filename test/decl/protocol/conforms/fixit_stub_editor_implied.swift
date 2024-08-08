// RUN: %target-typecheck-verify-swift -serialize-diagnostics-path %t.diag

protocol P1 {
  func foo1() // expected-note{{protocol requires function 'foo1()' with type '() -> ()}}
  func foo2() // expected-note{{protocol requires function 'foo2()' with type '() -> ()}}
}

protocol P2 {
  func bar1() // expected-note{{protocol requires function 'bar1()' with type '() -> ()}}
  func bar2() // expected-note{{protocol requires function 'bar2()' with type '() -> ()}}
}

class C1 : P1, P2 {} 
// expected-error@-1 {{type 'C1' does not conform to protocol 'P1'}} 
// expected-error@-2 {{type 'C1' does not conform to protocol 'P2'}} 
// expected-note@-3 {{add stubs for conformance}}{{20-20=\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n}}
