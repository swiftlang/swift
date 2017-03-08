// RUN: %target-typecheck-verify-swift -diagnostics-editor-mode

protocol P1 {
  func foo1()
  func foo2()
}

protocol P2 {
  func bar1()
  func bar2()
}

class C1 : P1, P2 {} // expected-error{{type 'C1' does not conform to protocol 'P1'}} expected-error{{type 'C1' does not conform to protocol 'P2'}} expected-note{{do you want to add protocol stubs?}}{{20-20=\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n}}
