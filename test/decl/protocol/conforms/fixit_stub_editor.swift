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

protocol P3 {
  associatedtype T1
  associatedtype T2
  associatedtype T3
}

protocol P4 : P3 {
  associatedtype T4 = T1
  associatedtype T5 = T2
  associatedtype T6 = T3
}

class C2 : P4 {} // expected-error{{type 'C2' does not conform to protocol 'P4'}} expected-error{{type 'C2' does not conform to protocol 'P3'}} expected-note{{do you want to add protocol stubs?}}{{16-16=\n    typealias T1 = <#type#>\n\n    typealias T2 = <#type#>\n\n    typealias T3 = <#type#>\n}}
