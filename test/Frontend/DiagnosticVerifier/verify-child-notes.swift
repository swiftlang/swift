// RUN: %target-typecheck-verify-swift -verify-child-notes

struct A {}
struct A {} // expected-error {{invalid redeclaration of 'A'}} {{children: expected-note@-1 {{'A' previously declared here}} }}

struct B {}
struct B {} /* expected-error {{invalid redeclaration of 'B'}} {{children:
                 expected-note@-2 {{'B' previously declared here}}
            }}*/

struct C {}
struct C {} // expected-error {{invalid redeclaration of 'C'}} {{children:
            //   expected-note@-2 {{'C' previously declared here}}
            // }}

struct D {} // #1
struct D {} /* expected-error {{invalid redeclaration of 'D'}} {{children:
                 expected-note@#1 {{'D' previously declared here}}
            }}*/

struct E {} // #2
struct E {} /* expected-error {{invalid redeclaration of 'E'}} {{children:
                 expected-note@#2 {{'E' previously declared here}}
            }}*/
struct E {} /* expected-error {{invalid redeclaration of 'E'}} {{children:
                 expected-note@#2 {{'E' previously declared here}}
            }}*/

struct F {}; struct G {} // #3
struct F {} /* expected-error {{invalid redeclaration of 'F'}} {{children:
                 expected-note@#3 {{previously declared here}}
            }}*/
struct G {} /* expected-error {{invalid redeclaration of 'G'}} {{children:
                 expected-note@#3 {{previously declared here}}
            }}*/


enum WithNone { case none }
func testAmbiguous(_ x: WithNone?) {
    switch x {
    case .none: // expected-warning {{assuming you mean 'Optional<WithNone>.none'; did you mean 'WithNone.none' instead?}} {{children: expected-note {{use 'nil' to silence this warning}} expected-note {{use 'none?' instead}} }}
        break
    default:
        break
    }
}
