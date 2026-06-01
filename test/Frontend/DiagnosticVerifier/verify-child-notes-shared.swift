// RUN: not %target-typecheck-verify-swift -verify-child-notes 2>&1 | %FileCheck %s --implicit-check-not error: --implicit-check-not warning: --implicit-check-not note:

struct A {}; struct B {}; struct C {}; struct D {}; struct E {} // #1

struct A {} /* expected-error {{invalid redeclaration of 'A'}} {{children:
                 expected-note@#1 {{previously declared here}}
            }}*/
struct C {} /* expected-error {{invalid redeclaration of 'C'}} {{children:
                 expected-note@#1 {{'C' previously declared here}}
            }}*/
struct E {} /* expected-error {{invalid redeclaration of 'E'}} {{children:
                 expected-note@#1 2{{previously declared here}}
CHECK: [[@LINE-1]]:{{[0-9]+}}: error: expected note not produced
            }}*/

