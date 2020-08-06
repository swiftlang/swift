// RUN: %target-typecheck-verify-swift

struct Bar { // expected-note {{in declaration of 'Bar'}}
 var fisr = 0x5F3759DF
 var a: Int

 // ensure that the id.id pattern is not interpreted as a function
 a.foo = 345 // expected-error {{expected declaration}}
 // ensure that the id.id pattern generating an expected declaration
 // diagnostic does not block further diagnostics.
 fisr.bar = 345
}
