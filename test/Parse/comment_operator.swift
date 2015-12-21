// RUN: %target-swift-frontend -parse -verify -module-name main %s

// Tests for interaction between comments & operators from SE-0037
// which defined comments to be whitespace for operator arity rules.

let foo: Bool! = true

// Used to be errors, should now work
let a = /* */!foo
1/**/+ 2
1 /**/+ 2
1 +/*hi*/2

// Used to work, should now be errors
foo/* */?.description  // expected-error * {{}}
foo/* */!              // expected-error * {{}}
1/**/+2                // expected-error * {{}}
1+/**/2                // expected-error * {{}}

// Continue to be errors
!/* */foo              // expected-error * {{}}
1+/* */2               // expected-error * {{}}

// Continue to work
foo!// this is dangerous
1 +/**/ 2
1 +/* hi */2
