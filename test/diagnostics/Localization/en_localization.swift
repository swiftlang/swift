// RUN: %target-typecheck-verify-swift -localization-path %S/Inputs -locale en

_ = "HI!
// expected-error@-1{{unterminated string literal}}
var self1 = self1
// expected-note@-1 2{{through reference here}}
// expected-error@-2 {{circular reference}}

struct Broken {
  var b : Bool = True // expected-error{{cannot find 'True' in scope}}
}
// CHECK_NAMES: error: cannot find 'True' in scope [cannot_find_in_scope]{{$}}
