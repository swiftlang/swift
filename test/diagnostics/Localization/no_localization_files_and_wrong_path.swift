// RUN: %target-typecheck-verify-swift -localization-path /Not_exsisting_path -locale en -disable-parser-lookup

// <unknown>:0: warning: cannot find translations for 'en' at '/Not_exsisting_path/en.yaml': no such file
// <unknown>:0: warning: specified localization directory '/Not_exsisting_path' does not exist, translation is disabled

_ = "HI!
// expected-error@-1{{unterminated string literal}}
var self1 = self1
// expected-note@-1 2{{through reference here}}
// expected-error@-2 {{circular reference}}

struct Broken {
  var b : Bool = True // expected-error{{cannot find 'True' in scope}}
}
