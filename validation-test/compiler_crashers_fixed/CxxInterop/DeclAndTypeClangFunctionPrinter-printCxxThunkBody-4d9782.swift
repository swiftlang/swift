// RUN: %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s -verify
class a<b> {
  // expected-note@-1 {{generic class 'a' declared here}}
  // expected-cparser-note@-2 {{in declaration of 'a'}}
  class func  0()
  // expected-cparser-error@-1 {{expected '{' in body of function declaration}}
  // expected-cparser-error@-2 {{function name can only start with a letter or underscore}} expected-astgen-error@-2 {{identifier can only start with a letter or underscore}}
  .
  // expected-cparser-error@-1 {{expected declaration}} expected-astgen-error@-1 {{unexpected code '.' in class}}
}
class c: a {
  // expected-error@-1 {{reference to generic type 'a' requires arguments in <...>}}
}
