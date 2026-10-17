// RUN: %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s -verify
class a<b> {
  // expected-note@-1 {{generic class 'a' declared here}}
  // expected-note@-2 {{in declaration of 'a'}}
  class func  0()
  // expected-error@-1 {{expected '{' in body of function declaration}}
  // expected-error@-2 {{function name can only start with a letter or underscore}}
  .
  // expected-error@-1 {{expected declaration}}
}
class c: a {
  // expected-error@-1 {{reference to generic type 'a' requires arguments in <...>}}
}
