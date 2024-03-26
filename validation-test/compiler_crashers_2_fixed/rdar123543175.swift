// RUN: %target-typecheck-verify-swift

public enum E : E.R {
// expected-error@-1 {{'E' has a raw type that depends on itself}}
// expected-note@-2 2{{through reference here}}
// expected-note@-3 {{while resolving type 'E.R'}}
// expected-error@-4 {{'R' is not a member type of enum 'rdar123543175.E'}}
// expected-note@-5 {{'E' declared here}}
  case x
}
