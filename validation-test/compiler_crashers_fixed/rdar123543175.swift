// RUN: %target-typecheck-verify-swift

public enum E : E.R {
// expected-error@-1 {{'E' has a raw type that depends on itself}}
// expected-note@-2 {{while resolving type 'E.R'}}
// expected-error@-3 {{'R' is not a member type of enum 'rdar123543175.E'}}
// expected-note@-4 {{'E' declared here}}
  case x
}
