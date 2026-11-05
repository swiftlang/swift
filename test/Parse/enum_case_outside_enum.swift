// RUN: %target-typecheck-verify-swift

// Cases are excluded from non-enums.
case FloatingCase // expected-cparser-error{{enum 'case' is not allowed outside of an enum}} expected-astgen-error{{'case' can only appear inside a 'switch' statement or 'enum' declaration}}
