// RUN: %target-typecheck-verify-swift -enable-experimental-feature Extern

// REQUIRES: swift_feature_Extern

@_extern(c) // expected-warning {{C name '+' may be invalid; explicitly specify the name in '@_extern(c)' to suppress this warning}}{{11-11=, "+"}}{{none}}
func +(a: Int, b: Bool) -> Bool
