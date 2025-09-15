// RUN: %target-typecheck-verify-swift

_ = a.init
_ = b.init

enum a : Int { case x = #/ /# }  // expected-error {{raw value for enum case must be a literal}}
enum b : String { case x = #file }  // expected-error {{raw value for enum case must be a literal}}
