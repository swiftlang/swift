// RUN: %target-typecheck-verify-swift

@com(interface: "00000000-0000-0000-C000-000000000046")
protocol IUnknown: AnyObject { }
// expected-error@-2 {{'@com' requires '-enable-experimental-com-interop'}}

@com
class CClass1 { }
// expected-error@-2 {{'@com' requires '-enable-experimental-com-interop'}}

@com(implementation: "00000000-0000-0000-0000-000000000000")
class CClass2 { }
// expected-error@-2 {{'@com' requires '-enable-experimental-com-interop'}}

@com(implementation: "00000000-0000-0000-0000-000000000000", threading: .free)
class CClass3 { }
// expected-error@-2 {{'@com' requires '-enable-experimental-com-interop'}}
