// {"kind":"typecheck","signature":"swift::constraints::FailureDiagnostic::resolveType(swift::Type, bool, bool) const","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b } {
let:
  String->a = a.b
