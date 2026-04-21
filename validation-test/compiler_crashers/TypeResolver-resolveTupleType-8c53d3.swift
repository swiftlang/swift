// {"kind":"typecheck","original":"3067a2f1","signature":"(anonymous namespace)::TypeResolver::resolveTupleType(swift::TupleTypeRepr*, swift::TypeResolutionOptions)","signatureAssert":"Assertion failed: (!noncopyableTy->is<TupleType>() && \"will use poor wording\"), function resolveTupleType","signatureNext":"TypeResolver::resolveType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a: ~Copyable
  typealias b = (a, a)
  (b b
