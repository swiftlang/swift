// {"kind":"typecheck","original":"9317f66b","signature":"swift::InterfaceTypeRequest::cacheResult(swift::Type) const","signatureAssert":"Assertion failed: (!type->is<InOutType>() && \"Interface type must be materializable\"), function cacheResult","signatureNext":"ParamDecl::createImplicit"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b {
  wrappedValue: b  init(wrappedValue: inout b
}
struct c {
  static  d: Double
  @a var e = &d
