// {"kind":"typecheck","original":"c6df5cd3","signature":"swift::ASTContext::setSideCachedPropertyWrapperBackingPropertyType(swift::VarDecl*, swift::Type)","signatureAssert":"Assertion failed: (!getImpl().PropertyWrapperBackingVarTypes[var] || getImpl().PropertyWrapperBackingVarTypes[var]->isEqual(type)), function setSideCachedPropertyWrapperBackingPropertyType","signatureNext":"applySolutionToInitialization"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
@propertyWrapper struct f<each c> {
  var wrappedValue: (repeat each c)
  struct d<each e: a> {
    @f<repeat each e.b> var g: (repeat each e.b)
  }
}
