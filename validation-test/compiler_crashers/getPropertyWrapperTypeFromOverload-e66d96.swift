// {"kind":"typecheck","original":"c9d8f598","signature":"getPropertyWrapperTypeFromOverload(swift::constraints::ConstraintSystem&, swift::constraints::SelectedOverload, llvm::function_ref<swift::VarDecl* (swift::VarDecl*)>)","signatureAssert":"Assertion failed: (!cs.hasType(D) && \"Should have recorded type for wrapper var\"), function getPropertyWrapperTypeFromOverload"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  wrappedValue : Bool  init(wrappedValue: Bool)
  var projectedValue
  init(projectedValue: Bool)
  func b(@a c: Bool) {
    b - c
  }
}
