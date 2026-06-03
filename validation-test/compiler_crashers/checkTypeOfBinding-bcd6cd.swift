// {"kind":"typecheck","original":"1066d2b2","signature":"swift::constraints::inference::checkTypeOfBinding(swift::TypeVariableType*, swift::Type)","signatureAssert":"Assertion failed: ((!typeVariables.empty() || hasError()) && \"Did not find type variables!\"), function getTypeVariables","signatureNext":"PotentialBindings::inferFromRelational"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a<b {
  @globalActor struct d {
    static  shared =
    enum c {
      case (@d()  -> Int = {
