// {"kind":"emit-silgen","signature":"swift::SILType::hasAbstractionDifference(swift::SILFunctionTypeRepresentation, swift::SILType)","signatureNext":"ScalarResultPlan::finish"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper
struct a {
  var wrappedValue: Int
  var projectedValue: Self {
  }
  init(projectedValue: Self) {
  }
}
func b(@a c: Int = 0) {
  b()
}
