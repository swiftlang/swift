// {"kind":"emit-silgen","original":"536cc827","signature":"swift::SILType::hasAbstractionDifference(swift::SILFunctionTypeRepresentation, swift::SILType)","signatureNext":"ScalarResultPlan::finish"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
enum a {
}
@propertyWrapper
struct b<c, d> {
  var wrappedValue: d
  init(projectedValue: e) {
  }
  var projectedValue: e {
  }
  struct e {
  }
}
func f(@b<a, [Int]> g: [Int] = []) {
  func h() {
    f()
  }
}
