
enum SCNMorpherCalculationMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case normalized
  case additive
}
@available(tvOS 8.0, *)
class SCNMorpher : NSObject, SCNAnimatable, NSSecureCoding {
  var targets: [SCNGeometry]
  func setWeight(_ weight: CGFloat, forTargetAt targetIndex: Int)
  @discardableResult
  func weightForTarget(at targetIndex: Int) -> CGFloat
  var calculationMode: SCNMorpherCalculationMode
}
