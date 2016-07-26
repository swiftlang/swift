
protocol GKRandom {
  @discardableResult
  func nextInt() -> Int
  @discardableResult
  func nextInt(withUpperBound upperBound: Int) -> Int
  @discardableResult
  func nextUniform() -> Float
  @discardableResult
  func nextBool() -> Bool
}
@available(tvOS 9.0, *)
class GKRandomSource : NSObject, GKRandom, NSSecureCoding, NSCopying {
  @discardableResult
  class func sharedRandom() -> GKRandomSource
  @discardableResult
  func arrayByShufflingObjects(in array: [AnyObject]) -> [AnyObject]
}
@available(tvOS 9.0, *)
class GKARC4RandomSource : GKRandomSource {
  @NSCopying var seed: NSData
  init(seed seed: NSData)
  func dropValues(withCount count: Int)
}
@available(tvOS 9.0, *)
class GKLinearCongruentialRandomSource : GKRandomSource {
  var seed: UInt64
  init(seed seed: UInt64)
}
@available(tvOS 9.0, *)
class GKMersenneTwisterRandomSource : GKRandomSource {
  var seed: UInt64
  init(seed seed: UInt64)
}
