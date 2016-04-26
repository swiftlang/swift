
@available(OSX 10.11, *)
class GKRandomDistribution : NSObject, GKRandom {
  var lowestValue: Int { get }
  var highestValue: Int { get }
  var numberOfPossibleOutcomes: Int { get }
  init(randomSource source: GKRandom, lowestValue lowestInclusive: Int, highestValue highestInclusive: Int)
  convenience init(lowestValue lowestInclusive: Int, highestValue highestInclusive: Int)
  convenience init(forDieWithSideCount sideCount: Int)
  @discardableResult
  class func d6() -> Self
  @discardableResult
  class func d20() -> Self
}
@available(OSX 10.11, *)
class GKGaussianDistribution : GKRandomDistribution {
  var mean: Float { get }
  var deviation: Float { get }
  init(randomSource source: GKRandom, mean mean: Float, deviation deviation: Float)
}
@available(OSX 10.11, *)
class GKShuffledDistribution : GKRandomDistribution {
}
