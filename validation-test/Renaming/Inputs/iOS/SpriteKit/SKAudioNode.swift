
@available(iOS 9.0, *)
class SKAudioNode : SKNode, NSCoding {
  convenience init(fileNamed name: String)
  convenience init(url url: NSURL)
  var autoplayLooped: Bool
  var isPositional: Bool
}
extension SKAction {
  @available(iOS 9.0, *)
  @discardableResult
  class func stereoPan(to v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func stereoPan(by v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeReverb(to v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeReverb(by v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeObstruction(to v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeObstruction(by v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeOcclusion(to v: Float, duration duration: NSTimeInterval) -> SKAction
  @available(iOS 9.0, *)
  @discardableResult
  class func changeOcclusion(by v: Float, duration duration: NSTimeInterval) -> SKAction
}
