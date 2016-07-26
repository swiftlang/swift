
class SKTextureAtlas : NSObject, NSCoding {
  convenience init(named name: String)
  @available(OSX 10.10, *)
  convenience init(dictionary properties: [String : AnyObject])
  @discardableResult
  func textureNamed(_ name: String) -> SKTexture
  class func preloadTextureAtlases(_ textureAtlases: [SKTextureAtlas], withCompletionHandler completionHandler: () -> Void)
  @available(OSX 10.11, *)
  class func preloadTextureAtlasesNamed(_ atlasNames: [String], withCompletionHandler completionHandler: (NSError?, [SKTextureAtlas]) -> Void)
  func preload(completionHandler completionHandler: () -> Void)
  var textureNames: [String] { get }
}

extension SKTextureAtlas : CustomPlaygroundQuickLookable {
}
