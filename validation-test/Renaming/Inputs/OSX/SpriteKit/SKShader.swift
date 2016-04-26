
@available(OSX 10.10, *)
class SKShader : NSObject, NSCopying, NSCoding {
  init(source source: String)
  init(source source: String, uniforms uniforms: [SKUniform])
  convenience init(fileNamed name: String)
  var source: String?
  var uniforms: [SKUniform]
  func addUniform(_ uniform: SKUniform)
  @discardableResult
  func uniformNamed(_ name: String) -> SKUniform?
  func removeUniformNamed(_ name: String)
}
