
@available(tvOS 8.0, *)
class SCNTechnique : NSObject, SCNAnimatable, NSCopying, NSSecureCoding {
  /*not inherited*/ init?(dictionary dictionary: [String : AnyObject])
  /*not inherited*/ init?(bySequencingTechniques techniques: [SCNTechnique])
  func handleBinding(ofSymbol symbol: String, using block: SCNBindingBlock? = nil)
  var dictionaryRepresentation: [String : AnyObject] { get }
  @available(tvOS 9.0, *)
  subscript(_ key: AnyObject) -> AnyObject? { get }
  @available(tvOS 9.0, *)
  func setObject(_ obj: AnyObject?, forKeyedSubscript key: NSCopying)
}
protocol SCNTechniqueSupport : NSObjectProtocol {
  @available(tvOS 8.0, *)
  @NSCopying var technique: SCNTechnique? { get set }
}
