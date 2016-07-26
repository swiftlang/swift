
@available(OSX 10.10, *)
class SCNTechnique : NSObject, SCNAnimatable, NSCopying, NSSecureCoding {
  /*not inherited*/ init?(dictionary dictionary: [String : AnyObject])
  /*not inherited*/ init?(bySequencingTechniques techniques: [SCNTechnique])
  func handleBinding(ofSymbol symbol: String, using block: SCNBindingBlock? = nil)
  var dictionaryRepresentation: [String : AnyObject] { get }
  @available(OSX 10.11, *)
  subscript(_ key: AnyObject) -> AnyObject? { get }
  @available(OSX 10.11, *)
  func setObject(_ obj: AnyObject?, forKeyedSubscript key: NSCopying)
}
protocol SCNTechniqueSupport : NSObjectProtocol {
  @available(OSX 10.10, *)
  @NSCopying var technique: SCNTechnique? { get set }
}
