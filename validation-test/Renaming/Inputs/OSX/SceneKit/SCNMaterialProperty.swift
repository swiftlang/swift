
enum SCNFilterMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  @available(OSX 10.9, *)
  case none
  @available(OSX 10.9, *)
  case nearest
  @available(OSX 10.9, *)
  case linear
}
enum SCNWrapMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  @available(OSX 10.9, *)
  case clamp
  @available(OSX 10.9, *)
  case `repeat`
  @available(OSX 10.9, *)
  case clampToBorder
  @available(OSX 10.9, *)
  case mirror
}
@available(OSX 10.8, *)
class SCNMaterialProperty : NSObject, SCNAnimatable, NSSecureCoding {
  @available(OSX 10.9, *)
  convenience init(contents contents: AnyObject)
  var contents: AnyObject?
  @available(OSX 10.9, *)
  var intensity: CGFloat
  var minificationFilter: SCNFilterMode
  var magnificationFilter: SCNFilterMode
  var mipFilter: SCNFilterMode
  var contentsTransform: SCNMatrix4
  var wrapS: SCNWrapMode
  var wrapT: SCNWrapMode
  var borderColor: AnyObject?
  var mappingChannel: Int
  @available(OSX 10.9, *)
  var maxAnisotropy: CGFloat
}
