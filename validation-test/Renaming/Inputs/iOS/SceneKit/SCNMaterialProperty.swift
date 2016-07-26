
enum SCNFilterMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  @available(iOS 8.0, *)
  case none
  @available(iOS 8.0, *)
  case nearest
  @available(iOS 8.0, *)
  case linear
}
enum SCNWrapMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  @available(iOS 8.0, *)
  case clamp
  @available(iOS 8.0, *)
  case `repeat`
  @available(iOS 9.0, *)
  case clampToBorder
  @available(iOS 8.0, *)
  case mirror
}
@available(iOS 8.0, *)
class SCNMaterialProperty : NSObject, SCNAnimatable, NSSecureCoding {
  @available(iOS 8.0, *)
  convenience init(contents contents: AnyObject)
  var contents: AnyObject?
  @available(iOS 8.0, *)
  var intensity: CGFloat
  var minificationFilter: SCNFilterMode
  var magnificationFilter: SCNFilterMode
  var mipFilter: SCNFilterMode
  var contentsTransform: SCNMatrix4
  var wrapS: SCNWrapMode
  var wrapT: SCNWrapMode
  var borderColor: AnyObject?
  var mappingChannel: Int
  @available(iOS 8.0, *)
  var maxAnisotropy: CGFloat
}
