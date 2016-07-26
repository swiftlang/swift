
typealias CGColorConverterRef = OpaquePointer
@discardableResult
func CGColorConverterGetTypeID() -> CFTypeID
enum CGColorConverterTransformType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case fromSpace
  case toSpace
  case applySpace
}
@discardableResult
func CGColorConverterCreateSimple(_ from: CGColorSpace?, _ to: CGColorSpace?) -> CGColorConverterRef?
func CGColorConverterRelease(_ _: CGColorConverterRef?)
