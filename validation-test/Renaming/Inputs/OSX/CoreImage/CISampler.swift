
@available(OSX 10.4, *)
class CISampler : NSObject, NSCopying {
  convenience init(image im: CIImage)
  init(image im: CIImage, options dict: [NSObject : AnyObject]? = [:])
  var definition: CIFilterShape { get }
  var extent: CGRect { get }
}

extension CISampler {
  convenience init(im im: CIImage, elements elements: (String, AnyObject)...)
}
@available(OSX 10.4, *)
let kCISamplerAffineMatrix: String
@available(OSX 10.4, *)
let kCISamplerWrapMode: String
@available(OSX 10.4, *)
let kCISamplerFilterMode: String
@available(OSX 10.4, *)
let kCISamplerWrapBlack: String
@available(OSX 10.4, *)
let kCISamplerWrapClamp: String
@available(OSX 10.4, *)
let kCISamplerFilterNearest: String
@available(OSX 10.4, *)
let kCISamplerFilterLinear: String
@available(OSX 10.4, *)
let kCISamplerColorSpace: String
