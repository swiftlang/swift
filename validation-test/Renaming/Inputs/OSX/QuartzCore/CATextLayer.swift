
class CATextLayer : CALayer {
  @NSCopying var string: AnyObject?
  var font: CFTypeRef?
  var fontSize: CGFloat
  var foregroundColor: CGColor?
  var isWrapped: Bool
  var truncationMode: String
  var alignmentMode: String
  var allowsFontSubpixelQuantization: Bool
}
@available(OSX 10.5, *)
let kCATruncationNone: String
@available(OSX 10.5, *)
let kCATruncationStart: String
@available(OSX 10.5, *)
let kCATruncationEnd: String
@available(OSX 10.5, *)
let kCATruncationMiddle: String
@available(OSX 10.5, *)
let kCAAlignmentNatural: String
@available(OSX 10.5, *)
let kCAAlignmentLeft: String
@available(OSX 10.5, *)
let kCAAlignmentRight: String
@available(OSX 10.5, *)
let kCAAlignmentCenter: String
@available(OSX 10.5, *)
let kCAAlignmentJustified: String
