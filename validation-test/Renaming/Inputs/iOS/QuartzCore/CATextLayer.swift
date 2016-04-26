
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
@available(iOS 3.2, *)
let kCATruncationNone: String
@available(iOS 3.2, *)
let kCATruncationStart: String
@available(iOS 3.2, *)
let kCATruncationEnd: String
@available(iOS 3.2, *)
let kCATruncationMiddle: String
@available(iOS 3.2, *)
let kCAAlignmentNatural: String
@available(iOS 3.2, *)
let kCAAlignmentLeft: String
@available(iOS 3.2, *)
let kCAAlignmentRight: String
@available(iOS 3.2, *)
let kCAAlignmentCenter: String
@available(iOS 3.2, *)
let kCAAlignmentJustified: String
