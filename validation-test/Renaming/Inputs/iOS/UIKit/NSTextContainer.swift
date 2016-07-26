
@available(iOS 7.0, *)
class NSTextContainer : NSObject, NSCoding, NSTextLayoutOrientationProvider {
  @available(iOS 7.0, *)
  init(size size: CGSize)
  unowned(unsafe) var layoutManager: @sil_unmanaged NSLayoutManager?
  @available(iOS 9.0, *)
  func replaceLayoutManager(_ newLayoutManager: NSLayoutManager)
  @available(iOS 7.0, *)
  var size: CGSize
  @available(iOS 7.0, *)
  var exclusionPaths: [UIBezierPath]
  @available(iOS 7.0, *)
  var lineBreakMode: NSLineBreakMode
  var lineFragmentPadding: CGFloat
  @available(iOS 7.0, *)
  var maximumNumberOfLines: Int
  @available(iOS 7.0, *)
  @discardableResult
  func lineFragmentRect(forProposedRect proposedRect: CGRect, at characterIndex: Int, writingDirection baseWritingDirection: NSWritingDirection, remaining remainingRect: UnsafeMutablePointer<CGRect>?) -> CGRect
  @available(iOS 9.0, *)
  var isSimpleRectangularTextContainer: Bool { get }
  var widthTracksTextView: Bool
  var heightTracksTextView: Bool
}
