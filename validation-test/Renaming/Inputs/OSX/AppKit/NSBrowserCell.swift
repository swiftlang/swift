
class NSBrowserCell : NSCell {
  @discardableResult
  class func branchImage() -> NSImage?
  @discardableResult
  class func highlightedBranchImage() -> NSImage?
  @discardableResult
  func highlightColor(in controlView: NSView) -> NSColor?
  var isLeaf: Bool
  var isLoaded: Bool
  func reset()
  func set()
  var alternateImage: NSImage?
}
