
class NSImageView : NSControl, NSAccessibilityImage {
  var image: NSImage?
  var imageAlignment: NSImageAlignment
  var imageScaling: NSImageScaling
  var imageFrameStyle: NSImageFrameStyle
  var isEditable: Bool
  var animates: Bool
  var allowsCutCopyPaste: Bool
}
struct __IVFlags {
  var _hasImageView: UInt32
  var _unused: UInt32
  var _rejectsMultiFileDrops: UInt32
  var _compatibleScalingAndAlignment: UInt32
  var _reserved: UInt32
  var _overridesDrawing: UInt32
  var _allowsCutCopyPaste: UInt32
  var _editable: UInt32
  init()
  init(_hasImageView _hasImageView: UInt32, _unused _unused: UInt32, _rejectsMultiFileDrops _rejectsMultiFileDrops: UInt32, _compatibleScalingAndAlignment _compatibleScalingAndAlignment: UInt32, _reserved _reserved: UInt32, _overridesDrawing _overridesDrawing: UInt32, _allowsCutCopyPaste _allowsCutCopyPaste: UInt32, _editable _editable: UInt32)
}
