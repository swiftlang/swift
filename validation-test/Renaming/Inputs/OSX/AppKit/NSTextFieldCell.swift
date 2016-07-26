
enum NSTextFieldBezelStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case squareBezel
  case roundedBezel
}
class NSTextFieldCell : NSActionCell {
  @NSCopying var backgroundColor: NSColor?
  var drawsBackground: Bool
  @NSCopying var textColor: NSColor?
  var bezelStyle: NSTextFieldBezelStyle
  var placeholderString: String?
  @NSCopying var placeholderAttributedString: NSAttributedString?
  @available(OSX 10.5, *)
  func setWantsNotificationForMarkedText(_ flag: Bool)
  @available(OSX 10.5, *)
  var allowedInputSourceLocales: [String]?
}
struct __tfFlags {
  var drawsBackground: UInt32
  var bezelStyle: UInt32
  var thcSortDirection: UInt32
  var thcSortPriority: UInt32
  var mini: UInt32
  var textColorIgnoresNormalDisableFlag: UInt32
  var textColorDisableFlag: UInt32
  var thcForceHighlightForSort: UInt32
  var invalidTextColor: UInt32
  var notificationForMarkedText: UInt32
  var inToolbar: UInt32
  var hasTextLayer: UInt32
  var isButtonTitle: UInt32
  var allowTightening: UInt32
  var thcHighlighted: UInt32
  var shouldNotClipToBounds: UInt32
  var allowsDefaultTightening: UInt32
  var reservedTextFieldCell: UInt32
  init()
  init(drawsBackground drawsBackground: UInt32, bezelStyle bezelStyle: UInt32, thcSortDirection thcSortDirection: UInt32, thcSortPriority thcSortPriority: UInt32, mini mini: UInt32, textColorIgnoresNormalDisableFlag textColorIgnoresNormalDisableFlag: UInt32, textColorDisableFlag textColorDisableFlag: UInt32, thcForceHighlightForSort thcForceHighlightForSort: UInt32, invalidTextColor invalidTextColor: UInt32, notificationForMarkedText notificationForMarkedText: UInt32, inToolbar inToolbar: UInt32, hasTextLayer hasTextLayer: UInt32, isButtonTitle isButtonTitle: UInt32, allowTightening allowTightening: UInt32, thcHighlighted thcHighlighted: UInt32, shouldNotClipToBounds shouldNotClipToBounds: UInt32, allowsDefaultTightening allowsDefaultTightening: UInt32, reservedTextFieldCell reservedTextFieldCell: UInt32)
}
