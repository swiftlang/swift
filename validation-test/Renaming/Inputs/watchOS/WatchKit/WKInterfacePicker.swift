
@available(watchOS 2.0, *)
class WKInterfacePicker : WKInterfaceObject {
  func focus()
  func resignFocus()
  func setSelectedItemIndex(_ itemIndex: Int)
  func setItems(_ items: [WKPickerItem]?)
  func setCoordinatedAnimations(_ coordinatedAnimations: [WKInterfaceObject]?)
  func setEnabled(_ enabled: Bool)
}
@available(watchOS 2.0, *)
class WKPickerItem : NSObject, NSSecureCoding {
  var title: String?
  var caption: String?
  @NSCopying var accessoryImage: WKImage?
  @NSCopying var contentImage: WKImage?
}
