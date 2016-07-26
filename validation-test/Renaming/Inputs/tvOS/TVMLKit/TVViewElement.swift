
@available(tvOS 9.0, *)
enum TVElementEventType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case play
  case select
  case holdSelect
  case highlight
  case change
}
@available(tvOS 9.0, *)
enum TVElementUpdateType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case subtree
  case children
  case node
}
@available(tvOS 9.0, *)
enum TVElementResettableProperty : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case updateType
  case autoHighlightIdentifier
}
@available(tvOS 9.0, *)
class TVViewElement : NSObject, NSCopying {
  var identifier: String { get }
  var name: String { get }
  weak var parent: @sil_weak TVViewElement? { get }
  var children: [TVViewElement]? { get }
  var attributes: [String : String]? { get }
  var style: TVViewElementStyle? { get }
  var autoHighlightIdentifier: String? { get }
  var isDisabled: Bool
  var updateType: TVElementUpdateType { get }
  @available(tvOS 9.0, *)
  func resetProperty(_ resettableProperty: TVElementResettableProperty)
}
