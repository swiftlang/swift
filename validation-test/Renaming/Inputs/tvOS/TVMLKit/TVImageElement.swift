
@available(tvOS 9.0, *)
enum TVImageType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case image
  case fullscreen
  case decoration
  case hero
}
@available(tvOS 9.0, *)
class TVImageElement : TVViewElement {
  var url: NSURL? { get }
  var srcset: [String : NSURL]? { get }
  var imageType: TVImageType { get }
}
