
@available(tvOS 9.0, *)
enum TVTopShelfContentStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case inset
  case sectioned
}
protocol TVTopShelfProvider {
  @available(tvOS 9.0, *)
  var topShelfStyle: TVTopShelfContentStyle { get }
  @available(tvOS 9.0, *)
  var topShelfItems: [TVContentItem] { get }
}
@available(tvOS 9.0, *)
let TVTopShelfItemsDidChangeNotification: String
@available(tvOS 9.0, *)
@discardableResult
func TVTopShelfImageSize(shape shape: TVContentItemImageShape, style style: TVTopShelfContentStyle) -> CGSize
