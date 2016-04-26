
@available(OSX 10.11.4, *)
enum MKSearchCompletionFilterType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case locationsAndQueries
  case locationsOnly
}
@available(OSX 10.11.4, *)
class MKLocalSearchCompleter : NSObject {
  var queryFragment: String
  var region: MKCoordinateRegion
  var filterType: MKSearchCompletionFilterType
  weak var delegate: @sil_weak MKLocalSearchCompleterDelegate?
  var results: [MKLocalSearchCompletion] { get }
  var isSearching: Bool { get }
  func cancel()
}
protocol MKLocalSearchCompleterDelegate : NSObjectProtocol {
  @available(OSX 10.11.4, *)
  optional func completerDidUpdateResults(_ completer: MKLocalSearchCompleter)
  @available(OSX 10.11.4, *)
  optional func completer(_ completer: MKLocalSearchCompleter, didFailWithError error: NSError)
}
@available(OSX 10.11.4, *)
class MKLocalSearchCompletion : NSObject {
  var title: String { get }
  var titleHighlightRanges: [NSValue] { get }
  var subtitle: String { get }
  var subtitleHighlightRanges: [NSValue] { get }
}
extension MKLocalSearchRequest {
  @available(OSX 10.11.4, *)
  init(completion completion: MKLocalSearchCompletion)
}
