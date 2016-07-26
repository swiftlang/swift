
@available(OSX 10.10, *)
class NCWidgetSearchViewController : NSViewController {
  @IBOutlet weak var delegate: @sil_weak NCWidgetSearchViewDelegate!
  var searchResults: [AnyObject]!
  var searchDescription: String!
  var searchResultsPlaceholderString: String!
  var searchResultKeyPath: String!
}
protocol NCWidgetSearchViewDelegate : NSObjectProtocol {
  @available(OSX 10.10, *)
  func widgetSearch(_ controller: NCWidgetSearchViewController!, searchForTerm searchTerm: String!, maxResults max: Int)
  @available(OSX 10.10, *)
  func widgetSearchTermCleared(_ controller: NCWidgetSearchViewController!)
  @available(OSX 10.10, *)
  func widgetSearch(_ controller: NCWidgetSearchViewController!, resultSelected object: AnyObject!)
}
