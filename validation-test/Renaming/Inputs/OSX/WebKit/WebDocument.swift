
protocol WebDocumentView : NSObjectProtocol {
  func setDataSource(_ dataSource: WebDataSource!)
  func dataSourceUpdated(_ dataSource: WebDataSource!)
  func setNeedsLayout(_ flag: Bool)
  func layout()
  func viewWillMove(toHostWindow hostWindow: NSWindow!)
  func viewDidMoveToHostWindow()
}
protocol WebDocumentSearching : NSObjectProtocol {
  @discardableResult
  func search(for string: String!, direction forward: Bool, caseSensitive caseFlag: Bool, wrap wrapFlag: Bool) -> Bool
}
protocol WebDocumentText : NSObjectProtocol {
  @discardableResult
  func supportsTextEncoding() -> Bool
  @discardableResult
  func string() -> String!
  @available(OSX 10.0, *)
  @discardableResult
  func attributedString() -> NSAttributedString!
  @discardableResult
  func selectedString() -> String!
  @available(OSX 10.0, *)
  @discardableResult
  func selectedAttributedString() -> NSAttributedString!
  func selectAll()
  func deselectAll()
}
protocol WebDocumentRepresentation : NSObjectProtocol {
  func setDataSource(_ dataSource: WebDataSource!)
  func receivedData(_ data: NSData!, with dataSource: WebDataSource!)
  func receivedError(_ error: NSError!, with dataSource: WebDataSource!)
  func finishedLoading(with dataSource: WebDataSource!)
  @discardableResult
  func canProvideDocumentSource() -> Bool
  @discardableResult
  func documentSource() -> String!
  @discardableResult
  func title() -> String!
}
