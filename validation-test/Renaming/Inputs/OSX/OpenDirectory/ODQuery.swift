
protocol ODQueryDelegate : NSObjectProtocol {
  @available(OSX 10.6, *)
  func query(_ inQuery: ODQuery!, foundResults inResults: [AnyObject]!, error inError: NSError!)
}
class ODQuery : NSObject, NSCopying {
  @available(OSX 10.6, *)
  init(node inNode: ODNode!, forRecordTypes inRecordTypeOrList: AnyObject!, attribute inAttribute: String!, matchType inMatchType: ODMatchType, queryValues inQueryValueOrList: AnyObject!, returnAttributes inReturnAttributeOrList: AnyObject!, maximumResults inMaximumResults: Int) throws
  @available(OSX 10.6, *)
  @discardableResult
  func resultsAllowingPartial(_ inAllowPartialResults: Bool) throws -> [AnyObject]
  @available(OSX 10.6, *)
  unowned(unsafe) var delegate: @sil_unmanaged ODQueryDelegate!
  @available(OSX 10.6, *)
  func schedule(in inRunLoop: NSRunLoop!, forMode inMode: String!)
  @available(OSX 10.6, *)
  func remove(from inRunLoop: NSRunLoop!, forMode inMode: String!)
  @available(OSX 10.6, *)
  func synchronize()
  @available(OSX 10.6, *)
  var operationQueue: NSOperationQueue!
}
