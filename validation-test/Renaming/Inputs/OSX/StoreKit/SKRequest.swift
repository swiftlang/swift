
@available(OSX 10.7, *)
class SKRequest : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged SKRequestDelegate?
  func cancel()
  func start()
}
protocol SKRequestDelegate : NSObjectProtocol {
  @available(OSX 10.7, *)
  optional func requestDidFinish(_ request: SKRequest)
  @available(OSX 10.7, *)
  optional func request(_ request: SKRequest, didFailWithError error: NSError?)
}
