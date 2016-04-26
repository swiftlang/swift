
@available(OSX 10.10, *)
class MCAdvertiserAssistant : NSObject {
  init(serviceType serviceType: String, discoveryInfo info: [String : String]?, session session: MCSession)
  func start()
  func stop()
  weak var delegate: @sil_weak MCAdvertiserAssistantDelegate?
  var session: MCSession { get }
  var discoveryInfo: [String : String]? { get }
  var serviceType: String { get }
}
protocol MCAdvertiserAssistantDelegate : NSObjectProtocol {
  @available(OSX 10.10, *)
  optional func advertiserAssistantWillPresentInvitation(_ advertiserAssistant: MCAdvertiserAssistant)
  @available(OSX 10.10, *)
  optional func advertiserAssistantDidDismissInvitation(_ advertiserAssistant: MCAdvertiserAssistant)
}
