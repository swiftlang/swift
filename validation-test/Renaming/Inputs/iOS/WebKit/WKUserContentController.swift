
@available(iOS 8.0, *)
class WKUserContentController : NSObject {
  var userScripts: [WKUserScript] { get }
  func addUserScript(_ userScript: WKUserScript)
  func removeAllUserScripts()
  func add(_ scriptMessageHandler: WKScriptMessageHandler, name name: String)
  func removeScriptMessageHandler(forName name: String)
}
