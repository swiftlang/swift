
@available(OSX 10.11, *)
class CKFetchWebAuthTokenOperation : CKDatabaseOperation {
  init(apiToken APIToken: String)
  var apiToken: String
  var fetchWebAuthTokenCompletionBlock: ((String, NSError) -> Void)?
}
