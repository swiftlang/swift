
@available(tvOS 9.2, *)
class CKFetchWebAuthTokenOperation : CKDatabaseOperation {
  init(apiToken APIToken: String)
  var apiToken: String
  var fetchWebAuthTokenCompletionBlock: ((String, NSError) -> Void)?
}
