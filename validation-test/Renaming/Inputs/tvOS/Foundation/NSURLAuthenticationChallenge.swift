
protocol NSURLAuthenticationChallengeSender : NSObjectProtocol {
  func use(_ credential: NSURLCredential, for challenge: NSURLAuthenticationChallenge)
  func continueWithoutCredential(for challenge: NSURLAuthenticationChallenge)
  func cancel(_ challenge: NSURLAuthenticationChallenge)
  optional func performDefaultHandling(for challenge: NSURLAuthenticationChallenge)
  optional func rejectProtectionSpaceAndContinue(with challenge: NSURLAuthenticationChallenge)
}
class NSURLAuthenticationChallenge : NSObject, NSSecureCoding {
  init(protectionSpace space: NSURLProtectionSpace, proposedCredential credential: NSURLCredential?, previousFailureCount previousFailureCount: Int, failureResponse response: NSURLResponse?, error error: NSError?, sender sender: NSURLAuthenticationChallengeSender)
  init(authenticationChallenge challenge: NSURLAuthenticationChallenge, sender sender: NSURLAuthenticationChallengeSender)
  @NSCopying var protectionSpace: NSURLProtectionSpace { get }
  @NSCopying var proposedCredential: NSURLCredential? { get }
  var previousFailureCount: Int { get }
  @NSCopying var failureResponse: NSURLResponse? { get }
  @NSCopying var error: NSError? { get }
  var sender: NSURLAuthenticationChallengeSender? { get }
}
