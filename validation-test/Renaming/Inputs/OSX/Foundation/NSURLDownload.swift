
class NSURLDownload : NSObject {
  @discardableResult
  class func canResumeDownloadDecoded(withEncodingMIMEType MIMEType: String) -> Bool
  @available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSession downloadTask (see NSURLSession.h)")
  init(request request: NSURLRequest, delegate delegate: NSURLDownloadDelegate?)
  @available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSession downloadTask (see NSURLSession.h)")
  init(resumeData resumeData: NSData, delegate delegate: NSURLDownloadDelegate?, path path: String)
  func cancel()
  func setDestination(_ path: String, allowOverwrite allowOverwrite: Bool)
  @NSCopying var request: NSURLRequest { get }
  @NSCopying var resumeData: NSData? { get }
  var deletesFileUponFailure: Bool
}
protocol NSURLDownloadDelegate : NSObjectProtocol {
  optional func downloadDidBegin(_ download: NSURLDownload)
  @discardableResult
  optional func download(_ download: NSURLDownload, willSend request: NSURLRequest, redirectResponse redirectResponse: NSURLResponse?) -> NSURLRequest?
  @discardableResult
  optional func download(_ connection: NSURLDownload, canAuthenticateAgainstProtectionSpace protectionSpace: NSURLProtectionSpace) -> Bool
  optional func download(_ download: NSURLDownload, didReceive challenge: NSURLAuthenticationChallenge)
  optional func download(_ download: NSURLDownload, didCancel challenge: NSURLAuthenticationChallenge)
  @discardableResult
  optional func downloadShouldUseCredentialStorage(_ download: NSURLDownload) -> Bool
  optional func download(_ download: NSURLDownload, didReceive response: NSURLResponse)
  optional func download(_ download: NSURLDownload, willResumeWith response: NSURLResponse, fromByte startingByte: Int64)
  optional func download(_ download: NSURLDownload, didReceiveDataOfLength length: Int)
  @discardableResult
  optional func download(_ download: NSURLDownload, shouldDecodeSourceDataOfMIMEType encodingType: String) -> Bool
  optional func download(_ download: NSURLDownload, decideDestinationWithSuggestedFilename filename: String)
  optional func download(_ download: NSURLDownload, didCreateDestination path: String)
  optional func downloadDidFinish(_ download: NSURLDownload)
  optional func download(_ download: NSURLDownload, didFailWithError error: NSError)
}
