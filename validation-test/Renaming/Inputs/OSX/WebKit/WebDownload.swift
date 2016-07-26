
class WebDownload : NSURLDownload {
}
protocol WebDownloadDelegate : NSURLDownloadDelegate {
  @discardableResult
  optional func downloadWindow(forAuthenticationSheet download: WebDownload!) -> NSWindow!
}
