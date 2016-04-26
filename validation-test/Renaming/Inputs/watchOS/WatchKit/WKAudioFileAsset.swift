
@available(watchOS 2.0, *)
class WKAudioFileAsset : NSObject {
  convenience init(url URL: NSURL)
  convenience init(url URL: NSURL, title title: String?, albumTitle albumTitle: String?, artist artist: String?)
  var url: NSURL { get }
  var duration: NSTimeInterval { get }
  var title: String? { get }
  var albumTitle: String? { get }
  var artist: String? { get }
}
