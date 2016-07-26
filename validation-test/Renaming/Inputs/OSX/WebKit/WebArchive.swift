
let WebArchivePboardType: String
class WebArchive : NSObject, NSCoding, NSCopying {
  init!(mainResource mainResource: WebResource!, subresources subresources: [AnyObject]!, subframeArchives subframeArchives: [AnyObject]!)
  init!(data data: NSData!)
  var mainResource: WebResource! { get }
  var subresources: [AnyObject]! { get }
  var subframeArchives: [AnyObject]! { get }
  @NSCopying var data: NSData! { get }
}
