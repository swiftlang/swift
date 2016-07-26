
class WebResource : NSObject, NSCoding, NSCopying {
  init!(data data: NSData!, url URL: NSURL!, mimeType MIMEType: String!, textEncodingName textEncodingName: String!, frameName frameName: String!)
  @NSCopying var data: NSData! { get }
  var url: NSURL! { get }
  var mimeType: String! { get }
  var textEncodingName: String! { get }
  var frameName: String! { get }
}
