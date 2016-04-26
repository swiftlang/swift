
@available(OSX 10.10, *)
class WKBackForwardListItem : NSObject {
  @NSCopying var url: NSURL { get }
  var title: String? { get }
  @NSCopying var initialURL: NSURL { get }
}
