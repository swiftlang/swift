
@available(tvOS 9.0, *)
class TVContentIdentifier : NSObject, NSCopying, NSSecureCoding {
  var identifier: String { get }
  @NSCopying var container: TVContentIdentifier? { get }
  init?(identifier identifier: String, container container: TVContentIdentifier?)
}
