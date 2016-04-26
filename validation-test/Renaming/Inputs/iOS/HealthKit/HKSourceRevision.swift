
@available(iOS 9.0, *)
class HKSourceRevision : NSObject, NSSecureCoding, NSCopying {
  var source: HKSource { get }
  var version: String? { get }
  init(source source: HKSource, version version: String)
}
