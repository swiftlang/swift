
@available(OSX 10.9, *)
class CBCentral : NSObject, NSCopying {
  var identifier: NSUUID { get }
  var maximumUpdateValueLength: Int { get }
}
