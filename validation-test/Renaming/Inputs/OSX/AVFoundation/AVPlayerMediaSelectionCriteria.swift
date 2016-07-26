
@available(OSX 10.9, *)
class AVPlayerMediaSelectionCriteria : NSObject {
  var preferredLanguages: [String]? { get }
  var preferredMediaCharacteristics: [String]? { get }
  init(preferredLanguages preferredLanguages: [String]?, preferredMediaCharacteristics preferredMediaCharacteristics: [String]?)
}
