
@available(OSX 10.8, *)
class NSTextAlternatives : NSObject {
  init(primaryString primaryString: String, alternativeStrings alternativeStrings: [String])
  var primaryString: String { get }
  var alternativeStrings: [String] { get }
  func noteSelectedAlternativeString(_ alternativeString: String)
}
@available(OSX 10.8, *)
let NSTextAlternativesSelectedAlternativeStringNotification: String
