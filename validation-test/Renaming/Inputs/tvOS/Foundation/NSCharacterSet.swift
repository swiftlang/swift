
var NSOpenStepUnicodeReservedBase: Int { get }
class NSCharacterSet : NSObject, NSCopying, NSMutableCopying, NSCoding {
  @discardableResult
  class func controlCharacters() -> NSCharacterSet
  @discardableResult
  class func whitespaces() -> NSCharacterSet
  @discardableResult
  class func whitespacesAndNewlines() -> NSCharacterSet
  @discardableResult
  class func decimalDigits() -> NSCharacterSet
  @discardableResult
  class func letters() -> NSCharacterSet
  @discardableResult
  class func lowercaseLetters() -> NSCharacterSet
  @discardableResult
  class func uppercaseLetters() -> NSCharacterSet
  @discardableResult
  class func nonBaseCharacters() -> NSCharacterSet
  @discardableResult
  class func alphanumerics() -> NSCharacterSet
  @discardableResult
  class func decomposables() -> NSCharacterSet
  @discardableResult
  class func illegalCharacters() -> NSCharacterSet
  @discardableResult
  class func punctuation() -> NSCharacterSet
  @discardableResult
  class func capitalizedLetters() -> NSCharacterSet
  @discardableResult
  class func symbols() -> NSCharacterSet
  @available(tvOS 2.0, *)
  @discardableResult
  class func newlines() -> NSCharacterSet
  /*not inherited*/ init(range aRange: NSRange)
  /*not inherited*/ init(charactersIn aString: String)
  /*not inherited*/ init(bitmapRepresentation data: NSData)
  /*not inherited*/ init?(contentsOfFile fName: String)
  @discardableResult
  func characterIsMember(_ aCharacter: unichar) -> Bool
  @NSCopying var bitmapRepresentation: NSData { get }
  @NSCopying var inverted: NSCharacterSet { get }
  @discardableResult
  func longCharacterIsMember(_ theLongChar: UTF32Char) -> Bool
  @discardableResult
  func isSuperset(of theOtherSet: NSCharacterSet) -> Bool
  @discardableResult
  func hasMemberInPlane(_ thePlane: UInt8) -> Bool
}
class NSMutableCharacterSet : NSCharacterSet, NSCopying, NSMutableCopying {
  func addCharacters(in aRange: NSRange)
  func removeCharacters(in aRange: NSRange)
  func addCharacters(in aString: String)
  func removeCharacters(in aString: String)
  func formUnion(with otherSet: NSCharacterSet)
  func formIntersection(with otherSet: NSCharacterSet)
  func invert()
}
