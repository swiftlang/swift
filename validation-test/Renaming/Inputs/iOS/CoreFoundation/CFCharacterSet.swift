
class CFCharacterSet {
}
class CFMutableCharacterSet {
}
enum CFCharacterSetPredefinedSet : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case control
  case whitespace
  case whitespaceAndNewline
  case decimalDigit
  case letter
  case lowercaseLetter
  case uppercaseLetter
  case nonBase
  case decomposable
  case alphaNumeric
  case punctuation
  case capitalizedLetter
  case symbol
  @available(iOS 2.0, *)
  case newline
  case illegal
}
@discardableResult
func CFCharacterSetGetTypeID() -> CFTypeID
@discardableResult
func CFCharacterSetGetPredefined(_ theSetIdentifier: CFCharacterSetPredefinedSet) -> CFCharacterSet!
@discardableResult
func CFCharacterSetCreateWithCharactersInRange(_ alloc: CFAllocator!, _ theRange: CFRange) -> CFCharacterSet!
@discardableResult
func CFCharacterSetCreateWithCharactersInString(_ alloc: CFAllocator!, _ theString: CFString!) -> CFCharacterSet!
@discardableResult
func CFCharacterSetCreateWithBitmapRepresentation(_ alloc: CFAllocator!, _ theData: CFData!) -> CFCharacterSet!
@discardableResult
func CFCharacterSetCreateInvertedSet(_ alloc: CFAllocator!, _ theSet: CFCharacterSet!) -> CFCharacterSet!
@discardableResult
func CFCharacterSetIsSupersetOfSet(_ theSet: CFCharacterSet!, _ theOtherset: CFCharacterSet!) -> Bool
@discardableResult
func CFCharacterSetHasMemberInPlane(_ theSet: CFCharacterSet!, _ thePlane: CFIndex) -> Bool
@discardableResult
func CFCharacterSetCreateMutable(_ alloc: CFAllocator!) -> CFMutableCharacterSet!
@discardableResult
func CFCharacterSetCreateCopy(_ alloc: CFAllocator!, _ theSet: CFCharacterSet!) -> CFCharacterSet!
@discardableResult
func CFCharacterSetCreateMutableCopy(_ alloc: CFAllocator!, _ theSet: CFCharacterSet!) -> CFMutableCharacterSet!
@discardableResult
func CFCharacterSetIsCharacterMember(_ theSet: CFCharacterSet!, _ theChar: UniChar) -> Bool
@discardableResult
func CFCharacterSetIsLongCharacterMember(_ theSet: CFCharacterSet!, _ theChar: UTF32Char) -> Bool
@discardableResult
func CFCharacterSetCreateBitmapRepresentation(_ alloc: CFAllocator!, _ theSet: CFCharacterSet!) -> CFData!
func CFCharacterSetAddCharactersInRange(_ theSet: CFMutableCharacterSet!, _ theRange: CFRange)
func CFCharacterSetRemoveCharactersInRange(_ theSet: CFMutableCharacterSet!, _ theRange: CFRange)
func CFCharacterSetAddCharactersInString(_ theSet: CFMutableCharacterSet!, _ theString: CFString!)
func CFCharacterSetRemoveCharactersInString(_ theSet: CFMutableCharacterSet!, _ theString: CFString!)
func CFCharacterSetUnion(_ theSet: CFMutableCharacterSet!, _ theOtherSet: CFCharacterSet!)
func CFCharacterSetIntersect(_ theSet: CFMutableCharacterSet!, _ theOtherSet: CFCharacterSet!)
func CFCharacterSetInvert(_ theSet: CFMutableCharacterSet!)
