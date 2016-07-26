
enum UITextAutocapitalizationType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case words
  case sentences
  case allCharacters
}
enum UITextAutocorrectionType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case no
  case yes
}
@available(iOS 5.0, *)
enum UITextSpellCheckingType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case no
  case yes
}
enum UIKeyboardType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case asciiCapable
  case numbersAndPunctuation
  case URL
  case numberPad
  case phonePad
  case namePhonePad
  case emailAddress
  @available(iOS 4.1, *)
  case decimalPad
  @available(iOS 5.0, *)
  case twitter
  @available(iOS 7.0, *)
  case webSearch
  static var alphabet: UIKeyboardType { get }
}
enum UIKeyboardAppearance : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  @available(iOS 7.0, *)
  case dark
  @available(iOS 7.0, *)
  case light
  static var alert: UIKeyboardAppearance { get }
}
enum UIReturnKeyType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case go
  case google
  case join
  case next
  case route
  case search
  case send
  case yahoo
  case done
  case emergencyCall
  @available(iOS 9.0, *)
  case `continue`
}
protocol UITextInputTraits : NSObjectProtocol {
  optional var autocapitalizationType: UITextAutocapitalizationType { get set }
  optional var autocorrectionType: UITextAutocorrectionType { get set }
  @available(iOS 5.0, *)
  optional var spellCheckingType: UITextSpellCheckingType { get set }
  optional var keyboardType: UIKeyboardType { get set }
  optional var keyboardAppearance: UIKeyboardAppearance { get set }
  optional var returnKeyType: UIReturnKeyType { get set }
  optional var enablesReturnKeyAutomatically: Bool { get set }
  optional var isSecureTextEntry: Bool { get set }
}
