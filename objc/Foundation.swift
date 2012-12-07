import Foundation // Clang module

func [asmname="swift_StringToNSString"]
convertStringToNSString(string : [byref] String) -> NSString

func [asmname="swift_NSStringToString"]
convertNSStringToString(nsstring : NSString, string : [byref] String)

extension String {
  /// \brief Convert from a Swift String to an Objective-C NSString
  func [conversion] __conversion() -> NSString {
    return convertStringToNSString(&this)
  }
}

extension NSString {
  /// \brief Convert from an Objective-C NSString to a Swift string.
  func [conversion] __conversion() -> String {
    var result : String
    convertNSStringToString(this, &result)
    return result
  }

  /// \brief Construct an NSString from a string literal.
  // FIXME: Fast-path in compiler/runtime to build a CFConstantString
  static func convertFromStringLiteral(s : String) -> NSString {
    return s
  }

  /// \brief Print NSStrings in the repl.
  func replPrint() {
    print(String(this))
  }
}
