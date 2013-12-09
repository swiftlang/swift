/// \brief Exposing the API of NSString on Swift's String

extension String {

  //
  // Class Methods
  //
  
  // + (const NSStringEncoding *)availableStringEncodings
  
  /// \brief returns an Array of the encodings string objects support
  /// in the application’s environment
  static func availableStringEncodings() -> NSStringEncoding[] {
    var result = Array<NSStringEncoding>()
    var p = NSString.availableStringEncodings()
    while p.get() != 0 {
      result.append(p.get())
      ++p
    }
    return result
  }

  // + (NSStringEncoding)defaultCStringEncoding

  /// \brief Returns the C-string encoding assumed for any method
  /// accepting a C string as an argument.
  static func defaultCStringEncoding() -> NSStringEncoding {
    return NSString.defaultCStringEncoding()
  }

  // + (NSString *)localizedNameOfStringEncoding:(NSStringEncoding)encoding

  /// \brief Returns a human-readable string giving the name of a
  /// given encoding.
  static func localizedNameOfStringEncoding(encoding: NSStringEncoding) -> String {
    return NSString.localizedNameOfStringEncoding(encoding)
  }

  // + (instancetype)localizedStringWithFormat:(NSString *)format, ...

  /// \brief Returns a string created by using a given format string as a
  /// template into which the remaining argument values are substituted
  /// according to the user's default locale.
  static func localizedStringWithFormat(format: String, items: CocoaFormattable...) -> String {
    // FIXME: because of Cocoa's use of varargs here, exposing this
    // from pure Swift code is tricky.  We'll come back to it.
    return ""
  }

  // + (NSString *)pathWithComponents:(NSArray *)components

  /// \brief Returns a string built from the strings in a given array
  /// by concatenating them with a path separator between each pair.
  static func pathWithComponents(components: String[]) -> String {
    var _components = NSMutableArray()
    for c in components {
      _components.addObject(c as NSString)
    }
    return NSString.pathWithComponents(_components)
  }

  // + (instancetype)string

  /// \brief Returns an empty string.
  static func string() -> String {
    return ""
  }

  // + (instancetype)stringWithCharacters:(const unichar *)chars length:(NSUInteger)length

  /// \brief Returns a string containing a given number of characters
  /// taken from a given C array of Unicode characters.
  static func stringWithCharacters(chars: UnsafePointer<unichar>, length: Int) -> String {
    return (NSString.stringWithCharacters(chars, length) as NSString)!
  }


  /*
  // FIXME: this one is complicated by the fact that cocoa can return
  // NULL and that we have an in/out NSError parameter.  We'll come back to it later.
  
  // + (instancetype)stringWithContentsOfFile:(NSString *)path encoding:(NSStringEncoding)enc error:(NSError **)error

  /// \brief Returns a string created by reading data from the file at
  /// a given path interpreted using a given encoding.
  static func stringWithContentsOfFile(path: String, encoding: NSStringEncoding) -> String {
    return NSString.stringWithContentsOfFile(path, encoding, UnsafePointer<NSError>.null())
  }
  
  static func stringWithContentsOfFile(
    path: String,
    encoding: NSStringEncoding,
    error: @inout NSError?)
  {
    // FIXME: 
    return NSString.stringWithContentsOfFile(path, encoding, UnsafePointer<NSError>.null())
  }
  */
}

protocol CocoaFormattable {}
