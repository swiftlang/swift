//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// \brief Exposing the API of NSString on Swift's String

// Open Issues
// ===========
//
// Some APIs traffic unavoidably in unsafe pointers.  What do we do about those?
//
// Property Lists need to be properly bridged
//
// It would be great to be able to define conversion operators TO
// NSArray and NSDictionary.  Currently the language only supports
// user-defined conversions FROM a type.
//
// What about bridging NSRange and Range<Int>?

extension NSArray {
  func _toArray<T>(body: (AnyObject) -> T) -> T[] {
    var length: Int = count
    var result = T[]()
    result.reserve(length)
    for i in 0...length {
      result.append(body(objectAtIndex(i)))
    }
    return result
  }

  func _toStringArray() -> String[] {
    return _toArray{ String(($0 as NSString)!) }
  }

  func _toRangeArray() -> NSRange[] {
    return _toArray{ ($0 as NSValue)!.rangeValue }
  }
}

func _toNSArray<T, U:AnyObject>(a: T[], f: (T)->U) -> NSArray {
  var result = NSMutableArray(capacity: a.count)
  for s in a {
    result.addObject(f(s))
  }
  return result
}

func _toNSArray(a: String[]) -> NSArray {
  return _toNSArray(a){ $0 as NSString }
}

func _countFormatSpecifiers(a: String) -> Int {
  var lastChar = _asUTF16CodeUnit(".") // anything other than % would work here
  var count = 0
  
  for c in a.unicodeScalars {
    if lastChar == _asUTF16CodeUnit("%") {
      if c == "%" {
        lastChar = _asUTF16CodeUnit(".") // a "%" following this one should not be taken as literal
      }
      else {
        ++count
        lastChar = _asUTF16CodeUnit(c)
      }
    } else {
      lastChar = _asUTF16CodeUnit(c)
    }
  }
  return count
}

extension String {

  /// \brief the corresponding NSString - a convenience for bridging code
  var _ns: NSString {
    return self as NSString
  }
  
  //
  // Class Methods
  //
  
  // + (const NSStringEncoding *)availableStringEncodings
  
  /// \brief returns an Array of the encodings string objects support
  /// in the application’s environment
  static func availableStringEncodings() -> NSStringEncoding[] {
    var result = NSStringEncoding[]()
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
  static func localizedStringWithFormat(format: String, _ arguments: CVarArg...) -> String {
    return String(format: format, arguments: arguments)
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
  /// taken from a given array of Unicode characters.
  static func stringWithCharacters(chars: unichar[]) -> String {
    var local = chars
    return NSString(characters: local, length: local.count)
  }

  // + (instancetype)stringWithContentsOfFile:(NSString *)path encoding:(NSStringEncoding)enc error:(NSError **)error

  /// \brief Returns a string created by reading data from the file at
  /// a given path interpreted using a given encoding.  Errors are
  /// written into the inout error argument
  static func stringWithContentsOfFile(
    path: String,
    encoding: NSStringEncoding,
    inout error: NSError?) -> String?
  {
    var result = NSString.stringWithContentsOfFile(path, encoding: encoding,
                                                   error: &error)
    
    return (result as NSString?).map { $0 }
  }

  // The Cocoa API allows passing NULL for the error if you want to
  // ignore it.  We provide an overload for that purpose here.
  
  /// \brief Returns a string created by reading data from the file at
  /// a given path interpreted using a given encoding.
  static func stringWithContentsOfFile(
    path: String, encoding: NSStringEncoding) -> String? {
    var ignoredError: NSError? = .None
    return stringWithContentsOfFile(path, encoding: encoding,
                                    error: &ignoredError)
  }

  // + (instancetype)stringWithContentsOfFile:(NSString *)path usedEncoding:(NSStringEncoding *) enc error:(NSError **)error

  /// \brief Returns a string created by reading data from the file at
  /// a given path and returns by reference the encoding used to
  /// interpret the file. Errors are written into the inout error
  /// argument
  static func stringWithContentsOfFile(
    path: String,
    inout usedEncoding: NSStringEncoding,
    inout error: NSError?) -> String?
  {
    var result = NSString.stringWithContentsOfFile(
      path, usedEncoding: &usedEncoding, error: &error
    )
    return (result as NSString?).map { $0 }
  }

  // The Cocoa API allows passing NULL for the error if you want to
  // ignore it.  We provide an overload for that purpose here.
  
  /// \brief Returns a string created by reading data from the file at
  /// a given path and returns by reference the encoding used to
  /// interpret the file.
  static func stringWithContentsOfFile(
    path: String, inout usedEncoding: NSStringEncoding) -> String? {
    var ignoredError: NSError? = .None
    return String.stringWithContentsOfFile(path, usedEncoding: &usedEncoding, 
                                           error: &ignoredError)
  }

  // + (instancetype)stringWithContentsOfURL:(NSURL *)url encoding:(NSStringEncoding)enc error:(NSError **)error

  /// \brief Returns a string created by reading data from a given URL
  /// interpreted using a given encoding. Errors are written into the
  /// inout error argument
  static func stringWithContentsOfURL(
    url: NSURL, encoding: NSStringEncoding, inout error: NSError?
  ) -> String? {
    var result = NSString.stringWithContentsOfURL(url, encoding: encoding, 
                                                  error: &error)
    return (result as NSString?).map { $0 }
  }

  // The Cocoa API allows passing NULL for the error if you want to
  // ignore it.  We provide an overload for that purpose here.

  /// \brief Returns a string created by reading data from a given URL
  /// interpreted using a given encoding. 
  static func stringWithContentsOfURL(
    url: NSURL, encoding: NSStringEncoding
  ) -> String? {
    var ignoredError: NSError? = .None
    return stringWithContentsOfURL(url, encoding: encoding, 
                                   error: &ignoredError)
  }

  // + (instancetype)stringWithContentsOfURL:(NSURL *)url usedEncoding:(NSStringEncoding *)enc error:(NSError **)error

  /// \brief Returns a string created by reading data from a given URL
  /// and returns by reference the encoding used to interpret the
  /// data.  Errors are written into the inout error argument
  static func stringWithContentsOfURL(
    url: NSURL,
    inout usedEncoding: NSStringEncoding,
    inout error: NSError?) -> String?
  {
    var result =
      NSString.stringWithContentsOfURL(url, usedEncoding: &usedEncoding, 
                                       error: &error)
    return (result as NSString?).map { $0 }
  }

  // The Cocoa API allows passing NULL for the error if you want to
  // ignore it.  We provide an overload for that purpose here.

  /// \brief Returns a string created by reading data from a given URL
  /// and returns by reference the encoding used to interpret the
  /// data.  
  static func stringWithContentsOfURL(
    url: NSURL,
    inout usedEncoding: NSStringEncoding
  ) -> String?
  {
    var ignoredError: NSError? = .None
    return stringWithContentsOfURL(url, usedEncoding: &usedEncoding, 
                                   error: &ignoredError)
  }


  // + (instancetype)stringWithCString:(const char *)cString encoding:(NSStringEncoding)enc

  /// \brief Returns a string containing the bytes in a given C array,
  /// interpreted according to a given encoding.
  static func stringWithCString(
    s: CString, encoding: NSStringEncoding) -> String {

    return NSString(CString:s, encoding: encoding)
  }

  // + (instancetype)stringWithFormat:(NSString *)format,, ...
  
  /// \brief Returns a string created by using a given format string
  /// as a template into which the remaining argument values are
  /// substituted.
  static func stringWithFormat(
    format: String, _ arguments: CVarArg...) -> String {
    return String(format: format, arguments: arguments)
  }

  // + (instancetype)stringWithString:(NSString *)aString

  /// \brief Returns a string created by copying the characters from
  /// another given string.
  static func stringWithString(aString: String) -> String {
    return aString
  }

  // + (instancetype)stringWithUTF8String:(const char *)bytes
  
  /// \brief Returns a string created by copying the data from a given
  /// C array of UTF8-encoded bytes.
  static func stringWithUTF8String(bytes: CString) -> String {
    return NSString(UTF8String: bytes)
  }

  //
  // Instance Methods
  //

  // @property BOOL boolValue;

  /// \brief Returns the Boolean value of the receiver’s text.
  var boolValue: Bool {
    return _ns.boolValue
  }

  // - (BOOL)canBeConvertedToEncoding:(NSStringEncoding)encoding

  /// \brief Returns a Boolean value that indicates whether the
  /// receiver can be converted to a given encoding without loss of
  /// information.
  func canBeConvertedToEncoding(encoding: NSStringEncoding) -> Bool {
    return _ns.canBeConvertedToEncoding(encoding)
  }

  // @property NSString* capitalizedString

  /// \brief Produce a string with the first character from each word changed
  /// to the corresponding uppercase value.
  var capitalizedString: String {
    return _ns.capitalizedString as String
  }

  // - (NSString *)capitalizedStringWithLocale:(NSLocale *)locale

  /// \brief Returns a capitalized representation of the receiver
  /// using the specified locale.
  func capitalizedStringWithLocale(locale: NSLocale) -> String{
    return _ns.capitalizedStringWithLocale(locale) as String
  }


  // - (NSComparisonResult)caseInsensitiveCompare:(NSString *)aString

  /// \brief Returns the result of invoking compare:options: with
  /// NSCaseInsensitiveSearch as the only option.
  func caseInsensitiveCompare(aString: String) -> NSComparisonResult {
    return _ns.caseInsensitiveCompare(aString as NSString)
  }

  // - (unichar)characterAtIndex:(NSUInteger)index

  /// \brief Returns the character at a given array position.
  func characterAtIndex(index: Int) -> unichar {
    return _ns.characterAtIndex(index)
  }

  // - (NSString *)commonPrefixWithString:(NSString *)aString options:(NSStringCompareOptions)mask
  
  /// \brief Returns a string containing characters the receiver and a
  /// given string have in common, starting from the beginning of each
  /// up to the first characters that aren’t equivalent.
  func commonPrefixWithString(
    aString: String, options: NSStringCompareOptions) -> String {
    return _ns.commonPrefixWithString(
      aString as NSString, options: options) as String
  }

  // - (NSComparisonResult)compare:(NSString *)aString
  
  /// \brief Returns the result of invoking compare:options:range:
  /// with no options and the receiver’s full extent as the range.
  func compare(aString: String) -> NSComparisonResult {
    return _ns.compare(aString as NSString)
  }

  // - (NSComparisonResult)compare:(NSString *)aString options:(NSStringCompareOptions)mask

  /// \brief Compares the string with the specified string using the
  /// given options.
  func compare(
    aString: String, options: NSStringCompareOptions) -> NSComparisonResult {
    return _ns.compare(aString as NSString, options: options)
  }

  // - (NSComparisonResult)compare:(NSString *)aString options:(NSStringCompareOptions)mask range:(NSRange)range

  /// \brief Returns the result of invoking
  /// compare:options:range:locale: with a nil locale.
  func compare(
    aString: String,
    options: NSStringCompareOptions,
    range: NSRange
  ) -> NSComparisonResult {
    return _ns.compare(
      aString as NSString, options: options, range: range)
  }

  // - (NSComparisonResult)compare:(NSString *)aString options:(NSStringCompareOptions)mask range:(NSRange)range locale:(id)locale

  /// \brief Compares the string using the specified options and
  /// returns the lexical ordering for the range.
  func compare(
    aString: NSString, options: NSStringCompareOptions,
    range: NSRange, locale: NSLocale) -> NSComparisonResult {
    return _ns.compare(
      aString as NSString, options: options, range: range, locale: locale)
  }

  /// \brief a Helper function for the two completePathIntoString overloads below
  func _completePathIntoString(
    inout outputName: String?, caseSensitive: Bool,
    matchesIntoArray: ObjCMutablePointer<NSArray?>, filterTypes: (String[])?) -> Int {

    var nsOutputName: NSString? = .None

    var result = self._ns.completePathIntoString(
      &nsOutputName,
      caseSensitive: caseSensitive,
      matchesIntoArray: matchesIntoArray,
      filterTypes: filterTypes ? _toNSArray(filterTypes!) : nil
    )
    
    outputName = nsOutputName.map { $0 }
    
    return result
  }
  
  // - (NSUInteger)completePathIntoString:(NSString **)outputName caseSensitive:(BOOL)flag matchesIntoArray:(NSArray **)outputArray filterTypes:(NSArray *)filterTypes

  /// \brief Interprets the receiver as a path in the file system and
  /// attempts to perform filename completion, returning a numeric
  /// value that indicates whether a match was possible, and by
  /// reference the longest path that matches the receiver.
  /// returns the actual number of matching paths.
  func completePathIntoString(
    inout outputName: String?, caseSensitive: Bool,
    inout matchesIntoArray: (String[])?,
    filterTypes: (String[])? = .None
  ) -> Int {

    var nsMatches: NSArray? = .None
    
    var result = _completePathIntoString(
      &outputName,
      caseSensitive: caseSensitive,
      matchesIntoArray: &nsMatches,
      filterTypes: filterTypes
    )

    matchesIntoArray = nsMatches.map { $0._toStringArray() }
    
    return result
  }

  // Cocoa allows us to pass NULL for matchesIntoArray, which
  // implies this overload
  
  /// \brief Interprets the receiver as a path in the file system and
  /// attempts to perform filename completion, returning a numeric
  /// value that indicates whether a match was possible, and by
  /// reference the longest path that matches the receiver.
  /// returns a positive number if any path matched.
  func completePathIntoString(
    inout outputName: String?,
    caseSensitive: Bool,
    filterTypes: (String[])? = .None
  ) -> Int {

    return _completePathIntoString(
      &outputName,
      caseSensitive: caseSensitive,
      matchesIntoArray: nil,
      filterTypes: filterTypes)
  }

  // - (NSArray *)componentsSeparatedByCharactersInSet:(NSCharacterSet *)separator
  
  /// \brief Returns an array containing substrings from the receiver
  /// that have been divided by characters in a given set.
  func componentsSeparatedByCharactersInSet(separator: NSCharacterSet) -> String[] {
    return _ns
    .componentsSeparatedByCharactersInSet(separator)
    ._toStringArray()
  }


  // - (NSArray *)componentsSeparatedByString:(NSString *)separator
  
  /// \brief Returns an array containing substrings from the receiver
  /// that have been divided by a given separator.
  func componentsSeparatedByString(separator: String) -> String[] {
    return _ns
    .componentsSeparatedByString(separator)
    ._toStringArray()
  }

  // - (const char *)cStringUsingEncoding:(NSStringEncoding)encoding

  /// \brief Returns a representation of the receiver as a C string
  /// using a given encoding.

  func cStringUsingEncoding(encoding: NSStringEncoding) -> CChar[] {
    return withExtendedLifetime(_ns) {
      (s: NSString) -> CChar[] in
      s.cStringUsingEncoding(encoding).persist()
    }
  }

  // - (NSData *)dataUsingEncoding:(NSStringEncoding)encoding
  // - (NSData *)dataUsingEncoding:(NSStringEncoding)encoding allowLossyConversion:(BOOL)flag

  /// \brief Returns an NSData object containing a representation of
  /// the receiver encoded using a given encoding.
  func dataUsingEncoding(
    encoding: NSStringEncoding,
    allowLossyConversion: Bool = false
  ) -> NSData {
    return _ns.dataUsingEncoding(encoding, 
                                 allowLossyConversion: allowLossyConversion)
  }

  // @property NSString* decomposedStringWithCanonicalMapping;

  /// \brief Returns a string made by normalizing the receiver’s
  /// contents using Form D.
  var decomposedStringWithCanonicalMapping: String {
    return _ns.decomposedStringWithCanonicalMapping
  }

  // @property NSString* decomposedStringWithCompatibilityMapping;
  
  /// \brief Returns a string made by normalizing the receiver’s
  /// contents using Form KD.
  var decomposedStringWithCompatibilityMapping: String {
    return _ns.decomposedStringWithCompatibilityMapping
  }

  // @property NSString* description

  /// \brief Returns the receiver.
  var description: String {
    return self
  }

  // @property double doubleValue;
  
  /// \brief Returns the floating-point value of the receiver’s text as a double.
  var doubleValue: Double {
    return _ns.doubleValue
  }

  // - (void)enumerateLinesUsingBlock:(void (^)(NSString *line, BOOL *stop))block
  
  /// \brief Enumerates all the lines in a string.
  func enumerateLinesUsingBlock(block: (line: String, inout stop: Bool)->()) {
    _ns.enumerateLinesUsingBlock {
      (line: String?, stop: CMutablePointer<ObjCBool>)
    in
      var stop_ = false
      block(line: line!, stop: &stop_)
      if stop_ {
        UnsafePointer(stop).set(true)
      }
    }
  }

  // - (void)enumerateLinguisticTagsInRange:(NSRange)range scheme:(NSString *)tagScheme options:(NSLinguisticTaggerOptions)opts orthography:(NSOrthography *)orthography usingBlock:(void (^)(NSString *tag, NSRange tokenRange, NSRange sentenceRange, BOOL *stop))block

  
  /// \brief Performs linguistic analysis on the specified string by
  /// enumerating the specific range of the string, providing the
  /// Block with the located tags.
  func enumerateLinguisticTagsInRange(
    range: NSRange, scheme: String, options: NSLinguisticTaggerOptions,
    orthography: NSOrthography?,
    usingBlock: (String, NSRange, NSRange, inout Bool)->()
  ) {
    _ns.enumerateLinguisticTagsInRange(
      range,
      scheme: scheme,
      options: options,
      orthography: orthography ? orthography! : nil
    ) {
      var stop_ = false
      usingBlock($0, $1, $2, &stop_)
      if stop_ {
        UnsafePointer($3).set(true)
      }
    }
  }

  // - (void)enumerateSubstringsInRange:(NSRange)range options:(NSStringEnumerationOptions)opts usingBlock:(void (^)(NSString *substring, NSRange substringRange, NSRange enclosingRange, BOOL *stop))block

  /// \brief Enumerates the substrings of the specified type in the
  /// specified range of the string.
  func enumerateSubstringsInRange(
    range: NSRange, options:NSStringEnumerationOptions,
    usingBlock: (
      substring: String, substringRange: NSRange,
      enclosingRange: NSRange, inout Bool
    )->()
  ) {
    _ns.enumerateSubstringsInRange(range, options: options) {
      var stop_ = false
      usingBlock(substring: $0, substringRange: $1, enclosingRange: $2, &stop_)
      if stop_ {
        UnsafePointer($3).set(true)
      }
    }
  }

  // @property NSStringEncoding fastestEncoding;

  /// \brief Returns the fastest encoding to which the receiver may be
  /// converted without loss of information.
  var fastestEncoding: NSStringEncoding {
    return _ns.fastestEncoding
  }

  // - (const char *)fileSystemRepresentation

  /// \brief Returns a file system-specific representation of the
  /// receiver.
  func fileSystemRepresentation() -> CChar[] {
    return _ns.fileSystemRepresentation.persist()
  }

  // @property float floatValue;
  
  /// \brief Returns the floating-point value of the receiver’s text as a float.
  var floatValue: Float {
    return _ns.floatValue
  }

  // - (BOOL)getBytes:(void *)buffer maxLength:(NSUInteger)maxBufferCount usedLength:(NSUInteger*)usedBufferCount encoding:(NSStringEncoding)encoding options:(NSStringEncodingConversionOptions)options range:(NSRange)range remainingRange:(NSRangePointer)leftover
  
  /// \brief Gets a given range of characters as bytes in a specified
  /// encoding.  Note: will get a maximum of min(buffer.count,
  /// maxLength) bytes.
  func getBytes(
    inout buffer: UInt8[], maxLength: Int, inout usedLength: Int,
    encoding: NSStringEncoding, options: NSStringEncodingConversionOptions,
    range: NSRange, inout remainingRange: NSRange
  ) -> Bool {
    return self._ns.getBytes(
      &buffer,
      maxLength: min(buffer.count, maxLength),
      usedLength: &usedLength, encoding: encoding, options: options,
      range: range, remainingRange: &remainingRange)
  }


  // - (void)getCharacters:(unichar *)buffer range:(NSRange)aRange

  /// \brief Copies characters from a given range in the receiver into
  /// a given buffer. Note: will copy a maximum of min(buffer.count,
  /// range.length) characters
  func getCharacters(inout buffer: unichar[], range: NSRange) {
    let r = NSRange(location: range.location, 
                    length: min(buffer.count, range.length))
    _ns.getCharacters(&buffer, range: r)
  }

  // - (BOOL)getCString:(char *)buffer maxLength:(NSUInteger)maxBufferCount encoding:(NSStringEncoding)encoding

  /// \brief Converts the receiver’s content to a given encoding and
  /// stores them in a buffer. Note: will store a maximum of
  /// min(buffer.count, maxLength) bytes.
  func getCString(
    inout buffer: CChar[], maxLength: Int, encoding: NSStringEncoding
  ) -> Bool {
    return _ns.getCString(&buffer, maxLength: min(buffer.count, maxLength), 
                          encoding: encoding)
  }
  
  // - (BOOL)getFileSystemRepresentation:(char *)buffer maxLength:(NSUInteger)maxLength
  
  /// \brief Interprets the receiver as a system-independent path and
  /// fills a buffer with a C-string in a format and encoding suitable
  /// for use with file-system calls. Note: will store a maximum of
  /// min(buffer.count, maxLength) bytes.
  func getFileSystemRepresentation(
    inout buffer: CChar[], maxLength: Int) -> Bool {
    return _ns.getFileSystemRepresentation(
      &buffer, maxLength: min(buffer.count, maxLength))
  }

  // - (void)getLineStart:(NSUInteger *)startIndex end:(NSUInteger *)lineEndIndex contentsEnd:(NSUInteger *)contentsEndIndex forRange:(NSRange)aRange
  
  /// \brief Returns by reference the beginning of the first line and
  /// the end of the last line touched by the given range.

  func getLineStart(
    inout startIndex: Int, inout end: Int,
    inout contentsEnd: Int, forRange: NSRange
  ) {
    var outArgs: (Int, Int, Int)
    = (startIndex, end, contentsEnd)
    
    withUnsafePointers(&outArgs.0, &outArgs.1, &outArgs.2) {
      self._ns.getLineStart($0, end: $1, contentsEnd: $2, forRange: forRange)
    }
    (startIndex, end, contentsEnd) = outArgs
  }

  // - (void)getParagraphStart:(NSUInteger *)startIndex end:(NSUInteger *)endIndex contentsEnd:(NSUInteger *)contentsEndIndex forRange:(NSRange)aRange

  /// \brief Returns by reference the beginning of the first paragraph
  /// and the end of the last paragraph touched by the given range.

  func getParagraphStart(
    inout startIndex: Int, inout end: Int,
    inout contentsEnd: Int, forRange: NSRange
  ) {
    var outArgs: (Int, Int, Int)
    = (startIndex, end, contentsEnd)
    
    withUnsafePointers(&outArgs.0, &outArgs.1, &outArgs.2) {
      self._ns.getParagraphStart($0, end: $1, contentsEnd: $2, 
                                 forRange: forRange)
    }
    (startIndex, end, contentsEnd) = outArgs
  }

  // - (NSUInteger)hash

  /// \brief Returns an unsigned integer that can be used as a hash
  /// table address.
  ///
  /// FIXME: Should be a property.
  func hash() -> Int {
    return _ns.hash
  }

  // - (BOOL)hasPrefix:(NSString *)aString
  
  /// \brief Returns a Boolean value that indicates whether a given
  /// string matches the beginning characters of the receiver.

  func hasPrefix(aString: String) -> Bool {
    return _ns.hasPrefix(aString)
  }

  // - (BOOL)hasSuffix:(NSString *)aString

  /// \brief Returns a Boolean value that indicates whether a given
  /// string matches the ending characters of the receiver.

  func hasSuffix(aString: String) -> Bool {
    return _ns.hasSuffix(aString)
  }


  /*
  Nothing to do here; already provided for String
  
  // - (instancetype)init

  /// \brief Returns an initialized NSString object that contains no
  /// characters.
  */

  // - (instancetype)initWithBytes:(const void *)bytes length:(NSUInteger)length encoding:(NSStringEncoding)encoding

  /// \brief Returns an initialized NSString object containing a given
  /// number of bytes from a given buffer of bytes interpreted in a
  /// given encoding.  Note: will store a maximum of
  /// min(bytes.count, length) bytes.
  init(inout bytes: UInt8[], length: Int, encoding: NSStringEncoding) {
    self = NSString(
      bytes: bytes, length: min(bytes.count, length),
      encoding: encoding)
  }

  // - (instancetype)initWithBytesNoCopy:(void *)bytes length:(NSUInteger)length encoding:(NSStringEncoding)encoding freeWhenDone:(BOOL)flag
  
  /// \brief Returns an initialized NSString object that contains a
  /// given number of bytes from a given buffer of bytes interpreted
  /// in a given encoding, and optionally frees the buffer.  WARNING:
  /// this method is not memory-safe!
  init(
    bytesNoCopy: COpaquePointer, length: Int,
    encoding: NSStringEncoding, freeWhenDone:Bool
  ) {
    self = NSString(
      bytesNoCopy: bytesNoCopy, length: length,
      encoding: encoding, freeWhenDone: freeWhenDone)
  }
  

  // - (instancetype)initWithCharacters:(const unichar *)characters length:(NSUInteger)length

  /// \brief Returns an initialized NSString object that contains a
  /// given number of characters from a given array of Unicode
  /// characters.
  init(characters: unichar[]) {
    self = .stringWithCharacters(characters)
  }

  /*
  There's no safe way to express this method that isn't identical to the previous one
  
  // - (instancetype)initWithCharactersNoCopy:(unichar *)characters length:(NSUInteger)length freeWhenDone:(BOOL)flag
  
  /// \brief Returns an initialized NSString object that contains a
  /// given number of characters from a given C array of Unicode
  /// characters.
  */

  // - (instancetype)initWithContentsOfFile:(NSString *)path encoding:(NSStringEncoding)enc error:(NSError **)error

  /// \brief Returns an NSString object initialized by reading data
  /// from the file at a given path using a given encoding.
  init(contentsOfFile: String, encoding: NSStringEncoding, inout error: NSError?) {
    self = String.stringWithContentsOfFile(
      contentsOfFile, encoding: encoding, error: &error)!
  }

  init(contentsOfFile: String, encoding: NSStringEncoding) {
    var ignoredError: NSError? = .None
    self = String.stringWithContentsOfFile(
      contentsOfFile, encoding: encoding, error: &ignoredError)!
  }


  // - (instancetype)initWithContentsOfFile:(NSString *)path usedEncoding:(NSStringEncoding *)enc error:(NSError **)error

  /// \brief Returns an NSString object initialized by reading data
  /// from the file at a given path and returns by reference the
  /// encoding used to interpret the characters.
  init(contentsOfFile: String,
      inout usedEncoding: NSStringEncoding,
      inout error: NSError?
  ) {
    self = String.stringWithContentsOfFile(
      contentsOfFile, usedEncoding: &usedEncoding, error: &error)!
  }

  init(
    contentsOfFile: String,
    inout usedEncoding: NSStringEncoding
  ) {
    var ignoredError: NSError? = .None
    self = String.stringWithContentsOfFile(
      contentsOfFile, usedEncoding: &usedEncoding, error: &ignoredError)!
  }

  // - (instancetype)initWithContentsOfURL:(NSURL *)url encoding:(NSStringEncoding)enc inout error: NSError?)
  
  /// \brief Returns an NSString object initialized by reading data
  /// from a given URL interpreted using a given encoding.
  init(contentsOfURL: NSURL, encoding: NSStringEncoding, inout error: NSError?) {
    self = String.stringWithContentsOfURL(contentsOfURL, encoding: encoding, error: &error)!
  }

  init(contentsOfURL: NSURL, encoding: NSStringEncoding) {
    var ignoredError: NSError? = .None
    self = String.stringWithContentsOfURL(
      contentsOfURL, encoding: encoding, error: &ignoredError)!
  }

  // - (instancetype)initWithContentsOfURL:(NSURL *)url usedEncoding:(NSStringEncoding *)enc error:(NSError **)error
  
  /// \brief Returns an NSString object initialized by reading data
  /// from a given URL and returns by reference the encoding used to
  /// interpret the data.
  init(
    contentsOfURL: NSURL,
    inout usedEncoding: NSStringEncoding,
    inout error: NSError?
  ) {
    self = String.stringWithContentsOfURL(
      contentsOfURL, usedEncoding: &usedEncoding, error: &error)!
  }

  init(
    contentsOfURL: NSURL,
    inout usedEncoding: NSStringEncoding
  ) {
    var ignoredError: NSError? = .None
    self = String.stringWithContentsOfURL(
      contentsOfURL, usedEncoding: &usedEncoding, error: &ignoredError)!
  }

  // - (instancetype)initWithCString:(const char *)nullTerminatedCString encoding:(NSStringEncoding)encoding

  /// \brief Returns an NSString object initialized using the
  /// characters in a given C array, interpreted according to a given
  /// encoding.
  init(CString: Swift.CString, encoding: NSStringEncoding) {
    self = .stringWithCString(CString, encoding: encoding)
  }

  // - (instancetype)initWithData:(NSData *)data encoding:(NSStringEncoding)encoding
  
  /// \brief Returns an NSString object initialized by converting
  /// given data into Unicode characters using a given encoding.

  init(data: NSData, encoding: NSStringEncoding) {
    self = NSString(data: data, encoding: encoding)
  }

  // - (instancetype)initWithFormat:(NSString *)format, ...
  
  /// \brief Returns an NSString object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted.
  init(format: String, _ _arguments: CVarArg...) {
    self = String(format: format, arguments: _arguments)
  }

  // - (instancetype)initWithFormat:(NSString *)format arguments:(va_list)argList

  /// \brief Returns an NSString object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to the user’s default locale.
  init(format: String, arguments: CVarArg[]) {
    securityCheck(_countFormatSpecifiers(format) <= arguments.count)
    self = withVaList(arguments) {
      NSString(format: format, arguments: $0)
    }
  }

  // - (instancetype)initWithFormat:(NSString *)format locale:(id)locale, ...

  /// \brief Returns an NSString object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to given locale information.
  init(format: String, locale: NSLocale, _ arguments: CVarArg...) {
    self = String(format: format, locale: locale, arguments: arguments)
  }

  // - (instancetype)initWithFormat:(NSString *)format locale:(id)locale arguments:(va_list)argList
  
  /// \brief Returns an NSString object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to given locale information.
  init(format: String, locale: NSLocale, arguments: CVarArg[]) {
    securityCheck(_countFormatSpecifiers(format) <= arguments.count, "More format specifiers than arguments")
    self = withVaList(arguments) {
      NSString(format: format, locale: locale, arguments: $0)
    }
  }

  // - (instancetype)initWithString:(NSString *)aString

  /// \brief Returns an NSString object initialized by copying the
  /// characters from another given string.
  init(string: String) {
    self = string
  }

  // - (instancetype)initWithUTF8String:(const char *)bytes
  
  /// \brief Returns an NSString object initialized by copying the
  /// characters from a given C array of UTF8-encoded bytes.

  init(UTF8String: CString) {
    self = String.stringWithUTF8String(UTF8String)
  }

  // @property NSInteger integerValue;

  /// \brief Returns the NSInteger value of the receiver’s text.
  var integerValue: Int {
    return _ns.integerValue
  }

  // @property Int intValue;
  
  /// \brief Returns the integer value of the receiver’s text.
  var intValue: Int {
    return Int(_ns.intValue)
  }

  /// @property BOOL absolutePath;

  /// \brief Returning a Boolean value that indicates whether the
  /// receiver represents an absolute path.
  var absolutePath: Bool {
    return _ns.absolutePath
  }

  // - (BOOL)isEqualToString:(NSString *)aString
  
  /// \brief Returns a Boolean value that indicates whether a given
  /// string is equal to the receiver using a literal Unicode-based
  /// comparison.

  func isEqualToString(aString: String) -> Bool {
    return _ns.isEqualToString(aString)
  }

  // @property NSString lastPathComponent;

  /// \brief Returns the last path component of the receiver.
  var lastPathComponent: String {
    return _ns.lastPathComponent
  }

  // @property NSUInteger length;

  /// \brief Returns the number of Unicode characters in the receiver.
  var length: Int {
    return _ns.length
  }

  // - (NSUInteger)lengthOfBytesUsingEncoding:(NSStringEncoding)enc

  /// \brief Returns the number of bytes required to store the
  /// receiver in a given encoding.
  func lengthOfBytesUsingEncoding(encoding: NSStringEncoding) -> Int {
    return _ns.lengthOfBytesUsingEncoding(encoding)
  }

  // - (NSRange)lineRangeForRange:(NSRange)aRange

  /// \brief Returns the range of characters representing the line or lines containing a given range.

  func lineRangeForRange(aRange: NSRange) -> NSRange {
    return _ns.lineRangeForRange(aRange)
  }

  // - (NSArray *)linguisticTagsInRange:(NSRange)range scheme:(NSString *)tagScheme options:(NSLinguisticTaggerOptions)opts orthography:(NSOrthography *)orthography tokenRanges:(NSArray**)tokenRanges

  /// \brief Returns an array of linguistic tags for the specified
  /// range and requested tags within the receiving string.
  func linguisticTagsInRange(
    range: NSRange, scheme: String, options: NSLinguisticTaggerOptions,
    orthography: NSOrthography?, inout tokenRanges: (NSRange[])?
  ) -> String[] {
    var tokenRanges_: NSArray? = .None
    var result = self._ns.linguisticTagsInRange(
      range,
      scheme: scheme,
      options: options,
      orthography: orthography ? orthography! : nil,
      tokenRanges: &tokenRanges_
    )
    tokenRanges = tokenRanges_!._toRangeArray()
    return result._toStringArray()
  }

  // - (NSComparisonResult)localizedCaseInsensitiveCompare:(NSString *)aString
  
  /// \brief Compares the string and a given string using a
  /// case-insensitive, localized, comparison.
  func localizedCaseInsensitiveCompare(aString: String) -> NSComparisonResult {
    return _ns.localizedCaseInsensitiveCompare(aString)
  }

  // - (NSComparisonResult)localizedCompare:(NSString *)aString
  
  /// \brief Compares the string and a given string using a localized
  /// comparison.
  func localizedCompare(aString: String) -> NSComparisonResult {
    return _ns.localizedCompare(aString)
  }

  /// \brief Compares strings as sorted by the Finder.
  func localizedStandardCompare(string: String) -> NSComparisonResult {
    return _ns.localizedStandardCompare(string)
  }

  // @property long long longLongValue

  /// \brief Returns the long long value of the receiver’s text.
  var longLongValue: Int64 {
    return self.longLongValue
  }

  // @property NSString * lowercaseString
  
  /// \brief Returns lowercased representation of the receiver.
  var lowercaseString: String {
    return _ns.lowercaseString
  }

  // - (NSString *)lowercaseStringWithLocale:(NSLocale *)locale

  /// \brief Returns a version of the string with all letters
  /// converted to lowercase, taking into account the specified
  /// locale.
  func lowercaseStringWithLocale(locale: NSLocale) -> String {
    return _ns.lowercaseStringWithLocale(locale)
  }

  // - (NSUInteger)maximumLengthOfBytesUsingEncoding:(NSStringEncoding)enc
  
  /// \brief Returns the maximum number of bytes needed to store the
  /// receiver in a given encoding.

  func maximumLengthOfBytesUsingEncoding(encoding: NSStringEncoding) -> Int {
    return _ns.maximumLengthOfBytesUsingEncoding(encoding)
  }

  // - (NSRange)paragraphRangeForRange:(NSRange)aRange
  
  /// \brief Returns the range of characters representing the
  /// paragraph or paragraphs containing a given range.

  func paragraphRangeForRange(aRange: NSRange) -> NSRange {
    return _ns.paragraphRangeForRange(aRange)
  }

  // @property NSArray* pathComponents
  
  /// \brief Returns an array of NSString objects containing, in
  /// order, each path component of the receiver.
  var pathComponents: String[] {
    return _ns.pathComponents._toStringArray()
  }

  // @property NSString* pathExtension;

  /// \brief Interprets the receiver as a path and returns the
  /// receiver’s extension, if any.
  var pathExtension: String {
    return _ns.pathExtension
  }

  // @property NSString* precomposedStringWithCanonicalMapping;

  /// \brief Returns a string made by normalizing the receiver’s
  /// contents using Form C.
  var precomposedStringWithCanonicalMapping: String {
    return _ns.precomposedStringWithCanonicalMapping
  }

  // @property NSString * precomposedStringWithCompatibilityMapping;
  
  /// \brief Returns a string made by normalizing the receiver’s
  /// contents using Form KC.
  var precomposedStringWithCompatibilityMapping: String {
    return _ns.precomposedStringWithCompatibilityMapping
  }

  // - (id)propertyList
  
  /// \brief Parses the receiver as a text representation of a
  /// property list, returning an NSString, NSData, NSArray, or
  /// NSDictionary object, according to the topmost element.
  func propertyList() -> AnyObject {
    return _ns.propertyList()
  }

  // - (NSDictionary *)propertyListFromStringsFileFormat

  /// \brief Returns a dictionary object initialized with the keys and
  /// values found in the receiver.
  func propertyListFromStringsFileFormat() -> Dictionary<String, AnyObject> {
    var result = Dictionary<String, AnyObject>()
    
    _ns.propertyListFromStringsFileFormat().enumerateKeysAndObjectsUsingBlock {
      (key: AnyObject?, value: AnyObject?, stop: CMutablePointer<ObjCBool>)->Void
    in
      print("")
    }
    return result
  }

  // - (NSRange)rangeOfCharacterFromSet:(NSCharacterSet *)aSet
  
  /// \brief Finds and returns the range in the receiver of the first
  /// character from a given character set.
  func rangeOfCharacterFromSet(aSet: NSCharacterSet) -> NSRange {
    return _ns.rangeOfCharacterFromSet(aSet)
  }

  // - (NSRange)rangeOfCharacterFromSet:(NSCharacterSet *)aSet options:(NSStringCompareOptions)mask
  
  /// \brief Finds and returns the range in the receiver of the first
  /// character, using given options, from a given character set.
  func rangeOfCharacterFromSet(aSet: NSCharacterSet, options: NSStringCompareOptions) -> NSRange {
    return _ns.rangeOfCharacterFromSet(aSet, options: options)
  }

  // - (NSRange)rangeOfCharacterFromSet:(NSCharacterSet *)aSet options:(NSStringCompareOptions)mask range:(NSRange)aRange

  /// \brief Finds and returns the range in the receiver of the first
  /// character from a given character set found in a given range with
  /// given options.

  func rangeOfCharacterFromSet(
    aSet: NSCharacterSet, options:NSStringCompareOptions, range: NSRange
  )-> NSRange {
    return _ns.rangeOfCharacterFromSet(aSet, options: options, range: range)
  }

  // - (NSRange)rangeOfComposedCharacterSequenceAtIndex:(NSUInteger)anIndex

  /// \brief Returns the range in the receiver of the composed
  /// character sequence located at a given index.
  func rangeOfComposedCharacterSequenceAtIndex(anIndex: Int) -> NSRange {
    return _ns.rangeOfComposedCharacterSequenceAtIndex(anIndex)
  }

  // - (NSRange)rangeOfComposedCharacterSequencesForRange:(NSRange)range
  
  /// \brief Returns the range in the string of the composed character
  /// sequences for a given range.
  func rangeOfComposedCharacterSequencesForRange(range: NSRange) -> NSRange {
    return _ns.rangeOfComposedCharacterSequencesForRange(range)
  }

  // - (NSRange)rangeOfString:(NSString *)aString

  /// \brief Finds and returns the range of the first occurrence of a
  /// given string within the receiver.
  func rangeOfString(aString: String) -> NSRange {
    return _ns.rangeOfString(aString)
  }

  // - (NSRange)rangeOfString:(NSString *)aString options:(NSStringCompareOptions)mask

  /// \brief Finds and returns the range of the first occurrence of a
  /// given string within the receiver, subject to given options.
  func rangeOfString(
    aString: NSString, options:NSStringCompareOptions
  ) -> NSRange {
    return _ns.rangeOfString(aString, options: options)
  }

  // - (NSRange)rangeOfString:(NSString *)aString options:(NSStringCompareOptions)mask range:(NSRange)aRange

  /// \brief Finds and returns the range of the first occurrence of a
  /// given string, within the given range of the receiver, subject to
  /// given options.
  func rangeOfString(
    aString: String, options: NSStringCompareOptions, range: NSRange
  ) -> NSRange {
    return _ns.rangeOfString(aString, options: options, range: range)
  }

  // - (NSRange)rangeOfString:(NSString *)aString options:(NSStringCompareOptions)mask range:(NSRange)searchRange locale:(NSLocale *)locale

  /// \brief Finds and returns the range of the first occurrence of a
  /// given string within a given range of the receiver, subject to
  /// given options, using the specified locale, if any.
  func rangeOfString(
    aString: NSString, options: NSStringCompareOptions, range: NSRange, locale: NSLocale
  ) -> NSRange {
    return _ns.rangeOfString(
      aString, options: options, range: range, locale: locale
    )
  }

  // @property NSStringEncoding smallestEncoding;

  /// \brief Returns the smallest encoding to which the receiver can
  /// be converted without loss of information.
  var smallestEncoding: NSStringEncoding {
    return _ns.smallestEncoding
  }

  // - (NSString *)stringByAbbreviatingWithTildeInPath

  /// \brief Returns a new string that replaces the current home
  /// directory portion of the current path with a tilde (~)
  /// character.
  func stringByAbbreviatingWithTildeInPath() -> String {
    return _ns.stringByAbbreviatingWithTildeInPath
  }

  // - (NSString *)stringByAddingPercentEncodingWithAllowedCharacters:(NSCharacterSet *)allowedCharacters

  /// \brief Returns a new string made from the receiver by replacing
  /// all characters not in the specified set with percent encoded
  /// characters.
  func stringByAddingPercentEncodingWithAllowedCharacters(
    allowedCharacters: NSCharacterSet
  ) -> String {
    return _ns.stringByAddingPercentEncodingWithAllowedCharacters(
      allowedCharacters
    )
  }

  // - (NSString *)stringByAddingPercentEscapesUsingEncoding:(NSStringEncoding)encoding

  /// \brief Returns a representation of the receiver using a given
  /// encoding to determine the percent escapes necessary to convert
  /// the receiver into a legal URL string.
  func stringByAddingPercentEscapesUsingEncoding(
    encoding: NSStringEncoding
  ) -> String {
    return _ns.stringByAddingPercentEscapesUsingEncoding(encoding)
  }

  // - (NSString *)stringByAppendingFormat:(NSString *)format, ...

  /// \brief Returns a string made by appending to the receiver a
  /// string constructed from a given format string and the following
  /// arguments.
  func stringByAppendingFormat(
    format: String, _ arguments: CVarArg...
  ) -> String {
    return _ns.stringByAppendingString(
      String(format: format, arguments: arguments))
  }

  // - (NSString *)stringByAppendingPathComponent:(NSString *)aString

  /// \brief Returns a new string made by appending to the receiver a given string.
  func stringByAppendingPathComponent(aString: String) -> String {
    return _ns.stringByAppendingPathComponent(aString)
  }

  // - (NSString *)stringByAppendingPathExtension:(NSString *)ext

  /// \brief Returns a new string made by appending to the receiver an
  /// extension separator followed by a given extension.
  func stringByAppendingPathExtension(ext: String) -> String {
    return _ns.stringByAppendingPathExtension(ext)
  }

  // - (NSString *)stringByAppendingString:(NSString *)aString

  /// \brief Returns a new string made by appending a given string to
  /// the receiver.
  func stringByAppendingString(aString: String) -> String {
    return _ns.stringByAppendingString(aString)
  }

  // @property NSString* stringByDeletingLastPathComponent;
  
  /// \brief Returns a new string made by deleting the last path
  /// component from the receiver, along with any final path
  /// separator.
  var stringByDeletingLastPathComponent: String {
    return _ns.stringByDeletingLastPathComponent
  }

  // @property NSString* stringByDeletingPathExtension;
  
  /// \brief Returns a new string made by deleting the extension (if
  /// any, and only the last) from the receiver.
  var stringByDeletingPathExtension: String {
    return _ns.stringByDeletingPathExtension
  }

  // @property NSString* stringByExpandingTildeInPath;
  
  /// \brief Returns a new string made by expanding the initial
  /// component of the receiver to its full path value.
  var stringByExpandingTildeInPath: String {
    return _ns.stringByExpandingTildeInPath
  }

  // - (NSString *)stringByFoldingWithOptions:(NSStringCompareOptions)options locale:(NSLocale *)locale

  /// \brief Returns a string with the given character folding options
  /// applied.
  func stringByFoldingWithOptions(
    options: NSStringCompareOptions, locale: NSLocale
  ) -> String {
    return _ns.stringByFoldingWithOptions(options, locale: locale)
  }

  // - (NSString *)stringByPaddingToLength:(NSUInteger)newLength withString:(NSString *)padString startingAtIndex:(NSUInteger)padIndex

  /// \brief Returns a new string formed from the receiver by either
  /// removing characters from the end, or by appending as many
  /// occurrences as necessary of a given pad string.
  func stringByPaddingToLength(
    newLength: Int, withString: String, startingAtIndex:Int
  ) -> String {
    return _ns.stringByPaddingToLength(
      newLength, withString: withString, startingAtIndex: startingAtIndex)
  }

  // @property NSString* stringByRemovingPercentEncoding;
  
  /// \brief Returns a new string made from the receiver by replacing
  /// all percent encoded sequences with the matching UTF-8
  /// characters.
  var stringByRemovingPercentEncoding: String {
    return _ns.stringByRemovingPercentEncoding
  }

  // - (NSString *)stringByReplacingCharactersInRange:(NSRange)range withString:(NSString *)replacement

  /// \brief Returns a new string in which the characters in a
  /// specified range of the receiver are replaced by a given string.
  func stringByReplacingCharactersInRange(
    range: NSRange, withString: String
  ) -> String {
    return _ns.stringByReplacingCharactersInRange(range, withString:withString)
  }

  // - (NSString *)stringByReplacingOccurrencesOfString:(NSString *)target withString:(NSString *)replacement

  /// \brief Returns a new string in which all occurrences of a target
  /// string in the receiver are replaced by another given string.
  func stringByReplacingOccurrencesOfString(target: String, withString: NSString) -> String {
    return _ns.stringByReplacingOccurrencesOfString(target, withString: withString)
  }

  // - (NSString *)stringByReplacingOccurrencesOfString:(NSString *)target withString:(NSString *)replacement options:(NSStringCompareOptions)options range:(NSRange)searchRange

  /// \brief Returns a new string in which all occurrences of a target
  /// string in a specified range of the receiver are replaced by
  /// another given string.
  func stringByReplacingOccurrencesOfString(
    target: String, withString: String, options: NSStringCompareOptions, range: NSRange
  ) -> String {
    return _ns.stringByReplacingOccurrencesOfString(
      target, withString: withString, options: options, range: range
    )
  }

  // - (NSString *)stringByReplacingPercentEscapesUsingEncoding:(NSStringEncoding)encoding

  /// \brief Returns a new string made by replacing in the receiver
  /// all percent escapes with the matching characters as determined
  /// by a given encoding.
  func stringByReplacingPercentEscapesUsingEncoding(
    encoding: NSStringEncoding
  ) -> String {
    return _ns.stringByReplacingPercentEscapesUsingEncoding(encoding)
  }

  // @property NSString* stringByResolvingSymlinksInPath;
  
  /// \brief Returns a new string made from the receiver by resolving
  /// all symbolic links and standardizing path.
  var stringByResolvingSymlinksInPath: String {
    return _ns.stringByResolvingSymlinksInPath
  }

  // @property NSString* stringByStandardizingPath;

  /// \brief Returns a new string made by removing extraneous path
  /// components from the receiver.
  var stringByStandardizingPath: String {
    return _ns.stringByStandardizingPath
  }

  // - (NSString *)stringByTrimmingCharactersInSet:(NSCharacterSet *)set

  /// \brief Returns a new string made by removing from both ends of
  /// the receiver characters contained in a given character set.
  func stringByTrimmingCharactersInSet(set: NSCharacterSet) -> String {
    return _ns.stringByTrimmingCharactersInSet(set)
  }

  // - (NSArray *)stringsByAppendingPaths:(NSArray *)paths

  /// \brief Returns an array of strings made by separately appending
  /// to the receiver each string in in a given array.
  func stringsByAppendingPaths(paths: String[]) -> String[] {
    return _ns.stringsByAppendingPaths(_toNSArray(paths))._toStringArray()
  }

  // - (NSString *)substringFromIndex:(NSUInteger)anIndex

  /// \brief Returns a new string containing the characters of the
  /// receiver from the one at a given index to the end.
  func substringFromIndex(index: Int) -> String {
    return _ns.substringFromIndex(index)
  }

  // - (NSString *)substringToIndex:(NSUInteger)anIndex

  /// \brief Returns a new string containing the characters of the
  /// receiver up to, but not including, the one at a given index.
  func substringToIndex(index: Int) -> String {
    return _ns.substringToIndex(index)
  }

  // - (NSString *)substringWithRange:(NSRange)aRange

  /// \brief Returns a string object containing the characters of the
  /// receiver that lie within a given range.
  func substringWithRange(aRange: NSRange) -> String {
    return _ns.substringWithRange(aRange)
  }

  // @property NSString* uppercaseString;
  
  /// \brief Returns a uppercased representation of the receiver.
  var uppercaseString: String {
    return _ns.uppercaseString
  }

  // - (NSString *)uppercaseStringWithLocale:(NSLocale *)locale

  /// \brief Returns a version of the string with all letters
  /// converted to uppercase, taking into account the specified
  /// locale.
  func uppercaseStringWithLocale(locale: NSLocale) -> String {
    return _ns.uppercaseStringWithLocale(locale)
  }

  // - (const char *)UTF8String

  /// \brief Returns a null-terminated UTF8 representation of the receiver.
  func UTF8String() -> CChar[] {
    return _ns.UTF8String.persist()
  }

  // - (BOOL)writeToFile:(NSString *)path atomically:(BOOL)useAuxiliaryFile encoding:(NSStringEncoding)enc error:(NSError **)error

  /// \brief Writes the contents of the receiver to a file at a given
  /// path using a given encoding.
  func writeToFile(
    path: NSString, atomically:Bool, encoding: NSStringEncoding, inout error: NSError?
  ) -> Bool {
    return self._ns.writeToFile(
      path, atomically: atomically, encoding: encoding,
      error: &error
    )
  }

  func writeToFile(
    path: NSString, atomically:Bool, encoding: NSStringEncoding) -> Bool {
      return _ns.writeToFile(path, atomically: atomically, encoding: encoding, error: nil)
  }
  
  // - (BOOL)writeToURL:(NSURL *)url atomically:(BOOL)useAuxiliaryFile encoding:(NSStringEncoding)enc error:(NSError **)error

  /// \brief Writes the contents of the receiver to the URL specified
  /// by url using the specified encoding.
  func writeToURL(
    url: NSURL, atomically: Bool, encoding: NSStringEncoding, inout error: NSError?
  ) -> Bool {
    return self._ns.writeToURL(
      url, atomically: atomically, encoding: encoding, error: &error
    )
  }

  func writeToURL(
    url: NSURL, atomically: Bool, encoding: NSStringEncoding
  ) -> Bool {
    return _ns.writeToURL(url, atomically: atomically, encoding: encoding, error: nil)
  }
}

@transparent
func == (lhs: NSString, rhs: NSString) -> Bool {
  return lhs.isEqualToString(rhs)
}

@transparent
func == (lhs: String, rhs: NSString) -> Bool {
  return lhs.isEqualToString(rhs)
}

@transparent
func == (lhs: NSString, rhs: String) -> Bool {
  return lhs.isEqualToString(rhs)
}

