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
//
// Exposing the API of NSString on Swift's String
//
//===----------------------------------------------------------------------===//

// Open Issues
// ===========
//
// Property Lists need to be properly bridged
//

func _toNSArray<T, U:AnyObject>(a: [T], @noescape f: (T) -> U) -> NSArray {
  var result = NSMutableArray(capacity: a.count)
  for s in a {
    result.addObject(f(s))
  }
  return result
}

func _toNSRange(r: Range<String.Index>) -> NSRange {
  return NSRange(
    location: r.startIndex._utf16Index, 
    length: r.endIndex._utf16Index - r.startIndex._utf16Index)
}

func _countFormatSpecifiers(a: String) -> Int {
  // The implementation takes advantage of the fact that internal
  // representation of String is UTF-16.  Because we only care about the ASCII
  // percent character, we don't need to decode UTF-16.

  let percentUTF16  = UTF16.CodeUnit(("%" as UnicodeScalar).value)
  let notPercentUTF16: UTF16.CodeUnit = 0
  var lastChar = notPercentUTF16 // anything other than % would work here
  var count = 0

  for c in a.utf16 {
    if lastChar == percentUTF16 {
      if c == percentUTF16 {
        // a "%" following this one should not be taken as literal
        lastChar = notPercentUTF16
      }
      else {
        ++count
        lastChar = c
      }
    } else {
      lastChar = c
    }
  }
  return count
}

extension String {

  //===--- Bridging Helpers -----------------------------------------------===//
  //===--------------------------------------------------------------------===//
  
  /// The corresponding `NSString` - a convenience for bridging code.
  var _ns: NSString {
    return self as NSString
  }

  /// Return an `Index` corresponding to the given offset in our UTF-16
  /// representation.
  func _index(utf16Index: Int) -> Index {
    return Index(_base: String.UnicodeScalarView.Index(utf16Index, _core))
  }

  /// Return a `Range<Index>` corresponding to the given `NSRange` of
  /// our UTF-16 representation.
  func _range(r: NSRange) -> Range<Index> {
    return _index(r.location)..<_index(r.location + r.length)
  }

  /// Return a `Range<Index>?` corresponding to the given `NSRange` of
  /// our UTF-16 representation.
  func _optionalRange(r: NSRange) -> Range<Index>? {
    if r.location == NSNotFound {
      return .None
    }
    return _range(r)
  }

  /// Invoke `body` on an `Int` buffer.  If `index` was converted from
  /// non-`nil`, convert the buffer to an `Index` and write it into the
  /// memory referred to by `index`
  func _withOptionalOutParameter<Result>(
    index: UnsafeMutablePointer<Index>,
    @noescape body: (UnsafeMutablePointer<Int>)->Result
  ) -> Result {
    var utf16Index: Int = 0
    let result = index._withBridgeValue(&utf16Index) {
      body($0)
    }
    index._setIfNonNil { self._index(utf16Index) }
    return result
  }
  
  /// Invoke `body` on an `NSRange` buffer.  If `range` was converted
  /// from non-`nil`, convert the buffer to a `Range<Index>` and write
  /// it into the memory referred to by `range`
  func _withOptionalOutParameter<Result>(
    range: UnsafeMutablePointer<Range<Index>>,
    @noescape body: (UnsafeMutablePointer<NSRange>)->Result
  ) -> Result {
    var nsRange = NSRange(location: 0, length: 0)
    let result = range._withBridgeValue(&nsRange) {
      body($0)
    }
    range._setIfNonNil { self._range(nsRange) }
    return result
  }

  //===--- Class Methods --------------------------------------------------===//
  //===--------------------------------------------------------------------===//

  // + (const NSStringEncoding *)availableStringEncodings

  /// Returns an Array of the encodings string objects support
  /// in the application’s environment.
  public static func availableStringEncodings() -> [NSStringEncoding] {
    var result = [NSStringEncoding]()
    var p = NSString.availableStringEncodings()
    while p.memory != 0 {
      result.append(p.memory)
      ++p
    }
    return result
  }

  // + (NSStringEncoding)defaultCStringEncoding

  /// Returns the C-string encoding assumed for any method accepting
  /// a C string as an argument.
  public static func defaultCStringEncoding() -> NSStringEncoding {
    return NSString.defaultCStringEncoding()
  }

  // + (NSString *)localizedNameOfStringEncoding:(NSStringEncoding)encoding

  /// Returns a human-readable string giving the name of a given encoding.
  public static func localizedNameOfStringEncoding(
    encoding: NSStringEncoding
  ) -> String {
    return NSString.localizedNameOfStringEncoding(encoding)
  }

  // + (instancetype)localizedStringWithFormat:(NSString *)format, ...

  /// Returns a string created by using a given format string as a
  /// template into which the remaining argument values are substituted
  /// according to the user's default locale.
  public static func localizedStringWithFormat(
    format: String, _ arguments: CVarArgType...
  ) -> String {
    return String(format: format, arguments: arguments)
  }

  // + (NSString *)pathWithComponents:(NSArray *)components

  /// Returns a string built from the strings in a given array
  /// by concatenating them with a path separator between each pair.
  public static func pathWithComponents(components: [String]) -> String {
    return NSString.pathWithComponents(components)
  }

  //===--- Non-failing factories ------------------------------------------===//
  // NSString factory functions that can't fail, and have a
  // corresponding constructor, are omitted.
  //
  // + (instancetype)string
  //
  // + (instancetype)
  //     stringWithCharacters:(const unichar *)chars length:(NSUInteger)length
  //
  // + (instancetype)stringWithFormat:(NSString *)format, ...
  //===--------------------------------------------------------------------===//
  
  // + (instancetype)
  //     stringWithContentsOfFile:(NSString *)path
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error

  /// Produces a string created by reading data from the file at a
  /// given path interpreted using a given encoding.
  public init?(
    contentsOfFile path: String,
    encoding enc: NSStringEncoding,
    error: NSErrorPointer = nil
  ) {
    if let ns = NSString(contentsOfFile: path, encoding: enc, error: error) {
      self = ns as String
    } else {
      return nil
    }
  }

  // + (instancetype)
  //     stringWithContentsOfFile:(NSString *)path
  //     usedEncoding:(NSStringEncoding *)
  //     enc error:(NSError **)error

  /// Produces a string created by reading data from the file at
  /// a given path and returns by reference the encoding used to
  /// interpret the file.
  public init?(
    contentsOfFile path: String,
    usedEncoding: UnsafeMutablePointer<NSStringEncoding> = nil,
    error: NSErrorPointer = nil
  ) {
    if let ns = NSString(
      contentsOfFile: path, usedEncoding: usedEncoding, error: error) {

      self = ns as String
    } else {
      return nil
    }
  }

  // + (instancetype)
  //     stringWithContentsOfURL:(NSURL *)url
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error

  /// Produces a string created by reading data from a given URL
  /// interpreted using a given encoding.  Errors are written into the
  /// inout `error` argument.
  public init?(
    contentsOfURL url: NSURL, 
    encoding enc: NSStringEncoding, 
    error: NSErrorPointer = nil
  ) {
    if let ns = NSString(contentsOfURL: url, encoding: enc, error: error) {
      self = ns as String
    } else {
      return nil
    }
  }

  // + (instancetype)
  //     stringWithContentsOfURL:(NSURL *)url
  //     usedEncoding:(NSStringEncoding *)enc
  //     error:(NSError **)error

  /// Produces a string created by reading data from a given URL
  /// and returns by reference the encoding used to interpret the
  /// data.  Errors are written into the inout `error` argument.
  public init?(
    contentsOfURL url: NSURL,
    usedEncoding enc: UnsafeMutablePointer<NSStringEncoding> = nil,
    error: NSErrorPointer = nil
  ) {
    if let ns = NSString(contentsOfURL: url, usedEncoding: enc, error: error) {
      self = ns as String
    } else {
      return nil
    }
  }

  // + (instancetype)
  //     stringWithCString:(const char *)cString
  //     encoding:(NSStringEncoding)enc

  /// Produces a string containing the bytes in a given C array,
  /// interpreted according to a given encoding.
  public init?(
    CString: UnsafePointer<CChar>,
    encoding enc: NSStringEncoding
  ) {
    if let ns = NSString(CString: CString, encoding: enc) {
      self = ns as String
    } else {
      return nil 
    }
  }


  //===--- Adds nothing for String beyond what String(s) does -------------===//
  // + (instancetype)stringWithString:(NSString *)aString
  //===--------------------------------------------------------------------===//
  
  // + (instancetype)stringWithUTF8String:(const char *)bytes

  /// Produces a string created by copying the data from a given
  /// C array of UTF8-encoded bytes.
  public init?(UTF8String bytes: UnsafePointer<CChar>) {
    if let ns = NSString(UTF8String: bytes) {
      self = ns as String
    } else {
      return nil
    }
  }

  //===--- Instance Methods/Properties-------------------------------------===//
  //===--------------------------------------------------------------------===//

  //===--- Omitted by agreement during API review 5/20/2014 ---------------===//
  // @property BOOL boolValue;
  
  // - (BOOL)canBeConvertedToEncoding:(NSStringEncoding)encoding

  /// Returns a Boolean value that indicates whether the
  /// `String` can be converted to a given encoding without loss of
  /// information.
  public func canBeConvertedToEncoding(encoding: NSStringEncoding) -> Bool {
    return _ns.canBeConvertedToEncoding(encoding)
  }

  // @property NSString* capitalizedString

  /// Produce a string with the first character from each word changed
  /// to the corresponding uppercase value.
  public var capitalizedString: String {
    return _ns.capitalizedString as String
  }

  // - (NSString *)capitalizedStringWithLocale:(NSLocale *)locale

  /// Returns a capitalized representation of the `String`
  /// using the specified locale.
  public func capitalizedStringWithLocale(locale: NSLocale?) -> String{
    return _ns.capitalizedStringWithLocale(locale) as String
  }


  // - (NSComparisonResult)caseInsensitiveCompare:(NSString *)aString

  /// Returns the result of invoking `compare:options:` with
  /// `NSCaseInsensitiveSearch` as the only option.
  public func caseInsensitiveCompare(aString: String) -> NSComparisonResult {
    return _ns.caseInsensitiveCompare(aString)
  }

  //===--- Omitted by agreement during API review 5/20/2014 ---------------===//
  // - (unichar)characterAtIndex:(NSUInteger)index
  //
  // We have a different meaning for "Character" in Swift, and we are
  // trying not to expose error-prone UTF-16 integer indexes
  
  // - (NSString *)
  //     commonPrefixWithString:(NSString *)aString
  //     options:(NSStringCompareOptions)mask

  /// Returns a string containing characters the `String` and a
  /// given string have in common, starting from the beginning of each
  /// up to the first characters that aren’t equivalent.
  public func commonPrefixWithString(
    aString: String, options: NSStringCompareOptions) -> String {
    return _ns.commonPrefixWithString(aString, options: options)
  }

  // - (NSComparisonResult)
  //     compare:(NSString *)aString
  //
  // - (NSComparisonResult)
  //     compare:(NSString *)aString options:(NSStringCompareOptions)mask
  //
  // - (NSComparisonResult)
  //     compare:(NSString *)aString options:(NSStringCompareOptions)mask 
  //     range:(NSRange)range
  //
  // - (NSComparisonResult)
  //     compare:(NSString *)aString options:(NSStringCompareOptions)mask 
  //     range:(NSRange)range locale:(id)locale

  /// Compares the string using the specified options and
  /// returns the lexical ordering for the range.
  public func compare(
    aString: String,
    options mask: NSStringCompareOptions = nil,
    range: Range<Index>? = nil,
    locale: NSLocale? = nil
  ) -> NSComparisonResult {
    // According to Ali Ozer, there may be some real advantage to
    // dispatching to the minimal selector for the supplied options.
    // So let's do that; the switch should compile away anyhow.
    return locale != nil ? _ns.compare(
      aString, options: mask,
      range: _toNSRange(range != nil ? range! : indices(self)), locale: locale)
    
    : range != nil ? _ns.compare(
      aString, options: mask, range: _toNSRange(range != nil ? range! : indices(self)))
      
    : mask != nil ? _ns.compare(aString, options: mask)
      
    : _ns.compare(aString)
  }

  // - (NSUInteger)
  //     completePathIntoString:(NSString **)outputName
  //     caseSensitive:(BOOL)flag
  //     matchesIntoArray:(NSArray **)outputArray
  //     filterTypes:(NSArray *)filterTypes

  /// Interprets the `String` as a path in the file system and
  /// attempts to perform filename completion, returning a numeric
  /// value that indicates whether a match was possible, and by
  /// reference the longest path that matches the `String`.
  /// Returns the actual number of matching paths.
  public func completePathIntoString(
    _ outputName: UnsafeMutablePointer<String> = nil,
    caseSensitive: Bool,
    matchesIntoArray: UnsafeMutablePointer<[String]> = nil,
    filterTypes: [String]? = nil
  ) -> Int {
    var nsMatches: NSArray?
    var nsOutputName: NSString?

    let result = outputName._withBridgeObject(&nsOutputName) {
      outputName in matchesIntoArray._withBridgeObject(&nsMatches) {
        matchesIntoArray in
        self._ns.completePathIntoString(
          outputName, caseSensitive: caseSensitive,
          matchesIntoArray: matchesIntoArray, filterTypes: filterTypes
        )
      }
    }

    if let matches = nsMatches {
      // Since this function is effectively a bridge thunk, use the
      // bridge thunk semantics for the NSArray conversion
      matchesIntoArray._setIfNonNil { _convertNSArrayToArray(matches) }
    }
    
    if let n = nsOutputName {
      outputName._setIfNonNil { n as String }
    }
    return result
  }

  // - (NSArray *)
  //     componentsSeparatedByCharactersInSet:(NSCharacterSet *)separator

  /// Returns an array containing substrings from the `String`
  /// that have been divided by characters in a given set.
  public func componentsSeparatedByCharactersInSet(
    separator: NSCharacterSet
  ) -> [String] {
    // FIXME: two steps due to <rdar://16971181>
    let nsa = _ns.componentsSeparatedByCharactersInSet(separator) as NSArray
    // Since this function is effectively a bridge thunk, use the
    // bridge thunk semantics for the NSArray conversion
    return _convertNSArrayToArray(nsa)
  }


  // - (NSArray *)componentsSeparatedByString:(NSString *)separator

  /// Returns an array containing substrings from the `String`
  /// that have been divided by a given separator.
  public func componentsSeparatedByString(separator: String) -> [String] {
    let nsa = _ns.componentsSeparatedByString(separator) as NSArray
    // Since this function is effectively a bridge thunk, use the
    // bridge thunk semantics for the NSArray conversion
    return _convertNSArrayToArray(nsa)
  }

  // - (const char *)cStringUsingEncoding:(NSStringEncoding)encoding

  /// Returns a representation of the `String` as a C string
  /// using a given encoding.
  public func cStringUsingEncoding(encoding: NSStringEncoding) -> [CChar]? {
    return withExtendedLifetime(_ns) {
      (s: NSString) -> [CChar]? in
      _persistCString(s.cStringUsingEncoding(encoding))
    }
  }

  // - (NSData *)dataUsingEncoding:(NSStringEncoding)encoding
  //
  // - (NSData *)
  //     dataUsingEncoding:(NSStringEncoding)encoding
  //     allowLossyConversion:(BOOL)flag

  /// Returns an `NSData` object containing a representation of
  /// the `String` encoded using a given encoding.
  public func dataUsingEncoding(
    encoding: NSStringEncoding,
    allowLossyConversion: Bool = false
  ) -> NSData? {
    return _ns.dataUsingEncoding(
      encoding, allowLossyConversion: allowLossyConversion)
  }

  // @property NSString* decomposedStringWithCanonicalMapping;

  /// Returns a string made by normalizing the `String`’s
  /// contents using Form D.
  public var decomposedStringWithCanonicalMapping: String {
    return _ns.decomposedStringWithCanonicalMapping
  }

  // @property NSString* decomposedStringWithCompatibilityMapping;

  /// Returns a string made by normalizing the `String`’s
  /// contents using Form KD.
  public var decomposedStringWithCompatibilityMapping: String {
    return _ns.decomposedStringWithCompatibilityMapping
  }

  //===--- Importing Foundation should not affect String printing ---------===//
  // Therefore, we're not exposing this:
  //
  //   @property NSString* description

  
  //===--- Omitted for consistency with API review results 5/20/2014 -----===//
  // @property double doubleValue;

  // - (void)
  //     enumerateLinesUsingBlock:(void (^)(NSString *line, BOOL *stop))block

  /// Enumerates all the lines in a string.
  public func enumerateLines(body: (line: String, inout stop: Bool)->()) {
    _ns.enumerateLinesUsingBlock {
      (line: String?, stop: UnsafeMutablePointer<ObjCBool>)
    in
      var stop_ = false
      body(line: line!, stop: &stop_)
      if stop_ {
        UnsafeMutablePointer<ObjCBool>(stop).memory = true
      }
    }
  }

  // - (void)
  //     enumerateLinguisticTagsInRange:(NSRange)range
  //     scheme:(NSString *)tagScheme
  //     options:(NSLinguisticTaggerOptions)opts
  //     orthography:(NSOrthography *)orthography
  //     usingBlock:(
  //       void (^)(
  //         NSString *tag, NSRange tokenRange, 
  //         NSRange sentenceRange, BOOL *stop)
  //       )block

  /// Performs linguistic analysis on the specified string by
  /// enumerating the specific range of the string, providing the
  /// Block with the located tags.
  public func enumerateLinguisticTagsInRange(
    range: Range<Index>, 
    scheme tagScheme: String, 
    options opts: NSLinguisticTaggerOptions,
    orthography: NSOrthography?,
    _ body:
      (String, Range<Index>, Range<Index>, inout Bool)->()
  ) {
    _ns.enumerateLinguisticTagsInRange(
      _toNSRange(range),
      scheme: tagScheme,
      options: opts,
      orthography: orthography != nil ? orthography! : nil
    ) {
      var stop_ = false
      body($0, self._range($1), self._range($2), &stop_)
      if stop_ {
        UnsafeMutablePointer($3).memory = true
      }
    }
  }

  // - (void)
  //     enumerateSubstringsInRange:(NSRange)range
  //     options:(NSStringEnumerationOptions)opts
  //     usingBlock:(
  //       void (^)(
  //         NSString *substring,
  //         NSRange substringRange,
  //         NSRange enclosingRange,
  //         BOOL *stop)
  //       )block

  /// Enumerates the substrings of the specified type in the
  /// specified range of the string.
  public func enumerateSubstringsInRange(
    range: Range<Index>,
    options opts:NSStringEnumerationOptions,
    _ body: (
      substring: String, substringRange: Range<Index>,
      enclosingRange: Range<Index>, inout Bool
    )->()
  ) {
    _ns.enumerateSubstringsInRange(_toNSRange(range), options: opts) {
      var stop_ = false
      
      body(substring: $0,
        substringRange: self._range($1),
        enclosingRange: self._range($2),
        &stop_)
      
      if stop_ {
        UnsafeMutablePointer($3).memory = true
      }
    }
  }

  // @property NSStringEncoding fastestEncoding;

  /// Returns the fastest encoding to which the `String` may be
  /// converted without loss of information.
  public var fastestEncoding: NSStringEncoding {
    return _ns.fastestEncoding
  }

  // - (const char *)fileSystemRepresentation

  /// Returns a file system-specific representation of the `String`.
  public func fileSystemRepresentation() -> [CChar] {
    return _persistCString(_ns.fileSystemRepresentation)!
  }

  //===--- Omitted for consistency with API review results 5/20/2014 ------===//
  // @property float floatValue;

  // - (BOOL)
  //     getBytes:(void *)buffer
  //     maxLength:(NSUInteger)maxBufferCount
  //     usedLength:(NSUInteger*)usedBufferCount
  //     encoding:(NSStringEncoding)encoding
  //     options:(NSStringEncodingConversionOptions)options
  //     range:(NSRange)range
  //     remainingRange:(NSRangePointer)leftover

  /// Gets a given range of characters as bytes in a specified encoding.
  /// Note: will get a maximum of `min(buffer.count, maxLength)` bytes.
  public func getBytes(
    inout buffer: [UInt8],
    maxLength: Int,
    usedLength: UnsafeMutablePointer<Int>,
    encoding: NSStringEncoding,
    options: NSStringEncodingConversionOptions,
    range: Range<Index>,
    remainingRange: UnsafeMutablePointer<Range<Index>>
  ) -> Bool {
    return _withOptionalOutParameter(remainingRange) {
      self._ns.getBytes(
        &buffer,
        maxLength: min(buffer.count, maxLength),
        usedLength: usedLength,
        encoding: encoding,
        options: options,
        range: _toNSRange(range),
        remainingRange: $0)
    }
  }

  // - (BOOL)
  //     getCString:(char *)buffer
  //     maxLength:(NSUInteger)maxBufferCount
  //     encoding:(NSStringEncoding)encoding

  /// Converts the `String`’s content to a given encoding and
  /// stores them in a buffer. Note: will store a maximum of
  /// `min(buffer.count, maxLength)` bytes.
  public func getCString(
    inout buffer: [CChar], maxLength: Int, encoding: NSStringEncoding
  ) -> Bool {
    return _ns.getCString(&buffer, maxLength: min(buffer.count, maxLength), 
                          encoding: encoding)
  }

  // - (BOOL)
  //     getFileSystemRepresentation:(char *)buffer
  //     maxLength:(NSUInteger)maxLength

  /// Interprets the `String` as a system-independent path and
  /// fills a buffer with a C-string in a format and encoding suitable
  /// for use with file-system calls. Note: will store a maximum of
  /// `min(buffer.count, maxLength)` bytes.
  public func getFileSystemRepresentation(
    inout buffer: [CChar], maxLength: Int) -> Bool {
    return _ns.getFileSystemRepresentation(
      &buffer, maxLength: min(buffer.count, maxLength))
  }

  // - (void)
  //     getLineStart:(NSUInteger *)startIndex
  //     end:(NSUInteger *)lineEndIndex
  //     contentsEnd:(NSUInteger *)contentsEndIndex
  //     forRange:(NSRange)aRange

  /// Returns by reference the beginning of the first line and
  /// the end of the last line touched by the given range.
  public func getLineStart(
    start: UnsafeMutablePointer<Index>,
    end: UnsafeMutablePointer<Index>,
    contentsEnd: UnsafeMutablePointer<Index>,
    forRange: Range<Index>
  ) {
    _withOptionalOutParameter(start) {
      start in self._withOptionalOutParameter(end) {
        end in self._withOptionalOutParameter(contentsEnd) {
          contentsEnd in self._ns.getLineStart(
            start, end: end,
            contentsEnd: contentsEnd,
            forRange: _toNSRange(forRange))
        }
      }
    }
  }

  // - (void)
  //     getParagraphStart:(NSUInteger *)startIndex
  //     end:(NSUInteger *)endIndex
  //     contentsEnd:(NSUInteger *)contentsEndIndex
  //     forRange:(NSRange)aRange

  /// Returns by reference the beginning of the first paragraph
  /// and the end of the last paragraph touched by the given range.
  public func getParagraphStart(
    start: UnsafeMutablePointer<Index>,
    end: UnsafeMutablePointer<Index>,
    contentsEnd: UnsafeMutablePointer<Index>,
    forRange: Range<Index>
  ) {
    _withOptionalOutParameter(start) {
      start in self._withOptionalOutParameter(end) {
        end in self._withOptionalOutParameter(contentsEnd) {
          contentsEnd in self._ns.getParagraphStart(
            start, end: end,
            contentsEnd: contentsEnd,
            forRange: _toNSRange(forRange))
        }
      }
    }
  }

  // - (NSUInteger)hash

  /// An unsigned integer that can be used as a hash table address.
  public var hash: Int {
    return _ns.hash
  }

  //===--- Already provided by String's core ------------------------------===//
  // - (instancetype)init

  //===--- Initializers that can fail presented as factory functions ------===//
  // - (instancetype)
  //     initWithBytes:(const void *)bytes
  //     length:(NSUInteger)length
  //     encoding:(NSStringEncoding)encoding

  /// Produces an initialized `NSString` object equivalent to the given
  /// `bytes` interpreted in the given `encoding`.
  public init? <
    S: SequenceType where S.Generator.Element == UInt8
  >(
    bytes: S, encoding: NSStringEncoding
  ) {
    let byteArray = Array(bytes)
    if let ns = NSString(
      bytes: byteArray, length: byteArray.count, encoding: encoding) {

      self = ns as String
    } else {
      return nil
    }
  }

  // - (instancetype)
  //     initWithBytesNoCopy:(void *)bytes
  //     length:(NSUInteger)length
  //     encoding:(NSStringEncoding)encoding
  //     freeWhenDone:(BOOL)flag

  /// Produces an initialized `String` object that contains a
  /// given number of bytes from a given buffer of bytes interpreted
  /// in a given encoding, and optionally frees the buffer.  WARNING:
  /// this initializer is not memory-safe!
  public init?(
    bytesNoCopy bytes: UnsafeMutablePointer<Void>, length: Int,
    encoding: NSStringEncoding, freeWhenDone flag: Bool
  ) {
    if let ns = NSString(
      bytesNoCopy: bytes, length: length, encoding: encoding,
      freeWhenDone: flag) {

      self = ns as String
    } else {
      return nil
    }
  }


  // - (instancetype)
  //     initWithCharacters:(const unichar *)characters
  //     length:(NSUInteger)length
  
  /// Returns an initialized `String` object that contains a
  /// given number of characters from a given array of Unicode
  /// characters.
  public init(
    utf16CodeUnits: UnsafePointer<unichar>,
    count: Int
  ) {
    self = NSString(characters: utf16CodeUnits, length: count) as String
  }

  // - (instancetype)
  //     initWithCharactersNoCopy:(unichar *)characters
  //     length:(NSUInteger)length
  //     freeWhenDone:(BOOL)flag

  /// Returns an initialized `String` object that contains a given
  /// number of characters from a given array of UTF-16 Code Units
  public init(
    utf16CodeUnitsNoCopy: UnsafePointer<unichar>,
    count: Int, 
    freeWhenDone flag: Bool
  ) {
    self = NSString(
      charactersNoCopy: UnsafeMutablePointer(utf16CodeUnitsNoCopy),
      length: count,
      freeWhenDone: flag) as String
  }

  //===--- Initializers that can fail dropped for factory functions -------===//
  // - (instancetype)
  //     initWithContentsOfFile:(NSString *)path
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error
  //
  // - (instancetype)
  //     initWithContentsOfFile:(NSString *)path
  //     usedEncoding:(NSStringEncoding *)enc
  //     error:(NSError **)error
  //
  // - (instancetype)
  //     initWithContentsOfURL:(NSURL *)url
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError**)error
  //  
  // - (instancetype)
  //     initWithContentsOfURL:(NSURL *)url
  //     usedEncoding:(NSStringEncoding *)enc
  //     error:(NSError **)error
  //
  // - (instancetype)
  //     initWithCString:(const char *)nullTerminatedCString
  //     encoding:(NSStringEncoding)encoding
  //
  // - (instancetype)
  //     initWithData:(NSData *)data
  //     encoding:(NSStringEncoding)encoding
  //===--------------------------------------------------------------------===//
  
  // FIXME: optional locale can't be handled with default
  // arguments due to <rdar://problem/16983329>
  
  // - (instancetype)initWithFormat:(NSString *)format, ...

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted.
  public init(format: String, _ arguments: CVarArgType...) {
    self = String(format: format, arguments: arguments)
  }

  // - (instancetype)
  //     initWithFormat:(NSString *)format
  //     arguments:(va_list)argList

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to the user’s default locale.
  public init(format: String, arguments: [CVarArgType]) {
    self = String(format: format, locale: nil, arguments: arguments)
  }
  
  // - (instancetype)initWithFormat:(NSString *)format locale:(id)locale, ...

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to given locale information.
  public init(format: String, locale: NSLocale?, _ args: CVarArgType...) {
    self = String(format: format, locale: locale, arguments: args)
  }

  // - (instancetype)
  //     initWithFormat:(NSString *)format
  //     locale:(id)locale
  //     arguments:(va_list)argList

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to given locale information.
  public init(format: String, locale: NSLocale?, arguments: [CVarArgType]) {
    _precondition(
      _countFormatSpecifiers(format) <= arguments.count,
      "Too many format specifiers (%<letter>) provided for the argument list"
    )
    self = withVaList(arguments) {
      NSString(format: format, locale: locale, arguments: $0) as String
    }
  }

  //===--- Already provided by core Swift ---------------------------------===//
  // - (instancetype)initWithString:(NSString *)aString
  
  //===--- Initializers that can fail dropped for factory functions -------===//
  // - (instancetype)initWithUTF8String:(const char *)bytes

  //===--- Omitted for consistency with API review results 5/20/2014 ------===//
  // @property NSInteger integerValue;
  // @property Int intValue;

  //===--- Omitted by apparent agreement during API review 5/20/2014 ------===//
  // @property BOOL absolutePath;
  // - (BOOL)isEqualToString:(NSString *)aString

  //===--- Kept for consistency with API review results 5/20/2014 ---------===//
  // We decided to keep pathWithComponents, so keeping this too
  // @property NSString lastPathComponent;

  /// Returns the last path component of the `String`.
  public var lastPathComponent: String {
    return _ns.lastPathComponent
  }

  //===--- Renamed by agreement during API review 5/20/2014 ---------------===//
  // @property NSUInteger length;

  /// Returns the number of Unicode characters in the `String`.
  public var utf16Count: Int {
    return _ns.length
  }

  // - (NSUInteger)lengthOfBytesUsingEncoding:(NSStringEncoding)enc

  /// Returns the number of bytes required to store the
  /// `String` in a given encoding.
  public func lengthOfBytesUsingEncoding(encoding: NSStringEncoding) -> Int {
    return _ns.lengthOfBytesUsingEncoding(encoding)
  }

  // - (NSRange)lineRangeForRange:(NSRange)aRange

  /// Returns the range of characters representing the line or lines
  /// containing a given range.
  public func lineRangeForRange(aRange: Range<Index>) -> Range<Index> {
    return _range(_ns.lineRangeForRange(_toNSRange(aRange)))
  }

  // - (NSArray *)
  //     linguisticTagsInRange:(NSRange)range
  //     scheme:(NSString *)tagScheme
  //     options:(NSLinguisticTaggerOptions)opts
  //     orthography:(NSOrthography *)orthography
  //     tokenRanges:(NSArray**)tokenRanges

  /// Returns an array of linguistic tags for the specified
  /// range and requested tags within the receiving string.
  public func linguisticTagsInRange(
    range: Range<Index>, 
    scheme tagScheme: String, 
    options opts: NSLinguisticTaggerOptions = nil,
    orthography: NSOrthography? = nil, 
    tokenRanges: UnsafeMutablePointer<[Range<Index>]> = nil // FIXME:Can this be nil?
  ) -> [String] {
    var nsTokenRanges: NSArray? = nil
    let result = tokenRanges._withBridgeObject(&nsTokenRanges) {
      self._ns.linguisticTagsInRange(
        _toNSRange(range), scheme: tagScheme, options: opts,
        orthography: orthography != nil ? orthography! : nil, tokenRanges: $0) as NSArray
    }
    
    if nsTokenRanges != nil {
      tokenRanges._setIfNonNil {
        (nsTokenRanges! as [AnyObject]).map {
          self._range($0.rangeValue)
        }
      }
    }
    
    return _convertNSArrayToArray(result)
  }

  // - (NSComparisonResult)localizedCaseInsensitiveCompare:(NSString *)aString

  /// Compares the string and a given string using a
  /// case-insensitive, localized, comparison.
  public
  func localizedCaseInsensitiveCompare(aString: String) -> NSComparisonResult {
    return _ns.localizedCaseInsensitiveCompare(aString)
  }

  // - (NSComparisonResult)localizedCompare:(NSString *)aString

  /// Compares the string and a given string using a localized
  /// comparison.
  public func localizedCompare(aString: String) -> NSComparisonResult {
    return _ns.localizedCompare(aString)
  }

  /// Compares strings as sorted by the Finder.
  public func localizedStandardCompare(string: String) -> NSComparisonResult {
    return _ns.localizedStandardCompare(string)
  }

  //===--- Omitted for consistency with API review results 5/20/2014 ------===//
  // @property long long longLongValue

  // - (NSString *)lowercaseStringWithLocale:(NSLocale *)locale

  /// Returns a version of the string with all letters
  /// converted to lowercase, taking into account the specified
  /// locale.
  public func lowercaseStringWithLocale(locale: NSLocale) -> String {
    return _ns.lowercaseStringWithLocale(locale)
  }

  // - (NSUInteger)maximumLengthOfBytesUsingEncoding:(NSStringEncoding)enc

  /// Returns the maximum number of bytes needed to store the
  /// `String` in a given encoding.
  public
  func maximumLengthOfBytesUsingEncoding(encoding: NSStringEncoding) -> Int {
    return _ns.maximumLengthOfBytesUsingEncoding(encoding)
  }

  // - (NSRange)paragraphRangeForRange:(NSRange)aRange

  /// Returns the range of characters representing the
  /// paragraph or paragraphs containing a given range.
  public func paragraphRangeForRange(aRange: Range<Index>) -> Range<Index> {
    return _range(_ns.paragraphRangeForRange(_toNSRange(aRange)))
  }

  // @property NSArray* pathComponents

  /// Returns an array of NSString objects containing, in
  /// order, each path component of the `String`.
  public var pathComponents: [String] {
    return _ns.pathComponents as! [String]
  }

  // @property NSString* pathExtension;

  /// Interprets the `String` as a path and returns the
  /// `String`’s extension, if any.
  public var pathExtension: String {
    return _ns.pathExtension
  }

  // @property NSString* precomposedStringWithCanonicalMapping;

  /// Returns a string made by normalizing the `String`’s
  /// contents using Form C.
  public var precomposedStringWithCanonicalMapping: String {
    return _ns.precomposedStringWithCanonicalMapping
  }

  // @property NSString * precomposedStringWithCompatibilityMapping;

  /// Returns a string made by normalizing the `String`’s
  /// contents using Form KC.
  public var precomposedStringWithCompatibilityMapping: String {
    return _ns.precomposedStringWithCompatibilityMapping
  }

  // - (id)propertyList

  /// Parses the `String` as a text representation of a
  /// property list, returning an NSString, NSData, NSArray, or
  /// NSDictionary object, according to the topmost element.
  public func propertyList() -> AnyObject {
    return _ns.propertyList()
  }

  // - (NSDictionary *)propertyListFromStringsFileFormat

  /// Returns a dictionary object initialized with the keys and
  /// values found in the `String`.
  public
  func propertyListFromStringsFileFormat() -> [String : String] {
    return _ns.propertyListFromStringsFileFormat() as! [String : String]
  }

  // - (NSRange)rangeOfCharacterFromSet:(NSCharacterSet *)aSet
  //
  // - (NSRange)
  //     rangeOfCharacterFromSet:(NSCharacterSet *)aSet
  //     options:(NSStringCompareOptions)mask
  //
  // - (NSRange)
  //     rangeOfCharacterFromSet:(NSCharacterSet *)aSet
  //     options:(NSStringCompareOptions)mask
  //     range:(NSRange)aRange
  
  /// Finds and returns the range in the `String` of the first
  /// character from a given character set found in a given range with
  /// given options.
  public func rangeOfCharacterFromSet(
    aSet: NSCharacterSet,
    options mask:NSStringCompareOptions = nil,
    range aRange: Range<Index>? = nil
  )-> Range<Index>? {
    return _optionalRange(
      _ns.rangeOfCharacterFromSet(
        aSet, options: mask,
        range: _toNSRange(aRange != nil ? aRange! : indices(self))))
  }

  // - (NSRange)rangeOfComposedCharacterSequenceAtIndex:(NSUInteger)anIndex

  /// Returns the range in the `String` of the composed
  /// character sequence located at a given index.
  public
  func rangeOfComposedCharacterSequenceAtIndex(anIndex: Index) -> Range<Index> {
    return _range(
      _ns.rangeOfComposedCharacterSequenceAtIndex(anIndex._utf16Index))
  }

  // - (NSRange)rangeOfComposedCharacterSequencesForRange:(NSRange)range

  /// Returns the range in the string of the composed character
  /// sequences for a given range.
  public func rangeOfComposedCharacterSequencesForRange(
    range: Range<Index>
  ) -> Range<Index> {
    // Theoretically, this will be the identity function.  In practice
    // I think users will be able to observe differences in the input
    // and output ranges due (if nothing else) to locale changes
    return _range(
      _ns.rangeOfComposedCharacterSequencesForRange(_toNSRange(range)))
  }

  // - (NSRange)rangeOfString:(NSString *)aString
  //
  // - (NSRange)
  //     rangeOfString:(NSString *)aString options:(NSStringCompareOptions)mask
  //
  // - (NSRange)
  //     rangeOfString:(NSString *)aString
  //     options:(NSStringCompareOptions)mask
  //     range:(NSRange)aRange
  //
  // - (NSRange)
  //     rangeOfString:(NSString *)aString
  //     options:(NSStringCompareOptions)mask
  //     range:(NSRange)searchRange
  //     locale:(NSLocale *)locale

  /// Finds and returns the range of the first occurrence of a
  /// given string within a given range of the `String`, subject to
  /// given options, using the specified locale, if any.
  public func rangeOfString(
    aString: String,
    options mask: NSStringCompareOptions = nil,
    range searchRange: Range<Index>? = nil,
    locale: NSLocale? = nil
  ) -> Range<Index>? {
    return _optionalRange(
      locale != nil ? _ns.rangeOfString(
        aString, options: mask,
        range: _toNSRange(searchRange != nil ? searchRange! : indices(self)),
        locale: locale
      )
      : searchRange != nil ? _ns.rangeOfString(
        aString, options: mask, range: _toNSRange(searchRange!)
      )
      : mask != nil ? _ns.rangeOfString(aString, options: mask)
      : _ns.rangeOfString(aString)
    )
  }

  // @property NSStringEncoding smallestEncoding;

  /// Returns the smallest encoding to which the `String` can
  /// be converted without loss of information.
  public var smallestEncoding: NSStringEncoding {
    return _ns.smallestEncoding
  }

  // @property NSString *stringByAbbreviatingWithTildeInPath;

  /// Returns a new string that replaces the current home
  /// directory portion of the current path with a tilde (`~`)
  /// character.
  public var stringByAbbreviatingWithTildeInPath: String {
    return _ns.stringByAbbreviatingWithTildeInPath
  }

  // - (NSString *)
  //     stringByAddingPercentEncodingWithAllowedCharacters:
  //       (NSCharacterSet *)allowedCharacters

  /// Returns a new string made from the `String` by replacing
  /// all characters not in the specified set with percent encoded
  /// characters.
  public func stringByAddingPercentEncodingWithAllowedCharacters(
    allowedCharacters: NSCharacterSet
  ) -> String? {
    // FIXME: the documentation states that this method can return nil if the
    // transformation is not possible, without going into futher details.  The
    // implementation can only return nil if malloc() returns nil, so in
    // practice this is not possible.  Still, to be consistent with
    // documentation, we declare the method as returning an optional String.
    //
    // <rdar://problem/17901698> Docs for -[NSString
    // stringByAddingPercentEncodingWithAllowedCharacters] don't precisely
    // describe when return value is nil
    return _ns.stringByAddingPercentEncodingWithAllowedCharacters(
      allowedCharacters
    )
  }

  // - (NSString *)
  //     stringByAddingPercentEscapesUsingEncoding:(NSStringEncoding)encoding

  /// Returns a representation of the `String` using a given
  /// encoding to determine the percent escapes necessary to convert
  /// the `String` into a legal URL string.
  public func stringByAddingPercentEscapesUsingEncoding(
    encoding: NSStringEncoding
  ) -> String? {
    return _ns.stringByAddingPercentEscapesUsingEncoding(encoding)
  }

  // - (NSString *)stringByAppendingFormat:(NSString *)format, ...

  /// Returns a string made by appending to the `String` a
  /// string constructed from a given format string and the following
  /// arguments.
  public func stringByAppendingFormat(
    format: String, _ arguments: CVarArgType...
  ) -> String {
    return _ns.stringByAppendingString(
      String(format: format, arguments: arguments))
  }

  // - (NSString *)stringByAppendingPathComponent:(NSString *)aString

  /// Returns a new string made by appending to the `String` a given string.
  public func stringByAppendingPathComponent(aString: String) -> String {
    return _ns.stringByAppendingPathComponent(aString)
  }

  // - (NSString *)stringByAppendingPathExtension:(NSString *)ext

  /// Returns a new string made by appending to the `String` an
  /// extension separator followed by a given extension.
  public func stringByAppendingPathExtension(ext: String) -> String? {
    // FIXME: This method can return nil in practice, for example when self is
    // an empty string.  OTOH, this is not documented, documentatios says that
    // it always returns a string.
    //
    // <rdar://problem/17902469> -[NSString stringByAppendingPathExtension] can
    // return nil
    return _ns.stringByAppendingPathExtension(ext)
  }

  // - (NSString *)stringByAppendingString:(NSString *)aString

  /// Returns a new string made by appending a given string to
  /// the `String`.
  public func stringByAppendingString(aString: String) -> String {
    return _ns.stringByAppendingString(aString)
  }

  // @property NSString* stringByDeletingLastPathComponent;

  /// Returns a new string made by deleting the last path
  /// component from the `String`, along with any final path
  /// separator.
  public var stringByDeletingLastPathComponent: String {
    return _ns.stringByDeletingLastPathComponent
  }

  // @property NSString* stringByDeletingPathExtension;

  /// Returns a new string made by deleting the extension (if
  /// any, and only the last) from the `String`.
  public var stringByDeletingPathExtension: String {
    return _ns.stringByDeletingPathExtension
  }

  // @property NSString* stringByExpandingTildeInPath;

  /// Returns a new string made by expanding the initial
  /// component of the `String` to its full path value.
  public var stringByExpandingTildeInPath: String {
    return _ns.stringByExpandingTildeInPath
  }

  // - (NSString *)
  //     stringByFoldingWithOptions:(NSStringCompareOptions)options
  //     locale:(NSLocale *)locale

  /// Returns a string with the given character folding options
  /// applied.
  public func stringByFoldingWithOptions(
    options: NSStringCompareOptions, locale: NSLocale
  ) -> String {
    return _ns.stringByFoldingWithOptions(options, locale: locale)
  }

  // - (NSString *)stringByPaddingToLength:(NSUInteger)newLength
  //     withString:(NSString *)padString
  //     startingAtIndex:(NSUInteger)padIndex

  /// Returns a new string formed from the `String` by either
  /// removing characters from the end, or by appending as many
  /// occurrences as necessary of a given pad string.
  public func stringByPaddingToLength(
    newLength: Int, withString padString: String, startingAtIndex padIndex: Int
  ) -> String {
    return _ns.stringByPaddingToLength(
      newLength, withString: padString, startingAtIndex: padIndex)
  }

  // @property NSString* stringByRemovingPercentEncoding;

  /// Returns a new string made from the `String` by replacing
  /// all percent encoded sequences with the matching UTF-8
  /// characters.
  public var stringByRemovingPercentEncoding: String? {
    return _ns.stringByRemovingPercentEncoding
  }

  // - (NSString *)
  //     stringByReplacingCharactersInRange:(NSRange)range
  //     withString:(NSString *)replacement

  /// Returns a new string in which the characters in a
  /// specified range of the `String` are replaced by a given string.
  public func stringByReplacingCharactersInRange(
    range: Range<Index>, withString replacement: String
  ) -> String {
    return _ns.stringByReplacingCharactersInRange(
      _toNSRange(range), withString: replacement)
  }

  // - (NSString *)
  //     stringByReplacingOccurrencesOfString:(NSString *)target
  //     withString:(NSString *)replacement
  //
  // - (NSString *)
  //     stringByReplacingOccurrencesOfString:(NSString *)target
  //     withString:(NSString *)replacement
  //     options:(NSStringCompareOptions)options
  //     range:(NSRange)searchRange

  /// Returns a new string in which all occurrences of a target
  /// string in a specified range of the `String` are replaced by
  /// another given string.
  public func stringByReplacingOccurrencesOfString(
    target: String,
    withString replacement: String,
    options: NSStringCompareOptions = nil,
    range searchRange: Range<Index>? = nil
  ) -> String {
    return (searchRange != nil) || (options != nil)
    ? _ns.stringByReplacingOccurrencesOfString(
      target,
      withString: replacement, options: options,
      range: _toNSRange(searchRange != nil ? searchRange! : indices(self))
    )
    : _ns.stringByReplacingOccurrencesOfString(target, withString: replacement)
  }

  // - (NSString *)
  //     stringByReplacingPercentEscapesUsingEncoding:(NSStringEncoding)encoding

  /// Returns a new string made by replacing in the `String`
  /// all percent escapes with the matching characters as determined
  /// by a given encoding.
  public func stringByReplacingPercentEscapesUsingEncoding(
    encoding: NSStringEncoding
  ) -> String? {
    return _ns.stringByReplacingPercentEscapesUsingEncoding(encoding)
  }

  // @property NSString* stringByResolvingSymlinksInPath;

  /// Returns a new string made from the `String` by resolving
  /// all symbolic links and standardizing path.
  public var stringByResolvingSymlinksInPath: String {
    return _ns.stringByResolvingSymlinksInPath
  }

  // @property NSString* stringByStandardizingPath;

  /// Returns a new string made by removing extraneous path
  /// components from the `String`.
  public var stringByStandardizingPath: String {
    return _ns.stringByStandardizingPath
  }

  // - (NSString *)stringByTrimmingCharactersInSet:(NSCharacterSet *)set

  /// Returns a new string made by removing from both ends of
  /// the `String` characters contained in a given character set.
  public func stringByTrimmingCharactersInSet(set: NSCharacterSet) -> String {
    return _ns.stringByTrimmingCharactersInSet(set)
  }

  // - (NSArray *)stringsByAppendingPaths:(NSArray *)paths

  /// Returns an array of strings made by separately appending
  /// to the `String` each string in in a given array.
  public func stringsByAppendingPaths(paths: [String]) -> [String] {
    return _ns.stringsByAppendingPaths(paths) as! [String]
  }

  // - (NSString *)substringFromIndex:(NSUInteger)anIndex

  /// Returns a new string containing the characters of the
  /// `String` from the one at a given index to the end.
  public func substringFromIndex(index: Index) -> String {
    return _ns.substringFromIndex(index._utf16Index)
  }

  // - (NSString *)substringToIndex:(NSUInteger)anIndex

  /// Returns a new string containing the characters of the
  /// `String` up to, but not including, the one at a given index.
  public func substringToIndex(index: Index) -> String {
    return _ns.substringToIndex(index._utf16Index)
  }

  // - (NSString *)substringWithRange:(NSRange)aRange

  /// Returns a string object containing the characters of the
  /// `String` that lie within a given range.
  public func substringWithRange(aRange: Range<Index>) -> String {
    return _ns.substringWithRange(_toNSRange(aRange))
  }


  // - (NSString *)uppercaseStringWithLocale:(NSLocale *)locale

  /// Returns a version of the string with all letters
  /// converted to uppercase, taking into account the specified
  /// locale.
  public func uppercaseStringWithLocale(locale: NSLocale) -> String {
    return _ns.uppercaseStringWithLocale(locale)
  }

  //===--- Omitted due to redundancy with "utf8" property -----------------===//
  // - (const char *)UTF8String

  // - (BOOL)
  //     writeToFile:(NSString *)path
  //     atomically:(BOOL)useAuxiliaryFile
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error

  /// Writes the contents of the `String` to a file at a given
  /// path using a given encoding.
  public func writeToFile(
    path: String, atomically useAuxiliaryFile:Bool,
    encoding enc: NSStringEncoding, error: NSErrorPointer = nil
  ) -> Bool {
    return self._ns.writeToFile(
      path, atomically: useAuxiliaryFile, encoding: enc, error: error)
  }

  // - (BOOL)
  //     writeToURL:(NSURL *)url
  //     atomically:(BOOL)useAuxiliaryFile
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error

  /// Writes the contents of the `String` to the URL specified
  /// by url using the specified encoding.
  public func writeToURL(
    url: NSURL, atomically useAuxiliaryFile: Bool,
    encoding enc: NSStringEncoding, error: NSErrorPointer = nil
  ) -> Bool {
    return self._ns.writeToURL(
      url, atomically: useAuxiliaryFile, encoding: enc, error: error)
  }
}

