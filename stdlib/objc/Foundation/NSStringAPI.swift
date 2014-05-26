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

func _toNSArray<T, U:AnyObject>(a: T[], f: (T)->U) -> NSArray {
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
  var lastChar = _asUTF16CodeUnit(".") // anything other than % would work here
  var count = 0

  for c in a.unicodeScalars {
    if lastChar == _asUTF16CodeUnit("%") {
      if c == "%" {
        // a "%" following this one should not be taken as literal
        lastChar = _asUTF16CodeUnit(".") 
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

  //===--- Bridging Helpers -----------------------------------------------===//
  //===--------------------------------------------------------------------===//
  
  /// The corresponding `NSString` - a convenience for bridging code.
  var _ns: NSString {
    return self as NSString
  }

  /// Return an `Index` corresponding to the given offset in our UTF16
  /// representation.
  func _index(utf16Index: Int) -> Index {
    return Index(String.UnicodeScalarView.IndexType(utf16Index, core))
  }

  /// Return a `Range<Index>` corresponding to the given `NSRange` of
  /// our UTF16 representation.
  func _range(r: NSRange) -> Range<Index> {
    return _index(r.location).._index(r.location + r.length)
  }

  /// Invoke `body` on an `Int` buffer.  If `index` was converted from
  /// non-`nil`, convert the buffer to an `Index` and write it into the
  /// memory referred to by `index`
  func _withOptionalOutParameter<Result>(
    index: CMutablePointer<Index>,
    body: (CMutablePointer<Int>)->Result
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
    range: CMutablePointer<Range<Index>>,
    body: (CMutablePointer<NSRange>)->Result
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
  static func availableStringEncodings() -> NSStringEncoding[] {
    var result = NSStringEncoding[]()
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
  static func defaultCStringEncoding() -> NSStringEncoding {
    return NSString.defaultCStringEncoding()
  }

  // + (NSString *)localizedNameOfStringEncoding:(NSStringEncoding)encoding

  /// Returns a human-readable string giving the name of a given encoding.
  static func localizedNameOfStringEncoding(
    encoding: NSStringEncoding
  ) -> String {
    return NSString.localizedNameOfStringEncoding(encoding)
  }

  // + (instancetype)localizedStringWithFormat:(NSString *)format, ...

  /// Returns a string created by using a given format string as a
  /// template into which the remaining argument values are substituted
  /// according to the user's default locale.
  static func localizedStringWithFormat(
    format: String, _ arguments: CVarArg...
  ) -> String {
    return String(format: format, arguments: arguments)
  }

  // + (NSString *)pathWithComponents:(NSArray *)components

  /// Returns a string built from the strings in a given array
  /// by concatenating them with a path separator between each pair.
  static func pathWithComponents(components: String[]) -> String {
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

  /// Returns a string created by reading data from the file at a
  /// given path interpreted using a given encoding.
  static func stringWithContentsOfFile(
    path: String,
    encoding enc: NSStringEncoding,
    error: NSErrorPointer = nil
  ) -> String? {
    return NSString.stringWithContentsOfFile(path, encoding: enc, error: error)
  }

  // + (instancetype)
  //     stringWithContentsOfFile:(NSString *)path
  //     usedEncoding:(NSStringEncoding *)
  //     enc error:(NSError **)error

  /// Returns a string created by reading data from the file at
  /// a given path and returns by reference the encoding used to
  /// interpret the file.
  static func stringWithContentsOfFile(
    path: String,
    usedEncoding: CMutablePointer<NSStringEncoding> = nil,
    error: NSErrorPointer = nil
  ) -> String? {
    return usedEncoding.withUnsafePointer {
      NSString.stringWithContentsOfFile(path, usedEncoding: $0, error: error)
    }
  }

  // + (instancetype)
  //     stringWithContentsOfURL:(NSURL *)url
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error

  /// Returns a string created by reading data from a given URL
  /// interpreted using a given encoding.  Errors are written into the
  /// inout `error` argument.
  static func stringWithContentsOfURL(
    url: NSURL, encoding enc: NSStringEncoding, error: NSErrorPointer = nil
  ) -> String? {
    return NSString.stringWithContentsOfURL(url, encoding: enc, error: error)
  }

  // + (instancetype)
  //     stringWithContentsOfURL:(NSURL *)url
  //     usedEncoding:(NSStringEncoding *)enc
  //     error:(NSError **)error

  /// Returns a string created by reading data from a given URL
  /// and returns by reference the encoding used to interpret the
  /// data.  Errors are written into the inout `error` argument.
  static func stringWithContentsOfURL(
    url: NSURL,
    usedEncoding enc: CMutablePointer<NSStringEncoding> = nil,
    error: NSErrorPointer = nil
  ) -> String? {
    return enc.withUnsafePointer {
      NSString.stringWithContentsOfURL(url, usedEncoding: $0, error: error)
    }
  }

  // + (instancetype)
  //     stringWithCString:(const char *)cString
  //     encoding:(NSStringEncoding)enc

  /// Returns a string containing the bytes in a given C array,
  /// interpreted according to a given encoding.
  static func stringWithCString(
    cString: CString,
    encoding enc: NSStringEncoding
  ) -> String? {
    return NSString.stringWithCString(cString, encoding: enc)
  }


  //===--- Adds nothing for String beyond what String(s) does -------------===//
  // + (instancetype)stringWithString:(NSString *)aString
  //===--------------------------------------------------------------------===//
  
  // + (instancetype)stringWithUTF8String:(const char *)bytes

  /// Returns a string created by copying the data from a given
  /// C array of UTF8-encoded bytes.
  static func stringWithUTF8String(bytes: CString) -> String? {
    return NSString.stringWithUTF8String(bytes)
  }

  //===--- Instance Methods/Properties-------------------------------------===//
  //===--------------------------------------------------------------------===//

  //===--- Omitted by agreement during API review 5/20/2014 ---------------===//
  // @property BOOL boolValue;
  
  // - (BOOL)canBeConvertedToEncoding:(NSStringEncoding)encoding

  /// Returns a Boolean value that indicates whether the
  /// `String` can be converted to a given encoding without loss of
  /// information.
  func canBeConvertedToEncoding(encoding: NSStringEncoding) -> Bool {
    return _ns.canBeConvertedToEncoding(encoding)
  }

  // @property NSString* capitalizedString

  /// Produce a string with the first character from each word changed
  /// to the corresponding uppercase value.
  var capitalizedString: String {
    return _ns.capitalizedString as String
  }

  // - (NSString *)capitalizedStringWithLocale:(NSLocale *)locale

  /// Returns a capitalized representation of the `String`
  /// using the specified locale.
  func capitalizedStringWithLocale(locale: NSLocale?) -> String{
    return _ns.capitalizedStringWithLocale(locale) as String
  }


  // - (NSComparisonResult)caseInsensitiveCompare:(NSString *)aString

  /// Returns the result of invoking `compare:options:` with
  /// `NSCaseInsensitiveSearch` as the only option.
  func caseInsensitiveCompare(aString: String) -> NSComparisonResult {
    return _ns.caseInsensitiveCompare(aString as NSString)
  }

  //===--- Omitted by agreement during API review 5/20/2014 ---------------===//
  // - (unichar)characterAtIndex:(NSUInteger)index
  //
  // We have a different meaning for "Character" in Swift, and we are
  // trying not to expose error-prone UTF16 integer indexes
  
  // - (NSString *)
  //     commonPrefixWithString:(NSString *)aString
  //     options:(NSStringCompareOptions)mask

  /// Returns a string containing characters the `String` and a
  /// given string have in common, starting from the beginning of each
  /// up to the first characters that aren’t equivalent.
  func commonPrefixWithString(
    aString: String, options: NSStringCompareOptions) -> String {
    return _ns.commonPrefixWithString(
      aString as NSString, options: options) as String
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
  func compare(
    aString: String,
    options mask: NSStringCompareOptions = nil,
    range: Range<Index>? = nil,
    locale: NSLocale? = nil
  ) -> NSComparisonResult {
    // According to Ali Ozer, there may be some real advantage to
    // dispatching to the minimal selector for the supplied options.
    // So let's do that; the switch should compile away anyhow.
    return locale ? _ns.compare(
      aString, options: mask,
      range: _toNSRange(range ? range! : indices(self)), locale: locale)
    
    : range ? _ns.compare(
      aString, options: mask, range: _toNSRange(range ? range! : indices(self)))
      
    : mask ? _ns.compare(aString, options: mask)
      
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
  func completePathIntoString(
    _ outputName: CMutablePointer<String> = nil,
    caseSensitive: Bool,
    matchesIntoArray: CMutablePointer<String[]> = nil,
    filterTypes: String[]? = nil
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
      outputName._setIfNonNil { n }
    }
    return result
  }

  // - (NSArray *)
  //     componentsSeparatedByCharactersInSet:(NSCharacterSet *)separator

  /// Returns an array containing substrings from the `String`
  /// that have been divided by characters in a given set.
  func componentsSeparatedByCharactersInSet(
    separator: NSCharacterSet
  ) -> String[] {
    // FIXME: two steps due to <rdar://16971181>
    let nsa = _ns.componentsSeparatedByCharactersInSet(separator)
    // Since this function is effectively a bridge thunk, use the
    // bridge thunk semantics for the NSArray conversion
    return _convertNSArrayToArray(nsa)
  }


  // - (NSArray *)componentsSeparatedByString:(NSString *)separator

  /// Returns an array containing substrings from the `String`
  /// that have been divided by a given separator.
  func componentsSeparatedByString(separator: String) -> String[] {
    let nsa = _ns.componentsSeparatedByString(separator)
    // Since this function is effectively a bridge thunk, use the
    // bridge thunk semantics for the NSArray conversion
    return _convertNSArrayToArray(nsa)
  }

  // - (const char *)cStringUsingEncoding:(NSStringEncoding)encoding

  /// Returns a representation of the `String` as a C string
  /// using a given encoding.
  func cStringUsingEncoding(encoding: NSStringEncoding) -> CChar[]? {
    return withExtendedLifetime(_ns) {
      (s: NSString) -> CChar[]? in
      s.cStringUsingEncoding(encoding).persist()
    }
  }

  // - (NSData *)dataUsingEncoding:(NSStringEncoding)encoding
  //
  // - (NSData *)
  //     dataUsingEncoding:(NSStringEncoding)encoding
  //     allowLossyConversion:(BOOL)flag

  /// Returns an `NSData` object containing a representation of
  /// the `String` encoded using a given encoding.
  func dataUsingEncoding(
    encoding: NSStringEncoding,
    allowLossyConversion: Bool = false
  ) -> NSData? {
    return _ns.dataUsingEncoding(
      encoding, allowLossyConversion: allowLossyConversion)
  }

  // @property NSString* decomposedStringWithCanonicalMapping;

  /// Returns a string made by normalizing the `String`’s
  /// contents using Form D.
  var decomposedStringWithCanonicalMapping: String {
    return _ns.decomposedStringWithCanonicalMapping
  }

  // @property NSString* decomposedStringWithCompatibilityMapping;

  /// Returns a string made by normalizing the `String`’s
  /// contents using Form KD.
  var decomposedStringWithCompatibilityMapping: String {
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
  func enumerateLines(body: (line: String, inout stop: Bool)->()) {
    _ns.enumerateLinesUsingBlock {
      (line: String?, stop: CMutablePointer<ObjCBool>)
    in
      var stop_ = false
      body(line: line!, stop: &stop_)
      if stop_ {
        UnsafePointer(stop).memory = true
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
  func enumerateLinguisticTagsInRange(
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
      orthography: orthography ? orthography! : nil
    ) {
      var stop_ = false
      body($0, self._range($1), self._range($2), &stop_)
      if stop_ {
        UnsafePointer($3).memory = true
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
  func enumerateSubstringsInRange(
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
        UnsafePointer($3).memory = true
      }
    }
  }

  // @property NSStringEncoding fastestEncoding;

  /// Returns the fastest encoding to which the `String` may be
  /// converted without loss of information.
  var fastestEncoding: NSStringEncoding {
    return _ns.fastestEncoding
  }

  // - (const char *)fileSystemRepresentation

  /// Returns a file system-specific representation of the `String`.
  func fileSystemRepresentation() -> CChar[] {
    return _ns.fileSystemRepresentation.persist()!
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
  func getBytes(
    inout buffer: UInt8[],
    maxLength: Int,
    usedLength: CMutablePointer<Int>,
    encoding: NSStringEncoding,
    options: NSStringEncodingConversionOptions,
    range: Range<Index>,
    remainingRange: CMutablePointer<Range<Index>>
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
  func getCString(
    inout buffer: CChar[], maxLength: Int, encoding: NSStringEncoding
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
  func getFileSystemRepresentation(
    inout buffer: CChar[], maxLength: Int) -> Bool {
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
  func getLineStart(
    start: CMutablePointer<Index>,
    end: CMutablePointer<Index>,
    contentsEnd: CMutablePointer<Index>,
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
  func getParagraphStart(
    start: CMutablePointer<Index>,
    end: CMutablePointer<Index>,
    contentsEnd: CMutablePointer<Index>,
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
  var hash: Int {
    return _ns.hash
  }

  //===--- Already provided by String's core ------------------------------===//
  // - (instancetype)init

  //===--- Initializers that can fail presented as factory functions ------===//
  // - (instancetype)
  //     initWithBytes:(const void *)bytes
  //     length:(NSUInteger)length
  //     encoding:(NSStringEncoding)encoding

  /// Returns an initialized NSString object containing a given
  /// number of bytes from a given buffer of bytes interpreted in a
  /// given encoding.  Note: will store a maximum of
  /// `min(bytes.count, length)` bytes.
  static func stringWithBytes(
    var bytes: UInt8[], 
    length: Int, 
    encoding: NSStringEncoding
  ) -> String? {
    return NSString(
      bytes: &bytes, length: min(bytes.count, length), encoding: encoding
    ) as NSString? // HACK: FIXME: coerce to optional NSString to handle nil
  }

  // - (instancetype)
  //     initWithBytesNoCopy:(void *)bytes
  //     length:(NSUInteger)length
  //     encoding:(NSStringEncoding)encoding
  //     freeWhenDone:(BOOL)flag

  /// Returns an initialized `String` object that contains a
  /// given number of bytes from a given buffer of bytes interpreted
  /// in a given encoding, and optionally frees the buffer.  WARNING:
  /// this method is not memory-safe!
  static func stringWithBytesNoCopy(
    bytes: CMutableVoidPointer, length: Int,
    encoding: NSStringEncoding, freeWhenDone flag: Bool
  ) -> String? {
    return NSString(
      bytesNoCopy: bytes, length: length,
      encoding: encoding, freeWhenDone: flag
    ) as NSString? // Important: coerce to optional NSString to handle nil
  }


  // - (instancetype)
  //     initWithCharacters:(const unichar *)characters
  //     length:(NSUInteger)length
  
  /// Returns an initialized `String` object that contains a
  /// given number of characters from a given array of Unicode
  /// characters.
  init(
    utf16CodeUnits: CConstPointer<unichar>, 
    count: Int
  ) {
    self = NSString(characters: utf16CodeUnits, length: count)
  }

  // - (instancetype)
  //     initWithCharactersNoCopy:(unichar *)characters
  //     length:(NSUInteger)length
  //     freeWhenDone:(BOOL)flag

  /// Returns an initialized `String` object that contains a given
  /// number of characters from a given array of UTF16 Code Units
  init(
    utf16CodeUnitsNoCopy: CConstPointer<unichar>, 
    count: Int, 
    freeWhenDone flag: Bool
  ) {
    self = utf16CodeUnitsNoCopy.withUnsafePointer {
      NSString(charactersNoCopy: $0, length: count, freeWhenDone: flag)
    }
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
  init(format: String, _ _arguments: CVarArg...) {
    self = String(format: format, arguments: _arguments)
  }

  // - (instancetype)
  //     initWithFormat:(NSString *)format
  //     arguments:(va_list)argList

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to the user’s default locale.
  init(format: String, arguments: CVarArg[]) {
    self = String(format: format, locale: nil, arguments: arguments)
  }
  
  // - (instancetype)initWithFormat:(NSString *)format locale:(id)locale, ...

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to given locale information.
  init(format: String, locale: NSLocale?, _ args: CVarArg...) {
    self = String(format: format, locale: locale, arguments: args)
  }

  // - (instancetype)
  //     initWithFormat:(NSString *)format
  //     locale:(id)locale
  //     arguments:(va_list)argList

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to given locale information.
  init(format: String, locale: NSLocale?, arguments: CVarArg[]) {
    _precondition(
      _countFormatSpecifiers(format) <= arguments.count,
      "Too many format specifiers (%<letter>) provided for the argument list"
    )
    self = withVaList(arguments) {
      NSString(format: format, locale: locale, arguments: $0)
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
  var lastPathComponent: String {
    return _ns.lastPathComponent
  }

  //===--- Renamed by agreement during API review 5/20/2014 ---------------===//
  // @property NSUInteger length;

  /// Returns the number of Unicode characters in the `String`.
  var utf16count: Int {
    return _ns.length
  }

  // - (NSUInteger)lengthOfBytesUsingEncoding:(NSStringEncoding)enc

  /// Returns the number of bytes required to store the
  /// `String` in a given encoding.
  func lengthOfBytesUsingEncoding(encoding: NSStringEncoding) -> Int {
    return _ns.lengthOfBytesUsingEncoding(encoding)
  }

  // - (NSRange)lineRangeForRange:(NSRange)aRange

  /// Returns the range of characters representing the line or lines
  /// containing a given range.
  func lineRangeForRange(aRange: Range<Index>) -> Range<Index> {
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
  func linguisticTagsInRange(
    range: Range<Index>, 
    scheme tagScheme: String, 
    options opts: NSLinguisticTaggerOptions = nil,
    orthography: NSOrthography? = nil, 
    tokenRanges: CMutablePointer<Range<Index>[]> = nil // FIXME:Can this be nil?
  ) -> String[] {
    var nsTokenRanges: NSArray? = nil
    let result = tokenRanges._withBridgeObject(&nsTokenRanges) {
      self._ns.linguisticTagsInRange(
        _toNSRange(range), scheme: tagScheme, options: opts,
        orthography: orthography ? orthography! : nil, tokenRanges: $0)
    }
    
    if nsTokenRanges {
      tokenRanges._setIfNonNil {
        (nsTokenRanges! as AnyObject[]).map {
          self._range($0.rangeValue)
        }
      }
    }
    
    return _convertNSArrayToArray(result)
  }

  // - (NSComparisonResult)localizedCaseInsensitiveCompare:(NSString *)aString

  /// Compares the string and a given string using a
  /// case-insensitive, localized, comparison.
  func localizedCaseInsensitiveCompare(aString: String) -> NSComparisonResult {
    return _ns.localizedCaseInsensitiveCompare(aString)
  }

  // - (NSComparisonResult)localizedCompare:(NSString *)aString

  /// Compares the string and a given string using a localized
  /// comparison.
  func localizedCompare(aString: String) -> NSComparisonResult {
    return _ns.localizedCompare(aString)
  }

  /// Compares strings as sorted by the Finder.
  func localizedStandardCompare(string: String) -> NSComparisonResult {
    return _ns.localizedStandardCompare(string)
  }

  //===--- Omitted for consistency with API review results 5/20/2014 ------===//
  // @property long long longLongValue

  // - (NSString *)lowercaseStringWithLocale:(NSLocale *)locale

  /// Returns a version of the string with all letters
  /// converted to lowercase, taking into account the specified
  /// locale.
  func lowercaseStringWithLocale(locale: NSLocale) -> String {
    return _ns.lowercaseStringWithLocale(locale)
  }

  // - (NSUInteger)maximumLengthOfBytesUsingEncoding:(NSStringEncoding)enc

  /// Returns the maximum number of bytes needed to store the
  /// `String` in a given encoding.
  func maximumLengthOfBytesUsingEncoding(encoding: NSStringEncoding) -> Int {
    return _ns.maximumLengthOfBytesUsingEncoding(encoding)
  }

  // - (NSRange)paragraphRangeForRange:(NSRange)aRange

  /// Returns the range of characters representing the
  /// paragraph or paragraphs containing a given range.
  func paragraphRangeForRange(aRange: Range<Index>) -> Range<Index> {
    return _range(_ns.paragraphRangeForRange(_toNSRange(aRange)))
  }

  // @property NSArray* pathComponents

  /// Returns an array of NSString objects containing, in
  /// order, each path component of the `String`.
  var pathComponents: String[] {
    return _ns.pathComponents as String[]
  }

  // @property NSString* pathExtension;

  /// Interprets the `String` as a path and returns the
  /// `String`’s extension, if any.
  var pathExtension: String {
    return _ns.pathExtension
  }

  // @property NSString* precomposedStringWithCanonicalMapping;

  /// Returns a string made by normalizing the `String`’s
  /// contents using Form C.
  var precomposedStringWithCanonicalMapping: String {
    return _ns.precomposedStringWithCanonicalMapping
  }

  // @property NSString * precomposedStringWithCompatibilityMapping;

  /// Returns a string made by normalizing the `String`’s
  /// contents using Form KC.
  var precomposedStringWithCompatibilityMapping: String {
    return _ns.precomposedStringWithCompatibilityMapping
  }

  // - (id)propertyList

  /// Parses the `String` as a text representation of a
  /// property list, returning an NSString, NSData, NSArray, or
  /// NSDictionary object, according to the topmost element.
  func propertyList() -> AnyObject {
    return _ns.propertyList()
  }

  // - (NSDictionary *)propertyListFromStringsFileFormat

  /// Returns a dictionary object initialized with the keys and
  /// values found in the `String`.
  func propertyListFromStringsFileFormat() -> Dictionary<String, String> {
    var result = Dictionary<String, String>()
    _ns.propertyListFromStringsFileFormat().enumerateKeysAndObjectsUsingBlock {
      key, value, stop in
      result[key! as String] = value! as? String
    }
    return result
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
  func rangeOfCharacterFromSet(
    aSet: NSCharacterSet,
    options mask:NSStringCompareOptions = nil,
    range aRange: Range<Index>? = nil
  )-> Range<Index> {
    return _range(
      _ns.rangeOfCharacterFromSet(
        aSet, options: mask,
        range: _toNSRange(aRange ? aRange! : indices(self))))
  }

  // - (NSRange)rangeOfComposedCharacterSequenceAtIndex:(NSUInteger)anIndex

  /// Returns the range in the `String` of the composed
  /// character sequence located at a given index.
  func rangeOfComposedCharacterSequenceAtIndex(anIndex: Index) -> Range<Index> {
    return _range(
      _ns.rangeOfComposedCharacterSequenceAtIndex(anIndex._utf16Index))
  }

  // - (NSRange)rangeOfComposedCharacterSequencesForRange:(NSRange)range

  /// Returns the range in the string of the composed character
  /// sequences for a given range.
  func rangeOfComposedCharacterSequencesForRange(
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
  func rangeOfString(
    aString: String,
    options mask: NSStringCompareOptions = nil,
    range searchRange: Range<Index>? = nil,
    locale: NSLocale? = nil
  ) -> Range<Index> {
    return _range(
      locale ? _ns.rangeOfString(
        aString, options: mask,
        range: _toNSRange(searchRange ? searchRange! : indices(self)),
        locale: locale
      )
      : searchRange ? _ns.rangeOfString(
        aString, options: mask, range: _toNSRange(searchRange!)
      )
      : mask ? _ns.rangeOfString(aString, options: mask)
      : _ns.rangeOfString(aString)
    )
  }

  // @property NSStringEncoding smallestEncoding;

  /// Returns the smallest encoding to which the `String` can
  /// be converted without loss of information.
  var smallestEncoding: NSStringEncoding {
    return _ns.smallestEncoding
  }

  // - (NSString *)stringByAbbreviatingWithTildeInPath

  /// Returns a new string that replaces the current home
  /// directory portion of the current path with a tilde (`~`)
  /// character.
  func stringByAbbreviatingWithTildeInPath() -> String {
    return _ns.stringByAbbreviatingWithTildeInPath
  }

  // - (NSString *)
  //     stringByAddingPercentEncodingWithAllowedCharacters:
  //       (NSCharacterSet *)allowedCharacters

  /// Returns a new string made from the `String` by replacing
  /// all characters not in the specified set with percent encoded
  /// characters.
  func stringByAddingPercentEncodingWithAllowedCharacters(
    allowedCharacters: NSCharacterSet
  ) -> String {
    return _ns.stringByAddingPercentEncodingWithAllowedCharacters(
      allowedCharacters
    )
  }

  // - (NSString *)
  //     stringByAddingPercentEscapesUsingEncoding:(NSStringEncoding)encoding

  /// Returns a representation of the `String` using a given
  /// encoding to determine the percent escapes necessary to convert
  /// the `String` into a legal URL string.
  func stringByAddingPercentEscapesUsingEncoding(
    encoding: NSStringEncoding
  ) -> String {
    return _ns.stringByAddingPercentEscapesUsingEncoding(encoding)
  }

  // - (NSString *)stringByAppendingFormat:(NSString *)format, ...

  /// Returns a string made by appending to the `String` a
  /// string constructed from a given format string and the following
  /// arguments.
  func stringByAppendingFormat(
    format: String, _ arguments: CVarArg...
  ) -> String {
    return _ns.stringByAppendingString(
      String(format: format, arguments: arguments))
  }

  // - (NSString *)stringByAppendingPathComponent:(NSString *)aString

  /// Returns a new string made by appending to the `String` a given string.
  func stringByAppendingPathComponent(aString: String) -> String {
    return _ns.stringByAppendingPathComponent(aString)
  }

  // - (NSString *)stringByAppendingPathExtension:(NSString *)ext

  /// Returns a new string made by appending to the `String` an
  /// extension separator followed by a given extension.
  func stringByAppendingPathExtension(ext: String) -> String {
    return _ns.stringByAppendingPathExtension(ext)
  }

  // - (NSString *)stringByAppendingString:(NSString *)aString

  /// Returns a new string made by appending a given string to
  /// the `String`.
  func stringByAppendingString(aString: String) -> String {
    return _ns.stringByAppendingString(aString)
  }

  // @property NSString* stringByDeletingLastPathComponent;

  /// Returns a new string made by deleting the last path
  /// component from the `String`, along with any final path
  /// separator.
  var stringByDeletingLastPathComponent: String {
    return _ns.stringByDeletingLastPathComponent
  }

  // @property NSString* stringByDeletingPathExtension;

  /// Returns a new string made by deleting the extension (if
  /// any, and only the last) from the `String`.
  var stringByDeletingPathExtension: String {
    return _ns.stringByDeletingPathExtension
  }

  // @property NSString* stringByExpandingTildeInPath;

  /// Returns a new string made by expanding the initial
  /// component of the `String` to its full path value.
  var stringByExpandingTildeInPath: String {
    return _ns.stringByExpandingTildeInPath
  }

  // - (NSString *)
  //     stringByFoldingWithOptions:(NSStringCompareOptions)options
  //     locale:(NSLocale *)locale

  /// Returns a string with the given character folding options
  /// applied.
  func stringByFoldingWithOptions(
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
  func stringByPaddingToLength(
    newLength: Int, withString padString: String, startingAtIndex padIndex: Int
  ) -> String {
    return _ns.stringByPaddingToLength(
      newLength, withString: padString, startingAtIndex: padIndex)
  }

  // @property NSString* stringByRemovingPercentEncoding;

  /// Returns a new string made from the `String` by replacing
  /// all percent encoded sequences with the matching UTF-8
  /// characters.
  var stringByRemovingPercentEncoding: String {
    return _ns.stringByRemovingPercentEncoding
  }

  // - (NSString *)
  //     stringByReplacingCharactersInRange:(NSRange)range
  //     withString:(NSString *)replacement

  /// Returns a new string in which the characters in a
  /// specified range of the `String` are replaced by a given string.
  func stringByReplacingCharactersInRange(
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
  func stringByReplacingOccurrencesOfString(
    target: String,
    withString replacement: String,
    options: NSStringCompareOptions = nil,
    range searchRange: Range<Index>? = nil
  ) -> String {
    // FIXME: " != nil" needed pending <rdar://problem/16997968> 
    return searchRange || options != nil 
    ? _ns.stringByReplacingOccurrencesOfString(
      target,
      withString: replacement, options: options,
      range: _toNSRange(searchRange ? searchRange! : indices(self))
    )
    : _ns.stringByReplacingOccurrencesOfString(target, withString: replacement)
  }

  // - (NSString *)
  //     stringByReplacingPercentEscapesUsingEncoding:(NSStringEncoding)encoding

  /// Returns a new string made by replacing in the `String`
  /// all percent escapes with the matching characters as determined
  /// by a given encoding.
  func stringByReplacingPercentEscapesUsingEncoding(
    encoding: NSStringEncoding
  ) -> String {
    return _ns.stringByReplacingPercentEscapesUsingEncoding(encoding)
  }

  // @property NSString* stringByResolvingSymlinksInPath;

  /// Returns a new string made from the `String` by resolving
  /// all symbolic links and standardizing path.
  var stringByResolvingSymlinksInPath: String {
    return _ns.stringByResolvingSymlinksInPath
  }

  // @property NSString* stringByStandardizingPath;

  /// Returns a new string made by removing extraneous path
  /// components from the `String`.
  var stringByStandardizingPath: String {
    return _ns.stringByStandardizingPath
  }

  // - (NSString *)stringByTrimmingCharactersInSet:(NSCharacterSet *)set

  /// Returns a new string made by removing from both ends of
  /// the `String` characters contained in a given character set.
  func stringByTrimmingCharactersInSet(set: NSCharacterSet) -> String {
    return _ns.stringByTrimmingCharactersInSet(set)
  }

  // - (NSArray *)stringsByAppendingPaths:(NSArray *)paths

  /// Returns an array of strings made by separately appending
  /// to the `String` each string in in a given array.
  func stringsByAppendingPaths(paths: String[]) -> String[] {
    return _ns.stringsByAppendingPaths(paths) as String[]
  }

  // - (NSString *)substringFromIndex:(NSUInteger)anIndex

  /// Returns a new string containing the characters of the
  /// `String` from the one at a given index to the end.
  func substringFromIndex(index: Int) -> String {
    return _ns.substringFromIndex(index)
  }

  // - (NSString *)substringToIndex:(NSUInteger)anIndex

  /// Returns a new string containing the characters of the
  /// `String` up to, but not including, the one at a given index.
  func substringToIndex(index: Int) -> String {
    return _ns.substringToIndex(index)
  }

  // - (NSString *)substringWithRange:(NSRange)aRange

  /// Returns a string object containing the characters of the
  /// `String` that lie within a given range.
  func substringWithRange(aRange: Range<Index>) -> String {
    return _ns.substringWithRange(_toNSRange(aRange))
  }


  // - (NSString *)uppercaseStringWithLocale:(NSLocale *)locale

  /// Returns a version of the string with all letters
  /// converted to uppercase, taking into account the specified
  /// locale.
  func uppercaseStringWithLocale(locale: NSLocale) -> String {
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
  func writeToFile(
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
  func writeToURL(
    url: NSURL, atomically useAuxiliaryFile: Bool,
    encoding enc: NSStringEncoding, error: NSErrorPointer = nil
  ) -> Bool {
    return self._ns.writeToURL(
      url, atomically: useAuxiliaryFile, encoding: enc, error: error)
  }
}

@transparent
func == (lhs: NSString, rhs: NSString) -> Bool {
  return String(lhs) == String(rhs)
}

@transparent
func == (lhs: String, rhs: NSString) -> Bool {
  return lhs == String(rhs)
}

@transparent
func == (lhs: NSString, rhs: String) -> Bool {
  return String(lhs) == rhs
}

