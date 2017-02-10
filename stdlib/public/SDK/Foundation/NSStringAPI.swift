//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

func _toNSArray<T, U : AnyObject>(_ a: [T], f: (T) -> U) -> NSArray {
  let result = NSMutableArray(capacity: a.count)
  for s in a {
    result.add(f(s))
  }
  return result
}

func _toNSRange(_ r: Range<String.Index>) -> NSRange {
  return NSRange(
    location: r.lowerBound._utf16Index,
    length: r.upperBound._utf16Index - r.lowerBound._utf16Index)
}

// We only need this for UnsafeMutablePointer, but there's not currently a way
// to write that constraint.
extension Optional {
  /// Invokes `body` with `nil` if `self` is `nil`; otherwise, passes the
  /// address of `object` to `body`.
  ///
  /// This is intended for use with Foundation APIs that return an Objective-C
  /// type via out-parameter where it is important to be able to *ignore* that
  /// parameter by passing `nil`. (For some APIs, this may allow the
  /// implementation to avoid some work.)
  ///
  /// In most cases it would be simpler to just write this code inline, but if
  /// `body` is complicated than that results in unnecessarily repeated code.
  internal func _withNilOrAddress<NSType : AnyObject, ResultType>(
    of object: inout NSType?,
    _ body:
      (AutoreleasingUnsafeMutablePointer<NSType?>?) -> ResultType
  ) -> ResultType {
    return self == nil ? body(nil) : body(&object)
  }
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
  func _index(_ utf16Index: Int) -> Index {
    return Index(
      _base: String.UnicodeScalarView.Index(_position: utf16Index),
      in: characters
    )
  }

  /// Return a `Range<Index>` corresponding to the given `NSRange` of
  /// our UTF-16 representation.
  func _range(_ r: NSRange) -> Range<Index> {
    return _index(r.location)..<_index(r.location + r.length)
  }

  /// Return a `Range<Index>?` corresponding to the given `NSRange` of
  /// our UTF-16 representation.
  func _optionalRange(_ r: NSRange) -> Range<Index>? {
    if r.location == NSNotFound {
      return nil
    }
    return _range(r)
  }

  /// Invoke `body` on an `Int` buffer.  If `index` was converted from
  /// non-`nil`, convert the buffer to an `Index` and write it into the
  /// memory referred to by `index`
  func _withOptionalOutParameter<Result>(
    _ index: UnsafeMutablePointer<Index>?,
    _ body: (UnsafeMutablePointer<Int>?) -> Result
  ) -> Result {
    var utf16Index: Int = 0
    let result = (index != nil ? body(&utf16Index) : body(nil))
    index?.pointee = self._index(utf16Index)
    return result
  }

  /// Invoke `body` on an `NSRange` buffer.  If `range` was converted
  /// from non-`nil`, convert the buffer to a `Range<Index>` and write
  /// it into the memory referred to by `range`
  func _withOptionalOutParameter<Result>(
    _ range: UnsafeMutablePointer<Range<Index>>?,
    _ body: (UnsafeMutablePointer<NSRange>?) -> Result
  ) -> Result {
    var nsRange = NSRange(location: 0, length: 0)
    let result = (range != nil ? body(&nsRange) : body(nil))
    range?.pointee = self._range(nsRange)
    return result
  }

  //===--- Class Methods --------------------------------------------------===//
  //===--------------------------------------------------------------------===//

  // @property (class) const NSStringEncoding *availableStringEncodings;

  /// Returns an Array of the encodings string objects support
  /// in the application's environment.
  public static var availableStringEncodings: [Encoding] {
    var result = [Encoding]()
    var p = NSString.availableStringEncodings
    while p.pointee != 0 {
      result.append(Encoding(rawValue: p.pointee))
      p += 1
    }
    return result
  }

  // @property (class) NSStringEncoding defaultCStringEncoding;

  /// Returns the C-string encoding assumed for any method accepting
  /// a C string as an argument.
  public static var defaultCStringEncoding: Encoding {
    return Encoding(rawValue: NSString.defaultCStringEncoding)
  }

  // + (NSString *)localizedNameOfStringEncoding:(NSStringEncoding)encoding

  /// Returns a human-readable string giving the name of a given encoding.
  public static func localizedName(
    of encoding: Encoding
  ) -> String {
    return NSString.localizedName(of: encoding.rawValue)
  }

  // + (instancetype)localizedStringWithFormat:(NSString *)format, ...

  /// Returns a string created by using a given format string as a
  /// template into which the remaining argument values are substituted
  /// according to the user's default locale.
  public static func localizedStringWithFormat(
    _ format: String, _ arguments: CVarArg...
  ) -> String {
    return String(format: format, locale: Locale.current,
      arguments: arguments)
  }

  // + (NSString *)pathWithComponents:(NSArray *)components

  /// Returns a string built from the strings in a given array
  /// by concatenating them with a path separator between each pair.
  @available(*, unavailable, message: "Use fileURL(withPathComponents:) on URL instead.")
  public static func path(withComponents components: [String]) -> String {
    return NSString.path(withComponents: components)
  }

  //===--------------------------------------------------------------------===//
  // NSString factory functions that have a corresponding constructor
  // are omitted.
  //
  // + (instancetype)string
  //
  // + (instancetype)
  //     stringWithCharacters:(const unichar *)chars length:(NSUInteger)length
  //
  // + (instancetype)stringWithFormat:(NSString *)format, ...
  //
  // + (instancetype)
  //     stringWithContentsOfFile:(NSString *)path
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error
  //
  // + (instancetype)
  //     stringWithContentsOfFile:(NSString *)path
  //     usedEncoding:(NSStringEncoding *)enc
  //     error:(NSError **)error
  //
  // + (instancetype)
  //     stringWithContentsOfURL:(NSURL *)url
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error
  //
  // + (instancetype)
  //     stringWithContentsOfURL:(NSURL *)url
  //     usedEncoding:(NSStringEncoding *)enc
  //     error:(NSError **)error
  //
  // + (instancetype)
  //     stringWithCString:(const char *)cString
  //     encoding:(NSStringEncoding)enc
  //===--------------------------------------------------------------------===//

  //===--- Adds nothing for String beyond what String(s) does -------------===//
  // + (instancetype)stringWithString:(NSString *)aString
  //===--------------------------------------------------------------------===//

  // + (instancetype)stringWithUTF8String:(const char *)bytes

  /// Produces a string created by copying the data from a given
  /// C array of UTF8-encoded bytes.
  public init?(utf8String bytes: UnsafePointer<CChar>) {
    if let ns = NSString(utf8String: bytes) {
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
  public func canBeConverted(to encoding: Encoding) -> Bool {
    return _ns.canBeConverted(to: encoding.rawValue)
  }

  // @property NSString* capitalizedString

  /// Produce a string with the first character from each word changed
  /// to the corresponding uppercase value.
  public var capitalized: String {
    return _ns.capitalized as String
  }

  // @property (readonly, copy) NSString *localizedCapitalizedString NS_AVAILABLE(10_11, 9_0);

  /// A capitalized representation of the `String` that is produced
  /// using the current locale.
  @available(OSX 10.11, iOS 9.0, *)
  public var localizedCapitalized: String {
    return _ns.localizedCapitalized
  }

  // - (NSString *)capitalizedStringWithLocale:(Locale *)locale

  /// Returns a capitalized representation of the `String`
  /// using the specified locale.
  public func capitalized(with locale: Locale?) -> String {
    return _ns.capitalized(with: locale) as String
  }

  // - (NSComparisonResult)caseInsensitiveCompare:(NSString *)aString

  /// Returns the result of invoking `compare:options:` with
  /// `NSCaseInsensitiveSearch` as the only option.
  public func caseInsensitiveCompare(_ aString: String) -> ComparisonResult {
    return _ns.caseInsensitiveCompare(aString)
  }

  //===--- Omitted by agreement during API review 5/20/2014 ---------------===//
  // - (unichar)characterAtIndex:(NSUInteger)index
  //
  // We have a different meaning for "Character" in Swift, and we are
  // trying not to expose error-prone UTF-16 integer indexes

  // - (NSString *)
  //     commonPrefixWithString:(NSString *)aString
  //     options:(StringCompareOptions)mask

  /// Returns a string containing characters the `String` and a
  /// given string have in common, starting from the beginning of each
  /// up to the first characters that aren't equivalent.
  public func commonPrefix(
    with aString: String, options: CompareOptions = []) -> String {
    return _ns.commonPrefix(with: aString, options: options)
  }

  // - (NSComparisonResult)
  //     compare:(NSString *)aString
  //
  // - (NSComparisonResult)
  //     compare:(NSString *)aString options:(StringCompareOptions)mask
  //
  // - (NSComparisonResult)
  //     compare:(NSString *)aString options:(StringCompareOptions)mask
  //     range:(NSRange)range
  //
  // - (NSComparisonResult)
  //     compare:(NSString *)aString options:(StringCompareOptions)mask
  //     range:(NSRange)range locale:(id)locale

  /// Compares the string using the specified options and
  /// returns the lexical ordering for the range.
  public func compare(
    _ aString: String,
    options mask: CompareOptions = [],
    range: Range<Index>? = nil,
    locale: Locale? = nil
  ) -> ComparisonResult {
    // According to Ali Ozer, there may be some real advantage to
    // dispatching to the minimal selector for the supplied options.
    // So let's do that; the switch should compile away anyhow.
    return locale != nil ? _ns.compare(
      aString,
      options: mask,
      range: _toNSRange(
        range ?? self.characters.startIndex..<self.characters.endIndex
      ),
      locale: locale
    )

    : range != nil ? _ns.compare(
      aString,
      options: mask,
      range: _toNSRange(range!)
    )

    : !mask.isEmpty ? _ns.compare(aString, options: mask)

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
  public func completePath(
    into outputName: UnsafeMutablePointer<String>? = nil,
    caseSensitive: Bool,
    matchesInto outputArray: UnsafeMutablePointer<[String]>? = nil,
    filterTypes: [String]? = nil
  ) -> Int {
    var nsMatches: NSArray?
    var nsOutputName: NSString?

    let result: Int = outputName._withNilOrAddress(of: &nsOutputName) {
      outputName in outputArray._withNilOrAddress(of: &nsMatches) {
        outputArray in
        // FIXME: completePath(...) is incorrectly annotated as requiring
        // non-optional output parameters. rdar://problem/25494184
        let outputNonOptionalName = unsafeBitCast(
          outputName, to: AutoreleasingUnsafeMutablePointer<NSString?>.self)
        let outputNonOptionalArray = unsafeBitCast(
          outputArray, to: AutoreleasingUnsafeMutablePointer<NSArray?>.self)
        return self._ns.completePath(
          into: outputNonOptionalName,
          caseSensitive: caseSensitive,
          matchesInto: outputNonOptionalArray,
          filterTypes: filterTypes
        )
      }
    }

    if let matches = nsMatches {
      // Since this function is effectively a bridge thunk, use the
      // bridge thunk semantics for the NSArray conversion
      outputArray?.pointee = matches as! [String]
    }

    if let n = nsOutputName {
      outputName?.pointee = n as String
    }
    return result
  }

  // - (NSArray *)
  //     componentsSeparatedByCharactersInSet:(NSCharacterSet *)separator

  /// Returns an array containing substrings from the `String`
  /// that have been divided by characters in a given set.
  public func components(separatedBy separator: CharacterSet) -> [String] {
    // FIXME: two steps due to <rdar://16971181>
    let nsa = _ns.components(separatedBy: separator) as NSArray
    // Since this function is effectively a bridge thunk, use the
    // bridge thunk semantics for the NSArray conversion
    return nsa as! [String]
  }


  // - (NSArray *)componentsSeparatedByString:(NSString *)separator

  /// Returns an array containing substrings from the `String`
  /// that have been divided by a given separator.
  public func components(separatedBy separator: String) -> [String] {
    let nsa = _ns.components(separatedBy: separator) as NSArray
    // Since this function is effectively a bridge thunk, use the
    // bridge thunk semantics for the NSArray conversion
    return nsa as! [String]
  }

  // - (const char *)cStringUsingEncoding:(NSStringEncoding)encoding

  /// Returns a representation of the `String` as a C string
  /// using a given encoding.
  public func cString(using encoding: Encoding) -> [CChar]? {
    return withExtendedLifetime(_ns) {
      (s: NSString) -> [CChar]? in
      _persistCString(s.cString(using: encoding.rawValue))
    }
  }

  // - (NSData *)dataUsingEncoding:(NSStringEncoding)encoding
  //
  // - (NSData *)
  //     dataUsingEncoding:(NSStringEncoding)encoding
  //     allowLossyConversion:(BOOL)flag

  /// Returns a `Data` containing a representation of
  /// the `String` encoded using a given encoding.
  public func data(
    using encoding: Encoding,
    allowLossyConversion: Bool = false
  ) -> Data? {
    return _ns.data(
      using: encoding.rawValue,
      allowLossyConversion: allowLossyConversion)
  }

  // @property NSString* decomposedStringWithCanonicalMapping;

  /// Returns a string made by normalizing the `String`'s
  /// contents using Form D.
  public var decomposedStringWithCanonicalMapping: String {
    return _ns.decomposedStringWithCanonicalMapping
  }

  // @property NSString* decomposedStringWithCompatibilityMapping;

  /// Returns a string made by normalizing the `String`'s
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
  //     enumerateLinesUsing:(void (^)(NSString *line, BOOL *stop))block

  /// Enumerates all the lines in a string.
  public func enumerateLines(
    invoking body: @escaping (_ line: String, _ stop: inout Bool) -> Void
  ) {
    _ns.enumerateLines {
      (line: String, stop: UnsafeMutablePointer<ObjCBool>)
    in
      var stop_ = false
      body(line, &stop_)
      if stop_ {
        stop.pointee = true
      }
    }
  }

  // - (void)
  //     enumerateLinguisticTagsInRange:(NSRange)range
  //     scheme:(NSString *)tagScheme
  //     options:(LinguisticTaggerOptions)opts
  //     orthography:(Orthography *)orthography
  //     usingBlock:(
  //       void (^)(
  //         NSString *tag, NSRange tokenRange,
  //         NSRange sentenceRange, BOOL *stop)
  //       )block

  /// Performs linguistic analysis on the specified string by
  /// enumerating the specific range of the string, providing the
  /// Block with the located tags.
  public func enumerateLinguisticTags(
    in range: Range<Index>,
    scheme tagScheme: String,
    options opts: NSLinguisticTagger.Options = [],
    orthography: NSOrthography? = nil,
    invoking body:
      (String, Range<Index>, Range<Index>, inout Bool) -> Void
  ) {
    _ns.enumerateLinguisticTags(
      in: _toNSRange(range),
      scheme: tagScheme,
      options: opts,
      orthography: orthography != nil ? orthography! : nil
    ) {
      var stop_ = false
      body($0, self._range($1), self._range($2), &stop_)
      if stop_ {
        $3.pointee = true
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
  public func enumerateSubstrings(
    in range: Range<Index>,
    options opts: EnumerationOptions = [],
    _ body: @escaping (
      _ substring: String?, _ substringRange: Range<Index>,
      _ enclosingRange: Range<Index>, inout Bool
    ) -> Void
  ) {
    _ns.enumerateSubstrings(in: _toNSRange(range), options: opts) {
      var stop_ = false

      body($0,
        self._range($1),
        self._range($2),
        &stop_)

      if stop_ {
        UnsafeMutablePointer($3).pointee = true
      }
    }
  }

  // @property NSStringEncoding fastestEncoding;

  /// Returns the fastest encoding to which the `String` may be
  /// converted without loss of information.
  public var fastestEncoding: Encoding {
    return Encoding(rawValue: _ns.fastestEncoding)
  }

  // - (const char *)fileSystemRepresentation

  /// Returns a file system-specific representation of the `String`.
  @available(*, unavailable, message: "Use getFileSystemRepresentation on URL instead.")
  public var fileSystemRepresentation: [CChar] {
    return _persistCString(_ns.fileSystemRepresentation)!
  }

  //===--- Omitted for consistency with API review results 5/20/2014 ------===//
  // @property float floatValue;

  // - (BOOL)
  //     getBytes:(void *)buffer
  //     maxLength:(NSUInteger)maxBufferCount
  //     usedLength:(NSUInteger*)usedBufferCount
  //     encoding:(NSStringEncoding)encoding
  //     options:(StringEncodingConversionOptions)options
  //     range:(NSRange)range
  //     remainingRange:(NSRangePointer)leftover

  /// Writes the given `range` of characters into `buffer` in a given
  /// `encoding`, without any allocations.  Does not NULL-terminate.
  ///
  /// - Parameter buffer: A buffer into which to store the bytes from
  ///   the receiver. The returned bytes are not NUL-terminated.
  ///
  /// - Parameter maxBufferCount: The maximum number of bytes to write
  ///   to buffer.
  ///
  /// - Parameter usedBufferCount: The number of bytes used from
  ///   buffer. Pass `nil` if you do not need this value.
  ///
  /// - Parameter encoding: The encoding to use for the returned bytes.
  ///
  /// - Parameter options: A mask to specify options to use for
  ///   converting the receiver's contents to `encoding` (if conversion
  ///   is necessary).
  ///
  /// - Parameter range: The range of characters in the receiver to get.
  ///
  /// - Parameter leftover: The remaining range. Pass `nil` If you do
  ///   not need this value.
  ///
  /// - Returns: `true` iff some characters were converted.
  ///
  /// - Note: Conversion stops when the buffer fills or when the
  ///   conversion isn't possible due to the chosen encoding.
  ///
  /// - Note: will get a maximum of `min(buffer.count, maxLength)` bytes.
  public func getBytes(
    _ buffer: inout [UInt8],
    maxLength maxBufferCount: Int,
    usedLength usedBufferCount: UnsafeMutablePointer<Int>,
    encoding: Encoding,
    options: EncodingConversionOptions = [],
    range: Range<Index>,
    remaining leftover: UnsafeMutablePointer<Range<Index>>
  ) -> Bool {
    return _withOptionalOutParameter(leftover) {
      self._ns.getBytes(
        &buffer,
        maxLength: Swift.min(buffer.count, maxBufferCount),
        usedLength: usedBufferCount,
        encoding: encoding.rawValue,
        options: options,
        range: _toNSRange(range),
        remaining: $0)
    }
  }

  // - (BOOL)
  //     getCString:(char *)buffer
  //     maxLength:(NSUInteger)maxBufferCount
  //     encoding:(NSStringEncoding)encoding

  /// Converts the `String`'s content to a given encoding and
  /// stores them in a buffer.
  /// - Note: will store a maximum of `min(buffer.count, maxLength)` bytes.
  public func getCString(
    _ buffer: inout [CChar], maxLength: Int, encoding: Encoding
  ) -> Bool {
    return _ns.getCString(&buffer,
                          maxLength: Swift.min(buffer.count, maxLength),
                          encoding: encoding.rawValue)
  }

  // - (BOOL)
  //     getFileSystemRepresentation:(char *)buffer
  //     maxLength:(NSUInteger)maxLength

  /// Interprets the `String` as a system-independent path and
  /// fills a buffer with a C-string in a format and encoding suitable
  /// for use with file-system calls.
  /// - Note: will store a maximum of `min(buffer.count, maxLength)` bytes.
  @available(*, unavailable, message: "Use getFileSystemRepresentation on URL instead.")
  public func getFileSystemRepresentation(
    _ buffer: inout [CChar], maxLength: Int) -> Bool {
    return _ns.getFileSystemRepresentation(
      &buffer, maxLength: Swift.min(buffer.count, maxLength))
  }

  // - (void)
  //     getLineStart:(NSUInteger *)startIndex
  //     end:(NSUInteger *)lineEndIndex
  //     contentsEnd:(NSUInteger *)contentsEndIndex
  //     forRange:(NSRange)aRange

  /// Returns by reference the beginning of the first line and
  /// the end of the last line touched by the given range.
  public func getLineStart(
    _ start: UnsafeMutablePointer<Index>,
    end: UnsafeMutablePointer<Index>,
    contentsEnd: UnsafeMutablePointer<Index>,
    for range: Range<Index>
  ) {
    _withOptionalOutParameter(start) {
      start in self._withOptionalOutParameter(end) {
        end in self._withOptionalOutParameter(contentsEnd) {
          contentsEnd in self._ns.getLineStart(
            start, end: end,
            contentsEnd: contentsEnd,
            for: _toNSRange(range))
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
    _ start: UnsafeMutablePointer<Index>,
    end: UnsafeMutablePointer<Index>,
    contentsEnd: UnsafeMutablePointer<Index>,
    for range: Range<Index>
  ) {
    _withOptionalOutParameter(start) {
      start in self._withOptionalOutParameter(end) {
        end in self._withOptionalOutParameter(contentsEnd) {
          contentsEnd in self._ns.getParagraphStart(
            start, end: end,
            contentsEnd: contentsEnd,
            for: _toNSRange(range))
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

  //===--- Initializers that can fail -------------------------------------===//
  // - (instancetype)
  //     initWithBytes:(const void *)bytes
  //     length:(NSUInteger)length
  //     encoding:(NSStringEncoding)encoding

  /// Produces an initialized `NSString` object equivalent to the given
  /// `bytes` interpreted in the given `encoding`.
  public init? <S: Sequence>(bytes: S, encoding: Encoding)
    where S.Iterator.Element == UInt8 {
    let byteArray = Array(bytes)
    if let ns = NSString(
      bytes: byteArray, length: byteArray.count, encoding: encoding.rawValue) {

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
    bytesNoCopy bytes: UnsafeMutableRawPointer, length: Int,
    encoding: Encoding, freeWhenDone flag: Bool
  ) {
    if let ns = NSString(
      bytesNoCopy: bytes, length: length, encoding: encoding.rawValue,
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
      charactersNoCopy: UnsafeMutablePointer(mutating: utf16CodeUnitsNoCopy),
      length: count,
      freeWhenDone: flag) as String
  }

  //===--- Initializers that can fail -------------------------------------===//

  // - (instancetype)
  //     initWithContentsOfFile:(NSString *)path
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error
  //

  /// Produces a string created by reading data from the file at a
  /// given path interpreted using a given encoding.
  public init(
    contentsOfFile path: String,
    encoding enc: Encoding
  ) throws {
    let ns = try NSString(contentsOfFile: path, encoding: enc.rawValue)
    self = ns as String
  }

  // - (instancetype)
  //     initWithContentsOfFile:(NSString *)path
  //     usedEncoding:(NSStringEncoding *)enc
  //     error:(NSError **)error

  /// Produces a string created by reading data from the file at
  /// a given path and returns by reference the encoding used to
  /// interpret the file.
  public init(
    contentsOfFile path: String,
    usedEncoding: inout Encoding
  ) throws {
    var enc: UInt = 0
    let ns = try NSString(contentsOfFile: path, usedEncoding: &enc)
    usedEncoding = Encoding(rawValue: enc)
    self = ns as String
  }

  public init(
    contentsOfFile path: String
  ) throws {
    let ns = try NSString(contentsOfFile: path, usedEncoding: nil)
    self = ns as String
  }

  // - (instancetype)
  //     initWithContentsOfURL:(NSURL *)url
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError**)error

  /// Produces a string created by reading data from a given URL
  /// interpreted using a given encoding.  Errors are written into the
  /// inout `error` argument.
  public init(
    contentsOf url: URL,
    encoding enc: Encoding
  ) throws {
    let ns = try NSString(contentsOf: url, encoding: enc.rawValue)
    self = ns as String
  }

  // - (instancetype)
  //     initWithContentsOfURL:(NSURL *)url
  //     usedEncoding:(NSStringEncoding *)enc
  //     error:(NSError **)error

  /// Produces a string created by reading data from a given URL
  /// and returns by reference the encoding used to interpret the
  /// data.  Errors are written into the inout `error` argument.
  public init(
    contentsOf url: URL,
    usedEncoding: inout Encoding
  ) throws {
    var enc: UInt = 0
    let ns = try NSString(contentsOf: url as URL, usedEncoding: &enc)
    usedEncoding = Encoding(rawValue: enc)
    self = ns as String
  }

  public init(
    contentsOf url: URL
  ) throws {
    let ns = try NSString(contentsOf: url, usedEncoding: nil)
    self = ns as String
  }

  // - (instancetype)
  //     initWithCString:(const char *)nullTerminatedCString
  //     encoding:(NSStringEncoding)encoding

  /// Produces a string containing the bytes in a given C array,
  /// interpreted according to a given encoding.
  public init?(
    cString: UnsafePointer<CChar>,
    encoding enc: Encoding
  ) {
    if let ns = NSString(cString: cString, encoding: enc.rawValue) {
      self = ns as String
    } else {
      return nil
    }
  }
  
  // FIXME: handle optional locale with default arguments
  
  // - (instancetype)
  //     initWithData:(NSData *)data
  //     encoding:(NSStringEncoding)encoding

  /// Returns a `String` initialized by converting given `data` into
  /// Unicode characters using a given `encoding`.
  public init?(data: Data, encoding: Encoding) {
    guard let s = NSString(data: data, encoding: encoding.rawValue) else { return nil }
    self = s as String
  }
  
  // - (instancetype)initWithFormat:(NSString *)format, ...

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted.
  public init(format: String, _ arguments: CVarArg...) {
    self = String(format: format, arguments: arguments)
  }

  // - (instancetype)
  //     initWithFormat:(NSString *)format
  //     arguments:(va_list)argList

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to the user's default locale.
  public init(format: String, arguments: [CVarArg]) {
    self = String(format: format, locale: nil, arguments: arguments)
  }

  // - (instancetype)initWithFormat:(NSString *)format locale:(id)locale, ...

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to given locale information.
  public init(format: String, locale: Locale?, _ args: CVarArg...) {
    self = String(format: format, locale: locale, arguments: args)
  }

  // - (instancetype)
  //     initWithFormat:(NSString *)format
  //     locale:(id)locale
  //     arguments:(va_list)argList

  /// Returns a `String` object initialized by using a given
  /// format string as a template into which the remaining argument
  /// values are substituted according to given locale information.
  public init(format: String, locale: Locale?, arguments: [CVarArg]) {
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
  @available(*, unavailable, message: "Use lastPathComponent on URL instead.")
  public var lastPathComponent: String {
    return _ns.lastPathComponent
  }

  //===--- Renamed by agreement during API review 5/20/2014 ---------------===//
  // @property NSUInteger length;

  /// Returns the number of Unicode characters in the `String`.
  @available(*, unavailable,
    message: "Take the count of a UTF-16 view instead, i.e. str.utf16.count")
  public var utf16Count: Int {
    return _ns.length
  }

  // - (NSUInteger)lengthOfBytesUsingEncoding:(NSStringEncoding)enc

  /// Returns the number of bytes required to store the
  /// `String` in a given encoding.
  public func lengthOfBytes(using encoding: Encoding) -> Int {
    return _ns.lengthOfBytes(using: encoding.rawValue)
  }

  // - (NSRange)lineRangeForRange:(NSRange)aRange

  /// Returns the range of characters representing the line or lines
  /// containing a given range.
  public func lineRange(for aRange: Range<Index>) -> Range<Index> {
    return _range(_ns.lineRange(for: _toNSRange(aRange)))
  }

  // - (NSArray *)
  //     linguisticTagsInRange:(NSRange)range
  //     scheme:(NSString *)tagScheme
  //     options:(LinguisticTaggerOptions)opts
  //     orthography:(Orthography *)orthography
  //     tokenRanges:(NSArray**)tokenRanges

  /// Returns an array of linguistic tags for the specified
  /// range and requested tags within the receiving string.
  public func linguisticTags(
    in range: Range<Index>,
    scheme tagScheme: String,
    options opts: NSLinguisticTagger.Options = [],
    orthography: NSOrthography? = nil,
    tokenRanges: UnsafeMutablePointer<[Range<Index>]>? = nil // FIXME:Can this be nil?
  ) -> [String] {
    var nsTokenRanges: NSArray?
    let result = tokenRanges._withNilOrAddress(of: &nsTokenRanges) {
      self._ns.linguisticTags(
        in: _toNSRange(range), scheme: tagScheme, options: opts,
        orthography: orthography, tokenRanges: $0) as NSArray
    }

    if nsTokenRanges != nil {
      tokenRanges?.pointee = (nsTokenRanges! as [AnyObject]).map {
        self._range($0.rangeValue)
      }
    }

    return result as! [String]
  }

  // - (NSComparisonResult)localizedCaseInsensitiveCompare:(NSString *)aString

  /// Compares the string and a given string using a
  /// case-insensitive, localized, comparison.
  public
  func localizedCaseInsensitiveCompare(_ aString: String) -> ComparisonResult {
    return _ns.localizedCaseInsensitiveCompare(aString)
  }

  // - (NSComparisonResult)localizedCompare:(NSString *)aString

  /// Compares the string and a given string using a localized
  /// comparison.
  public func localizedCompare(_ aString: String) -> ComparisonResult {
    return _ns.localizedCompare(aString)
  }

  /// Compares strings as sorted by the Finder.
  public func localizedStandardCompare(_ string: String) -> ComparisonResult {
    return _ns.localizedStandardCompare(string)
  }

  //===--- Omitted for consistency with API review results 5/20/2014 ------===//
  // @property long long longLongValue

  // @property (readonly, copy) NSString *localizedLowercase NS_AVAILABLE(10_11, 9_0);

  /// A lowercase version of the string that is produced using the current
  /// locale.
  @available(OSX 10.11, iOS 9.0, *)
  public var localizedLowercase: String {
    return _ns.localizedLowercase
  }

  // - (NSString *)lowercaseStringWithLocale:(Locale *)locale

  /// Returns a version of the string with all letters
  /// converted to lowercase, taking into account the specified
  /// locale.
  public func lowercased(with locale: Locale?) -> String {
    return _ns.lowercased(with: locale)
  }

  // - (NSUInteger)maximumLengthOfBytesUsingEncoding:(NSStringEncoding)enc

  /// Returns the maximum number of bytes needed to store the
  /// `String` in a given encoding.
  public
  func maximumLengthOfBytes(using encoding: Encoding) -> Int {
    return _ns.maximumLengthOfBytes(using: encoding.rawValue)
  }

  // - (NSRange)paragraphRangeForRange:(NSRange)aRange

  /// Returns the range of characters representing the
  /// paragraph or paragraphs containing a given range.
  public func paragraphRange(for aRange: Range<Index>) -> Range<Index> {
    return _range(_ns.paragraphRange(for: _toNSRange(aRange)))
  }

  // @property NSArray* pathComponents

  /// Returns an array of NSString objects containing, in
  /// order, each path component of the `String`.
  @available(*, unavailable, message: "Use pathComponents on URL instead.")
  public var pathComponents: [String] {
    return _ns.pathComponents
  }

  // @property NSString* pathExtension;

  /// Interprets the `String` as a path and returns the
  /// `String`'s extension, if any.
  @available(*, unavailable, message: "Use pathExtension on URL instead.")
  public var pathExtension: String {
    return _ns.pathExtension
  }

  // @property NSString* precomposedStringWithCanonicalMapping;

  /// Returns a string made by normalizing the `String`'s
  /// contents using Form C.
  public var precomposedStringWithCanonicalMapping: String {
    return _ns.precomposedStringWithCanonicalMapping
  }

  // @property NSString * precomposedStringWithCompatibilityMapping;

  /// Returns a string made by normalizing the `String`'s
  /// contents using Form KC.
  public var precomposedStringWithCompatibilityMapping: String {
    return _ns.precomposedStringWithCompatibilityMapping
  }

  // - (id)propertyList

  /// Parses the `String` as a text representation of a
  /// property list, returning an NSString, NSData, NSArray, or
  /// NSDictionary object, according to the topmost element.
  public func propertyList() -> Any {
    return _ns.propertyList()
  }

  // - (NSDictionary *)propertyListFromStringsFileFormat

  /// Returns a dictionary object initialized with the keys and
  /// values found in the `String`.
  public
  func propertyListFromStringsFileFormat() -> [String : String] {
    return _ns.propertyListFromStringsFileFormat() as! [String : String]? ?? [:]
  }

  // - (NSRange)rangeOfCharacterFromSet:(NSCharacterSet *)aSet
  //
  // - (NSRange)
  //     rangeOfCharacterFromSet:(NSCharacterSet *)aSet
  //     options:(StringCompareOptions)mask
  //
  // - (NSRange)
  //     rangeOfCharacterFromSet:(NSCharacterSet *)aSet
  //     options:(StringCompareOptions)mask
  //     range:(NSRange)aRange

  /// Finds and returns the range in the `String` of the first
  /// character from a given character set found in a given range with
  /// given options.
  public func rangeOfCharacter(
    from aSet: CharacterSet,
    options mask: CompareOptions = [],
    range aRange: Range<Index>? = nil
  ) -> Range<Index>? {
    return _optionalRange(
      _ns.rangeOfCharacter(
        from: aSet,
        options: mask,
        range: _toNSRange(
          aRange ?? self.characters.startIndex..<self.characters.endIndex
        )
      )
    )
  }

  // - (NSRange)rangeOfComposedCharacterSequenceAtIndex:(NSUInteger)anIndex

  /// Returns the range in the `String` of the composed
  /// character sequence located at a given index.
  public
  func rangeOfComposedCharacterSequence(at anIndex: Index) -> Range<Index> {
    return _range(
      _ns.rangeOfComposedCharacterSequence(at: anIndex._utf16Index))
  }

  // - (NSRange)rangeOfComposedCharacterSequencesForRange:(NSRange)range

  /// Returns the range in the string of the composed character
  /// sequences for a given range.
  public func rangeOfComposedCharacterSequences(
    for range: Range<Index>
  ) -> Range<Index> {
    // Theoretically, this will be the identity function.  In practice
    // I think users will be able to observe differences in the input
    // and output ranges due (if nothing else) to locale changes
    return _range(
      _ns.rangeOfComposedCharacterSequences(for: _toNSRange(range)))
  }

  // - (NSRange)rangeOfString:(NSString *)aString
  //
  // - (NSRange)
  //     rangeOfString:(NSString *)aString options:(StringCompareOptions)mask
  //
  // - (NSRange)
  //     rangeOfString:(NSString *)aString
  //     options:(StringCompareOptions)mask
  //     range:(NSRange)aRange
  //
  // - (NSRange)
  //     rangeOfString:(NSString *)aString
  //     options:(StringCompareOptions)mask
  //     range:(NSRange)searchRange
  //     locale:(Locale *)locale

  /// Finds and returns the range of the first occurrence of a
  /// given string within a given range of the `String`, subject to
  /// given options, using the specified locale, if any.
  public func range(
    of aString: String,
    options mask: CompareOptions = [],
    range searchRange: Range<Index>? = nil,
    locale: Locale? = nil
  ) -> Range<Index>? {
    return _optionalRange(
      locale != nil ? _ns.range(
        of: aString,
        options: mask,
        range: _toNSRange(
          searchRange ?? self.characters.startIndex..<self.characters.endIndex
        ),
        locale: locale
      )
      : searchRange != nil ? _ns.range(
        of: aString, options: mask, range: _toNSRange(searchRange!)
      )
      : !mask.isEmpty ? _ns.range(of: aString, options: mask)
      : _ns.range(of: aString)
    )
  }

  // - (BOOL)localizedStandardContainsString:(NSString *)str NS_AVAILABLE(10_11, 9_0);

  /// Returns `true` if `self` contains `string`, taking the current locale
  /// into account.
  ///
  /// This is the most appropriate method for doing user-level string searches,
  /// similar to how searches are done generally in the system.  The search is
  /// locale-aware, case and diacritic insensitive.  The exact list of search
  /// options applied may change over time.
  @available(OSX 10.11, iOS 9.0, *)
  public func localizedStandardContains(_ string: String) -> Bool {
    return _ns.localizedStandardContains(string)
  }

  // - (NSRange)localizedStandardRangeOfString:(NSString *)str NS_AVAILABLE(10_11, 9_0);

  /// Finds and returns the range of the first occurrence of a given string,
  /// taking the current locale into account.  Returns `nil` if the string was
  /// not found.
  ///
  /// This is the most appropriate method for doing user-level string searches,
  /// similar to how searches are done generally in the system.  The search is
  /// locale-aware, case and diacritic insensitive.  The exact list of search
  /// options applied may change over time.
  @available(OSX 10.11, iOS 9.0, *)
  public func localizedStandardRange(of string: String) -> Range<Index>? {
    return _optionalRange(_ns.localizedStandardRange(of: string))
  }

  // @property NSStringEncoding smallestEncoding;

  /// Returns the smallest encoding to which the `String` can
  /// be converted without loss of information.
  public var smallestEncoding: Encoding {
    return Encoding(rawValue: _ns.smallestEncoding)
  }

  // @property NSString *stringByAbbreviatingWithTildeInPath;

  /// Returns a new string that replaces the current home
  /// directory portion of the current path with a tilde (`~`)
  /// character.
  @available(*, unavailable, message: "Use abbreviatingWithTildeInPath on NSString instead.")
  public var abbreviatingWithTildeInPath: String {
    return _ns.abbreviatingWithTildeInPath
  }

  // - (NSString *)
  //     stringByAddingPercentEncodingWithAllowedCharacters:
  //       (NSCharacterSet *)allowedCharacters

  /// Returns a new string made from the `String` by replacing
  /// all characters not in the specified set with percent encoded
  /// characters.
  public func addingPercentEncoding(
    withAllowedCharacters allowedCharacters: CharacterSet
  ) -> String? {
    // FIXME: the documentation states that this method can return nil if the
    // transformation is not possible, without going into further details.  The
    // implementation can only return nil if malloc() returns nil, so in
    // practice this is not possible.  Still, to be consistent with
    // documentation, we declare the method as returning an optional String.
    //
    // <rdar://problem/17901698> Docs for -[NSString
    // stringByAddingPercentEncodingWithAllowedCharacters] don't precisely
    // describe when return value is nil
    return _ns.addingPercentEncoding(withAllowedCharacters:
      allowedCharacters
    )
  }

  // - (NSString *)
  //     stringByAddingPercentEscapesUsingEncoding:(NSStringEncoding)encoding

  /// Returns a representation of the `String` using a given
  /// encoding to determine the percent escapes necessary to convert
  /// the `String` into a legal URL string.
  @available(*, deprecated, message: "Use addingPercentEncoding(withAllowedCharacters:) instead, which always uses the recommended UTF-8 encoding, and which encodes for a specific URL component or subcomponent since each URL component or subcomponent has different rules for what characters are valid.")
  public func addingPercentEscapes(
    using encoding: Encoding
  ) -> String? {
    return _ns.addingPercentEscapes(using: encoding.rawValue)
  }

  // - (NSString *)stringByAppendingFormat:(NSString *)format, ...

  /// Returns a string made by appending to the `String` a
  /// string constructed from a given format string and the following
  /// arguments.
  public func appendingFormat(
    _ format: String, _ arguments: CVarArg...
  ) -> String {
    return _ns.appending(
      String(format: format, arguments: arguments))
  }

  // - (NSString *)stringByAppendingPathComponent:(NSString *)aString

  /// Returns a new string made by appending to the `String` a given string.
  @available(*, unavailable, message: "Use appendingPathComponent on URL instead.")
  public func appendingPathComponent(_ aString: String) -> String {
    return _ns.appendingPathComponent(aString)
  }

  // - (NSString *)stringByAppendingPathExtension:(NSString *)ext

  /// Returns a new string made by appending to the `String` an
  /// extension separator followed by a given extension.
  @available(*, unavailable, message: "Use appendingPathExtension on URL instead.")
  public func appendingPathExtension(_ ext: String) -> String? {
    // FIXME: This method can return nil in practice, for example when self is
    // an empty string.  OTOH, this is not documented, documentation says that
    // it always returns a string.
    //
    // <rdar://problem/17902469> -[NSString stringByAppendingPathExtension] can
    // return nil
    return _ns.appendingPathExtension(ext)
  }

  // - (NSString *)stringByAppendingString:(NSString *)aString

  /// Returns a new string made by appending a given string to
  /// the `String`.
  public func appending(_ aString: String) -> String {
    return _ns.appending(aString)
  }

  // @property NSString* stringByDeletingLastPathComponent;

  /// Returns a new string made by deleting the last path
  /// component from the `String`, along with any final path
  /// separator.
  @available(*, unavailable, message: "Use deletingLastPathComponent on URL instead.")
  public var deletingLastPathComponent: String {
    return _ns.deletingLastPathComponent
  }

  // @property NSString* stringByDeletingPathExtension;

  /// Returns a new string made by deleting the extension (if
  /// any, and only the last) from the `String`.
  @available(*, unavailable, message: "Use deletingPathExtension on URL instead.")
  public var deletingPathExtension: String {
    return _ns.deletingPathExtension
  }

  // @property NSString* stringByExpandingTildeInPath;

  /// Returns a new string made by expanding the initial
  /// component of the `String` to its full path value.
  @available(*, unavailable, message: "Use expandingTildeInPath on NSString instead.")
  public var expandingTildeInPath: String {
    return _ns.expandingTildeInPath
  }

  // - (NSString *)
  //     stringByFoldingWithOptions:(StringCompareOptions)options
  //     locale:(Locale *)locale

  @available(*, unavailable, renamed: "folding(options:locale:)")
  public func folding(
    _ options: CompareOptions = [], locale: Locale?
  ) -> String {
    return folding(options: options, locale: locale)
  }

  /// Returns a string with the given character folding options
  /// applied.
  public func folding(
    options: CompareOptions = [], locale: Locale?
  ) -> String {
    return _ns.folding(options: options, locale: locale)
  }

  // - (NSString *)stringByPaddingToLength:(NSUInteger)newLength
  //     withString:(NSString *)padString
  //     startingAtIndex:(NSUInteger)padIndex

  /// Returns a new string formed from the `String` by either
  /// removing characters from the end, or by appending as many
  /// occurrences as necessary of a given pad string.
  public func padding(
    toLength newLength: Int,
    withPad padString: String,
    startingAt padIndex: Int
  ) -> String {
    return _ns.padding(
      toLength: newLength, withPad: padString, startingAt: padIndex)
  }

  // @property NSString* stringByRemovingPercentEncoding;

  /// Returns a new string made from the `String` by replacing
  /// all percent encoded sequences with the matching UTF-8
  /// characters.
  public var removingPercentEncoding: String? {
    return _ns.removingPercentEncoding
  }

  // - (NSString *)
  //     stringByReplacingCharactersInRange:(NSRange)range
  //     withString:(NSString *)replacement

  /// Returns a new string in which the characters in a
  /// specified range of the `String` are replaced by a given string.
  public func replacingCharacters(
    in range: Range<Index>, with replacement: String
  ) -> String {
    return _ns.replacingCharacters(in: _toNSRange(range), with: replacement)
  }

  // - (NSString *)
  //     stringByReplacingOccurrencesOfString:(NSString *)target
  //     withString:(NSString *)replacement
  //
  // - (NSString *)
  //     stringByReplacingOccurrencesOfString:(NSString *)target
  //     withString:(NSString *)replacement
  //     options:(StringCompareOptions)options
  //     range:(NSRange)searchRange

  /// Returns a new string in which all occurrences of a target
  /// string in a specified range of the `String` are replaced by
  /// another given string.
  public func replacingOccurrences(
    of target: String,
    with replacement: String,
    options: CompareOptions = [],
    range searchRange: Range<Index>? = nil
  ) -> String {
    return (searchRange != nil) || (!options.isEmpty)
    ? _ns.replacingOccurrences(
      of: target,
      with: replacement,
      options: options,
      range: _toNSRange(
        searchRange ?? self.characters.startIndex..<self.characters.endIndex
      )
    )
    : _ns.replacingOccurrences(of: target, with: replacement)
  }

  // - (NSString *)
  //     stringByReplacingPercentEscapesUsingEncoding:(NSStringEncoding)encoding

  /// Returns a new string made by replacing in the `String`
  /// all percent escapes with the matching characters as determined
  /// by a given encoding.
  @available(*, deprecated, message: "Use removingPercentEncoding instead, which always uses the recommended UTF-8 encoding.")
  public func replacingPercentEscapes(
    using encoding: Encoding
  ) -> String? {
    return _ns.replacingPercentEscapes(using: encoding.rawValue)
  }

  // @property NSString* stringByResolvingSymlinksInPath;

  /// Returns a new string made from the `String` by resolving
  /// all symbolic links and standardizing path.
  @available(*, unavailable, message: "Use resolvingSymlinksInPath on URL instead.")
  public var resolvingSymlinksInPath: String {
    return _ns.resolvingSymlinksInPath
  }

  // @property NSString* stringByStandardizingPath;

  /// Returns a new string made by removing extraneous path
  /// components from the `String`.
  @available(*, unavailable, message: "Use standardizingPath on URL instead.")
  public var standardizingPath: String {
    return _ns.standardizingPath
  }

  // - (NSString *)stringByTrimmingCharactersInSet:(NSCharacterSet *)set

  /// Returns a new string made by removing from both ends of
  /// the `String` characters contained in a given character set.
  public func trimmingCharacters(in set: CharacterSet) -> String {
    return _ns.trimmingCharacters(in: set)
  }

  // - (NSArray *)stringsByAppendingPaths:(NSArray *)paths

  /// Returns an array of strings made by separately appending
  /// to the `String` each string in a given array.
  @available(*, unavailable, message: "Map over paths with appendingPathComponent instead.")
  public func strings(byAppendingPaths paths: [String]) -> [String] {
    fatalError("This function is not available")
  }

  // - (NSString *)substringFromIndex:(NSUInteger)anIndex

  /// Returns a new string containing the characters of the
  /// `String` from the one at a given index to the end.
  public func substring(from index: Index) -> String {
    return _ns.substring(from: index._utf16Index)
  }

  // - (NSString *)substringToIndex:(NSUInteger)anIndex

  /// Returns a new string containing the characters of the
  /// `String` up to, but not including, the one at a given index.
  public func substring(to index: Index) -> String {
    return _ns.substring(to: index._utf16Index)
  }

  // - (NSString *)substringWithRange:(NSRange)aRange

  /// Returns a string object containing the characters of the
  /// `String` that lie within a given range.
  public func substring(with aRange: Range<Index>) -> String {
    return _ns.substring(with: _toNSRange(aRange))
  }

  // @property (readonly, copy) NSString *localizedUppercaseString NS_AVAILABLE(10_11, 9_0);

  /// An uppercase version of the string that is produced using the current
  /// locale.
  @available(OSX 10.11, iOS 9.0, *)
  public var localizedUppercase: String {
    return _ns.localizedUppercase as String
  }

  // - (NSString *)uppercaseStringWithLocale:(Locale *)locale

  /// Returns a version of the string with all letters
  /// converted to uppercase, taking into account the specified
  /// locale.
  public func uppercased(with locale: Locale?) -> String {
    return _ns.uppercased(with: locale)
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
  public func write(
    toFile path: String, atomically useAuxiliaryFile:Bool,
    encoding enc: Encoding
  ) throws {
    try self._ns.write(
      toFile: path, atomically: useAuxiliaryFile, encoding: enc.rawValue)
  }

  // - (BOOL)
  //     writeToURL:(NSURL *)url
  //     atomically:(BOOL)useAuxiliaryFile
  //     encoding:(NSStringEncoding)enc
  //     error:(NSError **)error

  /// Writes the contents of the `String` to the URL specified
  /// by url using the specified encoding.
  public func write(
    to url: URL, atomically useAuxiliaryFile: Bool,
    encoding enc: Encoding
  ) throws {
    try self._ns.write(
      to: url, atomically: useAuxiliaryFile, encoding: enc.rawValue)
  }

  // - (nullable NSString *)stringByApplyingTransform:(NSString *)transform reverse:(BOOL)reverse NS_AVAILABLE(10_11, 9_0);

  /// Perform string transliteration.
  @available(OSX 10.11, iOS 9.0, *)
  public func applyingTransform(
    _ transform: StringTransform, reverse: Bool
  ) -> String? {
    return _ns.applyingTransform(transform, reverse: reverse)
  }

  //===--- From the 10.10 release notes; not in public documentation ------===//
  // No need to make these unavailable on earlier OSes, since they can
  // forward trivially to rangeOfString.

  /// Returns `true` iff `other` is non-empty and contained within
  /// `self` by case-sensitive, non-literal search.
  ///
  /// Equivalent to `self.rangeOfString(other) != nil`
  public func contains(_ other: String) -> Bool {
    let r = self.range(of: other) != nil
    if #available(OSX 10.10, iOS 8.0, *) {
      _sanityCheck(r == _ns.contains(other))
    }
    return r
  }
  
  /// Returns `true` iff `other` is non-empty and contained within
  /// `self` by case-insensitive, non-literal search, taking into
  /// account the current locale.
  ///
  /// Locale-independent case-insensitive operation, and other needs,
  /// can be achieved by calling
  /// `rangeOfString(_:options:_, range:_locale:_)`.
  ///
  /// Equivalent to
  ///
  ///     self.rangeOf(
  ///       other, options: .CaseInsensitiveSearch,
  ///       locale: Locale.current) != nil
  public func localizedCaseInsensitiveContains(_ other: String) -> Bool {
    let r = self.range(
      of: other, options: .caseInsensitive, locale: Locale.current
    ) != nil
    if #available(OSX 10.10, iOS 8.0, *) {
      _sanityCheck(r == _ns.localizedCaseInsensitiveContains(other))
    }
    return r
  }
}

// Pre-Swift-3 method names
extension String {

  @available(*, unavailable, renamed: "localizedName(of:)")
  public static func localizedNameOfStringEncoding(
    _ encoding: Encoding
  ) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, message: "Use fileURL(withPathComponents:) on URL instead.")
  public static func pathWithComponents(_ components: [String]) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "canBeConverted(to:)")
  public func canBeConvertedToEncoding(_ encoding: Encoding) -> Bool {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "capitalizedString(with:)")
  public func capitalizedStringWith(_ locale: Locale?) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "commonPrefix(with:options:)")
  public func commonPrefixWith(
    _ aString: String, options: CompareOptions) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "completePath(into:outputName:caseSensitive:matchesInto:filterTypes:)")
  public func completePathInto(
    _ outputName: UnsafeMutablePointer<String>? = nil,
    caseSensitive: Bool,
    matchesInto matchesIntoArray: UnsafeMutablePointer<[String]>? = nil,
    filterTypes: [String]? = nil
  ) -> Int {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "components(separatedBy:)")
  public func componentsSeparatedByCharactersIn(
    _ separator: CharacterSet
  ) -> [String] {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "componentsSeparated(by:)")
  public func componentsSeparatedBy(_ separator: String) -> [String] {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "cString(usingEncoding:)")
  public func cStringUsingEncoding(_ encoding: Encoding) -> [CChar]? {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "data(usingEncoding:allowLossyConversion:)")
  public func dataUsingEncoding(
    _ encoding: Encoding,
    allowLossyConversion: Bool = false
  ) -> Data? {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "enumerateLinguisticTags(in:scheme:options:orthography:_:)")
  public func enumerateLinguisticTagsIn(
    _ range: Range<Index>,
    scheme tagScheme: String,
    options opts: NSLinguisticTagger.Options,
    orthography: NSOrthography?,
    _ body:
      (String, Range<Index>, Range<Index>, inout Bool) -> Void
  ) {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "enumerateSubstrings(in:options:_:)")
  public func enumerateSubstringsIn(
    _ range: Range<Index>,
    options opts: EnumerationOptions = [],
    _ body: (
      _ substring: String?, _ substringRange: Range<Index>,
      _ enclosingRange: Range<Index>, inout Bool
    ) -> Void
  ) {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "getBytes(_:maxLength:usedLength:encoding:options:range:remaining:)")
  public func getBytes(
    _ buffer: inout [UInt8],
    maxLength maxBufferCount: Int,
    usedLength usedBufferCount: UnsafeMutablePointer<Int>,
    encoding: Encoding,
    options: EncodingConversionOptions = [],
    range: Range<Index>,
    remainingRange leftover: UnsafeMutablePointer<Range<Index>>
  ) -> Bool {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "getLineStart(_:end:contentsEnd:for:)")
  public func getLineStart(
    _ start: UnsafeMutablePointer<Index>,
    end: UnsafeMutablePointer<Index>,
    contentsEnd: UnsafeMutablePointer<Index>,
    forRange: Range<Index>
  ) {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "getParagraphStart(_:end:contentsEnd:for:)")
  public func getParagraphStart(
    _ start: UnsafeMutablePointer<Index>,
    end: UnsafeMutablePointer<Index>,
    contentsEnd: UnsafeMutablePointer<Index>,
    forRange: Range<Index>
  ) {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "lengthOfBytes(using:)")
  public func lengthOfBytesUsingEncoding(_ encoding: Encoding) -> Int {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "lineRange(for:)")
  public func lineRangeFor(_ aRange: Range<Index>) -> Range<Index> {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "linguisticTags(in:scheme:options:orthography:tokenRanges:)")
  public func linguisticTagsIn(
    _ range: Range<Index>,
    scheme tagScheme: String,
    options opts: NSLinguisticTagger.Options = [],
    orthography: NSOrthography? = nil,
    tokenRanges: UnsafeMutablePointer<[Range<Index>]>? = nil
  ) -> [String] {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "lowercased(with:)")
  public func lowercaseStringWith(_ locale: Locale?) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "maximumLengthOfBytes(using:)")
  public
  func maximumLengthOfBytesUsingEncoding(_ encoding: Encoding) -> Int {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "paragraphRange(for:)")
  public func paragraphRangeFor(_ aRange: Range<Index>) -> Range<Index> {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "rangeOfCharacter(from:options:range:)")
  public func rangeOfCharacterFrom(
    _ aSet: CharacterSet,
    options mask: CompareOptions = [],
    range aRange: Range<Index>? = nil
  ) -> Range<Index>? {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "rangeOfComposedCharacterSequence(at:)")
  public
  func rangeOfComposedCharacterSequenceAt(_ anIndex: Index) -> Range<Index> {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "rangeOfComposedCharacterSequences(for:)")
  public func rangeOfComposedCharacterSequencesFor(
    _ range: Range<Index>
  ) -> Range<Index> {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "range(of:options:range:locale:)")
  public func rangeOf(
    _ aString: String,
    options mask: CompareOptions = [],
    range searchRange: Range<Index>? = nil,
    locale: Locale? = nil
  ) -> Range<Index>? {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "localizedStandardRange(of:)")
  public func localizedStandardRangeOf(_ string: String) -> Range<Index>? {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "addingPercentEncoding(withAllowedCharacters:)")
  public func addingPercentEncodingWithAllowedCharacters(
    _ allowedCharacters: CharacterSet
  ) -> String? {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "addingPercentEscapes(using:)")
  public func addingPercentEscapesUsingEncoding(
    _ encoding: Encoding
  ) -> String? {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "appendingFormat")
  public func stringByAppendingFormat(
    _ format: String, _ arguments: CVarArg...
  ) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "padding(toLength:with:startingAt:)")
  public func byPaddingToLength(
    _ newLength: Int, withString padString: String, startingAt padIndex: Int
  ) -> String {
    fatalError("unavailable function can't be called")
  }
  
  @available(*, unavailable, renamed: "replacingCharacters(in:with:)")
  public func replacingCharactersIn(
    _ range: Range<Index>, withString replacement: String
  ) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "replacingOccurrences(of:with:options:range:)")
  public func replacingOccurrencesOf(
    _ target: String,
    withString replacement: String,
    options: CompareOptions = [],
    range searchRange: Range<Index>? = nil
  ) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "replacingPercentEscapes(usingEncoding:)")
  public func replacingPercentEscapesUsingEncoding(
    _ encoding: Encoding
  ) -> String? {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "trimmingCharacters(in:)")
  public func byTrimmingCharactersIn(_ set: CharacterSet) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "strings(byAppendingPaths:)")
  public func stringsByAppendingPaths(_ paths: [String]) -> [String] {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "substring(from:)")
  public func substringFrom(_ index: Index) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "substring(to:)")
  public func substringTo(_ index: Index) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "substring(with:)")
  public func substringWith(_ aRange: Range<Index>) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "uppercased(with:)")
  public func uppercaseStringWith(_ locale: Locale?) -> String {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "write(toFile:atomically:encoding:)")
  public func writeToFile(
    _ path: String, atomically useAuxiliaryFile:Bool,
    encoding enc: Encoding
  ) throws {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed: "write(to:atomically:encoding:)")
  public func writeToURL(
    _ url: URL, atomically useAuxiliaryFile: Bool,
    encoding enc: Encoding
  ) throws {
    fatalError("unavailable function can't be called")
  }
}
