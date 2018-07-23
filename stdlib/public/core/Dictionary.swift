//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

// Implementation notes
// ====================
//
// `Dictionary` uses two storage schemes: native storage and Cocoa storage.
//
// Native storage is a hash table with open addressing and linear probing. The
// bucket array forms a logical ring (e.g., a chain can wrap around the end of
// buckets array to the beginning of it).
//
// The logical bucket array is implemented as three arrays: Key, Value, and a
// bitmap that marks valid entries. An invalid entry marks the end of a chain.
// There is always at least one invalid entry among the buckets. `Dictionary`
// does not use tombstones.
//
// In addition to the native storage, `Dictionary` can also wrap an
// `NSDictionary` in order to allow bridging `NSDictionary` to `Dictionary` in
// `O(1)`.
//
// Currently native storage uses a data structure like this::
//
//   Dictionary<K,V> (a struct)
//   +------------------------------------------------+
//   |  _VariantDictionaryBuffer<K,V> (an enum)      |
//   | +--------------------------------------------+ |
//   | | [_NativeDictionaryBuffer<K,V> (a struct)] | |
//   | +---|----------------------------------------+ |
//   +----/-------------------------------------------+
//       /
//      |
//      V  _RawNativeDictionaryStorage (a class)
//   +-----------------------------------------------------------+
//   | bucketCount                                               |
//   | count                                                     |
//   | ptrToBits                                                 |
//   | ptrToKeys                                                 |
//   | ptrToValues                                               |
//   | [inline array of bits indicating whether bucket is set ]  |
//   | [inline array of keys                                  ]  |
//   | [inline array of values                                ]  |
//   +-----------------------------------------------------------+
//
//
// Cocoa storage uses a data structure like this::
//
//   Dictionary<K,V> (a struct)
//   +----------------------------------------------+
//   | _VariantDictionaryBuffer<K,V> (an enum)     |
//   | +----------------------------------------+   |
//   | | [ _CocoaDictionaryBuffer (a struct) ] |   |
//   | +---|------------------------------------+   |
//   +-----|----------------------------------------+
//         |
//     +---+
//     |
//     V  NSDictionary (a class)
//   +--------------+
//   | [refcount#1] |
//   +--------------+
//     ^
//     +-+
//       |     Dictionary<K,V>.Index (an enum)
//   +---|-----------------------------------+
//   |   |  _CocoaDictionaryIndex (a struct) |
//   | +-|-----------------------------+     |
//   | | * [ all keys ] [ next index ] |     |
//   | +-------------------------------+     |
//   +---------------------------------------+
//
//
// The Native Kinds of Storage
// ---------------------------
//
// There are three different classes that can provide a native backing storage:
// * `_RawNativeDictionaryStorage`
// * `_TypedNativeDictionaryStorage<K, V>`                   (extends Raw)
// * `_HashableTypedNativeDictionaryStorage<K: Hashable, V>` (extends Typed)
//
// (Hereafter RawStorage, TypedStorage, and HashableStorage, respectively)
//
// In a less optimized implementation, the parent classes could
// be eliminated, as they exist only to provide special-case behaviors.
// HashableStorage has everything a full implementation of a Dictionary
// requires, and is subsequently able to provide a full NSDictionary
// implementation. Note that HashableStorage must have the `K: Hashable`
// constraint because the NSDictionary implementation can't be provided in a
// constrained extension.
//
// In normal usage, you can expect the backing storage of a Dictionary to be a
// NativeStorage.
//
// TypedStorage is distinguished from HashableStorage to allow us to create a
// `_NativeDictionaryBuffer<AnyObject, AnyObject>`. Without the Hashable
// requirement, such a Buffer is restricted to operations which can be performed
// with only the structure of the Storage: indexing and iteration. This is used
// in _SwiftDeferredNSDictionary to construct twin "native" and "bridged"
// storage. Key-based lookups are performed on the native storage, with the
// resultant index then used on the bridged storage.
//
// The only thing that TypedStorage adds over RawStorage is an implementation of
// deinit, to clean up the AnyObjects it stores. Although it nominally
// inherits an NSDictionary implementation from RawStorage, this implementation
// isn't useful and is never used.
//
// RawStorage exists to allow a type-punned empty singleton Storage to be
// created. Any time an empty Dictionary is created, this Storage is used. If
// this type didn't exist, then NativeBuffer would have to store a Storage that
// declared its actual type parameters. Similarly, the empty singleton would
// have to declare its actual type parameters. If the singleton was, for
// instance, a `HashableStorage<(), ()>`, then it would be a violation of
// Swift's strict aliasing rules to pass it where a `HashableStorage<Int, Int>`
// was expected.
//
// It's therefore necessary for several types to store a RawStorage, rather than
// a TypedStorage, to allow for the possibility of the empty singleton.
// RawStorage also provides an implementation of an always-empty NSDictionary.
//
//
// Index Invalidation
// ------------------
//
// FIXME: decide if this guarantee is worth making, as it restricts
// collision resolution to first-come-first-serve. The most obvious alternative
// would be robin hood hashing. The Rust code base is the best
// resource on a *practical* implementation of robin hood hashing I know of:
// https://github.com/rust-lang/rust/blob/ac919fcd9d4a958baf99b2f2ed5c3d38a2ebf9d0/src/libstd/collections/hash/map.rs#L70-L178
//
// Indexing a container, `c[i]`, uses the integral offset stored in the index
// to access the elements referenced by the container. Generally, an index into
// one container has no meaning for another. However copy-on-write currently
// preserves indices under insertion, as long as reallocation doesn't occur:
//
//   var (i, found) = d.find(k) // i is associated with d's storage
//   if found {
//      var e = d            // now d is sharing its data with e
//      e[newKey] = newValue // e now has a unique copy of the data
//      return e[i]          // use i to access e
//   }
//
// The result should be a set of iterator invalidation rules familiar to anyone
// familiar with the C++ standard library.  Note that because all accesses to a
// dictionary storage are bounds-checked, this scheme never compromises memory
// safety.
//
//
// Bridging
// ========
//
// Bridging `NSDictionary` to `Dictionary`
// ---------------------------------------
//
// FIXME(eager-bridging): rewrite this based on modern constraints.
//
// `NSDictionary` bridges to `Dictionary<NSObject, AnyObject>` in `O(1)`,
// without memory allocation.
//
// Bridging `Dictionary` to `NSDictionary`
// ---------------------------------------
//
// `Dictionary<K, V>` bridges to `NSDictionary` in O(1)
// but may incur an allocation depending on the following conditions:
//
// * If the Dictionary is freshly allocated without any elements, then it
//   contains the empty singleton Storage which is returned as a toll-free
//   implementation of `NSDictionary`.
//
// * If both `K` and `V` are bridged verbatim, then `Dictionary<K, V>` is
//   still toll-free bridged to `NSDictionary` by returning its Storage.
//
// * If the Dictionary is actually a lazily bridged NSDictionary, then that
//   NSDictionary is returned.
//
// * Otherwise, bridging the `Dictionary` is done by wrapping its buffer in a
//   `_SwiftDeferredNSDictionary<K, V>`. This incurs an O(1)-sized allocation.
//
//   Complete bridging of the native Storage's elements to another Storage
//   is performed on first access. This is O(n) work, but is hopefully amortized
//   by future accesses.
//
//   This design ensures that:
//   - Every time keys or values are accessed on the bridged `NSDictionary`,
//     new objects are not created.
//   - Accessing the same element (key or value) multiple times will return
//     the same pointer.
//
// Bridging `NSSet` to `Set` and vice versa
// ----------------------------------------
//
// Bridging guarantees for `Set<Element>` are the same as for
// `Dictionary<Element, ()>`.
//

//===--- APIs unique to Dictionary<Key, Value> ----------------------------===//

/// A collection whose elements are key-value pairs.
///
/// A dictionary is a type of hash table, providing fast access to the entries
/// it contains. Each entry in the table is identified using its key, which is
/// a hashable type such as a string or number. You use that key to retrieve
/// the corresponding value, which can be any object. In other languages,
/// similar data types are known as hashes or associated arrays.
///
/// Create a new dictionary by using a dictionary literal. A dictionary literal
/// is a comma-separated list of key-value pairs, in which a colon separates
/// each key from its associated value, surrounded by square brackets. You can
/// assign a dictionary literal to a variable or constant or pass it to a
/// function that expects a dictionary.
///
/// Here's how you would create a dictionary of HTTP response codes and their
/// related messages:
///
///     var responseMessages = [200: "OK",
///                             403: "Access forbidden",
///                             404: "File not found",
///                             500: "Internal server error"]
///
/// The `responseMessages` variable is inferred to have type `[Int: String]`.
/// The `Key` type of the dictionary is `Int`, and the `Value` type of the
/// dictionary is `String`.
///
/// To create a dictionary with no key-value pairs, use an empty dictionary
/// literal (`[:]`).
///
///     var emptyDict: [String: String] = [:]
///
/// Any type that conforms to the `Hashable` protocol can be used as a
/// dictionary's `Key` type, including all of Swift's basic types. You can use
/// your own custom types as dictionary keys by making them conform to the
/// `Hashable` protocol.
///
/// Getting and Setting Dictionary Values
/// =====================================
///
/// The most common way to access values in a dictionary is to use a key as a
/// subscript. Subscripting with a key takes the following form:
///
///     print(responseMessages[200])
///     // Prints "Optional("OK")"
///
/// Subscripting a dictionary with a key returns an optional value, because a
/// dictionary might not hold a value for the key that you use in the
/// subscript.
///
/// The next example uses key-based subscripting of the `responseMessages`
/// dictionary with two keys that exist in the dictionary and one that does
/// not.
///
///     let httpResponseCodes = [200, 403, 301]
///     for code in httpResponseCodes {
///         if let message = responseMessages[code] {
///             print("Response \(code): \(message)")
///         } else {
///             print("Unknown response \(code)")
///         }
///     }
///     // Prints "Response 200: OK"
///     // Prints "Response 403: Access Forbidden"
///     // Prints "Unknown response 301"
///
/// You can also update, modify, or remove keys and values from a dictionary
/// using the key-based subscript. To add a new key-value pair, assign a value
/// to a key that isn't yet a part of the dictionary.
///
///     responseMessages[301] = "Moved permanently"
///     print(responseMessages[301])
///     // Prints "Optional("Moved permanently")"
///
/// Update an existing value by assigning a new value to a key that already
/// exists in the dictionary. If you assign `nil` to an existing key, the key
/// and its associated value are removed. The following example updates the
/// value for the `404` code to be simply "Not found" and removes the
/// key-value pair for the `500` code entirely.
///
///     responseMessages[404] = "Not found"
///     responseMessages[500] = nil
///     print(responseMessages)
///     // Prints "[301: "Moved permanently", 200: "OK", 403: "Access forbidden", 404: "Not found"]"
///
/// In a mutable `Dictionary` instance, you can modify in place a value that
/// you've accessed through a keyed subscript. The code sample below declares a
/// dictionary called `interestingNumbers` with string keys and values that
/// are integer arrays, then sorts each array in-place in descending order.
///
///     var interestingNumbers = ["primes": [2, 3, 5, 7, 11, 13, 17],
///                               "triangular": [1, 3, 6, 10, 15, 21, 28],
///                               "hexagonal": [1, 6, 15, 28, 45, 66, 91]]
///     for key in interestingNumbers.keys {
///         interestingNumbers[key]?.sort(by: >)
///     }
///
///     print(interestingNumbers["primes"]!)
///     // Prints "[17, 13, 11, 7, 5, 3, 2]"
///
/// Iterating Over the Contents of a Dictionary
/// ===========================================
///
/// Every dictionary is an unordered collection of key-value pairs. You can
/// iterate over a dictionary using a `for`-`in` loop, decomposing each
/// key-value pair into the elements of a tuple.
///
///     let imagePaths = ["star": "/glyphs/star.png",
///                       "portrait": "/images/content/portrait.jpg",
///                       "spacer": "/images/shared/spacer.gif"]
///
///     for (name, path) in imagePaths {
///         print("The path to '\(name)' is '\(path)'.")
///     }
///     // Prints "The path to 'star' is '/glyphs/star.png'."
///     // Prints "The path to 'portrait' is '/images/content/portrait.jpg'."
///     // Prints "The path to 'spacer' is '/images/shared/spacer.gif'."
///
/// The order of key-value pairs in a dictionary is stable between mutations
/// but is otherwise unpredictable. If you need an ordered collection of
/// key-value pairs and don't need the fast key lookup that `Dictionary`
/// provides, see the `DictionaryLiteral` type for an alternative.
///
/// You can search a dictionary's contents for a particular value using the
/// `contains(where:)` or `firstIndex(where:)` methods supplied by default
/// implementation. The following example checks to see if `imagePaths` contains
/// any paths in the `"/glyphs"` directory:
///
///     let glyphIndex = imagePaths.firstIndex(where: { $0.value.hasPrefix("/glyphs") })
///     if let index = glyphIndex {
///         print("The '\(imagesPaths[index].key)' image is a glyph.")
///     } else {
///         print("No glyphs found!")
///     }
///     // Prints "The 'star' image is a glyph.")
///
/// Note that in this example, `imagePaths` is subscripted using a dictionary
/// index. Unlike the key-based subscript, the index-based subscript returns
/// the corresponding key-value pair as a non-optional tuple.
///
///     print(imagePaths[glyphIndex!])
///     // Prints "("star", "/glyphs/star.png")"
///
/// A dictionary's indices stay valid across additions to the dictionary as
/// long as the dictionary has enough capacity to store the added values
/// without allocating more buffer. When a dictionary outgrows its buffer,
/// existing indices may be invalidated without any notification.
///
/// When you know how many new values you're adding to a dictionary, use the
/// `init(minimumCapacity:)` initializer to allocate the correct amount of
/// buffer.
///
/// Bridging Between Dictionary and NSDictionary
/// ============================================
///
/// You can bridge between `Dictionary` and `NSDictionary` using the `as`
/// operator. For bridging to be possible, the `Key` and `Value` types of a
/// dictionary must be classes, `@objc` protocols, or types that bridge to
/// Foundation types.
///
/// Bridging from `Dictionary` to `NSDictionary` always takes O(1) time and
/// space. When the dictionary's `Key` and `Value` types are neither classes
/// nor `@objc` protocols, any required bridging of elements occurs at the
/// first access of each element. For this reason, the first operation that
/// uses the contents of the dictionary may take O(*n*).
///
/// Bridging from `NSDictionary` to `Dictionary` first calls the `copy(with:)`
/// method (`- copyWithZone:` in Objective-C) on the dictionary to get an
/// immutable copy and then performs additional Swift bookkeeping work that
/// takes O(1) time. For instances of `NSDictionary` that are already
/// immutable, `copy(with:)` usually returns the same dictionary in O(1) time;
/// otherwise, the copying performance is unspecified. The instances of
/// `NSDictionary` and `Dictionary` share buffer using the same copy-on-write
/// optimization that is used when two instances of `Dictionary` share
/// buffer.
@_fixed_layout
public struct Dictionary<Key: Hashable, Value> {

  @usableFromInline
  internal typealias _Self = Dictionary<Key, Value>
  @usableFromInline
  internal typealias _VariantBuffer = _VariantDictionaryBuffer<Key, Value>
  @usableFromInline
  internal typealias _NativeBuffer = _NativeDictionaryBuffer<Key, Value>

  /// The element type of a dictionary: a tuple containing an individual
  /// key-value pair.
  public typealias Element = (key: Key, value: Value)

  @usableFromInline
  internal var _variantBuffer: _VariantBuffer

  /// Creates an empty dictionary.
  @inlinable // FIXME(sil-serialize-all)
  public init() {
    self = Dictionary<Key, Value>(_nativeBuffer: _NativeBuffer())
  }

  /// Creates an empty dictionary with preallocated space for at least the
  /// specified number of elements.
  ///
  /// Use this initializer to avoid intermediate reallocations of a dictionary's
  /// storage buffer when you know how many key-value pairs you are adding to a
  /// dictionary after creation.
  ///
  /// - Parameter minimumCapacity: The minimum number of key-value pairs that
  ///   the newly created dictionary should be able to store without
  ///   reallocating its storage buffer.
  @inlinable // FIXME(sil-serialize-all)
  public init(minimumCapacity: Int) {
    _variantBuffer = .native(_NativeBuffer(minimumCapacity: minimumCapacity))
  }

  /// Creates a new dictionary from the key-value pairs in the given sequence.
  ///
  /// You use this initializer to create a dictionary when you have a sequence
  /// of key-value tuples with unique keys. Passing a sequence with duplicate
  /// keys to this initializer results in a runtime error. If your
  /// sequence might have duplicate keys, use the
  /// `Dictionary(_:uniquingKeysWith:)` initializer instead.
  ///
  /// The following example creates a new dictionary using an array of strings
  /// as the keys and the integers in a countable range as the values:
  ///
  ///     let digitWords = ["one", "two", "three", "four", "five"]
  ///     let wordToValue = Dictionary(uniqueKeysWithValues: zip(digitWords, 1...5))
  ///     print(wordToValue["three"]!)
  ///     // Prints "3"
  ///     print(wordToValue)
  ///     // Prints "["three": 3, "four": 4, "five": 5, "one": 1, "two": 2]"
  ///
  /// - Parameter keysAndValues: A sequence of key-value pairs to use for
  ///   the new dictionary. Every key in `keysAndValues` must be unique.
  /// - Returns: A new dictionary initialized with the elements of
  ///   `keysAndValues`.
  /// - Precondition: The sequence must not have duplicate keys.
  @inlinable // FIXME(sil-serialize-all)
  public init<S: Sequence>(
    uniqueKeysWithValues keysAndValues: S
  ) where S.Element == (Key, Value) {
    if let d = keysAndValues as? Dictionary<Key, Value> {
      self = d
    } else {
      self = Dictionary(minimumCapacity: keysAndValues.underestimatedCount)
      // '_MergeError.keyCollision' is caught and handled with an appropriate
      // error message one level down, inside _variantBuffer.merge(_:...).
      try! _variantBuffer.merge(
        keysAndValues,
        uniquingKeysWith: { _, _ in throw _MergeError.keyCollision})
    }
  }

  /// Creates a new dictionary from the key-value pairs in the given sequence,
  /// using a combining closure to determine the value for any duplicate keys.
  ///
  /// You use this initializer to create a dictionary when you have a sequence
  /// of key-value tuples that might have duplicate keys. As the dictionary is
  /// built, the initializer calls the `combine` closure with the current and
  /// new values for any duplicate keys. Pass a closure as `combine` that
  /// returns the value to use in the resulting dictionary: The closure can
  /// choose between the two values, combine them to produce a new value, or
  /// even throw an error.
  ///
  /// The following example shows how to choose the first and last values for
  /// any duplicate keys:
  ///
  ///     let pairsWithDuplicateKeys = [("a", 1), ("b", 2), ("a", 3), ("b", 4)]
  ///
  ///     let firstValues = Dictionary(pairsWithDuplicateKeys,
  ///                                  uniquingKeysWith: { (first, _) in first })
  ///     // ["b": 2, "a": 1]
  ///
  ///     let lastValues = Dictionary(pairsWithDuplicateKeys,
  ///                                 uniquingKeysWith: { (_, last) in last })
  ///     // ["b": 4, "a": 3]
  ///
  /// - Parameters:
  ///   - keysAndValues: A sequence of key-value pairs to use for the new
  ///     dictionary.
  ///   - combine: A closure that is called with the values for any duplicate
  ///     keys that are encountered. The closure returns the desired value for
  ///     the final dictionary.
  @inlinable // FIXME(sil-serialize-all)
  public init<S: Sequence>(
    _ keysAndValues: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    self = Dictionary(minimumCapacity: keysAndValues.underestimatedCount)
    try _variantBuffer.merge(keysAndValues, uniquingKeysWith: combine)
  }

  /// Creates a new dictionary whose keys are the groupings returned by the
  /// given closure and whose values are arrays of the elements that returned
  /// each key.
  ///
  /// The arrays in the "values" position of the new dictionary each contain at
  /// least one element, with the elements in the same order as the source
  /// sequence.
  ///
  /// The following example declares an array of names, and then creates a
  /// dictionary from that array by grouping the names by first letter:
  ///
  ///     let students = ["Kofi", "Abena", "Efua", "Kweku", "Akosua"]
  ///     let studentsByLetter = Dictionary(grouping: students, by: { $0.first! })
  ///     // ["E": ["Efua"], "K": ["Kofi", "Kweku"], "A": ["Abena", "Akosua"]]
  ///
  /// The new `studentsByLetter` dictionary has three entries, with students'
  /// names grouped by the keys `"E"`, `"K"`, and `"A"`.
  ///
  /// - Parameters:
  ///   - values: A sequence of values to group into a dictionary.
  ///   - keyForValue: A closure that returns a key for each element in
  ///     `values`.
  @inlinable // FIXME(sil-serialize-all)
  public init<S: Sequence>(
    grouping values: S,
    by keyForValue: (S.Element) throws -> Key
  ) rethrows where Value == [S.Element] {
    self = [:]
    try _variantBuffer.nativeGroup(values, by: keyForValue)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(_nativeBuffer: _NativeDictionaryBuffer<Key, Value>) {
    _variantBuffer =
      .native(_nativeBuffer)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(_variantBuffer: _VariantBuffer) {
    self._variantBuffer = _variantBuffer
  }

#if _runtime(_ObjC)
  /// Private initializer used for bridging.
  ///
  /// Only use this initializer when both conditions are true:
  ///
  /// * it is statically known that the given `NSDictionary` is immutable;
  /// * `Key` and `Value` are bridged verbatim to Objective-C (i.e.,
  ///   are reference types).
  @inlinable // FIXME(sil-serialize-all)
  public init(_immutableCocoaDictionary: _NSDictionary) {
    _sanityCheck(
      _isBridgedVerbatimToObjectiveC(Key.self) &&
      _isBridgedVerbatimToObjectiveC(Value.self),
      "Dictionary can be backed by NSDictionary buffer only when both key and value are bridged verbatim to Objective-C")
    _variantBuffer = .cocoa(
      _CocoaDictionaryBuffer(cocoaDictionary: _immutableCocoaDictionary))
  }
#endif
}

//
// All APIs below should dispatch to `_variantBuffer`, without doing any
// additional processing.
//

extension Dictionary: Sequence {
  /// Returns an iterator over the dictionary's key-value pairs.
  ///
  /// Iterating over a dictionary yields the key-value pairs as two-element
  /// tuples. You can decompose the tuple in a `for`-`in` loop, which calls
  /// `makeIterator()` behind the scenes, or when calling the iterator's
  /// `next()` method directly.
  ///
  ///     let hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///     for (name, hueValue) in hues {
  ///         print("The hue of \(name) is \(hueValue).")
  ///     }
  ///     // Prints "The hue of Heliotrope is 296."
  ///     // Prints "The hue of Coral is 16."
  ///     // Prints "The hue of Aquamarine is 156."
  ///
  /// - Returns: An iterator over the dictionary with elements of type
  ///   `(key: Key, value: Value)`.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public func makeIterator() -> DictionaryIterator<Key, Value> {
    return _variantBuffer.makeIterator()
  }
}

// This is not quite Sequence.filter, because that returns [Element], not Self
extension Dictionary {
  /// Returns a new dictionary containing the key-value pairs of the dictionary
  /// that satisfy the given predicate.
  ///
  /// - Parameter isIncluded: A closure that takes a key-value pair as its
  ///   argument and returns a Boolean value indicating whether the pair
  ///   should be included in the returned dictionary.
  /// - Returns: A dictionary of the key-value pairs that `isIncluded` allows.
  @inlinable
  @available(swift, introduced: 4.0)
  public func filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> [Key: Value] {
    var result = Dictionary()
    for el in self {
      if try isIncluded(el) {
        result[el.key] = el.value
      }
    }
    return result
  }
}

extension Dictionary: Collection {
  /// The position of the first element in a nonempty dictionary.
  ///
  /// If the collection is empty, `startIndex` is equal to `endIndex`.
  ///
  /// - Complexity: Amortized O(1) if the dictionary does not wrap a bridged
  ///   `NSDictionary`. If the dictionary wraps a bridged `NSDictionary`, the
  ///   performance is unspecified.
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Index {
    return _variantBuffer.startIndex
  }

  /// The dictionary's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// If the collection is empty, `endIndex` is equal to `startIndex`.
  ///
  /// - Complexity: Amortized O(1) if the dictionary does not wrap a bridged
  ///   `NSDictionary`; otherwise, the performance is unspecified.
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Index {
    return _variantBuffer.endIndex
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(after i: Index) -> Index {
    return _variantBuffer.index(after: i)
  }

  /// Returns the index for the given key.
  ///
  /// If the given key is found in the dictionary, this method returns an index
  /// into the dictionary that corresponds with the key-value pair.
  ///
  ///     let countryCodes = ["BR": "Brazil", "GH": "Ghana", "JP": "Japan"]
  ///     let index = countryCodes.index(forKey: "JP")
  ///
  ///     print("Country code for \(countryCodes[index!].value): '\(countryCodes[index!].key)'.")
  ///     // Prints "Country code for Japan: 'JP'."
  ///
  /// - Parameter key: The key to find in the dictionary.
  /// - Returns: The index for `key` and its associated value if `key` is in
  ///   the dictionary; otherwise, `nil`.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public func index(forKey key: Key) -> Index? {
    // Complexity: amortized O(1) for native buffer, O(*n*) when wrapping an
    // NSDictionary.
    return _variantBuffer.index(forKey: key)
  }

  /// Accesses the key-value pair at the specified position.
  ///
  /// This subscript takes an index into the dictionary, instead of a key, and
  /// returns the corresponding key-value pair as a tuple. When performing
  /// collection-based operations that return an index into a dictionary, use
  /// this subscript with the resulting value.
  ///
  /// For example, to find the key for a particular value in a dictionary, use
  /// the `firstIndex(where:)` method.
  ///
  ///     let countryCodes = ["BR": "Brazil", "GH": "Ghana", "JP": "Japan"]
  ///     if let index = countryCodes.firstIndex(where: { $0.value == "Japan" }) {
  ///         print(countryCodes[index])
  ///         print("Japan's country code is '\(countryCodes[index].key)'.")
  ///     } else {
  ///         print("Didn't find 'Japan' as a value in the dictionary.")
  ///     }
  ///     // Prints "("JP", "Japan")"
  ///     // Prints "Japan's country code is 'JP'."
  ///
  /// - Parameter position: The position of the key-value pair to access.
  ///   `position` must be a valid index of the dictionary and not equal to
  ///   `endIndex`.
  /// - Returns: A two-element tuple with the key and value corresponding to
  ///   `position`.
  @inlinable // FIXME(sil-serialize-all)
  public subscript(position: Index) -> Element {
    return _variantBuffer.assertingGet(at: position)
  }

  /// The number of key-value pairs in the dictionary.
  ///
  /// - Complexity: O(1).
  @inlinable // FIXME(sil-serialize-all)
  public var count: Int {
    return _variantBuffer.count
  }

  //
  // `Sequence` conformance
  //

  /// A Boolean value that indicates whether the dictionary is empty.
  ///
  /// Dictionaries are empty when created with an initializer or an empty
  /// dictionary literal.
  ///
  ///     var frequencies: [String: Int] = [:]
  ///     print(frequencies.isEmpty)
  ///     // Prints "true"
  @inlinable // FIXME(sil-serialize-all)
  public var isEmpty: Bool {
    return count == 0
  }
}

extension Dictionary {
  /// Accesses the value associated with the given key for reading and writing.
  ///
  /// This *key-based* subscript returns the value for the given key if the key
  /// is found in the dictionary, or `nil` if the key is not found.
  ///
  /// The following example creates a new dictionary and prints the value of a
  /// key found in the dictionary (`"Coral"`) and a key not found in the
  /// dictionary (`"Cerise"`).
  ///
  ///     var hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///     print(hues["Coral"])
  ///     // Prints "Optional(16)"
  ///     print(hues["Cerise"])
  ///     // Prints "nil"
  ///
  /// When you assign a value for a key and that key already exists, the
  /// dictionary overwrites the existing value. If the dictionary doesn't
  /// contain the key, the key and value are added as a new key-value pair.
  ///
  /// Here, the value for the key `"Coral"` is updated from `16` to `18` and a
  /// new key-value pair is added for the key `"Cerise"`.
  ///
  ///     hues["Coral"] = 18
  ///     print(hues["Coral"])
  ///     // Prints "Optional(18)"
  ///
  ///     hues["Cerise"] = 330
  ///     print(hues["Cerise"])
  ///     // Prints "Optional(330)"
  ///
  /// If you assign `nil` as the value for the given key, the dictionary
  /// removes that key and its associated value.
  ///
  /// In the following example, the key-value pair for the key `"Aquamarine"`
  /// is removed from the dictionary by assigning `nil` to the key-based
  /// subscript.
  ///
  ///     hues["Aquamarine"] = nil
  ///     print(hues)
  ///     // Prints "["Coral": 18, "Heliotrope": 296, "Cerise": 330]"
  ///
  /// - Parameter key: The key to find in the dictionary.
  /// - Returns: The value associated with `key` if `key` is in the dictionary;
  ///   otherwise, `nil`.
  @inlinable // FIXME(sil-serialize-all)
  public subscript(key: Key) -> Value? {
    @inline(__always)
    get {
      return _variantBuffer.maybeGet(key)
    }
    set(newValue) {
      if let x = newValue {
        // FIXME(performance): this loads and discards the old value.
        _variantBuffer.updateValue(x, forKey: key)
      }
      else {
        // FIXME(performance): this loads and discards the old value.
        removeValue(forKey: key)
      }
    }
  }
}

extension Dictionary: ExpressibleByDictionaryLiteral {
  /// Creates a dictionary initialized with a dictionary literal.
  ///
  /// Do not call this initializer directly. It is called by the compiler to
  /// handle dictionary literals. To use a dictionary literal as the initial
  /// value of a dictionary, enclose a comma-separated list of key-value pairs
  /// in square brackets.
  ///
  /// For example, the code sample below creates a dictionary with string keys
  /// and values.
  ///
  ///     let countryCodes = ["BR": "Brazil", "GH": "Ghana", "JP": "Japan"]
  ///     print(countryCodes)
  ///     // Prints "["BR": "Brazil", "JP": "Japan", "GH": "Ghana"]"
  ///
  /// - Parameter elements: The key-value pairs that will make up the new
  ///   dictionary. Each key in `elements` must be unique.
  @inlinable // FIXME(sil-serialize-all)
  @_effects(readonly)
  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.init(_nativeBuffer: _NativeDictionaryBuffer.fromArray(elements))
  }
}

extension Dictionary {
  /// Accesses the value with the given key. If the dictionary doesn't contain
  /// the given key, accesses the provided default value as if the key and
  /// default value existed in the dictionary.
  ///
  /// Use this subscript when you want either the value for a particular key
  /// or, when that key is not present in the dictionary, a default value. This
  /// example uses the subscript with a message to use in case an HTTP response
  /// code isn't recognized:
  ///
  ///     var responseMessages = [200: "OK",
  ///                             403: "Access forbidden",
  ///                             404: "File not found",
  ///                             500: "Internal server error"]
  ///
  ///     let httpResponseCodes = [200, 403, 301]
  ///     for code in httpResponseCodes {
  ///         let message = responseMessages[code, default: "Unknown response"]
  ///         print("Response \(code): \(message)")
  ///     }
  ///     // Prints "Response 200: OK"
  ///     // Prints "Response 403: Access Forbidden"
  ///     // Prints "Response 301: Unknown response"
  ///
  /// When a dictionary's `Value` type has value semantics, you can use this
  /// subscript to perform in-place operations on values in the dictionary.
  /// The following example uses this subscript while counting the occurences
  /// of each letter in a string:
  ///
  ///     let message = "Hello, Elle!"
  ///     var letterCounts: [Character: Int] = [:]
  ///     for letter in message {
  ///         letterCounts[letter, defaultValue: 0] += 1
  ///     }
  ///     // letterCounts == ["H": 1, "e": 2, "l": 4, "o": 1, ...]
  ///
  /// When `letterCounts[letter, defaultValue: 0] += 1` is executed with a
  /// value of `letter` that isn't already a key in `letterCounts`, the
  /// specified default value (`0`) is returned from the subscript,
  /// incremented, and then added to the dictionary under that key.
  ///
  /// - Note: Do not use this subscript to modify dictionary values if the
  ///   dictionary's `Value` type is a class. In that case, the default value
  ///   and key are not written back to the dictionary after an operation.
  ///
  /// - Parameters:
  ///   - key: The key the look up in the dictionary.
  ///   - defaultValue: The default value to use if `key` doesn't exist in the
  ///     dictionary.
  /// - Returns: The value associated with `key` in the dictionary`; otherwise,
  ///   `defaultValue`.
  @inlinable // FIXME(sil-serialize-all)
  public subscript(
    key: Key, default defaultValue: @autoclosure () -> Value
  ) -> Value {
    @inline(__always)
    get {
      return _variantBuffer.maybeGet(key) ?? defaultValue()
    }
    mutableAddressWithNativeOwner {
      let (_, address) = _variantBuffer
        .pointerToValue(forKey: key, insertingDefault: defaultValue)
      return (address, Builtin.castToNativeObject(
        _variantBuffer.asNative._storage))
    }
  }

  /// Returns a new dictionary containing the keys of this dictionary with the
  /// values transformed by the given closure.
  ///
  /// - Parameter transform: A closure that transforms a value. `transform`
  ///   accepts each value of the dictionary as its parameter and returns a
  ///   transformed value of the same or of a different type.
  /// - Returns: A dictionary containing the keys and transformed values of
  ///   this dictionary.
  @inlinable // FIXME(sil-serialize-all)
  public func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> Dictionary<Key, T> {
    return try Dictionary<Key, T>(
      _variantBuffer: _variantBuffer.mapValues(transform))
  }

  /// Returns a new dictionary containing the keys of this dictionary with the
  /// values transformed by the given closure.
  /// - Parameter transform: A closure that transforms a value. `transform`
  ///   accepts each value of the dictionary as its parameter and returns a
  ///   transformed value of the same or of a different type.
  /// - Returns: A dictionary containing the keys and transformed values of
  ///   this dictionary.
  @inlinable // FIXME(sil-serialize-all)
  public func compactMapValues<T>(
    _ transform: (Value) throws -> T?
  ) rethrows -> Dictionary<Key, T> {
    return try self.reduce(into: [Key: T](), { (result, x) in
      if let value = try transform(x.value) {
        result[x.key] = value
      }
    })
  }

  /// Updates the value stored in the dictionary for the given key, or adds a
  /// new key-value pair if the key does not exist.
  ///
  /// Use this method instead of key-based subscripting when you need to know
  /// whether the new value supplants the value of an existing key. If the
  /// value of an existing key is updated, `updateValue(_:forKey:)` returns
  /// the original value.
  ///
  ///     var hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///
  ///     if let oldValue = hues.updateValue(18, forKey: "Coral") {
  ///         print("The old value of \(oldValue) was replaced with a new one.")
  ///     }
  ///     // Prints "The old value of 16 was replaced with a new one."
  ///
  /// If the given key is not present in the dictionary, this method adds the
  /// key-value pair and returns `nil`.
  ///
  ///     if let oldValue = hues.updateValue(330, forKey: "Cerise") {
  ///         print("The old value of \(oldValue) was replaced with a new one.")
  ///     } else {
  ///         print("No value was found in the dictionary for that key.")
  ///     }
  ///     // Prints "No value was found in the dictionary for that key."
  ///
  /// - Parameters:
  ///   - value: The new value to add to the dictionary.
  ///   - key: The key to associate with `value`. If `key` already exists in
  ///     the dictionary, `value` replaces the existing associated value. If
  ///     `key` isn't already a key of the dictionary, the `(key, value)` pair
  ///     is added.
  /// - Returns: The value that was replaced, or `nil` if a new key-value pair
  ///   was added.
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func updateValue(
    _ value: Value, forKey key: Key
  ) -> Value? {
    return _variantBuffer.updateValue(value, forKey: key)
  }

  /// Merges the key-value pairs in the given sequence into the dictionary,
  /// using a combining closure to determine the value for any duplicate keys.
  ///
  /// Use the `combine` closure to select a value to use in the updated
  /// dictionary, or to combine existing and new values. As the key-value
  /// pairs are merged with the dictionary, the `combine` closure is called
  /// with the current and new values for any duplicate keys that are
  /// encountered.
  ///
  /// This example shows how to choose the current or new values for any
  /// duplicate keys:
  ///
  ///     var dictionary = ["a": 1, "b": 2]
  ///
  ///     // Keeping existing value for key "a":
  ///     dictionary.merge(zip(["a", "c"], [3, 4])) { (current, _) in current }
  ///     // ["b": 2, "a": 1, "c": 4]
  ///
  ///     // Taking the new value for key "a":
  ///     dictionary.merge(zip(["a", "d"], [5, 6])) { (_, new) in new }
  ///     // ["b": 2, "a": 5, "c": 4, "d": 6]
  ///
  /// - Parameters:
  ///   - other:  A sequence of key-value pairs.
  ///   - combine: A closure that takes the current and new values for any
  ///     duplicate keys. The closure returns the desired value for the final
  ///     dictionary.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func merge<S: Sequence>(
    _ other: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    try _variantBuffer.merge(other, uniquingKeysWith: combine)
  }

  /// Merges the given dictionary into this dictionary, using a combining
  /// closure to determine the value for any duplicate keys.
  ///
  /// Use the `combine` closure to select a value to use in the updated
  /// dictionary, or to combine existing and new values. As the key-values
  /// pairs in `other` are merged with this dictionary, the `combine` closure
  /// is called with the current and new values for any duplicate keys that
  /// are encountered.
  ///
  /// This example shows how to choose the current or new values for any
  /// duplicate keys:
  ///
  ///     var dictionary = ["a": 1, "b": 2]
  ///
  ///     // Keeping existing value for key "a":
  ///     dictionary.merge(["a": 3, "c": 4]) { (current, _) in current }
  ///     // ["b": 2, "a": 1, "c": 4]
  ///
  ///     // Taking the new value for key "a":
  ///     dictionary.merge(["a": 5, "d": 6]) { (_, new) in new }
  ///     // ["b": 2, "a": 5, "c": 4, "d": 6]
  ///
  /// - Parameters:
  ///   - other:  A dictionary to merge.
  ///   - combine: A closure that takes the current and new values for any
  ///     duplicate keys. The closure returns the desired value for the final
  ///     dictionary.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func merge(
    _ other: [Key: Value],
    uniquingKeysWith combine: (Value, Value) throws -> Value) rethrows
  {
    try _variantBuffer.merge(
      other.lazy.map { ($0, $1) }, uniquingKeysWith: combine)
  }

  /// Creates a dictionary by merging key-value pairs in a sequence into the
  /// dictionary, using a combining closure to determine the value for
  /// duplicate keys.
  ///
  /// Use the `combine` closure to select a value to use in the returned
  /// dictionary, or to combine existing and new values. As the key-value
  /// pairs are merged with the dictionary, the `combine` closure is called
  /// with the current and new values for any duplicate keys that are
  /// encountered.
  ///
  /// This example shows how to choose the current or new values for any
  /// duplicate keys:
  ///
  ///     let dictionary = ["a": 1, "b": 2]
  ///     let newKeyValues = zip(["a", "b"], [3, 4])
  ///
  ///     let keepingCurrent = dictionary.merging(newKeyValues) { (current, _) in current }
  ///     // ["b": 2, "a": 1]
  ///     let replacingCurrent = dictionary.merging(newKeyValues) { (_, new) in new }
  ///     // ["b": 4, "a": 3]
  ///
  /// - Parameters:
  ///   - other:  A sequence of key-value pairs.
  ///   - combine: A closure that takes the current and new values for any
  ///     duplicate keys. The closure returns the desired value for the final
  ///     dictionary.
  /// - Returns: A new dictionary with the combined keys and values of this
  ///   dictionary and `other`.
  @inlinable // FIXME(sil-serialize-all)
  public func merging<S: Sequence>(
    _ other: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows -> [Key: Value] where S.Element == (Key, Value) {
    var result = self
    try result._variantBuffer.merge(other, uniquingKeysWith: combine)
    return result
  }

  /// Creates a dictionary by merging the given dictionary into this
  /// dictionary, using a combining closure to determine the value for
  /// duplicate keys.
  ///
  /// Use the `combine` closure to select a value to use in the returned
  /// dictionary, or to combine existing and new values. As the key-value
  /// pairs in `other` are merged with this dictionary, the `combine` closure
  /// is called with the current and new values for any duplicate keys that
  /// are encountered.
  ///
  /// This example shows how to choose the current or new values for any
  /// duplicate keys:
  ///
  ///     let dictionary = ["a": 1, "b": 2]
  ///     let otherDictionary = ["a": 3, "b": 4]
  ///
  ///     let keepingCurrent = dictionary.merging(otherDictionary)
  ///           { (current, _) in current }
  ///     // ["b": 2, "a": 1]
  ///     let replacingCurrent = dictionary.merging(otherDictionary)
  ///           { (_, new) in new }
  ///     // ["b": 4, "a": 3]
  ///
  /// - Parameters:
  ///   - other:  A dictionary to merge.
  ///   - combine: A closure that takes the current and new values for any
  ///     duplicate keys. The closure returns the desired value for the final
  ///     dictionary.
  /// - Returns: A new dictionary with the combined keys and values of this
  ///   dictionary and `other`.
  @inlinable // FIXME(sil-serialize-all)
  public func merging(
    _ other: [Key: Value],
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows -> [Key: Value] {
    var result = self
    try result.merge(other, uniquingKeysWith: combine)
    return result
  }

  /// Removes and returns the key-value pair at the specified index.
  ///
  /// Calling this method invalidates any existing indices for use with this
  /// dictionary.
  ///
  /// - Parameter index: The position of the key-value pair to remove. `index`
  ///   must be a valid index of the dictionary, and must not equal the
  ///   dictionary's end index.
  /// - Returns: The key-value pair that correspond to `index`.
  ///
  /// - Complexity: O(*n*), where *n* is the number of key-value pairs in the
  ///   dictionary.
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func remove(at index: Index) -> Element {
    return _variantBuffer.remove(at: index)
  }

  /// Removes the given key and its associated value from the dictionary.
  ///
  /// If the key is found in the dictionary, this method returns the key's
  /// associated value. On removal, this method invalidates all indices with
  /// respect to the dictionary.
  ///
  ///     var hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///     if let value = hues.removeValue(forKey: "Coral") {
  ///         print("The value \(value) was removed.")
  ///     }
  ///     // Prints "The value 16 was removed."
  ///
  /// If the key isn't found in the dictionary, `removeValue(forKey:)` returns
  /// `nil`.
  ///
  ///     if let value = hues.removeValueForKey("Cerise") {
  ///         print("The value \(value) was removed.")
  ///     } else {
  ///         print("No value found for that key.")
  ///     }
  ///     // Prints "No value found for that key.""
  ///
  /// - Parameter key: The key to remove along with its associated value.
  /// - Returns: The value that was removed, or `nil` if the key was not
  ///   present in the dictionary.
  ///
  /// - Complexity: O(*n*), where *n* is the number of key-value pairs in the
  ///   dictionary.
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func removeValue(forKey key: Key) -> Value? {
    return _variantBuffer.removeValue(forKey: key)
  }

  /// Removes all key-value pairs from the dictionary.
  ///
  /// Calling this method invalidates all indices with respect to the
  /// dictionary.
  ///
  /// - Parameter keepCapacity: Whether the dictionary should keep its
  ///   underlying buffer. If you pass `true`, the operation preserves the
  ///   buffer capacity that the collection has, otherwise the underlying
  ///   buffer is released.  The default is `false`.
  ///
  /// - Complexity: O(*n*), where *n* is the number of key-value pairs in the
  ///   dictionary.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    // The 'will not decrease' part in the documentation comment is worded very
    // carefully.  The capacity can increase if we replace Cocoa buffer with
    // native buffer.
    _variantBuffer.removeAll(keepingCapacity: keepCapacity)
  }
}

extension Dictionary {
  /// A collection containing just the keys of the dictionary.
  ///
  /// When iterated over, keys appear in this collection in the same order as
  /// they occur in the dictionary's key-value pairs. Each key in the keys
  /// collection has a unique value.
  ///
  ///     let countryCodes = ["BR": "Brazil", "GH": "Ghana", "JP": "Japan"]
  ///     print(countryCodes)
  ///     // Prints "["BR": "Brazil", "JP": "Japan", "GH": "Ghana"]"
  ///
  ///     for k in countryCodes.keys {
  ///         print(k)
  ///     }
  ///     // Prints "BR"
  ///     // Prints "JP"
  ///     // Prints "GH"
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, introduced: 4.0)
  public var keys: Keys {
    return Keys(self)
  }

  /// A collection containing just the values of the dictionary.
  ///
  /// When iterated over, values appear in this collection in the same order as
  /// they occur in the dictionary's key-value pairs.
  ///
  ///     let countryCodes = ["BR": "Brazil", "GH": "Ghana", "JP": "Japan"]
  ///     print(countryCodes)
  ///     // Prints "["BR": "Brazil", "JP": "Japan", "GH": "Ghana"]"
  ///
  ///     for v in countryCodes.values {
  ///         print(v)
  ///     }
  ///     // Prints "Brazil"
  ///     // Prints "Japan"
  ///     // Prints "Ghana"
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, introduced: 4.0)
  public var values: Values {
    get {
      return Values(self)
    }
    set {
      self = Dictionary(_variantBuffer: newValue._variantBuffer)
    }
  }

  /// A view of a dictionary's keys.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Keys
    : Collection, Equatable,
      CustomStringConvertible, CustomDebugStringConvertible {
    public typealias Element = Key

    @usableFromInline // FIXME(sil-serialize-all)
    internal var _variantBuffer: Dictionary._VariantBuffer

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ _dictionary: Dictionary) {
      self._variantBuffer = _dictionary._variantBuffer
    }

    // Collection Conformance
    // ----------------------

    @inlinable // FIXME(sil-serialize-all)
    public var startIndex: Index {
      return _variantBuffer.startIndex
    }

    @inlinable // FIXME(sil-serialize-all)
    public var endIndex: Index {
      return _variantBuffer.endIndex
    }

    @inlinable // FIXME(sil-serialize-all)
    public func index(after i: Index) -> Index {
      return _variantBuffer.index(after: i)
    }

    @inlinable // FIXME(sil-serialize-all)
    public subscript(position: Index) -> Element {
      return _variantBuffer.assertingGet(at: position).key
    }

    // Customization
    // -------------

    /// The number of keys in the dictionary.
    ///
    /// - Complexity: O(1).
    @inlinable // FIXME(sil-serialize-all)
    public var count: Int {
      return _variantBuffer.count
    }

    @inlinable // FIXME(sil-serialize-all)
    public var isEmpty: Bool {
      return count == 0
    }

    @inlinable // FIXME(sil-serialize-all)
    public func _customContainsEquatableElement(_ element: Element) -> Bool? {
      return _variantBuffer.containsKey(element)
    }

    @inlinable // FIXME(sil-serialize-all)
    public func _customIndexOfEquatableElement(_ element: Element) -> Index?? {
      return Optional(_variantBuffer.index(forKey: element))
    }

    @inlinable // FIXME(sil-serialize-all)
    public func _customLastIndexOfEquatableElement(_ element: Element) -> Index?? {
      // The first and last elements are the same because each element is unique.
      return _customIndexOfEquatableElement(element)
    }

    @inlinable // FIXME(sil-serialize-all)
    public static func ==(lhs: Keys, rhs: Keys) -> Bool {
      // Equal if the two dictionaries share storage.
      if case (.native(let lhsNative), .native(let rhsNative)) =
        (lhs._variantBuffer, rhs._variantBuffer),
        lhsNative._storage === rhsNative._storage {
        return true
      }

      // Not equal if the dictionaries are different sizes.
      if lhs.count != rhs.count {
        return false
      }

      // Perform unordered comparison of keys.
      for key in lhs {
        if !rhs.contains(key) {
          return false
        }
      }

      return true
    }

    @inlinable // FIXME(sil-serialize-all)
    public var description: String {
      return _makeCollectionDescription(for: self, withTypeName: nil)
    }

    public var debugDescription: String {
      return _makeCollectionDescription(for: self, withTypeName: "Dictionary.Keys")
    }
  }

  /// A view of a dictionary's values.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Values
    : MutableCollection, CustomStringConvertible, CustomDebugStringConvertible {
    public typealias Element = Value

    @usableFromInline // FIXME(sil-serialize-all)
    internal var _variantBuffer: Dictionary._VariantBuffer

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ _dictionary: Dictionary) {
      self._variantBuffer = _dictionary._variantBuffer
    }

    // Collection Conformance
    // ----------------------

    @inlinable // FIXME(sil-serialize-all)
    public var startIndex: Index {
      return _variantBuffer.startIndex
    }

    @inlinable // FIXME(sil-serialize-all)
    public var endIndex: Index {
      return _variantBuffer.endIndex
    }

    @inlinable // FIXME(sil-serialize-all)
    public func index(after i: Index) -> Index {
      return _variantBuffer.index(after: i)
    }

    @inlinable // FIXME(sil-serialize-all)
    public subscript(position: Index) -> Element {
      get {
        return _variantBuffer.assertingGet(at: position).value
      }
      mutableAddressWithNativeOwner {
        let address = _variantBuffer.pointerToValue(at: position)
        return (address, Builtin.castToNativeObject(
          _variantBuffer.asNative._storage))
      }
    }

    // Customization
    // -------------

    /// The number of values in the dictionary.
    ///
    /// - Complexity: O(1).
    @inlinable // FIXME(sil-serialize-all)
    public var count: Int {
      return _variantBuffer.count
    }

    @inlinable // FIXME(sil-serialize-all)
    public var isEmpty: Bool {
      return count == 0
    }

    @inlinable // FIXME(sil-serialize-all)
    public var description: String {
      return _makeCollectionDescription(for: self, withTypeName: nil)
    }

    public var debugDescription: String {
      return _makeCollectionDescription(for: self, withTypeName: "Dictionary.Values")
    }
  }
}

extension Dictionary: Equatable where Value: Equatable {
  @inlinable // FIXME(sil-serialize-all)
  public static func == (lhs: [Key: Value], rhs: [Key: Value]) -> Bool {
    switch (lhs._variantBuffer, rhs._variantBuffer) {
    case (.native(let lhsNative), .native(let rhsNative)):

      if lhsNative._storage === rhsNative._storage {
        return true
      }

      if lhsNative.count != rhsNative.count {
        return false
      }

      for (k, v) in lhs {
        let (pos, found) = rhsNative._find(k, startBucket: rhsNative._bucket(k))
        // FIXME: Can't write the simple code pending
        // <rdar://problem/15484639> Refcounting bug
        /*
        if !found || rhs[pos].value != lhsElement.value {
          return false
        }
        */
        if !found {
          return false
        }
        if rhsNative.value(at: pos.offset) != v {
          return false
        }
      }
      return true

  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      return _stdlib_NSObject_isEqual(
        lhsCocoa.cocoaDictionary, rhsCocoa.cocoaDictionary)

    case (.native(let lhsNative), .cocoa(let rhsCocoa)):

      if lhsNative.count != rhsCocoa.count {
        return false
      }

      let endIndex = lhsNative.endIndex
      var index = lhsNative.startIndex
      while index != endIndex {
        let (key, value) = lhsNative.assertingGet(at: index)
        let optRhsValue: AnyObject? =
          rhsCocoa.maybeGet(_bridgeAnythingToObjectiveC(key))

        guard let rhsValue = optRhsValue,
          value == _forceBridgeFromObjectiveC(rhsValue, Value.self)
        else {
          return false
        }

        lhsNative.formIndex(after: &index)
        continue
      }
      return true

    case (.cocoa, .native):
      return rhs == lhs
  #endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func != (lhs: [Key: Value], rhs: [Key: Value]) -> Bool {
    return !(lhs == rhs)
  }
}

extension Dictionary: Hashable where Value: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    var commutativeHash = 0
    for (k, v) in self {
      // Note that we use a copy of our own hasher here. This makes hash values
      // dependent on its state, eliminating static collision patterns.
      var elementHasher = hasher
      elementHasher.combine(k)
      elementHasher.combine(v)
      commutativeHash ^= elementHasher._finalize()
    }
    hasher.combine(commutativeHash)
  }
}

extension Dictionary: _HasCustomAnyHashableRepresentation
where Value: Hashable {
  public func _toCustomAnyHashable() -> AnyHashable? {
    return AnyHashable(_box: _DictionaryAnyHashableBox(self))
  }
}

internal struct _DictionaryAnyHashableBox<Key: Hashable, Value: Hashable>
  : _AnyHashableBox {
  internal let _value: Dictionary<Key, Value>
  internal let _canonical: Dictionary<AnyHashable, AnyHashable>

  internal init(_ value: Dictionary<Key, Value>) {
    self._value = value
    self._canonical = value as Dictionary<AnyHashable, AnyHashable>
  }

  internal var _base: Any {
    return _value
  }

  internal var _canonicalBox: _AnyHashableBox {
    return _DictionaryAnyHashableBox<AnyHashable, AnyHashable>(_canonical)
  }

  internal func _isEqual(to other: _AnyHashableBox) -> Bool? {
    guard
      let other = other as? _DictionaryAnyHashableBox<AnyHashable, AnyHashable>
    else {
      return nil
    }
    return _canonical == other._value
  }

  internal var _hashValue: Int {
    return _canonical.hashValue
  }

  internal func _hash(into hasher: inout Hasher) {
    _canonical.hash(into: &hasher)
  }

  internal func _rawHashValue(_seed: (UInt64, UInt64)) -> Int {
    return _canonical._rawHashValue(seed: _seed)
  }

  internal func _unbox<T: Hashable>() -> T? {
    return _value as? T
  }

  internal func _downCastConditional<T>(
    into result: UnsafeMutablePointer<T>
  ) -> Bool {
    guard let value = _value as? T else { return false }
    result.initialize(to: value)
    return true
  }
}

extension Dictionary: CustomStringConvertible, CustomDebugStringConvertible {
  @inlinable // FIXME(sil-serialize-all)
  internal func _makeDescription() -> String {
    if count == 0 {
      return "[:]"
    }

    var result = "["
    var first = true
    for (k, v) in self {
      if first {
        first = false
      } else {
        result += ", "
      }
      debugPrint(k, terminator: "", to: &result)
      result += ": "
      debugPrint(v, terminator: "", to: &result)
    }
    result += "]"
    return result
  }

  /// A string that represents the contents of the dictionary.
  @inlinable // FIXME(sil-serialize-all)
  public var description: String {
    return _makeDescription()
  }

  /// A string that represents the contents of the dictionary, suitable for
  /// debugging.
  public var debugDescription: String {
    return _makeDescription()
  }
}

@usableFromInline // FIXME(sil-serialize-all)
@_frozen // FIXME(sil-serialize-all)
internal enum _MergeError: Error {
  case keyCollision
}

#if _runtime(_ObjC)
/// Equivalent to `NSDictionary.allKeys`, but does not leave objects on the
/// autorelease pool.
@inlinable // FIXME(sil-serialize-all)
internal func _stdlib_NSDictionary_allKeys(_ nsd: _NSDictionary)
    -> _HeapBuffer<Int, AnyObject> {
  let count = nsd.count
  let storage = _HeapBuffer<Int, AnyObject>(
    _HeapBufferStorage<Int, AnyObject>.self, count, count)

  nsd.getObjects(nil, andKeys: storage.baseAddress, count: count)
  return storage
}
#endif

//===--- Compiler conversion/casting entry points for Dictionary<K, V> ----===//

/// Perform a non-bridged upcast that always succeeds.
///
/// - Precondition: `BaseKey` and `BaseValue` are base classes or base `@objc`
///   protocols (such as `AnyObject`) of `DerivedKey` and `DerivedValue`,
///   respectively.
@inlinable // FIXME(sil-serialize-all)
public func _dictionaryUpCast<DerivedKey, DerivedValue, BaseKey, BaseValue>(
    _ source: Dictionary<DerivedKey, DerivedValue>
) -> Dictionary<BaseKey, BaseValue> {
  var result = Dictionary<BaseKey, BaseValue>(minimumCapacity: source.count)

  for (k, v) in source {
    result[k as! BaseKey] = (v as! BaseValue)
  }
  return result
}

#if _runtime(_ObjC)

/// Implements an unconditional upcast that involves bridging.
///
/// The cast can fail if bridging fails.
///
/// - Precondition: `SwiftKey` and `SwiftValue` are bridged to Objective-C,
///   and at least one of them requires non-trivial bridging.
@inline(never)
public func _dictionaryBridgeToObjectiveC<
  SwiftKey, SwiftValue, ObjCKey, ObjCValue
>(
  _ source: Dictionary<SwiftKey, SwiftValue>
) -> Dictionary<ObjCKey, ObjCValue> {

  // Note: We force this function to stay in the swift dylib because
  // it is not performance sensitive and keeping it in the dylib saves
  // a new kilobytes for each specialization for all users of dictionary.

  _sanityCheck(
    !_isBridgedVerbatimToObjectiveC(SwiftKey.self) ||
    !_isBridgedVerbatimToObjectiveC(SwiftValue.self))
  _sanityCheck(
    _isClassOrObjCExistential(ObjCKey.self) ||
    _isClassOrObjCExistential(ObjCValue.self))

  var result = Dictionary<ObjCKey, ObjCValue>(minimumCapacity: source.count)
  let keyBridgesDirectly =
    _isBridgedVerbatimToObjectiveC(SwiftKey.self) ==
      _isBridgedVerbatimToObjectiveC(ObjCKey.self)
  let valueBridgesDirectly =
    _isBridgedVerbatimToObjectiveC(SwiftValue.self) ==
      _isBridgedVerbatimToObjectiveC(ObjCValue.self)
  for (key, value) in source {
    // Bridge the key
    var bridgedKey: ObjCKey
    if keyBridgesDirectly {
      bridgedKey = unsafeBitCast(key, to: ObjCKey.self)
    } else {
      let bridged: AnyObject = _bridgeAnythingToObjectiveC(key)
      bridgedKey = unsafeBitCast(bridged, to: ObjCKey.self)
    }

    // Bridge the value
    var bridgedValue: ObjCValue
    if valueBridgesDirectly {
      bridgedValue = unsafeBitCast(value, to: ObjCValue.self)
    } else {
      let bridged: AnyObject? = _bridgeAnythingToObjectiveC(value)
      bridgedValue = unsafeBitCast(bridged, to: ObjCValue.self)
    }

    result[bridgedKey] = bridgedValue
  }

  return result
}
#endif

/// Called by the casting machinery.
@_silgen_name("_swift_dictionaryDownCastIndirect")
internal func _dictionaryDownCastIndirect<SourceKey, SourceValue,
                                          TargetKey, TargetValue>(
  _ source: UnsafePointer<Dictionary<SourceKey, SourceValue>>,
  _ target: UnsafeMutablePointer<Dictionary<TargetKey, TargetValue>>) {
  target.initialize(to: _dictionaryDownCast(source.pointee))
}

/// Implements a forced downcast.  This operation should have O(1) complexity.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
///
/// - Precondition: `DerivedKey` is a subtype of `BaseKey`, `DerivedValue` is
///   a subtype of `BaseValue`, and all of these types are reference types.
@inlinable // FIXME(sil-serialize-all)
public func _dictionaryDownCast<BaseKey, BaseValue, DerivedKey, DerivedValue>(
  _ source: Dictionary<BaseKey, BaseValue>
) -> Dictionary<DerivedKey, DerivedValue> {

#if _runtime(_ObjC)
  if _isClassOrObjCExistential(BaseKey.self)
  && _isClassOrObjCExistential(BaseValue.self)
  && _isClassOrObjCExistential(DerivedKey.self)
  && _isClassOrObjCExistential(DerivedValue.self) {

    switch source._variantBuffer {
    case .native(let buffer):
      // Note: it is safe to treat the buffer as immutable here because
      // Dictionary will not mutate buffer with reference count greater than 1.
      return Dictionary(_immutableCocoaDictionary: buffer.bridged())
    case .cocoa(let cocoaBuffer):
      return Dictionary(_immutableCocoaDictionary: cocoaBuffer.cocoaDictionary)
    }
  }
#endif
  return _dictionaryDownCastConditional(source)!
}

/// Called by the casting machinery.
@_silgen_name("_swift_dictionaryDownCastConditionalIndirect")
internal func _dictionaryDownCastConditionalIndirect<SourceKey, SourceValue,
                                                     TargetKey, TargetValue>(
  _ source: UnsafePointer<Dictionary<SourceKey, SourceValue>>,
  _ target: UnsafeMutablePointer<Dictionary<TargetKey, TargetValue>>
) -> Bool {
  if let result: Dictionary<TargetKey, TargetValue>
       = _dictionaryDownCastConditional(source.pointee) {
    target.initialize(to: result)
    return true
  }
  return false
}

/// Implements a conditional downcast.
///
/// If the cast fails, the function returns `nil`.  All checks should be
/// performed eagerly.
///
/// - Precondition: `DerivedKey` is a subtype of `BaseKey`, `DerivedValue` is
///   a subtype of `BaseValue`, and all of these types are reference types.
@inlinable // FIXME(sil-serialize-all)
public func _dictionaryDownCastConditional<
  BaseKey, BaseValue, DerivedKey, DerivedValue
>(
  _ source: Dictionary<BaseKey, BaseValue>
) -> Dictionary<DerivedKey, DerivedValue>? {

  var result = Dictionary<DerivedKey, DerivedValue>()
  for (k, v) in source {
    guard let k1 = k as? DerivedKey, let v1 = v as? DerivedValue
    else { return nil }
    result[k1] = v1
  }
  return result
}

#if _runtime(_ObjC)
/// Implements an unconditional downcast that involves bridging.
///
/// - Precondition: At least one of `SwiftKey` or `SwiftValue` is a bridged value
///   type, and the corresponding `ObjCKey` or `ObjCValue` is a reference type.
@inlinable // FIXME(sil-serialize-all)
public func _dictionaryBridgeFromObjectiveC<
  ObjCKey, ObjCValue, SwiftKey, SwiftValue
>(
  _ source: Dictionary<ObjCKey, ObjCValue>
) -> Dictionary<SwiftKey, SwiftValue> {
  let result: Dictionary<SwiftKey, SwiftValue>? =
    _dictionaryBridgeFromObjectiveCConditional(source)
  _precondition(result != nil, "Dictionary cannot be bridged from Objective-C")
  return result!
}

/// Implements a conditional downcast that involves bridging.
///
/// If the cast fails, the function returns `nil`.  All checks should be
/// performed eagerly.
///
/// - Precondition: At least one of `SwiftKey` or `SwiftValue` is a bridged value
///   type, and the corresponding `ObjCKey` or `ObjCValue` is a reference type.
@inlinable // FIXME(sil-serialize-all)
public func _dictionaryBridgeFromObjectiveCConditional<
  ObjCKey, ObjCValue, SwiftKey, SwiftValue
>(
  _ source: Dictionary<ObjCKey, ObjCValue>
) -> Dictionary<SwiftKey, SwiftValue>? {
  _sanityCheck(
    _isClassOrObjCExistential(ObjCKey.self) ||
    _isClassOrObjCExistential(ObjCValue.self))
  _sanityCheck(
    !_isBridgedVerbatimToObjectiveC(SwiftKey.self) ||
    !_isBridgedVerbatimToObjectiveC(SwiftValue.self))

  let keyBridgesDirectly =
    _isBridgedVerbatimToObjectiveC(SwiftKey.self) ==
      _isBridgedVerbatimToObjectiveC(ObjCKey.self)
  let valueBridgesDirectly =
    _isBridgedVerbatimToObjectiveC(SwiftValue.self) ==
      _isBridgedVerbatimToObjectiveC(ObjCValue.self)

  var result = Dictionary<SwiftKey, SwiftValue>(minimumCapacity: source.count)
  for (key, value) in source {
    // Downcast the key.
    var resultKey: SwiftKey
    if keyBridgesDirectly {
      if let bridgedKey = key as? SwiftKey {
        resultKey = bridgedKey
      } else {
        return nil
      }
    } else {
      if let bridgedKey = _conditionallyBridgeFromObjectiveC(
        _reinterpretCastToAnyObject(key), SwiftKey.self) {
          resultKey = bridgedKey
      } else {
        return nil
      }
    }

    // Downcast the value.
    var resultValue: SwiftValue
    if valueBridgesDirectly {
      if let bridgedValue = value as? SwiftValue {
        resultValue = bridgedValue
      } else {
        return nil
      }
    } else {
      if let bridgedValue = _conditionallyBridgeFromObjectiveC(
        _reinterpretCastToAnyObject(value), SwiftValue.self) {
          resultValue = bridgedValue
      } else {
        return nil
      }
    }

    result[resultKey] = resultValue
  }
  return result
}
#endif

//===--- APIs templated for Dictionary and Set ----------------------------===//

/// This protocol is only used for compile-time checks that
/// every buffer type implements all required operations.
internal protocol _DictionaryBuffer {
  associatedtype Key
  associatedtype Value
  associatedtype Index
  associatedtype SequenceElement
  associatedtype SequenceElementWithoutLabels

  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after i: Index) -> Index
  func formIndex(after i: inout Index)
  func index(forKey key: Key) -> Index?
  var count: Int { get }

  func assertingGet(at i: Index) -> SequenceElement
  func maybeGet(_ key: Key) -> Value?
}

/// An instance of this class has all `Dictionary` data tail-allocated.
/// Enough bytes are allocated to hold the bitmap for marking valid entries,
/// keys, and values. The data layout starts with the bitmap, followed by the
/// keys, followed by the values.
//
// See the docs at the top of the file for more details on this type
//
// NOTE: The precise layout of this type is relied on in the runtime
// to provide a statically allocated empty singleton.
// See stdlib/public/stubs/GlobalObjects.cpp for details.
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
@_objc_non_lazy_realization
internal class _RawNativeDictionaryStorage
  : _SwiftNativeNSDictionary, _NSDictionaryCore
{
  @usableFromInline // FIXME(sil-serialize-all)
  @nonobjc
  internal final var bucketCount: Int

  @usableFromInline // FIXME(sil-serialize-all)
  internal final var count: Int

  @usableFromInline // FIXME(sil-serialize-all)
  internal final var initializedEntries: _UnsafeBitMap

  @usableFromInline // FIXME(sil-serialize-all)
  @nonobjc
  internal final var keys: UnsafeMutableRawPointer
  @usableFromInline // FIXME(sil-serialize-all)
  @nonobjc
  internal final var values: UnsafeMutableRawPointer

  @usableFromInline // FIXME(sil-serialize-all)
  internal final var seed: (UInt64, UInt64)

  // This API is unsafe and needs a `_fixLifetime` in the caller.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal final
  var _initializedHashtableEntriesBitMapBuffer: UnsafeMutablePointer<UInt> {
    return UnsafeMutablePointer(Builtin.projectTailElems(self, UInt.self))
  }

  /// The empty singleton that is used for every single Dictionary that is
  /// created without any elements. The contents of the storage should never
  /// be mutated.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal static var empty: _RawNativeDictionaryStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyDictionaryStorage))
  }

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only create this by using the `empty` singleton")
  }

#if _runtime(_ObjC)
  //
  // NSDictionary implementation, assuming Self is the empty singleton
  //

  /// Get the NSEnumerator implementation for self.
  /// _HashableTypedNativeDictionaryStorage overloads this to give
  /// _NativeSelfNSEnumerator proper type parameters.
  @objc
  internal func enumerator() -> _NSEnumerator {
    return _NativeDictionaryNSEnumerator<AnyObject, AnyObject>(
        _NativeDictionaryBuffer(_storage: self))
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return self
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int {
    // Even though we never do anything in here, we need to update the
    // state so that callers know we actually ran.

    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
    }
    state.pointee = theState

    return 0
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc(objectForKey:)
  internal func objectFor(_ aKey: AnyObject) -> AnyObject? {
    return nil
  }

  internal func keyEnumerator() -> _NSEnumerator {
    return enumerator()
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc(getObjects:andKeys:count:)
  internal func getObjects(
    _ objects: UnsafeMutablePointer<AnyObject>?,
    andKeys keys: UnsafeMutablePointer<AnyObject>?,
    count: Int) {
    // Do nothing, we're empty
  }
#endif
}

// See the docs at the top of this file for a description of this type
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
internal class _TypedNativeDictionaryStorage<Key, Value>
  : _RawNativeDictionaryStorage {

  deinit {
    let keys = self.keys.assumingMemoryBound(to: Key.self)
    let values = self.values.assumingMemoryBound(to: Value.self)

    if !_isPOD(Key.self) {
      for i in 0 ..< bucketCount {
        if initializedEntries[i] {
          (keys+i).deinitialize(count: 1)
        }
      }
    }

    if !_isPOD(Value.self) {
      for i in 0 ..< bucketCount {
        if initializedEntries[i] {
          (values+i).deinitialize(count: 1)
        }
      }
    }
    _fixLifetime(self)
  }

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only create this by calling Buffer's inits")
  }

#if _runtime(_ObjC)
  @inlinable // FIXME(sil-serialize-all)
  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("don't call this designated initializer")
  }
#endif
}

// See the docs at the top of this file for a description of this type
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
final internal class _HashableTypedNativeDictionaryStorage<Key: Hashable, Value>
  : _TypedNativeDictionaryStorage<Key, Value> {

  @usableFromInline
  internal typealias FullContainer = Dictionary<Key, Value>
  @usableFromInline
  internal typealias Buffer = _NativeDictionaryBuffer<Key, Value>

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only create this by calling Buffer's inits'")
  }

#if _runtime(_ObjC)
  // NSDictionary bridging:

  // All actual functionality comes from buffer/full, which are
  // just wrappers around a RawNativeDictionaryStorage.

  @inlinable // FIXME(sil-serialize-all)
  internal var buffer: Buffer {
    return Buffer(_storage: self)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var full: FullContainer {
    return FullContainer(_nativeBuffer: buffer)
  }

  @objc
  internal override func enumerator() -> _NSEnumerator {
    return _NativeDictionaryNSEnumerator<Key, Value>(
        Buffer(_storage: self))
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc(countByEnumeratingWithState:objects:count:)
  internal override func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int {
    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      theState.extra.0 = CUnsignedLong(full.startIndex._nativeIndex.offset)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var currIndex = _NativeDictionaryIndex<Key, Value>(
        offset: Int(theState.extra.0))
    let endIndex = buffer.endIndex
    var stored = 0
    for i in 0..<count {
      if (currIndex == endIndex) {
        break
      }

      unmanagedObjects[i] = buffer.bridgedKey(at: currIndex)

      stored += 1
      buffer.formIndex(after: &currIndex)
    }
    theState.extra.0 = CUnsignedLong(currIndex.offset)
    state.pointee = theState
    return stored
  }

  @inlinable // FIXME(sil-serialize-all)
  @nonobjc
  internal func getObjectFor(_ aKey: AnyObject) -> AnyObject? {
    guard let nativeKey = _conditionallyBridgeFromObjectiveC(aKey, Key.self)
    else { return nil }

    let (i, found) = buffer._find(nativeKey,
        startBucket: buffer._bucket(nativeKey))

    if found {
      return buffer.bridgedValue(at: i)
    }
    return nil
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @inlinable // FIXME(sil-serialize-all)
  @objc(objectForKey:)
  override func objectFor(_ aKey: AnyObject) -> AnyObject? {
    return getObjectFor(aKey)
  }

  // We also override the following methods for efficiency.
  @inlinable // FIXME(sil-serialize-all)
  @objc(getObjects:andKeys:count:)
  override func getObjects(
    _ objects: UnsafeMutablePointer<AnyObject>?,
    andKeys keys: UnsafeMutablePointer<AnyObject>?,
    count: Int) {
    _precondition(count >= 0, "Invalid count")
    guard count > 0 else { return }
    var i = 0 // Current position in the output buffers
    if let unmanagedKeys = _UnmanagedAnyObjectArray(keys) {
      if let unmanagedObjects = _UnmanagedAnyObjectArray(objects) {
        // keys nonnull, objects nonnull
        for (key, value) in full {
          unmanagedObjects[i] = _bridgeAnythingToObjectiveC(value)
          unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
          i += 1
          guard i < count else { break }
        }
      } else {
        // keys nonnull, objects null
        for (key, _) in full {
          unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
          i += 1
          guard i < count else { break }
        }
      }
    } else {
      if let unmanagedObjects = _UnmanagedAnyObjectArray(objects) {
        // keys null, objects nonnull
        for (_, value) in full {
          unmanagedObjects[i] = _bridgeAnythingToObjectiveC(value)
          i += 1
          guard i < count else { break }
        }
      } else {
        // do nothing, both are null
      }
    }
  }
#endif
}

/// A wrapper around _RawNativeDictionaryStorage that provides most of the
/// implementation of Dictionary.
///
/// This type and most of its functionality doesn't require Hashable at all.
/// The reason for this is to support storing AnyObject for bridging
/// with _SwiftDeferredNSDictionary. What functionality actually relies on
/// Hashable can be found in an extension.
@usableFromInline
@_fixed_layout
internal struct _NativeDictionaryBuffer<Key, Value> {
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias TypedStorage = _TypedNativeDictionaryStorage<Key, Value>
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias Buffer = _NativeDictionaryBuffer<Key, Value>
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias Index = _NativeDictionaryIndex<Key, Value>

  @usableFromInline
  internal typealias SequenceElementWithoutLabels = (Key, Value)

  /// See this comments on _RawNativeDictionaryStorage and its subclasses to
  /// understand why we store an untyped storage here.
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _storage: _RawNativeDictionaryStorage

  /// Creates a Buffer with a storage that is typed, but doesn't understand
  /// Hashing. Mostly for bridging; prefer `init(minimumCapacity:)`.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_exactBucketCount bucketCount: Int, unhashable: ()) {
    let bitmapWordCount = _UnsafeBitMap.sizeInWords(forSizeInBits: bucketCount)
    let storage = Builtin.allocWithTailElems_3(TypedStorage.self,
        bitmapWordCount._builtinWordValue, UInt.self,
        bucketCount._builtinWordValue, Key.self,
        bucketCount._builtinWordValue, Value.self)
    self.init(_exactBucketCount: bucketCount, storage: storage)
  }

  /// Given a bucket count and uninitialized _RawNativeDictionaryStorage,
  /// completes the initialization and returns a Buffer.
  @inlinable // FIXME(sil-serialize-all)
  internal init(
    _exactBucketCount bucketCount: Int,
    storage: _RawNativeDictionaryStorage
  ) {
    storage.bucketCount = bucketCount
    storage.count = 0

    self.init(_storage: storage)

    let initializedEntries = _UnsafeBitMap(
        storage: _initializedHashtableEntriesBitMapBuffer,
        bitCount: bucketCount)
    initializedEntries.initializeToZero()

    // Compute all the array offsets now, so we don't have to later
    let bitmapAddr = Builtin.projectTailElems(_storage, UInt.self)
    let bitmapWordCount = _UnsafeBitMap.sizeInWords(forSizeInBits: bucketCount)
    let keysAddr = Builtin.getTailAddr_Word(bitmapAddr,
           bitmapWordCount._builtinWordValue, UInt.self, Key.self)

    // Initialize header
    _storage.initializedEntries = initializedEntries
    _storage.keys = UnsafeMutableRawPointer(keysAddr)
    let valuesAddr = Builtin.getTailAddr_Word(keysAddr,
        bucketCount._builtinWordValue, Key.self, Value.self)
    _storage.values = UnsafeMutableRawPointer(valuesAddr)
    // We assign a unique hash seed to each distinct hash table size, so that we
    // avoid certain copy operations becoming quadratic, without breaking value
    // semantics. (See https://bugs.swift.org/browse/SR-3268)
    //
    // We don't need to generate a brand new seed for each table size: it's
    // enough to change a single bit in the global seed by XORing the bucket
    // count to it. (The bucket count is always a power of two.)
    //
    // FIXME: Use an approximation of true per-instance seeding. We can't just
    // use the base address, because COW copies need to share the same seed.
    let seed = Hasher._seed
    let perturbation = bucketCount
    _storage.seed = (seed.0 ^ UInt64(truncatingIfNeeded: perturbation), seed.1)
  }

  // Forwarding the individual fields of the storage in various forms

  @inlinable // FIXME(sil-serialize-all)
  internal var bucketCount: Int {
    return _assumeNonNegative(_storage.bucketCount)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var count: Int {
    set {
      _storage.count = newValue
    }
    get {
      return _assumeNonNegative(_storage.count)
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal
  var _initializedHashtableEntriesBitMapBuffer: UnsafeMutablePointer<UInt> {
    return _storage._initializedHashtableEntriesBitMapBuffer
  }

  // This API is unsafe and needs a `_fixLifetime` in the caller.
  @inlinable // FIXME(sil-serialize-all)
  internal var keys: UnsafeMutablePointer<Key> {
    return _storage.keys.assumingMemoryBound(to: Key.self)
  }

  // This API is unsafe and needs a `_fixLifetime` in the caller.
  @inlinable // FIXME(sil-serialize-all)
  internal var values: UnsafeMutablePointer<Value> {
    return _storage.values.assumingMemoryBound(to: Value.self)
  }

  /// Constructs a buffer adopting the given storage.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_storage: _RawNativeDictionaryStorage) {
    self._storage = _storage
  }

  /// Constructs an instance from the empty singleton.
  @inlinable // FIXME(sil-serialize-all)
  internal init() {
    self._storage = _RawNativeDictionaryStorage.empty
  }

  // Most of the implementation of the _DictionaryBuffer protocol,
  // but only the parts that don't actually rely on hashing.

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func key(at i: Int) -> Key {
    _sanityCheck(i >= 0 && i < bucketCount)
    _sanityCheck(isInitializedEntry(at: i))
    defer { _fixLifetime(self) }

    let res = (keys + i).pointee
    return res
  }

#if _runtime(_ObjC)
  /// Returns the key at the given Index, bridged.
  ///
  /// Intended for use with verbatim bridgeable keys.
  @inlinable // FIXME(sil-serialize-all)
  internal func bridgedKey(at index: Index) -> AnyObject {
    let k = key(at: index.offset)
    return _bridgeAnythingToObjectiveC(k)
  }

  /// Returns the value at the given Index, bridged.
  ///
  /// Intended for use with verbatim bridgeable keys.
  @inlinable // FIXME(sil-serialize-all)
  internal func bridgedValue(at index: Index) -> AnyObject {
    let v = value(at: index.offset)
    return _bridgeAnythingToObjectiveC(v)
  }
#endif

  @inlinable // FIXME(sil-serialize-all)
  internal func isInitializedEntry(at i: Int) -> Bool {
    _sanityCheck(i >= 0 && i < bucketCount)
    defer { _fixLifetime(self) }

    return _storage.initializedEntries[i]
  }

  @usableFromInline @_transparent
  internal func destroyEntry(at i: Int) {
    _sanityCheck(isInitializedEntry(at: i))
    defer { _fixLifetime(self) }

    (keys + i).deinitialize(count: 1)
    (values + i).deinitialize(count: 1)
    _storage.initializedEntries[i] = false
  }

  @usableFromInline @_transparent
  internal func initializeKey(_ k: Key, value v: Value, at i: Int) {
    _sanityCheck(!isInitializedEntry(at: i))
    defer { _fixLifetime(self) }

    (keys + i).initialize(to: k)
    (values + i).initialize(to: v)
    _storage.initializedEntries[i] = true
  }

  @usableFromInline @_transparent
  internal func moveInitializeEntry(from: Buffer, at: Int, toEntryAt: Int) {
    _sanityCheck(!isInitializedEntry(at: toEntryAt))
    defer { _fixLifetime(self) }

    (keys + toEntryAt).initialize(to: (from.keys + at).move())
    (values + toEntryAt).initialize(to: (from.values + at).move())
    from._storage.initializedEntries[at] = false
    _storage.initializedEntries[toEntryAt] = true
  }

  @usableFromInline @_transparent
  internal func value(at i: Int) -> Value {
    _sanityCheck(isInitializedEntry(at: i))
    defer { _fixLifetime(self) }

    return (values + i).pointee
  }

  @usableFromInline @_transparent
  internal func setKey(_ key: Key, value: Value, at i: Int) {
    _sanityCheck(isInitializedEntry(at: i))
    defer { _fixLifetime(self) }

    (keys + i).pointee = key
    (values + i).pointee = value
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var startIndex: Index {
    // We start at "index after -1" instead of "0" because we need to find the
    // first occupied slot.
    return index(after: Index(offset: -1))
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var endIndex: Index {
    return Index(offset: bucketCount)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(after i: Index) -> Index {
    _precondition(i != endIndex)
    var idx = i.offset + 1
    while idx < bucketCount && !isInitializedEntry(at: idx) {
      idx += 1
    }

    return Index(offset: idx)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func formIndex(after i: inout Index) {
    i = index(after: i)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func assertingGet(at i: Index) -> SequenceElement {
    _precondition(i.offset >= 0 && i.offset < bucketCount)
    _precondition(
      isInitializedEntry(at: i.offset),
      "Attempting to access Dictionary elements using an invalid Index")
    let key = self.key(at: i.offset)
    return (key, self.value(at: i.offset))

  }
}

extension _NativeDictionaryBuffer where Key: Hashable {
  @usableFromInline
  internal typealias HashTypedStorage =
    _HashableTypedNativeDictionaryStorage<Key, Value>
  @usableFromInline
  internal typealias SequenceElement = (key: Key, value: Value)

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal init(minimumCapacity: Int) {
    let bucketCount = _NativeDictionaryBuffer.bucketCount(
      forCapacity: minimumCapacity,
      maxLoadFactorInverse: _hashContainerDefaultMaxLoadFactorInverse)
    self.init(bucketCount: bucketCount)
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal init(bucketCount: Int) {
    // Actual bucket count is the next power of 2 greater than or equal to
    // bucketCount. Make sure that is representable.
    _sanityCheck(bucketCount <= (Int.max >> 1) + 1)
    let buckets = 1 &<< ((Swift.max(bucketCount, 2) - 1)._binaryLogarithm() + 1)
    self.init(_exactBucketCount: buckets)
  }

  /// Create a buffer instance with room for at least 'bucketCount' entries,
  /// marking all entries invalid.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_exactBucketCount bucketCount: Int) {
    let bitmapWordCount = _UnsafeBitMap.sizeInWords(forSizeInBits: bucketCount)
    let storage = Builtin.allocWithTailElems_3(HashTypedStorage.self,
        bitmapWordCount._builtinWordValue, UInt.self,
        bucketCount._builtinWordValue, Key.self,
        bucketCount._builtinWordValue, Value.self)
    self.init(_exactBucketCount: bucketCount, storage: storage)
  }

#if _runtime(_ObjC)
  @usableFromInline
  internal func bridged() -> _NSDictionary {
    // We can zero-cost bridge if our keys are verbatim
    // or if we're the empty singleton.

    // Temporary var for SOME type safety before a cast.
    let nsSet: _NSDictionaryCore

    if (_isBridgedVerbatimToObjectiveC(Key.self) &&
        _isBridgedVerbatimToObjectiveC(Value.self)) ||
        self._storage === _RawNativeDictionaryStorage.empty {
      nsSet = self._storage
    } else {
      nsSet = _SwiftDeferredNSDictionary(nativeBuffer: self)
    }

    // Cast from "minimal NSDictionary" to "NSDictionary"
    // Note that if you actually ask Swift for this cast, it will fail.
    // Never trust a shadow protocol!
    return unsafeBitCast(nsSet, to: _NSDictionary.self)
  }
#endif

  /// A textual representation of `self`.
  @inlinable // FIXME(sil-serialize-all)
  internal var description: String {
    var result = ""
#if INTERNAL_CHECKS_ENABLED
    for i in 0..<bucketCount {
      if isInitializedEntry(at: i) {
        let key = self.key(at: i)
        result += "bucket \(i), ideal bucket = \(_bucket(key)), key = \(key)\n"
      } else {
        result += "bucket \(i), empty\n"
      }
    }
#endif
    return result
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _bucketMask: Int {
    // The bucket count is not negative, therefore subtracting 1 will not
    // overflow.
    return bucketCount &- 1
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always) // For performance reasons.
  internal func _bucket(_ k: Key) -> Int {
    return k._rawHashValue(seed: _storage.seed) & _bucketMask
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _index(after bucket: Int) -> Int {
    // Bucket is within 0 and bucketCount. Therefore adding 1 does not overflow.
    return (bucket &+ 1) & _bucketMask
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _prev(_ bucket: Int) -> Int {
    // Bucket is not negative. Therefore subtracting 1 does not overflow.
    return (bucket &- 1) & _bucketMask
  }

  /// Search for a given key starting from the specified bucket.
  ///
  /// If the key is not present, returns the position where it could be
  /// inserted.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func _find(_ key: Key, startBucket: Int)
    -> (pos: Index, found: Bool) {

    var bucket = startBucket

    // The invariant guarantees there's always a hole, so we just loop
    // until we find one
    while true {
      let isHole = !isInitializedEntry(at: bucket)
      if isHole {
        return (Index(offset: bucket), false)
      }
      if self.key(at: bucket) == key {
        return (Index(offset: bucket), true)
      }
      bucket = _index(after: bucket)
    }
  }

  @usableFromInline @_transparent
  internal static func bucketCount(
    forCapacity capacity: Int,
    maxLoadFactorInverse: Double
  ) -> Int {
    // `capacity + 1` below ensures that we don't fill in the last hole
    return max(Int((Double(capacity) * maxLoadFactorInverse).rounded(.up)),
               capacity + 1)
  }

  /// Buffer should be uniquely referenced.
  /// The `key` should not be present in the Dictionary.
  /// This function does *not* update `count`.
  @inlinable // FIXME(sil-serialize-all)
  internal func unsafeAddNew(key newKey: Key, value: Value) {
    let (i, found) = _find(newKey, startBucket: _bucket(newKey))
    _precondition(
      !found, "Duplicate key found in Dictionary. Keys may have been mutated after insertion")
    initializeKey(newKey, value: value, at: i.offset)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func fromArray(_ elements: [SequenceElementWithoutLabels])
    -> Buffer
  {
    if elements.isEmpty {
      return Buffer()
    }

    var nativeBuffer = Buffer(minimumCapacity: elements.count)

    for (key, value) in elements {
      let (i, found) =
        nativeBuffer._find(key, startBucket: nativeBuffer._bucket(key))
      _precondition(!found, "Dictionary literal contains duplicate keys")
      nativeBuffer.initializeKey(key, value: value, at: i.offset)
    }
    nativeBuffer.count = elements.count

    return nativeBuffer
  }
}

extension _NativeDictionaryBuffer/*: _DictionaryBuffer */ where Key: Hashable {
  //
  // _DictionaryBuffer conformance
  //

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func index(forKey key: Key) -> Index? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }
    let (i, found) = _find(key, startBucket: _bucket(key))
    return found ? i : nil
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func maybeGet(_ key: Key) -> Value? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }

    let (i, found) = _find(key, startBucket: _bucket(key))
    if found {
      return self.value(at: i.offset)
    }
    return nil
  }
}

#if _runtime(_ObjC)
/// An NSEnumerator that works with any NativeDictionaryBuffer of
/// verbatim bridgeable elements. Used by the various NSDictionary impls.
final internal class _NativeDictionaryNSEnumerator<Key, Value>
  : _SwiftNativeNSEnumerator, _NSEnumerator {

  internal typealias Buffer = _NativeDictionaryBuffer<Key, Value>
  internal typealias Index = _NativeDictionaryIndex<Key, Value>

  internal override required init() {
    _sanityCheckFailure("don't call this designated initializer")
  }

  internal init(_ buffer: Buffer) {
    self.buffer = buffer
    nextIndex = buffer.startIndex
    endIndex = buffer.endIndex
  }

  internal var buffer: Buffer
  internal var nextIndex: Index
  internal var endIndex: Index

  //
  // NSEnumerator implementation.
  //
  // Do not call any of these methods from the standard library!
  //

  @objc
  internal func nextObject() -> AnyObject? {
    if nextIndex == endIndex {
      return nil
    }
    let key = buffer.bridgedKey(at: nextIndex)
    buffer.formIndex(after: &nextIndex)
    return key
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>,
    count: Int
  ) -> Int {
    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
    }

    if nextIndex == endIndex {
      state.pointee = theState
      return 0
    }

    // Return only a single element so that code can start iterating via fast
    // enumeration, terminate it, and continue via NSEnumerator.
    let key = buffer.bridgedKey(at: nextIndex)
    buffer.formIndex(after: &nextIndex)

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects)
    unmanagedObjects[0] = key
    state.pointee = theState
    return 1
  }
}
#endif

#if _runtime(_ObjC)
/// This class exists for Objective-C bridging. It holds a reference to a
/// NativeDictionaryBuffer, and can be upcast to NSSelf when bridging is
/// necessary.  This is the fallback implementation for situations where
/// toll-free bridging isn't possible. On first access, a NativeDictionaryBuffer
/// of AnyObject will be constructed containing all the bridged elements.
final internal class _SwiftDeferredNSDictionary<Key: Hashable, Value>
  : _SwiftNativeNSDictionary, _NSDictionaryCore {

  internal init(nativeBuffer: _NativeDictionaryBuffer<Key, Value>) {
    self.nativeBuffer = nativeBuffer
    super.init()
  }

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  @nonobjc
  internal var _heapStorageBridged_DoNotUse: AnyObject?

  /// The unbridged elements.
  internal var nativeBuffer: _NativeDictionaryBuffer<Key, Value>

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // Instances of this class should be visible outside of standard library as
    // having `NSDictionary` type, which is immutable.
    return self
  }

  //
  // NSDictionary implementation.
  //
  // Do not call any of these methods from the standard library!  Use only
  // `nativeBuffer`.
  //

  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @objc(objectForKey:)
  internal func objectFor(_ aKey: AnyObject) -> AnyObject? {
    return bridgingObjectForKey(aKey)
  }

  @objc
  internal func keyEnumerator() -> _NSEnumerator {
    return enumerator()
  }

  @objc(getObjects:andKeys:count:)
  internal func getObjects(
    _ objects: UnsafeMutablePointer<AnyObject>?,
    andKeys keys: UnsafeMutablePointer<AnyObject>?,
    count: Int
  ) {
    _precondition(count >= 0, "Invalid count")
    guard count > 0 else { return }
    bridgeEverything()
    var i = 0 // Current position in the output buffers
    let bucketCount = nativeBuffer.bucketCount

    if let unmanagedKeys = _UnmanagedAnyObjectArray(keys) {
      if let unmanagedObjects = _UnmanagedAnyObjectArray(objects) {
        // keys nonnull, objects nonnull
        for position in 0..<bucketCount {
          if bridgedBuffer.isInitializedEntry(at: position) {
            unmanagedObjects[i] = bridgedBuffer.value(at: position)
            unmanagedKeys[i] = bridgedBuffer.key(at: position)
            i += 1
            guard i < count else { break }
          }
        }
      } else {
        // keys nonnull, objects null
        for position in 0..<bucketCount {
          if bridgedBuffer.isInitializedEntry(at: position) {
            unmanagedKeys[i] = bridgedBuffer.key(at: position)
            i += 1
            guard i < count else { break }
          }
        }
      }
    } else {
      if let unmanagedObjects = _UnmanagedAnyObjectArray(objects) {
        // keys null, objects nonnull
        for position in 0..<bucketCount {
          if bridgedBuffer.isInitializedEntry(at: position) {
            unmanagedObjects[i] = bridgedBuffer.value(at: position)
            i += 1
            guard i < count else { break }
          }
        }
      } else {
        // do nothing, both are null
      }
    }
  }

  @objc(enumerateKeysAndObjectsWithOptions:usingBlock:)
  internal func enumerateKeysAndObjects(options: Int,
    using block: @convention(block) (Unmanaged<AnyObject>, Unmanaged<AnyObject>,
     UnsafeMutablePointer<UInt8>) -> Void) {
    bridgeEverything()
    let bucketCount = nativeBuffer.bucketCount
    var stop: UInt8 = 0
    for position in 0..<bucketCount {
      if bridgedBuffer.isInitializedEntry(at: position) {
        block(Unmanaged.passUnretained(bridgedBuffer.key(at: position)),
          Unmanaged.passUnretained(bridgedBuffer.value(at: position)),
          &stop)
      }
      if stop != 0 { return }
    }
  }

  /// Returns the pointer to the stored property, which contains bridged
  /// Dictionary elements.
  @nonobjc
  internal var _heapStorageBridgedPtr: UnsafeMutablePointer<AnyObject?> {
    return _getUnsafePointerToStoredProperties(self).assumingMemoryBound(
      to: Optional<AnyObject>.self)
  }

  /// The buffer for bridged Dictionary elements, if present.
  @nonobjc
  internal var _bridgedStorage: _RawNativeDictionaryStorage? {
    get {
      if let ref = _stdlib_atomicLoadARCRef(object: _heapStorageBridgedPtr) {
        return unsafeDowncast(ref, to: _RawNativeDictionaryStorage.self)
      }
      return nil
    }
  }

  /// Attach a buffer for bridged Dictionary elements.
  @nonobjc
  internal func _initializeHeapStorageBridged(_ newStorage: AnyObject) {
    _stdlib_atomicInitializeARCRef(
      object: _heapStorageBridgedPtr, desired: newStorage)
  }

  /// Returns the bridged Dictionary values.
  internal var bridgedBuffer: _NativeDictionaryBuffer<AnyObject, AnyObject> {
    return _NativeDictionaryBuffer(_storage: _bridgedStorage!)
  }

  @nonobjc
  internal func bridgeEverything() {
    if _fastPath(_bridgedStorage != nil) {
      return
    }

    // FIXME: rdar://problem/19486139 (split bridged buffers for keys and values)
    // We bridge keys and values unconditionally here, even if one of them
    // actually is verbatim bridgeable (e.g. Dictionary<Int, AnyObject>).
    // Investigate only allocating the buffer for a Set in this case.

    // Create buffer for bridged data.
    let bridged = _NativeDictionaryBuffer<AnyObject, AnyObject>(
      _exactBucketCount: nativeBuffer.bucketCount,
      unhashable: ())

    // Bridge everything.
    for i in 0..<nativeBuffer.bucketCount {
      if nativeBuffer.isInitializedEntry(at: i) {
        let key = _bridgeAnythingToObjectiveC(nativeBuffer.key(at: i))
        let val = _bridgeAnythingToObjectiveC(nativeBuffer.value(at: i))
        bridged.initializeKey(key, value: val, at: i)
      }
    }

    // Atomically put the bridged elements in place.
    _initializeHeapStorageBridged(bridged._storage)
  }

  @objc
  internal var count: Int {
    return nativeBuffer.count
  }

  @nonobjc
  internal func bridgingObjectForKey(_ aKey: AnyObject)
    -> AnyObject? {
    guard let nativeKey = _conditionallyBridgeFromObjectiveC(aKey, Key.self)
    else { return nil }

    let (i, found) = nativeBuffer._find(
      nativeKey, startBucket: nativeBuffer._bucket(nativeKey))
    if found {
      bridgeEverything()
      return bridgedBuffer.value(at: i.offset)
    }
    return nil
  }

  @objc
  internal func enumerator() -> _NSEnumerator {
    bridgeEverything()
    return _NativeDictionaryNSEnumerator<AnyObject, AnyObject>(bridgedBuffer)
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?,
    count: Int
  ) -> Int {
    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      theState.extra.0 = CUnsignedLong(nativeBuffer.startIndex.offset)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var currIndex = _NativeDictionaryIndex<Key, Value>(
        offset: Int(theState.extra.0))
    let endIndex = nativeBuffer.endIndex
    var stored = 0

    // Only need to bridge once, so we can hoist it out of the loop.
    if (currIndex != endIndex) {
      bridgeEverything()
    }

    for i in 0..<count {
      if (currIndex == endIndex) {
        break
      }

      let bridgedKey = bridgedBuffer.key(at: currIndex.offset)
      unmanagedObjects[i] = bridgedKey
      stored += 1
      nativeBuffer.formIndex(after: &currIndex)
    }
    theState.extra.0 = CUnsignedLong(currIndex.offset)
    state.pointee = theState
    return stored
  }
}
#else
final internal class _SwiftDeferredNSDictionary<Key: Hashable, Value> { }
#endif

#if _runtime(_ObjC)
@usableFromInline
@_fixed_layout
internal struct _CocoaDictionaryBuffer: _DictionaryBuffer {
  @usableFromInline
  internal var cocoaDictionary: _NSDictionary

  @inlinable // FIXME(sil-serialize-all)
  internal init(cocoaDictionary: _NSDictionary) {
    self.cocoaDictionary = cocoaDictionary
  }

  @usableFromInline
  internal typealias Index = _CocoaDictionaryIndex
  @usableFromInline
  internal typealias SequenceElement = (AnyObject, AnyObject)
  @usableFromInline
  internal typealias SequenceElementWithoutLabels = (AnyObject, AnyObject)

  @usableFromInline
  internal typealias Key = AnyObject
  @usableFromInline
  internal typealias Value = AnyObject

  @inlinable // FIXME(sil-serialize-all)
  internal var startIndex: Index {
    return Index(cocoaDictionary, startIndex: ())
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var endIndex: Index {
    return Index(cocoaDictionary, endIndex: ())
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(after i: Index) -> Index {
    return i.successor()
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func formIndex(after i: inout Index) {
    // FIXME: swift-3-indexing-model: optimize if possible.
    i = i.successor()
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(forKey key: Key) -> Index? {
    // Fast path that does not involve creating an array of all keys.  In case
    // the key is present, this lookup is a penalty for the slow path, but the
    // potential savings are significant: we could skip a memory allocation and
    // a linear search.
    if maybeGet(key) == nil {
      return nil
    }

    let allKeys = _stdlib_NSDictionary_allKeys(cocoaDictionary)
    var keyIndex = -1
    for i in 0..<allKeys.value {
      if _stdlib_NSObject_isEqual(key, allKeys[i]) {
        keyIndex = i
        break
      }
    }
    _sanityCheck(keyIndex >= 0,
        "Key was found in fast path, but not found later?")
    return Index(cocoaDictionary, allKeys, keyIndex)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var count: Int {
    return cocoaDictionary.count
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func assertingGet(at i: Index) -> SequenceElement {
    let key: Key = i.allKeys[i.currentKeyIndex]
    let value: Value = i.cocoaDictionary.objectFor(key)!
    return (key, value)

  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func maybeGet(_ key: Key) -> Value? {
    return cocoaDictionary.objectFor(key)
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func updateValue(_ value: Value, forKey key: Key) -> Value? {
    _sanityCheckFailure("cannot mutate NSDictionary")
  }

  @usableFromInline
  internal func _toNative<K: Hashable, V>(
    bucketCount: Int
  ) -> _NativeDictionaryBuffer<K, V> {
    let cocoaDictionary = self.cocoaDictionary
    var newNativeBuffer =
      _NativeDictionaryBuffer<K, V>(bucketCount: bucketCount)
    let oldCocoaIterator = _CocoaDictionaryIterator(cocoaDictionary)
    while let (key, value) = oldCocoaIterator.next() {
      newNativeBuffer.unsafeAddNew(
        key: _forceBridgeFromObjectiveC(key, K.self),
        value: _forceBridgeFromObjectiveC(value, V.self))
    }
    newNativeBuffer.count = cocoaDictionary.count
    return newNativeBuffer
  }
}
#endif

@usableFromInline
@_frozen
internal enum _VariantDictionaryBuffer<Key: Hashable, Value>
  : _DictionaryBuffer {

  @usableFromInline
  internal typealias NativeBuffer = _NativeDictionaryBuffer<Key, Value>
  @usableFromInline
  internal typealias NativeIndex = _NativeDictionaryIndex<Key, Value>
#if _runtime(_ObjC)
  @usableFromInline
  internal typealias CocoaBuffer = _CocoaDictionaryBuffer
#endif
  @usableFromInline
  internal typealias SequenceElement = (key: Key, value: Value)
  @usableFromInline
  internal typealias SequenceElementWithoutLabels = (key: Key, value: Value)
  @usableFromInline
  internal typealias SelfType = _VariantDictionaryBuffer

  case native(NativeBuffer)
#if _runtime(_ObjC)
  case cocoa(CocoaBuffer)
#endif

  @usableFromInline @_transparent
  internal var guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 || _canBeClass(Value.self) == 0
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func isUniquelyReferenced() -> Bool {
    // Note that &self drills down through .native(NativeBuffer) to the first
    // property in NativeBuffer, which is the reference to the storage.
    if _fastPath(guaranteedNative) {
      return _isUnique_native(&self)
    }

    switch self {
    case .native:
      return _isUnique_native(&self)
#if _runtime(_ObjC)
    case .cocoa:
      // Don't consider Cocoa buffer mutable, even if it is mutable and is
      // uniquely referenced.
      return false
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var asNative: NativeBuffer {
    get {
      switch self {
      case .native(let buffer):
        return buffer
#if _runtime(_ObjC)
      case .cocoa:
        _sanityCheckFailure("internal error: not backed by native buffer")
#endif
      }
    }
    set {
      self = .native(newValue)
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureNativeBuffer() {
#if _runtime(_ObjC)
    if _fastPath(guaranteedNative) { return }
    if case .cocoa(let cocoaBuffer) = self {
      migrateDataToNativeBuffer(cocoaBuffer)
    }
#endif
  }

#if _runtime(_ObjC)
  @inlinable // FIXME(sil-serialize-all)
  internal var asCocoa: CocoaBuffer {
    switch self {
    case .native:
      _sanityCheckFailure("internal error: not backed by NSDictionary")
    case .cocoa(let cocoaBuffer):
      return cocoaBuffer
    }
  }
#endif

  /// Return true if self is native.
  @inlinable // FIXME(sil-serialize-all)
  internal var _isNative: Bool {
#if _runtime(_ObjC)
    switch self {
    case .native:
      return true
    case .cocoa:
      return false
    }
#else
    return true
#endif
  }

  @inline(__always)
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureUniqueNativeBufferNative(
    withBucketCount desiredBucketCount: Int
  ) -> (reallocated: Bool, capacityChanged: Bool) {
    let oldBucketCount = asNative.bucketCount
    if oldBucketCount >= desiredBucketCount && isUniquelyReferenced() {
      return (reallocated: false, capacityChanged: false)
    }

    let oldNativeBuffer = asNative
    var newNativeBuffer = NativeBuffer(bucketCount: desiredBucketCount)
    let newBucketCount = newNativeBuffer.bucketCount
    for i in 0..<oldBucketCount {
      if oldNativeBuffer.isInitializedEntry(at: i) {
        if oldBucketCount == newBucketCount {
          let key = oldNativeBuffer.key(at: i)
          let value = oldNativeBuffer.value(at: i)
          newNativeBuffer.initializeKey(key, value: value , at: i)
        } else {
          let key = oldNativeBuffer.key(at: i)
          newNativeBuffer.unsafeAddNew(
            key: key,
            value: oldNativeBuffer.value(at: i))
        }
      }
    }
    newNativeBuffer.count = oldNativeBuffer.count

    self = .native(newNativeBuffer)
    return (reallocated: true,
      capacityChanged: oldBucketCount != newBucketCount)
  }

  @inline(__always)
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureUniqueNativeBuffer(
    withCapacity minimumCapacity: Int
  ) -> (reallocated: Bool, capacityChanged: Bool) {
    let bucketCount = NativeBuffer.bucketCount(
      forCapacity: minimumCapacity,
      maxLoadFactorInverse: _hashContainerDefaultMaxLoadFactorInverse)
    return ensureUniqueNativeBuffer(withBucketCount: bucketCount)
  }

  /// Ensure this we hold a unique reference to a native buffer
  /// having at least `minimumCapacity` elements.
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureUniqueNativeBuffer(
    withBucketCount desiredBucketCount: Int
  ) -> (reallocated: Bool, capacityChanged: Bool) {
#if _runtime(_ObjC)
    // This is a performance optimization that was put in to ensure that we did
    // not make a copy of self to call _isNative over the entire if region
    // causing at -Onone the uniqueness check to fail. This code used to be:
    //
    //  if _isNative {
    //    return ensureUniqueNativeBufferNative(
    //      withBucketCount: desiredBucketCount)
    //  }
    //
    // SR-6437
    let n = _isNative
    if n {
      return ensureUniqueNativeBufferNative(withBucketCount: desiredBucketCount)
    }

    switch self {
    case .native:
      fatalError("This should have been handled earlier")
    case .cocoa(let cocoaBuffer):
      self = .native(cocoaBuffer._toNative(bucketCount: desiredBucketCount))
      return (reallocated: true, capacityChanged: true)
    }
#else
    return ensureUniqueNativeBufferNative(withBucketCount: desiredBucketCount)
#endif
  }

#if _runtime(_ObjC)
  @inline(never)
  @usableFromInline
  internal mutating func migrateDataToNativeBuffer(
    _ cocoaBuffer: _CocoaDictionaryBuffer
  ) {
    let allocated = ensureUniqueNativeBuffer(
      withCapacity: cocoaBuffer.count).reallocated
    _sanityCheck(allocated, "failed to allocate native Dictionary buffer")
  }
#endif

  /// Reserves enough space for the specified number of elements to be stored
  /// without reallocating additional storage.
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func reserveCapacity(_ capacity: Int) {
    _ = ensureUniqueNativeBuffer(withCapacity: capacity)
  }

  /// The number of elements that can be stored without expanding the current
  /// storage.
  ///
  /// For bridged storage, this is equal to the current count of the
  /// collection, since any addition will trigger a copy of the elements into
  /// newly allocated storage. For native storage, this is the element count
  /// at which adding any more elements will exceed the load factor.
  @inlinable // FIXME(sil-serialize-all)
  internal var capacity: Int {
    switch self {
    case .native:
      return Int(Double(asNative.bucketCount) /
        _hashContainerDefaultMaxLoadFactorInverse)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return cocoaBuffer.count
#endif
    }
  }

  //
  // _DictionaryBuffer conformance
  //

  @usableFromInline
  internal typealias Index = DictionaryIndex<Key, Value>

  @inlinable // FIXME(sil-serialize-all)
  internal var startIndex: Index {
    if _fastPath(guaranteedNative) {
      return ._native(asNative.startIndex)
    }

    switch self {
    case .native:
      return ._native(asNative.startIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return ._cocoa(cocoaBuffer.startIndex)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var endIndex: Index {
    if _fastPath(guaranteedNative) {
      return ._native(asNative.endIndex)
    }

    switch self {
    case .native:
      return ._native(asNative.endIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return ._cocoa(cocoaBuffer.endIndex)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(after i: Index) -> Index {
    if _fastPath(guaranteedNative) {
      return ._native(asNative.index(after: i._nativeIndex))
    }

    switch self {
    case .native:
      return ._native(asNative.index(after: i._nativeIndex))
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return ._cocoa(cocoaBuffer.index(after: i._cocoaIndex))
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func formIndex(after i: inout Index) {
    // FIXME: swift-3-indexing-model: optimize if possible.
    i = index(after: i)
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func index(forKey key: Key) -> Index? {
    if _fastPath(guaranteedNative) {
      if let nativeIndex = asNative.index(forKey: key) {
        return ._native(nativeIndex)
      }
      return nil
    }

    switch self {
    case .native:
      if let nativeIndex = asNative.index(forKey: key) {
        return ._native(nativeIndex)
      }
      return nil
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      let anyObjectKey: AnyObject = _bridgeAnythingToObjectiveC(key)
      if let cocoaIndex = cocoaBuffer.index(forKey: anyObjectKey) {
        return ._cocoa(cocoaIndex)
      }
      return nil
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func containsKey(_ key: Key) -> Bool {
    if _fastPath(guaranteedNative) {
      return asNative.index(forKey: key) != nil
    }

    switch self {
    case .native:
      return asNative.index(forKey: key) != nil
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return SelfType.maybeGetFromCocoaBuffer(cocoaBuffer, forKey: key) != nil
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func assertingGet(at i: Index) -> SequenceElement {
    if _fastPath(guaranteedNative) {
      return asNative.assertingGet(at: i._nativeIndex)
    }

    switch self {
    case .native:
      return asNative.assertingGet(at: i._nativeIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      let (anyObjectKey, anyObjectValue) =
        cocoaBuffer.assertingGet(at: i._cocoaIndex)
      let nativeKey = _forceBridgeFromObjectiveC(anyObjectKey, Key.self)
      let nativeValue = _forceBridgeFromObjectiveC(anyObjectValue, Value.self)
      return (nativeKey, nativeValue)
#endif
    }
  }

#if _runtime(_ObjC)
  @inline(never)
  @usableFromInline
  internal static func maybeGetFromCocoaBuffer(
    _ cocoaBuffer: CocoaBuffer, forKey key: Key
  ) -> Value? {
    let anyObjectKey: AnyObject = _bridgeAnythingToObjectiveC(key)
    if let anyObjectValue = cocoaBuffer.maybeGet(anyObjectKey) {
      return _forceBridgeFromObjectiveC(anyObjectValue, Value.self)
    }
    return nil
  }
#endif

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func maybeGet(_ key: Key) -> Value? {
    if _fastPath(guaranteedNative) {
      return asNative.maybeGet(key)
    }

    switch self {
    case .native:
      return asNative.maybeGet(key)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return SelfType.maybeGetFromCocoaBuffer(cocoaBuffer, forKey: key)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeUpdateValue(
    _ value: Value, forKey key: Key
  ) -> Value? {
    var (i, found) = asNative._find(key, startBucket: asNative._bucket(key))

    let minBuckets = found
      ? asNative.bucketCount
      : NativeBuffer.bucketCount(
          forCapacity: asNative.count + 1,
          maxLoadFactorInverse: _hashContainerDefaultMaxLoadFactorInverse)

    let (_, capacityChanged) = ensureUniqueNativeBuffer(
      withBucketCount: minBuckets)
    if capacityChanged {
      i = asNative._find(key, startBucket: asNative._bucket(key)).pos
    }

    let oldValue: Value? = found ? asNative.value(at: i.offset) : nil
    if found {
      asNative.setKey(key, value: value, at: i.offset)
    } else {
      asNative.initializeKey(key, value: value, at: i.offset)
      asNative.count += 1
    }

    return oldValue
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func updateValue(
    _ value: Value, forKey key: Key
  ) -> Value? {

    if _fastPath(guaranteedNative) {
      return nativeUpdateValue(value, forKey: key)
    }

    switch self {
    case .native:
      return nativeUpdateValue(value, forKey: key)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      migrateDataToNativeBuffer(cocoaBuffer)
      return nativeUpdateValue(value, forKey: key)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativePointerToValue(at i: Index)
  -> UnsafeMutablePointer<Value> {
    // This is a performance optimization that was put in to ensure that we did
    // not make a copy of self to call asNative.bucketCount over
    // ensureUniqueNativeBefore causing at -Onone the uniqueness check to
    // fail. This code used to be:
    //
    // _ = ensureUniqueNativeBuffer(withBucketCount: bucketCount)
    //
    // SR-6437
    let bucketCount = asNative.bucketCount
    _ = ensureUniqueNativeBuffer(withBucketCount: bucketCount)
    return asNative.values + i._nativeIndex.offset
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func pointerToValue(at i: Index)
    -> UnsafeMutablePointer<Value> {
    if _fastPath(guaranteedNative) {
      return nativePointerToValue(at: i)
    }

    switch self {
    case .native:
      return nativePointerToValue(at: i)
#if _runtime(_ObjC)
    case .cocoa(let cocoaStorage):
      // We have to migrate the data to native storage before we can return a
      // mutable pointer. But after we migrate, the Cocoa index becomes
      // useless, so get the key first.
      let cocoaIndex = i._cocoaIndex
      let anyObjectKey: AnyObject =
        cocoaIndex.allKeys[cocoaIndex.currentKeyIndex]
      migrateDataToNativeBuffer(cocoaStorage)
      let key = _forceBridgeFromObjectiveC(anyObjectKey, Key.self)
      let nativeIndex = asNative.index(forKey: key)!

      return nativePointerToValue(at: ._native(nativeIndex))
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativePointerToValue(
    forKey key: Key, insertingDefault defaultValue: () -> Value
  ) -> (inserted: Bool, pointer: UnsafeMutablePointer<Value>) {

    var (i, found) = asNative._find(key, startBucket: asNative._bucket(key))
    if found {
      let pointer = nativePointerToValue(at: ._native(i))
      return (inserted: false, pointer: pointer)
    }

    let minCapacity = asNative.count + 1
    let (_, capacityChanged) = ensureUniqueNativeBuffer(
      withCapacity: minCapacity)

    if capacityChanged {
      i = asNative._find(key, startBucket: asNative._bucket(key)).pos
    }

    asNative.initializeKey(key, value: defaultValue(), at: i.offset)
    asNative.count += 1
    return (inserted: true, pointer: asNative.values + i.offset)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func pointerToValue(
    forKey key: Key, insertingDefault defaultValue: () -> Value
  ) -> (inserted: Bool, pointer: UnsafeMutablePointer<Value>) {
    ensureNativeBuffer()
    return nativePointerToValue(forKey: key, insertingDefault: defaultValue)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeInsert(
    _ value: Value, forKey key: Key
  ) -> (inserted: Bool, memberAfterInsert: Value) {

    var (i, found) = asNative._find(key, startBucket: asNative._bucket(key))
    if found {
      return (inserted: false, memberAfterInsert: asNative.value(at: i.offset))
    }

    let minCapacity = asNative.count + 1
    let (_, capacityChanged) = ensureUniqueNativeBuffer(
      withCapacity: minCapacity)

    if capacityChanged {
      i = asNative._find(key, startBucket: asNative._bucket(key)).pos
    }

    asNative.initializeKey(key, value: value, at: i.offset)
    asNative.count += 1

    return (inserted: true, memberAfterInsert: value)
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func insert(
    _ value: Value, forKey key: Key
  ) -> (inserted: Bool, memberAfterInsert: Value) {
    ensureNativeBuffer()
    return nativeInsert(value, forKey: key)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func nativeMapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _VariantDictionaryBuffer<Key, T> {
    var buffer = _NativeDictionaryBuffer<Key, T>(
      _exactBucketCount: asNative.bucketCount)

    // Because the keys in the current and new buffer are the same, we can
    // initialize to the same locations in the new buffer, skipping hash value
    // recalculations.
    var i = asNative.startIndex
    while i != asNative.endIndex {
      let (k, v) = asNative.assertingGet(at: i)
      try buffer.initializeKey(k, value: transform(v), at: i.offset)
      asNative.formIndex(after: &i)
    }
    buffer.count = asNative.count

    return .native(buffer)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _VariantDictionaryBuffer<Key, T> {
    if _fastPath(guaranteedNative) {
      return try nativeMapValues(transform)
    }

    switch self {
    case .native:
      return try nativeMapValues(transform)
#if _runtime(_ObjC)
    case .cocoa(let cocoaStorage):
      var storage: _VariantDictionaryBuffer<Key, T> = .native(
        _NativeDictionaryBuffer<Key, T>(minimumCapacity: cocoaStorage.count))

      var i = cocoaStorage.startIndex
      while i != cocoaStorage.endIndex {
        let (anyObjectKey, anyObjectValue) = cocoaStorage.assertingGet(at: i)
        let nativeKey = _forceBridgeFromObjectiveC(anyObjectKey, Key.self)
        let nativeValue = _forceBridgeFromObjectiveC(anyObjectValue, Value.self)
        _ = try storage.nativeInsert(transform(nativeValue), forKey: nativeKey)
        cocoaStorage.formIndex(after: &i)
      }

      return storage
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeMerge<S: Sequence>(
    _ keysAndValues: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    for (key, value) in keysAndValues {
      var (i, found) = asNative._find(key, startBucket: asNative._bucket(key))

      if found {
        // This is a performance optimization that was put in to ensure that we
        // did not make a copy of self to call asNative.bucketCount over
        // ensureUniqueNativeBefore causing at -Onone the uniqueness check to
        // fail. This code used to be:
        //
        // _ = ensureUniqueNativeBuffer(withBucketCount: asNative.bucketCount)
        //
        // SR-6437
        let bucketCount = asNative.bucketCount
        _ = ensureUniqueNativeBuffer(withBucketCount: bucketCount)
        do {
          let newValue = try combine(asNative.value(at: i.offset), value)
          asNative.setKey(key, value: newValue, at: i.offset)
        } catch _MergeError.keyCollision {
          fatalError("Duplicate values for key: '\(key)'")
        }
      } else {
        let minCapacity = asNative.count + 1
        let (_, capacityChanged) = ensureUniqueNativeBuffer(
          withCapacity: minCapacity)
        if capacityChanged {
          i = asNative._find(key, startBucket: asNative._bucket(key)).pos
        }

        asNative.initializeKey(key, value: value, at: i.offset)
        asNative.count += 1
      }
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func merge<S: Sequence>(
    _ keysAndValues: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    ensureNativeBuffer()
    try nativeMerge(keysAndValues, uniquingKeysWith: combine)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeGroup<S: Sequence>(
    _ values: S,
    by keyForValue: (S.Element) throws -> Key
  ) rethrows where Value == [S.Element] {
    defer { _fixLifetime(asNative) }
    for value in values {
      let key = try keyForValue(value)
      var (i, found) = asNative._find(key, startBucket: asNative._bucket(key))
      if found {
        asNative.values[i.offset].append(value)
      } else {
        let minCapacity = asNative.count + 1
        let (_, capacityChanged) = ensureUniqueNativeBuffer(
          withCapacity: minCapacity)
        if capacityChanged {
          i = asNative._find(key, startBucket: asNative._bucket(key)).pos
        }

        asNative.initializeKey(key, value: [value], at: i.offset)
        asNative.count += 1
      }
    }
  }

  /// - parameter idealBucket: The ideal bucket for the element being deleted.
  /// - parameter offset: The offset of the element that will be deleted.
  /// Precondition: there should be an initialized entry at offset.
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeDelete(
    _ nativeBuffer: NativeBuffer, idealBucket: Int, offset: Int
  ) {
    _sanityCheck(
        nativeBuffer.isInitializedEntry(at: offset), "expected initialized entry")

    var nativeBuffer = nativeBuffer

    // remove the element
    nativeBuffer.destroyEntry(at: offset)
    nativeBuffer.count -= 1

    // If we've put a hole in a chain of contiguous elements, some
    // element after the hole may belong where the new hole is.
    var hole = offset

    // Find the first bucket in the contiguous chain
    var start = idealBucket
    while nativeBuffer.isInitializedEntry(at: nativeBuffer._prev(start)) {
      start = nativeBuffer._prev(start)
    }

    // Find the last bucket in the contiguous chain
    var lastInChain = hole
    var b = nativeBuffer._index(after: lastInChain)
    while nativeBuffer.isInitializedEntry(at: b) {
      lastInChain = b
      b = nativeBuffer._index(after: b)
    }

    // Relocate out-of-place elements in the chain, repeating until
    // none are found.
    while hole != lastInChain {
      // Walk backwards from the end of the chain looking for
      // something out-of-place.
      var b = lastInChain
      while b != hole {
        let idealBucket = nativeBuffer._bucket(nativeBuffer.key(at: b))

        // Does this element belong between start and hole?  We need
        // two separate tests depending on whether [start, hole] wraps
        // around the end of the storage
        let c0 = idealBucket >= start
        let c1 = idealBucket <= hole
        if start <= hole ? (c0 && c1) : (c0 || c1) {
          break // Found it
        }
        b = nativeBuffer._prev(b)
      }

      if b == hole { // No out-of-place elements found; we're done adjusting
        break
      }

      // Move the found element into the hole
      nativeBuffer.moveInitializeEntry(
        from: nativeBuffer,
        at: b,
        toEntryAt: hole)
      hole = b
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeRemoveObject(forKey key: Key) -> Value? {
    var idealBucket = asNative._bucket(key)
    var (index, found) = asNative._find(key, startBucket: idealBucket)

    // Fast path: if the key is not present, we will not mutate the set,
    // so don't force unique buffer.
    if !found {
      return nil
    }

    // This is a performance optimization that was put in to ensure that we
    // did not make a copy of self to call asNative.bucketCount over
    // ensureUniqueNativeBefore causing at -Onone the uniqueness check to
    // fail. This code used to be:
    //
    // ... = ensureUniqueNativeBuffer(withBucketCount: asNative.bucketCount)
    //
    // SR-6437
    let bucketCount = asNative.bucketCount
    let (_, capacityChanged) = ensureUniqueNativeBuffer(
      withBucketCount: bucketCount)
    let nativeBuffer = asNative
    if capacityChanged {
      idealBucket = nativeBuffer._bucket(key)
      (index, found) = nativeBuffer._find(key, startBucket: idealBucket)
      _sanityCheck(found, "key was lost during buffer migration")
    }
    let oldValue = nativeBuffer.value(at: index.offset)
    nativeDelete(nativeBuffer, idealBucket: idealBucket,
      offset: index.offset)
    return oldValue
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeRemove(
    at nativeIndex: NativeIndex
  ) -> SequenceElement {
    // This is a performance optimization that was put in to ensure that we did
    // not make a copy of self to call asNative.bucketCount over
    // ensureUniqueNativeBefore causing at -Onone the uniqueness check to
    // fail. This code used to be:
    //
    // _ = ensureUniqueNativeBuffer(withBucketCount: asNative.bucketCount)
    //
    // SR-6437
    let bucketCount = asNative.bucketCount
    // The provided index should be valid, so we will always mutating the
    // set buffer.  Request unique buffer.
    _ = ensureUniqueNativeBuffer(withBucketCount: bucketCount)
    let nativeBuffer = asNative

    let result = nativeBuffer.assertingGet(at: nativeIndex)
    let key = result.0

    nativeDelete(nativeBuffer, idealBucket: nativeBuffer._bucket(key),
        offset: nativeIndex.offset)
    return result
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func remove(at index: Index) -> SequenceElement {
    if _fastPath(guaranteedNative) {
      return nativeRemove(at: index._nativeIndex)
    }

    switch self {
    case .native:
      return nativeRemove(at: index._nativeIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      // We have to migrate the data first.  But after we do so, the Cocoa
      // index becomes useless, so get the key first.
      //
      // FIXME(performance): fuse data migration and element deletion into one
      // operation.
      let index = index._cocoaIndex
      let anyObjectKey: AnyObject = index.allKeys[index.currentKeyIndex]
      migrateDataToNativeBuffer(cocoaBuffer)
      let key = _forceBridgeFromObjectiveC(anyObjectKey, Key.self)
      let value = nativeRemoveObject(forKey: key)

      return (key, value._unsafelyUnwrappedUnchecked)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func removeValue(forKey key: Key) -> Value? {
    if _fastPath(guaranteedNative) {
      return nativeRemoveObject(forKey: key)
    }

    switch self {
    case .native:
      return nativeRemoveObject(forKey: key)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      let anyObjectKey: AnyObject = _bridgeAnythingToObjectiveC(key)
      if cocoaBuffer.maybeGet(anyObjectKey) == nil {
        return nil
      }
      migrateDataToNativeBuffer(cocoaBuffer)
      return nativeRemoveObject(forKey: key)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeRemoveAll() {
    if !isUniquelyReferenced() {
        asNative = NativeBuffer(_exactBucketCount: asNative.bucketCount)
        return
    }

    // We have already checked for the empty dictionary case and unique
    // reference, so we will always mutate the dictionary buffer.
    var nativeBuffer = asNative

    for b in 0..<nativeBuffer.bucketCount {
      if nativeBuffer.isInitializedEntry(at: b) {
        nativeBuffer.destroyEntry(at: b)
      }
    }
    nativeBuffer.count = 0
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    if count == 0 {
      return
    }

    if !keepCapacity {
      self = .native(NativeBuffer(bucketCount: 2))
      return
    }

    if _fastPath(guaranteedNative) {
      nativeRemoveAll()
      return
    }

    switch self {
    case .native:
      nativeRemoveAll()
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      self = .native(NativeBuffer(minimumCapacity: cocoaBuffer.count))
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var count: Int {
    if _fastPath(guaranteedNative) {
      return asNative.count
    }

    switch self {
    case .native:
      return asNative.count
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return cocoaBuffer.count
#endif
    }
  }

  /// Returns an iterator over the `(Key, Value)` pairs.
  ///
  /// - Complexity: O(1).
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func makeIterator() -> DictionaryIterator<Key, Value> {
    switch self {
    case .native(let buffer):
      return ._native(
        start: asNative.startIndex, end: asNative.endIndex, buffer: buffer)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      return ._cocoa(cocoaBuffer)
#endif
    }
  }
}

@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
internal struct _NativeDictionaryIndex<Key, Value>: Comparable {
  @usableFromInline
  internal var offset: Int

  @inlinable // FIXME(sil-serialize-all)
  internal init(offset: Int) {
    self.offset = offset
  }
}

extension _NativeDictionaryIndex {

  @inlinable // FIXME(sil-serialize-all)
  internal static func < (
    lhs: _NativeDictionaryIndex<Key, Value>,
    rhs: _NativeDictionaryIndex<Key, Value>
  ) -> Bool {
    return lhs.offset < rhs.offset
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func <= (
    lhs: _NativeDictionaryIndex<Key, Value>,
    rhs: _NativeDictionaryIndex<Key, Value>
  ) -> Bool {
    return lhs.offset <= rhs.offset
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func > (
    lhs: _NativeDictionaryIndex<Key, Value>,
    rhs: _NativeDictionaryIndex<Key, Value>
  ) -> Bool {
    return lhs.offset > rhs.offset
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func >= (
    lhs: _NativeDictionaryIndex<Key, Value>,
    rhs: _NativeDictionaryIndex<Key, Value>
  ) -> Bool {
    return lhs.offset >= rhs.offset
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func == (
    lhs: _NativeDictionaryIndex<Key, Value>,
    rhs: _NativeDictionaryIndex<Key, Value>
  ) -> Bool {
    return lhs.offset == rhs.offset
  }
}

#if _runtime(_ObjC)
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
internal struct _CocoaDictionaryIndex: Comparable {
  // Assumption: we rely on NSDictionary.getObjects when being
  // repeatedly called on the same NSDictionary, returning items in the same
  // order every time.
  // Similarly, the same assumption holds for NSSet.allObjects.

  /// A reference to the NSDictionary, which owns members in `allObjects`,
  /// or `allKeys`, for NSSet and NSDictionary respectively.
  @usableFromInline // FIXME(sil-serialize-all)
  internal let cocoaDictionary: _NSDictionary
  // FIXME: swift-3-indexing-model: try to remove the cocoa reference, but make
  // sure that we have a safety check for accessing `allKeys`.  Maybe move both
  // into the dictionary/set itself.

  /// An unowned array of keys.
  @usableFromInline // FIXME(sil-serialize-all)
  internal var allKeys: _HeapBuffer<Int, AnyObject>

  /// Index into `allKeys`
  @usableFromInline // FIXME(sil-serialize-all)
  internal var currentKeyIndex: Int

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ cocoaDictionary: _NSDictionary, startIndex: ()) {
    self.cocoaDictionary = cocoaDictionary
    self.allKeys = _stdlib_NSDictionary_allKeys(cocoaDictionary)
    self.currentKeyIndex = 0
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ cocoaDictionary: _NSDictionary, endIndex: ()) {
    self.cocoaDictionary = cocoaDictionary
    self.allKeys = _stdlib_NSDictionary_allKeys(cocoaDictionary)
    self.currentKeyIndex = allKeys.value
  }

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ cocoaDictionary: _NSDictionary,
    _ allKeys: _HeapBuffer<Int, AnyObject>,
    _ currentKeyIndex: Int
  ) {
    self.cocoaDictionary = cocoaDictionary
    self.allKeys = allKeys
    self.currentKeyIndex = currentKeyIndex
  }

  /// Returns the next consecutive value after `self`.
  ///
  /// - Precondition: The next value is representable.
  @inlinable // FIXME(sil-serialize-all)
  internal func successor() -> _CocoaDictionaryIndex {
    // FIXME: swift-3-indexing-model: remove this method.
    _precondition(
      currentKeyIndex < allKeys.value, "Cannot increment endIndex")
    return _CocoaDictionaryIndex(cocoaDictionary, allKeys, currentKeyIndex + 1)
  }
}

extension _CocoaDictionaryIndex {

  @inlinable // FIXME(sil-serialize-all)
  internal static func < (
    lhs: _CocoaDictionaryIndex,
    rhs: _CocoaDictionaryIndex
  ) -> Bool {
    return lhs.currentKeyIndex < rhs.currentKeyIndex
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func <= (
    lhs: _CocoaDictionaryIndex,
    rhs: _CocoaDictionaryIndex
  ) -> Bool {
    return lhs.currentKeyIndex <= rhs.currentKeyIndex
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func > (
    lhs: _CocoaDictionaryIndex,
    rhs: _CocoaDictionaryIndex
  ) -> Bool {
    return lhs.currentKeyIndex > rhs.currentKeyIndex
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func >= (
    lhs: _CocoaDictionaryIndex,
    rhs: _CocoaDictionaryIndex
  ) -> Bool {
    return lhs.currentKeyIndex >= rhs.currentKeyIndex
  }
  @inlinable // FIXME(sil-serialize-all)
  internal static func == (
    lhs: _CocoaDictionaryIndex,
    rhs: _CocoaDictionaryIndex
  ) -> Bool {
    return lhs.currentKeyIndex == rhs.currentKeyIndex
  }
}
#endif

@_frozen // FIXME(sil-serialize-all)
@usableFromInline // FIXME(sil-serialize-all)
internal enum DictionaryIndexRepresentation<Key: Hashable, Value> {
  @usableFromInline
  typealias _Index = DictionaryIndex<Key, Value>
  @usableFromInline
  typealias _NativeIndex = _Index._NativeIndex
#if _runtime(_ObjC)
  @usableFromInline
  typealias _CocoaIndex = _Index._CocoaIndex
#endif

  case _native(_NativeIndex)
#if _runtime(_ObjC)
  case _cocoa(_CocoaIndex)
#endif
}

extension Dictionary {
  /// The position of a key-value pair in a dictionary.
  ///
  /// Dictionary has two subscripting interfaces:
  ///
  /// 1. Subscripting with a key, yielding an optional value:
  ///
  ///        v = d[k]!
  ///
  /// 2. Subscripting with an index, yielding a key-value pair:
  ///
  ///        (k, v) = d[i]
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Index: Comparable, Hashable {
    // Index for native buffer is efficient.  Index for bridged NSDictionary is
    // not, because neither NSEnumerator nor fast enumeration support moving
    // backwards.  Even if they did, there is another issue: NSEnumerator does
    // not support NSCopying, and fast enumeration does not document that it is
    // safe to copy the state.  So, we cannot implement Index that is a value
    // type for bridged NSDictionary in terms of Cocoa enumeration facilities.

    @usableFromInline
    internal typealias _NativeIndex = _NativeDictionaryIndex<Key, Value>
#if _runtime(_ObjC)
    @usableFromInline
    internal typealias _CocoaIndex = _CocoaDictionaryIndex
#endif

    @inlinable // FIXME(sil-serialize-all)
    internal init(_value: DictionaryIndexRepresentation<Key, Value>) {
      self._value = _value
    }

    @usableFromInline // FIXME(sil-serialize-all)
    internal var _value: DictionaryIndexRepresentation<Key, Value>

    @inlinable // FIXME(sil-serialize-all)
    internal static func _native(_ index: _NativeIndex) -> Index {
      return DictionaryIndex(_value: ._native(index))
    }

#if _runtime(_ObjC)
    @inlinable // FIXME(sil-serialize-all)
    internal static func _cocoa(_ index: _CocoaIndex) -> Index {
      return DictionaryIndex(_value: ._cocoa(index))
    }
#endif

    @usableFromInline @_transparent
    internal var _guaranteedNative: Bool {
      return _canBeClass(Key.self) == 0 && _canBeClass(Value.self) == 0
    }

    @usableFromInline @_transparent
    internal var _nativeIndex: _NativeIndex {
      switch _value {
      case ._native(let nativeIndex):
        return nativeIndex
#if _runtime(_ObjC)
      case ._cocoa:
        _sanityCheckFailure("internal error: does not contain a native index")
#endif
      }
    }

#if _runtime(_ObjC)
    @usableFromInline @_transparent
    internal var _cocoaIndex: _CocoaIndex {
      switch _value {
      case ._native:
        _sanityCheckFailure("internal error: does not contain a Cocoa index")
      case ._cocoa(let cocoaIndex):
        return cocoaIndex
      }
    }
#endif
  }
}

public typealias DictionaryIndex<Key: Hashable, Value> =
  Dictionary<Key, Value>.Index

extension Dictionary.Index {
  @inlinable // FIXME(sil-serialize-all)
  public static func == (
    lhs: Dictionary<Key, Value>.Index,
    rhs: Dictionary<Key, Value>.Index
  ) -> Bool {
    if _fastPath(lhs._guaranteedNative) {
      return lhs._nativeIndex == rhs._nativeIndex
    }

    switch (lhs._value, rhs._value) {
    case (._native(let lhsNative), ._native(let rhsNative)):
      return lhsNative == rhsNative
  #if _runtime(_ObjC)
    case (._cocoa(let lhsCocoa), ._cocoa(let rhsCocoa)):
      return lhsCocoa == rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different sets")
  #endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func < (
    lhs: Dictionary<Key, Value>.Index,
    rhs: Dictionary<Key, Value>.Index
  ) -> Bool {
    if _fastPath(lhs._guaranteedNative) {
      return lhs._nativeIndex < rhs._nativeIndex
    }

    switch (lhs._value, rhs._value) {
    case (._native(let lhsNative), ._native(let rhsNative)):
      return lhsNative < rhsNative
  #if _runtime(_ObjC)
    case (._cocoa(let lhsCocoa), ._cocoa(let rhsCocoa)):
      return lhsCocoa < rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different sets")
  #endif
    }
  }

  @inlinable
  public func hash(into hasher: inout Hasher) {
  #if _runtime(_ObjC)
    if _fastPath(_guaranteedNative) {
      hasher.combine(0 as UInt8)
      hasher.combine(_nativeIndex.offset)
      return
    }
    switch _value {
    case ._native(let nativeIndex):
      hasher.combine(0 as UInt8)
      hasher.combine(nativeIndex.offset)
    case ._cocoa(let cocoaIndex):
      hasher.combine(1 as UInt8)
      hasher.combine(cocoaIndex.currentKeyIndex)
    }
  #else
    hasher.combine(_nativeIndex.offset)
  #endif
  }
}

#if _runtime(_ObjC)
@usableFromInline
final internal class _CocoaDictionaryIterator: IteratorProtocol {
  internal typealias Element = (AnyObject, AnyObject)

  // Cocoa Dictionary iterator has to be a class, otherwise we cannot
  // guarantee that the fast enumeration struct is pinned to a certain memory
  // location.

  // This stored property should be stored at offset zero.  There's code below
  // relying on this.
  internal var _fastEnumerationState: _SwiftNSFastEnumerationState =
    _makeSwiftNSFastEnumerationState()

  // This stored property should be stored right after `_fastEnumerationState`.
  // There's code below relying on this.
  internal var _fastEnumerationStackBuf = _CocoaFastEnumerationStackBuf()

  internal let cocoaDictionary: _NSDictionary

  internal var _fastEnumerationStatePtr:
    UnsafeMutablePointer<_SwiftNSFastEnumerationState> {
    return _getUnsafePointerToStoredProperties(self).assumingMemoryBound(
      to: _SwiftNSFastEnumerationState.self)
  }

  internal var _fastEnumerationStackBufPtr:
    UnsafeMutablePointer<_CocoaFastEnumerationStackBuf> {
    return UnsafeMutableRawPointer(_fastEnumerationStatePtr + 1)
      .assumingMemoryBound(to: _CocoaFastEnumerationStackBuf.self)
  }

  // These members have to be word-sized integers, they cannot be limited to
  // Int8 just because our storage holds 16 elements: fast enumeration is
  // allowed to return inner pointers to the container, which can be much
  // larger.
  internal var itemIndex: Int = 0
  internal var itemCount: Int = 0

  internal init(_ cocoaDictionary: _NSDictionary) {
    self.cocoaDictionary = cocoaDictionary
  }

  @usableFromInline
  internal func next() -> Element? {
    if itemIndex < 0 {
      return nil
    }
    let cocoaDictionary = self.cocoaDictionary
    if itemIndex == itemCount {
      let stackBufCount = _fastEnumerationStackBuf.count
      // We can't use `withUnsafeMutablePointer` here to get pointers to
      // properties, because doing so might introduce a writeback storage, but
      // fast enumeration relies on the pointer identity of the enumeration
      // state struct.
      itemCount = cocoaDictionary.countByEnumerating(
        with: _fastEnumerationStatePtr,
        objects: UnsafeMutableRawPointer(_fastEnumerationStackBufPtr)
          .assumingMemoryBound(to: AnyObject.self),
        count: stackBufCount)
      if itemCount == 0 {
        itemIndex = -1
        return nil
      }
      itemIndex = 0
    }
    let itemsPtrUP =
    UnsafeMutableRawPointer(_fastEnumerationState.itemsPtr!)
      .assumingMemoryBound(to: AnyObject.self)
    let itemsPtr = _UnmanagedAnyObjectArray(itemsPtrUP)
    let key: AnyObject = itemsPtr[itemIndex]
    itemIndex += 1
    let value: AnyObject = cocoaDictionary.objectFor(key)!
    return (key, value)
  }
}
#endif

@usableFromInline
@_frozen // FIXME(sil-serialize-all)
internal enum DictionaryIteratorRepresentation<Key: Hashable, Value> {
  @usableFromInline
  internal typealias _Iterator = DictionaryIterator<Key, Value>
  @usableFromInline
  internal typealias _NativeBuffer = _NativeDictionaryBuffer<Key, Value>
  @usableFromInline
  internal typealias _NativeIndex = _Iterator._NativeIndex

  // For native buffer, we keep two indices to keep track of the iteration
  // progress and the buffer owner to make the buffer non-uniquely
  // referenced.
  //
  // Iterator is iterating over a frozen view of the collection
  // state, so it should keep its own reference to the buffer.
  case _native(
    start: _NativeIndex, end: _NativeIndex, buffer: _NativeBuffer)
#if _runtime(_ObjC)
  case _cocoa(_CocoaDictionaryIterator)
#endif
}

/// An iterator over the members of a `Dictionary<Key, Value>`.
@_fixed_layout // FIXME(sil-serialize-all)
public struct DictionaryIterator<Key: Hashable, Value>: IteratorProtocol {
  // Dictionary has a separate IteratorProtocol and Index because of efficiency
  // and implementability reasons.
  //
  // Index for native buffer is efficient.  Index for bridged NSDictionary is
  // not.
  //
  // Even though fast enumeration is not suitable for implementing
  // Index, which is multi-pass, it is suitable for implementing a
  // IteratorProtocol, which is being consumed as iteration proceeds.

  @usableFromInline
  internal typealias _NativeBuffer = _NativeDictionaryBuffer<Key, Value>
  @usableFromInline
  internal typealias _NativeIndex = _NativeDictionaryIndex<Key, Value>

  @usableFromInline
  internal var _state: DictionaryIteratorRepresentation<Key, Value>

  @inlinable // FIXME(sil-serialize-all)
  internal init(_state: DictionaryIteratorRepresentation<Key, Value>) {
    self._state = _state
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func _native(
    start: _NativeIndex, end: _NativeIndex, buffer: _NativeBuffer
  ) -> DictionaryIterator {
    return DictionaryIterator(
      _state: ._native(start: start, end: end, buffer: buffer))
  }
#if _runtime(_ObjC)
  @usableFromInline
  internal static func _cocoa(
    _ buffer: _CocoaDictionaryBuffer
  ) -> DictionaryIterator{
    let iterator = _CocoaDictionaryIterator(buffer.cocoaDictionary)
    return DictionaryIterator(_state: ._cocoa(iterator))
  }
#endif

  @usableFromInline @_transparent
  internal var _guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 || _canBeClass(Value.self) == 0
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func _nativeNext() -> (key: Key, value: Value)? {
    switch _state {
    case ._native(let startIndex, let endIndex, let buffer):
      if startIndex == endIndex {
        return nil
      }
      let result = buffer.assertingGet(at: startIndex)
      _state =
        ._native(start: buffer.index(after: startIndex), end: endIndex, buffer: buffer)
      return result
#if _runtime(_ObjC)
    case ._cocoa:
      _sanityCheckFailure("internal error: not backed by NSDictionary")
#endif
    }
  }

  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Once `nil` has been returned, all subsequent calls return `nil`.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public mutating func next() -> (key: Key, value: Value)? {
    if _fastPath(_guaranteedNative) {
      return _nativeNext()
    }

    switch _state {
    case ._native:
      return _nativeNext()
#if _runtime(_ObjC)
    case ._cocoa(let cocoaIterator):
      if let (anyObjectKey, anyObjectValue) = cocoaIterator.next() {
        let nativeKey = _forceBridgeFromObjectiveC(anyObjectKey, Key.self)
        let nativeValue = _forceBridgeFromObjectiveC(anyObjectValue, Value.self)
        return (nativeKey, nativeValue)
      }
      return nil
#endif
    }
  }
}

extension DictionaryIterator: CustomReflectable {
  /// A mirror that reflects the iterator.
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: EmptyCollection<(label: String?, value: Any)>())
  }
}

extension Dictionary: CustomReflectable {
  /// A mirror that reflects the dictionary.
  public var customMirror: Mirror {
    let style = Mirror.DisplayStyle.dictionary
    return Mirror(self, unlabeledChildren: self, displayStyle: style)
  }
}

/// Initializes a `Dictionary` from unique members.
///
/// Using a builder can be faster than inserting members into an empty
/// `Dictionary`.
@_fixed_layout // FIXME(sil-serialize-all)
public struct _DictionaryBuilder<Key: Hashable, Value> {

  @usableFromInline // FIXME(sil-serialize-all)
  internal var _result: Dictionary<Key, Value>
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _nativeBuffer: _NativeDictionaryBuffer<Key, Value>
  @usableFromInline // FIXME(sil-serialize-all)
  internal let _requestedCount: Int
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _actualCount: Int

  @inlinable // FIXME(sil-serialize-all)
  public init(count: Int) {
    _result = Dictionary<Key, Value>(minimumCapacity: count)
    _nativeBuffer = _result._variantBuffer.asNative
    _requestedCount = count
    _actualCount = 0
  }

  @inlinable // FIXME(sil-serialize-all)
  public mutating func add(key newKey: Key, value: Value) {
    _nativeBuffer.unsafeAddNew(key: newKey, value: value)
    _actualCount += 1
  }

  @inlinable // FIXME(sil-serialize-all)
  public mutating func take() -> Dictionary<Key, Value> {
    _precondition(_actualCount >= 0,
      "Cannot take the result twice")
    _precondition(_actualCount == _requestedCount,
      "The number of members added does not match the promised count")

    // Finish building the `Dictionary`.
    _nativeBuffer.count = _requestedCount

    // Prevent taking the result twice.
    _actualCount = -1
    return _result
  }
}

extension Dictionary {
  /// Removes and returns the first key-value pair of the dictionary if the
  /// dictionary isn't empty.
  ///
  /// The first element of the dictionary is not necessarily the first element
  /// added. Don't expect any particular ordering of key-value pairs.
  ///
  /// - Returns: The first key-value pair of the dictionary if the dictionary
  ///   is not empty; otherwise, `nil`.
  ///
  /// - Complexity: Averages to O(1) over many calls to `popFirst()`.
  @inlinable
  public mutating func popFirst() -> Element? {
    guard !isEmpty else { return nil }
    return remove(at: startIndex)
  }

  /// The total number of key-value pairs that the dictionary can contain without
  /// allocating new storage.
  @inlinable // FIXME(sil-serialize-all)
  public var capacity: Int {
    return _variantBuffer.capacity
  }

  /// Reserves enough space to store the specified number of key-value pairs.
  ///
  /// If you are adding a known number of key-value pairs to a dictionary, use this
  /// method to avoid multiple reallocations. This method ensures that the
  /// dictionary has unique, mutable, contiguous storage, with space allocated
  /// for at least the requested number of key-value pairs.
  ///
  /// Calling the `reserveCapacity(_:)` method on a dictionary with bridged
  /// storage triggers a copy to contiguous storage even if the existing
  /// storage has room to store `minimumCapacity` key-value pairs.
  ///
  /// - Parameter minimumCapacity: The requested number of key-value pairs to
  ///   store.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func reserveCapacity(_ minimumCapacity: Int) {
    _variantBuffer.reserveCapacity(minimumCapacity)
    _sanityCheck(self.capacity >= minimumCapacity)
  }
}

//===--- Bridging ---------------------------------------------------------===//

#if _runtime(_ObjC)
extension Dictionary {
  @inlinable // FIXME(sil-serialize-all)
  public func _bridgeToObjectiveCImpl() -> _NSDictionaryCore {
    switch _variantBuffer {
    case _VariantDictionaryBuffer.native(let buffer):
      return buffer.bridged()
    case _VariantDictionaryBuffer.cocoa(let cocoaBuffer):
      return cocoaBuffer.cocoaDictionary
    }
  }

  /// Returns the native Dictionary hidden inside this NSDictionary;
  /// returns nil otherwise.
  public static func _bridgeFromObjectiveCAdoptingNativeStorageOf(
    _ s: AnyObject
  ) -> Dictionary<Key, Value>? {

    // Try all three NSDictionary impls that we currently provide.

    if let deferredBuffer = s as? _SwiftDeferredNSDictionary<Key, Value> {
      return Dictionary(_nativeBuffer: deferredBuffer.nativeBuffer)
    }

    if let nativeStorage = s as? _HashableTypedNativeDictionaryStorage<Key, Value> {
      return Dictionary(_nativeBuffer:
          _NativeDictionaryBuffer(_storage: nativeStorage))
    }

    if s === _RawNativeDictionaryStorage.empty {
      return Dictionary()
    }

    // FIXME: what if `s` is native storage, but for different key/value type?
    return nil
  }
}
#endif
