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
//   |  Dictionary<K,V>._Variant (an enum)            |
//   | +--------------------------------------------+ |
//   | | [_NativeDictionary<K,V> (a struct)]        | |
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
//   | Dictionary<K,V>._Variant (an enum)           |
//   | +----------------------------------------+   |
//   | | [ _CocoaDictionary (a struct) ]        |   |
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
//   |   | _CocoaDictionary.Index (a struct) |
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
// `_NativeDictionary<AnyObject, AnyObject>`. Without the Hashable requirement,
// such a dictionary is restricted to operations which can be performed with
// only the structure of the Storage: indexing and iteration. This is used in
// _SwiftDeferredNSDictionary to construct twin "native" and "bridged"
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
// this type didn't exist, then _NativeDictionary would have to store a Storage
// that declared its actual type parameters. Similarly, the empty singleton
// would have to declare its actual type parameters. If the singleton was, for
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
// * Otherwise, bridging the `Dictionary` is done by wrapping it in a
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
/// provides, see the `KeyValuePairs` type for an alternative.
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
/// the corresponding key-value pair as a nonoptional tuple.
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
  /// The element type of a dictionary: a tuple containing an individual
  /// key-value pair.
  public typealias Element = (key: Key, value: Value)

  @usableFromInline
  internal var _variant: _Variant

  /// Creates an empty dictionary.
  @inlinable
  public init() {
    self.init(_native: _NativeDictionary())
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
  @inlinable
  public init(minimumCapacity: Int) {
    _variant = .native(_NativeDictionary(minimumCapacity: minimumCapacity))
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
  @inlinable
  public init<S: Sequence>(
    uniqueKeysWithValues keysAndValues: S
  ) where S.Element == (Key, Value) {
    if let d = keysAndValues as? Dictionary<Key, Value> {
      self = d
    } else {
      self = Dictionary(minimumCapacity: keysAndValues.underestimatedCount)
      // '_MergeError.keyCollision' is caught and handled with an appropriate
      // error message one level down, inside _variant.merge(_:...).
      try! _variant.merge(
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
  @inlinable
  public init<S: Sequence>(
    _ keysAndValues: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    self = Dictionary(minimumCapacity: keysAndValues.underestimatedCount)
    try _variant.merge(keysAndValues, uniquingKeysWith: combine)
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
  @inlinable
  public init<S: Sequence>(
    grouping values: S,
    by keyForValue: (S.Element) throws -> Key
  ) rethrows where Value == [S.Element] {
    self = [:]
    try _variant.nativeGroup(values, by: keyForValue)
  }

  @inlinable
  internal init(_native: _NativeDictionary<Key, Value>) {
    _variant = .native(_native)
  }

  @inlinable
  internal init(_variant: _Variant) {
    self._variant = _variant
  }

#if _runtime(_ObjC)
  /// Private initializer used for bridging.
  ///
  /// Only use this initializer when both conditions are true:
  ///
  /// * it is statically known that the given `NSDictionary` is immutable;
  /// * `Key` and `Value` are bridged verbatim to Objective-C (i.e.,
  ///   are reference types).
  @inlinable
  public init(_immutableCocoaDictionary: _NSDictionary) {
    _sanityCheck(
      _isBridgedVerbatimToObjectiveC(Key.self) &&
      _isBridgedVerbatimToObjectiveC(Value.self),
      "Dictionary can be backed by NSDictionary buffer only when both key and value are bridged verbatim to Objective-C")
    _variant = .cocoa(_CocoaDictionary(_immutableCocoaDictionary))
  }
#endif
}

//
// All APIs below should dispatch to `_variant`, without doing any
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
  @inlinable
  @inline(__always)
  public func makeIterator() -> Iterator {
    return _variant.makeIterator()
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
  @inlinable
  public var startIndex: Index {
    return _variant.startIndex
  }

  /// The dictionary's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// If the collection is empty, `endIndex` is equal to `startIndex`.
  ///
  /// - Complexity: Amortized O(1) if the dictionary does not wrap a bridged
  ///   `NSDictionary`; otherwise, the performance is unspecified.
  @inlinable
  public var endIndex: Index {
    return _variant.endIndex
  }

  @inlinable
  public func index(after i: Index) -> Index {
    return _variant.index(after: i)
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
  @inlinable
  @inline(__always)
  public func index(forKey key: Key) -> Index? {
    // Complexity: amortized O(1) for native dictionary, O(*n*) when wrapping an
    // NSDictionary.
    return _variant.index(forKey: key)
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
  @inlinable
  public subscript(position: Index) -> Element {
    return _variant.assertingGet(at: position)
  }

  /// The number of key-value pairs in the dictionary.
  ///
  /// - Complexity: O(1).
  @inlinable
  public var count: Int {
    return _variant.count
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
  @inlinable
  public var isEmpty: Bool {
    return count == 0
  }

  /// The first element of the dictionary.
  ///
  /// The first element of the dictionary is not necessarily the first element
  /// added to the dictionary. Don't expect any particular ordering of
  /// dictionary elements.
  ///
  /// If the dictionary is empty, the value of this property is `nil`.
  @inlinable
  public var first: Element? {
    // FIXME: It'd better to use an iterator than to subscript with startIndex,
    // because startIndex is currently O(n) in bridged dictionaries. However,
    // enumerators aren't guaranteed to have the same element order as allKeys.
    return count > 0 ? self[startIndex] : nil
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
  @inlinable
  public subscript(key: Key) -> Value? {
    @inline(__always)
    get {
      return _variant.maybeGet(key)
    }
    set(newValue) {
      if let x = newValue {
        // FIXME(performance): this loads and discards the old value.
        _variant.updateValue(x, forKey: key)
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
  @inlinable
  @_effects(readonly)
  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.init(_native: _NativeDictionary.fromArray(elements))
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
  @inlinable
  public subscript(
    key: Key, default defaultValue: @autoclosure () -> Value
  ) -> Value {
    @inline(__always)
    get {
      return _variant.maybeGet(key) ?? defaultValue()
    }
    mutableAddressWithNativeOwner {
      let (_, address) = _variant.pointerToValue(
        forKey: key,
        insertingDefault: defaultValue)
      return (address, Builtin.castToNativeObject(_variant.asNative._storage))
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
  ///
  /// - Complexity: O(*n*), where *n* is the length of the dictionary.
  @inlinable
  public func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> Dictionary<Key, T> {
    return try Dictionary<Key, T>(_variant: _variant.mapValues(transform))
  }

  /// Returns a new dictionary containing only the key-value pairs that have
  /// non-`nil` values as the result from the transform by the given closure.
  ///
  /// Use this method to receive a dictionary of nonoptional values when your
  /// transformation can produce an optional value.
  ///
  /// In this example, note the difference in the result of using `mapValues`
  /// and `compactMapValues` with a transformation that returns an optional
  /// `Int` value.
  ///
  ///     let data = ["a": "1", "b": "three", "c": "///4///"]
  ///
  ///     let m: [String: Int?] = data.mapValues { str in Int(str) }
  ///     // ["a": 1, "b": nil, "c": nil]
  ///
  ///     let c: [String: Int] = data.compactMapValues { str in Int(str) }
  ///     // ["a": 1]
  ///
  /// - Parameter transform: A closure that transforms a value. `transform`
  ///   accepts each value of the dictionary as its parameter and returns an
  ///   optional transformed value of the same or of a different type.
  /// - Returns: A dictionary containing the keys and non-`nil` transformed
  ///   values of this dictionary.
  ///
  /// - Complexity: O(*m* + *n*), where *n* is the length of the original
  ///   dictionary and *m* is the length of the resulting dictionary.
  @inlinable
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
  @inlinable
  @discardableResult
  public mutating func updateValue(
    _ value: Value, forKey key: Key
  ) -> Value? {
    return _variant.updateValue(value, forKey: key)
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
  @inlinable
  public mutating func merge<S: Sequence>(
    _ other: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    try _variant.merge(other, uniquingKeysWith: combine)
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
  @inlinable
  public mutating func merge(
    _ other: [Key: Value],
    uniquingKeysWith combine: (Value, Value) throws -> Value) rethrows
  {
    try _variant.merge(
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
  @inlinable
  public func merging<S: Sequence>(
    _ other: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows -> [Key: Value] where S.Element == (Key, Value) {
    var result = self
    try result._variant.merge(other, uniquingKeysWith: combine)
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
  @inlinable
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
  @inlinable
  @discardableResult
  public mutating func remove(at index: Index) -> Element {
    return _variant.remove(at: index)
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
  @inlinable
  @discardableResult
  public mutating func removeValue(forKey key: Key) -> Value? {
    return _variant.removeValue(forKey: key)
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
  @inlinable
  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    // The 'will not decrease' part in the documentation comment is worded very
    // carefully.  The capacity can increase if we replace Cocoa dictionary with
    // native dictionary.
    _variant.removeAll(keepingCapacity: keepCapacity)
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
  @inlinable
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
  @inlinable
  @available(swift, introduced: 4.0)
  public var values: Values {
    get {
      return Values(self)
    }
    set {
      self = Dictionary(_variant: newValue._variant)
    }
  }

  /// A view of a dictionary's keys.
  @_fixed_layout
  public struct Keys
    : Collection, Equatable,
      CustomStringConvertible, CustomDebugStringConvertible {
    public typealias Element = Key

    @usableFromInline
    internal var _variant: Dictionary<Key, Value>._Variant

    @inlinable
    internal init(_ _dictionary: Dictionary) {
      self._variant = _dictionary._variant
    }

    // Collection Conformance
    // ----------------------

    @inlinable
    public var startIndex: Index {
      return _variant.startIndex
    }

    @inlinable
    public var endIndex: Index {
      return _variant.endIndex
    }

    @inlinable
    public func index(after i: Index) -> Index {
      return _variant.index(after: i)
    }

    @inlinable
    public subscript(position: Index) -> Element {
      return _variant.assertingGet(at: position).key
    }

    // Customization
    // -------------

    /// The number of keys in the dictionary.
    ///
    /// - Complexity: O(1).
    @inlinable
    public var count: Int {
      return _variant.count
    }

    @inlinable
    public var isEmpty: Bool {
      return count == 0
    }

    @inlinable
    public func _customContainsEquatableElement(_ element: Element) -> Bool? {
      return _variant.containsKey(element)
    }

    @inlinable
    public func _customIndexOfEquatableElement(_ element: Element) -> Index?? {
      return Optional(_variant.index(forKey: element))
    }

    @inlinable
    public func _customLastIndexOfEquatableElement(_ element: Element) -> Index?? {
      // The first and last elements are the same because each element is unique.
      return _customIndexOfEquatableElement(element)
    }

    @inlinable
    public static func ==(lhs: Keys, rhs: Keys) -> Bool {
      // Equal if the two dictionaries share storage.
      if case (.native(let ln), .native(let rn)) = (lhs._variant, rhs._variant),
        ln._storage === rn._storage {
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

    @inlinable
    public var description: String {
      return _makeCollectionDescription(for: self, withTypeName: nil)
    }

    public var debugDescription: String {
      return _makeCollectionDescription(
        for: self,
        withTypeName: "Dictionary.Keys")
    }
  }

  /// A view of a dictionary's values.
  @_fixed_layout
  public struct Values
    : MutableCollection, CustomStringConvertible, CustomDebugStringConvertible {
    public typealias Element = Value

    @usableFromInline
    internal var _variant: Dictionary<Key, Value>._Variant

    @inlinable
    internal init(_ _dictionary: Dictionary) {
      self._variant = _dictionary._variant
    }

    // Collection Conformance
    // ----------------------

    @inlinable
    public var startIndex: Index {
      return _variant.startIndex
    }

    @inlinable
    public var endIndex: Index {
      return _variant.endIndex
    }

    @inlinable
    public func index(after i: Index) -> Index {
      return _variant.index(after: i)
    }

    @inlinable
    public subscript(position: Index) -> Element {
      get {
        return _variant.assertingGet(at: position).value
      }
      mutableAddressWithNativeOwner {
        let address = _variant.pointerToValue(at: position)
        return (address, Builtin.castToNativeObject(_variant.asNative._storage))
      }
    }

    // Customization
    // -------------

    /// The number of values in the dictionary.
    ///
    /// - Complexity: O(1).
    @inlinable
    public var count: Int {
      return _variant.count
    }

    @inlinable
    public var isEmpty: Bool {
      return count == 0
    }

    @inlinable
    public var description: String {
      return _makeCollectionDescription(for: self, withTypeName: nil)
    }

    public var debugDescription: String {
      return _makeCollectionDescription(
        for: self,
        withTypeName: "Dictionary.Values")
    }
  }
}

extension Dictionary: Equatable where Value: Equatable {
  @inlinable
  public static func == (lhs: [Key: Value], rhs: [Key: Value]) -> Bool {
    switch (lhs._variant, rhs._variant) {
    case (.native(let lhsNative), .native(let rhsNative)):

      if lhsNative._storage === rhsNative._storage {
        return true
      }

      if lhsNative.count != rhsNative.count {
        return false
      }

      for (k, v) in lhs {
        let (i, found) = rhsNative._find(k)
        if !found || rhsNative.value(at: i.bucket) != v {
          return false
        }
      }
      return true

  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      return _stdlib_NSObject_isEqual(lhsCocoa.object, rhsCocoa.object)

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

  @inlinable
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
  @inlinable
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
  @inlinable
  public var description: String {
    return _makeDescription()
  }

  /// A string that represents the contents of the dictionary, suitable for
  /// debugging.
  public var debugDescription: String {
    return _makeDescription()
  }
}

@usableFromInline
@_frozen
internal enum _MergeError: Error {
  case keyCollision
}

#if _runtime(_ObjC)
/// Equivalent to `NSDictionary.allKeys`, but does not leave objects on the
/// autorelease pool.
@inlinable
internal func _stdlib_NSDictionary_allKeys(
  _ nsd: _NSDictionary
) -> _HeapBuffer<Int, AnyObject> {
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
@inlinable
public func _dictionaryUpCast<DerivedKey, DerivedValue, BaseKey, BaseValue>(
    _ source: Dictionary<DerivedKey, DerivedValue>
) -> Dictionary<BaseKey, BaseValue> {
  var result = Dictionary<BaseKey, BaseValue>(minimumCapacity: source.count)

  for (k, v) in source {
    result[k as! BaseKey] = (v as! BaseValue)
  }
  return result
}

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
@inlinable
public func _dictionaryDownCast<BaseKey, BaseValue, DerivedKey, DerivedValue>(
  _ source: Dictionary<BaseKey, BaseValue>
) -> Dictionary<DerivedKey, DerivedValue> {

#if _runtime(_ObjC)
  if _isClassOrObjCExistential(BaseKey.self)
  && _isClassOrObjCExistential(BaseValue.self)
  && _isClassOrObjCExistential(DerivedKey.self)
  && _isClassOrObjCExistential(DerivedValue.self) {

    switch source._variant {
    case .native(let native):
      // Note: it is safe to treat the buffer as immutable here because
      // Dictionary will not mutate buffer with reference count greater than 1.
      return Dictionary(_immutableCocoaDictionary: native.bridged())
    case .cocoa(let cocoa):
      return Dictionary(_immutableCocoaDictionary: cocoa.object)
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
@inlinable
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

//===--- APIs templated for Dictionary and Set ----------------------------===//

/// This protocol is only used for compile-time checks that
/// every buffer type implements all required operations.
internal protocol _DictionaryBuffer {
  associatedtype Key
  associatedtype Value
  associatedtype Index

  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after i: Index) -> Index
  func formIndex(after i: inout Index)
  func index(forKey key: Key) -> Index?
  var count: Int { get }

  func assertingGet(at i: Index) -> (key: Key, value: Value)
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
@usableFromInline
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
  @inlinable
  @nonobjc
  internal static var empty: _RawNativeDictionaryStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyDictionaryStorage))
  }

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
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
    return _SwiftDictionaryNSEnumerator<AnyObject, AnyObject>(
        _NativeDictionary(_storage: self))
  }

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return self
  }

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
    return nil
  }

  internal func keyEnumerator() -> _NSEnumerator {
    return enumerator()
  }

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
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only create this by calling Buffer's inits")
  }

#if _runtime(_ObjC)
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
  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Only create this by calling Buffer's inits'")
  }

#if _runtime(_ObjC)
  // NSDictionary bridging:

  // All actual functionality comes from native/full, which are
  // just wrappers around a RawNativeDictionaryStorage.

  internal var native: _NativeDictionary<Key, Value> {
    return _NativeDictionary(_storage: self)
  }

  @objc
  internal override func enumerator() -> _NSEnumerator {
    return _SwiftDictionaryNSEnumerator<Key, Value>(
        _NativeDictionary(_storage: self))
  }

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
      theState.extra.0 = CUnsignedLong(native.startIndex.bucket)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var currIndex = _NativeDictionary<Key, Value>.Index(
        bucket: Int(theState.extra.0))
    let endIndex = native.endIndex
    var stored = 0
    for i in 0..<count {
      if (currIndex == endIndex) {
        break
      }

      unmanagedObjects[i] = native.bridgedKey(at: currIndex)

      stored += 1
      native.formIndex(after: &currIndex)
    }
    theState.extra.0 = CUnsignedLong(currIndex.bucket)
    state.pointee = theState
    return stored
  }

  @nonobjc
  internal func getObjectFor(_ aKey: AnyObject) -> AnyObject? {
    guard let nativeKey = _conditionallyBridgeFromObjectiveC(aKey, Key.self)
    else { return nil }

    let (i, found) = native._find(nativeKey)

    if found {
      return native.bridgedValue(at: i)
    }
    return nil
  }

  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @objc(objectForKey:)
  override func objectFor(_ aKey: AnyObject) -> AnyObject? {
    return getObjectFor(aKey)
  }

  // We also override the following methods for efficiency.
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
        for (key, value) in native {
          unmanagedObjects[i] = _bridgeAnythingToObjectiveC(value)
          unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
          i += 1
          guard i < count else { break }
        }
      } else {
        // keys nonnull, objects null
        for (key, _) in native {
          unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
          i += 1
          guard i < count else { break }
        }
      }
    } else {
      if let unmanagedObjects = _UnmanagedAnyObjectArray(objects) {
        // keys null, objects nonnull
        for (_, value) in native {
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
internal struct _NativeDictionary<Key, Value> {
  @usableFromInline
  internal typealias Element = (key: Key, value: Value)

  /// See this comments on _RawNativeDictionaryStorage and its subclasses to
  /// understand why we store an untyped storage here.
  @usableFromInline
  internal var _storage: _RawNativeDictionaryStorage

  /// Constructs an instance from the empty singleton.
  @inlinable
  internal init() {
    self._storage = _RawNativeDictionaryStorage.empty
  }

  /// Constructs a dictionary adopting the given storage.
  @inlinable
  internal init(_storage: _RawNativeDictionaryStorage) {
    self._storage = _storage
  }

  /// Creates a native dictionary with a storage that is typed, but doesn't
  /// understand Hashing. Mostly for bridging; prefer `init(minimumCapacity:)`.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_exactBucketCount bucketCount: Int, unhashable: ()) {
    let bitmapWordCount = _UnsafeBitMap.sizeInWords(forSizeInBits: bucketCount)
    let storage = Builtin.allocWithTailElems_3(
      _TypedNativeDictionaryStorage<Key, Value>.self,
      bitmapWordCount._builtinWordValue, UInt.self,
      bucketCount._builtinWordValue, Key.self,
      bucketCount._builtinWordValue, Value.self)
    self.init(_exactBucketCount: bucketCount, storage: storage)
  }

  /// Given a bucket count and uninitialized _RawNativeDictionaryStorage,
  /// completes the initialization and returns a native dictionary.
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
    get {
      return _assumeNonNegative(_storage.count)
    }
    set {
      _storage.count = newValue
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
    let k = key(at: index.bucket)
    return _bridgeAnythingToObjectiveC(k)
  }

  /// Returns the value at the given Index, bridged.
  ///
  /// Intended for use with verbatim bridgeable keys.
  @inlinable // FIXME(sil-serialize-all)
  internal func bridgedValue(at index: Index) -> AnyObject {
    let v = value(at: index.bucket)
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
  internal func moveInitializeEntry(
    from: _NativeDictionary,
    at: Int,
    toEntryAt: Int) {
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
    return index(after: Index(bucket: -1))
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var endIndex: Index {
    return Index(bucket: bucketCount)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func index(after i: Index) -> Index {
    _precondition(i != endIndex)
    var idx = i.bucket + 1
    while idx < bucketCount && !isInitializedEntry(at: idx) {
      idx += 1
    }

    return Index(bucket: idx)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func formIndex(after i: inout Index) {
    i = index(after: i)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func assertingGet(at i: Index) -> Element {
    _precondition(i.bucket >= 0 && i.bucket < bucketCount)
    _precondition(
      isInitializedEntry(at: i.bucket),
      "Attempting to access Dictionary elements using an invalid Index")
    let key = self.key(at: i.bucket)
    return (key, self.value(at: i.bucket))
  }
}

extension _NativeDictionary where Key: Hashable {
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal init(minimumCapacity: Int) {
    let bucketCount = _NativeDictionary.bucketCount(
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

  /// Create a dictionary instance with room for at least 'bucketCount' entries,
  /// marking all entries invalid.
  @inlinable // FIXME(sil-serialize-all)
  internal init(_exactBucketCount bucketCount: Int) {
    let bitmapWordCount = _UnsafeBitMap.sizeInWords(forSizeInBits: bucketCount)
    let storage = Builtin.allocWithTailElems_3(
      _HashableTypedNativeDictionaryStorage<Key, Value>.self,
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
      nsSet = _SwiftDeferredNSDictionary(self)
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

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func _find(_ key: Key) -> (pos: Index, found: Bool) {
    return _find(key, startBucket: _bucket(key))
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
        return (Index(bucket: bucket), false)
      }
      if self.key(at: bucket) == key {
        return (Index(bucket: bucket), true)
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
    return Swift.max(
      Int((Double(capacity) * maxLoadFactorInverse).rounded(.up)),
      capacity + 1)
  }

  /// Self should be uniquely referenced.
  /// The `key` should not be present in the Dictionary.
  /// This function does *not* update `count`.
  @inlinable // FIXME(sil-serialize-all)
  internal func unsafeAddNew(key newKey: Key, value: Value) {
    let (i, found) = _find(newKey)
    _precondition(
      !found, "Duplicate key found in Dictionary. Keys may have been mutated after insertion")
    initializeKey(newKey, value: value, at: i.bucket)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func fromArray(
    _ elements: [(Key, Value)]
  ) -> _NativeDictionary {
    if elements.isEmpty {
      return _NativeDictionary()
    }

    var dictionary = _NativeDictionary(minimumCapacity: elements.count)

    for (key, value) in elements {
      let (i, found) = dictionary._find(key)
      _precondition(!found, "Dictionary literal contains duplicate keys")
      dictionary.initializeKey(key, value: value, at: i.bucket)
    }
    dictionary.count = elements.count

    return dictionary
  }
}

extension _NativeDictionary where Key: Hashable {
  /// - parameter idealBucket: The ideal bucket for the element being deleted.
  /// - parameter bucket: The bucket containing the element to be deleted.
  /// Precondition: there should be an initialized entry in the specified
  /// bucket.
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func _delete(idealBucket: Int, bucket: Int) {
    _sanityCheck(isInitializedEntry(at: bucket), "expected initialized entry")

    // remove the element
    destroyEntry(at: bucket)
    self.count -= 1

    // If we've put a hole in a chain of contiguous elements, some
    // element after the hole may belong where the new hole is.
    var hole = bucket

    // Find the first bucket in the contiguous chain
    var start = idealBucket
    while isInitializedEntry(at: _prev(start)) {
      start = _prev(start)
    }

    // Find the last bucket in the contiguous chain
    var lastInChain = hole
    var b = _index(after: lastInChain)
    while isInitializedEntry(at: b) {
      lastInChain = b
      b = _index(after: b)
    }

    // Relocate out-of-place elements in the chain, repeating until
    // none are found.
    while hole != lastInChain {
      // Walk backwards from the end of the chain looking for
      // something out-of-place.
      var b = lastInChain
      while b != hole {
        let idealBucket = _bucket(self.key(at: b))

        // Does this element belong between start and hole?  We need
        // two separate tests depending on whether [start, hole] wraps
        // around the end of the storage
        let c0 = idealBucket >= start
        let c1 = idealBucket <= hole
        if start <= hole ? (c0 && c1) : (c0 || c1) {
          break // Found it
        }
        b = self._prev(b)
      }

      if b == hole { // No out-of-place elements found; we're done adjusting
        break
      }

      // Move the found element into the hole
      moveInitializeEntry(from: self, at: b, toEntryAt: hole)
      hole = b
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  mutating func _removeAll() {
    for b in 0 ..< bucketCount {
      if isInitializedEntry(at: b) {
        destroyEntry(at: b)
      }
    }
    count = 0
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
    var result = _NativeDictionary<Key, T>(_exactBucketCount: bucketCount)
    // Because the keys in the current and new buffer are the same, we can
    // initialize to the same locations in the new buffer, skipping hash value
    // recalculations.
    var i = startIndex
    while i != endIndex {
      let (k, v) = assertingGet(at: i)
      try result.initializeKey(k, value: transform(v), at: i.bucket)
      formIndex(after: &i)
    }
    result.count = self.count

    return result
  }
}

extension _NativeDictionary/*: _DictionaryBuffer */ where Key: Hashable {
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
    let (i, found) = _find(key)
    return found ? i : nil
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func maybeGet(_ key: Key) -> Value? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }

    let (i, found) = _find(key)
    if found {
      return self.value(at: i.bucket)
    }
    return nil
  }
}

#if _runtime(_ObjC)
/// An NSEnumerator that works with any _NativeDictionary of
/// verbatim bridgeable elements. Used by the various NSDictionary impls.
final internal class _SwiftDictionaryNSEnumerator<Key, Value>
  : _SwiftNativeNSEnumerator, _NSEnumerator {

  internal var base: _NativeDictionary<Key, Value>
  internal var nextIndex: _NativeDictionary<Key, Value>.Index
  internal var endIndex: _NativeDictionary<Key, Value>.Index

  internal override required init() {
    _sanityCheckFailure("don't call this designated initializer")
  }

  internal init(_ base: _NativeDictionary<Key, Value>) {
    self.base = base
    nextIndex = base.startIndex
    endIndex = base.endIndex
  }

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
    let key = base.bridgedKey(at: nextIndex)
    base.formIndex(after: &nextIndex)
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
    let key = base.bridgedKey(at: nextIndex)
    base.formIndex(after: &nextIndex)

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects)
    unmanagedObjects[0] = key
    state.pointee = theState
    return 1
  }
}
#endif

#if _runtime(_ObjC)
/// This class exists for Objective-C bridging. It holds a reference to a
/// _NativeDictionary, and can be upcast to NSSelf when bridging is
/// necessary.  This is the fallback implementation for situations where
/// toll-free bridging isn't possible. On first access, a _NativeDictionary
/// of AnyObject will be constructed containing all the bridged elements.
final internal class _SwiftDeferredNSDictionary<Key: Hashable, Value>
  : _SwiftNativeNSDictionary, _NSDictionaryCore {

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  @nonobjc
  private var _bridgedStorage_DoNotUse: AnyObject?

  /// The unbridged elements.
  internal var native: _NativeDictionary<Key, Value>

  internal init(_ native: _NativeDictionary<Key, Value>) {
    self.native = native
    super.init()
  }

  /// Returns the pointer to the stored property, which contains bridged
  /// Dictionary elements.
  @nonobjc
  private var _bridgedStoragePtr: UnsafeMutablePointer<AnyObject?> {
    return _getUnsafePointerToStoredProperties(self).assumingMemoryBound(
      to: Optional<AnyObject>.self)
  }

  /// The buffer for bridged Dictionary elements, if present.
  @nonobjc
  private var _bridgedStorage: _RawNativeDictionaryStorage? {
    get {
      if let ref = _stdlib_atomicLoadARCRef(object: _bridgedStoragePtr) {
        return unsafeDowncast(ref, to: _RawNativeDictionaryStorage.self)
      }
      return nil
    }
  }

  /// Attach a buffer for bridged Dictionary elements.
  @nonobjc
  private func _initializeBridgedStorage(_ storage: AnyObject) {
    _stdlib_atomicInitializeARCRef(object: _bridgedStoragePtr, desired: storage)
  }

  /// Returns the bridged Dictionary values.
  internal var bridged: _NativeDictionary<AnyObject, AnyObject> {
    return _NativeDictionary(_storage: _bridgedStorage!)
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
    let bridged = _NativeDictionary<AnyObject, AnyObject>(
      _exactBucketCount: native.bucketCount,
      unhashable: ())

    // Bridge everything.
    for i in 0..<native.bucketCount {
      if native.isInitializedEntry(at: i) {
        let key = _bridgeAnythingToObjectiveC(native.key(at: i))
        let val = _bridgeAnythingToObjectiveC(native.value(at: i))
        bridged.initializeKey(key, value: val, at: i)
      }
    }

    // Atomically put the bridged elements in place.
    _initializeBridgedStorage(bridged._storage)
  }

  //
  // NSDictionary implementation.
  //
  // Do not call any of these methods from the standard library!  Use only
  // `native`.
  //

  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // Instances of this class should be visible outside of standard library as
    // having `NSDictionary` type, which is immutable.
    return self
  }

  @objc(objectForKey:)
  internal func objectFor(_ aKey: AnyObject) -> AnyObject? {
    guard let nativeKey = _conditionallyBridgeFromObjectiveC(aKey, Key.self)
    else { return nil }

    let (i, found) = native._find(nativeKey)
    if found {
      bridgeEverything()
      return bridged.value(at: i.bucket)
    }
    return nil
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
    let bucketCount = native.bucketCount

    if let unmanagedKeys = _UnmanagedAnyObjectArray(keys) {
      if let unmanagedObjects = _UnmanagedAnyObjectArray(objects) {
        // keys nonnull, objects nonnull
        for position in 0..<bucketCount {
          if bridged.isInitializedEntry(at: position) {
            unmanagedObjects[i] = bridged.value(at: position)
            unmanagedKeys[i] = bridged.key(at: position)
            i += 1
            guard i < count else { break }
          }
        }
      } else {
        // keys nonnull, objects null
        for position in 0..<bucketCount {
          if bridged.isInitializedEntry(at: position) {
            unmanagedKeys[i] = bridged.key(at: position)
            i += 1
            guard i < count else { break }
          }
        }
      }
    } else {
      if let unmanagedObjects = _UnmanagedAnyObjectArray(objects) {
        // keys null, objects nonnull
        for position in 0..<bucketCount {
          if bridged.isInitializedEntry(at: position) {
            unmanagedObjects[i] = bridged.value(at: position)
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
    let bucketCount = native.bucketCount
    var stop: UInt8 = 0
    for position in 0..<bucketCount {
      if bridged.isInitializedEntry(at: position) {
        block(Unmanaged.passUnretained(bridged.key(at: position)),
          Unmanaged.passUnretained(bridged.value(at: position)),
          &stop)
      }
      if stop != 0 { return }
    }
  }

  @objc
  internal var count: Int {
    return native.count
  }

  @objc
  internal func enumerator() -> _NSEnumerator {
    bridgeEverything()
    return _SwiftDictionaryNSEnumerator<AnyObject, AnyObject>(bridged)
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
      theState.extra.0 = CUnsignedLong(native.startIndex.bucket)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var currIndex = _NativeDictionary<Key, Value>.Index(
        bucket: Int(theState.extra.0))
    let endIndex = native.endIndex
    var stored = 0

    // Only need to bridge once, so we can hoist it out of the loop.
    if (currIndex != endIndex) {
      bridgeEverything()
    }

    for i in 0..<count {
      if (currIndex == endIndex) {
        break
      }

      let bridgedKey = bridged.key(at: currIndex.bucket)
      unmanagedObjects[i] = bridgedKey
      stored += 1
      native.formIndex(after: &currIndex)
    }
    theState.extra.0 = CUnsignedLong(currIndex.bucket)
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
internal struct _CocoaDictionary: _DictionaryBuffer {
  @usableFromInline
  internal let object: _NSDictionary

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ object: _NSDictionary) {
    self.object = object
  }

  @usableFromInline
  internal typealias Key = AnyObject
  @usableFromInline
  internal typealias Value = AnyObject

  @inlinable // FIXME(sil-serialize-all)
  internal var startIndex: Index {
    return Index(self, startIndex: ())
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var endIndex: Index {
    return Index(self, endIndex: ())
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

    let allKeys = _stdlib_NSDictionary_allKeys(object)
    var keyIndex = -1
    for i in 0..<allKeys.value {
      if _stdlib_NSObject_isEqual(key, allKeys[i]) {
        keyIndex = i
        break
      }
    }
    _sanityCheck(keyIndex >= 0,
        "Key was found in fast path, but not found later?")
    return Index(self, allKeys, keyIndex)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var count: Int {
    return object.count
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func assertingGet(at i: Index) -> (key: Key, value: Value) {
    let key: Key = i.allKeys[i.currentKeyIndex]
    let value: Value = i.base.object.objectFor(key)!
    return (key, value)

  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func maybeGet(_ key: Key) -> Value? {
    return object.objectFor(key)
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func updateValue(_ value: Value, forKey key: Key) -> Value? {
    _sanityCheckFailure("cannot mutate NSDictionary")
  }

  @usableFromInline
  internal func _toNative<K: Hashable, V>(
    bucketCount: Int
  ) -> _NativeDictionary<K, V> {
    var result = _NativeDictionary<K, V>(bucketCount: bucketCount)
    let iterator = _CocoaDictionary.Iterator(self)
    while let (cocoaKey, cocoaValue) = iterator.next()  {
      result.unsafeAddNew(
        key: _forceBridgeFromObjectiveC(cocoaKey, K.self),
        value: _forceBridgeFromObjectiveC(cocoaValue, V.self))
      result.count += 1
    }
    return result
  }

  @usableFromInline
  internal func mapValues<Key: Hashable, Value, T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
    var result = _NativeDictionary<Key, T>(minimumCapacity: self.count)
    let iterator = _CocoaDictionary.Iterator(self)
    while let (cocoaKey, cocoaValue) = iterator.next()  {
      try result.unsafeAddNew(
        key: _forceBridgeFromObjectiveC(cocoaKey, Key.self),
        value: transform(_forceBridgeFromObjectiveC(cocoaValue, Value.self)))
      result.count += 1
    }
    return result
  }
}
#endif

extension Dictionary {
  @usableFromInline
  @_frozen
  internal enum _Variant {
    case native(_NativeDictionary<Key, Value>)
#if _runtime(_ObjC)
    case cocoa(_CocoaDictionary)
#endif
  }
}

extension Dictionary._Variant: _DictionaryBuffer {
  @usableFromInline
  internal typealias Element = (key: Key, value: Value)

  @usableFromInline
  @_transparent
  internal var guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 || _canBeClass(Value.self) == 0
  }

  @inlinable
  internal mutating func isUniquelyReferenced() -> Bool {
    // Note that &self drills down through .native(_NativeDictionary) to the
    // first property in _NativeDictionary, which is the reference to the
    // storage.
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

  @inlinable
  internal var asNative: _NativeDictionary<Key, Value> {
    get {
      switch self {
      case .native(let native):
        return native
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

  @inlinable
  internal mutating func ensureNative() {
#if _runtime(_ObjC)
    if _fastPath(guaranteedNative) { return }
    if case .cocoa(let cocoaDictionary) = self {
      migrateToNative(cocoaDictionary)
    }
#endif
  }

#if _runtime(_ObjC)
  @inlinable
  internal var asCocoa: _CocoaDictionary {
    switch self {
    case .native:
      _sanityCheckFailure("internal error: not backed by NSDictionary")
    case .cocoa(let cocoaDictionary):
      return cocoaDictionary
    }
  }
#endif

  /// Return true if self is native.
  @inlinable
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
  internal mutating func _ensureUniqueNative(
    withBucketCount desiredBucketCount: Int
  ) -> (reallocated: Bool, capacityChanged: Bool) {
    let oldBucketCount = asNative.bucketCount
    let isUnique = isUniquelyReferenced()
    if oldBucketCount >= desiredBucketCount && isUnique {
      return (reallocated: false, capacityChanged: false)
    }

    let oldDictionary = asNative
    var newDictionary = _NativeDictionary<Key, Value>(bucketCount: desiredBucketCount)
    let newBucketCount = newDictionary.bucketCount
    for i in 0..<oldBucketCount {
      if oldDictionary.isInitializedEntry(at: i) {
        if oldBucketCount == newBucketCount {
          let key = oldDictionary.key(at: i)
          let value = oldDictionary.value(at: i)
          newDictionary.initializeKey(key, value: value , at: i)
        } else {
          let key = oldDictionary.key(at: i)
          newDictionary.unsafeAddNew(
            key: key,
            value: oldDictionary.value(at: i))
        }
      }
    }
    newDictionary.count = oldDictionary.count

    self = .native(newDictionary)
    return (reallocated: true, capacityChanged: oldBucketCount != newBucketCount)
  }

  @inline(__always)
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureUniqueNative(
    withCapacity minimumCapacity: Int
  ) -> (reallocated: Bool, capacityChanged: Bool) {
    let bucketCount = _NativeDictionary<Key, Value>.bucketCount(
      forCapacity: minimumCapacity,
      maxLoadFactorInverse: _hashContainerDefaultMaxLoadFactorInverse)
    return ensureUniqueNative(withBucketCount: bucketCount)
  }

  /// Ensure this we hold a unique reference to a native dictionary
  /// having at least `minimumCapacity` elements.
  @inlinable // FIXME(sil-serialize-all)
  internal mutating func ensureUniqueNative(
    withBucketCount desiredBucketCount: Int
  ) -> (reallocated: Bool, capacityChanged: Bool) {
#if _runtime(_ObjC)
    // This is in a separate variable to make the uniqueness check work in
    // unoptimized builds; see https://bugs.swift.org/browse/SR-6437
    let n = _isNative
    if n {
      return _ensureUniqueNative(withBucketCount: desiredBucketCount)
    }

    switch self {
    case .native:
      fatalError("This should have been handled earlier")
    case .cocoa(let cocoaDictionary):
      self = .native(cocoaDictionary._toNative(bucketCount: desiredBucketCount))
      return (reallocated: true, capacityChanged: true)
    }
#else
    return _ensureUniqueNative(withBucketCount: desiredBucketCount)
#endif
  }

#if _runtime(_ObjC)
  @inline(never)
  @usableFromInline
  internal mutating func migrateToNative(_ cocoa: _CocoaDictionary) {
    let allocated = ensureUniqueNative(withCapacity: cocoa.count).reallocated
    _sanityCheck(allocated, "failed to allocate native Dictionary buffer")
  }
#endif

  /// Reserves enough space for the specified number of elements to be stored
  /// without reallocating additional storage.
  @inlinable
  internal mutating func reserveCapacity(_ capacity: Int) {
    _ = ensureUniqueNative(withCapacity: capacity)
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
    case .cocoa(let cocoaDictionary):
      return cocoaDictionary.count
#endif
    }
  }

  //
  // _DictionaryBuffer conformance
  //

  @usableFromInline
  internal typealias Index = Dictionary<Key, Value>.Index

  @inlinable
  internal var startIndex: Index {
    if _fastPath(guaranteedNative) {
      return ._native(asNative.startIndex)
    }

    switch self {
    case .native:
      return ._native(asNative.startIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaDictionary):
      return ._cocoa(cocoaDictionary.startIndex)
#endif
    }
  }

  @inlinable
  internal var endIndex: Index {
    if _fastPath(guaranteedNative) {
      return ._native(asNative.endIndex)
    }

    switch self {
    case .native:
      return ._native(asNative.endIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoaDictionary):
      return ._cocoa(cocoaDictionary.endIndex)
#endif
    }
  }

  @inlinable
  internal func index(after i: Index) -> Index {
    if _fastPath(guaranteedNative) {
      return ._native(asNative.index(after: i._asNative))
    }

    switch self {
    case .native:
      return ._native(asNative.index(after: i._asNative))
#if _runtime(_ObjC)
    case .cocoa(let cocoaDictionary):
      return ._cocoa(cocoaDictionary.index(after: i._asCocoa))
#endif
    }
  }

  @inlinable
  internal func formIndex(after i: inout Index) {
    // FIXME: swift-3-indexing-model: optimize if possible.
    i = index(after: i)
  }

  @inlinable
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
    case .cocoa(let cocoaDictionary):
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      if let cocoaIndex = cocoaDictionary.index(forKey: cocoaKey) {
        return ._cocoa(cocoaIndex)
      }
      return nil
#endif
    }
  }

  @inlinable
  @inline(__always)
  internal func containsKey(_ key: Key) -> Bool {
    if _fastPath(guaranteedNative) {
      return asNative.index(forKey: key) != nil
    }

    switch self {
    case .native:
      return asNative.index(forKey: key) != nil
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      return Dictionary._Variant.maybeGetFromCocoa(cocoa, forKey: key) != nil
#endif
    }
  }

  @inlinable
  internal func assertingGet(at i: Index) -> (key: Key, value: Value) {
    if _fastPath(guaranteedNative) {
      return asNative.assertingGet(at: i._asNative)
    }

    switch self {
    case .native:
      return asNative.assertingGet(at: i._asNative)
#if _runtime(_ObjC)
    case .cocoa(let cocoaBuffer):
      let (cocoaKey, cocoaValue) = cocoaBuffer.assertingGet(at: i._asCocoa)
      let nativeKey = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
      let nativeValue = _forceBridgeFromObjectiveC(cocoaValue, Value.self)
      return (nativeKey, nativeValue)
#endif
    }
  }

#if _runtime(_ObjC)
  @inline(never)
  @usableFromInline
  internal static func maybeGetFromCocoa(
    _ cocoaDictionary: _CocoaDictionary,
    forKey key: Key
  ) -> Value? {
    let cocoaKey = _bridgeAnythingToObjectiveC(key)
    if let cocoaValue = cocoaDictionary.maybeGet(cocoaKey) {
      return _forceBridgeFromObjectiveC(cocoaValue, Value.self)
    }
    return nil
  }
#endif

  @inlinable
  @inline(__always)
  internal func maybeGet(_ key: Key) -> Value? {
    if _fastPath(guaranteedNative) {
      return asNative.maybeGet(key)
    }

    switch self {
    case .native:
      return asNative.maybeGet(key)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      return Dictionary._Variant.maybeGetFromCocoa(cocoa, forKey: key)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeUpdateValue(
    _ value: Value, forKey key: Key
  ) -> Value? {
    var (i, found) = asNative._find(key)

    let minBuckets = found
      ? asNative.bucketCount
    : _NativeDictionary<Key, Value>.bucketCount(
          forCapacity: asNative.count + 1,
          maxLoadFactorInverse: _hashContainerDefaultMaxLoadFactorInverse)

    let (_, capacityChanged) = ensureUniqueNative(withBucketCount: minBuckets)
    if capacityChanged {
      i = asNative._find(key).pos
    }

    let oldValue: Value? = found ? asNative.value(at: i.bucket) : nil
    if found {
      asNative.setKey(key, value: value, at: i.bucket)
    } else {
      asNative.initializeKey(key, value: value, at: i.bucket)
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
      migrateToNative(cocoaBuffer)
      return nativeUpdateValue(value, forKey: key)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativePointerToValue(at i: Index)
  -> UnsafeMutablePointer<Value> {
    // This is in a separate variable to make the uniqueness check work in
    // unoptimized builds; see https://bugs.swift.org/browse/SR-6437
    let bucketCount = asNative.bucketCount
    _ = ensureUniqueNative(withBucketCount: bucketCount)
    return asNative.values + i._asNative.bucket
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
      let cocoaIndex = i._asCocoa
      let cocoaKey = cocoaIndex.allKeys[cocoaIndex.currentKeyIndex]
      migrateToNative(cocoaStorage)
      let key = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
      let nativeIndex = asNative.index(forKey: key)!

      return nativePointerToValue(at: ._native(nativeIndex))
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativePointerToValue(
    forKey key: Key, insertingDefault defaultValue: () -> Value
  ) -> (inserted: Bool, pointer: UnsafeMutablePointer<Value>) {

    var (i, found) = asNative._find(key)
    if found {
      let pointer = nativePointerToValue(at: ._native(i))
      return (inserted: false, pointer: pointer)
    }

    let minCapacity = asNative.count + 1
    let (_, capacityChanged) = ensureUniqueNative(withCapacity: minCapacity)

    if capacityChanged {
      i = asNative._find(key).pos
    }

    asNative.initializeKey(key, value: defaultValue(), at: i.bucket)
    asNative.count += 1
    return (inserted: true, pointer: asNative.values + i.bucket)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func pointerToValue(
    forKey key: Key, insertingDefault defaultValue: () -> Value
  ) -> (inserted: Bool, pointer: UnsafeMutablePointer<Value>) {
    ensureNative()
    return nativePointerToValue(forKey: key, insertingDefault: defaultValue)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeInsert(
    _ value: Value, forKey key: Key
  ) -> (inserted: Bool, memberAfterInsert: Value) {

    var (i, found) = asNative._find(key)
    if found {
      return (inserted: false, memberAfterInsert: asNative.value(at: i.bucket))
    }

    let minCapacity = asNative.count + 1
    let (_, capacityChanged) = ensureUniqueNative(withCapacity: minCapacity)

    if capacityChanged {
      i = asNative._find(key).pos
    }

    asNative.initializeKey(key, value: value, at: i.bucket)
    asNative.count += 1

    return (inserted: true, memberAfterInsert: value)
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func insert(
    _ value: Value, forKey key: Key
  ) -> (inserted: Bool, memberAfterInsert: Value) {
    ensureNative()
    return nativeInsert(value, forKey: key)
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> Dictionary<Key, T>._Variant {
    if _fastPath(guaranteedNative) {
      return try .native(asNative.mapValues(transform))
    }

    switch self {
    case .native(let nativeDictionary):
      return try .native(nativeDictionary.mapValues(transform))
#if _runtime(_ObjC)
    case .cocoa(let cocoaDictionary):
      return try .native(cocoaDictionary.mapValues(transform))
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeMerge<S: Sequence>(
    _ keysAndValues: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    for (key, value) in keysAndValues {
      var (i, found) = asNative._find(key)

      if found {
        // This is in a separate variable to make the uniqueness check work in
        // unoptimized builds; see https://bugs.swift.org/browse/SR-6437
        let bucketCount = asNative.bucketCount
        _ = ensureUniqueNative(withBucketCount: bucketCount)
        do {
          let newValue = try combine(asNative.value(at: i.bucket), value)
          asNative.setKey(key, value: newValue, at: i.bucket)
        } catch _MergeError.keyCollision {
          fatalError("Duplicate values for key: '\(key)'")
        }
      } else {
        let minCapacity = asNative.count + 1
        let (_, capacityChanged) = ensureUniqueNative(withCapacity: minCapacity)
        if capacityChanged {
          i = asNative._find(key).pos
        }

        asNative.initializeKey(key, value: value, at: i.bucket)
        asNative.count += 1
      }
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func merge<S: Sequence>(
    _ keysAndValues: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    ensureNative()
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
      var (i, found) = asNative._find(key)
      if found {
        asNative.values[i.bucket].append(value)
      } else {
        let minCapacity = asNative.count + 1
        let (_, capacityChanged) = ensureUniqueNative(withCapacity: minCapacity)
        if capacityChanged {
          i = asNative._find(key).pos
        }

        asNative.initializeKey(key, value: [value], at: i.bucket)
        asNative.count += 1
      }
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeRemoveValue(forKey key: Key) -> Value? {
    var idealBucket = asNative._bucket(key)
    var (index, found) = asNative._find(key, startBucket: idealBucket)

    // Fast path: if the key is not present, we will not mutate the set,
    // so don't force unique buffer.
    if !found {
      return nil
    }

    // This is in a separate variable to make the uniqueness check work in
    // unoptimized builds; see https://bugs.swift.org/browse/SR-6437
    let bucketCount = asNative.bucketCount
    let (_, capacityChanged) = ensureUniqueNative(withBucketCount: bucketCount)
    var native = asNative
    if capacityChanged {
      idealBucket = native._bucket(key)
      (index, found) = native._find(key, startBucket: idealBucket)
      _sanityCheck(found, "key was lost during buffer migration")
    }
    let oldValue = native.value(at: index.bucket)
    native._delete(idealBucket: idealBucket, bucket: index.bucket)
    return oldValue
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeRemove(
    at index: _NativeDictionary<Key, Value>.Index
  ) -> Element {
    // This is in a separate variable to make the uniqueness check work in
    // unoptimized builds; see https://bugs.swift.org/browse/SR-6437
    let bucketCount = asNative.bucketCount
    // The provided index should be valid, so we will always mutate the buffer.
    // Request unique buffer.
    _ = ensureUniqueNative(withBucketCount: bucketCount)
    var native = asNative
    let result = native.assertingGet(at: index)
    let key = result.0
    let idealBucket = native._bucket(key)
    native._delete(idealBucket: idealBucket, bucket: index.bucket)
    return result
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func remove(at index: Index) -> Element {
    if _fastPath(guaranteedNative) {
      return nativeRemove(at: index._asNative)
    }

    switch self {
    case .native:
      return nativeRemove(at: index._asNative)
#if _runtime(_ObjC)
    case .cocoa(let cocoaDictionary):
      // We have to migrate the data first.  But after we do so, the Cocoa
      // index becomes useless, so get the key first.
      //
      // FIXME(performance): fuse data migration and element deletion into one
      // operation.
      let index = index._asCocoa
      let cocoaKey = index.allKeys[index.currentKeyIndex]
      migrateToNative(cocoaDictionary)
      let key = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
      let value = nativeRemoveValue(forKey: key)

      return (key, value._unsafelyUnwrappedUnchecked)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  internal mutating func removeValue(forKey key: Key) -> Value? {
    if _fastPath(guaranteedNative) {
      return nativeRemoveValue(forKey: key)
    }

    switch self {
    case .native:
      return nativeRemoveValue(forKey: key)
#if _runtime(_ObjC)
    case .cocoa(let cocoaDictionary):
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      if cocoaDictionary.maybeGet(cocoaKey) == nil {
        return nil
      }
      migrateToNative(cocoaDictionary)
      return nativeRemoveValue(forKey: key)
#endif
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func nativeRemoveAll() {
    if !isUniquelyReferenced() {
      self = .native(_NativeDictionary(_exactBucketCount: asNative.bucketCount))
      return
    }

    // We have already checked for the empty dictionary case and unique
    // reference, so we will always mutate the dictionary.
    var native = asNative
    native._removeAll()
  }

  @inlinable // FIXME(sil-serialize-all)
  internal mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    if count == 0 {
      return
    }

    if !keepCapacity {
      self = .native(_NativeDictionary())
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
    case .cocoa(let cocoaDictionary):
      self = .native(_NativeDictionary(minimumCapacity: cocoaDictionary.count))
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
    case .cocoa(let cocoaDictionary):
      return cocoaDictionary.count
#endif
    }
  }

  /// Returns an iterator over the `(Key, Value)` pairs.
  ///
  /// - Complexity: O(1).
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  internal func makeIterator() -> Dictionary<Key, Value>.Iterator {
    switch self {
    case .native(let dictionary):
      return ._native(dictionary.makeIterator())
#if _runtime(_ObjC)
    case .cocoa(let dictionary):
      return ._cocoa(dictionary.makeIterator())
#endif
    }
  }
}

extension _NativeDictionary {
  @_fixed_layout
  @usableFromInline
  internal struct Index {
    @usableFromInline
    internal var bucket: Int

    @inlinable
    internal init(bucket: Int) {
      self.bucket = bucket
    }
  }
}

extension _NativeDictionary.Index: Equatable {
  @inlinable
  internal static func == (
    lhs: _NativeDictionary.Index,
    rhs: _NativeDictionary.Index
  ) -> Bool {
    return lhs.bucket == rhs.bucket
  }
}

extension _NativeDictionary.Index: Comparable {
  @inlinable
  internal static func < (
    lhs: _NativeDictionary.Index,
    rhs: _NativeDictionary.Index
  ) -> Bool {
    return lhs.bucket < rhs.bucket
  }
}

#if _runtime(_ObjC)
extension _CocoaDictionary {
  @_fixed_layout // FIXME(sil-serialize-all)
  @usableFromInline
  internal struct Index {
    // Assumption: we rely on NSDictionary.getObjects when being
    // repeatedly called on the same NSDictionary, returning items in the same
    // order every time.
    // Similarly, the same assumption holds for NSSet.allObjects.

    /// A reference to the NSDictionary, which owns members in `allObjects`,
    /// or `allKeys`, for NSSet and NSDictionary respectively.
    @usableFromInline // FIXME(sil-serialize-all)
    internal let base: _CocoaDictionary
    // FIXME: swift-3-indexing-model: try to remove the cocoa reference, but
    // make sure that we have a safety check for accessing `allKeys`.  Maybe
    // move both into the dictionary/set itself.

    /// An unowned array of keys.
    @usableFromInline // FIXME(sil-serialize-all)
    internal var allKeys: _HeapBuffer<Int, AnyObject>

    /// Index into `allKeys`
    @usableFromInline // FIXME(sil-serialize-all)
    internal var currentKeyIndex: Int

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ base: _CocoaDictionary, startIndex: ()) {
      self.base = base
      self.allKeys = _stdlib_NSDictionary_allKeys(base.object)
      self.currentKeyIndex = 0
    }

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ base: _CocoaDictionary, endIndex: ()) {
      self.base = base
      self.allKeys = _stdlib_NSDictionary_allKeys(base.object)
      self.currentKeyIndex = allKeys.value
    }

    @inlinable // FIXME(sil-serialize-all)
    internal init(
      _ base: _CocoaDictionary,
      _ allKeys: _HeapBuffer<Int, AnyObject>,
      _ currentKeyIndex: Int
    ) {
      self.base = base
      self.allKeys = allKeys
      self.currentKeyIndex = currentKeyIndex
    }

    /// Returns the next consecutive value after `self`.
    ///
    /// - Precondition: The next value is representable.
    @inlinable // FIXME(sil-serialize-all)
    internal func successor() -> Index {
      // FIXME: swift-3-indexing-model: remove this method.
      _precondition(
        currentKeyIndex < allKeys.value, "Cannot increment endIndex")
      return Index(base, allKeys, currentKeyIndex + 1)
    }
  }
}

extension _CocoaDictionary.Index: Equatable {
  @inlinable
  internal static func == (
    lhs: _CocoaDictionary.Index,
    rhs: _CocoaDictionary.Index
  ) -> Bool {
    return lhs.currentKeyIndex == rhs.currentKeyIndex
  }
}

extension _CocoaDictionary.Index: Comparable {
  @inlinable
  internal static func < (
    lhs: _CocoaDictionary.Index,
    rhs: _CocoaDictionary.Index
  ) -> Bool {
    return lhs.currentKeyIndex < rhs.currentKeyIndex
  }
}
#endif

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
  @_fixed_layout
  public struct Index {
    // Index for native dictionary is efficient.  Index for bridged NSDictionary
    // is not, because neither NSEnumerator nor fast enumeration support moving
    // backwards.  Even if they did, there is another issue: NSEnumerator does
    // not support NSCopying, and fast enumeration does not document that it is
    // safe to copy the state.  So, we cannot implement Index that is a value
    // type for bridged NSDictionary in terms of Cocoa enumeration facilities.

    @_frozen
    @usableFromInline
    internal enum _Variant {
      case native(_NativeDictionary<Key, Value>.Index)
#if _runtime(_ObjC)
      case cocoa(_CocoaDictionary.Index)
#endif
    }

    @usableFromInline
    internal var _variant: _Variant

    @inlinable
    internal init(_variant: _Variant) {
      self._variant = _variant
    }
  }
}

extension Dictionary.Index {
  @inlinable
  internal static func _native(
    _ index: _NativeDictionary<Key, Value>.Index
  ) -> Dictionary.Index {
    return Dictionary.Index(_variant: .native(index))
  }

#if _runtime(_ObjC)
  @inlinable
  internal static func _cocoa(
    _ index: _CocoaDictionary.Index
  ) -> Dictionary.Index {
    return Dictionary.Index(_variant: .cocoa(index))
  }
#endif

  @usableFromInline @_transparent
  internal var _guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 && _canBeClass(Value.self) == 0
  }

  @usableFromInline @_transparent
  internal var _asNative: _NativeDictionary<Key, Value>.Index {
    switch _variant {
    case .native(let nativeIndex):
      return nativeIndex
#if _runtime(_ObjC)
    case .cocoa:
      _sanityCheckFailure("internal error: does not contain a native index")
#endif
    }
  }

#if _runtime(_ObjC)
  @usableFromInline @_transparent
  internal var _asCocoa: _CocoaDictionary.Index {
    switch _variant {
    case .native:
      _sanityCheckFailure("internal error: does not contain a Cocoa index")
    case .cocoa(let cocoaIndex):
      return cocoaIndex
    }
  }
#endif
}

extension Dictionary.Index: Equatable {
  @inlinable
  public static func == (
    lhs: Dictionary<Key, Value>.Index,
    rhs: Dictionary<Key, Value>.Index
  ) -> Bool {
    if _fastPath(lhs._guaranteedNative) {
      return lhs._asNative == rhs._asNative
    }

    switch (lhs._variant, rhs._variant) {
    case (.native(let lhsNative), .native(let rhsNative)):
      return lhsNative == rhsNative
  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      return lhsCocoa == rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different sets")
  #endif
    }
  }
}

extension Dictionary.Index: Comparable {
  @inlinable
  public static func < (
    lhs: Dictionary<Key, Value>.Index,
    rhs: Dictionary<Key, Value>.Index
  ) -> Bool {
    if _fastPath(lhs._guaranteedNative) {
      return lhs._asNative < rhs._asNative
    }

    switch (lhs._variant, rhs._variant) {
    case (.native(let lhsNative), .native(let rhsNative)):
      return lhsNative < rhsNative
  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      return lhsCocoa < rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different sets")
  #endif
    }
  }
}

extension Dictionary.Index: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
  #if _runtime(_ObjC)
    if _fastPath(_guaranteedNative) {
      hasher.combine(0 as UInt8)
      hasher.combine(_asNative.bucket)
      return
    }
    switch _variant {
    case .native(let nativeIndex):
      hasher.combine(0 as UInt8)
      hasher.combine(nativeIndex.bucket)
    case .cocoa(let cocoaIndex):
      hasher.combine(1 as UInt8)
      hasher.combine(cocoaIndex.currentKeyIndex)
    }
  #else
    hasher.combine(_asNative.bucket)
  #endif
  }
}

extension _NativeDictionary: Sequence {
  @usableFromInline
  @_fixed_layout
  internal struct Iterator {
    // For native buffer, we keep two indices to keep track of the iteration
    // progress and the buffer owner to make the buffer non-uniquely
    // referenced.
    //
    // Iterator is iterating over a frozen view of the collection
    // state, so it should keep its own reference to the buffer.
    @usableFromInline
    internal var index: Index
    @usableFromInline
    internal var endIndex: Index
    @usableFromInline
    internal let base: _NativeDictionary

    @inlinable
    init(_ base: _NativeDictionary) {
      self.index = base.startIndex
      self.endIndex = base.endIndex
      self.base = base
    }
  }

  @inlinable
  internal func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension _NativeDictionary.Iterator: IteratorProtocol {
  @usableFromInline
  internal typealias Element = (key: Key, value: Value)

  @inlinable
  internal mutating func next() -> Element? {
    guard index != endIndex else { return nil }
    let result = base.assertingGet(at: index)
    base.formIndex(after: &index)
    return result
  }
}

#if _runtime(_ObjC)
extension _CocoaDictionary: Sequence {
  @usableFromInline
  final internal class Iterator {
    // Cocoa Dictionary iterator has to be a class, otherwise we cannot
    // guarantee that the fast enumeration struct is pinned to a certain memory
    // location.

    // This stored property should be stored at offset zero.  There's code below
    // relying on this.
    internal var _fastEnumerationState: _SwiftNSFastEnumerationState =
      _makeSwiftNSFastEnumerationState()

    // This stored property should be stored right after
    // `_fastEnumerationState`.  There's code below relying on this.
    internal var _fastEnumerationStackBuf = _CocoaFastEnumerationStackBuf()

    internal let base: _CocoaDictionary

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

    internal init(_ base: _CocoaDictionary) {
      self.base = base
    }
  }

  @usableFromInline
  @_effects(releasenone)
  internal func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension _CocoaDictionary.Iterator: IteratorProtocol {
  @usableFromInline
  internal typealias Element = (key: AnyObject, value: AnyObject)

  @usableFromInline
  internal func next() -> Element? {
    if itemIndex < 0 {
      return nil
    }
    let base = self.base
    if itemIndex == itemCount {
      let stackBufCount = _fastEnumerationStackBuf.count
      // We can't use `withUnsafeMutablePointer` here to get pointers to
      // properties, because doing so might introduce a writeback storage, but
      // fast enumeration relies on the pointer identity of the enumeration
      // state struct.
      itemCount = base.object.countByEnumerating(
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
    let value: AnyObject = base.object.objectFor(key)!
    return (key, value)
  }
}
#endif

extension Dictionary {
  /// An iterator over the members of a `Dictionary<Key, Value>`.
  @_fixed_layout
  public struct Iterator {
    // Dictionary has a separate IteratorProtocol and Index because of
    // efficiency and implementability reasons.
    //
    // Native dictionaries have efficient indices.
    // Bridged NSDictionary instances don't.
    //
    // Even though fast enumeration is not suitable for implementing
    // Index, which is multi-pass, it is suitable for implementing a
    // IteratorProtocol, which is being consumed as iteration proceeds.

    @usableFromInline
    @_frozen
    internal enum _Variant {
      case native(_NativeDictionary<Key, Value>.Iterator)
#if _runtime(_ObjC)
      case cocoa(_CocoaDictionary.Iterator)
#endif
    }

    @usableFromInline
    internal var _variant: _Variant

    @inlinable
    internal init(_variant: _Variant) {
      self._variant = _variant
    }
  }
}

extension Dictionary.Iterator {
  @inlinable
  internal static func _native(
    _ iterator: _NativeDictionary<Key, Value>.Iterator
  ) -> Dictionary.Iterator {
    return Dictionary.Iterator(_variant: .native(iterator))
  }

#if _runtime(_ObjC)
  @inlinable
  internal static func _cocoa(
    _ iterator: _CocoaDictionary.Iterator
  ) -> Dictionary.Iterator {
    return Dictionary.Iterator(_variant: .cocoa(iterator))
  }
#endif

  @usableFromInline @_transparent
  internal var _guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 || _canBeClass(Value.self) == 0
  }

  @usableFromInline @_transparent
  internal var _asNative: _NativeDictionary<Key, Value>.Iterator {
    get {
      switch _variant {
      case .native(let nativeIterator):
        return nativeIterator
#if _runtime(_ObjC)
      case .cocoa:
        _sanityCheckFailure("internal error: does not contain a native index")
#endif
      }
    }
    set {
      self._variant = .native(newValue)
    }
  }
}

extension Dictionary.Iterator: IteratorProtocol {
  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Once `nil` has been returned, all subsequent calls return `nil`.
  @inlinable
  @inline(__always)
  public mutating func next() -> (key: Key, value: Value)? {
    if _fastPath(_guaranteedNative) {
      return _asNative.next()
    }

    switch _variant {
    case .native:
      return _asNative.next()
#if _runtime(_ObjC)
    case .cocoa(let cocoaIterator):
      if let (cocoaKey, cocoaValue) = cocoaIterator.next() {
        let nativeKey = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
        let nativeValue = _forceBridgeFromObjectiveC(cocoaValue, Value.self)
        return (nativeKey, nativeValue)
      }
      return nil
#endif
    }
  }
}

extension Dictionary.Iterator: CustomReflectable {
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
@_fixed_layout
public struct _DictionaryBuilder<Key: Hashable, Value> {
  @usableFromInline
  internal var _target: _NativeDictionary<Key, Value>
  @usableFromInline
  internal let _requestedCount: Int
  @usableFromInline
  internal var _actualCount: Int

  @inlinable
  public init(count: Int) {
    _target = _NativeDictionary(minimumCapacity: count)
    _requestedCount = count
    _actualCount = 0
  }

  @inlinable
  public mutating func add(key newKey: Key, value: Value) {
    _target.unsafeAddNew(key: newKey, value: value)
    _actualCount += 1
  }

  @inlinable
  public mutating func take() -> Dictionary<Key, Value> {
    _precondition(_actualCount >= 0,
      "Cannot take the result twice")
    _precondition(_actualCount == _requestedCount,
      "The number of members added does not match the promised count")

    // Finish building the `Dictionary`.
    _target.count = _actualCount

    // Prevent taking the result twice.
    _actualCount = -1
    var result = _NativeDictionary<Key, Value>()
    swap(&result, &_target)
    return Dictionary(_native: result)
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
    return _variant.capacity
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
    _variant.reserveCapacity(minimumCapacity)
    _sanityCheck(self.capacity >= minimumCapacity)
  }
}

//===--- Bridging ---------------------------------------------------------===//

#if _runtime(_ObjC)
extension Dictionary {
  @inlinable // FIXME(sil-serialize-all)
  public func _bridgeToObjectiveCImpl() -> _NSDictionaryCore {
    switch _variant {
    case .native(let nativeDictionary):
      return nativeDictionary.bridged()
    case .cocoa(let cocoaDictionary):
      return cocoaDictionary.object
    }
  }

  /// Returns the native Dictionary hidden inside this NSDictionary;
  /// returns nil otherwise.
  public static func _bridgeFromObjectiveCAdoptingNativeStorageOf(
    _ s: AnyObject
  ) -> Dictionary<Key, Value>? {

    // Try all three NSDictionary impls that we currently provide.

    if let deferred = s as? _SwiftDeferredNSDictionary<Key, Value> {
      return Dictionary(_native: deferred.native)
    }

    typealias HTNDS = _HashableTypedNativeDictionaryStorage<Key, Value>
    if let nativeStorage = s as? HTNDS {
      return Dictionary(_native: _NativeDictionary(_storage: nativeStorage))
    }

    if s === _RawNativeDictionaryStorage.empty {
      return Dictionary()
    }

    // FIXME: what if `s` is native storage, but for different key/value type?
    return nil
  }
}
#endif

public typealias DictionaryIndex<Key: Hashable, Value> =
  Dictionary<Key, Value>.Index
public typealias DictionaryIterator<Key: Hashable, Value> =
  Dictionary<Key, Value>.Iterator
