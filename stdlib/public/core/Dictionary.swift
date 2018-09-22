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
// bitmap that marks valid entries. An unoccupied entry marks the end of a
// chain.  There is always at least one unoccupied entry among the
// buckets. `Dictionary` does not use tombstones.
//
// In addition to the native storage, `Dictionary` can also wrap an
// `NSDictionary` in order to allow bridging `NSDictionary` to `Dictionary` in
// `O(1)`.
//
// Native dictionary storage uses a data structure like this::
//
//   struct Dictionary<K,V>
//   +------------------------------------------------+
//   | enum Dictionary<K,V>._Variant                  |
//   | +--------------------------------------------+ |
//   | | [struct _NativeDictionary<K,V>             | |
//   | +---|----------------------------------------+ |
//   +----/-------------------------------------------+
//       /
//      |
//      V
//   class _RawDictionaryStorage
//   +-----------------------------------------------------------+
//   | <isa>                                                     |
//   | <refcount>                                                |
//   | _count                                                    |
//   | _capacity                                                 |
//   | _scale                                                    |
//   | _seed                                                     |
//   | _rawKeys                                                  |
//   | _rawValue                                                 |
//   | [inline bitset of occupied entries]                       |
//   | [inline array of keys]                                    |
//   | [inline array of values]                                  |
//   +-----------------------------------------------------------+
//
// Cocoa storage uses a data structure like this:
//
//   struct Dictionary<K,V>
//   +----------------------------------------------+
//   | enum Dictionary<K,V>._Variant                |
//   | +----------------------------------------+   |
//   | | [ struct _CocoaDictionary              |   |
//   | +---|------------------------------------+   |
//   +----/-----------------------------------------+
//       /
//      |
//      V
//   class NSDictionary
//   +--------------+
//   | [refcount#1] |
//   | etc.         |
//   +--------------+
//     ^
//     |
//      \  struct _CocoaDictionary.Index
//   +--|------------------------------------+
//   |  * base: _CocoaDictionary             |
//   |  allKeys: array of all keys           |
//   |  currentKeyIndex: index into allKeys  |
//   +---------------------------------------+
//
//
// The Native Kinds of Storage
// ---------------------------
//
// The native backing store is represented by three different classes:
// * `_RawDictionaryStorage`
// * `_EmptyDictionarySingleton` (extends Raw)
// * `_DictionaryStorage<K: Hashable, V>` (extends Raw)
//
// (Hereafter `Raw`, `Empty`, and `Storage`, respectively)
//
// In a less optimized implementation, `Raw` and `Empty` could be eliminated, as
// they exist only to provide special-case behaviors.
//
// `Empty` is the a type-punned empty singleton storage. Its single instance is
// created by the runtime during process startup. Because we use the same
// instance for all empty dictionaries, it cannot declare type parameters.
//
// `Storage` provides backing storage for regular native dictionaries. All
// non-empty native dictionaries use an instance of `Storage` to store their
// elements. `Storage` is a generic class with a nontrivial deinit.
//
// `Raw` is the base class for both `Empty` and `Storage`. It defines a full set
// of ivars to access dictionary contents. Like `Empty`, `Raw` is also
// non-generic; the base addresses it stores are represented by untyped raw
// pointers. The only reason `Raw` exists is to allow `_NativeDictionary` to
// treat `Empty` and `Storage` in a unified way.
//
// Storage classes don't contain much logic; `Raw` in particular is just a
// collection of ivars. `Storage` provides allocation/deinitialization logic,
// while `Empty`/`Storage` implement NSDictionary methods. All other operations
// are actually implemented by the `_NativeDictionary` and `_HashTable` structs.
//
// The `_HashTable` struct provides low-level hash table metadata operations.
// (Lookups, iteration, insertion, removal.) It owns and maintains the
// tail-allocated bitmap.
//
// `_NativeDictionary` implements the actual Dictionary operations.  It
// consists of a reference to a `Raw` instance, to allow for the possibility of
// the empty singleton.
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
// Bridging to `Dictionary<AnyHashable, AnyObject>` takes `O(n)` time, as the
// keys need to be fully rehashed after conversion to `AnyHashable`.
//
// Bridging `NSDictionary` to `Dictionary<Key, Value>` is O(1) if both Key and
// Value are bridged verbatim.
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
// `Dictionary<Element, NSObject>`.
//

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

  @inlinable
  internal init(_native: _NativeDictionary<Key, Value>) {
    _variant = .native(_native)
  }

#if _runtime(_ObjC)
  @inlinable
  internal init(_cocoa: _CocoaDictionary) {
    _variant = .cocoa(_cocoa)
  }

  /// Private initializer used for bridging.
  ///
  /// Only use this initializer when both conditions are true:
  ///
  /// * it is statically known that the given `NSDictionary` is immutable;
  /// * `Key` and `Value` are bridged verbatim to Objective-C (i.e.,
  ///   are reference types).
  @inlinable
  public // SPI(Foundation)
  init(_immutableCocoaDictionary: _NSDictionary) {
    _sanityCheck(
      _isBridgedVerbatimToObjectiveC(Key.self) &&
      _isBridgedVerbatimToObjectiveC(Value.self),
      """
      Dictionary can be backed by NSDictionary buffer only when both Key \
      and Value are bridged verbatim to Objective-C
      """)
    self.init(_cocoa: _CocoaDictionary(_immutableCocoaDictionary))
  }
#endif

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
    _variant = .native(_NativeDictionary(capacity: minimumCapacity))
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
      return
    }
    var native = _NativeDictionary<Key, Value>(
      capacity: keysAndValues.underestimatedCount)
    // '_MergeError.keyCollision' is caught and handled with an appropriate
    // error message one level down, inside native.merge(_:...).
    try! native.merge(
      keysAndValues,
      isUnique: true,
      uniquingKeysWith: { _, _ in throw _MergeError.keyCollision })
    self.init(_native: native)
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
    var native = _NativeDictionary<Key, Value>(
      capacity: keysAndValues.underestimatedCount)
    try native.merge(keysAndValues, isUnique: true, uniquingKeysWith: combine)
    self.init(_native: native)
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
    try self.init(_native: _NativeDictionary(grouping: values, by: keyForValue))
  }
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
  public __consuming func filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> [Key: Value] {
    // FIXME(performance): Try building a bitset of elements to keep, so that we
    // eliminate rehashings during insertion.
    var result = _NativeDictionary<Key, Value>()
    for element in self {
      if try isIncluded(element) {
        result.insertNew(key: element.key, value: element.value)
      }
    }
    return Dictionary(_native: result)
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
    return _variant.lookup(position)
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
    var it = makeIterator()
    return it.next()
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
      return _variant.lookup(key)
    }
    set(newValue) {
      if let x = newValue {
        _variant.setValue(x, forKey: key)
      } else {
        removeValue(forKey: key)
      }
    }
    _modify {
      // FIXME: This code should be moved to _variant, with Dictionary.subscript
      // yielding `&_variant[key]`.

      let (index, found) = _variant.mutatingFind(key)

      // FIXME: Mark this entry as being modified in hash table metadata
      // so that lldb can recognize it's not valid.

      // Move the old value (if any) out of storage, wrapping it into an
      // optional before yielding it.
      let native = _variant.asNative
      var value: Value? = found ? (native._values + index.bucket).move() : nil
      yield &value

      // Value is now potentially different. Check which one of the four
      // possible cases apply.
      switch (value, found) {
      case (let value?, true): // Mutation
        // Initialize storage to new value.
        (native._values + index.bucket).initialize(to: value)
      case (let value?, false): // Insertion
        // Insert the new entry at the correct place.
        // We've already ensured we have enough capacity.
        native._insert(at: index, key: key, value: value)
      case (nil, true): // Removal
        // We've already removed the value; deinitialize and remove the key too.
        (native._values + index.bucket).deinitialize(count: 1)
        native._delete(at: index)
      case (nil, false): // Noop
        // Easy!
        break
      }
      _fixLifetime(self)
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
    let native = _NativeDictionary<Key, Value>(capacity: elements.count)
    for (key, value) in elements {
      let (index, found) = native.find(key)
      _precondition(!found, "Dictionary literal contains duplicate keys")
      native._insert(at: index, key: key, value: value)
    }
    self.init(_native: native)
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
      return _variant.lookup(key) ?? defaultValue()
    }
    _modify {
      let index = _variant.lookupOrInsert(key, default: defaultValue)
      let address = _variant.asNative._values + index.bucket
      yield &address.pointee
      _fixLifetime(self)
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
    return try Dictionary<Key, T>(_native: _variant.mapValues(transform))
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
    let result: _NativeDictionary<Key, T> =
      try self.reduce(into: _NativeDictionary<Key, T>()) { (result, element) in
      if let value = try transform(element.value) {
        result.insertNew(key: element.key, value: value)
      }
    }
    return Dictionary<Key, T>(_native: result)
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
  public mutating func updateValue(_ value: Value, forKey key: Key) -> Value? {
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
    return Keys(_dictionary: self)
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
      return Values(_dictionary: self)
    }
    set {
      self._variant = newValue._variant
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
    internal init(_dictionary: Dictionary) {
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
      return _variant.key(at: position)
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
      return _variant.contains(element)
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
    internal init(_dictionary: Dictionary) {
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
        return _variant.value(at: position)
      }
      _modify {
        let index = _variant.ensureUniqueNative(preserving: position)
        let address = _variant.asNative._values + index.bucket
        yield &address.pointee
        _fixLifetime(self)
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
        let (index, found) = rhsNative.find(k)
        guard found, rhsNative.uncheckedValue(at: index) == v else {
          return false
        }
      }
      return true

  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      return lhsCocoa == rhsCocoa

    case (.native(let lhsNative), .cocoa(let rhsCocoa)):
      if lhsNative.count != rhsCocoa.count {
        return false
      }

      defer { _fixLifetime(lhsNative) }
      for index in lhsNative.hashTable {
        let (key, value) = lhsNative.lookup(index)
        guard
          let rhsValue = rhsCocoa.lookup(_bridgeAnythingToObjectiveC(key)),
          value == _forceBridgeFromObjectiveC(rhsValue, Value.self)
        else {
          return false
        }
      }
      return true

    case (.cocoa, .native):
      return rhs == lhs
  #endif
    }
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

  internal func _rawHashValue(_seed: Hasher._Seed) -> Int {
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

/// This protocol is only used for compile-time checks that
/// every buffer type implements all required operations.
internal protocol _DictionaryBuffer {
  associatedtype Key
  associatedtype Value
  associatedtype Index

  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after i: Index) -> Index
  func index(forKey key: Key) -> Index?
  var count: Int { get }

  func contains(_ key: Key) -> Bool
  func lookup(_ key: Key) -> Value?
  func lookup(_ index: Index) -> (key: Key, value: Value)
  func key(at index: Index) -> Key
  func value(at index: Index) -> Value
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
internal class _RawDictionaryStorage: _SwiftNativeNSDictionary {
  /// The current number of occupied entries in this dictionary.
  @usableFromInline
  @nonobjc
  internal final var _count: Int

  /// The maximum number of elements that can be inserted into this set without
  /// exceeding the hash table's maximum load factor.
  @usableFromInline
  @nonobjc
  internal final var _capacity: Int

  /// The scale of this dictionary. The number of buckets is 2 raised to the
  /// power of `scale`.
  @usableFromInline
  @nonobjc
  internal final var _scale: Int

  @usableFromInline
  internal final var _seed: Hasher._Seed

  @usableFromInline
  @nonobjc
  internal final var _rawKeys: UnsafeMutableRawPointer

  @usableFromInline
  @nonobjc
  internal final var _rawValues: UnsafeMutableRawPointer

  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @nonobjc
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }

  @inlinable
  @nonobjc
  internal final var _bucketCount: Int {
    @inline(__always) get { return 1 &<< _scale }
  }

  @inlinable
  @nonobjc
  internal final var _metadata: UnsafeMutablePointer<_HashTable.Word> {
    @inline(__always) get {
      let address = Builtin.projectTailElems(self, _HashTable.Word.self)
      return UnsafeMutablePointer(address)
    }
  }

  // The _HashTable struct contains pointers into tail-allocated storage, so
  // this is unsafe and needs `_fixLifetime` calls in the caller.
  @inlinable
  @nonobjc
  internal final var _hashTable: _HashTable {
    @inline(__always) get {
      return _HashTable(words: _metadata, bucketCount: _bucketCount)
    }
  }
}

/// The storage class for the singleton empty set.
/// The single instance of this class is created by the runtime.
@_fixed_layout
@usableFromInline
internal class _EmptyDictionarySingleton: _RawDictionaryStorage {
  @nonobjc
  internal override init(_doNotCallMe: ()) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }

#if _runtime(_ObjC)
  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }
#endif
}

#if _runtime(_ObjC)
extension _EmptyDictionarySingleton: _NSDictionaryCore {
  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return self
  }

  @objc
  internal var count: Int {
    return 0
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

  @objc(objectForKey:)
  internal func object(forKey aKey: AnyObject) -> AnyObject? {
    return nil
  }

  @objc(keyEnumerator)
  internal func keyEnumerator() -> _NSEnumerator {
    return _SwiftEmptyNSEnumerator()
  }

  @objc(getObjects:andKeys:count:)
  internal func getObjects(
    _ objects: UnsafeMutablePointer<AnyObject>?,
    andKeys keys: UnsafeMutablePointer<AnyObject>?,
    count: Int) {
    // Do nothing, we're empty
  }
}
#endif

extension _RawDictionaryStorage {
  /// The empty singleton that is used for every single Dictionary that is
  /// created without any elements. The contents of the storage should never
  /// be mutated.
  @inlinable
  @nonobjc
  internal static var empty: _EmptyDictionarySingleton {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyDictionarySingleton))
  }
}

// See the docs at the top of this file for a description of this type
@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
final internal class _DictionaryStorage<Key: Hashable, Value>
  : _RawDictionaryStorage, _NSDictionaryCore {
  // This type is made with allocWithTailElems, so no init is ever called.
  // But we still need to have an init to satisfy the compiler.
  @nonobjc
  override internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }

#if _runtime(_ObjC)
  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("This class cannot be directly initialized")
  }
#endif

  deinit {
    guard _count > 0 else { return }
    if !_isPOD(Key.self) {
      let keys = self._keys
      for index in _hashTable {
        (keys + index.bucket).deinitialize(count: 1)
      }
    }
    if !_isPOD(Value.self) {
      let values = self._values
      for index in _hashTable {
        (values + index.bucket).deinitialize(count: 1)
      }
    }
    _count = 0
    _fixLifetime(self)
  }

  @inlinable
  final internal var _keys: UnsafeMutablePointer<Key> {
    @inline(__always)
    get {
      return self._rawKeys.assumingMemoryBound(to: Key.self)
    }
  }

  @inlinable
  final internal var _values: UnsafeMutablePointer<Value> {
    @inline(__always)
    get {
      return self._rawValues.assumingMemoryBound(to: Value.self)
    }
  }

  internal var asNative: _NativeDictionary<Key, Value> {
    return _NativeDictionary(self)
  }

  @usableFromInline
  @_effects(releasenone)
  internal static func reallocate(
    original: _RawDictionaryStorage,
    capacity: Int
  ) -> (storage: _DictionaryStorage, rehash: Bool) {
    _sanityCheck(capacity >= original._count)
    let scale = _HashTable.scale(forCapacity: capacity)
    let rehash = (scale != original._scale)
    let newStorage = _DictionaryStorage<Key, Value>.allocate(scale: scale)
    return (newStorage, rehash)
  }

  @usableFromInline
  @_effects(releasenone)
  static internal func allocate(capacity: Int) -> _DictionaryStorage {
    let scale = _HashTable.scale(forCapacity: capacity)
    return allocate(scale: scale)
  }

  static internal func allocate(scale: Int) -> _DictionaryStorage {
    // The entry count must be representable by an Int value; hence the scale's
    // peculiar upper bound.
    _sanityCheck(scale >= 0 && scale < Int.bitWidth - 1)

    let bucketCount = 1 &<< scale
    let wordCount = _UnsafeBitset.wordCount(forCapacity: bucketCount)
    let storage = Builtin.allocWithTailElems_3(
      _DictionaryStorage<Key, Value>.self,
      wordCount._builtinWordValue, _HashTable.Word.self,
      bucketCount._builtinWordValue, Key.self,
      bucketCount._builtinWordValue, Value.self)

    let metadataAddr = Builtin.projectTailElems(storage, _HashTable.Word.self)
    let keysAddr = Builtin.getTailAddr_Word(
      metadataAddr, wordCount._builtinWordValue, _HashTable.Word.self,
      Key.self)
    let valuesAddr = Builtin.getTailAddr_Word(
      keysAddr, bucketCount._builtinWordValue, Key.self,
      Value.self)
    storage._count = 0
    storage._capacity = _HashTable.capacity(forScale: scale)
    storage._scale = scale
    storage._rawKeys = UnsafeMutableRawPointer(keysAddr)
    storage._rawValues = UnsafeMutableRawPointer(valuesAddr)

    // We use a slightly different hash seed whenever we change the size of the
    // hash table, so that we avoid certain copy operations becoming quadratic,
    // without breaking value semantics. (For background details, see
    // https://bugs.swift.org/browse/SR-3268)

    // FIXME: Use true per-instance seeding instead. Per-capacity seeding still
    // leaves hash values the same in same-sized tables, which may affect
    // operations on two tables at once. (E.g., union.)
    storage._seed = (
      Hasher._seed.0 ^ UInt64(truncatingIfNeeded: scale),
      Hasher._seed.1)
    // Initialize hash table metadata.
    storage._hashTable.clear()
    return storage
  }

#if _runtime(_ObjC)
  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return self
  }

  @objc
  internal var count: Int {
    return _count
  }

  @objc(keyEnumerator)
  internal func keyEnumerator() -> _NSEnumerator {
    return _SwiftDictionaryNSEnumerator<Key, Value>(asNative)
  }

  @objc(countByEnumeratingWithState:objects:count:)
  internal func countByEnumerating(
    with state: UnsafeMutablePointer<_SwiftNSFastEnumerationState>,
    objects: UnsafeMutablePointer<AnyObject>?, count: Int
  ) -> Int {
    var theState = state.pointee
    if theState.state == 0 {
      theState.state = 1 // Arbitrary non-zero value.
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(objects)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      theState.extra.0 = CUnsignedLong(asNative.startIndex.bucket)
    }

    // Test 'objects' rather than 'count' because (a) this is very rare anyway,
    // and (b) the optimizer should then be able to optimize away the
    // unwrapping check below.
    if _slowPath(objects == nil) {
      return 0
    }

    let unmanagedObjects = _UnmanagedAnyObjectArray(objects!)
    var index = _HashTable.Index(bucket: Int(theState.extra.0))
    let endIndex = asNative.endIndex
    _precondition(index == endIndex || _hashTable.isOccupied(index))
    var stored = 0
    for i in 0..<count {
      if index == endIndex { break }

      let key = asNative.uncheckedKey(at: index)
      unmanagedObjects[i] = _bridgeAnythingToObjectiveC(key)

      stored += 1
      index = asNative.index(after: index)
    }
    theState.extra.0 = CUnsignedLong(index.bucket)
    state.pointee = theState
    return stored
  }

  @objc(objectForKey:)
  internal func object(forKey aKey: AnyObject) -> AnyObject? {
    guard let nativeKey = _conditionallyBridgeFromObjectiveC(aKey, Key.self)
    else { return nil }

    let (index, found) = asNative.find(nativeKey)
    guard found else { return nil }
    let value = asNative.uncheckedValue(at: index)
    return _bridgeAnythingToObjectiveC(value)
  }

  @objc(getObjects:andKeys:count:)
  internal func getObjects(
    _ objects: UnsafeMutablePointer<AnyObject>?,
    andKeys keys: UnsafeMutablePointer<AnyObject>?,
    count: Int) {
    _precondition(count >= 0, "Invalid count")
    guard count > 0 else { return }
    var i = 0 // Current position in the output buffers
    switch (_UnmanagedAnyObjectArray(keys), _UnmanagedAnyObjectArray(objects)) {
    case (let unmanagedKeys?, let unmanagedObjects?):
      for (key, value) in asNative {
        unmanagedObjects[i] = _bridgeAnythingToObjectiveC(value)
        unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
        i += 1
        guard i < count else { break }
      }
    case (let unmanagedKeys?, nil):
      for (key, _) in asNative {
        unmanagedKeys[i] = _bridgeAnythingToObjectiveC(key)
        i += 1
        guard i < count else { break }
      }
    case (nil, let unmanagedObjects?):
      for (_, value) in asNative {
        unmanagedObjects[i] = _bridgeAnythingToObjectiveC(value)
        i += 1
        guard i < count else { break }
      }
    case (nil, nil):
      // Do nothing.
      break
    }
  }
#endif
}

/// A wrapper around _RawDictionaryStorage that provides most of the
/// implementation of Dictionary.
@usableFromInline
@_fixed_layout
internal struct _NativeDictionary<Key: Hashable, Value> {
  @usableFromInline
  internal typealias Element = (key: Key, value: Value)

  /// See this comments on _RawDictionaryStorage and its subclasses to
  /// understand why we store an untyped storage here.
  @usableFromInline
  internal var _storage: _RawDictionaryStorage

  /// Constructs an instance from the empty singleton.
  @inlinable
  internal init() {
    self._storage = _RawDictionaryStorage.empty
  }

  /// Constructs a dictionary adopting the given storage.
  @inlinable
  internal init(_ storage: _RawDictionaryStorage) {
    self._storage = storage
  }

  @usableFromInline
  @_effects(releasenone)
  internal init(capacity: Int) {
    let scale = _HashTable.scale(forCapacity: capacity)
    self._storage = _DictionaryStorage<Key, Value>.allocate(scale: scale)
  }

#if _runtime(_ObjC)
  @inlinable
  internal init(_ cocoa: _CocoaDictionary) {
    self.init(cocoa, capacity: cocoa.count)
  }

  @inlinable
  internal init(_ cocoa: _CocoaDictionary, capacity: Int) {
    _sanityCheck(cocoa.count <= capacity)
    self.init(capacity: capacity)
    for (key, value) in cocoa {
      insertNew(
        key: _forceBridgeFromObjectiveC(key, Key.self),
        value: _forceBridgeFromObjectiveC(value, Value.self))
    }
  }
#endif
}

extension _NativeDictionary { // Primitive fields
  @inlinable
  internal var capacity: Int {
    @inline(__always)
    get {
      return _assumeNonNegative(_storage._capacity)
    }
  }

  @inlinable
  internal var hashTable: _HashTable {
    @inline(__always) get {
      return _storage._hashTable
    }
  }

  // This API is unsafe and needs a `_fixLifetime` in the caller.
  @inlinable
  internal var _keys: UnsafeMutablePointer<Key> {
    return _storage._rawKeys.assumingMemoryBound(to: Key.self)
  }

  @inlinable
  internal var _values: UnsafeMutablePointer<Value> {
    return _storage._rawValues.assumingMemoryBound(to: Value.self)
  }
}

extension _NativeDictionary { // Low-level unchecked operations
  @inlinable
  @inline(__always)
  internal func uncheckedKey(at index: Index) -> Key {
    defer { _fixLifetime(self) }
    _sanityCheck(hashTable.isOccupied(index))
    return _keys[index.bucket]
  }

  @inlinable
  @inline(__always)
  internal func uncheckedValue(at index: Index) -> Value {
    defer { _fixLifetime(self) }
    _sanityCheck(hashTable.isOccupied(index))
    return _values[index.bucket]
  }

  @usableFromInline
  @inline(__always)
  internal func uncheckedInitialize(
    at index: Index,
    toKey key: Key,
    value: Value) {
    defer { _fixLifetime(self) }
    _sanityCheck(hashTable.isValid(index))
    (_keys + index.bucket).initialize(to: key)
    (_values + index.bucket).initialize(to: value)
  }

  @usableFromInline
  @inline(__always)
  internal func uncheckedDestroy(at index: Index) {
    defer { _fixLifetime(self) }
    _sanityCheck(hashTable.isOccupied(index))
    (_keys + index.bucket).deinitialize(count: 1)
    (_values + index.bucket).deinitialize(count: 1)
  }
}

extension _NativeDictionary { // Low-level lookup operations
  @inlinable
  @inline(__always)
  internal func hashValue(for key: Key) -> Int {
    return key._rawHashValue(seed: _storage._seed)
  }

  @inlinable
  @inline(__always)
  internal func find(_ key: Key) -> (index: Index, found: Bool) {
    return find(key, hashValue: self.hashValue(for: key))
  }

  /// Search for a given element, assuming it has the specified hash value.
  ///
  /// If the element is not present in this set, return the position where it
  /// could be inserted.
  @inlinable
  @inline(__always)
  internal func find(
    _ key: Key,
    hashValue: Int
  ) -> (index: Index, found: Bool) {
    let hashTable = self.hashTable
    var index = hashTable.idealIndex(forHashValue: hashValue)
    while hashTable._isOccupied(index) {
      if uncheckedKey(at: index) == key {
        return (index, true)
      }
      index = hashTable.index(wrappedAfter: index)
    }
    return (index, false)
  }
}

extension _NativeDictionary { // ensureUnique
  @inlinable
  internal mutating func resize(capacity: Int) {
    let capacity = Swift.max(capacity, self.capacity)
    let result = _NativeDictionary(
      _DictionaryStorage<Key, Value>.allocate(capacity: capacity))
    if count > 0 {
      for index in hashTable {
        let key = (_keys + index.bucket).move()
        let value = (_values + index.bucket).move()
        result._unsafeInsertNew(key: key, value: value)
      }
      // Clear out old storage, ensuring that its deinit won't overrelease the
      // elements we've just moved out.
      _storage._hashTable.clear()
      _storage._count = 0
    }
    _storage = result._storage
  }

  @inlinable
  internal mutating func copy(capacity: Int) -> Bool {
    let capacity = Swift.max(capacity, self.capacity)
    let (newStorage, rehash) = _DictionaryStorage<Key, Value>.reallocate(
      original: _storage,
      capacity: capacity)
    let result = _NativeDictionary(newStorage)
    if count > 0 {
      if rehash {
        for index in hashTable {
          result._unsafeInsertNew(
            key: self.uncheckedKey(at: index),
            value: self.uncheckedValue(at: index))
        }
      } else {
        result.hashTable.copyContents(of: hashTable)
        result._storage._count = self.count
        for index in hashTable {
          let key = uncheckedKey(at: index)
          let value = uncheckedValue(at: index)
          result.uncheckedInitialize(at: index, toKey: key, value: value)
        }
      }
    }
    _storage = result._storage
    return rehash
  }

  /// Ensure storage of self is uniquely held and can hold at least `capacity`
  /// elements. Returns true iff contents were rehashed.
  @inlinable
  @inline(__always)
  internal mutating func ensureUnique(isUnique: Bool, capacity: Int) -> Bool {
    if _fastPath(capacity <= self.capacity && isUnique) {
      return false
    }
    guard isUnique else {
      return copy(capacity: capacity)
    }
    resize(capacity: capacity)
    return true
  }

  @inlinable
  internal mutating func reserveCapacity(_ capacity: Int, isUnique: Bool) {
    _ = ensureUnique(isUnique: isUnique, capacity: capacity)
  }
}

extension _NativeDictionary: _DictionaryBuffer {
  @usableFromInline
  internal typealias Index = _HashTable.Index

  @inlinable
  internal var startIndex: Index {
    return hashTable.startIndex
  }

  @inlinable
  internal var endIndex: Index {
    return hashTable.endIndex
  }

  @inlinable
  internal func index(after index: Index) -> Index {
    return hashTable.index(after: index)
  }

  @inlinable
  internal func index(forKey key: Key) -> Index? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }
    let (index, found) = find(key)
    return found ? index : nil
  }

  @inlinable
  internal var count: Int {
    @inline(__always) get {
      return _assumeNonNegative(_storage._count)
    }
  }

  @inlinable
  @inline(__always)
  func contains(_ key: Key) -> Bool {
    return find(key).found
  }

  @inlinable
  @inline(__always)
  func lookup(_ key: Key) -> Value? {
    if count == 0 {
      // Fast path that avoids computing the hash of the key.
      return nil
    }
    let (index, found) = self.find(key)
    return found ? self.uncheckedValue(at: index) : nil
  }

  @inlinable
  @inline(__always)
  func lookup(_ index: Index) -> (key: Key, value: Value) {
    _precondition(hashTable.isOccupied(index),
      "Attempting to access Dictionary elements using an invalid Index")
    let key = self.uncheckedKey(at: index)
    let value = self.uncheckedValue(at: index)
    return (key, value)
  }

  @inlinable
  @inline(__always)
  func key(at index: Index) -> Key {
    _precondition(hashTable.isOccupied(index),
      "Attempting to access Dictionary elements using an invalid Index")
    return self.uncheckedKey(at: index)
  }

  @inlinable
  @inline(__always)
  func value(at index: Index) -> Value {
    _precondition(hashTable.isOccupied(index),
      "Attempting to access Dictionary elements using an invalid Index")
    return self.uncheckedValue(at: index)
  }
}

#if _runtime(_ObjC)
extension _NativeDictionary { // Bridging
  @usableFromInline
  internal func bridged() -> _NSDictionary {
    // We can zero-cost bridge if our keys are verbatim
    // or if we're the empty singleton.

    // Temporary var for SOME type safety before a cast.
    let nsDictionary: _NSDictionaryCore

    if _storage === _RawDictionaryStorage.empty || count == 0 {
      nsDictionary = _RawDictionaryStorage.empty
    } else if _isBridgedVerbatimToObjectiveC(Key.self),
      _isBridgedVerbatimToObjectiveC(Value.self) {
      nsDictionary = unsafeDowncast(
        _storage,
        to: _DictionaryStorage<Key, Value>.self)
    } else {
      nsDictionary = _SwiftDeferredNSDictionary(self)
    }

    // Cast from "minimal NSDictionary" to "NSDictionary"
    // Note that if you actually ask Swift for this cast, it will fail.
    // Never trust a shadow protocol!
    return unsafeBitCast(nsDictionary, to: _NSDictionary.self)
  }
}
#endif

// This function has a highly visible name to make it stand out in stack traces.
@usableFromInline
@inline(never)
internal func KEY_TYPE_OF_DICTIONARY_VIOLATES_HASHABLE_REQUIREMENTS(
  _ keyType: Any.Type
) -> Never {
  _assertionFailure(
    "Fatal error",
    """
    Duplicate keys of type '\(keyType)' were found in a Dictionary.
    This usually means either that the type violates Hashable's requirements, or
    that members of such a dictionary were mutated after insertion.
    """,
    flags: _fatalErrorFlags())
}

extension _NativeDictionary { // Insertions
  /// Insert a new element into uniquely held storage.
  /// Storage must be uniquely referenced with adequate capacity.
  /// The `key` must not be already present in the Dictionary.
  @inlinable
  internal func _unsafeInsertNew(key: Key, value: Value) {
    _sanityCheck(count + 1 <= capacity)
    let hashValue = self.hashValue(for: key)
    if _isDebugAssertConfiguration() {
      // In debug builds, perform a full lookup and trap if we detect duplicate
      // elements -- these imply that the Element type violates Hashable
      // requirements. This is generally more costly than a direct insertion,
      // because we'll need to compare elements in case of hash collisions.
      let (index, found) = find(key, hashValue: hashValue)
      guard !found else {
        KEY_TYPE_OF_DICTIONARY_VIOLATES_HASHABLE_REQUIREMENTS(Key.self)
      }
      hashTable.insert(index)
      uncheckedInitialize(at: index, toKey: key, value: value)
    } else {
      let index = hashTable.insertNew(hashValue: hashValue)
      uncheckedInitialize(at: index, toKey: key, value: value)
    }
    _storage._count += 1
  }

  /// Insert a new entry into uniquely held storage.
  /// Storage must be uniquely referenced.
  /// The `key` must not be already present in the Dictionary.
  @inlinable
  internal mutating func insertNew(key: Key, value: Value) {
    _ = ensureUnique(isUnique: true, capacity: count + 1)
    _unsafeInsertNew(key: key, value: value)
  }

  /// Same as find(_:), except assume a corresponding key/value pair will be
  /// inserted if it doesn't already exist, and mutated if it does exist. When
  /// this function returns, the storage is guaranteed to be native, uniquely
  /// held, and with enough capacity for a single insertion (if the key isn't
  /// already in the dictionary.)
  @inlinable
  @inline(__always)
  internal mutating func mutatingFind(
    _ key: Key,
    isUnique: Bool
  ) -> (index: Index, found: Bool) {
    let (index, found) = find(key)

    // Prepare storage.
    // If `key` isn't in the dictionary yet, assume that this access will end
    // up inserting it. (If we guess wrong, we might needlessly expand
    // storage; that's fine.) Otherwise this can only be a removal or an
    // in-place mutation.
    let rehashed = ensureUnique(
      isUnique: isUnique,
      capacity: count + (found ? 0 : 1))
    guard rehashed else { return (index, found) }
    let (i, f) = find(key)
    if f != found {
      KEY_TYPE_OF_DICTIONARY_VIOLATES_HASHABLE_REQUIREMENTS(Key.self)
    }
    return (i, found)
  }

  @inlinable
  internal func _insert(at index: Index, key: Key, value: Value) {
    _sanityCheck(count < capacity)
    hashTable.insert(index)
    uncheckedInitialize(at: index, toKey: key, value: value)
    _storage._count += 1
  }

  @inlinable
  internal mutating func updateValue(
    _ value: Value,
    forKey key: Key,
    isUnique: Bool
  ) -> Value? {
    let (index, found) = mutatingFind(key, isUnique: isUnique)
    if found {
      let oldValue = (_values + index.bucket).move()
      (_values + index.bucket).initialize(to: value)
      // FIXME: Replacing the old key with the new is unnecessary, unintuitive,
      // and actively harmful to some usecases. We shouldn't do it.
      // rdar://problem/32144087
      (_keys + index.bucket).pointee = key
      return oldValue
    }
    _insert(at: index, key: key, value: value)
    return nil
  }

  @inlinable
  internal mutating func setValue(
    _ value: Value,
    forKey key: Key,
    isUnique: Bool
  ) {
    let (index, found) = mutatingFind(key, isUnique: isUnique)
    if found {
      (_values + index.bucket).pointee = value
      // FIXME: Replacing the old key with the new is unnecessary, unintuitive,
      // and actively harmful to some usecases. We shouldn't do it.
      // rdar://problem/32144087
      (_keys + index.bucket).pointee = key
    } else {
      _insert(at: index, key: key, value: value)
    }
  }
}

extension _NativeDictionary: _HashTableDelegate {
  @inlinable
  @inline(__always)
  internal func hashValue(at index: Index) -> Int {
    return hashValue(for: uncheckedKey(at: index))
  }

  @inlinable
  @inline(__always)
  internal func moveEntry(from source: Index, to target: Index) {
    (_keys + target.bucket)
      .moveInitialize(from: _keys + source.bucket, count: 1)
    (_values + target.bucket)
      .moveInitialize(from: _values + source.bucket, count: 1)
  }
}

extension _NativeDictionary { // Deletion
  @inlinable
  internal func _delete(at index: Index) {
    hashTable.delete(at: index, with: self)
    _storage._count -= 1
    _sanityCheck(_storage._count >= 0)
  }

  @inlinable
  @inline(__always)
  internal mutating func uncheckedRemove(
    at index: Index,
    isUnique: Bool
  ) -> Element {
    _sanityCheck(hashTable.isOccupied(index))
    let rehashed = ensureUnique(isUnique: isUnique, capacity: capacity)
    _sanityCheck(!rehashed)
    let oldKey = (_keys + index.bucket).move()
    let oldValue = (_values + index.bucket).move()
    _delete(at: index)
    return (oldKey, oldValue)
  }

  @inlinable
  @inline(__always)
  internal mutating func remove(at index: Index, isUnique: Bool) -> Element {
    _precondition(hashTable.isOccupied(index), "Invalid index")
    return uncheckedRemove(at: index, isUnique: isUnique)
  }

  @usableFromInline
  internal mutating func removeAll(isUnique: Bool) {
    guard isUnique else {
      let scale = self._storage._scale
      _storage = _DictionaryStorage<Key, Value>.allocate(scale: scale)
      return
    }
    for index in hashTable {
      (_keys + index.bucket).deinitialize(count: 1)
      (_values + index.bucket).deinitialize(count: 1)
    }
    hashTable.clear()
    _storage._count = 0
  }
}

extension _NativeDictionary { // High-level operations
  @inlinable
  internal func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
    let result = _NativeDictionary<Key, T>(capacity: capacity)
    // Because the keys in the current and new buffer are the same, we can
    // initialize to the same locations in the new buffer, skipping hash value
    // recalculations.
    for index in hashTable {
      let key = self.uncheckedKey(at: index)
      let value = self.uncheckedValue(at: index)
      try result._insert(at: index, key: key, value: transform(value))
    }
    return result
  }

  @inlinable
  internal mutating func merge<S: Sequence>(
    _ keysAndValues: S,
    isUnique: Bool,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    var isUnique = isUnique
    for (key, value) in keysAndValues {
      let (index, found) = mutatingFind(key, isUnique: isUnique)
      isUnique = true
      if found {
        do {
          let v = (_values + index.bucket).move()
          let newValue = try combine(v, value)
          (_values + index.bucket).initialize(to: newValue)
        } catch _MergeError.keyCollision {
          fatalError("Duplicate values for key: '\(key)'")
        }
      } else {
        _insert(at: index, key: key, value: value)
      }
    }
  }

  @inlinable
  @inline(__always)
  internal init<S: Sequence>(
    grouping values: S,
    by keyForValue: (S.Element) throws -> Key
  ) rethrows where Value == [S.Element] {
    self.init()
    for value in values {
      let key = try keyForValue(value)
      let (index, found) = mutatingFind(key, isUnique: true)
      if found {
        _values[index.bucket].append(value)
      } else {
        _insert(at: index, key: key, value: [value])
      }
    }
  }
}

#if _runtime(_ObjC)
/// An NSEnumerator that works with any _NativeDictionary of
/// verbatim bridgeable elements. Used by the various NSDictionary impls.
final internal class _SwiftDictionaryNSEnumerator<Key: Hashable, Value>
  : _SwiftNativeNSEnumerator, _NSEnumerator {

  internal var base: _NativeDictionary<Key, Value>
  internal var bridgedKeys: _BridgingHashBuffer?
  internal var nextIndex: _NativeDictionary<Key, Value>.Index
  internal var endIndex: _NativeDictionary<Key, Value>.Index

  internal override required init() {
    _sanityCheckFailure("don't call this designated initializer")
  }

  internal init(_ base: _NativeDictionary<Key, Value>) {
    _sanityCheck(_isBridgedVerbatimToObjectiveC(Key.self))
    self.base = base
    self.bridgedKeys = nil
    self.nextIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  internal init(_ deferred: _SwiftDeferredNSDictionary<Key, Value>) {
    _sanityCheck(!_isBridgedVerbatimToObjectiveC(Key.self))
    self.base = deferred.native
    self.bridgedKeys = deferred.bridgeKeys()
    self.nextIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  private func bridgedKey(at index: _HashTable.Index) -> AnyObject {
    _sanityCheck(base.hashTable.isOccupied(index))
    if let bridgedKeys = self.bridgedKeys {
      return bridgedKeys[index]
    }
    return _bridgeAnythingToObjectiveC(base.uncheckedKey(at: index))
  }

  @objc
  internal func nextObject() -> AnyObject? {
    if nextIndex == endIndex {
      return nil
    }
    let index = nextIndex
    nextIndex = base.index(after: nextIndex)
    return self.bridgedKey(at: index)
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
    let unmanagedObjects = _UnmanagedAnyObjectArray(objects)
    unmanagedObjects[0] = self.bridgedKey(at: nextIndex)
    nextIndex = base.index(after: nextIndex)
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

  // This stored property must be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  @nonobjc
  private var _bridgedKeys_DoNotUse: AnyObject?

  // This stored property must be stored at offset zero.  We perform atomic
  // operations on it.
  //
  // Do not access this property directly.
  @nonobjc
  private var _bridgedValues_DoNotUse: AnyObject?

  /// The unbridged elements.
  internal var native: _NativeDictionary<Key, Value>

  internal init(_ native: _NativeDictionary<Key, Value>) {
    _sanityCheck(native.count > 0)
    _sanityCheck(!_isBridgedVerbatimToObjectiveC(Key.self) ||
      !_isBridgedVerbatimToObjectiveC(Value.self))
    self.native = native
    super.init()
  }

  @objc
  internal required init(
    objects: UnsafePointer<AnyObject?>,
    forKeys: UnsafeRawPointer,
    count: Int
  ) {
    _sanityCheckFailure("don't call this designated initializer")
  }

  deinit {
    _bridgedKeys?.deinitialize(elementsFrom: native.hashTable)
    _bridgedValues?.deinitialize(elementsFrom: native.hashTable)
  }

  @nonobjc
  private var _bridgedKeysPtr: UnsafeMutablePointer<AnyObject?> {
    return _getUnsafePointerToStoredProperties(self)
      .assumingMemoryBound(to: Optional<AnyObject>.self)
  }

  @nonobjc
  private var _bridgedValuesPtr: UnsafeMutablePointer<AnyObject?> {
    return _bridgedKeysPtr + 1
  }

  /// The buffer for bridged keys, if present.
  @nonobjc
  private var _bridgedKeys: _BridgingHashBuffer? {
    guard let ref = _stdlib_atomicLoadARCRef(object: _bridgedKeysPtr) else {
      return nil
    }
    return unsafeDowncast(ref, to: _BridgingHashBuffer.self)
  }

  /// The buffer for bridged values, if present.
  @nonobjc
  private var _bridgedValues: _BridgingHashBuffer? {
    guard let ref = _stdlib_atomicLoadARCRef(object: _bridgedValuesPtr) else {
      return nil
    }
    return unsafeDowncast(ref, to: _BridgingHashBuffer.self)
  }

  /// Attach a buffer for bridged Dictionary keys.
  @nonobjc
  private func _initializeBridgedKeys(_ storage: _BridgingHashBuffer) -> Bool {
    return _stdlib_atomicInitializeARCRef(
      object: _bridgedKeysPtr,
      desired: storage)
  }

  /// Attach a buffer for bridged Dictionary values.
  @nonobjc
  private func _initializeBridgedValues(
    _ storage: _BridgingHashBuffer
  ) -> Bool {
    return _stdlib_atomicInitializeARCRef(
      object: _bridgedValuesPtr,
      desired: storage)
  }

  @nonobjc
  internal func bridgeKeys() -> _BridgingHashBuffer? {
    if _isBridgedVerbatimToObjectiveC(Key.self) { return nil }
    if let bridgedKeys = _bridgedKeys { return bridgedKeys }

    // Allocate and initialize heap storage for bridged keys.
    let bridged = _BridgingHashBuffer.allocate(
      bucketCount: native._storage._bucketCount)
    for index in native.hashTable {
      let object = _bridgeAnythingToObjectiveC(native.uncheckedKey(at: index))
      bridged.initialize(at: index, to: object)
    }

    // Atomically put the bridged keys in place.
    if !_initializeBridgedKeys(bridged) {
      // Lost the race.
      bridged.deinitialize(elementsFrom: native.hashTable)
      return _bridgedKeys!
    }
    return bridged
  }

  @nonobjc
  internal func bridgeValues() -> _BridgingHashBuffer? {
    if _isBridgedVerbatimToObjectiveC(Value.self) { return nil }
    if let bridgedValues = _bridgedValues { return bridgedValues }

    // Allocate and initialize heap storage for bridged values.
    let bridged = _BridgingHashBuffer.allocate(
      bucketCount: native._storage._bucketCount)
    for index in native.hashTable {
      let object = _bridgeAnythingToObjectiveC(native.uncheckedValue(at: index))
      bridged.initialize(at: index, to: object)
    }

    // Atomically put the bridged values in place.
    if !_initializeBridgedValues(bridged) {
      // Lost the race.
      bridged.deinitialize(elementsFrom: native.hashTable)
      return _bridgedValues!
    }
    return bridged
  }

  @usableFromInline
  internal typealias Index = _HashTable.Index

  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // Instances of this class should be visible outside of standard library as
    // having `NSDictionary` type, which is immutable.
    return self
  }

  @objc(objectForKey:)
  internal func object(forKey aKey: AnyObject) -> AnyObject? {
    guard let nativeKey = _conditionallyBridgeFromObjectiveC(aKey, Key.self)
    else { return nil }

    let (index, found) = native.find(nativeKey)
    guard found else { return nil }
    if let bridgedValues = bridgeValues() {
      return bridgedValues[index]
    }
    return _bridgeAnythingToObjectiveC(native.uncheckedValue(at: index))
  }

  @inline(__always)
  private func _key(
    at index: Index,
    bridgedKeys: _BridgingHashBuffer?
  ) -> AnyObject {
    if let bridgedKeys = bridgedKeys {
      return bridgedKeys[index]
    }
    return _bridgeAnythingToObjectiveC(native.uncheckedKey(at: index))
  }

  @inline(__always)
  private func _value(
    at index: Index,
    bridgedValues: _BridgingHashBuffer?
  ) -> AnyObject {
    if let bridgedValues = bridgedValues {
      return bridgedValues[index]
    }
    return _bridgeAnythingToObjectiveC(native.uncheckedValue(at: index))
  }

  @objc
  internal func keyEnumerator() -> _NSEnumerator {
    if _isBridgedVerbatimToObjectiveC(Key.self) {
      return _SwiftDictionaryNSEnumerator<Key, Value>(native)
    }
    return _SwiftDictionaryNSEnumerator<Key, Value>(self)
  }

  @objc(getObjects:andKeys:count:)
  internal func getObjects(
    _ objects: UnsafeMutablePointer<AnyObject>?,
    andKeys keys: UnsafeMutablePointer<AnyObject>?,
    count: Int
  ) {
    _precondition(count >= 0, "Invalid count")
    guard count > 0 else { return }
    let bridgedKeys = bridgeKeys()
    let bridgedValues = bridgeValues()
    var i = 0 // Current position in the output buffers
    let bucketCount = native._storage._bucketCount

    defer { _fixLifetime(self) }

    switch (_UnmanagedAnyObjectArray(keys), _UnmanagedAnyObjectArray(objects)) {
    case (let unmanagedKeys?, let unmanagedObjects?):
      for index in native.hashTable {
        unmanagedKeys[i] = _key(at: index, bridgedKeys: bridgedKeys)
        unmanagedObjects[i] = _value(at: index, bridgedValues: bridgedValues)
        i += 1
        guard i < count else { break }
      }
    case (let unmanagedKeys?, nil):
      for index in native.hashTable {
        unmanagedKeys[i] = _key(at: index, bridgedKeys: bridgedKeys)
        i += 1
        guard i < count else { break }
      }
    case (nil, let unmanagedObjects?):
      for index in native.hashTable {
        unmanagedObjects[i] = _value(at: index, bridgedValues: bridgedValues)
        i += 1
        guard i < count else { break }
      }
    case (nil, nil):
      // Do nothing
      break
    }
  }

  @objc(enumerateKeysAndObjectsWithOptions:usingBlock:)
  internal func enumerateKeysAndObjects(
    options: Int,
    using block: @convention(block) (
      Unmanaged<AnyObject>,
      Unmanaged<AnyObject>,
      UnsafeMutablePointer<UInt8>
    ) -> Void) {
    let bridgedKeys = bridgeKeys()
    let bridgedValues = bridgeValues()

    defer { _fixLifetime(self) }

    var stop: UInt8 = 0
    for index in native.hashTable {
      let key = _key(at: index, bridgedKeys: bridgedKeys)
      let value = _value(at: index, bridgedValues: bridgedValues)
      block(
        Unmanaged.passUnretained(key),
        Unmanaged.passUnretained(value),
        &stop)
      if stop != 0 { return }
    }
  }

  @objc
  internal var count: Int {
    return native.count
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
    var index = _HashTable.Index(bucket: Int(theState.extra.0))
    let endIndex = native.endIndex
    _precondition(index == endIndex || native.hashTable.isOccupied(index))
    var stored = 0

    // Only need to bridge once, so we can hoist it out of the loop.
    let bridgedKeys = bridgeKeys()
    for i in 0..<count {
      if index == endIndex { break }

      unmanagedObjects[i] = _key(at: index, bridgedKeys: bridgedKeys)
      stored += 1
      index = native.index(after: index)
    }
    theState.extra.0 = CUnsignedLong(index.bucket)
    state.pointee = theState
    return stored
  }
}
#else
// FIXME: Remove
final internal class _SwiftDeferredNSDictionary<Key: Hashable, Value> { }
#endif

#if _runtime(_ObjC)
@usableFromInline
@_fixed_layout
internal struct _CocoaDictionary {
  @usableFromInline
  internal let object: _NSDictionary

  @inlinable
  internal init(_ object: _NSDictionary) {
    self.object = object
  }
}

extension _CocoaDictionary: Equatable {
  @usableFromInline
  internal static func ==(
    lhs: _CocoaDictionary,
    rhs: _CocoaDictionary
  ) -> Bool {
    return _stdlib_NSObject_isEqual(lhs.object, rhs.object)
  }
}

extension _CocoaDictionary: _DictionaryBuffer {
  @usableFromInline
  internal typealias Key = AnyObject
  @usableFromInline
  internal typealias Value = AnyObject

  @inlinable
  internal var startIndex: Index {
    return Index(self, startIndex: ())
  }

  @inlinable
  internal var endIndex: Index {
    return Index(self, endIndex: ())
  }

  @inlinable
  internal func index(after i: Index) -> Index {
    var i = i
    formIndex(after: &i)
    return i
  }

  @usableFromInline
  @_effects(releasenone)
  internal func formIndex(after i: inout Index) {
    _precondition(i.base.object === self.object, "Invalid index")
    _precondition(i.currentKeyIndex < i.allKeys.value,
      "Cannot increment endIndex")
    i.currentKeyIndex += 1
  }

  @usableFromInline
  internal func index(forKey key: Key) -> Index? {
    // Fast path that does not involve creating an array of all keys.  In case
    // the key is present, this lookup is a penalty for the slow path, but the
    // potential savings are significant: we could skip a memory allocation and
    // a linear search.
    if lookup(key) == nil {
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

  @inlinable
  internal var count: Int {
    return object.count
  }

  @inlinable
  @inline(__always)
  internal func contains(_ key: Key) -> Bool {
    return object.object(forKey: key) != nil
  }

  @inlinable
  @inline(__always)
  internal func lookup(_ key: Key) -> Value? {
    return object.object(forKey: key)
  }

  @inlinable
  @inline(__always)
  internal func lookup(_ index: Index) -> (key: Key, value: Value) {
    _precondition(index.base.object === self.object, "Invalid index")
    let key: Key = index.allKeys[index.currentKeyIndex]
    let value: Value = index.base.object.object(forKey: key)!
    return (key, value)
  }

  @inlinable
  @inline(__always)
  func key(at index: Index) -> Key {
    _precondition(index.base.object === self.object, "Invalid index")
    return index.allKeys[index.currentKeyIndex]
  }

  @inlinable
  @inline(__always)
  func value(at index: Index) -> Value {
    _precondition(index.base.object === self.object, "Invalid index")
    let key = index.allKeys[index.currentKeyIndex]
    return index.base.object.object(forKey: key)!
  }
}

extension _CocoaDictionary {
  @inlinable
  internal func mapValues<Key: Hashable, Value, T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
    var result = _NativeDictionary<Key, T>(capacity: self.count)
    for (cocoaKey, cocoaValue) in self {
      let key = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
      let value = _forceBridgeFromObjectiveC(cocoaValue, Value.self)
      try result.insertNew(key: key, value: transform(value))
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

extension Dictionary._Variant {
#if _runtime(_ObjC)
  @usableFromInline @_transparent
  internal var guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 || _canBeClass(Value.self) == 0
  }

  // Allow the optimizer to consider the surrounding code unreachable if Element
  // is guaranteed to be native.
  @usableFromInline @_transparent
  internal func cocoaPath() {
    if guaranteedNative {
      _conditionallyUnreachable()
    }
  }
#endif

  @inlinable
  internal mutating func isUniquelyReferenced() -> Bool {
    switch self {
    case .native:
      // Note that &self drills down through .native(_NativeDictionary) to the
      // first property in _NativeDictionary, which is the reference to the
      // storage.
      return _isUnique_native(&self)
#if _runtime(_ObjC)
    case .cocoa:
      cocoaPath()
      // Don't consider Cocoa buffer mutable, even if it is mutable and is
      // uniquely referenced.
      return false
#endif
    }
  }

  @inlinable
  internal var asNative: _NativeDictionary<Key, Value> {
    @inline(__always)
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
    @inline(__always)
    set {
      self = .native(newValue)
    }
  }

#if _runtime(_ObjC)
  @inlinable
  internal var asCocoa: _CocoaDictionary {
    switch self {
    case .native:
      _sanityCheckFailure("internal error: not backed by NSDictionary")
    case .cocoa(let cocoa):
      return cocoa
    }
  }
#endif

  /// Reserves enough space for the specified number of elements to be stored
  /// without reallocating additional storage.
  @inlinable
  internal mutating func reserveCapacity(_ capacity: Int) {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      asNative.reserveCapacity(capacity, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let capacity = Swift.max(cocoa.count, capacity)
      self = .native(_NativeDictionary(cocoa, capacity: capacity))
#endif
    }
  }

  /// The number of elements that can be stored without expanding the current
  /// storage.
  ///
  /// For bridged storage, this is equal to the current count of the
  /// collection, since any addition will trigger a copy of the elements into
  /// newly allocated storage. For native storage, this is the element count
  /// at which adding any more elements will exceed the load factor.
  @inlinable
  internal var capacity: Int {
    switch self {
    case .native:
      return asNative.capacity
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return cocoa.count
#endif
    }
  }
}

extension Dictionary._Variant: _DictionaryBuffer {
  @usableFromInline
  internal typealias Element = (key: Key, value: Value)
  @usableFromInline
  internal typealias Index = Dictionary<Key, Value>.Index

  @inlinable
  internal var startIndex: Index {
    switch self {
    case .native:
      return Index(_native: asNative.startIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return Index(_cocoa: cocoa.startIndex)
#endif
    }
  }

  @inlinable
  internal var endIndex: Index {
    switch self {
    case .native:
      return Index(_native: asNative.endIndex)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return Index(_cocoa: cocoa.endIndex)
#endif
    }
  }

  @inlinable
  internal func index(after i: Index) -> Index {
    switch self {
    case .native:
      return Index(_native: asNative.index(after: i._asNative))
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return Index(_cocoa: cocoa.index(after: i._asCocoa))
#endif
    }
  }

  @inlinable
  @inline(__always)
  internal func index(forKey key: Key) -> Index? {
    switch self {
    case .native:
      guard let index = asNative.index(forKey: key) else { return nil }
      return Index(_native: index)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      guard let index = cocoa.index(forKey: cocoaKey) else { return nil }
      return Index(_cocoa: index)
#endif
    }
  }

  @inlinable
  internal var count: Int {
    @inline(__always)
    get {
      switch self {
      case .native:
        return asNative.count
#if _runtime(_ObjC)
      case .cocoa(let cocoa):
        cocoaPath()
        return cocoa.count
#endif
      }
    }
  }

  @inlinable
  @inline(__always)
  func contains(_ key: Key) -> Bool {
    switch self {
    case .native:
      return asNative.contains(key)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      return cocoa.contains(cocoaKey)
#endif
    }
  }

  @inlinable
  @inline(__always)
  func lookup(_ key: Key) -> Value? {
    switch self {
    case .native:
      return asNative.lookup(key)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      guard let cocoaValue = cocoa.lookup(cocoaKey) else { return nil }
      return _forceBridgeFromObjectiveC(cocoaValue, Value.self)
#endif
    }
  }

  @inlinable
  @inline(__always)
  func lookup(_ index: Index) -> (key: Key, value: Value) {
    switch self {
    case .native:
      return asNative.lookup(index._asNative)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let (cocoaKey, cocoaValue) = cocoa.lookup(index._asCocoa)
      let nativeKey = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
      let nativeValue = _forceBridgeFromObjectiveC(cocoaValue, Value.self)
      return (nativeKey, nativeValue)
#endif
    }
  }

  @inlinable
  @inline(__always)
  func key(at index: Index) -> Key {
    switch self {
    case .native:
      return asNative.key(at: index._asNative)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = cocoa.key(at: index._asCocoa)
      return _forceBridgeFromObjectiveC(cocoaKey, Key.self)
#endif
    }
  }

  @inlinable
  @inline(__always)
  func value(at index: Index) -> Value {
    switch self {
    case .native:
      return asNative.value(at: index._asNative)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaValue = cocoa.value(at: index._asCocoa)
      return _forceBridgeFromObjectiveC(cocoaValue, Value.self)
#endif
    }
  }
}

extension Dictionary._Variant {
  /// Same as find(_:), except assume a corresponding key/value pair will be
  /// inserted if it doesn't already exist, and mutated if it does exist. When
  /// this function returns, the storage is guaranteed to be native, uniquely
  /// held, and with enough capacity for a single insertion (if the key isn't
  /// already in the dictionary.)
  @inlinable
  @inline(__always)
  internal mutating func mutatingFind(
    _ key: Key
  ) -> (index: _NativeDictionary<Key, Value>.Index, found: Bool) {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      return asNative.mutatingFind(key, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      var native = _NativeDictionary<Key, Value>(
        cocoa, capacity: cocoa.count + 1)
      let result = native.mutatingFind(key, isUnique: true)
      self = .native(native)
      return result
#endif
    }
  }

  @usableFromInline // FIMXE: Should be @inlinable (rdar://problem/44612356)
  internal mutating func lookupOrInsert(
    _ key: Key,
    default defaultValue: () -> Value
  ) -> _NativeDictionary<Key, Value>.Index {
    let (index, found) = mutatingFind(key)
    if !found {
      let value = defaultValue()
      asNative._insert(at: index, key: key, value: value)
    }
    return index
  }

  /// Ensure uniquely held native storage, while preserving the given index.
  /// (If the variant had bridged storage, then the returned index will be the
  /// corresponding native representation. Otherwise it's kept the same.)
  @inlinable
  @inline(__always)
  internal mutating func ensureUniqueNative(
    preserving index: Index
  ) -> _NativeDictionary<Key, Value>.Index {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      if !isUnique {
        let rehashed = asNative.copy(capacity: asNative.capacity)
        _sanityCheck(!rehashed)
      }
      return index._asNative
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      // We have to migrate the data first.  But after we do so, the Cocoa
      // index becomes useless, so get the key first.
      let cocoaKey = cocoa.key(at: index._asCocoa)
      let native = _NativeDictionary<Key, Value>(cocoa)
      self = .native(native)
      let nativeKey = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
      let (nativeIndex, found) = native.find(nativeKey)
      _precondition(found, "Bridging did not preserve equality")
      return nativeIndex
#endif
    }
  }

  @inlinable
  internal mutating func updateValue(
    _ value: Value,
    forKey key: Key
  ) -> Value? {
    switch self {
    case .native:
      let isUnique = self.isUniquelyReferenced()
      return asNative.updateValue(value, forKey: key, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      // Make sure we have space for an extra element.
      var native = _NativeDictionary<Key, Value>(
        cocoa,
        capacity: cocoa.count + 1)
      let result = native.updateValue(value, forKey: key, isUnique: true)
      self = .native(native)
      return result
#endif
    }
  }

  @inlinable
  internal mutating func setValue(_ value: Value, forKey key: Key) {
    switch self {
    case .native:
      let isUnique = self.isUniquelyReferenced()
      asNative.setValue(value, forKey: key, isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      // Make sure we have space for an extra element.
      var native = _NativeDictionary<Key, Value>(
        cocoa,
        capacity: cocoa.count + 1)
      native.setValue(value, forKey: key, isUnique: true)
      self = .native(native)
#endif
    }
  }

  @inlinable
  internal mutating func remove(at index: Index) -> Element {
    // FIXME(performance): fuse data migration and element deletion into one
    // operation.
    let index = ensureUniqueNative(preserving: index)
    return asNative.remove(at: index, isUnique: true)
  }

  @inlinable
  internal mutating func removeValue(forKey key: Key) -> Value? {
    switch self {
    case .native:
      let (index, found) = asNative.find(key)
      guard found else { return nil }
      let isUnique = isUniquelyReferenced()
      return asNative.uncheckedRemove(at: index, isUnique: isUnique).value
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      let cocoaKey = _bridgeAnythingToObjectiveC(key)
      guard cocoa.lookup(cocoaKey) != nil else { return nil }
      var native = _NativeDictionary<Key, Value>(cocoa)
      let (index, found) = native.find(key)
      _precondition(found, "Bridging did not preserve equality")
      let old = native.uncheckedRemove(at: index, isUnique: true).value
      self = .native(native)
      return old
#endif
    }
  }

  @inlinable
  internal mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    if !keepCapacity {
      self = .native(_NativeDictionary())
      return
    }
    guard count > 0 else { return }

    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      asNative.removeAll(isUnique: isUnique)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      self = .native(_NativeDictionary(capacity: cocoa.count))
#endif
    }
  }
}

extension Dictionary._Variant {
  /// Returns an iterator over the `(Key, Value)` pairs.
  ///
  /// - Complexity: O(1).
  @inlinable
  @inline(__always)
  internal func makeIterator() -> Dictionary<Key, Value>.Iterator {
    switch self {
    case .native(let native):
      return Dictionary.Iterator(_native: native.makeIterator())
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return Dictionary.Iterator(_cocoa: cocoa.makeIterator())
#endif
    }
  }
}

extension Dictionary._Variant {
  @inlinable
  internal func mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
    switch self {
    case .native(let native):
      return try native.mapValues(transform)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      return try cocoa.mapValues(transform)
#endif
    }
  }

  @inlinable
  internal mutating func merge<S: Sequence>(
    _ keysAndValues: S,
    uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    switch self {
    case .native:
      let isUnique = isUniquelyReferenced()
      try asNative.merge(
        keysAndValues,
        isUnique: isUnique,
        uniquingKeysWith: combine)
#if _runtime(_ObjC)
    case .cocoa(let cocoa):
      cocoaPath()
      var native = _NativeDictionary<Key, Value>(cocoa)
      try native.merge(
        keysAndValues,
        isUnique: true,
        uniquingKeysWith: combine)
      self = .native(native)
#endif
    }
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
  }
}

extension _CocoaDictionary.Index: Equatable {
  @inlinable
  internal static func == (
    lhs: _CocoaDictionary.Index,
    rhs: _CocoaDictionary.Index
  ) -> Bool {
    _precondition(lhs.base.object === rhs.base.object,
      "Comparing indexes from different dictionaries")
    return lhs.currentKeyIndex == rhs.currentKeyIndex
  }
}

extension _CocoaDictionary.Index: Comparable {
  @inlinable
  internal static func < (
    lhs: _CocoaDictionary.Index,
    rhs: _CocoaDictionary.Index
  ) -> Bool {
    _precondition(lhs.base.object === rhs.base.object,
      "Comparing indexes from different dictionaries")
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
    @inline(__always)
    internal init(_variant: _Variant) {
      self._variant = _variant
    }

    @inlinable
    @inline(__always)
    internal init(_native index: _NativeDictionary<Key, Value>.Index) {
      self.init(_variant: .native(index))
    }

#if _runtime(_ObjC)
    @inlinable
    @inline(__always)
    internal init(_cocoa index: _CocoaDictionary.Index) {
      self.init(_variant: .cocoa(index))
    }
#endif
  }
}

extension Dictionary.Index {
#if _runtime(_ObjC)
  @usableFromInline @_transparent
  internal var _guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 || _canBeClass(Value.self) == 0
  }

  // Allow the optimizer to consider the surrounding code unreachable if Element
  // is guaranteed to be native.
  @usableFromInline @_transparent
  internal func _cocoaPath() {
    if _guaranteedNative {
      _conditionallyUnreachable()
    }
  }
#endif

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
    switch (lhs._variant, rhs._variant) {
    case (.native(let lhsNative), .native(let rhsNative)):
      return lhsNative == rhsNative
  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      lhs._cocoaPath()
      return lhsCocoa == rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different dictionaries")
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
    switch (lhs._variant, rhs._variant) {
    case (.native(let lhsNative), .native(let rhsNative)):
      return lhsNative < rhsNative
  #if _runtime(_ObjC)
    case (.cocoa(let lhsCocoa), .cocoa(let rhsCocoa)):
      lhs._cocoaPath()
      return lhsCocoa < rhsCocoa
    default:
      _preconditionFailure("Comparing indexes from different dictionaries")
  #endif
    }
  }
}

extension Dictionary.Index: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
  #if _runtime(_ObjC)
    switch _variant {
    case .native(let nativeIndex):
      hasher.combine(0 as UInt8)
      hasher.combine(nativeIndex.bucket)
    case .cocoa(let cocoaIndex):
      _cocoaPath()
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
    // The iterator is iterating over a frozen view of the collection state, so
    // it keeps its own reference to the dictionary.
    @usableFromInline
    internal let base: _NativeDictionary
    @usableFromInline
    internal var iterator: _HashTable.Iterator

    @inlinable
    init(_ base: _NativeDictionary) {
      self.base = base
      self.iterator = base.hashTable.makeIterator()
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
    guard let index = iterator.next() else { return nil }
    let key = base.uncheckedKey(at: index)
    let value = base.uncheckedValue(at: index)
    return (key, value)
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
    let value: AnyObject = base.object.object(forKey: key)!
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

    @inlinable
    internal init(_native: _NativeDictionary<Key, Value>.Iterator) {
      self.init(_variant: .native(_native))
    }

#if _runtime(_ObjC)
    @inlinable
    internal init(_cocoa: _CocoaDictionary.Iterator) {
      self.init(_variant: .cocoa(_cocoa))
    }
#endif
  }
}

extension Dictionary.Iterator {
#if _runtime(_ObjC)
  @usableFromInline @_transparent
  internal var _guaranteedNative: Bool {
    return _canBeClass(Key.self) == 0 || _canBeClass(Value.self) == 0
  }

  /// Allow the optimizer to consider the surrounding code unreachable if
  /// Dictionary<Key, Value> is guaranteed to be native.
  @usableFromInline @_transparent
  internal func _cocoaPath() {
    if _guaranteedNative {
      _conditionallyUnreachable()
    }
  }
#endif

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
    switch _variant {
    case .native:
      return _asNative.next()
#if _runtime(_ObjC)
    case .cocoa(let cocoaIterator):
      _cocoaPath()
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
public // SPI(Foundation)
struct _DictionaryBuilder<Key: Hashable, Value> {
  @usableFromInline
  internal var _target: _NativeDictionary<Key, Value>
  @usableFromInline
  internal let _requestedCount: Int

  @inlinable
  public init(count: Int) {
    _target = _NativeDictionary(capacity: count)
    _requestedCount = count
  }

  @inlinable
  public mutating func add(key newKey: Key, value: Value) {
    _target.insertNew(key: newKey, value: value)
  }

  @inlinable
  public mutating func take() -> Dictionary<Key, Value> {
    _precondition(_target.capacity > 0 || _requestedCount == 0,
      "Cannot take the result twice")
    _precondition(_target.count == _requestedCount,
      "The number of members added does not match the promised count")

    // Prevent taking the result twice.
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
  @inlinable
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
  @inlinable
  public mutating func reserveCapacity(_ minimumCapacity: Int) {
    _variant.reserveCapacity(minimumCapacity)
    _sanityCheck(self.capacity >= minimumCapacity)
  }
}

//===--- Bridging ---------------------------------------------------------===//

#if _runtime(_ObjC)
extension Dictionary {
  @inlinable
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

    if let nativeStorage = s as? _DictionaryStorage<Key, Value> {
      return Dictionary(_native: _NativeDictionary(nativeStorage))
    }

    if s === _RawDictionaryStorage.empty {
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
