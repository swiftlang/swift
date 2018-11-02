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

@_exported import Foundation // Clang module
import CoreFoundation
import _SwiftCoreFoundationOverlayShims
import _SwiftFoundationOverlayShims

private func _utfRangeToCFRange(_ inRange : Range<Unicode.Scalar>) -> CFRange {
    return CFRange(
        location: Int(inRange.lowerBound.value),
        length: Int(inRange.upperBound.value - inRange.lowerBound.value))
}

private func _utfRangeToCFRange(_ inRange : ClosedRange<Unicode.Scalar>) -> CFRange {
    return CFRange(
        location: Int(inRange.lowerBound.value),
        length: Int(inRange.upperBound.value - inRange.lowerBound.value + 1))
}

// MARK: -

fileprivate final class _CharacterSetStorage : Hashable {
    fileprivate enum Backing {
        case immutable(CFCharacterSet)
        case mutable(CFMutableCharacterSet)
    }
    
    fileprivate var _backing : Backing
   
    @nonobjc 
    init(immutableReference r : CFCharacterSet) {
        _backing = .immutable(r)
    }

    @nonobjc
    init(mutableReference r : CFMutableCharacterSet) {
        _backing = .mutable(r)
    }
    
    // MARK: -
    
    fileprivate var hashValue : Int {
        switch _backing {
        case .immutable(let cs):
            return Int(CFHash(cs))
        case .mutable(let cs):
            return Int(CFHash(cs))
        }
    }
    
    fileprivate static func ==(_ lhs : _CharacterSetStorage, _ rhs : _CharacterSetStorage) -> Bool {
        switch (lhs._backing, rhs._backing) {
        case (.immutable(let cs1), .immutable(let cs2)):
            return CFEqual(cs1, cs2)
        case (.immutable(let cs1), .mutable(let cs2)):
            return CFEqual(cs1, cs2)
        case (.mutable(let cs1), .immutable(let cs2)):
            return CFEqual(cs1, cs2)
        case (.mutable(let cs1), .mutable(let cs2)):
            return CFEqual(cs1, cs2)
        }
    }
    
    // MARK: -
    
    fileprivate func mutableCopy() -> _CharacterSetStorage {
        switch _backing {
        case .immutable(let cs):
            return _CharacterSetStorage(mutableReference: CFCharacterSetCreateMutableCopy(nil, cs))
        case .mutable(let cs):
            return _CharacterSetStorage(mutableReference: CFCharacterSetCreateMutableCopy(nil, cs))
        }
    }

    
    // MARK: Immutable Functions
    
    fileprivate var bitmapRepresentation : Data {
        switch _backing {
        case .immutable(let cs):
            return CFCharacterSetCreateBitmapRepresentation(nil, cs) as Data
        case .mutable(let cs):
            return CFCharacterSetCreateBitmapRepresentation(nil, cs) as Data
        }
    }
    
    fileprivate func hasMember(inPlane plane: UInt8) -> Bool {
        switch _backing {
        case .immutable(let cs):
            return CFCharacterSetHasMemberInPlane(cs, CFIndex(plane))
        case .mutable(let cs):
            return CFCharacterSetHasMemberInPlane(cs, CFIndex(plane))
        }
    }
    
    // MARK: Mutable functions
    
    fileprivate func insert(charactersIn range: Range<Unicode.Scalar>) {
        switch _backing {
        case .immutable(let cs):
            let r = CFCharacterSetCreateMutableCopy(nil, cs)!
            CFCharacterSetAddCharactersInRange(r, _utfRangeToCFRange(range))
            _backing = .mutable(r)
        case .mutable(let cs):
            CFCharacterSetAddCharactersInRange(cs, _utfRangeToCFRange(range))
        }
    }
    
    fileprivate func insert(charactersIn range: ClosedRange<Unicode.Scalar>) {
        switch _backing {
        case .immutable(let cs):
            let r = CFCharacterSetCreateMutableCopy(nil, cs)!
            CFCharacterSetAddCharactersInRange(r, _utfRangeToCFRange(range))
            _backing = .mutable(r)
        case .mutable(let cs):
            CFCharacterSetAddCharactersInRange(cs, _utfRangeToCFRange(range))
        }
    }
    
    fileprivate func remove(charactersIn range: Range<Unicode.Scalar>) {
        switch _backing {
        case .immutable(let cs):
            let r = CFCharacterSetCreateMutableCopy(nil, cs)!
            CFCharacterSetRemoveCharactersInRange(r, _utfRangeToCFRange(range))
            _backing = .mutable(r)
        case .mutable(let cs):
            CFCharacterSetRemoveCharactersInRange(cs, _utfRangeToCFRange(range))
        }
    }
    
    fileprivate func remove(charactersIn range: ClosedRange<Unicode.Scalar>) {
        switch _backing {
        case .immutable(let cs):
            let r = CFCharacterSetCreateMutableCopy(nil, cs)!
            CFCharacterSetRemoveCharactersInRange(r, _utfRangeToCFRange(range))
            _backing = .mutable(r)
        case .mutable(let cs):
            CFCharacterSetRemoveCharactersInRange(cs, _utfRangeToCFRange(range))
        }
    }
    
    fileprivate func insert(charactersIn string: String) {
        switch _backing {
        case .immutable(let cs):
            let r = CFCharacterSetCreateMutableCopy(nil, cs)!
            CFCharacterSetAddCharactersInString(r, string as CFString)
            _backing = .mutable(r)
        case .mutable(let cs):
            CFCharacterSetAddCharactersInString(cs, string as CFString)
        }
    }
    
    fileprivate func remove(charactersIn string: String) {
        switch _backing {
        case .immutable(let cs):
            let r = CFCharacterSetCreateMutableCopy(nil, cs)!
            CFCharacterSetRemoveCharactersInString(r, string as CFString)
            _backing = .mutable(r)
        case .mutable(let cs):
            CFCharacterSetRemoveCharactersInString(cs, string as CFString)
        }
    }
    
    fileprivate func invert() {
        switch _backing {
        case .immutable(let cs):
            let r = CFCharacterSetCreateMutableCopy(nil, cs)!
            CFCharacterSetInvert(r)
            _backing = .mutable(r)
        case .mutable(let cs):
            CFCharacterSetInvert(cs)
        }
    }
    
    // -----
    // MARK: -
    // MARK: SetAlgebraType
    
    @discardableResult
    fileprivate func insert(_ character: Unicode.Scalar) -> (inserted: Bool, memberAfterInsert: Unicode.Scalar) {
        insert(charactersIn: character...character)
        // TODO: This should probably return the truth, but figuring it out requires two calls into NSCharacterSet
        return (true, character)
    }
    
    @discardableResult
    fileprivate func update(with character: Unicode.Scalar) -> Unicode.Scalar? {
        insert(character)
        // TODO: This should probably return the truth, but figuring it out requires two calls into NSCharacterSet
        return character
    }
    
    @discardableResult
    fileprivate func remove(_ character: Unicode.Scalar) -> Unicode.Scalar? {
        // TODO: Add method to CFCharacterSet to do this in one call
        let result : Unicode.Scalar? = contains(character) ? character : nil
        remove(charactersIn: character...character)
        return result
    }
    
    fileprivate func contains(_ member: Unicode.Scalar) -> Bool {
        switch _backing {
        case .immutable(let cs):
            return CFCharacterSetIsLongCharacterMember(cs, member.value)
        case .mutable(let cs):
            return CFCharacterSetIsLongCharacterMember(cs, member.value)
        }
    }
    
    // MARK: -
    // Why do these return CharacterSet instead of CharacterSetStorage?
    // We want to keep the knowledge of if the returned value happened to contain a mutable or immutable CFCharacterSet as close to the creation of that instance as possible
    

    // When the underlying collection does not have a method to return new CharacterSets with changes applied, so we will copy and apply here
    private static func _apply(_ lhs : _CharacterSetStorage, _ rhs : _CharacterSetStorage, _ f : (CFMutableCharacterSet, CFCharacterSet) -> ()) -> CharacterSet {
        let copyOfMe : CFMutableCharacterSet
        switch lhs._backing {
        case .immutable(let cs):
            copyOfMe = CFCharacterSetCreateMutableCopy(nil, cs)!
        case .mutable(let cs):
            copyOfMe = CFCharacterSetCreateMutableCopy(nil, cs)!
        }
        
        switch rhs._backing {
        case .immutable(let cs):
            f(copyOfMe, cs)
        case .mutable(let cs):
            f(copyOfMe, cs)
        }
        
        return CharacterSet(_uncopiedStorage: _CharacterSetStorage(mutableReference: copyOfMe))
    }
    
    private func _applyMutation(_ other : _CharacterSetStorage, _ f : (CFMutableCharacterSet, CFCharacterSet) -> ()) {
        switch _backing {
        case .immutable(let cs):
            let r = CFCharacterSetCreateMutableCopy(nil, cs)!
            switch other._backing {
            case .immutable(let otherCs):
                f(r, otherCs)
            case .mutable(let otherCs):
                f(r, otherCs)
            }
            _backing = .mutable(r)
        case .mutable(let cs):
            switch other._backing {
            case .immutable(let otherCs):
                f(cs, otherCs)
            case .mutable(let otherCs):
                f(cs, otherCs)
            }
        }

    }
    
    fileprivate var inverted : CharacterSet {
        switch _backing {
        case .immutable(let cs):
            return CharacterSet(_uncopiedStorage: _CharacterSetStorage(immutableReference: CFCharacterSetCreateInvertedSet(nil, cs)))
        case .mutable(let cs):
            // Even if input is mutable, the result is immutable
            return CharacterSet(_uncopiedStorage: _CharacterSetStorage(immutableReference: CFCharacterSetCreateInvertedSet(nil, cs)))
        }
    }

    fileprivate func union(_ other: _CharacterSetStorage) -> CharacterSet {
        return _CharacterSetStorage._apply(self, other, CFCharacterSetUnion)
    }
    
    fileprivate func formUnion(_ other: _CharacterSetStorage) {
        _applyMutation(other, CFCharacterSetUnion)
    }
    
    fileprivate func intersection(_ other: _CharacterSetStorage) -> CharacterSet {
        return _CharacterSetStorage._apply(self, other, CFCharacterSetIntersect)
    }
    
    fileprivate func formIntersection(_ other: _CharacterSetStorage) {
        _applyMutation(other, CFCharacterSetIntersect)
    }
    
    fileprivate func subtracting(_ other: _CharacterSetStorage) -> CharacterSet {
        return intersection(other.inverted._storage)
    }
    
    fileprivate func subtract(_ other: _CharacterSetStorage) {
        _applyMutation(other.inverted._storage, CFCharacterSetIntersect)
    }
    
    fileprivate func symmetricDifference(_ other: _CharacterSetStorage) -> CharacterSet {
        return union(other).subtracting(intersection(other))
    }
    
    fileprivate func formSymmetricDifference(_ other: _CharacterSetStorage) {
        // This feels like cheating
        _backing = symmetricDifference(other)._storage._backing
    }
    
    fileprivate func isSuperset(of other: _CharacterSetStorage) -> Bool {
        switch _backing {
        case .immutable(let cs):
            switch other._backing {
            case .immutable(let otherCs):
                return CFCharacterSetIsSupersetOfSet(cs, otherCs)
            case .mutable(let otherCs):
                return CFCharacterSetIsSupersetOfSet(cs, otherCs)
            }
        case .mutable(let cs):
            switch other._backing {
            case .immutable(let otherCs):
                return CFCharacterSetIsSupersetOfSet(cs, otherCs)
            case .mutable(let otherCs):
                return CFCharacterSetIsSupersetOfSet(cs, otherCs)
            }
        }
    }
    
    // MARK: -
    
    fileprivate var description: String {
        switch _backing {
        case .immutable(let cs):
            return CFCopyDescription(cs) as String
        case .mutable(let cs):
            return CFCopyDescription(cs) as String
        }
    }
    
    fileprivate var debugDescription: String {
        return description
    }
    
    // MARK: -
    
    public func bridgedReference() -> NSCharacterSet {
        switch _backing {
        case .immutable(let cs):
            return cs as NSCharacterSet
        case .mutable(let cs):
            return cs as NSCharacterSet
        }
    }
}

// MARK: -

/**
 A `CharacterSet` represents a set of Unicode-compliant characters. Foundation types use `CharacterSet` to group characters together for searching operations, so that they can find any of a particular set of characters during a search.
 
 This type provides "copy-on-write" behavior, and is also bridged to the Objective-C `NSCharacterSet` class.
*/
public struct CharacterSet : ReferenceConvertible, Equatable, Hashable, SetAlgebra {
    public typealias ReferenceType = NSCharacterSet
    
    fileprivate var _storage : _CharacterSetStorage
    
    // MARK: Init methods
    
    /// Initialize an empty instance.
    public init() {
        // It's unlikely that we are creating an empty character set with no intention to mutate it
        _storage = _CharacterSetStorage(mutableReference: CFCharacterSetCreateMutable(nil))
    }
    
    /// Initialize with a range of integers.
    ///
    /// It is the caller's responsibility to ensure that the values represent valid `Unicode.Scalar` values, if that is what is desired.
    public init(charactersIn range: Range<Unicode.Scalar>) {
        _storage = _CharacterSetStorage(immutableReference: CFCharacterSetCreateWithCharactersInRange(nil, _utfRangeToCFRange(range)))
    }

    /// Initialize with a closed range of integers.
    ///
    /// It is the caller's responsibility to ensure that the values represent valid `Unicode.Scalar` values, if that is what is desired.
    public init(charactersIn range: ClosedRange<Unicode.Scalar>) {
        _storage = _CharacterSetStorage(immutableReference: CFCharacterSetCreateWithCharactersInRange(nil, _utfRangeToCFRange(range)))
    }

    /// Initialize with the characters in the given string.
    ///
    /// - parameter string: The string content to inspect for characters.
    public init(charactersIn string: String) {
        _storage = _CharacterSetStorage(immutableReference: CFCharacterSetCreateWithCharactersInString(nil, string as CFString))
    }
    
    /// Initialize with a bitmap representation.
    ///
    /// This method is useful for creating a character set object with data from a file or other external data source.
    /// - parameter data: The bitmap representation.
    public init(bitmapRepresentation data: Data) {
        _storage = _CharacterSetStorage(immutableReference: CFCharacterSetCreateWithBitmapRepresentation(nil, data as CFData))
    }
    
    /// Initialize with the contents of a file.
    ///
    /// Returns `nil` if there was an error reading the file.
    /// - parameter file: The file to read.
    public init?(contentsOfFile file: String) {
        do {
            let data = try Data(contentsOf: URL(fileURLWithPath: file), options: .mappedIfSafe)
            _storage = _CharacterSetStorage(immutableReference: CFCharacterSetCreateWithBitmapRepresentation(nil, data as CFData))
        } catch {
            return nil
        }
    }

    fileprivate init(_bridged characterSet: NSCharacterSet) {
        _storage = _CharacterSetStorage(immutableReference: characterSet.copy() as! CFCharacterSet)
    }
    
    fileprivate init(_uncopiedImmutableReference characterSet: CFCharacterSet) {
        _storage = _CharacterSetStorage(immutableReference: characterSet)
    }

    fileprivate init(_uncopiedStorage : _CharacterSetStorage) {
        _storage = _uncopiedStorage
    }

    fileprivate init(_builtIn: CFCharacterSetPredefinedSet) {
        _storage = _CharacterSetStorage(immutableReference: CFCharacterSetGetPredefined(_builtIn))
    }
    
    // MARK: Static functions
    
    /// Returns a character set containing the characters in Unicode General Category Cc and Cf.
    public static var controlCharacters : CharacterSet {
        return CharacterSet(_builtIn: .control)
    }
    
    /// Returns a character set containing the characters in Unicode General Category Zs and `CHARACTER TABULATION (U+0009)`.
    public static var whitespaces : CharacterSet {
        return CharacterSet(_builtIn: .whitespace)
    }
    
    /// Returns a character set containing characters in Unicode General Category Z*, `U+000A ~ U+000D`, and `U+0085`.
    public static var whitespacesAndNewlines : CharacterSet {
        return CharacterSet(_builtIn: .whitespaceAndNewline)
    }
    
    /// Returns a character set containing the characters in the category of Decimal Numbers.
    public static var decimalDigits : CharacterSet {
        return CharacterSet(_builtIn: .decimalDigit)
    }
    
    /// Returns a character set containing the characters in Unicode General Category L* & M*.
    public static var letters : CharacterSet {
        return CharacterSet(_builtIn: .letter)
    }
    
    /// Returns a character set containing the characters in Unicode General Category Ll.
    public static var lowercaseLetters : CharacterSet {
        return CharacterSet(_builtIn: .lowercaseLetter)
    }
    
    /// Returns a character set containing the characters in Unicode General Category Lu and Lt.
    public static var uppercaseLetters : CharacterSet {
        return CharacterSet(_builtIn: .uppercaseLetter)
    }
    
    /// Returns a character set containing the characters in Unicode General Category M*.
    public static var nonBaseCharacters : CharacterSet {
        return CharacterSet(_builtIn: .nonBase)
    }
    
    /// Returns a character set containing the characters in Unicode General Categories L*, M*, and N*.
    public static var alphanumerics : CharacterSet {
        return CharacterSet(_builtIn: .alphaNumeric)
    }
    
    /// Returns a character set containing individual Unicode characters that can also be represented as composed character sequences (such as for letters with accents), by the definition of "standard decomposition" in version 3.2 of the Unicode character encoding standard.
    public static var decomposables : CharacterSet {
        return CharacterSet(_builtIn: .decomposable)
    }
    
    /// Returns a character set containing values in the category of Non-Characters or that have not yet been defined in version 3.2 of the Unicode standard.
    public static var illegalCharacters : CharacterSet {
        return CharacterSet(_builtIn: .illegal)
    }
    
    @available(*, unavailable, renamed: "punctuationCharacters")
    public static var punctuation : CharacterSet {
        return CharacterSet(_builtIn: .punctuation)
    }

    /// Returns a character set containing the characters in Unicode General Category P*.
    public static var punctuationCharacters : CharacterSet {
        return CharacterSet(_builtIn: .punctuation)
    }
    
    /// Returns a character set containing the characters in Unicode General Category Lt.
    public static var capitalizedLetters : CharacterSet {
        return CharacterSet(_builtIn: .capitalizedLetter)
    }
    
    /// Returns a character set containing the characters in Unicode General Category S*.
    public static var symbols : CharacterSet {
        return CharacterSet(_builtIn: .symbol)
    }
    
    /// Returns a character set containing the newline characters (`U+000A ~ U+000D`, `U+0085`, `U+2028`, and `U+2029`).
    public static var newlines : CharacterSet {
        return CharacterSet(_builtIn: .newline)
    }
    
    // MARK: Static functions, from NSURL

    /// Returns the character set for characters allowed in a user URL subcomponent.
    public static var urlUserAllowed : CharacterSet {
        if #available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
            return CharacterSet(_uncopiedImmutableReference: _CFURLComponentsGetURLUserAllowedCharacterSet() as NSCharacterSet)
        } else {
            return CharacterSet(_uncopiedImmutableReference: _NSURLComponentsGetURLUserAllowedCharacterSet() as! NSCharacterSet)
        }
    }
    
    /// Returns the character set for characters allowed in a password URL subcomponent.
    public static var urlPasswordAllowed : CharacterSet {
        if #available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
            return CharacterSet(_uncopiedImmutableReference: _CFURLComponentsGetURLPasswordAllowedCharacterSet() as NSCharacterSet)
        } else {
            return CharacterSet(_uncopiedImmutableReference: _NSURLComponentsGetURLPasswordAllowedCharacterSet() as! NSCharacterSet)
        }
    }
    
    /// Returns the character set for characters allowed in a host URL subcomponent.
    public static var urlHostAllowed : CharacterSet {
        if #available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
            return CharacterSet(_uncopiedImmutableReference: _CFURLComponentsGetURLHostAllowedCharacterSet() as NSCharacterSet)
        } else {
            return CharacterSet(_uncopiedImmutableReference: _NSURLComponentsGetURLHostAllowedCharacterSet() as! NSCharacterSet)
        }
    }
    
    /// Returns the character set for characters allowed in a path URL component.
    public static var urlPathAllowed : CharacterSet {
        if #available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
            return CharacterSet(_uncopiedImmutableReference: _CFURLComponentsGetURLPathAllowedCharacterSet() as NSCharacterSet)
        } else {
            return CharacterSet(_uncopiedImmutableReference: _NSURLComponentsGetURLPathAllowedCharacterSet() as! NSCharacterSet)
        }
    }
    
    /// Returns the character set for characters allowed in a query URL component.
    public static var urlQueryAllowed : CharacterSet {
        if #available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
            return CharacterSet(_uncopiedImmutableReference: _CFURLComponentsGetURLQueryAllowedCharacterSet() as NSCharacterSet)
        } else {
            return CharacterSet(_uncopiedImmutableReference: _NSURLComponentsGetURLQueryAllowedCharacterSet() as! NSCharacterSet)
        }
    }
    
    /// Returns the character set for characters allowed in a fragment URL component.
    public static var urlFragmentAllowed : CharacterSet {
        if #available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
            return CharacterSet(_uncopiedImmutableReference: _CFURLComponentsGetURLFragmentAllowedCharacterSet() as NSCharacterSet)
        } else {
            return CharacterSet(_uncopiedImmutableReference: _NSURLComponentsGetURLFragmentAllowedCharacterSet() as! NSCharacterSet)
        }
    }
    
    // MARK: Immutable functions
    
    /// Returns a representation of the `CharacterSet` in binary format.
    @nonobjc
    public var bitmapRepresentation: Data {
        return _storage.bitmapRepresentation
    }
    
    /// Returns an inverted copy of the receiver.
    @nonobjc
    public var inverted : CharacterSet {
        return _storage.inverted
    }
    
    /// Returns true if the `CharacterSet` has a member in the specified plane.
    ///
    /// This method makes it easier to find the plane containing the members of the current character set. The Basic Multilingual Plane (BMP) is plane 0.
    public func hasMember(inPlane plane: UInt8) -> Bool {
        return _storage.hasMember(inPlane: plane)
    }
    
    // MARK: Mutable functions
    
    /// Insert a range of integer values in the `CharacterSet`.
    ///
    /// It is the caller's responsibility to ensure that the values represent valid `Unicode.Scalar` values, if that is what is desired.
    public mutating func insert(charactersIn range: Range<Unicode.Scalar>) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.insert(charactersIn: range)
    }

    /// Insert a closed range of integer values in the `CharacterSet`.
    ///
    /// It is the caller's responsibility to ensure that the values represent valid `Unicode.Scalar` values, if that is what is desired.
    public mutating func insert(charactersIn range: ClosedRange<Unicode.Scalar>) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.insert(charactersIn: range)
    }

    /// Remove a range of integer values from the `CharacterSet`.
    public mutating func remove(charactersIn range: Range<Unicode.Scalar>) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.remove(charactersIn: range)
    }

    /// Remove a closed range of integer values from the `CharacterSet`.
    public mutating func remove(charactersIn range: ClosedRange<Unicode.Scalar>) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.remove(charactersIn: range)
    }

    /// Insert the values from the specified string into the `CharacterSet`.
    public mutating func insert(charactersIn string: String) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.insert(charactersIn: string)
    }
    
    /// Remove the values from the specified string from the `CharacterSet`.
    public mutating func remove(charactersIn string: String) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.remove(charactersIn: string)
    }
    
    /// Invert the contents of the `CharacterSet`.
    public mutating func invert() {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.invert()
    }
    
    // -----
    // MARK: -
    // MARK: SetAlgebraType
    
    /// Insert a `Unicode.Scalar` representation of a character into the `CharacterSet`.
    ///
    /// `Unicode.Scalar` values are available on `Swift.String.UnicodeScalarView`.
    @discardableResult
    public mutating func insert(_ character: Unicode.Scalar) -> (inserted: Bool, memberAfterInsert: Unicode.Scalar) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        return _storage.insert(character)
    }

    /// Insert a `Unicode.Scalar` representation of a character into the `CharacterSet`.
    ///
    /// `Unicode.Scalar` values are available on `Swift.String.UnicodeScalarView`.
    @discardableResult
    public mutating func update(with character: Unicode.Scalar) -> Unicode.Scalar? {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        return _storage.update(with: character)
    }

    
    /// Remove a `Unicode.Scalar` representation of a character from the `CharacterSet`.
    ///
    /// `Unicode.Scalar` values are available on `Swift.String.UnicodeScalarView`.
    @discardableResult
    public mutating func remove(_ character: Unicode.Scalar) -> Unicode.Scalar? {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        return _storage.remove(character)
    }
    
    /// Test for membership of a particular `Unicode.Scalar` in the `CharacterSet`.
    public func contains(_ member: Unicode.Scalar) -> Bool {
        return _storage.contains(member)
    }
    
    /// Returns a union of the `CharacterSet` with another `CharacterSet`.
    public func union(_ other: CharacterSet) -> CharacterSet {
        return _storage.union(other._storage)
    }
    
    /// Sets the value to a union of the `CharacterSet` with another `CharacterSet`.
    public mutating func formUnion(_ other: CharacterSet) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.formUnion(other._storage)
    }
    
    /// Returns an intersection of the `CharacterSet` with another `CharacterSet`.
    public func intersection(_ other: CharacterSet) -> CharacterSet {
        return _storage.intersection(other._storage)
    }
    
    /// Sets the value to an intersection of the `CharacterSet` with another `CharacterSet`.
    public mutating func formIntersection(_ other: CharacterSet) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.formIntersection(other._storage)
    }

    /// Returns a `CharacterSet` created by removing elements in `other` from `self`.
    public func subtracting(_ other: CharacterSet) -> CharacterSet {
        return _storage.subtracting(other._storage)
    }

    /// Sets the value to a `CharacterSet` created by removing elements in `other` from `self`.
    public mutating func subtract(_ other: CharacterSet) {
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.mutableCopy()
        }
        _storage.subtract(other._storage)
    }

    /// Returns an exclusive or of the `CharacterSet` with another `CharacterSet`.
    public func symmetricDifference(_ other: CharacterSet) -> CharacterSet {
        return _storage.symmetricDifference(other._storage)
    }
    
    /// Sets the value to an exclusive or of the `CharacterSet` with another `CharacterSet`.
    public mutating func formSymmetricDifference(_ other: CharacterSet) {
        self = symmetricDifference(other)
    }
    
    /// Returns true if `self` is a superset of `other`.
    public func isSuperset(of other: CharacterSet) -> Bool {
        return _storage.isSuperset(of: other._storage)
    }

    // MARK: -
    
    public var hashValue: Int {
        return _storage.hashValue
    }

    /// Returns true if the two `CharacterSet`s are equal.
    public static func ==(lhs : CharacterSet, rhs: CharacterSet) -> Bool {
        return lhs._storage == rhs._storage
    }
}


// MARK: Objective-C Bridging
extension CharacterSet : _ObjectiveCBridgeable {
    public static func _getObjectiveCType() -> Any.Type {
        return NSCharacterSet.self
    }
    
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSCharacterSet {
        return _storage.bridgedReference()
    }
    
    public static func _forceBridgeFromObjectiveC(_ input: NSCharacterSet, result: inout CharacterSet?) {
        result = CharacterSet(_bridged: input)
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSCharacterSet, result: inout CharacterSet?) -> Bool {
        result = CharacterSet(_bridged: input)
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSCharacterSet?) -> CharacterSet {
        guard let src = source else { return CharacterSet() }
        return CharacterSet(_bridged: src)
    }
    
}

extension CharacterSet : CustomStringConvertible, CustomDebugStringConvertible {
    public var description: String {
        return _storage.description
    }

    public var debugDescription: String {
        return _storage.debugDescription
    }
}

extension NSCharacterSet : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as CharacterSet)
    }
}

extension CharacterSet : Codable {
    private enum CodingKeys : Int, CodingKey {
        case bitmap
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let bitmap = try container.decode(Data.self, forKey: .bitmap)
        self.init(bitmapRepresentation: bitmap)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(self.bitmapRepresentation, forKey: .bitmap)
    }
}
