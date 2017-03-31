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

private func _utfRangeToNSRange(_ inRange : Range<UnicodeScalar>) -> NSRange {
    return NSMakeRange(Int(inRange.lowerBound.value), Int(inRange.upperBound.value - inRange.lowerBound.value))
}

private func _utfRangeToNSRange(_ inRange : ClosedRange<UnicodeScalar>) -> NSRange {
    return NSMakeRange(Int(inRange.lowerBound.value), Int(inRange.upperBound.value - inRange.lowerBound.value + 1))
}

internal final class _SwiftNSCharacterSet : _SwiftNativeNSCharacterSet, _SwiftNativeFoundationType {
    internal typealias ImmutableType = NSCharacterSet
    internal typealias MutableType = NSMutableCharacterSet
    
    var __wrapped : _MutableUnmanagedWrapper<ImmutableType, MutableType>
    
    init(immutableObject: AnyObject) {
        // Take ownership.
        __wrapped = .Immutable(Unmanaged.passRetained(_unsafeReferenceCast(immutableObject, to: ImmutableType.self)))
    }
    
    init(mutableObject: AnyObject) {
        // Take ownership.
        __wrapped = .Mutable(Unmanaged.passRetained(_unsafeReferenceCast(mutableObject, to: MutableType.self)))
    }
    
    internal required init(unmanagedImmutableObject: Unmanaged<ImmutableType>) {
        // Take ownership.
        __wrapped = .Immutable(unmanagedImmutableObject)
        
        super.init()
    }
    
    internal required init(unmanagedMutableObject: Unmanaged<MutableType>) {
        // Take ownership.
        __wrapped = .Mutable(unmanagedMutableObject)
        
        super.init()
    }
    
    deinit {
        releaseWrappedObject()
    }

    @objc(copyWithZone:)
    func copy(with zone: NSZone? = nil) -> Any {
        return _mapUnmanaged { $0.copy(with: zone) }
    }

    @objc(mutableCopyWithZone:)
    func mutableCopy(with zone: NSZone? = nil) -> Any {
        return _mapUnmanaged { $0.mutableCopy(with: zone) }
    }

    @objc
    public var classForCoder: AnyClass {
        return NSCharacterSet.self
    }
}

/**
 A `CharacterSet` represents a set of Unicode-compliant characters. Foundation types use `CharacterSet` to group characters together for searching operations, so that they can find any of a particular set of characters during a search.
 
 This type provides "copy-on-write" behavior, and is also bridged to the Objective-C `NSCharacterSet` class.
*/
public struct CharacterSet : ReferenceConvertible, Equatable, Hashable, SetAlgebra, _MutablePairBoxing {
    public typealias ReferenceType = NSCharacterSet

    internal typealias SwiftNSWrapping = _SwiftNSCharacterSet
    internal typealias ImmutableType = SwiftNSWrapping.ImmutableType
    internal typealias MutableType = SwiftNSWrapping.MutableType
    
    internal var _wrapped : _SwiftNSCharacterSet
    
    // MARK: Init methods
    
    fileprivate init(_bridged characterSet: NSCharacterSet) {
        // We must copy the input because it might be mutable; just like storing a value type in ObjC
        _wrapped = _SwiftNSCharacterSet(immutableObject: characterSet.copy() as AnyObject)
    }
    
    /// Initialize an empty instance.
    public init() {
        _wrapped = _SwiftNSCharacterSet(immutableObject: NSCharacterSet())
    }
    
    /// Initialize with a range of integers.
    ///
    /// It is the caller's responsibility to ensure that the values represent valid `UnicodeScalar` values, if that is what is desired.
    public init(charactersIn range: Range<UnicodeScalar>) {
        _wrapped = _SwiftNSCharacterSet(immutableObject: NSCharacterSet(range: _utfRangeToNSRange(range)))
    }

    /// Initialize with a closed range of integers.
    ///
    /// It is the caller's responsibility to ensure that the values represent valid `UnicodeScalar` values, if that is what is desired.
    public init(charactersIn range: ClosedRange<UnicodeScalar>) {
        _wrapped = _SwiftNSCharacterSet(immutableObject: NSCharacterSet(range: _utfRangeToNSRange(range)))
    }

    /// Initialize with the characters in the given string.
    ///
    /// - parameter string: The string content to inspect for characters.
    public init(charactersIn string: String) {
        _wrapped = _SwiftNSCharacterSet(immutableObject: NSCharacterSet(charactersIn: string))
    }
    
    /// Initialize with a bitmap representation.
    ///
    /// This method is useful for creating a character set object with data from a file or other external data source.
    /// - parameter data: The bitmap representation.
    public init(bitmapRepresentation data: Data) {
        _wrapped = _SwiftNSCharacterSet(immutableObject: NSCharacterSet(bitmapRepresentation: data))
    }
    
    /// Initialize with the contents of a file.
    ///
    /// Returns `nil` if there was an error reading the file.
    /// - parameter file: The file to read.
    public init?(contentsOfFile file: String) {
        if let interior = NSCharacterSet(contentsOfFile: file) {
            _wrapped = _SwiftNSCharacterSet(immutableObject: interior)
        } else {
            return nil
        }
    }

    public var hashValue: Int {
        return _mapUnmanaged { $0.hashValue }
    }
    
    private init(reference: NSCharacterSet) {
        _wrapped = _SwiftNSCharacterSet(immutableObject: reference)
    }
    
    // MARK: Static functions
    
    /// Returns a character set containing the characters in Unicode General Category Cc and Cf.
    public static var controlCharacters : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.controlCharacters as NSCharacterSet)
    }
    
    /// Returns a character set containing the characters in Unicode General Category Zs and `CHARACTER TABULATION (U+0009)`.
    public static var whitespaces : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.whitespaces as NSCharacterSet)
    }
    
    /// Returns a character set containing characters in Unicode General Category Z*, `U+000A ~ U+000D`, and `U+0085`.
    public static var whitespacesAndNewlines : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.whitespacesAndNewlines as NSCharacterSet)
    }
    
    /// Returns a character set containing the characters in the category of Decimal Numbers.
    public static var decimalDigits : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.decimalDigits as NSCharacterSet)
    }
    
    /// Returns a character set containing the characters in Unicode General Category L* & M*.
    public static var letters : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.letters as NSCharacterSet)
    }
    
    /// Returns a character set containing the characters in Unicode General Category Ll.
    public static var lowercaseLetters : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.lowercaseLetters as NSCharacterSet)
    }
    
    /// Returns a character set containing the characters in Unicode General Category Lu and Lt.
    public static var uppercaseLetters : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.uppercaseLetters as NSCharacterSet)
    }
    
    /// Returns a character set containing the characters in Unicode General Category M*.
    public static var nonBaseCharacters : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.nonBaseCharacters as NSCharacterSet)
    }
    
    /// Returns a character set containing the characters in Unicode General Categories L*, M*, and N*.
    public static var alphanumerics : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.alphanumerics as NSCharacterSet)
    }
    
    /// Returns a character set containing individual Unicode characters that can also be represented as composed character sequences (such as for letters with accents), by the definition of "standard decomposition" in version 3.2 of the Unicode character encoding standard.
    public static var decomposables : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.decomposables as NSCharacterSet)
    }
    
    /// Returns a character set containing values in the category of Non-Characters or that have not yet been defined in version 3.2 of the Unicode standard.
    public static var illegalCharacters : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.illegalCharacters as NSCharacterSet)
    }
    
    @available(*, unavailable, renamed: "punctuationCharacters")
    public static var punctuation : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.punctuationCharacters as NSCharacterSet)
    }

    /// Returns a character set containing the characters in Unicode General Category P*.
    public static var punctuationCharacters : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.punctuationCharacters as NSCharacterSet)
    }
    
    /// Returns a character set containing the characters in Unicode General Category Lt.
    public static var capitalizedLetters : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.capitalizedLetters as NSCharacterSet)
    }
    
    /// Returns a character set containing the characters in Unicode General Category S*.
    public static var symbols : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.symbols as NSCharacterSet)
    }
    
    /// Returns a character set containing the newline characters (`U+000A ~ U+000D`, `U+0085`, `U+2028`, and `U+2029`).
    public static var newlines : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.newlines as NSCharacterSet)
    }
    
    // MARK: Static functions, from NSURL

    /// Returns the character set for characters allowed in a user URL subcomponent.
    public static var urlUserAllowed : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.urlUserAllowed as NSCharacterSet)
    }
    
    /// Returns the character set for characters allowed in a password URL subcomponent.
    public static var urlPasswordAllowed : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.urlPasswordAllowed as NSCharacterSet)
    }
    
    /// Returns the character set for characters allowed in a host URL subcomponent.
    public static var urlHostAllowed : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.urlHostAllowed as NSCharacterSet)
    }
    
    /// Returns the character set for characters allowed in a path URL component.
    public static var urlPathAllowed : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.urlPathAllowed as NSCharacterSet)
    }
    
    /// Returns the character set for characters allowed in a query URL component.
    public static var urlQueryAllowed : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.urlQueryAllowed as NSCharacterSet)
    }
    
    /// Returns the character set for characters allowed in a fragment URL component.
    public static var urlFragmentAllowed : CharacterSet {
        return CharacterSet(reference: NSCharacterSet.urlFragmentAllowed as NSCharacterSet)
    }
    
    // MARK: Immutable functions
    
    /// Returns a representation of the `CharacterSet` in binary format.
    public var bitmapRepresentation: Data {
        return _mapUnmanaged { $0.bitmapRepresentation }
    }
    
    /// Returns an inverted copy of the receiver.
    public var inverted : CharacterSet {
        return _mapUnmanaged { $0.inverted }
    }
    
    /// Returns true if the `CharacterSet` has a member in the specified plane.
    ///
    /// This method makes it easier to find the plane containing the members of the current character set. The Basic Multilingual Plane (BMP) is plane 0.
    public func hasMember(inPlane plane: UInt8) -> Bool {
        return _mapUnmanaged { $0.hasMemberInPlane(plane) }
    }
    
    // MARK: Mutable functions
    
    /// Insert a range of integer values in the `CharacterSet`.
    ///
    /// It is the caller's responsibility to ensure that the values represent valid `UnicodeScalar` values, if that is what is desired.
    public mutating func insert(charactersIn range: Range<UnicodeScalar>) {
        let nsRange = _utfRangeToNSRange(range)
        _applyUnmanagedMutation {
          $0.addCharacters(in: nsRange)
        }
    }

    /// Insert a closed range of integer values in the `CharacterSet`.
    ///
    /// It is the caller's responsibility to ensure that the values represent valid `UnicodeScalar` values, if that is what is desired.
    public mutating func insert(charactersIn range: ClosedRange<UnicodeScalar>) {
        let nsRange = _utfRangeToNSRange(range)
        _applyUnmanagedMutation {
            $0.addCharacters(in: nsRange)
        }
    }

    /// Remove a range of integer values from the `CharacterSet`.
    public mutating func remove(charactersIn range: Range<UnicodeScalar>) {
        let nsRange = _utfRangeToNSRange(range)
        _applyUnmanagedMutation {
            $0.removeCharacters(in: nsRange)
        }
    }

    /// Remove a closed range of integer values from the `CharacterSet`.
    public mutating func remove(charactersIn range: ClosedRange<UnicodeScalar>) {
        let nsRange = _utfRangeToNSRange(range)
        _applyUnmanagedMutation {
            $0.removeCharacters(in: nsRange)
        }
    }

    /// Insert the values from the specified string into the `CharacterSet`.
    public mutating func insert(charactersIn string: String) {
        _applyUnmanagedMutation {
            $0.addCharacters(in: string)
        }
    }
    
    /// Remove the values from the specified string from the `CharacterSet`.
    public mutating func remove(charactersIn string: String) {
        _applyUnmanagedMutation {
            $0.removeCharacters(in: string)
        }
    }
    
    /// Invert the contents of the `CharacterSet`.
    public mutating func invert() {
        _applyUnmanagedMutation { $0.invert() }
    }
    
    // -----
    // MARK: -
    // MARK: SetAlgebraType
    
    /// Insert a `UnicodeScalar` representation of a character into the `CharacterSet`.
    ///
    /// `UnicodeScalar` values are available on `Swift.String.UnicodeScalarView`.
    @discardableResult
    public mutating func insert(_ character: UnicodeScalar) -> (inserted: Bool, memberAfterInsert: UnicodeScalar) {
        let nsRange = NSMakeRange(Int(character.value), 1)
        _applyUnmanagedMutation {
            $0.addCharacters(in: nsRange)
        }
        // TODO: This should probably return the truth, but figuring it out requires two calls into NSCharacterSet
        return (true, character)
    }

    /// Insert a `UnicodeScalar` representation of a character into the `CharacterSet`.
    ///
    /// `UnicodeScalar` values are available on `Swift.String.UnicodeScalarView`.
    @discardableResult
    public mutating func update(with character: UnicodeScalar) -> UnicodeScalar? {
        let nsRange = NSMakeRange(Int(character.value), 1)
        _applyUnmanagedMutation {
            $0.addCharacters(in: nsRange)
        }
        // TODO: This should probably return the truth, but figuring it out requires two calls into NSCharacterSet
        return character
    }

    
    /// Remove a `UnicodeScalar` representation of a character from the `CharacterSet`.
    ///
    /// `UnicodeScalar` values are available on `Swift.String.UnicodeScalarView`.
    @discardableResult
    public mutating func remove(_ character: UnicodeScalar) -> UnicodeScalar? {
        // TODO: Add method to NSCharacterSet to do this in one call
        let result : UnicodeScalar? = contains(character) ? character : nil
        let r = NSMakeRange(Int(character.value), 1)
        _applyUnmanagedMutation {
            $0.removeCharacters(in: r)
        }
        return result
    }
    
    /// Test for membership of a particular `UnicodeScalar` in the `CharacterSet`.
    public func contains(_ member: UnicodeScalar) -> Bool {
        return _mapUnmanaged { $0.longCharacterIsMember(member.value) }
    }
    
    /// Returns a union of the `CharacterSet` with another `CharacterSet`.
    public func union(_ other: CharacterSet) -> CharacterSet {
        // The underlying collection does not have a method to return new CharacterSets with changes applied, so we will copy and apply here
        var result = self
        result.formUnion(other)
        return result
    }
    
    /// Sets the value to a union of the `CharacterSet` with another `CharacterSet`.
    public mutating func formUnion(_ other: CharacterSet) {
        _applyUnmanagedMutation { $0.formUnion(with: other) }
    }
    
    /// Returns an intersection of the `CharacterSet` with another `CharacterSet`.
    public func intersection(_ other: CharacterSet) -> CharacterSet {
        // The underlying collection does not have a method to return new CharacterSets with changes applied, so we will copy and apply here
        var result = self
        result.formIntersection(other)
        return result
    }
    
    /// Sets the value to an intersection of the `CharacterSet` with another `CharacterSet`.
    public mutating func formIntersection(_ other: CharacterSet) {
        _applyUnmanagedMutation {
            $0.formIntersection(with: other)
        }
    }

    /// Returns a `CharacterSet` created by removing elements in `other` from `self`.
    public func subtracting(_ other: CharacterSet) -> CharacterSet {
        return intersection(other.inverted)
    }

    /// Sets the value to a `CharacterSet` created by removing elements in `other` from `self`.
    public mutating func subtract(_ other: CharacterSet) {
        self = subtracting(other)
    }

    /// Returns an exclusive or of the `CharacterSet` with another `CharacterSet`.
    public func symmetricDifference(_ other: CharacterSet) -> CharacterSet {
        return union(other).subtracting(intersection(other))
    }
    
    /// Sets the value to an exclusive or of the `CharacterSet` with another `CharacterSet`.
    public mutating func formSymmetricDifference(_ other: CharacterSet) {
        self = symmetricDifference(other)
    }
    
    /// Returns true if `self` is a superset of `other`.
    public func isSuperset(of other: CharacterSet) -> Bool {
        if other.isEmpty {
            return true 
        }
        return _mapUnmanaged { $0.isSuperset(of: other) }
    }

    /// Returns true if the two `CharacterSet`s are equal.
    public static func ==(lhs : CharacterSet, rhs: CharacterSet) -> Bool {
        return lhs._wrapped.isEqual(rhs as NSCharacterSet)
    }
}


// MARK: Objective-C Bridging
extension CharacterSet : _ObjectiveCBridgeable {
    public static func _getObjectiveCType() -> Any.Type {
        return NSCharacterSet.self
    }
    
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSCharacterSet {
        return unsafeBitCast(_wrapped, to: NSCharacterSet.self)
    }
    
    public static func _forceBridgeFromObjectiveC(_ input: NSCharacterSet, result: inout CharacterSet?) {
        result = CharacterSet(_bridged: input)
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSCharacterSet, result: inout CharacterSet?) -> Bool {
        result = CharacterSet(_bridged: input)
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSCharacterSet?) -> CharacterSet {
        return CharacterSet(_bridged: source!)
    }
    
}

extension CharacterSet : CustomStringConvertible, CustomDebugStringConvertible {
    public var description: String {
        return _mapUnmanaged { $0.description }
    }

    public var debugDescription: String {
        return _mapUnmanaged { $0.debugDescription }
    }
}

extension NSCharacterSet : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as CharacterSet)
    }
}

extension _SwiftNSCharacterSet {
    
    // Stubs
    // -----
    
    // Immutable
    
    @objc(bitmapRepresentation)
    var bitmapRepresentation: Data {
        return _mapUnmanaged { $0.bitmapRepresentation }
    }
    
    @objc(invertedSet)
    var inverted : CharacterSet {
        return _mapUnmanaged { $0.inverted }
    }
    
    @objc(hasMemberInPlane:)
    func hasMember(inPlane plane: UInt8) -> Bool {
        return _mapUnmanaged { $0.hasMemberInPlane(plane) }
    }
    
    @objc(characterIsMember:)
    func characterIsMember(_ member: unichar) -> Bool {
        return _mapUnmanaged { $0.characterIsMember(member) }
    }
    
    @objc(longCharacterIsMember:)
    func longCharacterIsMember(_ member: UTF32Char) -> Bool {
        return _mapUnmanaged { $0.longCharacterIsMember(member) }
    }
    
    @objc(isSupersetOfSet:)
    func isSuperset(of other: CharacterSet) -> Bool {
        return _mapUnmanaged {
            // this is a work around for <rdar://problem/27768939>
            return CFCharacterSetIsSupersetOfSet($0 as CFCharacterSet, (other as NSCharacterSet).copy() as! CFCharacterSet)
        }
    }
    
}
