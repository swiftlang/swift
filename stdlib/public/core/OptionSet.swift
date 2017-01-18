//===--- OptionSet.swift --------------------------------------------------===//
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

/// A type that presents a mathematical set interface to a bit mask.
///
/// You use the `OptionSet` protocol to represent bit mask types, where
/// individual bits represent members of the set. Adopting this protocol in
/// your custom types lets you perform set-related operations such as
/// membership tests, unions, and intersections on those types. What's more,
/// when implemented using specific criteria, adoption of this protocol
/// requires no extra work on your part.
///
/// When creating an option set, include a `rawValue` property in your type
/// declaration. The `rawValue` property must be of a type that conforms to
/// the `BitwiseOperations` protocol, such as `Int` or `UInt8`. Next, create
/// unique options as static properties of your custom type using unique
/// powers of two (1, 2, 4, 8, 16, and so forth) for each individual
/// property's raw value so that each property can be represented by a single
/// bit of the type's raw value.
///
/// For example, consider a custom type called `ShippingOptions` that is an
/// option set of the possible ways to ship a customer's purchase.
/// `ShippingOptions` includes a `rawValue` property of type `Int` that stores
/// the bit mask of available shipping options. The static members `NextDay`,
/// `SecondDay`, `Priority`, and `Standard` are unique, individual options.
///
///     struct ShippingOptions: OptionSet {
///         let rawValue: Int
///
///         static let nextDay    = ShippingOptions(rawValue: 1 << 0)
///         static let secondDay  = ShippingOptions(rawValue: 1 << 1)
///         static let priority   = ShippingOptions(rawValue: 1 << 2)
///         static let standard   = ShippingOptions(rawValue: 1 << 3)
///
///         static let express: ShippingOptions = [.nextDay, .secondDay]
///         static let all: ShippingOptions = [.express, .priority, .standard]
///     }
///
/// Declare additional preconfigured option set values as static properties
/// initialized with an array literal containing other option values. In the
/// example, because the `express` static property is assigned an array
/// literal with the `nextDay` and `secondDay` options, it will contain those
/// two elements.
///
/// Using an Option Set Type
/// ========================
///
/// When you need to create an instance of an option set, assign one of the
/// type's static members to your variable or constant. Alternatively, to
/// create an option set instance with multiple members, assign an array
/// literal with multiple static members of the option set. To create an empty
/// instance, assign an empty array literal to your variable.
///
///     let singleOption: ShippingOptions = .priority
///     let multipleOptions: ShippingOptions = [.nextDay, .secondDay, .priority]
///     let noOptions: ShippingOptions = []
///
/// Use set-related operations to check for membership and to add or remove
/// members from an instance of your custom option set type. The following
/// example shows how you can determine free shipping options based on a
/// customer's purchase price:
///
///     let purchasePrice = 87.55
///
///     var freeOptions: ShippingOptions = []
///     if purchasePrice > 50 {
///         freeOptions.insert(.priority)
///     }
///
///     if freeOptions.contains(.priority) {
///         print("You've earned free priority shipping!")
///     } else {
///         print("Add more to your cart for free priority shipping!")
///     }
///     // Prints "You've earned free priority shipping!"
///
/// - SeeAlso: `BitwiseOperations`, `SetAlgebra`
public protocol OptionSet : SetAlgebra, RawRepresentable {
  // We can't constrain the associated Element type to be the same as
  // Self, but we can do almost as well with a default and a
  // constrained extension
  
  /// The element type of the option set.
  ///
  /// To inherit all the default implementations from the `OptionSet` protocol,
  /// the `Element` type must be `Self`, the default.
  associatedtype Element = Self

  // FIXME: This initializer should just be the failable init from
  // RawRepresentable. Unfortunately, current language limitations
  // that prevent non-failable initializers from forwarding to
  // failable ones would prevent us from generating the non-failing
  // default (zero-argument) initializer.  Since OptionSet's main
  // purpose is to create convenient conformances to SetAlgebra,
  // we opt for a non-failable initializer.
  
  /// Creates a new option set from the given raw value.
  ///
  /// This initializer always succeeds, even if the value passed as `rawValue`
  /// exceeds the static properties declared as part of the option set. This
  /// example creates an instance of `ShippingOptions` with a raw value beyond
  /// the highest element, with a bit mask that effectively contains all the
  /// declared static members.
  ///
  ///     let extraOptions = ShippingOptions(rawValue: 255)
  ///     print(extraOptions.isStrictSuperset(of: .all))
  ///     // Prints "true"
  ///
  /// - Parameter rawValue: The raw value of the option set to create. Each bit
  ///   of `rawValue` potentially represents an element of the option set,
  ///   though raw values may include bits that are not defined as distinct
  ///   values of the `OptionSet` type.
  init(rawValue: RawValue)
}

/// `OptionSet` requirements for which default implementations
/// are supplied.
///
/// - Note: A type conforming to `OptionSet` can implement any of
///  these initializers or methods, and those implementations will be
///  used in lieu of these defaults.
extension OptionSet {
  /// Returns a new option set of the elements contained in this set, in the
  /// given set, or in both.
  ///
  /// This example uses the `union(_:)` method to add two more shipping options
  /// to the default set.
  ///
  ///     let defaultShipping = ShippingOptions.standard
  ///     let memberShipping = defaultShipping.union([.secondDay, .priority])
  ///     print(memberShipping.contains(.priority))
  ///     // Prints "true"
  ///
  /// - Parameter other: An option set.
  /// - Returns: A new option set made up of the elements contained in this
  ///   set, in `other`, or in both.
  public func union(_ other: Self) -> Self {
    var r: Self = Self(rawValue: self.rawValue)
    r.formUnion(other)
    return r
  }
  
  /// Returns a new option set with only the elements contained in both this
  /// set and the given set.
  ///
  /// This example uses the `intersection(_:)` method to limit the available
  /// shipping options to what can be used with a PO Box destination.
  ///
  ///     // Can only ship standard or priority to PO Boxes
  ///     let poboxShipping: ShippingOptions = [.standard, .priority]
  ///     let memberShipping: ShippingOptions =
  ///             [.standard, .priority, .secondDay]
  ///
  ///     let availableOptions = memberShipping.intersection(poboxShipping)
  ///     print(availableOptions.contains(.priority))
  ///     // Prints "true"
  ///     print(availableOptions.contains(.secondDay))
  ///     // Prints "false"
  ///
  /// - Parameter other: An option set.
  /// - Returns: A new option set with only the elements contained in both this
  ///   set and `other`.
  public func intersection(_ other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.formIntersection(other)
    return r
  }
  
  /// Returns a new option set with the elements contained in this set or in
  /// the given set, but not in both.
  ///
  /// - Parameter other: An option set.
  /// - Returns: A new option set with only the elements contained in either
  ///   this set or `other`, but not in both.
  public func symmetricDifference(_ other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.formSymmetricDifference(other)
    return r
  }
}

/// `OptionSet` requirements for which default implementations are
/// supplied when `Element == Self`, which is the default.
///
/// - Note: A type conforming to `OptionSet` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension OptionSet where Element == Self {
  /// Returns a Boolean value that indicates whether a given element is a
  /// member of the option set.
  ///
  /// This example uses the `contains(_:)` method to check whether next-day
  /// shipping is in the `availableOptions` instance.
  ///
  ///     let availableOptions = ShippingOptions.express
  ///     if availableOptions.contains(.nextDay) {
  ///         print("Next day shipping available")
  ///     }
  ///     // Prints "Next day shipping available"
  ///
  /// - Parameter member: The element to look for in the option set.
  /// - Returns: `true` if the option set contains `member`; otherwise,
  ///   `false`.
  public func contains(_ member: Self) -> Bool {
    return self.isSuperset(of: member)
  }
  
  /// Adds the given element to the option set if it is not already a member.
  ///
  /// In the following example, the `.secondDay` shipping option is added to
  /// the `freeOptions` option set if `purchasePrice` is greater than 50.0. For
  /// the `ShippingOptions` declaration, see the `OptionSet` protocol
  /// discussion.
  ///
  ///     let purchasePrice = 87.55
  ///
  ///     var freeOptions: ShippingOptions = [.standard, .priority]
  ///     if purchasePrice > 50 {
  ///         freeOptions.insert(.secondDay)
  ///     }
  ///     print(freeOptions.contains(.secondDay))
  ///     // Prints "true"
  ///
  /// - Parameter newMember: The element to insert.
  /// - Returns: `(true, newMember)` if `newMember` was not contained in
  ///   `self`. Otherwise, returns `(false, oldMember)`, where `oldMember` is
  ///   the member of the set equal to `newMember`.
  @discardableResult
  public mutating func insert(
    _ newMember: Element
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    let oldMember = self.intersection(newMember)
    let shouldInsert = oldMember != newMember
    let result = (
      inserted: shouldInsert,
      memberAfterInsert: shouldInsert ? newMember : oldMember)
    if shouldInsert {
      self.formUnion(newMember)
    }
    return result
  }
  
  /// Removes the given element and all elements subsumed by it.
  ///
  /// In the following example, the `.priority` shipping option is removed from
  /// the `options` option set. Attempting to remove the same shipping option
  /// a second time results in `nil`, because `options` no longer contains
  /// `.priority` as a member.
  ///
  ///     var options: ShippingOptions = [.secondDay, .priority]
  ///     let priorityOption = options.remove(.priority)
  ///     print(priorityOption == .priority)
  ///     // Prints "true"
  ///
  ///     print(options.remove(.priority))
  ///     // Prints "nil"
  ///
  /// In the next example, the `.express` element is passed to `remove(_:)`.
  /// Although `.express` is not a member of `options`, `.express` subsumes
  /// the remaining `.secondDay` element of the option set. Therefore,
  /// `options` is emptied and the intersection between `.express` and
  /// `options` is returned.
  ///
  ///     let expressOption = options.remove(.express)
  ///     print(expressOption == .express)
  ///     // Prints "false"
  ///     print(expressOption == .secondDay)
  ///     // Prints "true"
  ///
  /// - Parameter member: The element of the set to remove.
  /// - Returns: The intersection of `[member]` and the set, if the
  ///   intersection was nonempty; otherwise, `nil`.
  @discardableResult
  public mutating func remove(_ member: Element) -> Element? {
    let r = isSuperset(of: member) ? Optional(member) : nil
    self.subtract(member)
    return r
  }

  /// Inserts the given element into the set.
  ///
  /// If `newMember` is not contained in the set but subsumes current members
  /// of the set, the subsumed members are returned.
  ///
  ///     var options: ShippingOptions = [.secondDay, .priority]
  ///     let replaced = options.update(with: .express)
  ///     print(replaced == .secondDay)
  ///     // Prints "true"
  ///
  /// - Returns: The intersection of `[newMember]` and the set if the
  ///   intersection was nonempty; otherwise, `nil`.
  @discardableResult
  public mutating func update(with newMember: Element) -> Element? {
    let r = self.intersection(newMember)
    self.formUnion(newMember)
    return r.isEmpty ? nil : r
  }
}

/// `OptionSet` requirements for which default implementations are
/// supplied when `RawValue` conforms to `BitwiseOperations`,
/// which is the usual case.  Each distinct bit of an option set's
/// `.rawValue` corresponds to a disjoint value of the `OptionSet`.
///
/// - `union` is implemented as a bitwise "or" (`|`) of `rawValue`s
/// - `intersection` is implemented as a bitwise "and" (`&`) of
///   `rawValue`s
/// - `symmetricDifference` is implemented as a bitwise "exclusive or"
///    (`^`) of `rawValue`s
///
/// - Note: A type conforming to `OptionSet` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension OptionSet where RawValue : BitwiseOperations {
  /// Creates an empty option set.
  ///
  /// This initializer creates an option set with a raw value of zero.
  public init() {
    self.init(rawValue: .allZeros)
  }

  /// Inserts the elements of another set into this option set.
  ///
  /// This method is implemented as a `|` (bitwise OR) operation on the
  /// two sets' raw values.
  ///
  /// - Parameter other: An option set.
  public mutating func formUnion(_ other: Self) {
    self = Self(rawValue: self.rawValue | other.rawValue)
  }
  
  /// Removes all elements of this option set that are not 
  /// also present in the given set.
  ///
  /// This method is implemented as a `&` (bitwise AND) operation on the
  /// two sets' raw values.
  ///
  /// - Parameter other: An option set.
  public mutating func formIntersection(_ other: Self) {
    self = Self(rawValue: self.rawValue & other.rawValue)
  }
  
  /// Replaces this set with a new set containing all elements 
  /// contained in either this set or the given set, but not in both.
  ///
  /// This method is implemented as a `^` (bitwise XOR) operation on the two
  /// sets' raw values.
  ///
  /// - Parameter other: An option set.
  public mutating func formSymmetricDifference(_ other: Self) {
    self = Self(rawValue: self.rawValue ^ other.rawValue)
  }
}

@available(*, unavailable, renamed: "OptionSet")
public typealias OptionSetType = OptionSet

