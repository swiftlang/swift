// Interval.swift - Unified and complete interval represenatation.
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// -----------------------------------------------------------------------------
///
/// This file comtains the definitaion of the `Interval` generic struct, along
/// with its extensions. Extensions of `Comparable`, `Range`, and `Collection`
/// that closely relate to `Interval` are also contained within, but potentially
/// will be moved into their own files.
///
// -----------------------------------------------------------------------------

// TODO: Decide whether to allow closed unbounded intervals: [-∞, ∞],
// and up date the documentation if the decision is yes.

// TODO: Update the entire documentation as the prototype progresses.

/// An interval.
///
/// An interval has a lower boundary and an upper boundary, and a lower endpoint
/// and an upper endpoint. It represents a range, or a continuous set, of values
/// of a `Comparable` type. Each boundary can be either closed or open, and each
/// endpoint can be either bounded or unbounded. A closed boundary includes its
/// corresponding endpoint in the interval, while an open boundary does not. A
/// bounded endpoint provides a tangible boundary for the interval; an unbounded
/// lower endpoint is equivalent to the abstract negative infinity, an unbounded
/// upper endpoint the positive infinity. An unbounded endpoint always comes
/// with an open boundary.
///
/// Based of its boundries and endpoints, an interval can be empty, degenerate,
/// or propper. An empty interval contains no members, a degenerate interval 1
/// and only 1 member, and a proper interval more than 1 member.
///
/// An interval whose `Member` conforms to the `Strideable` protocol is iterated
/// in the inverse (descending) order, if `isInverse` is set to `true`.
///
public struct Interval<Member: Hashable & Comparable>: Hashable {
  
  // MARK: - Supporting Types
  
  /// A boundary's style, either closed or open.
  public enum Boundary: Hashable {
    
    /// Converts the boundary style from the "inclusive"/"exclusive"
    /// spelling to the "closed"/"open" spelling.
    /// - Parameter boundaryClusivity: The given
    ///   "inclusive"/"exclusive" spelling.
    public init(clusivity boundaryClusivity: BoundaryClusivity) {
      switch boundaryClusivity {
      case .exclusive: self = .open
      case .inclusive: self = .closed
      }
    }
    
    /// A closed boundary
    case closed
    /// An open boundary
    case open
  }
  
  /// A boundary's style, either inclusive or exclusive.
  public enum BoundaryClusivity {
    /// An inclusive boundary.
    case inclusive
    /// An exclusive boundary.
    case exclusive
  }
  
  /// An endpoint's style, either bounded or unbounded.
  public enum Endpoint: Hashable {
    /// A bounded endpoint with a value of `Member` type.
    case bounded(_ value: Member)
    /// An unbounded endpoint.
    case unbounded
  }
  
  // MARK: - Interval-Defining Properties
  
  /// The interval's lower boundary.
  public let lowerBoundary: Boundary
  
  /// The interval's upper boundary.
  public let upperBoundary: Boundary
  
  /// The interval's lower endpoint.
  public let lowerEndpoint: Endpoint
  
  /// The interval's upper endpoint.
  public let upperEndpoint: Endpoint
  
  // MARK: - Creating an Interval
  
  /// Creates an interval with the given boundaries and endpoints.
  /// - Parameters:
  ///   - lowerBoundary: The interval's lower boundary.
  ///   - lowerEndpoint: The interval's lower endpoint.
  ///   - upperEndpoint: The interval's upper endpoint.
  ///   - upperBoundary: The interval's upper boundary.
  ///   - isInverse: Whether the interval is iterated in the inverse
  ///     (descending) direction, if `Member` conforms to `Strideable`,
  ///     default `false`.
  @inlinable
  public init(
    lowerBoundary: Boundary,
    lowerEndpoint: Endpoint,
    upperEndpoint: Endpoint,
    upperBoundary: Boundary,
    inInverseStridingDirection isInverse: Bool = false
  ) {
    
    self.lowerBoundary = lowerBoundary
    self.lowerEndpoint = lowerEndpoint
    self.upperEndpoint = upperEndpoint
    self.upperBoundary = upperBoundary
    
    isLowerClosed = lowerBoundary == .closed
    isUpperClosed = upperBoundary == .closed
    
    // Compare against `.unbounded` instead of `bounded`,
    // in order to avoid getting the associated values using a switch-case.
    
    isLowerBounded = lowerEndpoint != .unbounded
    isUpperBounded = upperEndpoint != .unbounded
    
    // An interval must be proper if it's not bounded.
    
    if case let .bounded(lowerEndpoint) = lowerEndpoint,
       case let .bounded(upperEndpoint) = upperEndpoint {
      
      isEmpty = lowerEndpoint > upperEndpoint || (
        isOpen && (
          lowerEndpoint == upperEndpoint
            || lowerEndpoint.borders(on: upperEndpoint)
        )
      )
      
      isDegenerate = !isEmpty && (
        (isClosed && lowerEndpoint == upperEndpoint)
          || (isHalfOpen && lowerEndpoint.borders(on: upperEndpoint))
          || (isOpen && lowerEndpoint.sharesCommonNeighbor(with: upperEndpoint))
      )
      
    }
    
    self.isInverse = isInverse
    
  }
  
  /// Creates an interval with the given boundaries and endpoints.
  /// - Parameters:
  ///   - lowerEndpoint: The interval's lower endpoint.
  ///   - lowerBoundaryClusivity: Whether the interval's lower boundary includes
  ///     or excludes its corresponding endpoint.
  ///   - upperEndpoint: The interval's upper endpoint.
  ///   - upperBoundaryClusivity: Whether the interval's upper boundary includes
  ///     or excludes its corresponding endpoint.
  ///   - isInverse: Whether the interval is iterated in the inverse
  ///     (descending) direction, if `Member` conforms to `Strideable`,
  ///     default `false`.
  @inlinable
  public init(
    from lowerEndpoint: Member, _ lowerBoundaryClusivity: BoundaryClusivity,
    to upperEndpoint: Member, _ upperBoundaryClusivity: BoundaryClusivity,
    inInverseStridingDirection isInverse: Bool = false
  ) {
    self.init(
      lowerBoundary: Boundary(clusivity: lowerBoundaryClusivity),
      lowerEndpoint: .bounded(lowerEndpoint),
      upperEndpoint: .bounded(upperEndpoint),
      upperBoundary: Boundary(clusivity: upperBoundaryClusivity),
      inInverseStridingDirection: isInverse
    )
  }
  
  /// Creates an interval with the given endpoints and overall boundary style.
  /// - Parameters:
  ///   - lowerEndpoint: The interval's lower endpoint.
  ///   - upperEndpoint: The interval's upper endpoint.
  ///   - boundaryClusivity: Whether the interval's boundaries include or
  ///     exclude both endpoints.
  ///   - isInverse: Whether the interval is iterated in the inverse
  ///     (descending) direction, if `Member` conforms to `Strideable`,
  ///     default `false`.
  @inlinable
  public init(
    from lowerEndpoint: Member, to upperEndpoint: Member,
    _ boundaryClusivity: BoundaryClusivity,
    inInverseStridingDirection isInverse: Bool = false
  ) {
    self.init(
      from: lowerEndpoint, boundaryClusivity,
      to: upperEndpoint, boundaryClusivity,
      inInverseStridingDirection: isInverse
    )
  }
  
  /// Creates an upper-unbounded interval with the given lower boundary and
  /// endpoint.
  /// - Parameters:
  ///   - lowerEndpoint: The interval's lower endpoint.
  ///   - lowerBoundaryClusivity: Whether the interval's lower boundary includes
  ///     or excludes its corresponding endpoint.
  ///   - isInverse: Whether the interval is iterated in the inverse
  ///     (descending) direction, if `Member` conforms to `Strideable`,
  ///     default `false`.
  @inlinable
  public init(
    toUnboundedFrom lowerEndpoint: Member,
    _ lowerBoundaryClusivity: BoundaryClusivity,
    inInverseStridingDirection isInverse: Bool = false
  ) {
    self.init(
      lowerBoundary: Boundary(clusivity: lowerBoundaryClusivity),
      lowerEndpoint: .bounded(lowerEndpoint),
      upperEndpoint: .unbounded,
      upperBoundary: .open,
      inInverseStridingDirection: isInverse
    )
  }
  
  /// Creates a lower-unbounded interval with the given upper boundary and
  /// endpoint.
  /// - Parameters:
  ///   - upperEndpoint: The interval's upper endpoint.
  ///   - upperBoundaryClusivity: Whether the interval's upper boundary includes
  ///     or excludes its corresponding endpoint.
  ///   - isInverse: Whether the interval is iterated in the inverse
  ///     (descending) direction, if `Member` conforms to `Strideable`,
  ///     default `false`.
  @inlinable
  public init(
    fromUnboundedTo upperEndpoint: Member,
    _ upperBoundaryClusivity: BoundaryClusivity,
    inInverseStridingDirection isInverse: Bool = false
  ) {
    self.init(
      lowerBoundary: .open,
      lowerEndpoint: .unbounded,
      upperEndpoint: .bounded(upperEndpoint),
      upperBoundary: Boundary(clusivity: upperBoundaryClusivity),
      inInverseStridingDirection: isInverse
    )
  }
  
  // MARK: - Creating an Interval from a Range
  
  /// Creates an interval from the given closed range.
  /// - Parameter closedRange: The closed range.
  @inlinable
  public init(closedRange: ClosedRange<Member>) {
    self.init(
      from: closedRange.lowerBound,
      to: closedRange.upperBound,
      .inclusive
    )
  }
  
  /// Creates an interval from the given lower-closed bounded range.
  /// - Parameter range: The lower-closed bounded range.
  @inlinable
  public init(range: Range<Member>) {
    self.init(
      from: range.lowerBound, .inclusive,
      to: range.upperBound, .exclusive
    )
  }
  
  /// Creates an interval from the given lower-bounded partial range.
  /// - Parameter lowerBoundedPartialRange: The lower-bounded partial range.
  @inlinable
  public init(lowerBoundedPartialRange: PartialRangeFrom<Member>) {
    self.init(
      toUnboundedFrom: lowerBoundedPartialRange.lowerBound,
      .inclusive
    )
  }
  
  /// Creates an interval from the given upper-bounded and -closed partial
  /// range.
  /// - Parameter upperBoundedAndClosedPartialRange: The upper-bounded and
  ///   -closed partial range.
  @inlinable
  public init(upperBoundedAndClosedPartialRange: PartialRangeThrough<Member>) {
    self.init(
      toUnboundedFrom: upperBoundedAndClosedPartialRange.upperBound,
      .inclusive
    )
  }
  
  /// Creates an interval from the given upper-bounded and -open partial range.
  /// - Parameter upperBoundedAndOpenPartialRange: The upper-bounded and -open
  ///   partial range.
  @inlinable
  public init(upperBoundedAndOpenPartialRange: PartialRangeUpTo<Member>) {
    self.init(
      toUnboundedFrom: upperBoundedAndOpenPartialRange.upperBound,
      .exclusive
    )
  }
  
  /// Creates an interval from the given unbounded range.
  /// - Parameter unboundedRange: The unbounded range.
  @inlinable
  public init(unboundedRange: UnboundedRange) {
    self = Self.unbounded
  }
  
  // MARK: - Special Intervals
  
  /// An unbounded interval.
  public static var unbounded: Self {
    .init(
      lowerBoundary: .open,
      lowerEndpoint: .unbounded,
      upperEndpoint: .unbounded,
      upperBoundary: .open
    )
  }
  
  // MARK: - Boundary Styles
  
  /// A Boolean value indicating whether the interval is closed.
  public var isClosed: Bool { isLowerClosed && isUpperClosed }
  
  /// A Boolean value indicating whether the interval is open.
  public var isOpen: Bool { isLowerOpen && isUpperOpen }
  
  /// A Boolean value indicating whether the interval is half-open.
  public var isHalfOpen: Bool {
    // This is probably slightly more perforant than !(isClosed || isOpen)
    (isLowerOpen && isUpperClosed) || (isLowerClosed && isUpperOpen)
  }
  
  /// A Boolean value indicating whether the interval's lower boundary is
  /// closed.
  public let isLowerClosed: Bool
  
  /// A Boolean value indicating whether the interval's upper boundary is
  /// closed.
  public let isUpperClosed: Bool
  
  /// A Boolean value indicating whether the interval's lower boundary is open.
  public var isLowerOpen: Bool { !isLowerClosed }
  
  /// A Boolean value indicating whether the interval's upper boundary is open.
  public var isUpperOpen: Bool { !isUpperClosed }
  
  // MARK: - Endpoint Styles
  
  /// A Boolean value indicating whether the interval is bounded.
  public var isBounded: Bool { isLowerBounded && isUpperBounded }
  
  /// A Boolean value indicating whether the interval is unbounded.
  public var isUnbounded: Bool { !isLowerUnbounded && isUpperUnbounded }
  
  /// A Boolean value indicating whether the interval is half-bounded.
  public var isHalfBounded: Bool {
    // This is probably slightly more perforant than !(isBounded || isUnbounded)
    (isLowerBounded && isUpperUnbounded) || (isLowerUnbounded && isUpperBounded)
  }
  
  /// A Boolean value indicating whether the interval's lower endpoint is
  /// bounded.
  public let isLowerBounded: Bool
  
  /// A Boolean value indicating whether the interval's upper endpoint is
  /// bounded.
  public let isUpperBounded: Bool
  
  /// A Boolean value indicating whether the interval's lower endpoint is
  /// unbounded.
  public var isLowerUnbounded: Bool { !isLowerBounded }
  
  /// A Boolean value indicating whether the interval's upper endpoint is
  /// unbounded.
  public var isUpperUnbounded: Bool { !isUpperBounded }
  
  // MARK: - Interval Cardinality
  
  /// A Boolean value indicating whether the interval contains no members.
  public let isEmpty: Bool = false
  
  /// A Boolean value indicating whether the interval contains 1 and only 1
  /// member.
  public let isDegenerate: Bool = false
  
  /// A Boolean value indicating whether the interval contains more than 1
  /// member.
  public var isProper: Bool { !isEmpty && !isDegenerate }
  
  // MARK: - Related Intervals
  
  // The discription for `interior` and `closure are lifted straightly from
  // Wikipedia: https://en.wikipedia.org/wiki/Interval_(mathematics)#Terminology
  
  /// The interval's interior.
  ///
  /// The interior of an interval _I_ is the largest open interval that is
  /// contained in _I_; it is also the set of points in _I_ which are not
  /// endpoints of _I_.
  ///
  /// An empty interval's interior is an empty interior.
  @inlinable
  public var interior: Self? {
//    guard self.isProper else { return nil }
    // An empty interval's interior is an empty interval.
    return Self(
      lowerBoundary: .open,
      lowerEndpoint: self.lowerEndpoint,
      upperEndpoint: self.upperEndpoint,
      upperBoundary: .open
    )
  }
  
  // TODO: Decide if an unbounded interval can be an closed interval.
  
  /// The interval's closure.
  ///
  /// The closure of an interval _I_ is the smallest closed interval that
  /// contains _I_; it is also the set _I_ augmented with its finite endpoints.
  ///
  @inlinable
  public var closure: Self? {
    guard self.isBounded else { return nil }
    
    return Self(
      lowerBoundary: .closed,
      lowerEndpoint: self.lowerEndpoint,
      upperEndpoint: self.upperEndpoint,
      upperBoundary: .closed
    )
  }
  
  // MARK: - Iterating Direction
  
  // This part is not spun off into an extension `where Member: Strideable` is
  // because extensions can not have stored instance properties, and `isInverse`
  // has to be an instance property.
  
  /// A Boolean value indicating whether the interval is iterated in the inverse
  /// (descending) direction.
  ///
  /// It's only effective if `Member` conforms to `Strideable`.
  public var isInverse: Bool = false
  
  /// Reverses the interval's iterating direction.
  ///
  /// It's only effective if `Member` conforms to `Strideable`.
  @inlinable
  public mutating func reverse() {
    isInverse.toggle()
  }
  
  /// Creates a copy of the interval with its iterating direction reversed.
  ///
  /// It's only effective if `Member` conforms to `Strideable`.
  ///
  /// - Returns: A copy of the interval with its iterating direction reversed.
  @inlinable
  func reversed() -> Self {
    var newInterval = self
    newInterval.reverse()
    return newInterval
  }
  
  // MARK: - Comparing Intervals
  
  /// Returns a Boolean value that indicates whether this interval is a
  /// subinterval of the given other interval.
  /// - Parameter other: The other interval.
  /// - Returns: `true` if the interval is a subinterval of `other`; otherwise,
  ///   `false`.
  @inlinable
  public func isSubinterval(of other: Self) -> Bool {
    self.assumingBothLowerUnboundedIsContained(within: other)
      && self.assumingBothUpperUnboundedIsContained(within: other)
  }
  
  /// Returns a Boolean value that indicates whether this interval is a strict
  /// subinterval of the given other interval.
  /// - Parameter other: The other interval.
  /// - Returns: `true` if the interval is a strict subinterval of `other`;
  ///   otherwise, `false`.
  @inlinable
  public func isStrictSubinterval(of other: Self) -> Bool {
    (self.isSubinterval(of: other) && self != other) &&
      // Because empty intervals still contain their lower and upper endpoint
      // and boundary information, they need special handling.
      !(self.isEmpty && other.isEmpty)
  }
  
  /// Returns a Boolean value that indicates whether this interval is a
  /// superinterval of the given other interval.
  /// - Parameter other: The other interval.
  /// - Returns: `true` if the interval is a superinterval of `other`;
  ///   otherwise, `false`.
  @inlinable
  public func isSuperinterval(of other: Self) -> Bool {
    other.isSubinterval(of: self)
  }
  
  /// Returns a Boolean value that indicates whether this interval is a strict
  /// superinterval of the given other interval.
  /// - Parameter other: The other interval.
  /// - Returns: `true` if the interval is a strict superinterval of `other`;
  ///   otherwise, `false`.
  @inlinable
  public func isStrictSuperinterval(of other: Self) -> Bool {
    other.isStrictSubinterval(of: self)
  }
  
  /// Returns a Boolean value that indicates whether this interval overlaps the
  /// given other interval.
  /// - Parameter other: The other interval.
  /// - Returns: `true` if the interval overlaps `other`; otherwise, `false`.
  @inlinable
  public func overlaps(_ other: Self) -> Bool {
    !(self.fullyPrecedes(other) || self.fullySucceeds(other))
  }
  
  /// Returns a Boolean value that indicates whether this interval fully
  /// precedes the given other interval.
  /// - Parameter other: The other interval.
  /// - Returns: `true` if the interval fully precedes `other`; otherwise,
  ///   `false`.
  @inlinable
  public func fullyPrecedes(_ other: Self) -> Bool {
    guard
      case let .bounded(selfUpperEndpoint) = self.upperEndpoint,
      case let .bounded(otherLowerEndpoint) = other.lowerEndpoint
    else { return false }
    
    return selfUpperEndpoint < otherLowerEndpoint
  }
  
  /// Returns a Boolean value that indicates whether this interval fully
  /// succeeds the given other interval.
  /// - Parameter other: The other interval.
  /// - Returns: `true` if the interval fully succeeds `other`; otherwise,
  ///   `false`.
  @inlinable
  public func fullySucceeds(_ other: Self) -> Bool {
    guard
      case let .bounded(otherUpperEndpoint) = other.upperEndpoint,
      case let .bounded(selfLowerEndpoint) = self.lowerEndpoint
    else { return false }
    
    return  otherUpperEndpoint < selfLowerEndpoint
  }
  
  /// Returns a Boolean value that indicates whether this interval's lower
  /// endpoint fully precedes the given other interval's.
  /// - Parameter other: The other interval.
  /// - Returns: `true` if the interval's lower endpoint fully precedes the
  ///   other's; otherwise, `false`.
  @inlinable
  private func assumingBothUpperUnboundedIsContained(
    within other: Self
  ) -> Bool {
    guard
      case let .bounded(selfLowerEndpoint) = self.lowerEndpoint,
      case let .bounded(otherLowerEndpoint) = other.lowerEndpoint
    else { return other.isLowerUnbounded || self.isLowerBounded }
    
    return selfLowerEndpoint >= otherLowerEndpoint
  }
  
  /// Returns a Boolean value that indicates whether this interval's upper
  /// endpoint fully succeeds the given other interval's.
  /// - Parameter other: The other interval.
  /// - Returns: `true` if the interval's upper endpoint fully succeeds the
  ///   other's; otherwise, `false`.
  @inlinable
  private func assumingBothLowerUnboundedIsContained(
    within other: Self
  ) -> Bool {
    guard
      case let .bounded(selfUpperEndpoint) = self.upperEndpoint,
      case let .bounded(otherUpperEndpoint) = other.upperEndpoint
    else { return other.isUpperUnbounded || self.isUpperBounded }
    
    return selfUpperEndpoint <= otherUpperEndpoint
  }
  
  // MARK: - Testing for Membership
  
  /// Returns a Boolean value indicating whether the given value is contained
  /// within the interval.
  /// - Parameter value: The value to check for containment.
  /// - Returns: `true` if `value` is contained within the interval; otherwise,
  ///   `false`.
  @inlinable
  public func contains(_ value: Member) -> Bool {
    if self.isUnbounded { return true }
    
    var valueIsAboveLowerEndpoint: Bool
    var valueIsBelowUpperEndpoint: Bool
    
    if case let .bounded(lowerEndpoint) = lowerEndpoint {
      valueIsAboveLowerEndpoint = value > lowerEndpoint
        || (self.isLowerClosed && value == lowerEndpoint)
    } else { valueIsAboveLowerEndpoint = true }
    
    if case let .bounded(upperEndpoint) = upperEndpoint {
      valueIsBelowUpperEndpoint = value < upperEndpoint
        || (self.isUpperClosed && value == upperEndpoint)
    } else { valueIsBelowUpperEndpoint = true }
    
    return valueIsAboveLowerEndpoint && valueIsBelowUpperEndpoint
  }
  
  /// Returns a Boolean value indicating whether a value is included in an
  /// interval.
  ///
  /// You can use the pattern-matching operator (`~=`) to test whether a value
  /// is included in an interval. The pattern-matching operator is used
  /// internally in `case` statements for pattern matching. The following
  /// example uses the `~=` operator to test whether an integer is included in
  /// an interval of single-digit numbers:
  ///
  ///     let chosenNumber = 3
  ///     if 0<~~<=10 ~= chosenNumber {
  ///         print("\(chosenNumber) is a single digit.")
  ///     }
  ///     // Prints "3 is a single digit."
  ///
  /// - Parameters:
  ///   - pattern: An interval
  ///   - bound: A value to match against `pattern`.
  /// - Returns: `true` if `value` is contained within `pattern`; otherwise,
  ///   `false`.
  @inlinable
  public static func ~= (pattern: Self, value: Member) -> Bool {
    return pattern.contains(value)
  }
  
  // MARK: - Converting Intervals
  
  /// Returns the interval of indices described by another interval within the
  /// given collection.
  ///
  /// You can use the `relative(to:)` method to convert an interval, which could
  /// be half- or fully unbounded, into a bounded interval. The _new_ bounded
  /// lower endpoint will have a closed boundary, and upper endpoint open. The
  /// following example uses this method to convert `(-∞, 4)` into `[0, 4)`,
  /// using an array instance to bound the interval with a lower endpoint.
  ///
  ///     let numbers = [10, 20, 30, 40, 50, 60, 70]
  ///     let upToFour = ~~~~<4
  ///
  ///     let i1 = upToFour.relative(to: numbers)
  ///     // i1 == 0<=~~<4
  ///
  /// The `i1` interval is bounded on the lower boundary by `0` because that is
  /// the starting index of the `numbers` array. When the collection passed to
  /// `relative(to:)` starts with a different index, that index is used as the
  /// lower endpoint instead. The next example creates a slice of `numbers`
  /// starting at index `2`, and then uses the slice with `relative(to:)` to
  /// convert `upToFour` to a bounded interval.
  ///
  ///     let numbersSuffix = numbers[2<=~~~]
  ///     // numbersSuffix == [30, 40, 50, 60, 70]
  ///
  ///     let i2 = upToFour.relative(to: numbersSuffix)
  ///     // i2 == 2<=~~<4
  ///
  /// Use this method only if you need the bounded interval it produces. To
  /// access a slice of a collection using an interval, use the collection's
  /// generic subscript that uses an interval as its parameter.
  ///
  ///     let numbersPrefix = numbers[upToFour]
  ///     // numbersPrefix == [10, 20, 30, 40]
  ///
  /// - Parameter collection: The collection to evaluate this interval in
  ///   relation to.
  /// - Returns: An interval suitable for slicing `collection`. The returned
  ///   interval is _not_ guaranteed to be inside the bounds of `collection`.
  ///   Callers should apply the same preconditions to the return value as they
  ///   would to an interval provided directly by the user.
  @inlinable
  public func relative<C: Collection>(
    to collection: C
  ) -> Interval<Member> where C.Index == Member {
    
    if self.isBounded { return self }
    
    var newLowerBoundary: Boundary
    var newUpperBoundary: Boundary
    
    var newLowerEndpoint: Endpoint
    var newUpperEndpoint: Endpoint
    
    if self.isLowerUnbounded {
      newLowerEndpoint = .bounded(collection.startIndex)
      newLowerBoundary = .closed
    } else {
      newLowerEndpoint = self.lowerEndpoint
      newLowerBoundary = self.lowerBoundary
    }
    
    if self.isUpperUnbounded {
      newUpperEndpoint = .bounded(collection.endIndex)
      newUpperBoundary = .open
    } else {
      newUpperEndpoint = self.upperEndpoint
      newUpperBoundary = self.upperBoundary
    }
    
    return Interval(
      lowerBoundary: newLowerBoundary,
      lowerEndpoint: newLowerEndpoint,
      upperEndpoint: newUpperEndpoint,
      upperBoundary: newUpperBoundary
    )
    
  }
  
}

// MARK: - CustomStringConvertible Conformance

extension Interval: CustomStringConvertible where
  Member: CustomStringConvertible {
  
  public var description: String {
    
    let lowerBoundaryCharacter: Character = self.isLowerBounded ? "[" : "("
    let upperBoundaryCharacter: Character = self.isUpperBounded ? "]" : ")"
    
    var lowerEndpointString: String
    var upperEndpointString: String
    
    switch lowerEndpoint {
    case .unbounded:
      lowerEndpointString = "-∞"
    case .bounded(let lowerEndpoint):
      lowerEndpointString = String(describing: lowerEndpoint)
    }
    
    switch upperEndpoint {
    case .unbounded:
      upperEndpointString = "∞"
    case .bounded(let upperEndpoint):
      upperEndpointString = String(describing: upperEndpoint)
    }
    
    return """
      \(lowerBoundaryCharacter)\(lowerEndpointString), \
      \(upperEndpointString)\(upperBoundaryCharacter)
      """
    
  }
  
}

// MARK: - LosslessStringConvertible Conformance

extension Interval: LosslessStringConvertible where
  Member: LosslessStringConvertible {
  
  @inlinable
  public init?(_ description: String) {
    
    guard
      let lowerBoundaryCharacter = description.first,
      let upperBoundaryCharacter = description.last
    else { return nil }
    
    let descriptionSansBoundaryCharacters = description
      .prefix(upTo: description.endIndex)
      .suffix(from: description.index(after: description.startIndex))
      .drop { $0.isWhitespace }
    
    let endpointStrings = descriptionSansBoundaryCharacters
      .split(separator: ",")
    
    guard endpointStrings.count == 2 else { return nil }
    
    let lowerEndpointString = endpointStrings[0]
    let upperEndpointString = endpointStrings[1]
    
    var lowerBoundary: Boundary
    var upperBoundary: Boundary
    
    var lowerEndpoint: Endpoint
    var upperEndpoint: Endpoint
    
    switch lowerBoundaryCharacter {
    case "[": lowerBoundary = .closed
    case "(": lowerBoundary = .open
    default: return nil
    }
    
    switch upperBoundaryCharacter {
    case "]": upperBoundary = .closed
    case ")": upperBoundary = .open
    default: return nil
    }
    
    switch lowerEndpointString {
    case "-∞":
      lowerEndpoint = .unbounded
    default:
      guard let lowerEndpointValue = Member(String(lowerEndpointString)) else {
        return nil
      }
      lowerEndpoint = .bounded(lowerEndpointValue)
    }
    
    switch upperEndpointString {
    case "+∞", "∞":
      upperEndpoint = .unbounded
    default:
      guard let upperEndpointValue = Member(String(upperEndpointString)) else {
        return nil
      }
      upperEndpoint = .bounded(upperEndpointValue)
    }
    
    self.init(
      lowerBoundary: lowerBoundary,
      lowerEndpoint: lowerEndpoint,
      upperEndpoint: upperEndpoint,
      upperBoundary: upperBoundary
    )
    
  }
  
}

// MARK: - Comparable Extensions for Testing Proximity between Values

extension Comparable where Self: Strideable {
  /// Tests if this value is right next to the given other value.
  /// - Parameter other: The given other value.
  /// - Returns: `true` if the 2 values are right next to each other, `false`
  ///   otherwise.
  @inlinable
  public func borders(on other: Self) -> Bool {
    self.separates(from: other, byDegrees: 1)
  }
  
  /// Tests if this value shares a common neighbor with the given other value.
  /// - Parameter other: The given other value.
  /// - Returns: `true` if the 2 values are both right next to a 3rd value,
  ///   `false` otherwise.
  /// - Note: The 2 values could be equal, if the function returns true.
  @inlinable
  public func sharesCommonNeighbor(with other: Self) -> Bool {
    self.separates(from: other, byDegrees: 2) || self == other
  }
  
  /// Tests the correctness of the Bacon number between this value and the given
  /// other value.
  /// - Parameters:
  ///   - other: The given other value.
  ///   - degrees: The Bacon number to test for.
  /// - Returns: `true` if the Bacon number is correct, `false` otherwise.
  @inlinable
  public func separates(
    from other: Self, byDegrees degrees: Self.Stride
  ) -> Bool {
    other == self.advanced(by: degrees) || self == other.advanced(by: degrees)
  }
}

extension Comparable {
  /// Tests if the value is right next to the given other value.
  /// - Parameter other: The given other value.
  /// - Returns: `true` if the 2 values are right next to each other, `false`
  ///   otherwise.
  @inlinable
  public func borders(on other: Self) -> Bool { false }
  
  /// Tests if the value shares a common neighbor with the given other value.
  /// - Parameter other: The given other value.
  /// - Returns: `true` if the 2 values are both right next to a 3rd value,
  ///   `false` otherwise.
  /// - Note: The 2 values could be equal, if the function returns true.
  @inlinable
  public func sharesCommonNeighbor(with other: Self) -> Bool {
    self == other
  }
}

// MARK: - Comparable Extension for Interval Operators

extension Comparable where Self: Hashable {
  
  /// Creates a closed and bounded interval from the given endpoints.
  /// - Parameters:
  ///   - lowerEndpoint: The given lower endpoint.
  ///   - upperEndpoint: The given upper endpoint.
  /// - Returns: A closed and bounded interval.
  @_transparent
  public static func <=~<= (
    lowerEndpoint: Self,
    upperEndpoint: Self
  ) -> Interval<Self> {
    return Interval(from: lowerEndpoint, to: upperEndpoint, .inclusive)
  }
  
  /// Creates a lower-closed, upper-open, and bounded interval from the given
  /// endpoints.
  /// - Parameters:
  ///   - lowerEndpoint: The given lower endpoint.
  ///   - upperEndpoint: The given upper endpoint.
  /// - Returns: A lower-closed, upper-open, and bounded interval.
  @_transparent
  public static func <=~~< (
    lowerEndpoint: Self,
    upperEndpoint: Self
  ) -> Interval<Self> {
    return Interval(
      from: lowerEndpoint, .inclusive,
      to: upperEndpoint, .exclusive
    )
  }
  
  /// Creates a lower-open, upper-closed, and bounded interval from the given
  /// endpoints.
  /// - Parameters:
  ///   - lowerEndpoint: The given lower endpoint.
  ///   - upperEndpoint: The given upper endpoint.
  /// - Returns: A lower-open, upper-closed, and bounded interval.
  @_transparent
  public static func <~~<= (
    lowerEndpoint: Self,
    upperEndpoint: Self
  ) -> Interval<Self> {
    return Interval(
      from: lowerEndpoint, .exclusive,
      to: upperEndpoint, .inclusive
    )
  }
  
  /// Creates an open and bounded interval from the given endpoints.
  /// - Parameters:
  ///   - lowerEndpoint: The given lower endpoint.
  ///   - upperEndpoint: The given upper endpoint.
  /// - Returns: An open and bounded interval.
  @_transparent
  public static func <~~~< (
    lowerEndpoint: Self,
    upperEndpoint: Self
  ) -> Interval<Self> {
    return Interval(from: lowerEndpoint, to: upperEndpoint, .exclusive)
  }
  
  /// Creates a lower-open and -unbounded and upper-closed and -bounded interval
  /// from the given upper endpoint.
  /// - Parameter upperEndpoint: The given upper endpoint.
  /// - Returns: A lower-open and -unbounded and right-closed and -bounded
  ///   interval.
  @_transparent
  public static prefix func ~~~<= (upperEndpoint: Self) -> Interval<Self> {
    return Interval(fromUnboundedTo: upperEndpoint, .inclusive)
  }
  
  /// Creates a lower-unbounded, upper-bounded, and open interval from the given
  /// upper endpoint.
  /// - Parameter upperEndpoint: The given upper endpoint.
  /// - Returns: A lower-unbounded, upper-bounded, and open interval.
  @_transparent
  public static prefix func ~~~~< (upperEndpoint: Self) -> Interval<Self> {
    return Interval(fromUnboundedTo: upperEndpoint, .exclusive)
  }
  
  /// Creates a lower-closed and -bounded and upper-open and -unbounded interval
  /// from the given lower endpoint.
  /// - Parameter lowerEndpoint: The given lower endpoint.
  /// - Returns: A lower-closed and -bounded and upper-open and -unbounded
  ///   interval.
  @_transparent
  public static postfix func <=~~~ (lowerEndpoint: Self) -> Interval<Self> {
    return Interval(toUnboundedFrom: lowerEndpoint, .inclusive)
  }
  
  /// Creates a lower-bounded, upper-unbounded, and open interval from the given
  /// lower endpoint.
  /// - Parameter lowerEndpoint: The given lower endpoint.
  /// - Returns: A lower-bounded, upper-unbounded, and open interval.
  @_transparent
  public static postfix func <~~~~ (lowerEndpoint: Self) -> Interval<Self> {
    return Interval(toUnboundedFrom: lowerEndpoint, .exclusive)
  }
  
}

  // MARK: Inverse Interval Operators

extension Comparable where Self: Hashable & Strideable {
  
  /// Creates a closed and bounded interval from the given endpoints that's
  /// iterated in reverse.
  /// - Parameters:
  ///   - upperEndpoint: The given upper endpoint.
  ///   - lowerEndpoint: The given lower endpoint.
  /// - Returns: A closed and bounded interval that's iterated in reverse.
  @_transparent
  public static func >=~>= (
    upperEndpoint: Self,
    lowerEndpoint: Self
  ) -> Interval<Self> {
    return Interval(
      from: lowerEndpoint, to: upperEndpoint, .inclusive,
      inInverseStridingDirection: true
    )
  }
  
  /// Creates a lower-closed, upper-open, and bounded interval from the given
  /// endpoints that's iterated in reverse.
  /// - Parameters:
  ///   - upperEndpoint: The given upper endpoint.
  ///   - lowerEndpoint: The given lower endpoint.
  /// - Returns: A lower-closed, upper-open, and bounded interval that's
  ///   iterated in reverse.
  @_transparent
  public static func >~~>= (
    upperEndpoint: Self,
    lowerEndpoint: Self
  ) -> Interval<Self> {
    return Interval(
      from: lowerEndpoint, .inclusive,
      to: upperEndpoint, .exclusive,
      inInverseStridingDirection: true
    )
  }
  
  /// Creates a lower-open, upper-closed, and bounded interval from the given
  /// endpoints that's iterated in reverse.
  /// - Parameters:
  ///   - upperEndpoint: The given upper endpoint.
  ///   - lowerEndpoint: The given lower endpoint.
  /// - Returns: A lower-open, upper-closed, and bounded interval that's
  ///   iterated in reverse.
  @_transparent
  public static func >=~~> (
    upperEndpoint: Self,
    lowerEndpoint: Self
  ) -> Interval<Self> {
    return Interval(
      from: lowerEndpoint, .exclusive,
      to: upperEndpoint, .inclusive,
      inInverseStridingDirection: true
    )
  }
  
  /// Creates an open and bounded interval from the given endpoints that's
  /// iterated in reverse.
  /// - Parameters:
  ///   - upperEndpoint: The given upper endpoint.
  ///   - lowerEndpoint: The given lower endpoint.
  /// - Returns: An open and bounded interval that's iterated in reverse.
  @_transparent
  public static func >~~~> (
    upperEndpoint: Self,
    lowerEndpoint: Self
  ) -> Interval<Self> {
    return Interval(
      from: lowerEndpoint, to: upperEndpoint, .exclusive,
      inInverseStridingDirection: true
    )
  }
  
  /// Creates a lower-open and -unbounded and upper-closed and -bounded interval
  /// from the given upper endpoint that's iterated in reverse.
  /// - Parameter upperEndpoint: The given upper endpoint.
  /// - Returns: A lower-open and -unbounded and right-closed and -bounded
  ///   interval that's iterated in reverse.
  @_transparent
  public static postfix func >=~~~ (upperEndpoint: Self) -> Interval<Self> {
    return Interval(
      fromUnboundedTo: upperEndpoint, .inclusive,
      inInverseStridingDirection: true
    )
  }
  
  /// Creates a lower-unbounded, upper-bounded, and open interval from the given
  /// upper endpoint that's iterated in reverse.
  /// - Parameter upperEndpoint: The given upper endpoint.
  /// - Returns: A lower-unbounded, upper-bounded, and open interval that's
  ///   iterated in reverse.
  @_transparent
  public static postfix func >~~~~ (upperEndpoint: Self) -> Interval<Self> {
    return Interval(
      fromUnboundedTo: upperEndpoint, .exclusive,
      inInverseStridingDirection: true
    )
  }
  
  /// Creates a lower-closed and -bounded and upper-open and -unbounded interval
  /// from the given lower endpoint that's iterated in reverse.
  /// - Parameter lowerEndpoint: The given lower endpoint.
  /// - Returns: A lower-closed and -bounded and upper-open and -unbounded
  ///   interval that's iterated in reverse.
  @_transparent
  public static prefix func ~~~>= (lowerEndpoint: Self) -> Interval<Self> {
    return Interval(
      toUnboundedFrom: lowerEndpoint, .inclusive,
      inInverseStridingDirection: true
    )
  }
  
  /// Creates a lower-bounded, upper-unbounded, and open interval from the given
  /// lower endpoint that's iterated in reverse.
  /// - Parameter lowerEndpoint: The given lower endpoint.
  /// - Returns: A lower-bounded, upper-unbounded, and open interval that's
  ///   iterated in reverse.
  @_transparent
  public static prefix func ~~~~> (lowerEndpoint: Self) -> Interval<Self> {
    return Interval(
      toUnboundedFrom: lowerEndpoint, .exclusive,
      inInverseStridingDirection: true
    )
  }
  
}
