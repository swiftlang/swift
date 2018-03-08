//===--- PlaygroundDisplay.swift ------------------------------------------===//
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

/// A type that supplies a custom description for playground logging.
///
/// All types have a default description for playgrounds. This protocol
/// allows types to provide custom descriptions which are then logged in
/// place of the original instance.
///
/// Playground logging can generate, at a minimum, a structured description
/// of any type. Playground logging is also capable of generating a richer,
/// more specialized description of core types -- for instance, the contents
/// of a `String` are logged, as are the components of an `NSColor` or
/// `UIColor`.
///
/// The current playground logging implementation logs specialized
/// descriptions of at least the following types:
///
/// - `String` and `NSString`
/// - `Int` and `UInt` (including the sized variants)
/// - `Float` and `Double`
/// - `Bool`
/// - `Date` and `NSDate`
/// - `NSAttributedString`
/// - `NSNumber`
/// - `NSRange`
/// - `URL` and `NSURL`
/// - `CGPoint`, `CGSize`, and `CGRect`
/// - `NSColor`, `UIColor`, `CGColor`, and `CIColor`
/// - `NSImage`, `UIImage`, `CGImage`, and `CIImage`
/// - `NSBezierPath` and `UIBezierPath`
/// - `NSView` and `UIView`
///
/// Playground logging may also be able to support specialized descriptions
/// of other types.
///
/// Implementors of `CustomPlaygroundDisplayConvertible` may return a value of
/// one of the above types to also receive a specialized log description.
/// Implementors may also return any other type, and playground logging will
/// generated structured logging for the returned value.
///
/// - note: `CustomPlaygroundDisplayConvertible` conformances chain -- that is,
///   if `playgroundDescription` returns an instance which itself conforms to
///   `CustomPlaygroundDisplayConvertible`, then playground logging will ask for
///   that instance's `playgroundDescription` and so on. It is permissible for
///   playground logging implementations to place a reasonable limit on this
///   kind of chaining to prevent infinite loops.
public protocol CustomPlaygroundDisplayConvertible {
  /// Returns the custom playground description for this instance.
  ///
  /// If this type has value semantics, the instance returned should be
  /// unaffected by subsequent mutations if possible.
  var playgroundDescription: Any { get }
}