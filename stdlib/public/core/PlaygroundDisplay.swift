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
/// Playground logging can generate, at a minimum, a structured description
/// of any type. If you want to provide a custom description of your type to be
/// logged in place of the default description, conform to the
/// `CustomPlaygroundDisplayConvertible` protocol.
///
/// Playground logging generates a richer, more specialized description of core
/// types. For example, the contents of a `String` are logged, as are the
/// components of an `NSColor` or `UIColor`. The current playground logging
/// implementation logs specialized descriptions of at least the following
/// types:
///
/// - `String` and `NSString`
/// - `Int`, `UInt`, and the other standard library integer types
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
/// Conforming to the CustomPlaygroundDisplayConvertible Protocol
/// -------------------------------------------------------------
///
/// To add `CustomPlaygroundDisplayConvertible` conformance to your custom type,
/// implement the `playgroundDescription` property. If your implementation
/// returns an instance of one of the types above, that type's specialized
/// description is used. If you return any other type, a structured description
/// is generated.
///
/// If your type has value semantics, the `playgroundDescription` should be
/// unaffected by subsequent mutations, if possible.
///
/// If your type's `playgroundDescription` returns an instance which itself
/// conforms to `CustomPlaygroundDisplayConvertible`, then that type's
/// `playgroundDescription` will be used, and so on. To prevent infinite loops,
/// playground logging implementations can place a reasonable limit on this
/// kind of chaining.
public protocol CustomPlaygroundDisplayConvertible {
  /// A custom playground description for this instance.
  var playgroundDescription: Any { get }
}
