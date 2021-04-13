////===----------------------------------------------------------------------===//
////
//// This source file is part of the Swift.org open source project
////
//// Copyright (c) 2021 Apple Inc. and the Swift project authors
//// Licensed under Apache License v2.0 with Runtime Library Exception
////
//// See https://swift.org/LICENSE.txt for license information
//// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
////
////===----------------------------------------------------------------------===//

/// The Sendable protocol indicates that value of the given type can
/// be safely used in concurrent code.
@_marker public protocol Sendable { }

/// The UnsafeSendable protocol indicates that value of the given type
/// can be safely used in concurrent code, but disables some safety checking
/// at the conformance site.
@available(*, deprecated, message: "Use @unchecked Sendable instead")
@_marker public protocol UnsafeSendable: Sendable { }

// Historical names
@available(*, deprecated, renamed: "Sendable")
public typealias ConcurrentValue = Sendable

@available(*, deprecated, renamed: "Sendable")
public typealias UnsafeConcurrentValue = UnsafeSendable
