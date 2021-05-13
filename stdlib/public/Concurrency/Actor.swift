//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import _SwiftConcurrencyShims

/// Common protocol to which all actors conform.
///
/// The \c Actor protocol generalizes over all actor types. Actor types
/// implicitly conform to this protocol.
@available(SwiftStdlib 5.5, *)
public protocol Actor: AnyObject, Sendable {
}

/// Called to initialize the default actor instance in an actor.
/// The implementation will call this within the actor's initializer.
@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_defaultActor_initialize")
public func _defaultActorInitialize(_ actor: AnyObject)

/// Called to destroy the default actor instance in an actor.
/// The implementation will call this within the actor's deinit.
@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_defaultActor_destroy")
public func _defaultActorDestroy(_ actor: AnyObject)

/// FIXME: only exists for the quick-and-dirty MainActor implementation.
@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_MainActor_register")
fileprivate func _registerMainActor(actor: AnyObject)

/// A singleton actor whose executor is equivalent to 
/// \c DispatchQueue.main, which is the main dispatch queue.
@available(SwiftStdlib 5.5, *)
@globalActor public actor MainActor {
  public static let shared = MainActor()
  
  init() {
    _registerMainActor(actor: self)
  }
}

@available(SwiftStdlib 5.5, *)
extension MainActor {
  /// Execute the given body closure on the main actor.
  public static func run<T>(
    resultType: T.Type = T.self,
    body: @MainActor @Sendable () throws -> T
  ) async rethrows -> T {
    @MainActor func runOnMain(body: @MainActor @Sendable () throws -> T) async rethrows -> T {
      return try body()
    }

    return try await runOnMain(body: body)
  }
}
