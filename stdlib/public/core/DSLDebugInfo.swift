//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

/// A type that can be called as a function to trigger breakpoints
/// that are set on the associated result builder component.
@available(SwiftStdlib 5.8, *)
@frozen
public struct DSLDebugInfoProvider {
  @usableFromInline
  let callback: @convention(thin) (Any, () -> Void) -> Void

  /// To be invoked by the compiler only.
  @usableFromInline
  init(_ callback: @convention(thin) (Any, () -> Void) -> Void) {
    self.callback = callback
  }

  /// Trigger breakpoints that are set on the associated source location and
  /// provide debug information with additional stack frames.
  ///
  /// - Parameters:
  ///   - context: The debugging variables to show.
  ///   - stackTrace: The additional stack trace to insert before providing
  ///     the context, where the last stack frame in the collection is the
  ///     top of the additional stack.
  ///   - then: The action to perform after providing the DSL debug context.
  @inlinable
  public func callAsFunction(
    context: Any,
    stackTrace: some Collection<DSLDebugStackFrame>,
    then: () -> Void = {}
  ) {
    if let current = stackTrace.first {
      current.provider(context: current.context) {
        self(context: context, stackTrace: stackTrace.dropFirst(), then: then)
      }
    } else {
      self(context: context, then: then)
    }
  }

  /// Trigger breakpoints that are set on the associated source location and
  /// provide debug information.
  ///
  /// - Parameters:
  ///   - context: The debugging variables to show.
  ///   - then: The action to perform after providing the DSL debug context.
  @inlinable
  public func callAsFunction(context: Any, then: () -> Void = {}) {
    callback(context, then)
  }
}

/// A mechanism for DSLs to specify where a stack frame is for,
/// and the debugging context to show for such stack frame.
@available(SwiftStdlib 5.8, *)
@frozen
public struct DSLDebugStackFrame {
  /// The debug info provider specifying the source location.
  public var provider: DSLDebugInfoProvider
  /// The debugging variables to show.
  public var context: Any

  /// Constructs a DSL debug stack frame.
  ///
  /// - Parameters:
  ///   - provider: The debug info provider specifying the source location
  ///     of this stack frame.
  ///   - context: The debugging variables to show for this stack frame.
  @inlinable
  public init(provider: DSLDebugInfoProvider, context: Any) {
    self.provider = provider
    self.context = context
  }
}
