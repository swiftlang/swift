//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import SwiftConcurrencyInternalShims

@available(SwiftStdlib 5.1, *)
extension Result where Success: ~Copyable {
  /// Creates a new result by evaluating an async throwing closure, capturing the
  /// returned value as a success, or any thrown error as a failure.
  ///
  /// - Parameter body: A potentially throwing async closure to evaluate.
  @export(implementation)
  public nonisolated(nonsending) init(
    catching body: nonisolated(nonsending) () async throws(Failure) -> Success
  ) async {
    // FIXME: this may also require similar changes as the synchronous Result.init(catching:) support non-escapable types.
    do {
      self = .success(try await body())
    } catch {
      self = .failure(error)
    }
  }
}