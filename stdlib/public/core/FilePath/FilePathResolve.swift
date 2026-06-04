/*
 This source file is part of the SE-0529 reference implementation

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
*/

extension FilePath {
  /// Resolve this path against the filesystem, producing an absolute
  /// path with all symbolic links and `.`/`..` components resolved.
  ///
  /// All intermediate components must exist. Throws if the path
  /// cannot be resolved.
  ///
  /// This operation is synchronous and may block.
  @available(*, noasync)
  @available(SwiftStdlib 9999, *)
  public func resolve() throws -> FilePath {
    preconditionFailure("resolve() is not yet implemented in the reference implementation")
  }
}
