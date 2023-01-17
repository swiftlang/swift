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

import Swift

@available(SwiftStdlib 5.9, *)
@frozen
public struct ExtendedExistentialShape: PublicLayout {
  public typealias Layout = (
    flags: Flags,
    existentialType: RelativeDirectPointer<CChar>,
    requirementSignatureHeader: GenericSignature.Header
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension ExtendedExistentialShape {
  @inlinable
  public var flags: Flags {
    layout.flags
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension ExtendedExistentialShape: Equatable {
  @inlinable
  public static func ==(
    lhs: ExtendedExistentialShape,
    rhs: ExtendedExistentialShape
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension ExtendedExistentialShape: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
