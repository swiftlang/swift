//===--- ArrayDifferentiation.swift ---------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Structural protocol, used to support extensible
// library-defined automatic derived conformances. This API is not stable 
// and subject to change.
//
// Please see forum discussion for more information about the proposal:
// https://forums.swift.org/t/automatic-requirement-satisfaction-in-plain-swift/37158
//
//===----------------------------------------------------------------------===//

import Swift

/// A type that can be converted to and from its structural representation.
public protocol Structural {
    /// A structural representation for `Self`.
    associatedtype StructuralRepresentation

    /// Creates an instance from the given structural representation.
    init(structuralRepresentation: StructuralRepresentation)

    /// A structural representation of `self`.
    var structuralRepresentation: StructuralRepresentation { get set }
}
