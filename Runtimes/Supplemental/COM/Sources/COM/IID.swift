//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A phantom type tag used to distinguish Interface Identifiers (IIDs) from
/// other GUIDs.
///
/// `IIDTag` serves as a compile-time marker to create a distinct type for IIDs
/// through generic specialization. It carries no runtime value but enables the
/// type system to differentiate between IIDs and general-purpose GUIDs.
public enum IIDTag {}

/// A globally unique identifier that specifically represents a COM Interface
/// Identifier (IID).
///
/// An `IID` is a specialized `GUID` that uniquely identifies a COM interface.
/// Each COM interface has a corresponding IID that is used for runtime
/// identification and interface querying.
///
/// IIDs for Swift-specific interfaces can be derived deterministically using
/// the Swift UUID namespace (see the file-level documentation for details on
/// GUID generation).
///
/// ## Relationship to GUID
///
/// `IID` is a type-safe wrapper around `GUID` that prevents accidental mixing
/// of interface identifiers with other types of GUIDs at compile time.
public typealias IID = GUID<IIDTag>
