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

/// A phantom type tag used to distinguish Class Identifiers (CLSIDs) from other
/// GUIDs.
///
/// `CLSIDTag` serves as a compile-time marker to create a distinct type for
/// CLSIDs through generic specialization.  It carries no runtime value but
/// enables the type system to differentiate between CLSIDs and general-purpose
/// GUIDs.
public enum CLSIDTag {}

/// A globally unique identifier that specifically represents a COM Class
/// Identifier (CLSID).
///
/// A `CLSID` is a specialized `GUID` that uniquely identifies a COM class or
/// component. CLSIDs are used for object instantiation, registration, and
/// runtime identification of concrete COM implementations.
///
/// CLSIDs for Swift-specific classes can be derived deterministically using the
/// Swift UUID namespace (see the file-level documentation in ISwiftObject.swift
/// for details on GUID generation).
///
/// ## Relationship to GUID
///
/// `CLSID` is a type-safe wrapper around `GUID` that prevents accidental mixing
/// of class identifiers with other types of GUIDs at compile time.
public typealias CLSID = GUID<CLSIDTag>
