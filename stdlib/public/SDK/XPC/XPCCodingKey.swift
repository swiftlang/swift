// stdlib/public/SDK/XPC/XPCCodingKey.swift - CodingKey Implementation for XPC
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// -----------------------------------------------------------------------------
///
/// This file degines a coding key specific for supporting XPC. This mainly used
/// for the super key.
///
// -----------------------------------------------------------------------------


public struct XPCCodingKey: CodingKey {
    public var stringValue: String

    public init?(stringValue: String) {
        self.stringValue = stringValue
    }

    public var intValue: Int?

    public init?(intValue: Int) {
        self.intValue = intValue
        self.stringValue = String(intValue)
    }

    public init(intValue: Int, stringValue: String) {
        self.intValue = intValue
        self.stringValue = stringValue
    }

    static let superKey = XPCCodingKey(intValue: 0, stringValue: "super")
}
