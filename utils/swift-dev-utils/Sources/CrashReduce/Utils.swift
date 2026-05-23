//===--- Utils.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Utils

// I don't plan on forming any Regex with captures and writing
// nonisolated(unsafe) everywhere is annoying.
extension Regex: @unchecked @retroactive Sendable {}
