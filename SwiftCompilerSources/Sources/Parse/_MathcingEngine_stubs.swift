//===--- _MatchingEnginge_stubs.swift -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Only provides stub functions for Package.swift builds which doesn't know
// where the "string-processing" sources exist.
// NOTE: This file is not used in CMake builds.

func libswiftLexRegexLiteral(
  _ curPtrPtr: UnsafeMutablePointer<UnsafePointer<CChar>?>?,
  _ bufferEndPtr: UnsafePointer<CChar>?,
  _ errOut: UnsafeMutablePointer<UnsafePointer<CChar>?>?
) -> /*CompletelyErroneous*/ CBool {
  fatalError("not implemented")
}

func libswiftParseRegexLiteral(
  _ inputPtr: UnsafePointer<CChar>?,
  _ errOut: UnsafeMutablePointer<UnsafePointer<CChar>?>?,
  _ versionOut: UnsafeMutablePointer<CUnsignedInt>?,
  _ captureStructureOut: UnsafeMutableRawPointer?,
  _ captureStructureSize: CUnsignedInt
) {
  fatalError("not implemented")
}
