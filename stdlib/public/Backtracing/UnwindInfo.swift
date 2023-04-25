//===--- UnwindInfo.swift - Holds information about unwind information ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// UnwindInfo is a struct that holds information about the available unwind
// information for a given image.  We support DWARF, with or without the GNU
// eh_frame_hdr index, compact unwind and ARM EHABI unwind information.
//
//===----------------------------------------------------------------------===//

import Swift

internal struct UnwindInfo {
    public typealias Address = UInt64
    public typealias Size = UInt64

    public struct Range {
        public let base: Address
        public let size: Size
    }

    public var dwarfSection: Range?
    public var ehFrameHdrSection: Range?
    public var compactUnwindSection: Range?
    public var armSection: Range?
}
