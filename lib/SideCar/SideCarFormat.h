//===--- SideCarFormat.h - The internals of side car files ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Contains various constants and helper types to deal with side car
/// files.
///
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SIDE_CAR_FORMAT_H
#define SWIFT_SIDE_CAR_FORMAT_H

#include "swift/Serialization/BCRecordLayout.h" // FIXME: layering

namespace swift {
namespace side_car {

using namespace swift::serialization;

/// Magic number for side car files.
const unsigned char SIDE_CAR_SIGNATURE[] = { 0xE2, 0x9C, 0xA8, 0x01 };

/// Side car file major version number.
///
const uint16_t VERSION_MAJOR = 0;

/// Side car file minor version number.
///
/// When the format changes IN ANY WAY, this number should be incremented.
const uint16_t VERSION_MINOR = 0;

using IdentifierID = Fixnum<31>;
using IdentifierIDField = BCVBR<16>;

using ClassID = Fixnum<31>;
using ClassIDField = BCVBR<31>;

using ModuleID = Fixnum<15>;
using ModuleIDField = BCVBR<8>;

using SelectorID = Fixnum<31>;
using SelectorIDField = BCVBR<16>;

} // end namespace side_car
} // end namespace swift

#endif // LLVM_SWIFT_SIDE_CAR_FORMAT_H
