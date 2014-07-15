//===--- SideCarReader.h - Side Car Reader ----------------------*- C++ -*-===//
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
//
// This file defines the \c SideCarReader class that reads source
// side-car data providing additional information about source code as
// a separate input, such as the non-nil/nilable annotations for
// method parameters.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SIDE_CAR_READER_H
#define SWIFT_SIDE_CAR_READER_H
namespace swift {
namespace side_car {

/// A class that reads side-car data from a binary file that was written by
/// the \c SideCarWriter.
class SideCarReader {
};

} // end namespace side_car
} // end namespace swift

#endif // LLVM_SWIFT_SIDE_CAR_READER_H
