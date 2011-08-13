//===--- Options.h - Swift Language IR Generation Options -------*- C++ -*-===//
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
// This file defines the options which control the generation of IR for
// swift files.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_OPTIONS_H
#define SWIFT_IRGEN_OPTIONS_H

#include <string>

namespace swift {
namespace irgen {

/// irgen::Options - The set of options support by IR generation.
class Options {
public:
  std::string OutputFilename;
};

} // end namespace irgen
} // end namespace swift

#endif
