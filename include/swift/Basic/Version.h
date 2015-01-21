//===- Version.h - Swift Version Number -------------------------*- C++ -*-===//
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
/// \brief Defines version macros and version-related utility functions
/// for Swift.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_VERSION_H
#define SWIFT_BASIC_VERSION_H

#include <string>

namespace swift {
namespace version {

/// Retrieves the numeric {major, minor} Swift version.
std::pair<unsigned, unsigned> getSwiftNumericVersion();

/// Retrieves a string representing the complete Swift version, which includes
/// the Swift version number, the repository version, and the vendor tag.
std::string getSwiftFullVersion();

} // end namespace version
} // end namespace swift

#endif
