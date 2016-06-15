//===--- Dwarf.h - DWARF constants ------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines several temporary Swift-specific DWARF constants.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DWARF_H
#define SWIFT_BASIC_DWARF_H

#include "llvm/Support/Dwarf.h"

namespace swift {
  /// The DWARF version emitted by the Swift compiler.
  const unsigned GenericDWARFVersion = 3;
  const unsigned CygwinDWARFVersion = 4;

  static const char MachOASTSegmentName[] = "__SWIFT";
  static const char MachOASTSectionName[] = "__ast";
  static const char ELFASTSectionName[] = ".swift_ast";
  static const char COFFASTSectionName[] = "swiftast";
} // end namespace swift

#endif // SWIFT_BASIC_DWARF_H
