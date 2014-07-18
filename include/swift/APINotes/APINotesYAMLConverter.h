//===--- APINotesYAMLConverter.h - Side Car YAML format reader --*- C++ -*-===//
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
// This file reads sidecar data specified in YAML format.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_API_NOTES_YAML_CONVERTER_H
#define SWIFT_API_NOTES_YAML_CONVERTER_H
#include "llvm/ADT/StringRef.h"

namespace swift {
namespace api_notes {

  /// Converts side car from YAML format into binary format.
  void buildSidecarFromYAML(llvm::StringRef fromFileName,
                            llvm::StringRef toFileName);

} // end namespace api_notes
} // end namespace swift

#endif // LLVM_SWIFT_API_NOTES_YAML_CONVERTER_H
