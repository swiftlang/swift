//=== APINotesYAMLCompiler.h - API Notes YAML to binary compiler *- C++ -*-===//
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
// This file reads sidecar API notes specified in YAML format.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_API_NOTES_YAML_COMPILER_H
#define SWIFT_API_NOTES_YAML_COMPILER_H
#include "llvm/ADT/StringRef.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
namespace api_notes {

  /// Converts API notes from YAML format to binary format.
  void compileAPINotes(llvm::StringRef fromFileName, llvm::raw_ostream &os);

} // end namespace api_notes
} // end namespace swift

#endif // LLVM_SWIFT_API_NOTES_YAML_COMPILER_H
