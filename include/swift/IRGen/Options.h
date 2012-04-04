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

enum class OutputKind : unsigned {
  /// Just generate an LLVM module and return it.
  Module,

  /// Generate an LLVM module and write it out as LLVM assembly.
  LLVMAssembly,

  /// Generate an LLVM module and write it out as LLVM bitcode.
  LLVMBitcode,

  /// Generate an LLVM module and compile it to assembly.
  NativeAssembly,

  /// Generate an LLVM module, compile it, and assemble into an object file.
  ObjectFile
};

/// irgen::Options - The set of options support by IR generation.
class Options {
public:
  std::string OutputFilename;
  std::string Triple;

  /// The kind of compilation we should do.
  OutputKind OutputKind : 3;

  /// Should we spend time verifying that the IR we produce is
  /// well-formed?
  unsigned Verify : 1;

  /// The optimization level, as in -O2.
  unsigned OptLevel : 2;

  Options() : OutputKind(OutputKind::LLVMAssembly), Verify(true), OptLevel(0) {}
};

} // end namespace irgen
} // end namespace swift

#endif
