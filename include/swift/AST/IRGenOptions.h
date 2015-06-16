//===--- IRGenOptions.h - Swift Language IR Generation Options --*- C++ -*-===//
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

#ifndef SWIFT_AST_IRGENOPTIONS_H
#define SWIFT_AST_IRGENOPTIONS_H

#include "swift/AST/LinkLibrary.h"
#include <string>
#include <vector>

namespace swift {

enum class IRGenOutputKind : unsigned {
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

enum class IRGenDebugInfoKind : unsigned {
  None,       /// No debug info.
  LineTables, /// Line tables only.
  Normal      /// Line tables + DWARF types.
};

enum class IRGenEmbedMode : unsigned {
  None,
  EmbedMarker,
  EmbedBitcode
};

/// The set of options supported by IR generation.
class IRGenOptions {
public:
  /// The name of the first input file, used by the debug info.
  std::string MainInputFilename;
  std::vector<std::string> OutputFilenames;
  std::string ModuleName;
  // The command line string that is to be stored in the DWARF debug info.
  std::string DWARFDebugFlags;

  /// The libraries and frameworks specified on the command line.
  SmallVector<LinkLibrary, 4> LinkLibraries;

  /// If non-empty, the (unmangled) name of a dummy symbol to emit that can be
  /// used to force-load this module.
  std::string ForceLoadSymbolName;

  /// The kind of compilation we should do.
  IRGenOutputKind OutputKind : 3;

  /// Should we spend time verifying that the IR we produce is
  /// well-formed?
  unsigned Verify : 1;

  /// Whether or not to run optimization passes.
  unsigned Optimize : 1;

  /// Whether we should emit debug info.
  IRGenDebugInfoKind DebugInfoKind : 2;

  /// \brief Whether we're generating IR for the JIT.
  unsigned UseJIT : 1;
  
  /// \brief Whether we should run LLVM optimizations after IRGen.
  unsigned DisableLLVMOptzns : 1;

  /// \brief Whether we should run LLVM ARC optimizations after IRGen.
  unsigned DisableLLVMARCOpts : 1;

  /// \brief Whether we should run LLVM SLP vectorizer.
  unsigned DisableLLVMSLPVectorizer : 1;

  /// Disable frame pointer elimination?
  unsigned DisableFPElim : 1;

  /// Whether or not this is the Swift half of a mixed-source framework.
  unsigned HasUnderlyingModule : 1;
  
  /// Special codegen for playgrounds.
  unsigned Playground : 1;
  
  /// Emit code to verify that static and runtime type layout are consistent for
  /// the given type names.
  SmallVector<StringRef, 1> VerifyTypeLayoutNames;

  /// Frameworks that we should not autolink against.
  SmallVector<std::string, 1> DisableAutolinkFrameworks;

  /// Instrument code to generate profiling information.
  unsigned GenerateProfile : 1;

  /// Whether we should embed the bitcode file.
  IRGenEmbedMode EmbedMode : 2;

  /// List of backend command-line options for -embed-bitcode.
  std::vector<uint8_t> CmdArgs;

  IRGenOptions() : OutputKind(IRGenOutputKind::LLVMAssembly), Verify(true),
                   Optimize(false), DebugInfoKind(IRGenDebugInfoKind::None),
                   UseJIT(false), DisableLLVMOptzns(false),
                   DisableLLVMARCOpts(false), DisableLLVMSLPVectorizer(false),
                   DisableFPElim(true), HasUnderlyingModule(false),
                   Playground(false), GenerateProfile(false),
                   EmbedMode(IRGenEmbedMode::None) {}
  
  /// Gets the name of the specified output filename.
  /// If multiple files are specified, the last one is returned.
  StringRef getSingleOutputFilename() const {
    if (OutputFilenames.size() >= 1)
      return OutputFilenames.back();
    return StringRef();
  }
};

} // end namespace swift

#endif
