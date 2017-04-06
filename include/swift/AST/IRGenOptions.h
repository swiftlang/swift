//===--- IRGenOptions.h - Swift Language IR Generation Options --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/Basic/Sanitizers.h"
// FIXME: This include is just for llvm::SanitizerCoverageOptions. We should
// split the header upstream so we don't include so much.
#include "llvm/Transforms/Instrumentation.h"
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
  ASTTypes,   /// Line tables + AST type references.
  DwarfTypes, /// Line tables + AST type references + DWARF types.
  Normal = ASTTypes /// The setting LLDB prefers.
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

  /// The compilation directory for the debug info.
  std::string DebugCompilationDir;

  /// The DWARF version of debug info.
  unsigned DWARFVersion;

  /// The command line string that is to be stored in the DWARF debug info.
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

  /// Which sanitizer is turned on.
  SanitizerKind Sanitize : 2;

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
  
  /// Special codegen for playgrounds.
  unsigned Playground : 1;

  /// Emit runtime calls to check the end of the lifetime of stack promoted
  /// objects.
  unsigned EmitStackPromotionChecks : 1;

  /// The maximum number of bytes used on a stack frame for stack promotion
  /// (includes alloc_stack allocations).
  unsigned StackPromotionSizeLimit = 1024;

  /// Emit code to verify that static and runtime type layout are consistent for
  /// the given type names.
  SmallVector<StringRef, 1> VerifyTypeLayoutNames;

  /// Frameworks that we should not autolink against.
  SmallVector<std::string, 1> DisableAutolinkFrameworks;

  /// Instrument code to generate profiling information.
  unsigned GenerateProfile : 1;

  /// Print the LLVM inline tree at the end of the LLVM pass pipeline.
  unsigned PrintInlineTree : 1;

  /// Whether we should embed the bitcode file.
  IRGenEmbedMode EmbedMode : 2;

  /// Add names to LLVM values.
  unsigned HasValueNamesSetting : 1;
  unsigned ValueNames : 1;

  /// Emit nominal type field metadata.
  unsigned EnableReflectionMetadata : 1;

  /// Emit names of struct stored properties and enum cases.
  unsigned EnableReflectionNames : 1;

  /// Should we try to build incrementally by not emitting an object file if it
  /// has the same IR hash as the module that we are preparing to emit?
  ///
  /// This is a debugging option meant to make it easier to perform compile time
  /// measurements on a non-clean build directory.
  unsigned UseIncrementalLLVMCodeGen : 1;

  /// Enable use of the swiftcall calling convention.
  unsigned UseSwiftCall : 1;

  /// List of backend command-line options for -embed-bitcode.
  std::vector<uint8_t> CmdArgs;

  /// Which sanitizer coverage is turned on.
  llvm::SanitizerCoverageOptions SanitizeCoverage;

  IRGenOptions()
      : DWARFVersion(2), OutputKind(IRGenOutputKind::LLVMAssembly),
        Verify(true), Optimize(false), Sanitize(SanitizerKind::None),
        DebugInfoKind(IRGenDebugInfoKind::None), UseJIT(false),
        DisableLLVMOptzns(false), DisableLLVMARCOpts(false),
        DisableLLVMSLPVectorizer(false), DisableFPElim(true), Playground(false),
        EmitStackPromotionChecks(false), GenerateProfile(false),
        PrintInlineTree(false), EmbedMode(IRGenEmbedMode::None),
        HasValueNamesSetting(false), ValueNames(false),
        EnableReflectionMetadata(true), EnableReflectionNames(true),
        UseIncrementalLLVMCodeGen(true), UseSwiftCall(false), CmdArgs(),
        SanitizeCoverage(llvm::SanitizerCoverageOptions()) {}

  /// Gets the name of the specified output filename.
  /// If multiple files are specified, the last one is returned.
  StringRef getSingleOutputFilename() const {
    if (OutputFilenames.size() >= 1)
      return OutputFilenames.back();
    return StringRef();
  }

  // Get a hash of all options which influence the llvm compilation but are not
  // reflected in the llvm module itself.
  unsigned getLLVMCodeGenOptionsHash() {
    unsigned Hash = 0;
    Hash = (Hash << 1) | Optimize;
    Hash = (Hash << 1) | DisableLLVMOptzns;
    Hash = (Hash << 1) | DisableLLVMARCOpts;
    return Hash;
  }

  /// Should LLVM IR value names be emitted and preserved?
  bool shouldProvideValueNames() const {
    // If the command line contains an explicit request about whether to add
    // LLVM value names, honor it.  Otherwise, add value names only if the
    // final result is textual LLVM assembly.
    if (HasValueNamesSetting) {
      return ValueNames;
    } else {
      return OutputKind == IRGenOutputKind::LLVMAssembly;
    }
  }

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    return llvm::hash_value(0);
  }
};

} // end namespace swift

#endif
