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
#include "swift/Basic/PathRemapper.h"
#include "swift/Basic/Sanitizers.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/OptimizationMode.h"
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

enum class IRGenDebugInfoLevel : unsigned {
  None,       ///< No debug info.
  LineTables, ///< Line tables only.
  ASTTypes,   ///< Line tables + AST type references.
  DwarfTypes, ///< Line tables + AST type references + DWARF types.
  Normal = ASTTypes ///< The setting LLDB prefers.
};

enum class IRGenDebugInfoFormat : unsigned {
  None,
  DWARF,
  CodeView
};

enum class IRGenEmbedMode : unsigned {
  None,
  EmbedMarker,
  EmbedBitcode
};

/// The set of options supported by IR generation.
class IRGenOptions {
public:
  std::string ModuleName;

  /// The compilation directory for the debug info.
  std::string DebugCompilationDir;

  /// The DWARF version of debug info.
  unsigned DWARFVersion;

  /// The command line string that is to be stored in the debug info.
  std::string DebugFlags;

  /// List of -Xcc -D macro definitions.
  std::vector<std::string> ClangDefines;

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

  OptimizationMode OptMode;

  /// Which sanitizer is turned on.
  OptionSet<SanitizerKind> Sanitizers;

  /// What level of debug info to generate.
  IRGenDebugInfoLevel DebugInfoLevel : 2;

  /// What type of debug info to generate.
  IRGenDebugInfoFormat DebugInfoFormat : 2;

  /// Path prefixes that should be rewritten in debug info.
  PathRemapper DebugPrefixMap;

  /// \brief Whether we're generating IR for the JIT.
  unsigned UseJIT : 1;
  
  /// \brief Whether we're generating code for the integrated REPL.
  unsigned IntegratedREPL : 1;
  
  /// \brief Whether we should run LLVM optimizations after IRGen.
  unsigned DisableLLVMOptzns : 1;

  /// Whether we should run swift specific LLVM optimizations after IRGen.
  unsigned DisableSwiftSpecificLLVMOptzns : 1;

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

  /// Enables resilient class layout.
  unsigned EnableClassResilience : 1;

  /// Bypass resilience when accessing resilient frameworks.
  unsigned EnableResilienceBypass : 1;

  /// The path to load legacy type layouts from.
  StringRef ReadTypeInfoPath;

  /// Should we try to build incrementally by not emitting an object file if it
  /// has the same IR hash as the module that we are preparing to emit?
  ///
  /// This is a debugging option meant to make it easier to perform compile time
  /// measurements on a non-clean build directory.
  unsigned UseIncrementalLLVMCodeGen : 1;

  /// Enable use of the swiftcall calling convention.
  unsigned UseSwiftCall : 1;

  /// Instrument code to generate profiling information.
  unsigned GenerateProfile : 1;

  /// Path to the profdata file to be used for PGO, or the empty string.
  std::string UseProfile = "";

  /// List of backend command-line options for -embed-bitcode.
  std::vector<uint8_t> CmdArgs;

  /// Which sanitizer coverage is turned on.
  llvm::SanitizerCoverageOptions SanitizeCoverage;

  /// The different modes for dumping IRGen type info.
  enum class TypeInfoDumpFilter {
    All,
    Resilient,
    Fragile
  };

  TypeInfoDumpFilter TypeInfoFilter;

  IRGenOptions()
      : DWARFVersion(2), OutputKind(IRGenOutputKind::LLVMAssembly),
        Verify(true), OptMode(OptimizationMode::NotSet),
        Sanitizers(OptionSet<SanitizerKind>()),
        DebugInfoLevel(IRGenDebugInfoLevel::None),
        DebugInfoFormat(IRGenDebugInfoFormat::None),
        UseJIT(false), IntegratedREPL(false),
        DisableLLVMOptzns(false), DisableSwiftSpecificLLVMOptzns(false),
        DisableLLVMSLPVectorizer(false), DisableFPElim(true), Playground(false),
        EmitStackPromotionChecks(false), PrintInlineTree(false),
        EmbedMode(IRGenEmbedMode::None), HasValueNamesSetting(false),
        ValueNames(false), EnableReflectionMetadata(true),
        EnableReflectionNames(true), EnableClassResilience(false),
        EnableResilienceBypass(false), UseIncrementalLLVMCodeGen(true),
        UseSwiftCall(false), GenerateProfile(false), CmdArgs(),
        SanitizeCoverage(llvm::SanitizerCoverageOptions()),
        TypeInfoFilter(TypeInfoDumpFilter::All) {}

  // Get a hash of all options which influence the llvm compilation but are not
  // reflected in the llvm module itself.
  unsigned getLLVMCodeGenOptionsHash() {
    unsigned Hash = (unsigned)OptMode;
    Hash = (Hash << 1) | DisableLLVMOptzns;
    Hash = (Hash << 1) | DisableSwiftSpecificLLVMOptzns;
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

  bool shouldOptimize() const {
    return OptMode > OptimizationMode::NoOptimization;
  }

  bool optimizeForSize() const {
    return OptMode == OptimizationMode::ForSize;
  }

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    return llvm::hash_value(0);
  }
};

} // end namespace swift

#endif
