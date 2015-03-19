//===--- FrontendOptions.h --------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_FRONTEND_FRONTENDOPTIONS_H
#define SWIFT_FRONTEND_FRONTENDOPTIONS_H

#include "swift/AST/Module.h"

#include <string>
#include <vector>

namespace llvm {
  class MemoryBuffer;
}

namespace swift {

class SelectedInput {
public:
  /// The index of the input, in either FrontendOptions::InputFilenames or
  /// FrontendOptions::InputBuffers, depending on this SelectedInput's
  /// InputKind.
  unsigned Index;

  enum class InputKind {
    /// Denotes a file input, in FrontendOptions::InputFilenames
    Filename,

    /// Denotes a buffer input, in FrontendOptions::InputBuffers
    Buffer,
  };

  /// The kind of input which this SelectedInput represents.
  InputKind Kind;

  SelectedInput(unsigned Index, InputKind Kind = InputKind::Filename)
      : Index(Index), Kind(Kind) {}

  /// \returns true if the SelectedInput's Kind is a filename
  bool isFilename() const { return Kind == InputKind::Filename; }

  /// \returns true if the SelectedInput's Kind is a buffer
  bool isBuffer() const { return Kind == InputKind::Buffer; }
};

enum class InputFileKind {
  IFK_None,
  IFK_Swift,
  IFK_Swift_Library,
  IFK_Swift_REPL,
  IFK_SIL,
  IFK_LLVM_IR
};

/// Options for controlling the behavior of the frontend.
class FrontendOptions {
public:
  /// The names of input files to the frontend.
  std::vector<std::string> InputFilenames;

  /// Input buffers which may override the file contents of input files.
  std::vector<llvm::MemoryBuffer *> InputBuffers;

  /// The input for which output should be generated. If not set, output will
  /// be generated for the whole module.
  Optional<SelectedInput> PrimaryInput;

  /// The kind of input on which the frontend should operate.
  InputFileKind InputKind = InputFileKind::IFK_Swift;

  /// The specified output files. If only a single outputfile is generated,
  /// the name of the last specified file is taken.
  std::vector<std::string> OutputFilenames;

  /// An arbitrary module to import and make implicitly visible.
  std::string ImplicitImportModuleName;

  /// An Objective-C header to import and make implicitly visible.
  std::string ImplicitObjCHeaderPath;

  /// The name of the module which the frontend is building.
  std::string ModuleName;

  /// The path to which we should emit a serialized module.
  std::string ModuleOutputPath;

  /// The path to which we should emit a module documentation file.
  std::string ModuleDocOutputPath;

  /// The name of the library to link against when using this module.
  std::string ModuleLinkName;

  /// The path to which we should emit an Objective-C header for the module.
  std::string ObjCHeaderOutputPath;

  /// Path to a file which should contain serialized diagnostics for this
  /// frontend invocation.
  std::string SerializedDiagnosticsPath;

  /// The path to which we should output a Make-style dependencies file.
  std::string DependenciesFilePath;

  /// The path to which we should output a Swift reference dependencies file.
  std::string ReferenceDependenciesFilePath;

  /// Arguments which should be passed in immediate mode.
  std::vector<std::string> ImmediateArgv;

  /// \brief A list of arguments to forward to LLVM's option processing; this
  /// should only be used for debugging and experimental features.
  std::vector<std::string> LLVMArgs;

  enum ActionType {
    NoneAction, ///< No specific action
    Parse, ///< Parse and type-check only
    DumpParse, ///< Parse only and dump AST
    DumpAST, ///< Parse, type-check, and dump AST
    PrintAST, ///< Parse, type-check, and pretty-print AST

    EmitSILGen, ///< Emit raw SIL
    EmitSIL, ///< Emit canonical SIL

    EmitModuleOnly, ///< Emit module only

    EmitSIBGen, ///< Emit serialized AST + raw SIL
    EmitSIB, ///< Emit serialized AST + canonical SIL

    Immediate, ///< Immediate mode
    REPL, ///< REPL mode

    EmitAssembly, ///< Emit assembly
    EmitIR, ///< Emit LLVM IR
    EmitBC, ///< Emit LLVM BC
    EmitObject, ///< Emit object file
  };

  /// Indicates the action the user requested that the frontend perform.
  ActionType RequestedAction = NoneAction;

  /// Indicates that the input(s) should be parsed as the Swift stdlib.
  bool ParseStdlib = false;

  /// If set, emitted module files will always contain options for the
  /// debugger to use.
  bool AlwaysSerializeDebuggingOptions = false;

  /// If set, dumps wall time taken to check each function body to llvm::errs().
  bool DebugTimeFunctionBodies = false;

  /// Indicates whether function body parsing should be delayed
  /// until the end of all files.
  bool DelayedFunctionBodyParsing = false;

  /// Indicates whether or not an import statement can pick up a Swift source
  /// file (as opposed to a module file).
  bool EnableSourceImport = false;

  /// Indicates whether we are compiling for testing.
  ///
  /// \see Module::isTestingEnabled
  bool EnableTesting = false;

  /// Indicates that the frontend should emit "verbose" SIL
  /// (if asked to emit SIL).
  bool EmitVerboseSIL = false;

  /// If set, this module is part of a mixed Objective-C/Swift framework, and
  /// the Objective-C half should implicitly be visible to the Swift sources.
  bool ImportUnderlyingModule = false;

  /// If set, the header provided in ImplicitObjCHeaderPath will be rewritten
  /// by the Clang importer as part of semantic analysis.
  bool SerializeBridgingHeader = false;

  /// Indicates that all generated SIL should be serialized into a module,
  /// not just code considered fragile.
  bool SILSerializeAll = false;

  /// Indicates whether or not the frontend should print statistics upon
  /// termination.
  bool PrintStats = false;

  /// Indicates whether or not the Clang importer should print statistics upon
  /// termination.
  bool PrintClangStats = false;

  /// Indicates whether the playground transformation should be applied.
  bool PlaygroundTransform = false;

  /// Indicates whether standard help should be shown.
  bool PrintHelp = false;

  /// Indicates whether full help (including "hidden" options) should be shown.
  bool PrintHelpHidden = false;

  /// Should we sort SIL functions, vtables, witness tables, and global
  /// variables by name when we print it out. This eases diffing of SIL files.
  bool EmitSortedSIL = false;

  /// An enum with different modes for automatically crashing at defined times.
  enum class DebugCrashMode {
    None, ///< Don't automatically crash.
    AssertAfterParse, ///< Automatically assert after parsing.
    CrashAfterParse, ///< Automatically crash after parsing.
  };

  /// Indicates a debug crash mode for the frontend.
  DebugCrashMode CrashMode = DebugCrashMode::None;

  /// Indicates whether the RequestedAction has output.
  bool actionHasOutput() const;

  /// Indicates whether the RequestedAction will immediately run code.
  bool actionIsImmediate() const;

  void forAllOutputPaths(std::function<void(const std::string &)> fn) const;
  
  /// Gets the name of the specified output filename.
  /// If multiple files are specified, the last one is returned.
  StringRef getSingleOutputFilename() const {
    if (OutputFilenames.size() >= 1)
      return OutputFilenames.back();
    return StringRef();
  }

  /// Sets a single filename as output filename.
  void setSingleOutputFilename(const std::string &FileName) {
    OutputFilenames.clear();
    OutputFilenames.push_back(FileName);
  }
};

}

#endif
