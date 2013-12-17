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

#include <string>
#include <vector>

namespace swift {

/// Options for controlling the behavior of the frontend.
class FrontendOptions {
public:
  /// The names of input files to the frontend.
  std::vector<std::string> InputFilenames;

  /// The name of the primary output file which should be created
  /// by the frontend.
  std::string OutputFilename;

  /// The name of the module which the frontend is building.
  std::string ModuleName;

  enum ActionType {
    Parse, ///< Parse only
    DumpParse, ///< Parse and dump AST
    DumpAST, ///< Parse, type-check, and dump AST
    PrintAST, ///< Parse, type-check, and pretty-print AST

    EmitSILGen, ///< Emit raw SIL
    EmitSIL, ///< Emit canonical SIL

    EmitModuleOnly, ///< Emit module only

    Immediate, ///< Immediate mode
    REPL, ///< REPL mode

    EmitAssembly, ///< Emit assembly
    EmitIR, ///< Emit LLVM IR
    EmitBC, ///< Emit LLVM BC
    EmitObject, ///< Emit object file
  };

  /// Indicates the action the user requested that the frontend perform.
  ActionType RequestedAction = Parse;

  /// Indicates whether function body parsing should be delayed
  /// until the end of all files.
  bool DelayedFunctionBodyParsing = false;

  /// Indicates that the frontend should emit "verbose" SIL
  /// (if asked to emit SIL).
  bool EmitVerboseSIL = false;

  /// Path to a "module source list", which lists all files in the module.
  /// Currently unused.
  std::string ModuleSourceListPath;

  /// Path to a file which should contain serialized diagnostics for this
  /// frontend invocation.
  std::string SerializedDiagnosticsPath;

  /// Arguments which should be passed in immediate mode.
  std::vector<std::string> ImmediateArgv;

  /// Indicates whether or not the frontend should print statistics upon
  /// termination.
  bool PrintStats = false;

  /// Indicates whether standard help should be shown.
  bool PrintHelp = false;

  /// Indicates whether full help (including "hidden" options) should be shown.
  bool PrintHelpHidden = false;
};

}

#endif
