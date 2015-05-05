//===-- Immediate.h - Entry point for swift immediate mode -----*- C++ -*--===//
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
// This is the entry point to the swift immediate mode, which takes a
// source file, and runs it immediately using the JIT.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IMMEDIATE_H
#define SWIFT_IMMEDIATE_H

#include <string>
#include <vector>

namespace swift {
  class CompilerInstance;
  class SILModule;
  class IRGenOptions;
  class SILOptions;

  // Using LLVM containers to store command-line arguments turns out
  // to be a lose, because LLVM's execution engine demands this vector
  // type.  We can flip the typedef if/when the LLVM interface
  // supports LLVM containers.
  using ProcessCmdLine = std::vector<std::string>;
  
  /// Publicly available REPL state information.
  class REPLContext {
  public:
    /// The SourceMgr buffer ID of the REPL input.
    unsigned CurBufferID;
    
    /// The index into the source file's Decls at which to start
    /// typechecking the next REPL input.
    unsigned CurElem;

    /// The index into the source file's Decls at which to start
    /// irgenning the next REPL input.
    unsigned CurIRGenElem;

    /// \brief Whether we have run replApplicationMain().
    bool RanREPLApplicationMain;
  };

  /// Attempt to run the script identified by the given compiler instance.
  ///
  /// \return the result returned from main(), if execution succeeded
  int RunImmediately(CompilerInstance &CI, const ProcessCmdLine &CmdLine,
                     IRGenOptions &IRGenOpts, const SILOptions &SILOpts);

  void REPL(CompilerInstance &CI, const ProcessCmdLine &CmdLine, bool ParseStdlib);
  void REPLRunLoop(CompilerInstance &CI, const ProcessCmdLine &CmdLine, bool ParseStdlib);
}  // end namespace swift

#endif

