//===-- Immediate.h - Entry point for swift immediate mode ----------------===//
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
// TranslationUnit, and runs it immediately using the JIT.
//
//===----------------------------------------------------------------------===//

#include <string>

namespace swift {
  class ASTContext;
  class TranslationUnit;
  class SILModule;
  
  namespace irgen {
    class Options;
  }
  
  /// Publicly available REPL state information.
  class REPLContext {
  public:
    /// The index of the next response metavariable to bind to a REPL result.
    unsigned NextResponseVariableIndex;
    
    /// The SourceMgr buffer ID of the REPL input.
    unsigned BufferID;
    
    /// The index into the TranslationUnit's Decls at which to start
    /// typechecking the next REPL input.
    unsigned CurTUElem;

    /// The index into the TranslationUnit's Decls at which to start
    /// irgenning the next REPL input.
    unsigned CurIRGenElem;

    /// \brief Whether we have run replApplicationMain().
    bool RanREPLApplicationMain;
  };

  void RunImmediately(irgen::Options &Options,
                      TranslationUnit *TU, SILModule *SILMod = nullptr);
  void REPL(ASTContext &Context);
  void REPLRunLoop(ASTContext &Context);
}
