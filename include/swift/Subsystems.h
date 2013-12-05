//===--- Subsystems.h - Swift Compiler Subsystem Entrypoints ----*- C++ -*-===//
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
//  This file declares the main entrypoints to the various subsystems.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SUBSYSTEMS_H
#define SWIFT_SUBSYSTEMS_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

#include <memory>

namespace llvm {
  class MemoryBuffer;
  class Module;
  class FunctionPass;
}

namespace swift {
  class ASTContext;
  class CodeCompletionCallbacksFactory;
  class Decl;
  class DeclContext;
  class DelayedParsingCallbacks;
  class Module;
  class Parser;
  class PersistentParserState;
  class SILModule;
  class SILParserTUState;
  class SourceFile;
  class SourceManager;
  class Token;
  struct TypeLoc;

  namespace irgen {
    class Options;
  }
  
  /// SILParserState - This is a context object used to optionally maintain SIL
  /// parsing context for the parser.
  class SILParserState {
  public:
    SILModule *M;
    SILParserTUState *S;

    explicit SILParserState(SILModule *M);
    ~SILParserState();
  };

  /// @{

  /// \brief Check that the source file is well formed, aborting and spewing
  /// errors if not.
  ///
  /// "Well-formed" here means following the invariants of the AST, not that the
  /// code written by the user makes sense.
  void verify(SourceFile &SF);
  void verify(Decl *D);

  /// @}

  /// \brief Parse a single buffer into the given source file.
  ///
  /// If the source file is the main file, stop parsing after the next
  /// stmt-brace-item with side-effects.
  ///
  /// \param SF the file within the module being parsed.
  ///
  /// \param BufferID the buffer to parse from.
  ///
  /// \param[out] Done set to \c true if end of the buffer was reached.
  ///
  /// \param SIL if non-null, we're parsing a SIL file.
  ///
  /// \param PersistentState if non-null the same PersistentState object can
  /// be used to resume parsing or parse delayed function bodies.
  ///
  /// \param DelayedParseCB if non-null enables delayed parsing for function
  /// bodies.
  ///
  /// \return true if the parser found code with side effects.
  bool parseIntoSourceFile(SourceFile &SF, unsigned BufferID, bool *Done,
                           SILParserState *SIL = nullptr,
                           PersistentParserState *PersistentState = nullptr,
                           DelayedParsingCallbacks *DelayedParseCB = nullptr);

  /// \brief Finish the parsing by going over the nodes that were delayed
  /// during the first parsing pass.
  void performDelayedParsing(DeclContext *DC,
                             PersistentParserState &PersistentState,
                             CodeCompletionCallbacksFactory *Factory);

  /// \brief Lex and return a vector of tokens for the given buffer.
  std::vector<Token> tokenize(SourceManager &SM, unsigned BufferID,
                              unsigned Offset = 0, unsigned EndOffset = 0,
                              bool KeepComments = true,
                              bool TokenizeInterpolatedString = true);

  /// Once parsing is complete, this walks the AST to resolve imports, record
  /// operators, and do other top-level validation.
  ///
  /// \param StartElem Where to start for incremental name binding in the main
  ///                  source file.
  void performNameBinding(SourceFile &SF, unsigned StartElem = 0);
  
  /// Once parsing and name-binding are complete, this walks the AST to resolve
  /// types and diagnose problems therein.
  ///
  /// \param StartElem Where to start for incremental type-checking in the main
  ///                  source file.
  void performTypeChecking(SourceFile &SF, unsigned StartElem = 0);

  /// \brief Recursively validate the specified type.
  ///
  /// This is used when dealing with partial source files (e.g. SIL parsing,
  /// code completion).
  ///
  /// \returns false on success, true on error.
  bool performTypeLocChecking(ASTContext &Ctx, TypeLoc &T,
                              bool isSILType, DeclContext *DC,
                              bool ProduceDiagnostics = true);

  /// Turn the given module into SIL IR.
  ///
  /// The module must contain source files.
  std::unique_ptr<SILModule> performSILGeneration(Module *M);

  /// Turn a source file into SIL IR.
  std::unique_ptr<SILModule> performSILGeneration(SourceFile &SF,
                                                  unsigned StartElem = 0);

  using ModuleOrSourceFile = PointerUnion<Module *, SourceFile *>;

  /// Serializes a module or single source file to the given output file.
  void serialize(ModuleOrSourceFile DC, const SILModule *M,
                 const char *outputPath,
                 ArrayRef<std::string> inputFilenames = {},
                 StringRef moduleLinkName = {});

  /// Serializes a module or single source file to a stream.
  void serializeToStream(ModuleOrSourceFile DC,
                         llvm::raw_ostream &out,
                         const SILModule *M = nullptr,
                         ArrayRef<std::string> inputFilenames = {},
                         StringRef moduleLinkName = {});

  /// Turn the given module into either LLVM IR or native code.
  void performIRGeneration(irgen::Options &Opts, llvm::Module *Module,
                           swift::Module *M, SILModule *SILMod);

  /// Turn the given source file into either LLVM IR or native code.
  void performIRGeneration(irgen::Options &Opts, llvm::Module *Module,
                           SourceFile &SF, SILModule *SILMod,
                           unsigned StartElem = 0);

  // Optimization passes.
  llvm::FunctionPass *createSwiftARCOptPass();
  llvm::FunctionPass *createSwiftARCExpandPass();

  /// The extension for serialized modules.
  static const char * const SERIALIZED_MODULE_EXTENSION = "swiftmodule";
  /// The extension for SIL files.
  static const char * const SIL_EXTENSION = "sil";
} // end namespace swift

#endif
