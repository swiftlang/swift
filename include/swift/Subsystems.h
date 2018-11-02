//===--- Subsystems.h - Swift Compiler Subsystem Entrypoints ----*- C++ -*-===//
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
//  This file declares the main entrypoints to the various subsystems.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SUBSYSTEMS_H
#define SWIFT_SUBSYSTEMS_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/PrimarySpecificPaths.h"
#include "swift/Basic/Version.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Mutex.h"

#include <memory>

namespace llvm {
  class GlobalVariable;
  class MemoryBuffer;
  class Module;
  class TargetOptions;
  class TargetMachine;
}

namespace swift {
  class GenericSignatureBuilder;
  class ASTContext;
  class CodeCompletionCallbacksFactory;
  class Decl;
  class DeclContext;
  class DelayedParsingCallbacks;
  class DiagnosticConsumer;
  class DiagnosticEngine;
  class Evaluator;
  class FileUnit;
  class GenericEnvironment;
  class GenericParamList;
  class IRGenOptions;
  class LangOptions;
  class ModuleDecl;
  class Parser;
  class PersistentParserState;
  class SerializationOptions;
  class SILOptions;
  class SILModule;
  class SILParserTUState;
  class SourceFile;
  class SourceManager;
  class SyntaxParsingCache;
  class Token;
  class TopLevelContext;
  struct TypeLoc;
  class UnifiedStatsReporter;

  /// Used to optionally maintain SIL parsing context for the parser.
  ///
  /// When not parsing SIL, this has no overhead.
  class SILParserState {
  public:
    std::unique_ptr<SILParserTUState> Impl;

    explicit SILParserState(SILModule *M);
    ~SILParserState();
  };

  /// @{

  /// \returns true if the declaration should be verified.  This can return
  /// false to decrease the number of declarations we verify in a single
  /// compilation.
  bool shouldVerify(const Decl *D, const ASTContext &Context);

  /// \brief Check that the source file is well-formed, aborting and spewing
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
  std::vector<Token> tokenize(const LangOptions &LangOpts,
                              const SourceManager &SM, unsigned BufferID,
                              unsigned Offset = 0, unsigned EndOffset = 0,
                              DiagnosticEngine *Diags = nullptr,
                              bool KeepComments = true,
                              bool TokenizeInterpolatedString = true,
                              ArrayRef<Token> SplitTokens = ArrayRef<Token>());

  /// Once parsing is complete, this walks the AST to resolve imports, record
  /// operators, and do other top-level validation.
  ///
  /// \param StartElem Where to start for incremental name binding in the main
  ///                  source file.
  void performNameBinding(SourceFile &SF, unsigned StartElem = 0);

  /// Once type-checking is complete, this instruments code with calls to an
  /// intrinsic that record the expected values of local variables so they can
  /// be compared against the results from the debugger.
  void performDebuggerTestingTransform(SourceFile &SF);

  /// Once parsing and name-binding are complete, this optionally transforms the
  /// ASTs to add calls to external logging functions.
  ///
  /// \param HighPerformance True if the playground transform should omit
  /// instrumentation that has a high runtime performance impact.
  void performPlaygroundTransform(SourceFile &SF, bool HighPerformance);
  
  /// Once parsing and name-binding are complete this optionally walks the ASTs
  /// to add calls to externally provided functions that simulate
  /// "program counter"-like debugging events.
  void performPCMacro(SourceFile &SF, TopLevelContext &TLC);
  
  /// Flags used to control type checking.
  enum class TypeCheckingFlags : unsigned {
    /// Whether to delay checking that benefits from having the entire
    /// module parsed, e.g., Objective-C method override checking.
    DelayWholeModuleChecking = 1 << 0,

    /// If set, dumps wall time taken to check each function body to
    /// llvm::errs().
    DebugTimeFunctionBodies = 1 << 1,

    /// Indicates that the type checker is checking code that will be
    /// immediately executed.
    ForImmediateMode = 1 << 2,

    /// If set, dumps wall time taken to type check each expression to
    /// llvm::errs().
    DebugTimeExpressions = 1 << 3,
  };

  /// Once parsing and name-binding are complete, this walks the AST to resolve
  /// types and diagnose problems therein.
  ///
  /// \param StartElem Where to start for incremental type-checking in the main
  /// source file.
  ///
  /// \param WarnLongFunctionBodies If non-zero, warn when a function body takes
  /// longer than this many milliseconds to type-check
  void performTypeChecking(SourceFile &SF, TopLevelContext &TLC,
                           OptionSet<TypeCheckingFlags> Options,
                           unsigned StartElem = 0,
                           unsigned WarnLongFunctionBodies = 0,
                           unsigned WarnLongExpressionTypeChecking = 0,
                           unsigned ExpressionTimeoutThreshold = 0,
                           unsigned SwitchCheckingInvocationThreshold = 0);

  /// Now that we have type-checked an entire module, perform any type
  /// checking that requires the full module, e.g., Objective-C method
  /// override checking.
  ///
  /// Note that clients still perform this checking file-by-file to
  /// provide a somewhat defined order in which diagnostics should be
  /// emitted.
  void performWholeModuleTypeChecking(SourceFile &SF);

  /// Incrementally type-check only added external definitions.
  void typeCheckExternalDefinitions(SourceFile &SF);

  /// \brief Recursively validate the specified type.
  ///
  /// This is used when dealing with partial source files (e.g. SIL parsing,
  /// code completion).
  ///
  /// \returns false on success, true on error.
  bool performTypeLocChecking(ASTContext &Ctx, TypeLoc &T,
                              DeclContext *DC,
                              bool ProduceDiagnostics = true);

  /// \brief Recursively validate the specified type.
  ///
  /// This is used when dealing with partial source files (e.g. SIL parsing,
  /// code completion).
  ///
  /// \returns false on success, true on error.
  bool performTypeLocChecking(ASTContext &Ctx, TypeLoc &T,
                              bool isSILMode,
                              bool isSILType,
                              GenericEnvironment *GenericEnv,
                              DeclContext *DC,
                              bool ProduceDiagnostics = true);

  /// Expose TypeChecker's handling of GenericParamList to SIL parsing.
  GenericEnvironment *handleSILGenericParams(ASTContext &Ctx,
                                             GenericParamList *genericParams,
                                             DeclContext *DC);

  /// Turn the given module into SIL IR.
  ///
  /// The module must contain source files.
  ///
  /// if \p wholeModuleCompilation is true, the optimizer assumes that the SIL
  /// of all files in the module is present in the SILModule.
  std::unique_ptr<SILModule>
  performSILGeneration(ModuleDecl *M, SILOptions &options,
                       bool wholeModuleCompilation = false);

  /// Turn a source file into SIL IR.
  ///
  /// If \p StartElem is provided, the module is assumed to be only part of the
  /// SourceFile, and any optimizations should take that into account.
  std::unique_ptr<SILModule>
  performSILGeneration(FileUnit &SF, SILOptions &options,
                       Optional<unsigned> StartElem = None);

  using ModuleOrSourceFile = PointerUnion<ModuleDecl *, SourceFile *>;

  /// Serializes a module or single source file to the given output file.
  void serialize(ModuleOrSourceFile DC, const SerializationOptions &options,
                 const SILModule *M = nullptr);

  /// Get the CPU, subtarget feature options, and triple to use when emitting code.
  std::tuple<llvm::TargetOptions, std::string, std::vector<std::string>,
             std::string>
  getIRTargetOptions(IRGenOptions &Opts, ASTContext &Ctx);

  /// Turn the given Swift module into either LLVM IR or native code
  /// and return the generated LLVM IR module.
  /// If you set an outModuleHash, then you need to call performLLVM.
  std::unique_ptr<llvm::Module>
  performIRGeneration(IRGenOptions &Opts, ModuleDecl *M,
                      std::unique_ptr<SILModule> SILMod,
                      StringRef ModuleName, const PrimarySpecificPaths &PSPs,
                      llvm::LLVMContext &LLVMContext,
                      ArrayRef<std::string> parallelOutputFilenames,
                      llvm::GlobalVariable **outModuleHash = nullptr);

  /// Turn the given Swift module into either LLVM IR or native code
  /// and return the generated LLVM IR module.
  /// If you set an outModuleHash, then you need to call performLLVM.
  std::unique_ptr<llvm::Module>
  performIRGeneration(IRGenOptions &Opts, SourceFile &SF,
                      std::unique_ptr<SILModule> SILMod,
                      StringRef ModuleName, const PrimarySpecificPaths &PSPs,
                      llvm::LLVMContext &LLVMContext,
                      unsigned StartElem = 0,
                      llvm::GlobalVariable **outModuleHash = nullptr);

  /// Given an already created LLVM module, construct a pass pipeline and run
  /// the Swift LLVM Pipeline upon it. This does not cause the module to be
  /// printed, only to be optimized.
  void performLLVMOptimizations(IRGenOptions &Opts, llvm::Module *Module,
                                llvm::TargetMachine *TargetMachine);

  /// Wrap a serialized module inside a swift AST section in an object file.
  void createSwiftModuleObjectFile(SILModule &SILMod, StringRef Buffer,
                                   StringRef OutputPath);

  /// Turn the given LLVM module into native code and return true on error.
  bool performLLVM(IRGenOptions &Opts, ASTContext &Ctx, llvm::Module *Module,
                   StringRef OutputFilename,
                   UnifiedStatsReporter *Stats=nullptr);

  /// Run the LLVM passes. In multi-threaded compilation this will be done for
  /// multiple LLVM modules in parallel.
  /// \param Diags may be null if LLVM code gen diagnostics are not required.
  /// \param DiagMutex may also be null if a mutex around \p Diags is not
  ///                  required.
  /// \param HashGlobal used with incremental LLVMCodeGen to know if a module
  ///                   was already compiled, may be null if not desired.
  /// \param Module LLVM module to code gen, required.
  /// \param TargetMachine target of code gen, required.
  /// \param effectiveLanguageVersion version of the language, effectively.
  /// \param OutputFilename Filename for output.
  bool performLLVM(IRGenOptions &Opts, DiagnosticEngine *Diags,
                   llvm::sys::Mutex *DiagMutex,
                   llvm::GlobalVariable *HashGlobal,
                   llvm::Module *Module,
                   llvm::TargetMachine *TargetMachine,
                   const version::Version &effectiveLanguageVersion,
                   StringRef OutputFilename,
                   UnifiedStatsReporter *Stats=nullptr);

  /// Creates a TargetMachine from the IRGen opts and AST Context.
  std::unique_ptr<llvm::TargetMachine>
  createTargetMachine(IRGenOptions &Opts, ASTContext &Ctx);

  /// A convenience wrapper for Parser functionality.
  class ParserUnit {
  public:
    ParserUnit(SourceManager &SM, unsigned BufferID,
               const LangOptions &LangOpts, StringRef ModuleName,
               SyntaxParsingCache *SyntaxCache = nullptr);
    ParserUnit(SourceManager &SM, unsigned BufferID);
    ParserUnit(SourceManager &SM, unsigned BufferID,
               unsigned Offset, unsigned EndOffset);

    ~ParserUnit();

    Parser &getParser();
    SourceFile &getSourceFile();
    DiagnosticEngine &getDiagnosticEngine();
    const LangOptions &getLangOptions() const;

  private:
    struct Implementation;
    Implementation &Impl;
  };

  /// Register AST-level request functions with the evaluator.
  ///
  /// The ASTContext will automatically call these upon construction.
  void registerAccessRequestFunctions(Evaluator &evaluator);

  /// Register Sema-level request functions with the evaluator.
  ///
  /// Clients that form an ASTContext and will perform any semantic queries
  /// using Sema-level logic should call these functions after forming the
  /// ASTContext.
  void registerTypeCheckerRequestFunctions(Evaluator &evaluator);

} // end namespace swift

#endif // SWIFT_SUBSYSTEMS_H
