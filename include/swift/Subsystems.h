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
#include "llvm/ADT/StringSet.h"
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
  class DiagnosticConsumer;
  class DiagnosticEngine;
  class Evaluator;
  class FileUnit;
  class GenericEnvironment;
  class GenericParamList;
  class IRGenOptions;
  class LangOptions;
  class ModuleDecl;
  typedef void *OpaqueSyntaxNode;
  class Parser;
  class PersistentParserState;
  class SerializationOptions;
  class SILOptions;
  class SILModule;
  class SILParserTUState;
  class SourceFile;
  class SourceManager;
  class SyntaxParseActions;
  class SyntaxParsingCache;
  class Token;
  class TopLevelContext;
  class TypeChecker;
  class TypeCheckerOptions;
  struct TypeLoc;
  class UnifiedStatsReporter;
  enum class SourceFileKind;

  namespace Lowering {
    class TypeConverter;
  }

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

  /// Check that the source file is well-formed, aborting and spewing
  /// errors if not.
  ///
  /// "Well-formed" here means following the invariants of the AST, not that the
  /// code written by the user makes sense.
  void verify(SourceFile &SF);
  void verify(Decl *D);

  /// @}

  /// Parse a single buffer into the given source file.
  ///
  /// \param SF The file within the module being parsed.
  ///
  /// \param BufferID The buffer to parse from.
  ///
  /// \param PersistentState If non-null the same PersistentState object can be
  /// used to save parser state for code completion.
  ///
  /// \param DelayBodyParsing Whether parsing of type and function bodies can be
  /// delayed.
  void parseIntoSourceFile(SourceFile &SF, unsigned BufferID,
                           PersistentParserState *PersistentState = nullptr,
                           bool DelayBodyParsing = true);

  /// Parse a source file's SIL declarations into a given SIL module.
  void parseSourceFileSIL(SourceFile &SF, SILParserState *sil);

  /// Finish the code completion.
  void performCodeCompletionSecondPass(PersistentParserState &PersistentState,
                                       CodeCompletionCallbacksFactory &Factory);

  /// Lex and return a vector of tokens for the given buffer.
  std::vector<Token> tokenize(const LangOptions &LangOpts,
                              const SourceManager &SM, unsigned BufferID,
                              unsigned Offset = 0, unsigned EndOffset = 0,
                              DiagnosticEngine *Diags = nullptr,
                              bool KeepComments = true,
                              bool TokenizeInterpolatedString = true,
                              ArrayRef<Token> SplitTokens = ArrayRef<Token>());

  /// Once parsing is complete, this walks the AST to resolve imports, record
  /// operators, and do other top-level validation.
  void performNameBinding(SourceFile &SF);

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
  /// "program counter"-like debugging events. See the comment at the top of
  /// lib/Sema/PCMacro.cpp for a description of the calls inserted.
  void performPCMacro(SourceFile &SF);

  /// Creates a type checker instance on the given AST context, if it
  /// doesn't already have one.
  ///
  /// \returns a reference to the type checker instance.
  TypeChecker &createTypeChecker(ASTContext &Ctx);

  /// Bind all 'extension' visible from \p SF to the extended nominal.
  void bindExtensions(SourceFile &SF);

  /// Once parsing and name-binding are complete, this walks the AST to resolve
  /// types and diagnose problems therein.
  void performTypeChecking(SourceFile &SF);

  /// Now that we have type-checked an entire module, perform any type
  /// checking that requires the full module, e.g., Objective-C method
  /// override checking.
  ///
  /// Note that clients still perform this checking file-by-file to
  /// provide a somewhat defined order in which diagnostics should be
  /// emitted.
  void performWholeModuleTypeChecking(SourceFile &SF);

  /// Checks to see if any of the imports in \p M use `@_implementationOnly` in
  /// one file and not in another.
  ///
  /// Like redeclaration checking, but for imports. This isn't part of
  /// swift::performWholeModuleTypeChecking because it's linear in the number
  /// of declarations in the module.
  void checkInconsistentImplementationOnlyImports(ModuleDecl *M);

  /// Recursively validate the specified type.
  ///
  /// This is used when dealing with partial source files (e.g. SIL parsing,
  /// code completion).
  ///
  /// \returns false on success, true on error.
  bool performTypeLocChecking(ASTContext &Ctx, TypeLoc &T,
                              DeclContext *DC,
                              bool ProduceDiagnostics = true);

  /// Recursively validate the specified type.
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
  GenericEnvironment *handleSILGenericParams(GenericParamList *genericParams,
                                             DeclContext *DC);

  /// Turn the given module into SIL IR.
  ///
  /// The module must contain source files. The optimizer will assume that the
  /// SIL of all files in the module is present in the SILModule.
  std::unique_ptr<SILModule>
  performSILGeneration(ModuleDecl *M, Lowering::TypeConverter &TC,
                       const SILOptions &options);

  /// Turn a source file into SIL IR.
  std::unique_ptr<SILModule>
  performSILGeneration(FileUnit &SF, Lowering::TypeConverter &TC,
                       const SILOptions &options);

  using ModuleOrSourceFile = PointerUnion<ModuleDecl *, SourceFile *>;

  /// Serializes a module or single source file to the given output file.
  void serialize(ModuleOrSourceFile DC, const SerializationOptions &options,
                 const SILModule *M = nullptr);

  /// Serializes a module or single source file to the given output file and
  /// returns back the file's contents as a memory buffer.
  ///
  /// Use this if you intend to immediately load the serialized module, as that
  /// will both avoid extra filesystem traffic and will ensure you read back
  /// exactly what was written.
  void serializeToBuffers(ModuleOrSourceFile DC,
                          const SerializationOptions &opts,
                          std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
                          std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
                          std::unique_ptr<llvm::MemoryBuffer> *moduleSourceInfoBuffer,
                          const SILModule *M = nullptr);

  // SWIFT_ENABLE_TENSORFLOW
  /// Serializes a module or single source file to a memory buffer, and returns
  /// the memory buffer in an output parameter. Does not write to the
  /// filesystem.
  ///
  /// \param moduleBuffer will be set to a pointer to the serialized module
  ///                     buffer. nullptr is allowed, in which case the module
  ///                     will not be serialized.
  /// \param moduleDocBuffer will be set to a pointer to the serialized module
  ///                        doc buffer. nullptr is allowed, in which case the
  ///                        module doc will not be serialized.
  void serializeToMemory(ModuleOrSourceFile DC,
                         const SerializationOptions &options,
                         std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
                         std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
                         const SILModule *M = nullptr);

  /// Get the CPU, subtarget feature options, and triple to use when emitting code.
  std::tuple<llvm::TargetOptions, std::string, std::vector<std::string>,
             std::string>
  getIRTargetOptions(const IRGenOptions &Opts, ASTContext &Ctx);

  /// Turn the given Swift module into either LLVM IR or native code
  /// and return the generated LLVM IR module.
  /// If you set an outModuleHash, then you need to call performLLVM.
  std::unique_ptr<llvm::Module>
  performIRGeneration(const IRGenOptions &Opts, ModuleDecl *M,
                      std::unique_ptr<SILModule> SILMod,
                      StringRef ModuleName, const PrimarySpecificPaths &PSPs,
                      llvm::LLVMContext &LLVMContext,
                      ArrayRef<std::string> parallelOutputFilenames,
                      llvm::GlobalVariable **outModuleHash = nullptr,
                      llvm::StringSet<> *LinkerDirectives = nullptr);

  /// Turn the given Swift module into either LLVM IR or native code
  /// and return the generated LLVM IR module.
  /// If you set an outModuleHash, then you need to call performLLVM.
  std::unique_ptr<llvm::Module>
  performIRGeneration(const IRGenOptions &Opts, SourceFile &SF,
                      std::unique_ptr<SILModule> SILMod,
                      StringRef ModuleName, const PrimarySpecificPaths &PSPs,
                      StringRef PrivateDiscriminator,
                      llvm::LLVMContext &LLVMContext,
                      llvm::GlobalVariable **outModuleHash = nullptr,
                      llvm::StringSet<> *LinkerDirectives = nullptr);

  /// Given an already created LLVM module, construct a pass pipeline and run
  /// the Swift LLVM Pipeline upon it. This does not cause the module to be
  /// printed, only to be optimized.
  void performLLVMOptimizations(const IRGenOptions &Opts, llvm::Module *Module,
                                llvm::TargetMachine *TargetMachine);

  /// Wrap a serialized module inside a swift AST section in an object file.
  void createSwiftModuleObjectFile(SILModule &SILMod, StringRef Buffer,
                                   StringRef OutputPath);

  /// Turn the given LLVM module into native code and return true on error.
  bool performLLVM(const IRGenOptions &Opts, ASTContext &Ctx, llvm::Module *Module,
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
  bool performLLVM(const IRGenOptions &Opts, DiagnosticEngine *Diags,
                   llvm::sys::Mutex *DiagMutex,
                   llvm::GlobalVariable *HashGlobal,
                   llvm::Module *Module,
                   llvm::TargetMachine *TargetMachine,
                   const version::Version &effectiveLanguageVersion,
                   StringRef OutputFilename,
                   UnifiedStatsReporter *Stats=nullptr);

  /// Dump YAML describing all fixed-size types imported from the given module.
  bool performDumpTypeInfo(const IRGenOptions &Opts,
                           SILModule &SILMod,
                           llvm::LLVMContext &LLVMContext);

  /// Creates a TargetMachine from the IRGen opts and AST Context.
  std::unique_ptr<llvm::TargetMachine>
  createTargetMachine(const IRGenOptions &Opts, ASTContext &Ctx);

  /// A convenience wrapper for Parser functionality.
  class ParserUnit {
  public:
    ParserUnit(SourceManager &SM, SourceFileKind SFKind, unsigned BufferID,
               const LangOptions &LangOpts, const TypeCheckerOptions &TyOpts,
               StringRef ModuleName,
               std::shared_ptr<SyntaxParseActions> spActions = nullptr,
               SyntaxParsingCache *SyntaxCache = nullptr);
    ParserUnit(SourceManager &SM, SourceFileKind SFKind, unsigned BufferID);
    ParserUnit(SourceManager &SM, SourceFileKind SFKind, unsigned BufferID,
               unsigned Offset, unsigned EndOffset);

    ~ParserUnit();

    OpaqueSyntaxNode parse();

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

  /// Register AST-level request functions with the evaluator.
  ///
  /// The ASTContext will automatically call these upon construction.
  void registerNameLookupRequestFunctions(Evaluator &evaluator);

  /// Register Parse-level request functions with the evaluator.
  ///
  /// Clients that form an ASTContext and will perform any parsing queries
  /// using Parse-level logic should call these functions after forming the
  /// ASTContext.
  void registerParseRequestFunctions(Evaluator &evaluator);

  /// Register Sema-level request functions with the evaluator.
  ///
  /// Clients that form an ASTContext and will perform any semantic queries
  /// using Sema-level logic should call these functions after forming the
  /// ASTContext.
  void registerTypeCheckerRequestFunctions(Evaluator &evaluator);

  /// Register SILGen-level request functions with the evaluator.
  ///
  /// Clients that form an ASTContext and will perform any SIL generation
  /// should call this functions after forming the ASTContext.
  void registerSILGenRequestFunctions(Evaluator &evaluator);

  /// Register IDE-level request functions with the evaluator.
  ///
  /// The ASTContext will automatically call these upon construction.
  void registerIDERequestFunctions(Evaluator &evaluator);

  /// Register type check request functions for IDE's usage with the evaluator.
  ///
  /// The ASTContext will automatically call these upon construction.
  /// Calling registerIDERequestFunctions will invoke this function as well.
  void registerIDETypeCheckRequestFunctions(Evaluator &evaluator);

} // end namespace swift

#endif // SWIFT_SUBSYSTEMS_H
