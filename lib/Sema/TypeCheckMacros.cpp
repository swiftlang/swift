//===--- TypeCheckMacros.cpp -  Macro Handling ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for the evaluation of macros.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckMacros.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/CompilerPlugin.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Macro.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"

using namespace swift;

extern "C" void *swift_ASTGen_lookupMacro(const char *macroName);

extern "C" void swift_ASTGen_destroyMacro(void *macro);

extern "C" void *
swift_ASTGen_getMacroEvaluationContext(const void *sourceFile,
                                       void *declContext, void *astContext,
                                       const char *evaluatedSource,
                                       ptrdiff_t evaluatedSourceLength);

extern "C" ptrdiff_t swift_ASTGen_evaluateMacro(
    void *sourceFile, const void *sourceLocation,
    const char **evaluatedSource, ptrdiff_t *evaluatedSourceLength);

extern "C" void
swift_ASTGen_getMacroGenericSignature(void *macro,
                                      const char **genericSignaturePtr,
                                      ptrdiff_t *genericSignatureLengthPtr);

extern "C" void
swift_ASTGen_getMacroTypeSignature(void *macro,
                                   const char **signaturePtr,
                                   ptrdiff_t *signatureLengthPtr);

/// Create a new macro signature context buffer describing the macro signature.
///
/// The macro signature is a user-defined generic signature and return
/// type that serves as the "interface type" of references to the macro. The
/// current implementation takes those pieces of syntax from the macro itself,
/// then inserts them into a Swift typealias that looks like
///
/// \code
/// typealias __MacroEvaluationContext\(macro.genericSignature ?? "") =
///     \(macro.signature)
/// \endcode
///
/// So that we can use all of Swift's native name lookup and type resolution
/// facilities to map the parsed signature type back into a semantic
/// \c GenericSignature and \c Type ASTs. The macro signature source above is
/// provided via \c signatureSource.
static NullTerminatedStringRef
getMacroSignatureContextBuffer(
    ASTContext &ctx, Optional<StringRef> genericSignature,
    StringRef typeSignature
) {
  std::string source;
  llvm::raw_string_ostream out(source);
  out << "typealias __MacroEvaluationContext"
     << (genericSignature ? *genericSignature : "")
     << " = " << typeSignature << "\n";
  auto len = source.length();
  auto *buffer = (char *)malloc(len + 1);
  memcpy(buffer, source.data(), len + 1);
  return {buffer, len};
}

/// Compute the macro signature for a macro given the source code for its
/// generic signature and type signature.
static Optional<std::pair<GenericSignature, Type>>
getMacroSignature(
    ModuleDecl *mod, Identifier macroName,
    Optional<StringRef> genericSignature,
    StringRef typeSignature
) {
  // Form a buffer containing the macro signature context.
  ASTContext &ctx = mod->getASTContext();
  StringRef signatureSource = getMacroSignatureContextBuffer(
      ctx, genericSignature, typeSignature);

  // Create a new source buffer with the contents of the macro's
  // signature.
  SourceManager &sourceMgr = ctx.SourceMgr;
  std::string bufferName;
  {
    llvm::raw_string_ostream out(bufferName);
    out << "Macro signature of #" << macroName;
  }
  auto macroBuffer = llvm::MemoryBuffer::getMemBuffer(
      signatureSource, bufferName);

  // FIXME: We should tell the source manager that this is a macro signature
  // buffer.
  unsigned macroBufferID =
      sourceMgr.addNewSourceBuffer(std::move(macroBuffer));
  auto macroSourceFile = new (ctx) SourceFile(
      *mod, SourceFileKind::Library, macroBufferID);
  mod->addAuxiliaryFile(*macroSourceFile);

  // FIXME: Inject imports, which we don't yet have everywhere.

  // Parse the struct declaration used for the macro evaluation context.
  auto *start = sourceMgr.getLocForBufferStart(macroBufferID)
                    .getOpaquePointerValue();
  auto decl = (Decl *)swift_ASTGen_getMacroEvaluationContext(
      start, (DeclContext *)macroSourceFile,
      &ctx, signatureSource.data(), signatureSource.size());

  if (!decl)
    return None;

  // Make sure imports are resolved in this file.
  performImportResolution(*macroSourceFile);

  auto typealias = cast<TypeAliasDecl>(decl);
  return std::make_pair(
      typealias->getGenericSignature(), typealias->getUnderlyingType());
}

/// Create a macro.
static Macro *createMacro(
    ModuleDecl *mod, Identifier macroName,
    Macro::ImplementationKind implKind,
    Optional<StringRef> genericSignature, StringRef typeSignature,
    void* opaqueHandle
) {
  // Get the type signature of the macro.
  auto signature = getMacroSignature(
      mod, macroName, genericSignature, typeSignature);
  if (!signature) {
    // FIXME: Swap in ErrorTypes, perhaps?
    return nullptr;
  }

  // FIXME: All macros are expression macros right now
  ASTContext &ctx = mod->getASTContext();
  return new (ctx) Macro(
      Macro::Expression, implKind, macroName,
      signature->first, signature->second,
      /*FIXME:owningModule*/mod, /*FIXME:supplementalImportModules*/{},
      opaqueHandle);
}

/// Create a builtin macro.
static Macro *createBuiltinMacro(
    ModuleDecl *mod, Identifier macroName, void *opaqueHandle) {
  // Form the macro generic signature.
  const char *genericSignaturePtr;
  ptrdiff_t genericSignatureLength;
  swift_ASTGen_getMacroGenericSignature(opaqueHandle, &genericSignaturePtr,
                                        &genericSignatureLength);
  SWIFT_DEFER {
    free((void*)genericSignaturePtr);
  };

  Optional<StringRef> genericSignature;
  if (genericSignaturePtr && genericSignatureLength)
    genericSignature = StringRef(genericSignaturePtr, genericSignatureLength);

  // Form the macro type signature.
  const char *typeSignaturePtr;
  ptrdiff_t typeSignatureLength;
  swift_ASTGen_getMacroTypeSignature(opaqueHandle, &typeSignaturePtr,
                                     &typeSignatureLength);
  SWIFT_DEFER {
    free((void*)typeSignaturePtr);
  };
  StringRef typeSignature = StringRef(typeSignaturePtr, typeSignatureLength);

  return createMacro(
      mod, macroName, Macro::ImplementationKind::Builtin,
      genericSignature, typeSignature,
      opaqueHandle);
}

/// Create a plugin-based macro.
static Macro *createPluginMacro(
    ModuleDecl *mod, Identifier macroName, CompilerPlugin *plugin) {
  auto genSignature = plugin->invokeGenericSignature();
  SWIFT_DEFER {
    if (genSignature)
      free((void*)genSignature->data());
  };

  auto typeSignature = plugin->invokeTypeSignature();
  SWIFT_DEFER {
    free((void*)typeSignature.data());
  };

  return createMacro(
      mod, macroName, Macro::ImplementationKind::Plugin, genSignature,
      typeSignature, plugin);
}

ArrayRef<Macro *> MacroLookupRequest::evaluate(
    Evaluator &evaluator, Identifier macroName, ModuleDecl *mod
) const {
#if SWIFT_SWIFT_PARSER
  ASTContext &ctx = mod->getASTContext();
  SmallVector<Macro *, 2> macros;

  // Look for a builtin macro with this name.
  if (auto *builtinHandle = swift_ASTGen_lookupMacro(
          macroName.str().str().c_str())) {
    if (auto builtinMacro = createBuiltinMacro(mod, macroName, builtinHandle)) {
      macros.push_back(builtinMacro);

      // Make sure we clean up after the macro.
      ctx.addCleanup([builtinHandle]() {
        swift_ASTGen_destroyMacro(builtinHandle);
      });
    } else {
      // Destroy the builtin macro handle now; nothing references it.
      swift_ASTGen_destroyMacro(builtinHandle);
    }
  }

  // Look for a loaded plugin based on the macro name.
  // FIXME: This API needs to be able to return multiple plugins, because
  // several plugins could export a macro with the same name.
  if (auto *plugin = ctx.getLoadedPlugin(macroName.str())) {
    if (auto pluginMacro = createPluginMacro(mod, macroName, plugin)) {
      macros.push_back(pluginMacro);
    }
  }

  return ctx.AllocateCopy(macros);
#else
  return { };
#endif // SWIFT_SWIFT_PARSER
}

#if SWIFT_SWIFT_PARSER
Expr *swift::expandMacroExpr(
    DeclContext *dc, Expr *expr, StringRef macroName, Type expandedType
) {
  ASTContext &ctx = dc->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  // FIXME: Introduce a more robust way to ensure that we can get the "exported"
  // source file for a given context. If it's within a macro expansion, it
  // may not have a C++ SourceFile, but will have a Syntax tree.
  //
  // FIXME^2: And find a better name for "exportedSourceFile".
  auto sourceFile = dc->getParentSourceFile();
  if (!sourceFile)
    return nullptr;

  // Evaluate the macro.
  NullTerminatedStringRef evaluatedSource;

  // FIXME: The caller should tell us what macro is being expanded, so we
  // don't do this lookup again.

  // Built-in macros go through `MacroSystem` in Swift Syntax linked to this
  // compiler.
  if (auto *macro = swift_ASTGen_lookupMacro(macroName.str().c_str())) {
    auto astGenSourceFile = sourceFile->exportedSourceFile;
    if (!astGenSourceFile)
      return nullptr;
  
    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_evaluateMacro(
        astGenSourceFile, expr->getStartLoc().getOpaquePointerValue(),
        &evaluatedSourceAddress, &evaluatedSourceLength);
    if (!evaluatedSourceAddress)
      return nullptr;
    evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                              (size_t)evaluatedSourceLength);
    swift_ASTGen_destroyMacro(macro);
  }
  // Other macros go through a compiler plugin.
  else {
    auto mee = cast<MacroExpansionExpr>(expr);
    auto *plugin = ctx.getLoadedPlugin(macroName);
    assert(plugin && "Should have been checked during earlier type checking");
    auto bufferID = sourceFile->getBufferID();
    auto sourceFileText = sourceMgr.getEntireTextForBuffer(*bufferID);
    auto evaluated = plugin->invokeRewrite(
        /*targetModuleName*/ dc->getParentModule()->getName().str(),
        /*filePath*/ sourceFile->getFilename(),
        /*sourceFileText*/ sourceFileText,
        /*range*/ Lexer::getCharSourceRangeFromSourceRange(
            sourceMgr, mee->getSourceRange()),
        ctx);
    if (evaluated)
      evaluatedSource = *evaluated;
    else
      return nullptr;
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName;
  {
    llvm::raw_string_ostream out(bufferName);

    out << "Macro expansion of #" << macroName;
    if (auto bufferID = sourceFile->getBufferID()) {
      unsigned startLine, startColumn;
      std::tie(startLine, startColumn) =
          sourceMgr.getLineAndColumnInBuffer(expr->getStartLoc(), *bufferID);

      SourceLoc endLoc =
          Lexer::getLocForEndOfToken(sourceMgr, expr->getEndLoc());
      unsigned endLine, endColumn;
      std::tie(endLine, endColumn) =
          sourceMgr.getLineAndColumnInBuffer(endLoc, *bufferID);

      out << " in " << sourceMgr.getIdentifierForBuffer(*bufferID) << ":"
          << startLine << ":" << startColumn
          << "-" << endLine << ":" << endColumn;
    }
  }

  // Create a new source buffer with the contents of the expanded macro.
  auto macroBuffer =
      llvm::MemoryBuffer::getMemBuffer(evaluatedSource, bufferName);
  unsigned macroBufferID = sourceMgr.addNewSourceBuffer(std::move(macroBuffer));

  // Create a source file to hold the macro buffer. This is automatically
  // registered with the enclosing module.
  auto macroSourceFile = new (ctx) SourceFile(
      *dc->getParentModule(), SourceFileKind::MacroExpansion, macroBufferID,
      /*parsingOpts=*/{}, /*isPrimary=*/false, expr);

  // Parse the expression.
  Parser parser(macroBufferID, *macroSourceFile, &ctx.Diags, nullptr, nullptr);
  parser.consumeTokenWithoutFeedingReceiver();

  // Set up a "local context" for parsing, so that we have a source of
  // closure and local-variable discriminators.
  LocalContext tempContext{};
  parser.CurDeclContext = dc;
  parser.CurLocalContext = &tempContext;
  {
    DiscriminatorFinder finder;
    expr->walk(finder);

    unsigned closureDiscriminator;
    if (finder.getFirstDiscriminator() ==
          AbstractClosureExpr::InvalidDiscriminator)
      closureDiscriminator = 0;
    else
      closureDiscriminator = finder.getFirstDiscriminator() + 1;

    tempContext.overrideNextClosureDiscriminator(closureDiscriminator);
  }

  auto parsedResult = parser.parseExpr(diag::expected_macro_expansion_expr);
  if (parsedResult.isParseError() || parsedResult.isNull()) {
    // Tack on a note to say where we expanded the macro from?
    return nullptr;
  }

  // Type-check the expanded expression.
  // FIXME: Would like to pass through type checking options like "discarded"
  // that are captured by TypeCheckExprOptions.
  Expr *expandedExpr = parsedResult.get();
  constraints::ContextualTypeInfo contextualType {
    TypeLoc::withoutLoc(expandedType),
    // FIXME: Add a contextual type purpose for macro expansion.
    ContextualTypePurpose::CTP_CoerceOperand
  };

  Type realExpandedType = TypeChecker::typeCheckExpression(
      expandedExpr, dc, contextualType);
  if (!realExpandedType)
    return nullptr;

  assert((expandedType->isEqual(realExpandedType) ||
          realExpandedType->hasError()) &&
         "Type checking changed the result type?");
  return expandedExpr;
}

#endif // SWIFT_SWIFT_PARSER
