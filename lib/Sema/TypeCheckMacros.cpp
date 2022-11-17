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
#include "swift/AST/PrettyStackTrace.h"
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

extern "C" void
swift_ASTGen_getMacroOwningModule(void *macro,
                                  const char **owningModuleNamePtr,
                                  ptrdiff_t *owningModuleNameLengthPtr);

extern "C" void
swift_ASTGen_getMacroSupplementalSignatureModules(
    void *macro, const char **moduleNamesPtr, ptrdiff_t *moduleNamesLengthPtr);

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

#if SWIFT_SWIFT_PARSER

/// Compute the macro signature for a macro given the source code for its
/// generic signature and type signature.
static Optional<std::pair<GenericSignature, Type>>
getMacroSignature(
    ModuleDecl *mod, Identifier macroName,
    Optional<StringRef> genericSignature,
    StringRef typeSignature,
    ModuleDecl *owningModule,
    ArrayRef<ModuleDecl *> supplementalImportModules
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

  // Parse the struct declaration used for the macro evaluation context.
  auto *start = sourceMgr.getLocForBufferStart(macroBufferID)
                    .getOpaquePointerValue();
  auto decl = (Decl *)swift_ASTGen_getMacroEvaluationContext(
      start, (DeclContext *)macroSourceFile,
      &ctx, signatureSource.data(), signatureSource.size());

  if (!decl)
    return None;

  /// Add an import to the module.
  auto addImport = [&](ModuleDecl *importedModule) {
    ImportPath::Builder importPath;
    importPath.push_back(importedModule->getName(), SourceLoc());
    auto importDecl = ImportDecl::create(
        ctx, macroSourceFile, SourceLoc(), ImportKind::Module,
        SourceLoc(), importPath.get());
    importDecl->setImplicit();
    macroSourceFile->addTopLevelDecl(importDecl);
  };
  addImport(owningModule);
  std::for_each(supplementalImportModules.begin(),
                supplementalImportModules.end(),
                addImport);

  // Make sure imports are resolved in this file.
  performImportResolution(*macroSourceFile);

  auto typealias = cast<TypeAliasDecl>(decl);
  return std::make_pair(
      typealias->getGenericSignature(), typealias->getUnderlyingType());
}

/// Create a macro.
static MacroDecl *createMacro(
    ModuleDecl *mod, Identifier macroName,
    MacroDecl::ImplementationKind implKind,
    Optional<StringRef> genericSignature, StringRef typeSignature,
    StringRef owningModuleName,
    ArrayRef<StringRef> supplementalImportModuleNames,
    void* opaqueHandle
) {
  ASTContext &ctx = mod->getASTContext();

  // Look up the owning module.
  ModuleDecl *owningModule = ctx.getLoadedModule(
      ctx.getIdentifier(owningModuleName));
  if (!owningModule) {
    // FIXME: diagnostic here
    return nullptr;
  }
  // FIXME: Check that mod imports owningModule

  // Look up all of the supplemental import modules.
  SmallVector<ModuleDecl *, 1> supplementalImportModules;
  for (auto supplementalModuleName : supplementalImportModuleNames) {
    auto supplementalModuleId = ctx.getIdentifier(supplementalModuleName);
    if (auto supplementalModule = ctx.getLoadedModule(supplementalModuleId)) {
      supplementalImportModules.push_back(supplementalModule);

      // FIXME: Check that mod imports supplementalModule somewhere.
    }
  }

  // Get the type signature of the macro.
  auto signature = getMacroSignature(
      mod, macroName, genericSignature, typeSignature, owningModule,
      supplementalImportModules);
  if (!signature) {
    // FIXME: Swap in ErrorTypes, perhaps?
    return nullptr;
  }

  // FIXME: All macros are expression macros right now
  auto macro = new (ctx) MacroDecl(
      MacroDecl::Expression, implKind, macroName,
      owningModule, supplementalImportModules,
      opaqueHandle);

  // FIXME: Make these lazily computed.
  macro->setGenericSignature(signature->first);
  macro->setInterfaceType(signature->second);

  return macro;
}

/// Create a builtin macro.
static MacroDecl *createBuiltinMacro(
    ModuleDecl *mod, Identifier macroName, void *opaqueHandle) {
  // Get the macro generic signature.
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

  // Get the macro type signature.
  const char *typeSignaturePtr;
  ptrdiff_t typeSignatureLength;
  swift_ASTGen_getMacroTypeSignature(opaqueHandle, &typeSignaturePtr,
                                     &typeSignatureLength);
  SWIFT_DEFER {
    free((void*)typeSignaturePtr);
  };
  StringRef typeSignature = StringRef(typeSignaturePtr, typeSignatureLength);

  // Get the owning module name.
  const char *owningModuleNamePtr;
  ptrdiff_t owningModuleNameLength;
  swift_ASTGen_getMacroOwningModule(opaqueHandle, &owningModuleNamePtr,
                                    &owningModuleNameLength);
  SWIFT_DEFER {
    free((void*)owningModuleNamePtr);
  };
  StringRef owningModuleName = StringRef(
      owningModuleNamePtr, owningModuleNameLength);

  // Get the supplemental signature module names.
  const char *supplementalModuleNamesPtr;
  ptrdiff_t supplementalModuleNamesLength;
  swift_ASTGen_getMacroSupplementalSignatureModules(
      opaqueHandle, &supplementalModuleNamesPtr,
      &supplementalModuleNamesLength);
  SWIFT_DEFER {
    free((void*)supplementalModuleNamesPtr);
  };
  SmallVector<StringRef, 2> supplementalModuleNames;
  StringRef(owningModuleNamePtr, owningModuleNameLength)
    .split(supplementalModuleNames, ";", -1, false);

  return createMacro(
      mod, macroName, MacroDecl::ImplementationKind::Builtin,
      genericSignature, typeSignature,
      owningModuleName, supplementalModuleNames,
      opaqueHandle);
}

/// Create a plugin-based macro.
static MacroDecl *createPluginMacro(
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

  auto owningModuleName = plugin->invokeOwningModule();
  SWIFT_DEFER {
    free((void*)owningModuleName.data());
  };

  // Get the supplemental signature module names.
  auto supplementalModuleNamesStr =
      plugin->invokeSupplementalSignatureModules();
  SWIFT_DEFER {
    free((void*)supplementalModuleNamesStr.data());
  };
  SmallVector<StringRef, 2> supplementalModuleNames;
  supplementalModuleNamesStr
    .split(supplementalModuleNames, ";", -1, false);

  return createMacro(
      mod, macroName, MacroDecl::ImplementationKind::Plugin, genSignature,
      typeSignature, owningModuleName, supplementalModuleNames, plugin);
}
#endif

ArrayRef<MacroDecl *> MacroLookupRequest::evaluate(
    Evaluator &evaluator, Identifier macroName, ModuleDecl *mod
) const {
#if SWIFT_SWIFT_PARSER
  ASTContext &ctx = mod->getASTContext();
  SmallVector<MacroDecl *, 2> macros;

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
  for (auto plugin: ctx.getLoadedPlugins(macroName.str())) {
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
    DeclContext *dc, Expr *expr, ConcreteDeclRef macroRef, Type expandedType
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

  MacroDecl *macro = cast<MacroDecl>(macroRef.getDecl());
  {
    PrettyStackTraceExpr debugStack(ctx, "expanding macro", expr);

    switch (macro->implementationKind) {
    case MacroDecl::ImplementationKind::Builtin: {
      // Builtin macros are handled via ASTGen.
      auto astGenSourceFile = sourceFile->exportedSourceFile;
      if (!astGenSourceFile)
        return nullptr;

      // FIXME: Tell ASTGen which macro to use.
      const char *evaluatedSourceAddress;
      ptrdiff_t evaluatedSourceLength;
      swift_ASTGen_evaluateMacro(
          astGenSourceFile, expr->getStartLoc().getOpaquePointerValue(),
          &evaluatedSourceAddress, &evaluatedSourceLength);
      if (!evaluatedSourceAddress)
        return nullptr;
      evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                                (size_t)evaluatedSourceLength);
      break;
    }

    case MacroDecl::ImplementationKind::Plugin: {
      auto *plugin = (CompilerPlugin *)macro->opaqueHandle;
      auto bufferID = sourceFile->getBufferID();
      auto sourceFileText = sourceMgr.getEntireTextForBuffer(*bufferID);
      SmallVector<CompilerPlugin::Diagnostic, 8> pluginDiags;
      SWIFT_DEFER {
        for (auto &diag : pluginDiags)
          free((void*)diag.message.data());
      };
      auto evaluated = plugin->invokeRewrite(
         /*targetModuleName*/ dc->getParentModule()->getName().str(),
         /*filePath*/ sourceFile->getFilename(),
         /*sourceFileText*/ sourceFileText,
         /*range*/ Lexer::getCharSourceRangeFromSourceRange(
             sourceMgr, expr->getSourceRange()), ctx, pluginDiags);
      for (auto &diag : pluginDiags) {
        auto loc = sourceMgr.getLocForOffset(*bufferID, diag.position);
        switch (diag.severity) {
        case CompilerPlugin::DiagnosticSeverity::Note:
          ctx.Diags.diagnose(loc, diag::macro_note,
              ctx.getIdentifier(plugin->getName()),
              diag.message);
          break;
        case CompilerPlugin::DiagnosticSeverity::Warning:
          ctx.Diags.diagnose(loc, diag::macro_warning,
              ctx.getIdentifier(plugin->getName()),
              diag.message);
          break;
        case CompilerPlugin::DiagnosticSeverity::Error:
          ctx.Diags.diagnose(loc, diag::macro_error,
              ctx.getIdentifier(plugin->getName()),
              diag.message);
          break;
        }
      }
      if (evaluated)
        evaluatedSource = *evaluated;
      else
        return nullptr;
      break;
    }
    }
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName;
  {
    llvm::raw_string_ostream out(bufferName);

    out << "Macro expansion of #" << macro->getName();
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

  // Dump macro expansions to standard output, if requested.
  if (ctx.LangOpts.DumpMacroExpansions) {
    llvm::errs() << bufferName << " as " << expandedType.getString()
      << "\n------------------------------\n"
      << evaluatedSource
      << "\n------------------------------\n";
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

  PrettyStackTraceExpr debugStack(
      ctx, "type checking expanded macro", expandedExpr);
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
