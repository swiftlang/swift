//===--- ClangImporter.cpp - Import Clang Modules -------------------------===//
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
// This file implements support for loading Clang modules into Swift.
//
//===----------------------------------------------------------------------===//
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "ImporterImpl.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/StringExtras.h"
#include "swift/ClangImporter/ClangImporterOptions.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/Version.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Path.h"
#include <algorithm>
#include <memory>

using namespace swift;

//===--------------------------------------------------------------------===//
// Importer statistics
//===--------------------------------------------------------------------===//
#define DEBUG_TYPE "Clang module importer"
STATISTIC(NumNullaryMethodNames,
          "nullary selectors imported");
STATISTIC(NumUnaryMethodNames,
          "unary selectors imported");
STATISTIC(NumNullaryInitMethodsMadeUnary,
          "nullary Objective-C init methods turned into unary initializers");
STATISTIC(NumMultiMethodNames,
          "multi-part selector method names imported");
STATISTIC(NumPrepositionSplitMethodNames,
          "selectors where the first selector piece was split on a "
          "preposition");
STATISTIC(NumLinkingVerbNonSplits,
          "selectors where splitting was prevented by a linking verb ");
STATISTIC(NumLinkingVerbNonSplitsPreposition,
          "selectors where preposition splitting was prevented by a linking "
          "verb ");
STATISTIC(NumPrepositionTrailingFirstPiece,
          "selectors where the first piece ends in a preposition");
STATISTIC(NumSelectorsNotSplit,
          "selectors that were not split (for any reason)");
STATISTIC(NumMethodsMissingFirstArgName,
          "selectors where the first argument name is missing");

// Commonly-used Clang classes.
using clang::CompilerInstance;
using clang::CompilerInvocation;

ClangImporterCtorTy swift::getClangImporterCtor() {
  return &ClangImporter::create;
}

#pragma mark Internal data structures
namespace {
  class SwiftModuleLoaderAction : public clang::SyntaxOnlyAction {
  protected:
    /// BeginSourceFileAction - Callback at the start of processing a single
    /// input.
    ///
    /// \return True on success; on failure \see ExecutionAction() and
    /// EndSourceFileAction() will not be called.
    virtual bool BeginSourceFileAction(CompilerInstance &ci,
                                       StringRef filename) {
      // Enable incremental processing, so we can load modules after we've
      // finished parsing our fake translation unit.
      ci.getPreprocessor().enableIncrementalProcessing();

      return clang::SyntaxOnlyAction::BeginSourceFileAction(ci, filename);
    }
  };
}


ClangImporter::ClangImporter(ASTContext &ctx,
                             const ClangImporterOptions &clangImporterOpts)
  : Impl(*new Implementation(ctx, clangImporterOpts))
{
}

ClangImporter::~ClangImporter() {
  delete &Impl;
}

void ClangImporter::setTypeResolver(LazyResolver &resolver) {
  Impl.setTypeResolver(&resolver);
}

void ClangImporter::clearTypeResolver() {
  Impl.setTypeResolver(nullptr);
}

#pragma mark Module loading

// FIXME: Unify this with the similar code in the driver.
static void configureDefaultCPU(const llvm::Triple &triple, 
                                std::vector<std::string> &args) {
  if (triple.isOSDarwin() && triple.getArch() == llvm::Triple::arm64) {
    args.push_back("-target-cpu");
    args.push_back("cyclone");
    args.push_back("-target-feature");
    args.push_back("+neon");
  }
}

ClangImporter *ClangImporter::create(ASTContext &ctx, StringRef targetTriple,
    const ClangImporterOptions &clangImporterOpts) {
  std::unique_ptr<ClangImporter> importer{
    new ClangImporter(ctx, clangImporterOpts)
  };

  // Get the SearchPathOptions to use when creating the Clang importer.
  SearchPathOptions &searchPathOpts = ctx.SearchPathOpts;

  // Create a Clang diagnostics engine.
  // FIXME: Route these diagnostics back to Swift's diagnostics engine,
  // somehow. We'll lose macro expansions, but so what.
  auto clangDiags(CompilerInstance::createDiagnostics(
                    new clang::DiagnosticOptions, 0, nullptr));

  // Don't stop emitting messages if we ever can't find a file.
  // FIXME: This is actually a general problem: any "fatal" error could mess up
  // the CompilerInvocation.
  clangDiags->setDiagnosticMapping(clang::diag::err_module_not_found,
                                   clang::diag::Mapping::MAP_ERROR,
                                   clang::SourceLocation());

  // Construct the invocation arguments for Objective-C ARC with the current
  // target.
  //
  // FIXME: Figure out an appropriate OS deployment version to pass along.
  std::vector<std::string> invocationArgStrs = {
    "-x", "objective-c", "-std=gnu11", "-fobjc-arc", "-fmodules", "-fblocks",
    "-fsyntax-only", "-w", "-triple", targetTriple.str(),
    "-I", searchPathOpts.RuntimeResourcePath,
    "-DSWIFT_CLASS_EXTRA=__attribute__((annotate(\""
      SWIFT_NATIVE_ANNOTATION_STRING "\")))",
    "-DSWIFT_PROTOCOL_EXTRA=__attribute__((annotate(\""
      SWIFT_NATIVE_ANNOTATION_STRING "\")))",
    "-fretain-comments-from-system-headers",
    "-fmodules-validate-system-headers",
    "swift.m"
  };

  configureDefaultCPU(llvm::Triple(targetTriple), invocationArgStrs);

  if (ctx.LangOpts.EnableAppExtensionRestrictions) {
    invocationArgStrs.push_back("-fapplication-extension");
  }

  if (searchPathOpts.SDKPath.empty()) {
    invocationArgStrs.push_back("-nostdsysteminc");
  } else {
    invocationArgStrs.push_back("-isysroot");
    invocationArgStrs.push_back(searchPathOpts.SDKPath);
  }

  for (auto path : searchPathOpts.ImportSearchPaths) {
    invocationArgStrs.push_back("-I");
    invocationArgStrs.push_back(path);
  }

  for (auto path : searchPathOpts.FrameworkSearchPaths) {
    invocationArgStrs.push_back("-F");
    invocationArgStrs.push_back(path);
  }

  const std::string &moduleCachePath = clangImporterOpts.ModuleCachePath;

  // Set the module cache path.
  if (moduleCachePath.empty()) {
    llvm::SmallString<128> DefaultModuleCache;
    llvm::sys::path::system_temp_directory(/*erasedOnReboot=*/false,
                                           DefaultModuleCache);
    llvm::sys::path::append(DefaultModuleCache, "org.llvm.clang");
    llvm::sys::path::append(DefaultModuleCache, "ModuleCache");
    invocationArgStrs.push_back("-fmodules-cache-path=");
    invocationArgStrs.back().append(DefaultModuleCache.str());
  } else {
    invocationArgStrs.push_back("-fmodules-cache-path=");
    invocationArgStrs.back().append(moduleCachePath);
  }

  const std::string &overrideResourceDir = clangImporterOpts.OverrideResourceDir;

  if (overrideResourceDir.empty()) {
    llvm::SmallString<128> resourceDir(searchPathOpts.RuntimeResourcePath);
  
    // Adjust the path torefer to our copy of the Clang headers under
    // lib/swift/clang.
  
    llvm::sys::path::append(resourceDir, "clang", CLANG_VERSION_STRING);
  
    // Set the Clang resource directory to the path we computed.
    invocationArgStrs.push_back("-resource-dir");
    invocationArgStrs.push_back(resourceDir.str());
  } else {
    invocationArgStrs.push_back("-resource-dir");
    invocationArgStrs.push_back(overrideResourceDir);
  }

  for (auto extraArg : clangImporterOpts.ExtraArgs) {
    invocationArgStrs.push_back(extraArg);
  }

  std::vector<const char *> invocationArgs;
  for (auto &argStr : invocationArgStrs)
    invocationArgs.push_back(argStr.c_str());

  // Create a new Clang compiler invocation.
  llvm::IntrusiveRefCntPtr<CompilerInvocation> invocation
    = new CompilerInvocation;
  if (!CompilerInvocation::CreateFromArgs(*invocation,
                                         &invocationArgs.front(),
                                         (&invocationArgs.front() +
                                          invocationArgs.size()),
                                         *clangDiags))
    return nullptr;
  importer->Impl.Invocation = invocation;

  // Create an almost-empty memory buffer corresponding to the file "swift.m"
  auto sourceBuffer = llvm::MemoryBuffer::getMemBuffer("extern int __swift;");
  invocation->getPreprocessorOpts().addRemappedFile("swift.m", sourceBuffer);

  // Create a compiler instance.
  importer->Impl.Instance.reset(new CompilerInstance);
  auto &instance = *importer->Impl.Instance;
  instance.setDiagnostics(&*clangDiags);
  instance.setInvocation(&*invocation);

  // Create the associated action.
  importer->Impl.Action.reset(new SwiftModuleLoaderAction);

  // Execute the action. We effectively inline most of
  // CompilerInstance::ExecuteAction here, because we need to leave the AST
  // open for future module loading.
  // FIXME: This has to be cleaned up on the Clang side before we can improve
  // things here.

  // Create the target instance.
  instance.setTarget(
    clang::TargetInfo::CreateTargetInfo(*clangDiags,&instance.getTargetOpts()));
  if (!instance.hasTarget())
    return nullptr;

  // Inform the target of the language options.
  //
  // FIXME: We shouldn't need to do this, the target should be immutable once
  // created. This complexity should be lifted elsewhere.
  instance.getTarget().setForcedLangOptions(instance.getLangOpts());

  // Run the action.
  auto &action = *importer->Impl.Action;
  if (action.BeginSourceFile(instance, instance.getFrontendOpts().Inputs[0])) {
    action.Execute();
    // Note: don't call EndSourceFile here!
  }
  // FIXME: This is necessary because Clang doesn't really support what we're
  // doing, and TUScope has gone stale.
  instance.getSema().TUScope = nullptr;

  // Create the selectors we'll be looking for.
  auto &clangContext = importer->Impl.Instance->getASTContext();
  importer->Impl.objectAtIndexedSubscript
    = clangContext.Selectors.getUnarySelector(
        &clangContext.Idents.get("objectAtIndexedSubscript"));
  clang::IdentifierInfo *setObjectAtIndexedSubscriptIdents[2] = {
    &clangContext.Idents.get("setObject"),
    &clangContext.Idents.get("atIndexedSubscript")
  };
  importer->Impl.setObjectAtIndexedSubscript
    = clangContext.Selectors.getSelector(2, setObjectAtIndexedSubscriptIdents);
  importer->Impl.objectForKeyedSubscript
    = clangContext.Selectors.getUnarySelector(
        &clangContext.Idents.get("objectForKeyedSubscript"));
  clang::IdentifierInfo *setObjectForKeyedSubscriptIdents[2] = {
    &clangContext.Idents.get("setObject"),
    &clangContext.Idents.get("forKeyedSubscript")
  };
  importer->Impl.setObjectForKeyedSubscript
    = clangContext.Selectors.getSelector(2, setObjectForKeyedSubscriptIdents);

  return importer.release();
}

Module *ClangImporter::loadModule(
    SourceLoc importLoc,
    ArrayRef<std::pair<Identifier, SourceLoc>> path) {

  // Convert the Swift import path over to a Clang import path.
  // FIXME: Map source locations over. Fun, fun!
  SmallVector<std::pair<clang::IdentifierInfo *, clang::SourceLocation>, 4>
    clangPath;

  auto &clangContext = Impl.Instance->getASTContext();
  for (auto component : path) {
    clangPath.push_back({&clangContext.Idents.get(component.first.str()),
                         clang::SourceLocation()} );
  }

  // Load the Clang module.
  // FIXME: The source location here is completely bogus. It can't be
  // invalid, and it can't be the same thing twice in a row, so we just use
  // a counter. Having real source locations would be far, far better.
  // FIXME: This should not print a message if we just can't find a Clang
  // module -- that's Swift's responsibility, since there could in theory be a
  // later module loader.
  auto &srcMgr = clangContext.getSourceManager();
  clang::SourceLocation clangImportLoc
    = srcMgr.getLocForStartOfFile(srcMgr.getMainFileID())
            .getLocWithOffset(Impl.ImportCounter++);
  auto clangModule = Impl.Instance->loadModule(clangImportLoc,
                                               clangPath,
                                               clang::Module::AllVisible,
                                               /*IsInclusionDirective=*/false);
  if (!clangModule)
    return nullptr;

  // Bump the generation count.
  ++Impl.Generation;
  Impl.SwiftContext.bumpGeneration();
  Impl.CachedVisibleDecls.clear();
  Impl.CurrentCacheState = Implementation::CacheState::Invalid;

  auto &cacheEntry = Impl.ModuleWrappers[clangModule];
  if (ClangModuleUnit *cached = cacheEntry.getPointer()) {
    Module *M = cached->getParentModule();
    if (!cacheEntry.getInt()) {
      // Force load adapter modules for all imported modules.
      // FIXME: This forces the creation of wrapper modules for all imports as
      // well, and may do unnecessary work.
      cacheEntry.setInt(true);
      M->forAllVisibleModules(path, [&](Module::ImportedModule import) {});
    }
    return M;
  }

  // Build the representation of the Clang module in Swift.
  // FIXME: The name of this module could end up as a key in the ASTContext,
  // but that's not correct for submodules.
  Identifier name = Impl.SwiftContext.getIdentifier((*clangModule).Name);
  auto result = Module::create(name, Impl.SwiftContext);

  auto file = new (Impl.SwiftContext) ClangModuleUnit(*result, *this,
                                                      clangModule);
  result->addFile(*file);
  cacheEntry.setPointerAndInt(file, true);

  // FIXME: Total hack.
  if (!Impl.firstClangModule)
    Impl.firstClangModule = file;

  // Force load adapter modules for all imported modules.
  // FIXME: This forces the creation of wrapper modules for all imports as
  // well, and may do unnecessary work.
  result->forAllVisibleModules(path, [](Module::ImportedModule import) {});

  return result;
}

ClangImporter::Implementation::Implementation(ASTContext &ctx,
                                              const ClangImporterOptions &opts)
  : SwiftContext(ctx),
    SplitPrepositions(ctx.LangOpts.SplitPrepositions),
    InferImplicitProperties(opts.InferImplicitProperties),
    ImportFactoryMethodsAsConstructors(opts.ImportFactoryMethodsAsConstructors)
{
}

ClangModuleUnit *
ClangImporter::Implementation::getWrapperForModule(ClangImporter &importer,
                                                   clang::Module *underlying) {
  auto &cacheEntry = ModuleWrappers[underlying];
  if (ClangModuleUnit *cached = cacheEntry.getPointer())
    return cached;

  // FIXME: Handle hierarchical names better.
  Identifier name = SwiftContext.getIdentifier(underlying->Name);
  auto wrapper = Module::create(name, SwiftContext);

  auto file = new (SwiftContext) ClangModuleUnit(*wrapper, importer,
                                                 underlying);
  wrapper->addFile(*file);
  cacheEntry.setPointer(file);

  return file;
}

clang::Module *ClangImporter::Implementation::getClangSubmoduleForDecl(
    const clang::Decl *D,
    bool allowForwardDeclaration) {
  const clang::Decl *actual = nullptr;
  if (auto OID = dyn_cast<clang::ObjCInterfaceDecl>(D)) {
    // Put the Objective-C class into the module that contains the @interface
    // definition, not just some @class forward declaration.
    actual = OID->getDefinition();
    if (!actual && !allowForwardDeclaration)
      return nullptr;

  } else if (auto TD = dyn_cast<clang::TagDecl>(D)) {
    actual = TD->getDefinition();
    if (!actual && !allowForwardDeclaration)
      return nullptr;
  }

  if (!actual)
    actual = D->getCanonicalDecl();

  return actual->getOwningModule();
}

ClangModuleUnit *ClangImporter::Implementation::getClangModuleForDecl(
    const clang::Decl *D,
    bool allowForwardDeclaration) {
  clang::Module *M = getClangSubmoduleForDecl(D, allowForwardDeclaration);
  if (!M)
    return nullptr;

  // Get the parent module because currently we don't represent submodules with
  // ClangModule.
  // FIXME: this is just a workaround until we can import submodules.
  M = M->getTopLevelModule();

  auto &importer =
    static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());
  return getWrapperForModule(importer, M);
}

#pragma mark Source locations
clang::SourceLocation
ClangImporter::Implementation::importSourceLoc(SourceLoc loc) {
  // FIXME: Implement!
  return clang::SourceLocation();
}

SourceLoc
ClangImporter::Implementation::importSourceLoc(clang::SourceLocation loc) {
  // FIXME: Implement!
  return SourceLoc();
}

SourceRange
ClangImporter::Implementation::importSourceRange(clang::SourceRange loc) {
  // FIXME: Implement!
  return SourceRange();
}

#pragma mark Importing names

/// \brief Determine whether the given name is reserved for Swift.
static bool isSwiftReservedName(StringRef name) {
  /// FIXME: Check Swift keywords.
  return llvm::StringSwitch<bool>(name)
           .Cases("true", "false", true)
           .Default(false);
}

clang::DeclarationName
ClangImporter::Implementation::importName(Identifier name) {
  // FIXME: When we start dealing with C++, we can map over some operator
  // names.
  if (name.isOperator())
    return clang::DeclarationName();

  if (isSwiftReservedName(name.str()))
    return clang::DeclarationName();

  // Map the identifier. If it's some kind of keyword, it can't be mapped.
  auto ident = &Instance->getASTContext().Idents.get(name.str());
  if (ident->getTokenID() != clang::tok::identifier)
    return clang::DeclarationName();

  return ident;
}

Identifier
ClangImporter::Implementation::importName(clang::DeclarationName name,
                                          StringRef suffix,
                                          StringRef removePrefix) {
  // FIXME: At some point, we'll be able to import operators as well.
  if (!name || name.getNameKind() != clang::DeclarationName::Identifier)
    return Identifier();

  StringRef nameStr = name.getAsIdentifierInfo()->getName();
  // Remove the prefix, if any.
  if (!removePrefix.empty()) {
    assert(nameStr.startswith(removePrefix)
           && "name doesn't start with given removal prefix");
    nameStr = nameStr.slice(removePrefix.size(), nameStr.size());
  }

  // Get the Swift identifier.
  if (suffix.empty()) {
    if (isSwiftReservedName(nameStr))
      return Identifier();

    return SwiftContext.getIdentifier(nameStr);
  }

  // Append the suffix, and try again.
  llvm::SmallString<64> nameBuf;
  nameBuf += nameStr;
  nameBuf += suffix;

  if (isSwiftReservedName(nameBuf))
    return Identifier();

  return SwiftContext.getIdentifier(nameBuf);
}

/// Split the given selector piece at the given index, updating
/// capitalization as required.
///
/// \param selector The selector piece.
/// \param index The index at which to split.
/// \param buffer Buffer used for scratch space.
///
/// \returns a pair of strings \c (before,after), where \c after has
/// has its capitalization adjusted if necessary.
static std::pair<StringRef, StringRef>
splitSelectorPieceAt(StringRef selector, unsigned index,
                     SmallVectorImpl<char> &buffer) {
  // If the split point is at the end of the selector, the solution is
  // trivial.
  if (selector.str().size() == index) {
    return { selector, "" };
  }

  // Split at the index and lowercase the parameter name.
  return { selector.substr(0, index),
           camel_case::toLowercaseWord(selector.substr(index), buffer) };
}

/// Split the first selector piece into a function name and first
/// parameter name, if possible.
///
/// \param selector The selector piece to split.
/// \param scratch Scratch space to use when forming strings.
///
/// \returns a (function name, first parameter name) pair describing
/// the split. If no split is possible, the function name will be \c
/// selector and the parameter name will be empty.
static std::pair<StringRef, StringRef> 
splitFirstSelectorPiece(StringRef selector,
                        SmallVectorImpl<char> &scratch) {
  // Get the camelCase words in this selector.
  auto words = camel_case::getWords(selector);
  if (words.empty())
    return { selector, "" };

  // If "init" is the first word, we have an initializer.
  if (*words.begin() == "init") {
    return splitSelectorPieceAt(selector, 4, scratch);
  }

  // Scan words from the back of the selector looking for a
  // preposition.
  auto lastPrep = words.rbegin();
  auto end = words.rend();
  for (; lastPrep != end; ++lastPrep) {
    // If we found a linking verb, don't split.
    if (isLinkingVerb(*lastPrep)) {
      ++NumLinkingVerbNonSplits;
      return { selector, "" };
    }

    if (getPrepositionKind(*lastPrep) != PK_None) {
      // Found a preposition.
      break;
    }
  }

  // If we found a preposition, split here.
  if (lastPrep != end) {
    // .. unless there is a linking verb.
    for (auto i = std::next(lastPrep); i != end; ++i) {
      // If we found a linking verb, don't split.
      if (isLinkingVerb(*i)) {
        ++NumLinkingVerbNonSplitsPreposition;
        return { selector, "" };
      }
    }

    ++NumPrepositionSplitMethodNames;
    if (lastPrep == words.rbegin())
      ++NumPrepositionTrailingFirstPiece;
    return splitSelectorPieceAt(selector,
                                std::prev(lastPrep.base()).getPosition(),
                                scratch);
  }

  // We did not find a preposition. Nothing to split.
  ++NumSelectorsNotSplit;
  return { selector, "" };
}

/// Import an argument name.
static Identifier importArgName(ASTContext &ctx, StringRef name) {
  // Simple case: empty name.
  if (name.empty())
    return Identifier();

  // If the first word isn't a non-directional preposition, lowercase
  // it to form the argument name.
  llvm::SmallString<16> scratch;
  auto firstWord = camel_case::getFirstWord(name);
  if (!ctx.LangOpts.SplitPrepositions ||
      getPrepositionKind(firstWord) != PK_Nondirectional)
    return ctx.getIdentifier(camel_case::toLowercaseWord(name, scratch));

  // The first word is a non-directional preposition.

  // If there's only one word, we're left with no argument name.
  if (firstWord.size() == name.size())
    return Identifier();
  
  return ctx.getIdentifier(
           camel_case::toLowercaseWord(name.substr(firstWord.size()), scratch));
}

/// Map an Objective-C selector name to a Swift method name.
static DeclName mapSelectorName(ASTContext &ctx,
                                ObjCSelector selector,
                                bool isInitializer) {
  // Zero-argument selectors.
  if (selector.getNumArgs() == 0) {
    ++NumNullaryMethodNames;

    auto name = selector.getSelectorPieces()[0];
    StringRef nameText = name.empty()? "" : name.str();

    // Simple case.
    if (!isInitializer || nameText.size() == 4)
      return DeclName(ctx, name, { });

    // This is an initializer with no parameters but a name that
    // contains more than 'init', so synthesize an argument to capture
    // what follows 'init'.
    ++NumNullaryInitMethodsMadeUnary;
    assert(camel_case::getFirstWord(nameText).equals("init"));
    auto baseName = ctx.Id_init;
    auto argName = importArgName(ctx, nameText.substr(4));
    return DeclName(ctx, baseName, argName);
  }

  // Determine the base name and first argument name.
  Identifier baseName;
  SmallVector<Identifier, 2> argumentNames;
  Identifier firstPiece = selector.getSelectorPieces()[0];
  StringRef firstPieceText = firstPiece.empty()? "" : firstPiece.str();
  if (isInitializer) {
    assert(camel_case::getFirstWord(firstPieceText).equals("init"));
    baseName = ctx.Id_init;
    argumentNames.push_back(importArgName(ctx, firstPieceText.substr(4)));
  } else if (ctx.LangOpts.SplitPrepositions) {
    llvm::SmallString<16> scratch;
    StringRef funcName, firstArgName;
    std::tie(funcName, firstArgName) = splitFirstSelectorPiece(firstPieceText,
                                                               scratch);
    baseName = ctx.getIdentifier(funcName);
    argumentNames.push_back(importArgName(ctx, firstArgName));
  } else {
    baseName = firstPiece;
    argumentNames.push_back(Identifier());
  }

  if (argumentNames[0].empty())
    ++NumMethodsMissingFirstArgName;

  // Determine the remaining argument names.
  unsigned n = selector.getNumArgs();
  if (n == 1)
    ++NumUnaryMethodNames;
  else
    ++NumMultiMethodNames;

  for (auto piece : selector.getSelectorPieces().slice(1)) {
    if (piece.empty())
      argumentNames.push_back(piece);
    else
      argumentNames.push_back(importArgName(ctx, piece.str()));
  }
  return DeclName(ctx, baseName, argumentNames);
}

namespace {
  /// Function object used to create Clang selectors from strings.
  class CreateSelector {
    ASTContext &Ctx;
      
  public:
    CreateSelector(ASTContext &ctx) : Ctx(ctx){ }

    template<typename ...Strings>
    ObjCSelector operator()(unsigned numParams, Strings ...strings) const {
      Identifier pieces[sizeof...(Strings)] = {
        (strings[0]? Ctx.getIdentifier(strings) : Identifier())...
      };
      
      assert((numParams == 0 && sizeof...(Strings) == 1) ||
             (numParams > 0 && sizeof...(Strings) == numParams));
      return ObjCSelector(Ctx, numParams, pieces);
    }
  };

  /// Function object used to create Swift method names from strings.
  class CreateMethodName {
    ASTContext &Ctx;
    Identifier BaseName;

  public:
    CreateMethodName(ASTContext &ctx, StringRef baseName) 
      : Ctx(ctx)
    { 
      BaseName = Ctx.getIdentifier(baseName);
    }

    template<typename ...Strings>
    DeclName operator()(Strings ...strings) const {
      Identifier pieces[sizeof...(Strings)] = {
        (strings[0]? Ctx.getIdentifier(strings) : Identifier())...
      };

      return DeclName(Ctx, BaseName, pieces);
    }
  };
}

ObjCSelector ClangImporter::Implementation::importSelector(
               clang::Selector selector) {
  auto &ctx = SwiftContext;

  // Handle zero-argument selectors directly.
  if (selector.isUnarySelector()) {
    Identifier name;
    if (auto id = selector.getIdentifierInfoForSlot(0))
      name = ctx.getIdentifier(id->getName());
    return ObjCSelector(ctx, 0, name);
  }

  SmallVector<Identifier, 2> pieces;
  for (auto i = 0u, n = selector.getNumArgs(); i != n; ++i) {
    Identifier piece;
    if (auto id = selector.getIdentifierInfoForSlot(i))
      piece = ctx.getIdentifier(id->getName());
    pieces.push_back(piece);
  }

  return ObjCSelector(ctx, pieces.size(), pieces);
}

DeclName ClangImporter::Implementation::mapSelectorToDeclName(
           ObjCSelector selector,
           bool isInitializer)
{
  // If we haven't computed any selector mappings before, load the
  // default mappings.
  if (SelectorMappings.empty() && SplitPrepositions) {
    // Function object that 
    CreateSelector createSelector(SwiftContext);

#define MAP_SELECTOR(Selector, ForMethod, ForInitializer, BaseName, ArgNames) \
    {                                                                   \
      auto selector = createSelector Selector;                          \
      CreateMethodName createMethodName(SwiftContext, BaseName);        \
      auto methodName = createMethodName ArgNames;                      \
      assert((ForMethod || ForInitializer) &&                           \
             "Must be for a method or initializer");                    \
      assert(selector.getNumArgs() == methodName.getArgumentNames().size()); \
      if (ForMethod)                                                    \
        SelectorMappings[{selector, false}] = methodName;               \
      if (ForInitializer) {                                             \
        assert(methodName.getBaseName() == SwiftContext.Id_init);       \
        SelectorMappings[{selector, true}] = methodName;                \
      }                                                                 \
    }
#include "MappedNames.def"
  }

  // Check whether we've already mapped this selector.
  auto known = SelectorMappings.find({selector, isInitializer});
  if (known != SelectorMappings.end())
    return known->second;

  // Map the selector.
  auto result = mapSelectorName(SwiftContext, selector, isInitializer);

  // Cache the result and return.
  SelectorMappings[{selector, isInitializer}] = result;
  return result;
}

DeclName ClangImporter::Implementation::mapFactorySelectorToInitializerName(
           ObjCSelector selector,
           StringRef className) {
  auto firstPiece = selector.getSelectorPieces()[0];
  if (firstPiece.empty())
    return DeclName();

  // Match the camelCase beginning of the first selector piece to the
  // ending of the class name.
  auto firstPieceStr = firstPiece.str();
  auto methodWords = camel_case::getWords(firstPieceStr);
  auto classWords = camel_case::getWords(className);
  auto methodWordIter = methodWords.begin(),
    methodWordIterEnd = methodWords.end();
  auto classWordRevIter = classWords.rbegin(),
    classWordRevIterEnd = classWords.rend();

  // Find the last instance of the first word in the method's name within
  // the words in the class name.
  while (classWordRevIter != classWordRevIterEnd &&
         !camel_case::sameWordIgnoreFirstCase(*methodWordIter,
                                              *classWordRevIter)) {
    ++classWordRevIter;
  }

  // If we didn't find the first word in the method's name at all, we're
  // done.
  if (classWordRevIter == classWordRevIterEnd)
    return DeclName();

  // Now, match from the first word up until the end of the class.
  auto classWordIter = classWordRevIter.base(),
  classWordIterEnd = classWords.end();
  ++methodWordIter;
  while (classWordIter != classWordIterEnd &&
         methodWordIter != methodWordIterEnd &&
         camel_case::sameWordIgnoreFirstCase(*classWordIter,
                                             *methodWordIter)) {
    ++classWordIter;
    ++methodWordIter;
  }

  // If we didn't reach the end of the class name, don't match.
  if (classWordIter != classWordIterEnd)
    return DeclName();

  // We found the chopping point. Form the first argument name.
  llvm::SmallString<32> scratch;
  SmallVector<Identifier, 4> argumentNames;
  argumentNames.push_back(
    importArgName(SwiftContext,
                  splitSelectorPieceAt(firstPieceStr,
                                       methodWordIter.getPosition(),
                                       scratch).second));

  // Handle nullary factory methods.
  if (selector.getNumArgs() == 0) {
    if (argumentNames[0].empty())
      return DeclName(SwiftContext, SwiftContext.Id_init, { });

    // FIXME: Fake up an empty tuple here?
    return DeclName();
  }

  // Map the remaining selector pieces.
  for (auto piece : selector.getSelectorPieces().slice(1)) {
    if (piece.empty())
      argumentNames.push_back(piece);
    else
      argumentNames.push_back(importArgName(SwiftContext, piece.str()));
  }

  return DeclName(SwiftContext, SwiftContext.Id_init, argumentNames);
}

void ClangImporter::Implementation::populateKnownDesignatedInits() {
  if (!KnownDesignatedInits.empty())
    return;

  CreateSelector createSelector(SwiftContext);
#define DESIGNATED_INIT(ClassName, Selector)                            \
  KnownDesignatedInits[#ClassName].push_back(createSelector Selector);
#include "DesignatedInits.def"
}

bool ClangImporter::Implementation::hasDesignatedInitializers(
       const clang::ObjCInterfaceDecl *classDecl) {
  if (classDecl->hasDesignatedInitializers())
    return true;

  populateKnownDesignatedInits();
  return KnownDesignatedInits.count(classDecl->getName());
}

bool ClangImporter::Implementation::isDesignatedInitializer(
       const clang::ObjCInterfaceDecl *classDecl,
       const clang::ObjCMethodDecl *method) {
  // If the information is on the AST, use it.
  if (classDecl->hasDesignatedInitializers())
    return method->hasAttr<clang::ObjCDesignatedInitializerAttr>();

  populateKnownDesignatedInits();
  auto known = KnownDesignatedInits.find(classDecl->getName());
  assert(known != KnownDesignatedInits.end() && 
         "Check hasDesignatedInitializers() first!");
  return std::find(known->second.begin(), known->second.end(),
                   importSelector(method->getSelector()))
           != known->second.end();
}

#pragma mark Name lookup
void ClangImporter::lookupValue(Identifier name, VisibleDeclConsumer &consumer){
  auto &pp = Impl.Instance->getPreprocessor();
  auto &sema = Impl.Instance->getSema();

  // If the given name matches one of a special set of renamed
  // protocol names, perform protocol lookup. For example, the
  // NSObject protocol is named NSObjectProto so that it does not
  // conflict with the NSObject class.
  // FIXME: It would be better to put protocols into a submodule, so
  // that normal name lookup would prefer the class (NSObject) but
  // protocols would be visible with, e.g., protocols.NSObject.
  auto lookupNameKind = clang::Sema::LookupOrdinaryName;
  if (false) { }
#define RENAMED_PROTOCOL(ObjCName, SwiftName)             \
  else if (name.str().equals(#SwiftName)) {               \
    name = Impl.SwiftContext.getIdentifier(#ObjCName);    \
    lookupNameKind = clang::Sema::LookupObjCProtocolName; \
  }
#include "swift/ClangImporter/RenamedProtocols.def"

  // Map the name. If we can't represent the Swift name in Clang, bail out now.
  auto clangName = Impl.importName(name);
  if (!clangName)
    return;
  
  // See if there's a preprocessor macro we can import by this name.
  clang::IdentifierInfo *clangID = clangName.getAsIdentifierInfo();
  if (clangID && clangID->hasMacroDefinition()) {
    if (auto clangMacro = pp.getMacroInfo(clangID)) {
      if (auto valueDecl = Impl.importMacro(name, clangMacro)) {
        consumer.foundDecl(valueDecl, DeclVisibilityKind::VisibleAtTopLevel);
      }
    }
  }

  // Perform name lookup into the global scope.
  // FIXME: Map source locations over.
  clang::LookupResult lookupResult(sema, clangName, clang::SourceLocation(),
                                   lookupNameKind);
  bool FoundType = false;
  bool FoundAny = false;
  if (sema.LookupName(lookupResult, /*Scope=*/0)) {
    // FIXME: Filter based on access path? C++ access control?
    for (auto decl : lookupResult) {
      if (auto swiftDecl = Impl.importDeclReal(decl->getUnderlyingDecl()))
        if (auto valueDecl = dyn_cast<ValueDecl>(swiftDecl)) {
          // If the importer gave us a declaration from the stdlib, make sure
          // it does not show up in the lookup results for the imported module.
          if (valueDecl->getDeclContext()->isModuleScopeContext() &&
              valueDecl->getModuleContext() == Impl.getStdlibModule())
            continue;

          consumer.foundDecl(valueDecl, DeclVisibilityKind::VisibleAtTopLevel);
          FoundType = FoundType || isa<TypeDecl>(valueDecl);
          FoundAny = true;
        }
    }
  }

  if (lookupNameKind == clang::Sema::LookupOrdinaryName && !FoundType) {
    // Look up a tag name if we did not find a type with this name already.
    // We don't want to introduce multiple types with same name.
    lookupResult.clear(clang::Sema::LookupTagName);
    if (sema.LookupName(lookupResult, /*Scope=*/0)) {
      // FIXME: Filter based on access path? C++ access control?
      for (auto decl : lookupResult) {
        if (auto swiftDecl = Impl.importDeclReal(decl->getUnderlyingDecl()))
          if (auto valueDecl = dyn_cast<ValueDecl>(swiftDecl)) {
            consumer.foundDecl(valueDecl, DeclVisibilityKind::VisibleAtTopLevel);
            FoundAny = true;
          }
      }
    }
  }

  if (lookupNameKind == clang::Sema::LookupOrdinaryName && !FoundAny) {
    // Look up a protocol name if we did not find anything with this
    // name already.
    lookupResult.clear(clang::Sema::LookupObjCProtocolName);
    if (sema.LookupName(lookupResult, /*Scope=*/0)) {
      // FIXME: Filter based on access path? C++ access control?
      for (auto decl : lookupResult) {
        if (auto swiftDecl = Impl.importDeclReal(decl->getUnderlyingDecl()))
          if (auto valueDecl = dyn_cast<ValueDecl>(swiftDecl))
            consumer.foundDecl(valueDecl, DeclVisibilityKind::VisibleAtTopLevel);
      }
    }
  }
}

static bool isDeclaredInModule(const ClangModuleUnit *ModuleFilter,
                               const Decl *VD) {
  auto ContainingUnit = VD->getDeclContext()->getModuleScopeContext();
  return ModuleFilter == ContainingUnit;
}

static const clang::Module *getClangOwningModule(ClangNode Node,
                                            const clang::ASTContext &ClangCtx) {
  auto ExtSource = ClangCtx.getExternalSource();
  assert(ExtSource);
  if (const clang::Decl *D = Node.getAsDecl())
    return ExtSource->getModule(D->getOwningModuleID());
  if (const clang::MacroInfo *MI = Node.getAsMacro())
    return ExtSource->getModule(MI->getOwningModuleID());

  return nullptr;
}

static bool isVisibleFromModule(const ClangModuleUnit *ModuleFilter,
                                const ValueDecl *VD) {
  // Include a value from module X if:
  // * no particular module was requested, or
  // * module X was specifically requested.
  if (!ModuleFilter)
    return true;

  auto ContainingUnit = VD->getDeclContext()->getModuleScopeContext();
  if (ModuleFilter == ContainingUnit)
    return true;

  auto Wrapper = dyn_cast<ClangModuleUnit>(ContainingUnit);
  if (!Wrapper)
    return false;

  auto ClangNode = VD->getClangNode();
  assert(ClangNode);

  auto OwningClangModule = getClangOwningModule(ClangNode,
                                            ModuleFilter->getClangASTContext());

  // FIXME: This only triggers for implicitly-generated decls like
  // __builtin_va_list, which we probably shouldn't be importing anyway.
  // But it also includes the builtin declarations for 'id', 'Class', 'SEL',
  // and '__int128_t'.
  if (!OwningClangModule)
    return true;

  // FIXME: If this is in another module, we shouldn't really be considering
  // it here, but the recursive lookup through exports doesn't seem to be
  // working right yet.
  return ModuleFilter->getClangModule()->isModuleVisible(OwningClangModule);
}


namespace {
class ClangVectorDeclConsumer : public clang::VisibleDeclConsumer {
  std::vector<clang::NamedDecl *> results;
public:
  ClangVectorDeclConsumer() = default;

  void FoundDecl(clang::NamedDecl *ND, clang::NamedDecl *Hiding,
                 clang::DeclContext *Ctx, bool InBaseClass) override {
    if (!ND->getIdentifier())
      return;

    if (ND->isModulePrivate())
      return;

    results.push_back(ND);
  }

  llvm::MutableArrayRef<clang::NamedDecl *> getResults() {
    return results;
  }
};

class FilteringVisibleDeclConsumer : public swift::VisibleDeclConsumer {
  swift::VisibleDeclConsumer &NextConsumer;
  const ClangModuleUnit *ModuleFilter = nullptr;

public:
  FilteringVisibleDeclConsumer(swift::VisibleDeclConsumer &consumer,
                               const ClangModuleUnit *CMU)
      : NextConsumer(consumer), ModuleFilter(CMU) {}

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    if (isVisibleFromModule(ModuleFilter, VD))
      NextConsumer.foundDecl(VD, Reason);
  }
};

class FilteringDeclaredDeclConsumer : public swift::VisibleDeclConsumer {
  swift::VisibleDeclConsumer &NextConsumer;
  SmallVectorImpl<ExtensionDecl *> &ExtensionResults;
  const ClangModuleUnit *ModuleFilter = nullptr;

public:
  FilteringDeclaredDeclConsumer(swift::VisibleDeclConsumer &consumer,
                             SmallVectorImpl<ExtensionDecl *> &ExtensionResults,
                                const ClangModuleUnit *CMU)
      : NextConsumer(consumer),
        ExtensionResults(ExtensionResults),
        ModuleFilter(CMU) {}

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    if (isDeclaredInModule(ModuleFilter, VD)) {
      NextConsumer.foundDecl(VD, Reason);
      return;
    }
    // Also report the extensions of the module that extend types from another
    // module.
    if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
      for (auto Ext : NTD->getExtensions()) {
        if (isDeclaredInModule(ModuleFilter, Ext))
          ExtensionResults.push_back(Ext);
      }
    }
  }
};

} // unnamed namespace

void ClangImporter::lookupVisibleDecls(VisibleDeclConsumer &Consumer) const {
  if (Impl.CurrentCacheState != Implementation::CacheState::Valid) {
    do {
      Impl.CurrentCacheState = Implementation::CacheState::InProgress;
      Impl.CachedVisibleDecls.clear();

      ClangVectorDeclConsumer clangConsumer;
      auto &sema = Impl.getClangSema();
      sema.LookupVisibleDecls(sema.getASTContext().getTranslationUnitDecl(),
                              clang::Sema::LookupNameKind::LookupAnyName,
                              clangConsumer);

      // Sort all the Clang decls we find, so that we process them
      // deterministically. This *shouldn't* be necessary, but the importer
      // definitely still has ordering dependencies.
      auto results = clangConsumer.getResults();
      llvm::array_pod_sort(results.begin(), results.end(),
                           [](clang::NamedDecl * const *lhs,
                              clang::NamedDecl * const *rhs) -> int {
        return clang::DeclarationName::compare((*lhs)->getDeclName(),
                                               (*rhs)->getDeclName());
      });

      for (const clang::NamedDecl *clangDecl : results) {
        if (Impl.CurrentCacheState != Implementation::CacheState::InProgress)
          break;
        if (Decl *imported = Impl.importDeclReal(clangDecl))
          Impl.CachedVisibleDecls.push_back(cast<ValueDecl>(imported));
      }
      
      // If we changed things /while/ we were caching, we need to start over
      // and try again. Fortunately we record a map of decls we've already
      // imported, so most of the work is just the lookup and then going
      // through the list.
    } while (Impl.CurrentCacheState != Implementation::CacheState::InProgress);

    auto &ClangPP = Impl.getClangPreprocessor();
    for (auto I = ClangPP.macro_begin(), E = ClangPP.macro_end(); I != E; ++I) {
      if (!I->first->hasMacroDefinition())
        continue;
      auto Name = Impl.importName(I->first);
      if (Name.empty())
        continue;
      if (auto *Imported =
              Impl.importMacro(Name, I->second->getMacroInfo())) {
        Impl.CachedVisibleDecls.push_back(Imported);
      }
    }

    Impl.CurrentCacheState = Implementation::CacheState::Valid;
  }

  for (auto VD : Impl.CachedVisibleDecls)
    Consumer.foundDecl(VD, DeclVisibilityKind::VisibleAtTopLevel);
}

void ClangModuleUnit::lookupVisibleDecls(Module::AccessPathTy AccessPath,
                                         VisibleDeclConsumer &Consumer,
                                         NLKind LookupKind) const {
  FilteringVisibleDeclConsumer filterConsumer(Consumer, this);
  owner.lookupVisibleDecls(filterConsumer);
}

namespace {
class VectorDeclPtrConsumer : public swift::VisibleDeclConsumer {
public:
  SmallVectorImpl<Decl *> &Results;
  explicit VectorDeclPtrConsumer(SmallVectorImpl<Decl *> &Decls)
    : Results(Decls) {}

  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    Results.push_back(VD);
  }
};
} // unnamed namespace

void ClangModuleUnit::getTopLevelDecls(SmallVectorImpl<Decl*> &results) const {
  VectorDeclPtrConsumer Consumer(results);
  SmallVector<ExtensionDecl *, 16> Extensions;
  FilteringDeclaredDeclConsumer FilterConsumer(Consumer, Extensions, this);
  owner.lookupVisibleDecls(FilterConsumer);
  results.append(Extensions.begin(), Extensions.end());
}

static void getImportDecls(ClangModuleUnit *ClangUnit, clang::Module *M,
                           SmallVectorImpl<Decl *> &Results) {
  SmallVector<clang::Module *, 1> Exported;
  M->getExportedModules(Exported);

  ASTContext &Ctx = ClangUnit->getASTContext();

  for (auto *ImportedMod : M->Imports) {
    SmallVector<std::pair<swift::Identifier, swift::SourceLoc>, 4> AccessPath;
    auto *TmpMod = ImportedMod;
    while (TmpMod) {
      AccessPath.push_back({ Ctx.getIdentifier(TmpMod->Name), SourceLoc() });
      TmpMod = TmpMod->Parent;
    }
    std::reverse(AccessPath.begin(), AccessPath.end());

    bool IsExported = false;
    for (auto *ExportedMod : Exported) {
      if (ImportedMod == ExportedMod) {
        IsExported = true;
        break;
      }
    }

    auto *ID = ImportDecl::create(Ctx, ClangUnit, SourceLoc(),
                                  ImportKind::Module, SourceLoc(), AccessPath);
    if (IsExported)
      ID->getMutableAttrs().add(new (Ctx) ExportedAttr(/*IsImplicit=*/false));
    Results.push_back(ID);
    ID->setClangNode(ImportedMod);
  }
}

void ClangModuleUnit::getDisplayDecls(SmallVectorImpl<Decl*> &results) const {
  getImportDecls(const_cast<ClangModuleUnit *>(this), clangModule, results);
  getTopLevelDecls(results);
}

void ClangModuleUnit::lookupValue(Module::AccessPathTy accessPath,
                                  DeclName name, NLKind lookupKind,
                                  SmallVectorImpl<ValueDecl*> &results) const {
  if (!Module::matchesAccessPath(accessPath, name))
    return;
  
  // There should be no multi-part top-level decls in a Clang module.
  if (!name.isSimpleName())
    return;
  
  VectorDeclConsumer vectorWriter(results);
  FilteringVisibleDeclConsumer filteringConsumer(vectorWriter, this);
  
  owner.lookupValue(name.getBaseName(), filteringConsumer);
}

void ClangImporter::loadExtensions(NominalTypeDecl *nominal,
                                   unsigned previousGeneration) {
  auto objcClass = dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                     nominal->getClangDecl());
  if (!objcClass) {
    if (auto typeResolver = Impl.getTypeResolver())
      typeResolver->resolveDeclSignature(nominal);
    if (nominal->isObjC()) {
      // Map the name. If we can't represent the Swift name in Clang, bail out
      // now.
      auto clangName = Impl.importName(nominal->getName());
      if (!clangName)
        return;

      auto &sema = Impl.Instance->getSema();

      // Perform name lookup into the global scope.
      // FIXME: Map source locations over.
      clang::LookupResult lookupResult(sema, clangName,
                                       clang::SourceLocation(),
                                       clang::Sema::LookupOrdinaryName);
      if (sema.LookupName(lookupResult, /*Scope=*/0)) {
        // FIXME: Filter based on access path? C++ access control?
        for (auto clangDecl : lookupResult) {
          objcClass = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl);
          if (objcClass)
            break;
        }
      }
    }
  }

  if (!objcClass)
    return;

  // Import all of the visible categories. Simply loading them adds them to
  // the list of extensions.
  for (auto I = objcClass->visible_categories_begin(),
            E = objcClass->visible_categories_end();
       I != E; ++I) {
    Impl.importDeclReal(*I);
  }
}

void ClangModuleUnit::getImportedModules(
    SmallVectorImpl<Module::ImportedModule> &imports,
    Module::ImportFilter filter) const {

  auto topLevelAdapter = getAdapterModule();

  SmallVector<clang::Module *, 8> imported;
  if (filter == Module::ImportFilter::Public) {
    clangModule->getExportedModules(imported);
  } else {
    if (filter == Module::ImportFilter::All) {
      imported.append(clangModule->Imports.begin(), clangModule->Imports.end());
    } else {
      SmallVector<clang::Module *, 8> publicImports;
      clangModule->getExportedModules(publicImports);
      std::copy_if(clangModule->Imports.begin(), clangModule->Imports.end(),
                   std::back_inserter(imported), [&](clang::Module *mod) {
        return publicImports.end() != std::find(publicImports.begin(),
                                                publicImports.end(),
                                                mod);
      });
    }

    // FIXME: The parent module isn't exactly a private import, but it is
    // needed for link dependencies.
    if (clangModule->Parent)
      imported.push_back(clangModule->Parent);
  }

  for (auto importMod : imported) {
    auto wrapper = owner.Impl.getWrapperForModule(owner, importMod);

    auto actualMod = wrapper->getAdapterModule();
    if (!actualMod || actualMod == topLevelAdapter)
      actualMod = wrapper->getParentModule();

    imports.push_back({Module::AccessPathTy(), actualMod});
  }

  if (filter != Module::ImportFilter::Public)
    imports.push_back({Module::AccessPathTy(),
                       getASTContext().getStdlibModule()});
}

/// Returns true if the first selector piece matches the given identifier.
static bool selectorMatchesName(clang::Selector sel, DeclName name) {
  if (name.isSimpleName())
    return sel.getNameForSlot(0) == name.getBaseName().str();
  
  if (sel.getNumArgs() != name.getArgumentNames().size() + 1)
    return false;
  if (sel.getNameForSlot(0) != name.getBaseName().str())
    return false;

  for (unsigned i = 1, e = sel.getNumArgs(); i < e; ++i)
    if (sel.getNameForSlot(i) != name.getArgumentNames()[i-1].str())
      return false;
  return true;
}

static void lookupClassMembersImpl(ClangImporter::Implementation &Impl,
                                   VisibleDeclConsumer &consumer,
                                   DeclName name = DeclName()) {
  clang::Sema &S = Impl.getClangSema();
  clang::ExternalASTSource *source = S.getExternalSource();

  // When looking for a subscript, we actually look for the getters
  // and setters.
  clang::IdentifierInfo *objectAtIndexedSubscriptId = nullptr;
  clang::IdentifierInfo *objectForKeyedSubscriptId = nullptr;
  clang::IdentifierInfo *setObjectId = nullptr;
  clang::IdentifierInfo *atIndexedSubscriptId = nullptr;
  clang::IdentifierInfo *forKeyedSubscriptId = nullptr;
  bool isSubscript = name.isSimpleName(Impl.SwiftContext.Id_subscript);
  if (isSubscript) {
    auto &identTable = S.Context.Idents;
    objectAtIndexedSubscriptId = &identTable.get("objectAtIndexedSubscript");
    objectForKeyedSubscriptId = &identTable.get("objectForKeyedSubscript");
    setObjectId = &identTable.get("setObject");
    atIndexedSubscriptId = &identTable.get("atIndexedSubscript");
    forKeyedSubscriptId = &identTable.get("forKeyedSubscript");
  }

  // Function that determines whether the given selector is acceptable.
  auto acceptableSelector = [&](clang::Selector sel) -> bool {
    if (!name)
      return true;

    switch (sel.getNumArgs()) {
    case 0:
      if (isSubscript)
        return false;

      break;

    case 1:
      if (isSubscript)
        return sel.getIdentifierInfoForSlot(0) == objectAtIndexedSubscriptId ||
               sel.getIdentifierInfoForSlot(0) == objectForKeyedSubscriptId;

      break;

    case 2:
      if (isSubscript)
        return (sel.getIdentifierInfoForSlot(0) == setObjectId &&
                (sel.getIdentifierInfoForSlot(1) == atIndexedSubscriptId ||
                 sel.getIdentifierInfoForSlot(1) == forKeyedSubscriptId));

      break;

    default:
      if (isSubscript)
        return false;

      break;
    }

    return selectorMatchesName(sel, name);
  };

  // Force load all external methods.
  // FIXME: Copied from Clang's SemaCodeComplete.
  for (uint32_t i = 0, n = source->GetNumExternalSelectors(); i != n; ++i) {
    clang::Selector sel = source->GetExternalSelector(i);
    if (sel.isNull() || S.MethodPool.count(sel))
      continue;
    if (!acceptableSelector(sel))
      continue;

    S.ReadMethodPool(sel);
  }

  // FIXME: Does not include methods from protocols.
  // FIXME: Do we really have to import every single method?
  // FIXME: Need a more efficient table in Clang to find "all selectors whose
  // first piece is this name".
  auto importMethods = [&](const clang::ObjCMethodList *list) {
    for (; list != nullptr; list = list->getNext()) {
      if (list->Method->isUnavailable())
        continue;

      // If the method is a property accessor, we want the property.
      const clang::NamedDecl *searchForDecl = list->Method;
      if (list->Method->isPropertyAccessor()) {
        if (auto property = list->Method->findPropertyDecl()) {
          // ... unless we are enumerating all decls.  In this case, if we see
          // a getter, return a property.  If we see a setter, we know that
          // there is a getter, and we will visit it and return a property at
          // that time.
          if (!name && list->Method->param_size() != 0)
            continue;
          searchForDecl = property;
        }
      }

      if (auto VD = cast_or_null<ValueDecl>(Impl.importDeclReal(searchForDecl))) {
        if (isSubscript || !name) {
          // When searching for a subscript, we may have found a getter.  If so,
          // use the subscript instead.
          if (auto func = dyn_cast<FuncDecl>(VD)) {
            auto known = Impl.Subscripts.find({func, nullptr});
            if (known != Impl.Subscripts.end()) {
              consumer.foundDecl(known->second, DeclVisibilityKind::DynamicLookup);
            }
          }

          // If we were looking only for subscripts, don't report the getter.
          if (isSubscript)
            continue;
        }

        consumer.foundDecl(VD, DeclVisibilityKind::DynamicLookup);
      }
    }
  };

  for (auto entry : S.MethodPool) {
    if (!acceptableSelector(entry.first))
      continue;
  
    auto &methodListPair = entry.second;
    if (methodListPair.first.Method)
      importMethods(&methodListPair.first);
    if (methodListPair.second.Method)
      importMethods(&methodListPair.second);
  }
}

void
ClangModuleUnit::lookupClassMember(Module::AccessPathTy accessPath,
                                   DeclName name,
                                   SmallVectorImpl<ValueDecl*> &results) const {
  // FIXME: Not limited by module.
  VectorDeclConsumer consumer(results);
  lookupClassMembersImpl(owner.Impl, consumer, name);
}

void ClangModuleUnit::lookupClassMembers(Module::AccessPathTy accessPath,
                                         VisibleDeclConsumer &consumer) const {
  // FIXME: Not limited by module.
  lookupClassMembersImpl(owner.Impl, consumer);
}

void ClangModuleUnit::collectLinkLibraries(
    Module::LinkLibraryCallback callback) const {
  for (auto clangLinkLib : clangModule->LinkLibraries) {
    LibraryKind kind;
    if (clangLinkLib.IsFramework)
      kind = LibraryKind::Framework;
    else
      kind = LibraryKind::Library;
    
    callback(LinkLibrary(clangLinkLib.Library, kind));
  }
}

StringRef ClangModuleUnit::getFilename() const {
  return clangModule->getASTFile()->getName();
}

clang::TargetInfo &ClangImporter::getTargetInfo() const {
  return Impl.Instance->getTarget();
}

clang::ASTContext &ClangImporter::getClangASTContext() const {
  return Impl.getClangASTContext();
}

clang::Preprocessor &ClangImporter::getClangPreprocessor() const {
  return Impl.getClangPreprocessor();
}
const clang::Module *ClangImporter::getClangOwningModule(ClangNode Node) const {
  return ::getClangOwningModule(Node, getClangASTContext());
}

std::string ClangImporter::getClangModuleHash() const {
  return Impl.Invocation->getModuleHash();
}

void ClangImporter::verifyAllModules() {
#ifndef NDEBUG
  if (Impl.ImportCounter == Impl.VerifiedImportCounter)
    return;

  // Collect the Decls before verifying them; the act of verifying may cause
  // more decls to be imported and modify the map while we are iterating it.
  SmallVector<Decl *, 8> Decls;
  for (auto &I : Impl.ImportedDecls)
    if (Decl *D = I.second)
      Decls.push_back(D);

  for (auto D : Decls)
    verify(D);

  Impl.VerifiedImportCounter = Impl.ImportCounter;
#endif
}

//===----------------------------------------------------------------------===//
// ClangModule Implementation
//===----------------------------------------------------------------------===//

ClangModuleUnit::ClangModuleUnit(Module &M, ClangImporter &owner,
                                 clang::Module *clangModule)
  : LoadedFile(FileUnitKind::ClangModule, M), owner(owner),
    clangModule(clangModule) {
}

bool ClangModuleUnit::hasClangModule(Module *M) {
  for (auto F : M->getFiles()) {
    if (isa<ClangModuleUnit>(F))
      return true;
  }
  return false;
}

bool ClangModuleUnit::isTopLevel() const {
  return !clangModule->isSubModule();
}

bool ClangModuleUnit::isSystemModule() const {
  return clangModule->IsSystem;
}

clang::ASTContext &ClangModuleUnit::getClangASTContext() const {
  return owner.getClangASTContext();
}

Module *ClangModuleUnit::getAdapterModule() const {
  if (!isTopLevel()) {
    // FIXME: Is this correct for submodules?
    auto topLevel = clangModule->getTopLevelModule();
    auto wrapper = owner.Impl.getWrapperForModule(owner, topLevel);
    return wrapper->getAdapterModule();

  }

  if (!adapterModule.getInt()) {
    // FIXME: Include proper source location.
    Module *M = getParentModule();
    ASTContext &Ctx = M->Ctx;
    auto adapter = Ctx.getModule(Module::AccessPathTy({M->Name, SourceLoc()}));
    if (adapter == M) {
      adapter = nullptr;
    } else {
      auto &sharedModuleRef = Ctx.LoadedModules[M->Name.str()];
      assert(!sharedModuleRef || sharedModuleRef == adapter ||
             sharedModuleRef == M);
      sharedModuleRef = adapter;
    }

    auto mutableThis = const_cast<ClangModuleUnit *>(this);
    mutableThis->adapterModule.setPointerAndInt(adapter, true);
  }

  return adapterModule.getPointer();
}
