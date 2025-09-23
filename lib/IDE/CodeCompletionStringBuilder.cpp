//===--- CodeCompletionStringBuilder.cpp --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CodeCompletionStringBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/Types.h"
#include "swift/Basic/StringExtras.h"
#include "swift/IDE/CodeCompletionStringPrinter.h"
#include "swift/Parse/Lexer.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::ide;

Type ide::eraseArchetypes(Type type, GenericSignature genericSig) {
  if (auto *genericFuncType = type->getAs<GenericFunctionType>()) {
    assert(genericFuncType->getGenericSignature()->isEqual(genericSig) &&
           "if not, just use the GFT's signature instead below");

    SmallVector<AnyFunctionType::Param, 8> erasedParams;
    for (const auto &param : genericFuncType->getParams()) {
      auto erasedTy = eraseArchetypes(param.getPlainType(), genericSig);
      erasedParams.emplace_back(param.withType(erasedTy));
    }
    return GenericFunctionType::get(
        genericSig, erasedParams,
        eraseArchetypes(genericFuncType->getResult(), genericSig),
        genericFuncType->getExtInfo());
  }

  return type.transformRec([&](Type t) -> std::optional<Type> {
    // FIXME: Code completion should only deal with one or the other,
    // and not both.
    if (auto *archetypeType = t->getAs<ArchetypeType>()) {
      // Don't erase opaque archetype.
      if (isa<OpaqueTypeArchetypeType>(archetypeType) &&
          archetypeType->isRoot())
        return std::nullopt;

      auto genericSig =
          archetypeType->getGenericEnvironment()->getGenericSignature();
      auto upperBound =
          genericSig->getUpperBound(archetypeType->getInterfaceType(),
                                    /*forExistentialSelf=*/false,
                                    /*withParameterizedProtocols=*/false);

      if (!upperBound->isAny())
        return upperBound;
    }

    if (t->isTypeParameter() && genericSig) {
      auto upperBound =
          genericSig->getUpperBound(t,
                                    /*forExistentialSelf=*/false,
                                    /*withParameterizedProtocols=*/false);

      if (!upperBound->isAny())
        return upperBound;
    }

    return std::nullopt;
  });
}

/// Return whether \p param has a non-desirable default value for code
/// completion.
///
/// 'ClangImporter::Implementation::inferDefaultArgument()' automatically adds
/// default values for some parameters;
///   * NS_OPTIONS enum type with the name '...Options'.
///   * NSDictionary and labeled 'options', 'attributes', or 'userInfo'.
///
/// But sometimes, this behavior isn't really desirable. This function add a
/// heuristic where if a parameter matches all the following condition, we
/// consider the imported default value is _not_ desirable:
///   * it is the first parameter,
///   * it doesn't have an argument label, and
///   * the imported function base name ends with those words
/// For example, ClangImporter imports:
///
///   -(void)addAttributes:(NSDictionary *)attrs, options:(NSDictionary *)opts;
///
/// as:
///
///   func addAttributes(_ attrs: [AnyHashable:Any] = [:],
///                      options opts: [AnyHashable:Any] = [:])
///
/// In this case, we don't want 'attrs' defaulted because the function name have
/// 'Attribute' in its name so calling 'value.addAttribute()' doesn't make
/// sense, but we _do_ want to keep 'opts' defaulted.
///
/// Note that:
///
///   -(void)performWithOptions:(NSDictionary *) opts;
///
/// This doesn't match the condition because the base name of the function in
/// Swift is 'peform':
///
///   func perform(options opts: [AnyHashable:Any] = [:])
///
static bool isNonDesirableImportedDefaultArg(const ParamDecl *param) {
  auto kind = param->getDefaultArgumentKind();
  if (kind != DefaultArgumentKind::EmptyArray &&
      kind != DefaultArgumentKind::EmptyDictionary)
    return false;

  if (!param->getArgumentName().empty())
    return false;

  auto *func = dyn_cast<FuncDecl>(param->getDeclContext());
  if (!func->hasClangNode())
    return false;
  if (func->getParameters()->front() != param)
    return false;
  if (func->getBaseName().isSpecial())
    return false;

  auto baseName = func->getBaseName().getIdentifier().str();
  switch (kind) {
  case DefaultArgumentKind::EmptyArray:
    return (baseName.ends_with("Options"));
  case DefaultArgumentKind::EmptyDictionary:
    return (baseName.ends_with("Options") || baseName.ends_with("Attributes") ||
            baseName.ends_with("UserInfo"));
  default:
    llvm_unreachable("unhandled DefaultArgumentKind");
  }
}

bool swift::ide::hasInterestingDefaultValue(const ParamDecl *param) {
  if (!param)
    return false;

  switch (param->getDefaultArgumentKind()) {
  case DefaultArgumentKind::Normal:
  case DefaultArgumentKind::NilLiteral:
  case DefaultArgumentKind::StoredProperty:
  case DefaultArgumentKind::Inherited:
    return true;

  case DefaultArgumentKind::EmptyArray:
  case DefaultArgumentKind::EmptyDictionary:
    if (isNonDesirableImportedDefaultArg(param))
      return false;
    return true;

  case DefaultArgumentKind::None:
#define MAGIC_IDENTIFIER(NAME, STRING) case DefaultArgumentKind::NAME:
#include "swift/AST/MagicIdentifierKinds.def"
  case DefaultArgumentKind::ExpressionMacro:
    return false;
  }
}

void CodeCompletionStringBuilder::addChunkWithText(
    CodeCompletionString::Chunk::ChunkKind Kind, StringRef Text) {
  addChunkWithTextNoCopy(Kind, Text.copy(Allocator));
}

StringRef CodeCompletionStringBuilder::escapeKeyword(
    StringRef Word, bool escapeAllKeywords,
    llvm::SmallString<16> &EscapedKeyword) {
  EscapedKeyword.clear();
  bool shouldEscape = false;
  if (escapeAllKeywords) {
#define KEYWORD(kw) .Case(#kw, true)
    shouldEscape = llvm::StringSwitch<bool>(Word)
#include "swift/AST/TokenKinds.def"
                       .Default(Lexer::identifierMustAlwaysBeEscaped(Word));
  } else {
    shouldEscape =
        !canBeArgumentLabel(Word) || Lexer::identifierMustAlwaysBeEscaped(Word);
  }

  if (!shouldEscape)
    return Word;

  return escapeWithBackticks(Word, EscapedKeyword);
}

void CodeCompletionStringBuilder::withNestedGroup(
    CodeCompletionString::Chunk::ChunkKind Kind,
    llvm::function_ref<void()> body) {
  ++CurrentNestingLevel;
  addSimpleChunk(Kind);
  body();
  --CurrentNestingLevel;
}

void CodeCompletionStringBuilder::addCallArgument(
    Identifier Name, Identifier LocalName, Type Ty, Type ContextTy,
    bool IsVarArg, bool IsInOut, bool IsIUO, bool IsAutoClosure,
    bool IsLabeledTrailingClosure, bool IsForOperator, bool HasDefault,
    StringRef DefaultValue) {
  ++CurrentNestingLevel;
  using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

  addSimpleChunk(ChunkKind::CallArgumentBegin);

  if (AnnotateResults) {
    llvm::SmallString<16> EscapedKeyword;
    if (!Name.empty()) {
      addChunkWithText(ChunkKind::CallArgumentName,
                       escapeKeyword(Name.str(), false, EscapedKeyword));
      if (!LocalName.empty() && Name != LocalName) {
        addChunkWithTextNoCopy(ChunkKind::Text, " ");
        getLastChunk().setIsAnnotation();
        addChunkWithText(ChunkKind::CallArgumentInternalName,
                         escapeKeyword(LocalName.str(), false, EscapedKeyword));
        getLastChunk().setIsAnnotation();
      }
      addChunkWithTextNoCopy(ChunkKind::CallArgumentColon, ": ");
    } else if (!LocalName.empty()) {
      addChunkWithTextNoCopy(ChunkKind::CallArgumentName, "_");
      getLastChunk().setIsAnnotation();
      addChunkWithTextNoCopy(ChunkKind::Text, " ");
      getLastChunk().setIsAnnotation();
      addChunkWithText(ChunkKind::CallArgumentInternalName,
                       escapeKeyword(LocalName.str(), false, EscapedKeyword));
      addChunkWithTextNoCopy(ChunkKind::CallArgumentColon, ": ");
    } else if (!IsForOperator) {
      addChunkWithTextNoCopy(ChunkKind::CallArgumentName, "_");
      if (!IsLabeledTrailingClosure)
        getLastChunk().setIsAnnotation();
      addChunkWithTextNoCopy(ChunkKind::CallArgumentColon, ": ");
      if (!IsLabeledTrailingClosure)
        getLastChunk().setIsAnnotation();
    }
  } else {
    llvm::SmallString<16> stash;
    ChunkKind nameKind;
    StringRef nameStr;
    if (!Name.empty()) {
      nameKind = ChunkKind::CallArgumentName;
      nameStr = escapeKeyword(Name.str(), false, stash);
    } else if (IsLabeledTrailingClosure) {
      nameKind = ChunkKind::CallArgumentName;
      nameStr = "_";
    } else if (!LocalName.empty()) {
      nameKind = ChunkKind::CallArgumentInternalName;
      nameStr = escapeKeyword(LocalName.str(), false, stash);
    }
    if (!nameStr.empty()) {
      if (UnderscoreEmptyArgumentLabel &&
          nameKind == ChunkKind::CallArgumentInternalName) {
        addChunkWithTextNoCopy(ChunkKind::CallArgumentName, "_");
        addChunkWithTextNoCopy(ChunkKind::Text, " ");
      }

      addChunkWithText(nameKind, nameStr);
      addChunkWithTextNoCopy(ChunkKind::CallArgumentColon, ": ");
    }
  }

  // 'inout' arguments are printed specially.
  if (IsInOut) {
    if (FullParameterFlags) {
      addChunkWithTextNoCopy(ChunkKind::Keyword, "inout");
      addChunkWithTextNoCopy(ChunkKind::Text, " ");
    } else {
      addChunkWithTextNoCopy(ChunkKind::Ampersand, "&");
    }
    Ty = Ty->getInOutObjectType();
  }

  // If the parameter is of the type @autoclosure ()->output, then the
  // code completion should show the parameter of the output type
  // instead of the function type ()->output.
  if (IsAutoClosure) {
    // 'Ty' may be ErrorType.
    if (auto funcTy = Ty->getAs<FunctionType>())
      Ty = funcTy->getResult();
  }

  NonRecursivePrintOptions nrOptions;
  if (IsIUO)
    nrOptions |= NonRecursivePrintOption::ImplicitlyUnwrappedOptional;

  PrintOptions PO;
  PO.SkipAttributes = true;
  PO.OpaqueReturnTypePrinting =
      PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword;
  if (ContextTy)
    PO.setBaseType(ContextTy);
  if (AnnotateResults) {
    withNestedGroup(ChunkKind::CallArgumentTypeBegin, [&]() {
      CodeCompletionStringPrinter printer(*this);
      auto TL = TypeLoc::withoutLoc(Ty);
      printer.printTypePre(TL);
      Ty->print(printer, PO, nrOptions);
      printer.printTypePost(TL);
    });
  } else {
    std::string TypeName = Ty->getString(PO, nrOptions);
    addChunkWithText(ChunkKind::CallArgumentType, TypeName);
  }

  if (HasDefault) {
    withNestedGroup(ChunkKind::CallArgumentDefaultBegin, [&]() {
      if (DefaultValue.empty())
        return;

      addWhitespace(" ");
      addEqual();
      addWhitespace(" ");
      addChunkWithText(ChunkKind::Text, DefaultValue);
    });
  }

  // Look through optional types and type aliases to find out if we have
  // function type.
  Ty = Ty->lookThroughAllOptionalTypes();
  if (auto AFT = Ty->getAs<AnyFunctionType>()) {
    // If this is a closure type, add ChunkKind::CallArgumentClosureType or
    // ChunkKind::CallArgumentClosureExpr for labeled trailing closures.
    PrintOptions PO;
    PO.PrintFunctionRepresentationAttrs =
        PrintOptions::FunctionRepresentationMode::None;
    PO.SkipAttributes = true;
    PO.OpaqueReturnTypePrinting =
        PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword;
    PO.AlwaysTryPrintParameterLabels = true;
    if (ContextTy)
      PO.setBaseType(ContextTy);

    if (IsLabeledTrailingClosure) {
      // Expand the closure body.
      SmallString<32> buffer;
      llvm::raw_svector_ostream OS(buffer);

      bool firstParam = true;
      for (const auto &param : AFT->getParams()) {
        if (!firstParam)
          OS << ", ";
        firstParam = false;

        if (param.hasLabel()) {
          OS << param.getLabel();
        } else if (param.hasInternalLabel()) {
          OS << param.getInternalLabel();
        } else {
          OS << "<#";
          if (param.isInOut())
            OS << "inout ";
          OS << param.getPlainType()->getString(PO);
          if (param.isVariadic())
            OS << "...";
          OS << "#>";
        }
      }

      if (!firstParam)
        OS << " in";

      addChunkWithText(
          CodeCompletionString::Chunk::ChunkKind::CallArgumentClosureExpr,
          OS.str());
    } else {
      // Add the closure type.
      addChunkWithText(
          CodeCompletionString::Chunk::ChunkKind::CallArgumentClosureType,
          AFT->getString(PO));
    }
  }

  if (IsVarArg)
    addEllipsis();

  --CurrentNestingLevel;
}

void CodeCompletionStringBuilder::addTypeAnnotation(
    Type T, const PrintOptions &PO, NonRecursivePrintOptions nrOptions,
    StringRef suffix) {
  T = T->getReferenceStorageReferent();

  // Replace '()' with 'Void'.
  if (T->isVoid())
    T = T->getASTContext().getVoidDecl()->getDeclaredInterfaceType();

  if (AnnotateResults) {
    withNestedGroup(CodeCompletionString::Chunk::ChunkKind::TypeAnnotationBegin,
                    [&]() {
                      CodeCompletionStringPrinter printer(*this);
                      auto TL = TypeLoc::withoutLoc(T);
                      printer.printTypePre(TL);
                      T->print(printer, PO, nrOptions);
                      printer.printTypePost(TL);
                      if (!suffix.empty())
                        printer.printText(suffix);
                    });
  } else {
    auto str = T.getString(PO, nrOptions);
    if (!suffix.empty())
      str += suffix.str();
    addTypeAnnotation(str);
  }
}

void CodeCompletionStringBuilder::addValueBaseName(DeclBaseName Name,
                                                   bool IsMember) {
  auto NameStr = Name.userFacingName();
  if (Name.mustAlwaysBeEscaped()) {
    // Names that are raw identifiers must always be escaped regardless of
    // their position.
    SmallString<16> buffer;
    addBaseName(escapeWithBackticks(NameStr, buffer));
    return;
  }

  bool shouldEscapeKeywords;
  if (Name.isSpecial()) {
    // Special names (i.e. 'init') are always displayed as its user facing
    // name.
    shouldEscapeKeywords = false;
  } else if (IsMember) {
    // After dot. User can write any keyword after '.' except for `init` and
    // `self`. E.g. 'func `init`()' must be called by 'expr.`init`()'.
    shouldEscapeKeywords = NameStr == "self" || NameStr == "init";
  } else {
    // As primary expresson. We have to escape almost every keywords except
    // for 'self' and 'Self'.
    shouldEscapeKeywords = NameStr != "self" && NameStr != "Self";
  }

  if (!shouldEscapeKeywords) {
    addBaseName(NameStr);
  } else {
    SmallString<16> buffer;
    addBaseName(escapeKeyword(NameStr, true, buffer));
  }
}

bool CodeCompletionStringBuilder::addCallArgumentPatterns(
    ArrayRef<AnyFunctionType::Param> typeParams,
    ArrayRef<const ParamDecl *> declParams, const DeclContext *DC,
    GenericSignature genericSig, DefaultArgumentOutputMode defaultArgsMode,
    bool includeDefaultValues) {
  assert(declParams.empty() || typeParams.size() == declParams.size());

  bool modifiedBuilder = false;
  bool needComma = false;
  // Iterate over each parameter.
  for (unsigned i = 0; i != typeParams.size(); ++i) {
    auto &typeParam = typeParams[i];

    Identifier argName = typeParam.getLabel();
    Identifier bodyName;
    bool isIUO = false;
    bool hasDefault = false;

    // Scratch for getting default value string representation
    SmallString<32> Scratch;
    StringRef defaultValue;

    if (!declParams.empty()) {
      const ParamDecl *PD = declParams[i];
      hasDefault =
          PD->isDefaultArgument() && !isNonDesirableImportedDefaultArg(PD);

      if (hasDefault) {
        // Skip default arguments if we're either not including them or they
        // aren't interesting
        bool skipDefaultArgument;
        switch (defaultArgsMode) {
        case DefaultArgumentOutputMode::None:
          skipDefaultArgument = true;
          break;
        case DefaultArgumentOutputMode::Interesting:
          skipDefaultArgument = !hasInterestingDefaultValue(PD);
          break;
        case DefaultArgumentOutputMode::All:
          skipDefaultArgument = false;
          break;
        }

        if (skipDefaultArgument)
          continue;
      }

      argName = PD->getArgumentName();
      bodyName = PD->getParameterName();
      isIUO = PD->isImplicitlyUnwrappedOptional();

      if (hasDefault && includeDefaultValues)
        defaultValue = PD->getDefaultValueStringRepresentation(Scratch);
    }

    bool isVariadic = typeParam.isVariadic();
    bool isInOut = typeParam.isInOut();
    bool isAutoclosure = typeParam.isAutoClosure();
    Type paramTy = typeParam.getPlainType();
    if (isVariadic)
      paramTy = ParamDecl::getVarargBaseTy(paramTy);

    Type contextTy;
    if (auto typeContext = DC->getInnermostTypeContext())
      contextTy = typeContext->getDeclaredTypeInContext();

    if (needComma)
      addComma();
    addCallArgument(argName, bodyName, eraseArchetypes(paramTy, genericSig),
                    contextTy, isVariadic, isInOut, isIUO, isAutoclosure,
                    /*IsLabeledTrailingClosure=*/false,
                    /*IsForOperator=*/false, hasDefault, defaultValue);

    modifiedBuilder = true;
    needComma = true;
  }

  return modifiedBuilder;
}

bool CodeCompletionStringBuilder::addCallArgumentPatterns(
    const AnyFunctionType *AFT, const ParameterList *Params,
    const DeclContext *DC, GenericSignature genericSig,
    DefaultArgumentOutputMode defaultArgsMode, bool includeDefaultValues) {
  ArrayRef<const ParamDecl *> declParams;
  if (Params)
    declParams = Params->getArray();
  return addCallArgumentPatterns(AFT->getParams(), declParams, DC, genericSig,
                                 defaultArgsMode, includeDefaultValues);
}

void CodeCompletionStringBuilder::addTypeAnnotation(
    Type T, const DeclContext *DC, GenericSignature genericSig) {
  PrintOptions PO;
  PO.OpaqueReturnTypePrinting =
      PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword;
  if (auto typeContext = DC->getInnermostTypeContext())
    PO.setBaseType(typeContext->getDeclaredTypeInContext());
  addTypeAnnotation(eraseArchetypes(T, genericSig), PO);
}

void CodeCompletionStringBuilder::
    addTypeAnnotationForImplicitlyUnwrappedOptional(Type T,
                                                    const DeclContext *DC,
                                                    GenericSignature genericSig,
                                                    bool dynamicOrOptional) {
  std::string suffix;
  // FIXME: This retains previous behavior, but in reality the type of dynamic
  // lookups is IUO, not Optional as it is for the @optional attribute.
  if (dynamicOrOptional) {
    T = T->getOptionalObjectType();
    suffix = "?";
  }

  NonRecursivePrintOptions nrOptions =
      NonRecursivePrintOption::ImplicitlyUnwrappedOptional;

  PrintOptions PO;
  PO.OpaqueReturnTypePrinting =
      PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword;
  if (auto typeContext = DC->getInnermostTypeContext())
    PO.setBaseType(typeContext->getDeclaredTypeInContext());
  addTypeAnnotation(eraseArchetypes(T, genericSig), PO, nrOptions, suffix);
}

void CodeCompletionStringBuilder::addEffectsSpecifiers(
    const AnyFunctionType *AFT, const AbstractFunctionDecl *AFD,
    bool forceAsync) {
  assert(AFT != nullptr);

  // 'async'.
  if (forceAsync || (AFD && AFD->hasAsync()) ||
      (AFT->hasExtInfo() && AFT->isAsync()))
    addAnnotatedAsync();

  // 'throws' or 'rethrows'.
  if (AFD && AFD->getAttrs().hasAttribute<RethrowsAttr>())
    addAnnotatedRethrows();
  else if (AFT->hasExtInfo() && AFT->isThrowing())
    addAnnotatedThrows();
}
