//===--- TypeCheckAttrABI.cpp - Type Checking for @abi Attribute ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements diagnostics for the @abi attribute.
///
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Effects.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Parse/Lexer.h"
#include <optional>

using namespace swift;

namespace {

/// Like ASTVisitor, but the visit methods are passed a pair of nodes to compare.
/// Either node might be \c nil , indicating that a matching node was not found.
template <typename ImplClass>
class ASTComparisonVisitor {
public:
  bool visit(Decl *D1, Decl *D2) {
    DeclKind kind = D1 ? D1->getKind() : D2->getKind();
    switch (kind) {
#define DECL(CLASS, PARENT) \
    case DeclKind::CLASS: \
      return static_cast<ImplClass*>(this) \
        ->visit##CLASS##Decl(static_cast<CLASS##Decl*>(D1), \
                             static_cast<CLASS##Decl*>(D2));
#include "swift/AST/DeclNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }

#define DECL(CLASS, PARENT) \
  bool visitParentOf##CLASS##Decl(CLASS##Decl *D1, CLASS##Decl *D2) {\
    return static_cast<ImplClass*>(this)->visit##PARENT(D1, D2); \
  }
#define ABSTRACT_DECL(CLASS, PARENT) DECL(CLASS, PARENT)
#include "swift/AST/DeclNodes.def"
};

/// Describes the effects that have been applied to a declaration, packaging up
/// various bits of info used for \c \@abi diagnostics.
struct DeclEffects {
  PossibleEffects effects;
  PossibleEffects polymorphicEffects;

  SourceLoc asyncLoc;
  SourceLoc throwsLoc;

  std::optional<Type> effectiveThrownType;
  TypeRepr *thrownTypeRepr;

  DeclEffects(AbstractFunctionDecl *afd)
    : effects(), polymorphicEffects(),
      asyncLoc(afd->getAsyncLoc()), throwsLoc(afd->getThrowsLoc()),
      effectiveThrownType(afd->getEffectiveThrownErrorType()),
      thrownTypeRepr(afd->getThrownTypeRepr())
  {
    if (afd->hasEffect(EffectKind::Async))
      effects |= EffectKind::Async;
    if (afd->hasPolymorphicEffect(EffectKind::Async))
      polymorphicEffects |= EffectKind::Async;

    if (afd->hasEffect(EffectKind::Throws))
      effects |= EffectKind::Throws;
    if (afd->hasPolymorphicEffect(EffectKind::Throws))
      polymorphicEffects |= EffectKind::Throws;
  }

  bool anyContains(EffectKind effect) const {
    return effects.contains(effect) || polymorphicEffects.contains(effect);
  }
};

/// Describes the relationship between a given type and the declaration it
/// belongs to--e.g. is this its result type? a parameter type? etc. Used with
/// a couple of \c \@abi diagnostics.
class TypeOrigin {
public:
  // Cases must be kept in sync with DiagnosticsSema TYPE_ORIGIN
  enum class Kind : uint8_t {
    Unspecified = 0,
    Parameter = 1,
    SelfParameter = 2,
    Result = 3,
    ThrowsEffect = 4,
  };

private:
  llvm::PointerIntPair<Decl *, 3, Kind> declAndKind;

  TypeOrigin(Decl *decl, Kind kind)
    : declAndKind(decl, kind) {}

public:
  static TypeOrigin forUnspecified() {
    return TypeOrigin(nullptr, Kind::Unspecified);
  }

  static TypeOrigin forParameter(ParamDecl *paramDecl) {
    return TypeOrigin(paramDecl,
                      paramDecl->isSelfParameter() ? Kind::SelfParameter
                                                   : Kind::Parameter);
  }

  static TypeOrigin forResult() {
    return TypeOrigin(nullptr, Kind::Result);
  }

  static TypeOrigin forThrowsEffect() {
    return TypeOrigin(nullptr, Kind::ThrowsEffect);
  }

  Kind getKind() const {
    return declAndKind.getInt();
  }

  Decl *getDecl() const {
    return declAndKind.getPointer();
  }
};

/// Emit a fix-it replacing \p charRange with \p newText , inserting or
/// removing whitespace after \c charRange in a way suitable for editing a
/// sequence of whitespce-separated keywords.
void fixItReplaceKeywords(InFlightDiagnostic &diag,
                          CharSourceRange charRange,
                          StringRef newText) {
  auto &SM = diag.getSourceManager();
  auto charRangeIsFollowedByWhitespace = [&]() -> bool {
    auto str = SM.extractText({ charRange.getEnd(), 1 });
    return str.empty() ? false : isspace(str.front());
  };

  SmallString<32> scratch;
  scratch += newText;

  if (newText.empty()) {
    // Eat trailing whitespace.
    while (charRangeIsFollowedByWhitespace()) {
      charRange = { charRange.getStart(), charRange.getByteLength() + 1 };
    }
  } else {
    if (!charRangeIsFollowedByWhitespace()) {
      scratch.push_back(' ');
    }
  }

  diag.fixItReplaceChars(charRange.getStart(), charRange.getEnd(), scratch);
}

/// Emit a fix-it replacing \p range with \p newText , inserting or
/// removing whitespace after \c range in a way suitable for editing a
/// sequence of whitespce-separated keywords.
void fixItReplaceKeywords(InFlightDiagnostic &diag,
                          SourceRange range,
                          StringRef newText) {
  auto &SM = diag.getSourceManager();
  auto charRange = Lexer::getCharSourceRangeFromSourceRange(SM, range);
  return fixItReplaceKeywords(diag, charRange, newText);
}

/// Returns a string representation of \p attr suitable to either replace an
/// existing attribute or be inserted as a new attribute, depending on the
/// value of \p toInsert .
StringRef printAttr(DeclAttribute *attr,
                    Decl *decl,
                    SmallVectorImpl<char> &scratch,
                    bool toInsert = false) {
  auto &ctx = decl->getASTContext();
  auto opts = PrintOptions::printForDiagnostics(AccessLevel::Private,
                              ctx.TypeCheckerOpts.PrintFullConvention);
  opts.PrintLongAttrsOnSeparateLines = false;

  llvm::raw_svector_ostream os{scratch};
  StreamPrinter printer{os};
  attr->print(printer, opts, decl);

  auto str = StringRef(scratch.begin(), scratch.size());
  if (!toInsert)
    str = str.trim(' ');
  return str;
}

/// Emit \c diag::attr_abi_matching_attr_here in the best available location.
void noteAttrHere(DeclAttribute *attr, Decl *decl, bool isMatch = false) {
  auto &ctx = decl->getASTContext();
  SourceLoc loc = attr->getLocation();
  if (loc.isValid())
    ctx.Diags.diagnose(loc, diag::attr_abi_matching_attr_here,
                       isMatch, attr->isDeclModifier(), attr->isImplicit());
  else
    ctx.Diags.diagnose(decl, diag::attr_abi_matching_attr_here,
                       isMatch, attr->isDeclModifier(), attr->isImplicit());
}

/// Get the best available \c SourceLoc representing the type in \p storage .
SourceLoc getTypeLoc(AbstractStorageDecl *storage, Decl *owner = nullptr) {
  auto loc = storage->getTypeSourceRangeForDiagnostics().Start;
  if (loc.isInvalid())
    loc = storage->getLoc();
  if (loc.isInvalid() && owner)
    loc = owner->getLoc();
  return loc;
}

/// Get a decl's generic signature, if it has one.
GenericSignature getGenericSignature(Decl *decl) {
  if (auto genericCtx = decl->getAsGenericContext())
    return genericCtx->getGenericSignature();
  return GenericSignature();
}

class ABIDeclChecker : public ASTComparisonVisitor<ABIDeclChecker> {
  ASTContext &ctx;
  Decl *diagnoseOnDecl;
  ABIAttr *abiAttr;

  /// Used to de-duplicate short-form \c \@available attrs. See \c checkAttr() .
  SmallSetVector<SourceLoc, 4> diagnosedAvailableAttrSourceLocs;

  /// This emits a diagnostic with a fixit to remove the attribute.
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnoseAndRemoveAttr(DeclAttribute *attr,
                                           ArgTypes &&...Args) {
    return swift::diagnoseAndRemoveAttr(diagnoseOnDecl, attr,
                                        std::forward<ArgTypes>(Args)...);
  }

public:
  ABIDeclChecker(ASTContext &ctx, Decl *diagnoseOnDecl, ABIAttr *abiAttr)
    : ctx(ctx), diagnoseOnDecl(diagnoseOnDecl), abiAttr(abiAttr) {}

  // MARK: @abi checking - decls

  void check(Decl *api, Decl *abi) {
    // Do the declarations have the same kind, broadly speaking? Many kinds have
    // special mangling behavior (e.g. inits vs normal funcs) that make it
    // unrealistic to treat one kind as though it were another.
    // (And if they don't, we can't really compare them properly.
    if (api->getKind() != abi->getKind()) {
      // FIXME: DescriptiveDeclKind is overly specific; we really just want to
      //        say that e.g. a `func` can't have the ABI of a `var`.
      diagnoseAndRemoveAttr(abiAttr, diag::attr_abi_mismatched_kind, api, abi);
      return;
    }

    visit(api, abi);
  }

  bool checkParameterFlags(ParameterTypeFlags api, ParameterTypeFlags abi,
                           ParameterTypeFlags apiOrig,
                           ParameterTypeFlags abiOrig,
                           Type apiType, Type abiType,
                           SourceLoc apiTypeLoc, SourceLoc abiTypeLoc,
                           TypeOrigin origin) {
    // Some keywords are spelled differently for a `self` parameter.
    bool isSelfParam = origin.getKind() == TypeOrigin::Kind::SelfParameter;

    bool didDiagnose = false;

    auto noteShouldMatch = [&](bool isModifier) {
      if (isSelfParam)
        ctx.Diags.diagnose(apiTypeLoc, diag::attr_abi_matching_attr_here,
                           /*matches=*/false, isModifier, /*isImplicit=*/false);
      else
        ctx.Diags.diagnose(apiTypeLoc, diag::attr_abi_should_match_type_here);
    };

    // These assertions represent values that should have been normalized.
    ASSERT(!api.isVariadic() && !abi.isVariadic());
    ASSERT(!api.isAutoClosure() && !abi.isAutoClosure());
    ASSERT(!api.isNonEphemeral() && !abi.isNonEphemeral());
    ASSERT(!api.isIsolated() && !abi.isIsolated());
    ASSERT(!api.isSending() && !abi.isSending());
    ASSERT(!api.isCompileTimeLiteral() && !abi.isCompileTimeLiteral());

    if (api.getOwnershipSpecifier() != abi.getOwnershipSpecifier()) {
      auto getSpelling = [=](ParamSpecifier spec) -> StringRef {
        // Customize a couple of names to match what the developer would
        // actually write.
        if (spec == ParamSpecifier::Default)
          return "";
        if (spec == ParamSpecifier::InOut && isSelfParam)
          return "mutating";
        if (spec == ParamSpecifier::LegacyOwned && isSelfParam)
          return "__consuming";
        if (spec == ParamSpecifier::ImplicitlyCopyableConsuming)
          return "sending";
        return getNameForParamSpecifier(spec);
      };

      ctx.Diags.diagnose(abiTypeLoc, diag::attr_abi_mismatched_param_modifier,
                         getSpelling(abiOrig.getOwnershipSpecifier()),
                         getSpelling(apiOrig.getOwnershipSpecifier()),
                         /*isModifier=*/true, unsigned(origin.getKind()),
                         origin.getDecl());
      noteShouldMatch(/*isModifier=*/true);
      didDiagnose = true;
    }

    if (api.isNoDerivative() != abi.isNoDerivative()) {
      ctx.Diags.diagnose(abiTypeLoc, diag::attr_abi_mismatched_param_modifier,
                         abiOrig.isNoDerivative() ? "noDerivative" : "",
                         apiOrig.isNoDerivative() ? "noDerivative" : "",
                         /*isModifier=*/false, unsigned(origin.getKind()),
                         origin.getDecl());
      noteShouldMatch(/*isModifier=*/false);
      didDiagnose = true;
    }

    if (api.isAddressable() != abi.isAddressable()) {
      StringRef spelling = isSelfParam ? "_addressableSelf" : "_addressable";
      ctx.Diags.diagnose(abiTypeLoc, diag::attr_abi_mismatched_param_modifier,
                         abiOrig.isAddressable() ? spelling : "",
                         apiOrig.isAddressable() ? spelling : "",
                         /*isModifier=*/false, unsigned(origin.getKind()),
                         origin.getDecl());
      noteShouldMatch(/*isModifier=*/false);
      didDiagnose = true;
    }

    if (!didDiagnose && api != abi) {
      // Flag difference not otherwise diagnosed. This is a fallback diagnostic.
      ctx.Diags.diagnose(abiTypeLoc, diag::attr_abi_mismatched_type,
                         unsigned(origin.getKind()), origin.getDecl(),
                         abiType, apiType);
      ctx.Diags.diagnose(apiTypeLoc, diag::attr_abi_should_match_type_here);
      didDiagnose = true;
    }

    return didDiagnose;
  }

  bool checkParameter(ParamDecl *api, ParamDecl *abi,
                      ValueDecl *apiDecl, ValueDecl *abiDecl) {
    ASSERT(api && abi);

    bool didDiagnose = false;
    if (auto defaultExpr = abi->getStructuralDefaultExpr()) {
      // Forbidden.
      ctx.Diags.diagnose(defaultExpr->getLoc(),
                         diag::attr_abi_no_default_arguments, abi);
      // TODO: Fix removing default arg (requires SourceLoc for equal sign)

      // Don't return immediately because we can independently check the type.
      didDiagnose = true;
    }

    auto apiOrig = api->toFunctionParam();
    auto abiOrig = abi->toFunctionParam();
    // FIXME: Do `self` params have the same default param specifier behavior?
    auto apiNorm = normalizeParam(apiOrig, apiDecl);
    auto abiNorm = normalizeParam(abiOrig, abiDecl);

    // FIXME: Refine to point to specific modifiers where possible.
    SourceLoc apiTypeLoc = getTypeLoc(api, apiDecl);
    SourceLoc abiTypeLoc = getTypeLoc(abi, abiDecl);

    didDiagnose |= checkType(apiNorm.getPlainType(), abiNorm.getPlainType(),
                             apiTypeLoc, abiTypeLoc,
                             getGenericSignature(apiDecl),
                             getGenericSignature(abiDecl),
                             TypeOrigin::forParameter(abi));

    didDiagnose |= checkParameterFlags(apiNorm.getParameterFlags(),
                                       abiNorm.getParameterFlags(),
                                       apiOrig.getParameterFlags(),
                                       abiOrig.getParameterFlags(),
                                       apiNorm.getPlainType(),
                                       abiNorm.getPlainType(),
                                       apiTypeLoc, abiTypeLoc,
                                       TypeOrigin::forParameter(abi));

    didDiagnose |= checkAttrs(api->getAttrs(), abi->getAttrs(), api, abi);

    return didDiagnose;
  }

  bool checkParameterList(ParameterList *api, ParameterList *abi,
                          ValueDecl *apiDecl, ValueDecl *abiDecl) {
    // Do the declarations have the same number of parameters?
    if (api->size() != abi->size()) {
      diagnoseAndRemoveAttr(abiAttr, diag::attr_abi_mismatched_arity, apiDecl,
                            /*genericParams=*/false);
      return true;
    }

    bool didDiagnose = false;

    for (auto pair : llvm::zip(*api, *abi)) {
      didDiagnose |= checkParameter(std::get<0>(pair), std::get<1>(pair),
                                    apiDecl, abiDecl);
    }

    return didDiagnose;
  }

  bool checkImplicitSelfParam(ParamDecl *api, ParamDecl *abi,
                              ValueDecl *apiDecl, ValueDecl *abiDecl) {
    if (!api && !abi)
      // Nothing to check
      return false;

    if ((api && !abi) || (!api && abi)) {
      diagnoseAndRemoveAttr(abiAttr, diag::attr_abi_mismatched_arity,
                            apiDecl, /*genericParams=*/false);
      return true;
    }

    return checkParameter(api, abi, apiDecl, abiDecl);
  }

  bool checkGenericSignature(GenericSignature api, GenericSignature abi,
                             Decl *apiDecl, Decl *abiDecl) {
    if (api.isNull() && abi.isNull())
      return false;

    if (api.isNull()) {
      abiDecl->diagnose(diag::attr_abi_extra_generic_signature, apiDecl);
      return true;
    }

    if (abi.isNull()) {
      abiDecl->diagnose(diag::attr_abi_missing_generic_signature,
                        api.getAsString());
      return true;
    }

    auto apiNorm = normalizeGenericSignature(api);
    auto abiNorm = normalizeGenericSignature(abi);

    if (!apiNorm->isEqual(abiNorm)) {
      abiDecl->diagnose(diag::attr_abi_mismatched_generic_signature,
                        abi.getAsString(), api.getAsString());
      apiDecl->diagnose(diag::attr_abi_should_match_type_here);
      return true;
    }

    return false;
  }

  bool checkEffects(DeclEffects api, DeclEffects abi, Decl *apiDecl,
                    Decl *abiDecl) {
    bool didDiagnose = false;

    // Do the declarations match in throwing behavior? We don't care about
    // `throws` vs. `rethrows` here, just whether callers will account for an
    // error return.
    bool apiThrows = api.anyContains(EffectKind::Throws);
    bool abiThrows = abi.anyContains(EffectKind::Throws);

    if (apiThrows != abiThrows) {
      diagnoseAndRemoveAttr(abiAttr, diag::attr_abi_mismatched_throws,
                            apiDecl, /*abiCanThrow=*/abiThrows);
      didDiagnose = true;
    } else if (apiThrows && abiThrows) {
      // If both throw, make sure the throw types are compatible.
      auto apiThrowType = api.effectiveThrownType.value_or(ctx.getNeverType());
      auto abiThrowType = abi.effectiveThrownType.value_or(ctx.getNeverType());

      didDiagnose |= checkType(apiThrowType, abiThrowType,
                               api.throwsLoc, abi.throwsLoc,
                               getGenericSignature(apiDecl),
                               getGenericSignature(abiDecl),
                               TypeOrigin::forThrowsEffect());
    }

    // Do the declarations match in async-ness?
    if (api.anyContains(EffectKind::Async) != abi.anyContains(EffectKind::Async)) {
      diagnoseAndRemoveAttr(abiAttr, diag::attr_abi_mismatched_async,
                            apiDecl, /*abiHasAsync=*/abi.anyContains(EffectKind::Async));
      didDiagnose = true;
    }

    return didDiagnose;
  }

  bool checkFailable(ConstructorDecl *api, ConstructorDecl *abi) {
    if (api->isFailable() == abi->isFailable()) {
      return false;
    }

    auto diag = ctx.Diags.diagnose(abiAttr->getLocation(),
                                   diag::attr_abi_failable_mismatch, api,
                                   api->isFailable(), abi->isFailable());

    if (api->isFailable())
      diag.fixItInsertAfter(abi->getLoc(),
                            api->isImplicitlyUnwrappedOptional() ? "!" : "?");
    else
      diag.fixItRemove(abi->getFailabilityLoc());

    return true;
  }

  bool checkStaticAndFinal(ValueDecl *api, ValueDecl *abi) {
    // The `static`, `class`, and `final` keywords all need to be evaluated
    // together because of their intertwined semantics. This pile of code
    // diagnoses errors in either or both.

    /// Returns the type and location of the declaration's `static` or `class` keyword, if any.
    auto getStaticSpelling = [](Decl *decl) -> Located<StaticSpellingKind> {
      if (auto var = dyn_cast<VarDecl>(decl))
        decl = var->getParentPatternBinding();

      if (auto pbd = dyn_cast<PatternBindingDecl>(decl))
        return { pbd->getStaticSpelling(), pbd->getStaticLoc() };

      if (auto subscript = dyn_cast<SubscriptDecl>(decl))
        return { subscript->getStaticSpelling(), subscript->getStaticLoc() };

      if (auto func = dyn_cast<FuncDecl>(decl))
        return { func->getStaticSpelling(), func->getStaticLoc() };

      return { StaticSpellingKind::None, SourceLoc() };
    };

    /// Represents the combination of `class`, `static`, and `final` keywords
    /// for a given declaration.
    enum class StaticnessAndFinality : uint8_t {
      InstanceAndOverridable,
      ClassAndOverridable,
      InstanceAndFinal,
      ClassAndFinal,
      Static,
    };

    /// Returns a `StaticnessAndFinality` corresponding to the given values.
    auto getStaticnessAndFinality = [](StaticSpellingKind staticSpelling,
                                       bool isFinal) {
      switch (staticSpelling) {
      case StaticSpellingKind::None:
        return isFinal ? StaticnessAndFinality::InstanceAndFinal
                       : StaticnessAndFinality::InstanceAndOverridable;

      case StaticSpellingKind::KeywordStatic:
        return StaticnessAndFinality::Static;

      case StaticSpellingKind::KeywordClass:
        return isFinal ? StaticnessAndFinality::ClassAndFinal
                       : StaticnessAndFinality::ClassAndOverridable;
      }

      llvm_unreachable("unknown StaticSpellingKind");
    };

    auto apiSAF = getStaticnessAndFinality(getStaticSpelling(api).Item,
                                           api->isFinal());

    auto abiStaticSpelling = getStaticSpelling(abi);
    auto abiSAF = getStaticnessAndFinality(abiStaticSpelling.Item,
                                           abi->isFinal());

    /// Collapses down the difference between `Static` and `ClassAndFinal`.
    auto getSemantics = [](StaticnessAndFinality syntax) {
      if (syntax == StaticnessAndFinality::ClassAndFinal)
        return StaticnessAndFinality::Static;
      return syntax;
    };

    if (getSemantics(apiSAF) != getSemantics(abiSAF)) {
      auto diag = abi->diagnose(diag::attr_abi_static_final_mismatch,
                                uint8_t(abiSAF), abi, uint8_t(apiSAF), api,
                                api->getDeclContext()->getSelfClassDecl());

      SourceLoc insertLoc = abi->getAttributeInsertionLoc(/*forModifier=*/true);
      SourceLoc replaceLoc = abiStaticSpelling.Loc;
      SourceLoc deleteLoc;

      // If there's a (non-implicit) `final`, we may want to fix it.
      auto finalAttr = abi->getAttrs().getAttribute<FinalAttr>();
      if (finalAttr && !finalAttr->isImplicit())
        deleteLoc = finalAttr->getLocation();

      // If only one is valid, that should be `replaceLoc`; if both are valid,
      // `replaceLoc` should come before `deleteLoc`.
      if (deleteLoc.isValid() && (replaceLoc.isInvalid() ||
                                 ctx.SourceMgr.isBefore(deleteLoc, replaceLoc)))
        std::swap(deleteLoc, replaceLoc);

      // Delete the keyword at `deleteLoc`, if there is one.
      if (deleteLoc.isValid())
        fixItReplaceKeywords(diag, deleteLoc, "");

      StringRef spellings[] = { "", "class", "final", "final class", "static" };
      auto newKeywords = spellings[uint8_t(apiSAF)];

      // Either replace the first keyword, or insert new keywords.
      if (replaceLoc.isValid())
        fixItReplaceKeywords(diag, replaceLoc, newKeywords);
      else
        fixItReplaceKeywords(diag, CharSourceRange(insertLoc, 0), newKeywords);

      return true;
    }

    return false;
  }

  /// This declaration should not be in an `@abi` attribute.
#define UNSUPPORTED_DECL(NAME) \
  bool visit##NAME##Decl(NAME##Decl *api, NAME##Decl *abi) { \
    return visitParentOf##NAME##Decl(api, abi); \
  }

  /// This declaration has no additional validation logic.
#define PASSTHROUGH_DECL(NAME) \
  bool visit##NAME##Decl(NAME##Decl *api, NAME##Decl *abi) { \
    return visitParentOf##NAME##Decl(api, abi); \
  }

  bool visitDecl(Decl *api, Decl *abi) {
    bool didDiagnose = checkAttrs(api->getAttrs(), abi->getAttrs(), api, abi);

    if (api->getAsGenericContext()) {
      didDiagnose |= checkGenericSignature(getGenericSignature(api),
                                           getGenericSignature(abi),
                                           api, abi);
    }

    return didDiagnose;
  }

  bool visitValueDecl(ValueDecl *api, ValueDecl *abi) {
    if (visitParentOfValueDecl(api, abi))
      return true;

    return checkStaticAndFinal(api, abi);
  }

  PASSTHROUGH_DECL(Type)
  PASSTHROUGH_DECL(GenericType)
  PASSTHROUGH_DECL(NominalType)
  PASSTHROUGH_DECL(Operator)

  UNSUPPORTED_DECL(Enum)
  UNSUPPORTED_DECL(Struct)
  UNSUPPORTED_DECL(Class)

  // TODO: When supported, diagnose if Protocol::isMarkerProtocol()
  //       (mangler can't handle invertible protocols with @abi)
  UNSUPPORTED_DECL(Protocol)

  UNSUPPORTED_DECL(BuiltinTuple)
  UNSUPPORTED_DECL(OpaqueType)
  UNSUPPORTED_DECL(TypeAlias)
  UNSUPPORTED_DECL(GenericTypeParam)
  UNSUPPORTED_DECL(AssociatedType)
  UNSUPPORTED_DECL(Module)
  UNSUPPORTED_DECL(Param)
  UNSUPPORTED_DECL(Destructor)
  UNSUPPORTED_DECL(Macro)
  UNSUPPORTED_DECL(EnumElement)
  UNSUPPORTED_DECL(Extension)
  UNSUPPORTED_DECL(TopLevelCode)
  UNSUPPORTED_DECL(Import)
  UNSUPPORTED_DECL(PrecedenceGroup)
  UNSUPPORTED_DECL(Missing)
  UNSUPPORTED_DECL(MissingMember)
  UNSUPPORTED_DECL(PatternBinding)
  UNSUPPORTED_DECL(EnumCase)
  UNSUPPORTED_DECL(Accessor)
  UNSUPPORTED_DECL(InfixOperator)
  UNSUPPORTED_DECL(PrefixOperator)
  UNSUPPORTED_DECL(PostfixOperator)
  UNSUPPORTED_DECL(MacroExpansion)
  UNSUPPORTED_DECL(Using)

  bool visitAbstractFunctionDecl(AbstractFunctionDecl *api,
                                 AbstractFunctionDecl *abi) {
    if (visitParentOfAbstractFunctionDecl(api, abi))
      return true;

    // FIXME: How much should we diagnose in IRGen for more precise ABI info?

    if (checkImplicitSelfParam(api->getImplicitSelfDecl(),
                               abi->getImplicitSelfDecl(),
                               api, abi))
      return true;

    if (checkParameterList(api->getParameters(), abi->getParameters(),
                           api, abi))
      return true;

    return checkEffects(DeclEffects(api), DeclEffects(abi), api, abi);
    // NOTE: Does not check result type--that's the subclass's responsibility!
  }

  bool visitFuncDecl(FuncDecl *api, FuncDecl *abi) {
    if (visitParentOfFuncDecl(api, abi))
      return true;

    // Intentionally ignoring `hasSendingResult()` because it doesn't affect
    // calling convention.

    return checkType(api->getResultInterfaceType(),
                     abi->getResultInterfaceType(),
                     api->getResultTypeSourceRange().Start,
                     abi->getResultTypeSourceRange().Start,
                     getGenericSignature(api), getGenericSignature(abi),
                     TypeOrigin::forResult());
  }

  bool visitConstructorDecl(ConstructorDecl *api, ConstructorDecl *abi) {
    if (visitParentOfConstructorDecl(api, abi))
      return true;

    return checkFailable(api, abi);
  }

  bool visitAbstractStorageDecl(AbstractStorageDecl *api,
                                AbstractStorageDecl *abi) {
    if (visitParentOfAbstractStorageDecl(api, abi))
      return true;

    if (checkType(api->getValueInterfaceType(), abi->getValueInterfaceType(),
                  getTypeLoc(api), getTypeLoc(abi),
                  getGenericSignature(api), getGenericSignature(abi),
                  TypeOrigin::forUnspecified()))
      return true;

    return false;
  }

  bool visitVarDecl(VarDecl *api, VarDecl *abi) {
    if (visitParentOfVarDecl(api, abi))
      return true;
    return false;
  }

  bool visitSubscriptDecl(SubscriptDecl *api, SubscriptDecl *abi) {
    if (visitParentOfSubscriptDecl(api, abi))
      return true;

    if (checkParameterList(api->getIndices(), abi->getIndices(), api, abi))
      return true;

    return false;
  }

#undef UNSUPPORTED_DECL
#undef PASSTHROUGH_DECL

  // MARK: @abi checking - attributes

  /// Are these attributes similar enough that they should be checked against
  /// one another? At minimum this means they're of the same kind, but for some
  /// attrs there are additional criteria.
  bool canCompareAttrs(DeclAttribute *api, DeclAttribute *abi,
                       Decl *apiDecl, Decl *abiDecl) {
    if (api->getKind() != abi->getKind())
      return false;

    auto getAvailableDomain = [](Decl *D, DeclAttribute *A) {
      return D->getSemanticAvailableAttr(cast<AvailableAttr>(A))->getDomain();
    };

    // Extra logic for specific attributes.
    switch (api->getKind()) {
    case DeclAttrKind::Expose:
      return cast<ExposeAttr>(api)->getExposureKind()
                  == cast<ExposeAttr>(abi)->getExposureKind();

    case DeclAttrKind::Extern:
      return cast<ExternAttr>(api)->getExternKind()
                  == cast<ExternAttr>(abi)->getExternKind();

    case DeclAttrKind::Available:
      return getAvailableDomain(apiDecl, api)
                  == getAvailableDomain(abiDecl, abi);
      return true;

    default:
      break;
    }

    return true;
  }

  /// Check two attribute lists against one another.
  /// 
  /// This pairs up attributes which are sufficiently similar (as determined by
  /// \c canCompareAttrs() ) and then checks them. Attributes which
  /// have no counterpart are checked individually. 
  bool checkAttrs(DeclAttributes api, DeclAttributes abi,
                  Decl *apiDecl, Decl *abiDecl) {
    bool didDiagnose = false;

    // Collect all ABI attrs.
    SmallVector<DeclAttribute*, 32> remainingABIDeclAttrs;
    for (auto *abiDeclAttr : abi) {
      remainingABIDeclAttrs.push_back(abiDeclAttr);
    }

    // Visit each API attr, pairing it with an ABI attr if possible.
    // Note that this will visit even invalid attributes.
    for (auto *apiDeclAttr : api) {
      auto abiAttrIter = llvm::find_if(remainingABIDeclAttrs,
                                        [&](DeclAttribute *abiDeclAttr) {
        return abiDeclAttr && canCompareAttrs(apiDeclAttr, abiDeclAttr,
                                              apiDecl, abiDecl);
      });
      DeclAttribute *abiDeclAttr = nullptr;
      if (abiAttrIter != remainingABIDeclAttrs.end()) {
        // Found a matching ABI attr. Claim and use it.
        std::swap(abiDeclAttr, *abiAttrIter);
      }
      didDiagnose |= checkAttr(apiDeclAttr, abiDeclAttr, apiDecl, abiDecl);
    }

    // Visit leftover ABI attrs.
    for (auto *abiDeclAttr : remainingABIDeclAttrs) {
      if (abiDeclAttr)
        didDiagnose |= checkAttr(nullptr, abiDeclAttr, apiDecl, abiDecl);
    }
    return didDiagnose;
  }

  /// Check a single attribute against its counterpart. If an attribute has no
  /// counterpart, the counterpart may be \c nullptr ; either \p abi or \p abi
  /// may be \c nullptr , but never both. 
  bool checkAttr(DeclAttribute *api, DeclAttribute *abi,
                 Decl *apiDecl, Decl *abiDecl) {
    ASSERT(api || abi && "checkAttr() should have at least one attribute");

    // If either attribute has already been diagnosed, don't check here.
    if ((api && api->isInvalid()) || (abi && abi->isInvalid()))
      return true;

    auto kind = api ? api->getKind() : abi->getKind();
    auto behaviors = DeclAttribute::getBehaviors(kind);

    switch (behaviors & DeclAttribute::InABIAttrMask) {
    case DeclAttribute::UnreachableInABIAttr:
      ASSERT(abiAttr->canAppearOnDecl(apiDecl)
                && "checking @abi on decl that can't have it???");
      ASSERT(!abiAttr->canAppearOnDecl(apiDecl)
                 && "unreachable-in-@abi attr on reachable decl???");

      // If the asserts are disabled, fall through to no checking.
      LLVM_FALLTHROUGH;

    case DeclAttribute::UnconstrainedInABIAttr:
      // No checking required.
      return false;

    case DeclAttribute::ForbiddenInABIAttr:
      // Diagnose if ABI has attribute.
      if (abi) {
        // A shorthand `@available(foo 1, bar 2, *)` attribute gets parsed into
        // several separate `AvailableAttr`s, each with the full range of the
        // shorthand attribute. If we've already diagnosed one of them, don't
        // diagnose the rest; otherwise, record that we've diagnosed this one.
        if (isa<AvailableAttr>(abi) &&
              !diagnosedAvailableAttrSourceLocs.insert(abi->getLocation()))
          return true;

        diagnoseAndRemoveAttr(abi, diag::attr_abi_forbidden_attr,
                              abi->getAttrName(), abi->isDeclModifier());
        return true;
      }

      return false;

    case DeclAttribute::EquivalentInABIAttr:
      // Diagnose if API doesn't have attribute.
      if (!api) {
        diagnoseAndRemoveAttr(abi, diag::attr_abi_extra_attr,
                              abi->getAttrName(), abi->isDeclModifier(),
                              abi->isImplicit());
        return true;
      }

      // Diagnose if ABI doesn't have attribute.
      if (!abi) {
        SmallString<64> scratch;
        auto apiAttrAsString = printAttr(api, apiDecl, scratch,
                                         /*toInsert=*/true);

        ctx.Diags.diagnose(abiDecl, diag::attr_abi_missing_attr,
                           api->getAttrName(), api->isDeclModifier())
          .fixItInsert(abiDecl->getAttributeInsertionLoc(api->isDeclModifier()),
                       apiAttrAsString);
        noteAttrHere(api, apiDecl);
        return true;
      }

      // Diagnose if two attributes are mismatched.
      if (!api->isEquivalent(abi, apiDecl)) {
        SmallString<64> scratch;
        auto apiAttrAsString = printAttr(api, apiDecl, scratch);

        ctx.Diags.diagnose(abi->getLocation(), diag::attr_abi_mismatched_attr,
                           abi->getAttrName(), abi->isDeclModifier(),
                           apiAttrAsString)
          .fixItReplace(abi->getRangeWithAt(), apiAttrAsString);
        noteAttrHere(api, apiDecl);
        return true;
      }

      return false;
    }

    llvm_unreachable("unknown InABIAttrMask behavior");
  }

  // MARK: @abi checking - types

  bool checkType(Type api, Type abi, SourceLoc apiLoc, SourceLoc abiLoc,
                 GenericSignature apiSig, GenericSignature abiSig,
                 TypeOrigin origin) {
    if (!api.isNull() && !abi.isNull()) {
      Type apiNorm = normalizeType(api->getReducedType(apiSig));
      Type abiNorm = normalizeType(abi->getReducedType(abiSig));
      if (apiNorm->isEqual(abiNorm)) {
        return false;
      }
    }

    ctx.Diags.diagnose(abiLoc, diag::attr_abi_mismatched_type,
                       unsigned(origin.getKind()), origin.getDecl(), abi, api);
    ctx.Diags.diagnose(apiLoc, diag::attr_abi_should_match_type_here);
    return true;
  }

  /// Fold away details of \p original that do not affect the calling
  /// conventions used for this type.
  static Type normalizeType(Type original) {
    return original.transformRec(&tryNormalizeOutermostType);
  }

  /// Fold away details of \p original that do not affect the calling
  /// conventions used for this parameter. Does \em not fully normalize
  /// \c original.getPlainType() , though it may slightly modify it.
  /// Pass \c nullptr to \p forDecl for a parameter belonging to a closure.
  static AnyFunctionType::Param
  normalizeParam(const AnyFunctionType::Param &original, ValueDecl *forDecl) {
    Type ty = original.getPlainType();
    auto flags = original.getParameterFlags();

    // We will smash away (non-parameter pack) variadics; turn the type into
    // an array.
    if (flags.isVariadic()) {
      ty = original.getParameterType();
    }

    // Flatten ownership information down to consume/borrow/inout, which are the
    // only distinctions that matter for calling conventions and memory
    // management. This removes the distinction between e.g. `__owned` and
    // `consuming`, or between the declaration's default parameter ownership
    // convention and an explicit equivalent.
    auto ownership = normalizeOwnership(flags.getOwnershipSpecifier(),
                                        forDecl);

    // Eliminate flags with no effect on the calling convention.
    flags = flags
      .withVariadic(false)
      .withCompileTimeLiteral(false)
      .withAutoClosure(false)
      .withNonEphemeral(false)
      .withIsolated(false)
      .withSending(false)
      .withOwnershipSpecifier(ownership);

    return AnyFunctionType::Param(ty, Identifier(), flags, Identifier());
  }

  /// Folds away \p original to one of \c Consuming , \c Borrowing , or
  /// \c InOut , which are the only parameter ownership behaviors relevant to ABI.
  /// Pass \c nullptr to \p forDecl for a parameter belonging to a closure.
  static ParamSpecifier normalizeOwnership(ParamSpecifier original,
                                           ValueDecl *forDecl) {
    switch (original) {
    case ParamSpecifier::Default:
      return getDefaultParamSpecifier(forDecl);
      break;
    case swift::ParamSpecifier::InOut:
      return ParamSpecifier::InOut;
      break;
    case ParamSpecifier::Borrowing:
    case ParamSpecifier::LegacyShared:
      return ParamSpecifier::Borrowing;
      break;
    case swift::ParamSpecifier::Consuming:
    case swift::ParamSpecifier::LegacyOwned:
    case swift::ParamSpecifier::ImplicitlyCopyableConsuming:
      return ParamSpecifier::Consuming;
      break;
    }
  }

  static GenericSignature normalizeGenericSignature(GenericSignature original) {
    // FIXME: Are there other ABI-tolerable generic signature differences?
    return original.withoutMarkerProtocols();
  }

  static
  FunctionTypeIsolation normalizeIsolation(FunctionTypeIsolation original) {
    // Isolation doesn't affect the ABI, except that `@isolated(any)` (a.k.a.
    // `FunctionTypeIsolation::Kind::Erased`) has a different ABI from all the
    // others. See docs/SIL/Types.td for details.
    return original.isErased() ? FunctionTypeIsolation::forErased()
                               : FunctionTypeIsolation::forNonIsolated();
  }

  static std::optional<Type> tryNormalizeOutermostType(TypeBase *original) {
    // Function types: Eliminate anything that doesn't affect calling convention.
    if (auto func = original->getAs<AnyFunctionType>()) {
      // Ignore `@escaping`, `@Sendable`, `sending` on result, and most
      // isolation; they have no ABI effect.
      auto normalizedExt = func->getExtInfo().intoBuilder()
        .withNoEscape(false)
        .withSendable(false)
        .withSendingResult(false)
        .withIsolation(normalizeIsolation(func->getIsolation()))
        .build();

      // Ignore ignorable parts of parameters.
      SmallVector<AnyFunctionType::Param, 8> normalizedParams;
      for (auto param : func->getParams()) {
        auto normalizedParam = normalizeParam(param, /*forDecl=*/nullptr);
        normalizedParam = normalizedParam.withType(
                                 normalizeType(normalizedParam.getPlainType()));
        normalizedParams.push_back(normalizedParam);
      }

      if (isa<FunctionType>(func))
        return FunctionType::get(normalizedParams,
                                 normalizeType(func->getResult()),
                                 normalizedExt);

      ASSERT(isa<GenericFunctionType>(func));

      // Ignore ignorable parts of the generic signature.
      auto sig = original->getAs<GenericFunctionType>()->getGenericSignature();
      return GenericFunctionType::get(normalizeGenericSignature(sig),
                                      normalizedParams,
                                      normalizeType(func->getResult()),
                                      normalizedExt);
    }

    // Protocol-related types: Remove marker protocols.
    if (auto comp = original->getAs<ProtocolCompositionType>()) {
      auto normalized = comp->withoutMarkerProtocols();
      if (!normalized->isEqual(comp))
        return normalized;
    }

    if (auto proto = original->getAs<ProtocolType>()) {
      if (proto->getDecl()->isMarkerProtocol())
        return proto->getASTContext().TheAnyType;
    }

    if (auto existential = original->getAs<ExistentialType>()) {
      // Pull out the constraint and see how it'll normalize.
      auto normConstraint = normalizeType(existential->getConstraintType());

      // If the constraint is no longer existential, pull it out of the type.
      if (!normConstraint->isExistentialType())
        return normConstraint;
    }

    // Tuples: Remove labels.
    if (auto tuple = original->getAs<TupleType>()) {
      bool needsNormalization = false;
      SmallVector<TupleTypeElt, 8> unlabeledElements;

      for (auto elem : tuple->getElements()) {
        needsNormalization |= !elem.getName().empty();
        unlabeledElements.push_back(elem.getWithoutName());
      }

      if (!needsNormalization)
        return tuple;

      return TupleType::get(unlabeledElements, tuple->getASTContext());
    }

    // TODO: Allow Optional/non-Optional variance when ABI-compatible?
    // TODO: Allow variance in exact class of object?

    return std::nullopt;
  }
};

void checkABIAttrPBD(PatternBindingDecl *APBD, VarDecl *VD) {
  auto PBD = VD->getParentPatternBinding();

  Decl *anchorVD = nullptr;
  for (auto i : range(PBD->getNumPatternEntries())) {
    anchorVD = PBD->getAnchoringVarDecl(i);
    if (anchorVD)
      break;
  }

  // To make sure we only diagnose this stuff once, check that VD is the
  // first anchoring variable in the PBD.
  if (anchorVD != VD)
    return;

  // In the final approved feature, we only permit single-variable patterns.
  // (However, the rest of the compiler tolerates them.)
  if (!PBD->getSingleVar() || !APBD->getSingleVar()) {
    PBD->diagnose(diag::attr_abi_multiple_vars,
                  anchorVD ? anchorVD->getDescriptiveKind()
                           : PBD->getDescriptiveKind());
    return;
  }

  // Check the ABI PBD--this is what checks the underlying vars.
  TypeChecker::typeCheckDecl(APBD);
}

} // end anonymous namespace

void TypeChecker::checkDeclABIAttribute(Decl *D, ABIAttr *attr) {
  Decl *AD = attr->abiDecl;
  if (isa<VarDecl>(D) && isa<PatternBindingDecl>(AD)) {
    auto VD = cast<VarDecl>(D);
    auto APBD = cast<PatternBindingDecl>(AD);

    // Diagnose dissimilar PBD structures.
    checkABIAttrPBD(APBD, VD);

    // Do the rest of this checking on the corresponding VarDecl, not the
    // PBD that's actually in the attribute. Note that `AD` will become null
    // if they're too dissimilar to match up.
    AD = APBD->getVarAtSimilarStructuralPosition(VD);
  }
  // TODO: EnumElementDecl?

  if (!AD)
    return;

  // Check the ABI decl and bail if there was a problem with it.
  TypeChecker::typeCheckDecl(AD);
  if (AD->isInvalid())
    return;

  // Apply more precise checks.
  ABIDeclChecker(D->getASTContext(), D, attr).check(D, AD);
}
