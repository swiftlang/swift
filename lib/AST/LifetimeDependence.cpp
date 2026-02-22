//===--- LifetimeDependence.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024-2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/LifetimeDependence.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/MapVector.h"

#define DEBUG_TYPE "LifetimeDependence"

using namespace swift;

/// Determine whether Type t is "unknown", meaning we cannot safely determine
/// whether it is Escapable by calling TypeBase::isEscapable.
static bool isTypeUnknown(Type t) {
  // These types would hit an assertion in
  // TypeBase::computeInvertibleConformances.
  if (t->hasUnboundGenericType() || t->hasTypeParameter())
    return true;
  // This type would hit an assertion in checkRequirements.
  if (t->hasTypeVariable())
    return true;

  return false;
}

std::string LifetimeDescriptor::getString() const {
  switch (kind) {
  case DescriptorKind::Named: {
    bool shouldEscape =
        escapeIdentifierInContext(getName(), PrintNameContext::Normal);
    if (shouldEscape) {
      return ("`" + getName().str() + "`").str();
    }
    return getName().str().str();
  }
  case DescriptorKind::Ordered:
    return std::to_string(getIndex());
  case DescriptorKind::Self:
    return "self";
  }
  llvm_unreachable("Invalid DescriptorKind");
}

LifetimeEntry *
LifetimeEntry::create(const ASTContext &ctx, SourceLoc startLoc,
                      SourceLoc endLoc, ArrayRef<LifetimeDescriptor> sources,
                      std::optional<LifetimeDescriptor> targetDescriptor) {
  unsigned size = totalSizeToAlloc<LifetimeDescriptor>(sources.size());
  void *mem = ctx.Allocate(size, alignof(LifetimeEntry));
  return new (mem) LifetimeEntry(startLoc, endLoc, sources, targetDescriptor);
}

std::string LifetimeEntry::getString() const {
  std::string result = "(";
  if (targetDescriptor.has_value()) {
    result += targetDescriptor->getString();
    result += ": ";
  }

  bool firstElem = true;
  for (auto source : getSources()) {
    if (!firstElem) {
      result += ", ";
    }
    auto lifetimeKind = source.getParsedLifetimeDependenceKind();
    auto kindString = getNameForParsedLifetimeDependenceKind(lifetimeKind);
    bool printSpace = (lifetimeKind == ParsedLifetimeDependenceKind::Borrow ||
                       lifetimeKind == ParsedLifetimeDependenceKind::Inherit);
    if (!kindString.empty()) {
      result += kindString;
    }
    if (printSpace) {
      result += " ";
    }
    result += source.getString();
    firstElem = false;
  }
  result += ")";
  return result;
}

namespace swift {
bool matchLifetimeDependencies(
    const ArrayRef<LifetimeDependenceInfo> from,
    const ArrayRef<LifetimeDependenceInfo> to) {

  // Require each LifetimeDependenceInfo in 'from' to have a corresponding
  // LifetimeDependenceInfo in 'to', but not necessarily vice-versa.
  if (from.size() > to.size())
    return false;

  // If from and to are the same array, they naturally match. This case should
  // be reasonably common because lifetime dependence info is canonicalized.
  if (from.data() == to.data() && from.size() == to.size())
    return true;

  for (const auto &fromDep : from) {
    // For each LifetimeDependenceInfo in 'from', there must be one in 'to' that
    // satisfies it.
    const auto toDep = getLifetimeDependenceFor(to, fromDep.getTargetIndex());
    if (!toDep || !fromDep.convertibleTo(*toDep))
      return false;
  }
  return true;
}

std::optional<LifetimeDependenceInfo>
getLifetimeDependenceFor(ArrayRef<LifetimeDependenceInfo> lifetimeDependencies,
                         unsigned index) {
  for (auto dep : lifetimeDependencies) {
    if (dep.getTargetIndex() == index) {
      return dep;
    }
  }
  return std::nullopt;
}

bool
filterEscapableLifetimeDependencies(GenericSignature sig,
        ArrayRef<LifetimeDependenceInfo> inputs,
        SmallVectorImpl<LifetimeDependenceInfo> &outputs,
        llvm::function_ref<Type (unsigned targetIndex)> getSubstTargetType) {
  bool didRemoveLifetimeDependencies = false;

  for (auto &depInfo : inputs) {
    auto targetIndex = depInfo.getTargetIndex();
    Type substTy = getSubstTargetType(targetIndex);

    // If the type still contains type variables we don't know whether we
    // can drop the dependency.
    if (substTy->hasTypeVariable())
      continue;

    // Drop the dependency if the target type is Escapable.
    if (sig || !substTy->hasTypeParameter()) {
      if (substTy->isEscapable(sig)) {
        didRemoveLifetimeDependencies = true;
        continue;
      }
    }
    
    // Otherwise, keep the dependency.
    outputs.push_back(depInfo);
  }

  return didRemoveLifetimeDependencies;
}

StringRef
getNameForParsedLifetimeDependenceKind(ParsedLifetimeDependenceKind kind) {
  switch (kind) {
  case ParsedLifetimeDependenceKind::Borrow:
    return "borrow";
  case ParsedLifetimeDependenceKind::Inherit:
    return "copy";
  case ParsedLifetimeDependenceKind::Inout:
    return "&";
  default:
    return "";
  }
}

} // namespace swift

std::string LifetimeDependenceInfo::getString() const {
  std::string lifetimeDependenceString = "@lifetime(";
  auto addressable = getAddressableIndices();
  auto condAddressable = getConditionallyAddressableIndices();
  
  bool isFirstSpecifier = true;
  auto getSourceString = [&](IndexSubset *bitvector, StringRef kind) {
    std::string result;
    for (unsigned i = 0; i < bitvector->getCapacity(); i++) {
      if (bitvector->contains(i)) {
        if (!isFirstSpecifier) {
          result += ", ";
        }
        result += kind;
        if (addressable && addressable->contains(i)) {
          result += "address ";
        } else if (condAddressable && condAddressable->contains(i)) {
          result += "address_for_deps ";
        }
        result += std::to_string(i);
        isFirstSpecifier = false;
      }
    }
    return result;
  };
  // Unlike the AST printer, there is no need check isDefaultSuppressed() for
  // SIL printing because SIL does not assume any defaults.
  if (hasImmortalSpecifier()) {
    if (!isFirstSpecifier) {
      lifetimeDependenceString += ", ";
    }
    lifetimeDependenceString += "immortal";
    isFirstSpecifier = false;
  }
  if (inheritLifetimeParamIndices) {
    assert(!inheritLifetimeParamIndices->isEmpty());
    lifetimeDependenceString +=
        getSourceString(inheritLifetimeParamIndices, "copy ");
  }
  if (scopeLifetimeParamIndices) {
    assert(!scopeLifetimeParamIndices->isEmpty());
    lifetimeDependenceString +=
        getSourceString(scopeLifetimeParamIndices, "borrow ");
  }
  lifetimeDependenceString += ") ";
  return lifetimeDependenceString;
}

void LifetimeDependenceInfo::Profile(llvm::FoldingSetNodeID &ID) const {
  ID.AddBoolean(hasImmortalSpecifier());
  ID.AddBoolean(isFromAnnotation());
  ID.AddInteger(targetIndex);
  if (inheritLifetimeParamIndices) {
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Inherit);
    inheritLifetimeParamIndices->Profile(ID);
  }
  if (scopeLifetimeParamIndices) {
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Scope);
    scopeLifetimeParamIndices->Profile(ID);
  }
  if (hasAddressableParamIndices()) {
    ID.AddBoolean(true);
    getAddressableIndices()->Profile(ID);
  } else {
    ID.AddBoolean(false);
  }
}

void LifetimeDependenceInfo::getConcatenatedData(
    SmallVectorImpl<bool> &concatenatedData) const {
  auto pushData = [&](IndexSubset *paramIndices) {
    if (paramIndices == nullptr) {
      return;
    }
    assert(!paramIndices->isEmpty());

    for (unsigned i = 0; i < paramIndices->getCapacity(); i++) {
      if (paramIndices->contains(i)) {
        concatenatedData.push_back(true);
        continue;
      }
      concatenatedData.push_back(false);
    }
  };
  if (hasInheritLifetimeParamIndices()) {
    pushData(inheritLifetimeParamIndices);
  }
  if (hasScopeLifetimeParamIndices()) {
    pushData(scopeLifetimeParamIndices);
  }
  if (hasAddressableParamIndices()) {
    pushData(getAddressableIndices());
  }
}

namespace {
static bool isBitwiseCopyable(Type type, ASTContext &ctx) {
  auto *bitwiseCopyableProtocol =
      ctx.getProtocol(KnownProtocolKind::BitwiseCopyable);
  if (!bitwiseCopyableProtocol) {
    return false;
  }
  if (type->hasError())
    return false;

  return (bool)checkConformance(type, bitwiseCopyableProtocol);
}

static bool isDiagnosedNonEscapable(Type type) {
  if (type->hasError()) {
    return false;
  }
  // FIXME: This check is temporary until rdar://139976667 is fixed.
  // ModuleType created with ModuleType::get methods are ~Copyable and
  // ~Escapable because the Copyable and Escapable conformance is not added to
  // them by default.
  if (type->is<ModuleType>()) {
    return false;
  }
  return !type->isEscapable();
}

static bool isDiagnosedEscapable(Type type) {
  if (type->hasError()) {
    return false;
  }
  return type->isEscapable();
}
} // anonymous namespace

namespace {
enum class HasAnnotation { Annotated, Inferred };
enum class TargetKind { Inout, Result };

// Temporary data structure for building target dependencies. Used by the
// LifetimeDependenceChecker.
struct LifetimeDependenceBuilder {
  struct TargetDeps {
    SmallBitVector inheritIndices;
    SmallBitVector scopeIndices;
    HasAnnotation hasAnnotationStatus;
    TargetKind targetKind;
    bool hasImmortalSpecifier = false;

    TargetDeps(HasAnnotation hasAnnotation, TargetKind targetKind,
               unsigned capacity)
        : inheritIndices(capacity), scopeIndices(capacity),
          hasAnnotationStatus(hasAnnotation), targetKind(targetKind) {}

    bool empty() const {
      return !(hasImmortalSpecifier || inheritIndices.any()
               || scopeIndices.any());
    }

    bool hasAnnotation() const {
      return hasAnnotationStatus == HasAnnotation::Annotated;
    }

    bool isInout() const {
      return targetKind == TargetKind::Inout;
    }

    void addIfNew(unsigned sourceIndex, LifetimeDependenceKind kind) {
      // Some inferrence rules may attempt to add an inherit dependency after a
      // scope dependency (accessor wrapper + getter method).
      if (hasImmortalSpecifier || inheritIndices[sourceIndex]
          || scopeIndices[sourceIndex]) {
        return;
      }
      switch (kind) {
      case LifetimeDependenceKind::Inherit:
        inheritIndices.set(sourceIndex);
        break;
      case LifetimeDependenceKind::Scope:
        scopeIndices.set(sourceIndex);
        break;
      }
    }
  };

  const unsigned resultIndex;
  llvm::SmallMapVector<unsigned, TargetDeps, 4> depsArray;

  LifetimeDependenceBuilder(unsigned resultIndex): resultIndex(resultIndex) {}

public:
  // True if the builder is uninitialized. This may, however, be false even if
  // all TargetDeps are themselves empty.
  bool empty() const { return depsArray.empty(); }

  unsigned sourceIndexCap() const { return resultIndex; }

  TargetKind targetKindForIndex(unsigned targetIndex) const {
    return
      (targetIndex == resultIndex) ? TargetKind::Result : TargetKind::Inout;
  }

  // Return TargetDeps for 'targetIndex' if it has at least one source
  // dependency.
  const TargetDeps *getTargetDepsOrNull(unsigned targetIndex) const {
    auto iter = depsArray.find(targetIndex);
    if (iter != depsArray.end() && !iter->second.empty()) {
      return &iter->second;
    }
    return nullptr;
  }

  bool hasTargetDeps(unsigned targetIndex) const {
    return getTargetDepsOrNull(targetIndex) != nullptr;
  }

  TargetDeps *createAnnotatedTargetDeps(unsigned targetIndex) {
    auto iterAndInserted =
      depsArray.try_emplace(targetIndex, HasAnnotation::Annotated,
                            targetKindForIndex(targetIndex), sourceIndexCap());
    if (!iterAndInserted.second)
      return nullptr;

    return &iterAndInserted.first->second;
  }

  // Check this before diagnosing any broken inference to avoid diagnosing a
  // target that has an explicit annotation.
  TargetDeps *getInferredTargetDeps(unsigned targetIndex) {
    auto iter = depsArray.try_emplace(targetIndex, HasAnnotation::Inferred,
                                      targetKindForIndex(targetIndex),
                                      sourceIndexCap()).first;
    auto &deps = iter->second;
    return deps.hasAnnotation() ? nullptr : &deps;
  }

  void inferDependency(unsigned targetIndex, unsigned sourceIndex,
                       LifetimeDependenceKind kind) {
    auto targetDeps = getInferredTargetDeps(targetIndex);
    if (!targetDeps)
      return;
    targetDeps->addIfNew(sourceIndex, kind);
  }

  void inferInoutDependency(unsigned paramIndex) {
    auto iter =
      depsArray.try_emplace(paramIndex, HasAnnotation::Inferred,
                            TargetKind::Inout, sourceIndexCap()).first;
    // An immortal specifier erases any inferred inout dependency;
    // other annotations do not.
    if (!iter->second.hasImmortalSpecifier) {
      iter->second.addIfNew(paramIndex, LifetimeDependenceKind::Inherit);
    }
  }

  // Allocate LifetimeDependenceInfo in the ASTContext. Initialize it by
  // copying heap-allocated TargetDeps fields into ASTContext allocations
  // (e.g. convert SmallBitVector to IndexSubset).
  std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
  initializeDependenceInfoArray(ASTContext &ctx) const {
    if (depsArray.empty()) {
      return std::nullopt;
    }
    // Inference might attempt to infer a target, but fail leaving the source
    // indices empty.
    SmallVector<LifetimeDependenceInfo, 4> lifetimeDependencies;
    for (auto &idxAndDeps : depsArray) {
      unsigned targetIndex = idxAndDeps.first;
      auto &deps = idxAndDeps.second;
      if (deps.empty())
        continue;

      IndexSubset *inheritIndices = nullptr;
      if (deps.inheritIndices.any()) {
        inheritIndices = IndexSubset::get(ctx, deps.inheritIndices);
        ASSERT(!deps.hasImmortalSpecifier || deps.isInout() &&
               "cannot combine immortal lifetime with parameter dependency");
      }
      IndexSubset *scopeIndices = nullptr;
      if (deps.scopeIndices.any()) {
        scopeIndices = IndexSubset::get(ctx, deps.scopeIndices);
        ASSERT(!deps.hasImmortalSpecifier || deps.isInout() &&
               "cannot combine immortal lifetime with parameter dependency");
      }
      lifetimeDependencies.push_back(LifetimeDependenceInfo(
          /*inheritLifetimeParamIndices*/ inheritIndices,
          /*scopeLifetimeParamIndices*/ scopeIndices, targetIndex,
          /*hasImmortalSpecifier*/ deps.hasImmortalSpecifier,
          /*isFromAnnotation*/ deps.hasAnnotation()));
    }
    if (lifetimeDependencies.empty()) {
      return std::nullopt;
    }
    return ctx.AllocateCopy(lifetimeDependencies);
  }
};

/// Diagnostics for ~Escpable types in function signatures. This lowers
/// @_lifetime attributes to the SILFunction's lifetime dependencies and
/// implements the lifetime inferrence rules.
class LifetimeDependenceChecker {
  using TargetDeps = LifetimeDependenceBuilder::TargetDeps;
  using Param = AnyFunctionType::Param;

  SmallVector<LifetimeEntry *, 2> lifetimeEntries;
  struct ParamInfo {
    Param param;
    unsigned index;
    SourceLoc loc;
    Type typeInContext;

    Type getInterfaceType() const { return param.getPlainType(); }

    StringRef name() const { return param.getInternalLabel().str(); }
  };
  SmallVector<ParamInfo, 4> parameterInfos;

  /// The AbstractFunctionDecl, if one is being checked. Otherwise nullptr.
  AbstractFunctionDecl *_Nullable afd;
  ASTContext &ctx;

  // The source file the function being checked was declared in, if present.
  SourceFile const *_Nullable sourceFile;

  ProtocolDecl *escapableDecl;

  GenericEnvironment *_Nullable genericEnv;

  // 'resultIndex' is a pseudo-parameter-index used by LifetimeDependenceInfo to
  // represent the function result.
  const unsigned resultIndex;

  // The result or yield type of the function being checked in its generic
  // environment.
  Type resultTy;

  SourceLoc returnLoc;

  // A parameter corresponding to the implicit self declaration of
  // the function, if it has one. Otherwise, std::nullopt.
  std::optional<ParamInfo> implicitSelfParamInfo;

  LifetimeDependenceBuilder depBuilder;

  bool const isImplicit;
  bool const isInit;
  bool const hasUnsafeNonEscapableResult;

  // True if lifetime diganostics have already been performed. Avoids redundant
  // diagnostics, and allows bypassing diagnostics for special cases.
  bool performedDiagnostics = false;

public:
  static unsigned getResultIndex(AbstractFunctionDecl *afd) {
    return afd->hasImplicitSelfDecl()
      ? (unsigned)(afd->getParameters()->size() + 1)
      : (unsigned)afd->getParameters()->size();
  }

  static unsigned getResultIndex(EnumElementDecl *eed) {
    auto *paramList = eed->getParameterList();
    return paramList ? (unsigned)(paramList->size() + 1) : 1;
  }

  static Type getResultOrYieldInterface(DeclContext *functionDC) {
    if (auto *accessor = dyn_cast<AccessorDecl>(functionDC);
        accessor && accessor->isCoroutine()) {
      return accessor->getStorage()->getValueInterfaceType();
    }
    if (auto fn = dyn_cast<FuncDecl>(functionDC)) {
      return fn->getResultInterfaceType();
    }
    auto ctor = cast<ConstructorDecl>(functionDC);
    return ctor->getResultInterfaceType();
  }

  static SourceLoc getReturnLoc(AbstractFunctionDecl *afd) {
    auto resultTypeRepr = afd->getResultTypeRepr();
    return resultTypeRepr ? resultTypeRepr->getLoc() : afd->getLoc();
  }

  static std::optional<ParamInfo> getSelfParamInfo(AbstractFunctionDecl *afd) {
    auto *selfDecl = afd->getImplicitSelfDecl();
    if (!selfDecl)
      return std::nullopt;

    Type selfInterfaceType = selfDecl->toFunctionParam().getPlainType();
    unsigned selfIndex = afd->getParameters()->size();
    return ParamInfo{selfDecl->toFunctionParam(),
      selfIndex, selfDecl->getLoc(),
      afd->mapTypeIntoEnvironment(selfInterfaceType)};
  }

  const ParamInfo &getParamForIndex(unsigned paramIndex) {
    if (implicitSelfParamInfo && paramIndex == implicitSelfParamInfo->index)
      return *implicitSelfParamInfo;

    assert(paramIndex < parameterInfos.size() && "unexpected result index");
    return parameterInfos[paramIndex];
  }

  Type getEnvTypeForIndex(unsigned paramOrResultIndex) {
    if (paramOrResultIndex == resultIndex)
      return resultTy;

    if (implicitSelfParamInfo &&
        paramOrResultIndex == implicitSelfParamInfo->index)
      return implicitSelfParamInfo->typeInContext;

    return parameterInfos[paramOrResultIndex].typeInContext;
  }

private:
  static auto collectDeclLifetimeEntries(DeclAttributes const &attrs) {
    decltype(lifetimeEntries) lifetimeEntries;
    for (auto attr : attrs.getAttributes<LifetimeAttr>()) {
      lifetimeEntries.push_back(attr->getLifetimeEntry());
    }
    return lifetimeEntries;
  }

  static auto collectFunctionTypeLifetimeEntries(
    ArrayRef<LifetimeTypeAttr *> lifetimeAttrs) {
    decltype(lifetimeEntries) lifetimeEntries;
    for (auto *attr : lifetimeAttrs) {
      lifetimeEntries.push_back(attr->getLifetimeEntry());
    }
    return lifetimeEntries;
  }

  static auto collectDeclParameterInfo(ParameterList const *params,
                                       DeclContext *DC) {
    decltype(parameterInfos) parameterInfos;
    for (auto [index, param] : enumerate(*params)) {
      parameterInfos.push_back(
        {param->toFunctionParam(), (unsigned)index, param->getLoc(),
         DC->mapTypeIntoEnvironment(param->getInterfaceType())});
    }
    return parameterInfos;
  }

  static auto collectFunctionTypeParameterInfo(FunctionTypeRepr *funcRepr,
                                               AnyFunctionType *funcType,
                                               GenericEnvironment *env) {
    decltype(parameterInfos) parameterInfos;

    // We only ever use the second names of function type parameters for
    // lifetimes.
    ArrayRef<Param> params = funcType->getParams();
    ArrayRef<TupleTypeReprElement> argReprs =
      funcRepr->getArgsTypeRepr()->getElements();

    assert(params.size() == argReprs.size());
    for (auto [index, param] : enumerate(params)) {
      auto const &arg = argReprs[index];
      parameterInfos.push_back(
        {param, (unsigned)index,
           // If an argument has no second name, use the location of its type.
           arg.SecondNameLoc.isValid() ? arg.SecondNameLoc : arg.Type->getLoc(),
           GenericEnvironment::mapTypeIntoEnvironment(
               env, param.getPlainType())});
    }
    return parameterInfos;
  }

public:
  LifetimeDependenceChecker(AbstractFunctionDecl *afd)
      : lifetimeEntries(collectDeclLifetimeEntries(afd->getAttrs())),
        parameterInfos(collectDeclParameterInfo(afd->getParameters(), afd)),
        afd(afd), ctx(afd->getDeclContext()->getASTContext()),
        sourceFile(afd->getParentSourceFile()),
        escapableDecl(ctx.getProtocol(
            swift::getKnownProtocolKind(InvertibleProtocolKind::Escapable))),
        genericEnv(afd->getGenericEnvironment()),
        resultIndex(getResultIndex(afd)),
        resultTy(afd->mapTypeIntoEnvironment(getResultOrYieldInterface(afd))),
        returnLoc(getReturnLoc(afd)),
        implicitSelfParamInfo(getSelfParamInfo(afd)),
        depBuilder(resultIndex),
        isImplicit(afd->isImplicit()),
        isInit(isa<ConstructorDecl>(afd)),
        hasUnsafeNonEscapableResult(
            afd->getAttrs().hasAttribute<UnsafeNonEscapableResultAttr>()) {}

  LifetimeDependenceChecker(FunctionTypeRepr *funcRepr,
                            AnyFunctionType *funcType,
                            ArrayRef<LifetimeTypeAttr *> lifetimeAttrs,
                            DeclContext *dc, GenericEnvironment *env)
      : lifetimeEntries(collectFunctionTypeLifetimeEntries(lifetimeAttrs)),
        parameterInfos(
          collectFunctionTypeParameterInfo(funcRepr, funcType, env)),
        afd(nullptr), ctx(funcType->getASTContext()),
        sourceFile(dc->getParentSourceFile()),
        escapableDecl(ctx.getProtocol(
            swift::getKnownProtocolKind(InvertibleProtocolKind::Escapable))),
        genericEnv(env),
        resultIndex(funcType->getParams().size()),
        resultTy(
          GenericEnvironment::mapTypeIntoEnvironment(env,
                                                     funcType->getResult())),
        returnLoc(funcRepr->getResultTypeRepr()->getLoc()),
        implicitSelfParamInfo(std::nullopt),
        depBuilder(resultIndex),
        isImplicit(false),
        isInit(false),
        hasUnsafeNonEscapableResult(false) {}

  std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
  currentDependencies() const {
    return depBuilder.initializeDependenceInfoArray(ctx);
  }

  /// Perform lifetime dependence checks for a function type.
  std::optional<llvm::ArrayRef<LifetimeDependenceInfo>> checkFuncType() {
    // Check if the function type contains any types for which we cannot
    // determine Escapability. We cannot perform lifetime inference for such
    // types, or check the correctness of lifetime annotations on them, so bail
    // out. Emit diagnostics if there are any lifetime attributes.
    //
    // We could make this more granular, only bailing out if a lifetime source
    // or target contains an unbound generic, but these cases seem too niche to
    // be worth the effort.
    //
    // Even if there were no explicit lifetime entries, we still need to
    // diagnose failed inference if a parameter or the result was ~Escapable.
    const auto isNonEscapableSafe = [](Type t) {
      return !isTypeUnknown(t) && isDiagnosedNonEscapable(t);
    };
    const bool shouldDiagnose =
        !lifetimeEntries.empty() ||
        llvm::any_of(parameterInfos,
                     [&](const ParamInfo &paramInfo) {
                       return isNonEscapableSafe(paramInfo.typeInContext);
                     }) ||
        isNonEscapableSafe(resultTy);
    bool unknownTypeFound = false;
    for (const auto &paramInfo : parameterInfos) {
      if (isTypeUnknown(paramInfo.typeInContext)) {
        unknownTypeFound = true;
        if (shouldDiagnose)
          diagnose(paramInfo.loc, diag::lifetime_dependence_unknown_type,
                   "parameter");
      }
    }
    if (isTypeUnknown(resultTy)) {
      unknownTypeFound = true;
      if (shouldDiagnose)
        diagnose(returnLoc, diag::lifetime_dependence_unknown_type, "result");
    }

    if (unknownTypeFound)
      return std::nullopt;

    return checkCommon();
  }

  /// Perform lifetime dependence checks for a function declaration.
  std::optional<llvm::ArrayRef<LifetimeDependenceInfo>> checkFuncDecl() {
    assert(nullptr != afd && (isa<FuncDecl>(afd) || isa<ConstructorDecl>(afd)));
    assert(depBuilder.empty());

    // Handle Builtins first because, even though Builtins require
    // LifetimeDependence, we don't force the experimental feature
    // to be enabled when importing the Builtin module.
    if (afd->isImplicit() && afd->getModuleContext()->isBuiltinModule()) {
      inferBuiltin();
      return currentDependencies();
    }

    return checkCommon();
  }

  std::optional<llvm::ArrayRef<LifetimeDependenceInfo>> checkCommon() {
    if (!ctx.LangOpts.hasFeature(Feature::LifetimeDependence)
        && !ctx.LangOpts.hasFeature(Feature::Lifetimes)
        && !ctx.SourceMgr.isImportMacroGeneratedLoc(returnLoc)) {

      // Infer inout dependencies without requiring a feature flag. On
      // returning, 'depBuilder' contains any inferred dependencies. This does
      // not issue any diagnostics because using unsupported lifetime features
      // may generate a different diagnostic when the feature flag is disabled.
      inferMutatingSelf();
      inferInoutParams();

      diagnoseMissingResultDependencies(
        diag::lifetime_dependence_feature_required_return.ID);
      diagnoseMissingSelfDependencies(
        diag::lifetime_dependence_feature_required_mutating.ID);
      diagnoseMissingInoutDependencies(
        diag::lifetime_dependence_feature_required_inout.ID);

      return currentDependencies();
    }

    if (!lifetimeEntries.empty()) {
      initializeAttributeDeps();
      if (performedDiagnostics)
        return std::nullopt;
    }
    // Methods or functions with @_unsafeNonescapableResult do not require
    // lifetime annotation and do not infer any lifetime dependency.
    if (hasUnsafeNonEscapableResult) {
      return currentDependencies();
    }

    inferOrDiagnose();

    // If precise diagnostics were already issued, bypass
    // diagnoseMissingDependencies to avoid redundant diagnostics.
    if (!performedDiagnostics) {
      diagnoseMissingResultDependencies(
        diag::lifetime_dependence_cannot_infer_return.ID);
      diagnoseMissingSelfDependencies(
        diag::lifetime_dependence_cannot_infer_mutating.ID);
      diagnoseMissingInoutDependencies(
        diag::lifetime_dependence_cannot_infer_inout.ID);
    }
    return currentDependencies();
  }

  static std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
  checkEnumElementDecl(EnumElementDecl *eed) {
    auto const resultIndex = getResultIndex(eed);
    LifetimeDependenceBuilder depBuilder(resultIndex);
    auto *parentEnum = eed->getParentEnum();
    auto enumType = parentEnum->mapTypeIntoEnvironment(
      parentEnum->getDeclaredInterfaceType());

    // Add early bailout for imported enums.
    if (parentEnum->hasClangNode()) {
      return std::nullopt;
    }

    // Escapable enum, bailout.
    if (!isDiagnosedNonEscapable(enumType)) {
      return std::nullopt;
    }
    auto *params = eed->getParameterList();
    // No payload, bailout.
    if (!params) {
      return std::nullopt;
    }

    TargetDeps *resultDeps = depBuilder.getInferredTargetDeps(resultIndex);
    ASSERT(resultDeps && "enum declaration has a lifetime attribute");

    // Add all indices of ~Escapable parameters as lifetime dependence sources.
    for (size_t i = 0; i < params->size(); i++) {
      auto paramType = params->get(i)->getTypeInContext();
      if (!isDiagnosedNonEscapable(paramType)) {
        continue;
      }
      resultDeps->inheritIndices.set(i);
    }
    return depBuilder.initializeDependenceInfoArray(eed->getASTContext());
  }

protected:
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(
    SourceLoc Loc, Diag<ArgTypes...> ID,
    typename detail::PassArgument<ArgTypes>::type... Args) {
    performedDiagnostics = true;
    return ctx.Diags.diagnose(Loc, ID, std::move(Args)...);
  }

  // For initializers, the implicit self parameter is ignored and instead shows
  // up as the result type.
  //
  // Note: Do not use this to reserve the self parameter index.
  // LifetimeDependenceInfo always reserves an extra formal parameter
  // index for hasImplicitSelfDecl(), even for initializers. During function
  // type lowering, it is mapped to the metatype parameter. Without reserving
  // the extra formal self parameter, a dependency targeting the formal result
  // index would incorrectly target the SIL metatype parameter.
  bool hasImplicitSelfParam() const {
    return !isInit && implicitSelfParamInfo.has_value();
  }

  // In SIL, implicit initializers and accessors become explicit.
  bool isImplicitOrSIL() const {
    if (isImplicit) {
      return true;
    }
    // TODO: remove this check once SIL prints @lifetime.
    if (sourceFile) {
      // The AST printer makes implicit initializers explicit, but does not
      // print the @lifetime annotations. Until that is fixed, avoid
      // diagnosing this as an error.
      if (sourceFile->Kind == SourceFileKind::SIL) {
        return true;
      }
    }
    return false;
  }

  bool isInterfaceFile() const {
    // TODO: remove this check once all compilers that are rev-locked to the
    // stdlib print the 'copy' dependence kind in the interface (Aug '25)
    if (sourceFile && sourceFile->Kind == SourceFileKind::Interface) {
      return true;
    }
    return false;
  }

  // Infer ambiguous cases for backward compatibility.
  bool useLazyInference() const {
    return isInterfaceFile()
      || ctx.LangOpts.EnableExperimentalLifetimeDependenceInference;
  }

  // ==========================================================================
  // MARK: Catch-all diagnostics for missing attributes and inferrence rules.
  // ==========================================================================

  std::string diagnosticQualifier() const {
    if (isImplicit) {
      if (isInit) {
        return "an implicit initializer";
      }
      if (auto *ad = dyn_cast_or_null<AccessorDecl>(afd)) {
        std::string qualifier = "the '";
        qualifier += accessorKindName(ad->getAccessorKind());
        qualifier += "' accessor";
        return qualifier;
      }
    }
    if (implicitSelfParamInfo.has_value()) {
      if (isInit) {
        return "an initializer";
      }
      if (implicitSelfParamInfo->param.isInOut()) {
        return "a mutating method";
      }
      return "a method";
    }
    return "a function";
  }
  
  // Ensure that dependencies exist for any return value or inout parameter that
  // needs one. Always runs before the checker completes if no other diagnostics
  // were issued.
  void diagnoseMissingResultDependencies(DiagID diagID) {
    if (!isDiagnosedNonEscapable(resultTy)) {
      return;
    }
    if (!depBuilder.hasTargetDeps(resultIndex)) {
      ctx.Diags.diagnose(returnLoc, diagID,
                         {StringRef(diagnosticQualifier())});
    }
  }

  // Ensure that dependencies exist for any mutating self value. Always runs
  // before the checker completes if no other diagnostics were issued. For
  // initializers, the inout self parameter is actually considered the result
  // type so is not handled here.
  void diagnoseMissingSelfDependencies(DiagID diagID) {
    if (!hasImplicitSelfParam()) {
      return;
    }
    if (!implicitSelfParamInfo->param.isInOut()) {
      return;
    }
    if (!isDiagnosedNonEscapable(implicitSelfParamInfo->typeInContext)) {
      return;
    }
    if (!depBuilder.hasTargetDeps(implicitSelfParamInfo->index)) {
      ctx.Diags.diagnose(implicitSelfParamInfo->loc, diagID,
                         {StringRef(diagnosticQualifier())});
    }
  }
  
  void diagnoseMissingInoutDependencies(DiagID diagID) {
    unsigned paramIndex = 0;
    for (auto &paramInfo : parameterInfos) {
      SWIFT_DEFER { paramIndex++; };
      if (!paramInfo.param.isInOut()) {
        continue;
      }
      if (!isDiagnosedNonEscapable(paramInfo.typeInContext)) {
        continue;
      }
      if (!depBuilder.hasTargetDeps(paramIndex)) {
        ctx.Diags.diagnose(paramInfo.loc, diagID,
                           {StringRef(diagnosticQualifier()),
                            paramInfo.name()});
        if (diagID == diag::lifetime_dependence_cannot_infer_inout.ID) {
          ctx.Diags.diagnose(
              paramInfo.loc,
              diag::lifetime_dependence_cannot_infer_inout_suggest,
              paramInfo.name());
        }
      }
    }
  }

  // ==========================================================================
  // MARK: attribute parsing and inference helpers
  // ==========================================================================

  // Attribute parsing helper.
  bool isCompatibleWithOwnership(ParsedLifetimeDependenceKind kind,
                                 Type paramType, ValueOwnership loweredOwnership,
                                 bool isInterfaceFile = false) const {
    if (kind == ParsedLifetimeDependenceKind::Inherit) {
      return true;
    }

    if (kind == ParsedLifetimeDependenceKind::Borrow) {
      // An owned/consumed BitwiseCopyable value can be effectively borrowed
      // because its lifetime can be indefinitely extended.
      if (loweredOwnership == ValueOwnership::Owned &&
          isBitwiseCopyable(paramType, ctx)) {
        return true;
      }
      if (isInterfaceFile) {
        return loweredOwnership == ValueOwnership::Shared ||
               loweredOwnership == ValueOwnership::InOut;
      }
      return loweredOwnership == ValueOwnership::Shared;
    }
    assert(kind == ParsedLifetimeDependenceKind::Inout);
    return loweredOwnership == ValueOwnership::InOut;
  }

  // Inferrence helper.
  bool isCompatibleWithOwnership(LifetimeDependenceKind kind,
                                 ParamInfo const &paramInfo) const {
    if (kind == LifetimeDependenceKind::Inherit) {
      return true;
    }

    auto paramType = paramInfo.typeInContext;
    auto loweredOwnership = getLoweredOwnership(paramInfo.param);
    // Lifetime dependence always propagates through temporary BitwiseCopyable
    // values, even if the dependence is scoped.
    if (isBitwiseCopyable(paramType, ctx)) {
      return true;
    }
    assert(kind == LifetimeDependenceKind::Scope);
    return loweredOwnership == ValueOwnership::Shared ||
           loweredOwnership == ValueOwnership::InOut;
  }


  // ==========================================================================
  // MARK: Same-type inference
  // ==========================================================================

  /// Is 'sourceEnvType' Escapable under any of the conformance requirements in
  /// 'targetReqs'?
  ///
  /// If true, we will infer a default dependency because a lifetime requirement
  /// in the source is always present in the target. The source may have
  /// additional lifetime requirements which are not copied to the
  /// target. Conversely, the target may depend on multiple sources.
  ///
  /// Example:
  ///
  ///    struct NE1: ~Escapable {}
  ///    struct NE2: ~Escapable {}
  ///    func foo(arg: NE1?) -> NE1 // DEFAULT: @_lifetime(copy arg)
  ///    func foo(arg: NE1?) -> NE2 // ERROR: missing annotation
  ///
  /// Invariant: hasSameTypeRequirement can only return true when
  /// hasGuaranteedLifetime is also true.
  ///
  bool hasSameTypeRequirement(Type sourceEnvType,
                              const llvm::SmallDenseSet<Type> &targetReqs) {

    LLVM_DEBUG(llvm::dbgs() << "\nSource Type: " << sourceEnvType << "\n");
    SmallVector<Type, 4> sourceReqs;
    if (!collectRequiredTypesForNonEscapable(sourceEnvType, sourceReqs)) {
      return false;
    }
    if (sourceReqs.empty()) {
      // The source is unconditionally Escapable.
      return false;
    }
    LLVM_DEBUG(llvm::dbgs() << "\nSource reqs:\n";
               for (auto sourceReq : sourceReqs) {
                 sourceReq.dump(llvm::dbgs());
               });
    return llvm::any_of(sourceReqs, [&](Type sourceReq) {
      return targetReqs.contains(sourceReq);
    });
  }

  bool collectRequiredTypesForNonEscapable(Type envType,
                                           SmallVectorImpl<Type> &inverseReqs) {
    if (envType->hasError()) {
      LLVM_DEBUG(
        llvm::dbgs() << "Error Type: " << envType << "\n");
      return false;
    }
    auto confRef = lookupConformance(envType, escapableDecl);
    if (confRef.isInvalid()) {
      LLVM_DEBUG(
        llvm::dbgs() << "Outer non-Escapable Type: " << envType << "\n");
      inverseReqs.push_back(envType);
      return true;
    }
    return collectRequiredTypesRecursively(confRef, inverseReqs);
  }

  bool collectRequiredTypesRecursively(ProtocolConformanceRef confRef,
                                       SmallVectorImpl<Type> &inverseReqs) {

    LLVM_DEBUG(llvm::dbgs() << "Collect for conformance:\n";
               confRef.print(llvm::dbgs());
               llvm::dbgs() << "\n");

    if (confRef.isAbstract()) {
      // Abstract conformances unconditionally conform.
      return true;
    }
    if (confRef.isPack()) {
      // Parameters packs cannot yet suppress Escapable, so bailout.
      return false;
      /* TODO:
      PackType *packType = confRef.getPack()->getType();
      for (auto subConfRef : confRef.getPack()->getPatternConformances()) {
        if (!collectRequiredTypesRecursively(subConfRef, inverseReqs)) {
          return false;
        }
      }
      return true;
      */
    }
    if (confRef.isConcrete()) {
      ProtocolConformance *conformance = confRef.getConcrete();
      switch (conformance->getKind()) {
      case ProtocolConformanceKind::Self:
      case ProtocolConformanceKind::Builtin:
      case ProtocolConformanceKind::Normal:
        // These types conform without requiring another type.
        return true;
      case ProtocolConformanceKind::Inherited:
        // InheritedConformance is not allowed for suppressible protocols.
        return true;
      case ProtocolConformanceKind::Specialized:
        // fall through to the recursive implementation.
        break;
      }
      SubstitutionMap subMap = conformance->getSubstitutionMap();
      // Use the 'subMap' signature, not the conformance signature.
      GenericSignature subSig = subMap.getGenericSignature();
      auto subConformances = subMap.getConformances();
      for (auto &req : subSig.getRequirements()) {
        if (req.getKind() != RequirementKind::Conformance)
          continue;

        // GenericSignature's conformance Requirements line up with
        // subMap.getConformances().
        ProtocolConformanceRef subConfRef = subConformances.front();
        subConformances = subConformances.slice(1);
        if (subConfRef.isInvalid()) {
          Type envType = req.getFirstType().subst(subMap);
          LLVM_DEBUG(llvm::dbgs() << "Nested non-Escapable Type: "
                     << envType << "\n");
          inverseReqs.push_back(envType);
          continue;
        }
        if (subConfRef.getProtocol()
            ->isSpecificProtocol(KnownProtocolKind::Escapable)) {
          if (!collectRequiredTypesRecursively(subConfRef, inverseReqs)) {
            return false;
          }
        }
      }
      return true;
    }
    // unknown conformance kind
    return false;
  }

  // ==========================================================================
  // MARK: @_lifetime attribute semantics
  // ==========================================================================

  /// Resolve the dependency kind based on the descriptor syntax and check that
  /// it is consistent with parameter ownership.
  std::optional<LifetimeDependenceKind>
  resolveSourceDescriptor(LifetimeDescriptor descriptor,
                          ParamInfo const &sourceParam,
                          unsigned targetIndex) {
    auto const loc = descriptor.getLoc();
    auto const type = sourceParam.typeInContext;
    auto const parsedLifetimeKind =
      descriptor.getParsedLifetimeDependenceKind();
    auto const loweredOwnership = getLoweredOwnership(sourceParam.param);

    switch (parsedLifetimeKind) {
    case ParsedLifetimeDependenceKind::Default: {
      if (type->isEscapable()) {
        if (loweredOwnership == ValueOwnership::Shared ||
            loweredOwnership == ValueOwnership::InOut) {
          return LifetimeDependenceKind::Scope;
        }
        diagnose(
            loc,
            diag::lifetime_dependence_cannot_use_default_escapable_consuming,
            getOwnershipSpelling(loweredOwnership));
        return std::nullopt;
      }
      if (useLazyInference()) {
        return LifetimeDependenceKind::Inherit;
      }
      diagnose(loc, diag::lifetime_dependence_cannot_infer_kind,
               diagnosticQualifier(), descriptor.getString());
      return std::nullopt;
    }

    case ParsedLifetimeDependenceKind::Borrow: LLVM_FALLTHROUGH;
    case ParsedLifetimeDependenceKind::Inout: {
      // @lifetime(borrow x) is valid only for borrowing parameters.
      // @lifetime(&x) is valid only for inout parameters.
      if (isCompatibleWithOwnership(parsedLifetimeKind, type, loweredOwnership,
                                    isInterfaceFile())) {
        return LifetimeDependenceKind::Scope;
      }
      diagnose(loc,
               diag::lifetime_dependence_parsed_borrow_with_ownership,
               getNameForParsedLifetimeDependenceKind(parsedLifetimeKind),
               getOwnershipSpelling(loweredOwnership));
      switch (loweredOwnership) {
      case ValueOwnership::Shared:
        diagnose(loc,
                 diag::lifetime_dependence_parsed_borrow_with_ownership_fix,
                 "borrow ", descriptor.getString());
        break;
      case ValueOwnership::InOut:
        diagnose(loc,
                 diag::lifetime_dependence_parsed_borrow_with_ownership_fix,
                 "&", descriptor.getString());
        break;
      case ValueOwnership::Owned:
      case ValueOwnership::Default:
        break;
      }
      return std::nullopt;
    }
    case ParsedLifetimeDependenceKind::Inherit: {
      if (checkNonEscapableSource(descriptor, sourceParam, loweredOwnership))
        return LifetimeDependenceKind::Inherit;

      return std::nullopt;
    }
  }
  }

  // @lifetime(copy x) is only valid if 'x' has a ~Escapable type.
  bool checkNonEscapableSource(LifetimeDescriptor descriptor,
                               const ParamInfo &sourceParam,
                               ValueOwnership ownership) {
    // Allow recompiling old interfaces with a newer compiler.
    if (isInterfaceFile())
      return true;

    // @_hasUnsafeNonEscapableResult bypasses requires-escapable.
    // e.g. _overrideLifetime(_:, copying:)
    if (hasUnsafeNonEscapableResult) {
      return true;
    }

    // Does source have a guaranteed lifetime assuming the target is
    // non-Escapable?

    // Get the contextual source and target types.
    auto loc = descriptor.getLoc();
    if (sourceParam.typeInContext->isEscapable()) {
      diagnose(loc, diag::lifetime_dependence_invalid_inherit_escapable_type);
      diagnose(loc,
               diag::lifetime_escapable_source_requires_escapable_note,
               (ownership == ValueOwnership::InOut) ? "&" : "borrow ",
               descriptor.getString());
      return false;
    }
    return true;
  }

  // Finds the Param* and its index from a LifetimeDescriptor or returns
  // nullptr.
  ParamInfo const *getParamFromDescriptor(LifetimeDescriptor descriptor) {
    switch (descriptor.getDescriptorKind()) {
    case LifetimeDescriptor::DescriptorKind::Named: {
      const ParamInfo *candidate = llvm::find_if(
          parameterInfos, [name = descriptor.getName()](auto const &paramInfo) {
            return paramInfo.param.getInternalLabel() == name;
          });

      if (parameterInfos.end() == candidate) {
        diagnose(descriptor.getLoc(),
                 diag::lifetime_dependence_invalid_param_name,
                 descriptor.getName());
        return nullptr;
      }
      return candidate;
    }
    case LifetimeDescriptor::DescriptorKind::Ordered: {
      auto paramIndex = descriptor.getIndex();
      if (paramIndex >= parameterInfos.size()) {
        diagnose(descriptor.getLoc(),
                 diag::lifetime_dependence_invalid_param_index,
                 paramIndex);
        return nullptr;
      }
      return &parameterInfos[paramIndex];
    }
    case LifetimeDescriptor::DescriptorKind::Self: {
      if (!hasImplicitSelfParam()) {
        diagnose(descriptor.getLoc(),
                 diag::lifetime_dependence_invalid_self_in_static);
        return nullptr;
      }
      if (isInit) {
        diagnose(descriptor.getLoc(),
                 diag::lifetime_dependence_invalid_self_in_init);
        return nullptr;
      }
      return &implicitSelfParamInfo.value();
    }
    }
  }

  // Initialize 'depBuilder' based on the function's @_lifetime attributes.
  void initializeAttributeDeps() {
    for (LifetimeEntry *entry : lifetimeEntries) {
      auto targetDescriptor = entry->getTargetDescriptor();
      unsigned targetIndex;
      if (targetDescriptor.has_value()) {
        auto targetParam = getParamFromDescriptor(*targetDescriptor);
        if (!targetParam) {
          return;
        }
        // TODO: support dependencies on non-inout parameters.
        targetIndex = targetParam->index;
        if (!targetParam->param.isInOut()) {
          ctx.Diags.diagnose(targetParam->loc,
                             diag::lifetime_parameter_requires_inout,
                             targetDescriptor->getString());
        }
        if (isDiagnosedEscapable(targetParam->typeInContext)) {
          diagnose(targetDescriptor->getLoc(),
                   diag::lifetime_target_requires_nonescapable, "target");
        }
      } else {
        if (isDiagnosedEscapable(resultTy)) {
          diagnose(entry->getLoc(), diag::lifetime_target_requires_nonescapable,
                   "result");
        }
        targetIndex = resultIndex;
      }
      TargetDeps *deps = depBuilder.createAnnotatedTargetDeps(targetIndex);
      if (deps == nullptr) {
        diagnose(entry->getLoc(),
                 diag::lifetime_dependence_duplicate_target);
        return;
      }
      for (auto source : entry->getSources()) {
        initializeDescriptorDeps(targetIndex, *deps, source);
      }
    }
  }

  // Get the value ownership of param if it is non-default. Otherwise, compute
  // the lowered value ownership. The supplied Param must be a member of
  // parameters or implicitSelfParamInfo.value().
  ValueOwnership getLoweredOwnership(Param const &param) const {
    auto const ownership = param.getValueOwnership();
    if (ownership != ValueOwnership::Default)
      return ownership;
    if (nullptr != afd && isa<ConstructorDecl>(afd)) {
      return ValueOwnership::Owned;
    }
    if (auto *ad = dyn_cast_or_null<AccessorDecl>(afd)) {
      auto const isSelfParameter = implicitSelfParamInfo.has_value() &&
                                   &param == &(implicitSelfParamInfo->param);
      if (ad->getAccessorKind() == AccessorKind::Set) {
        return isSelfParameter ? ValueOwnership::InOut : ValueOwnership::Owned;
      }
      if (isYieldingMutableAccessor(ad->getAccessorKind())) {
        assert(isSelfParameter);
        return ValueOwnership::InOut;
      }
    }
    return ValueOwnership::Shared;
  }

  // Initialize TargetDeps based on the function's @_lifetime attributes.
  void initializeDescriptorDeps(unsigned targetIndex,
                                TargetDeps &deps,
                                LifetimeDescriptor source) {
    if (source.isImmortalSpecifier()) {
      // Record the immortal dependency even if it is invalid to suppress other diagnostics.
      deps.hasImmortalSpecifier = true;
      auto immortalParam =
          llvm::find_if(parameterInfos, [](auto const &paramInfo) {
            return paramInfo.param.getInternalLabel().is("immortal");
          });

      if (immortalParam != parameterInfos.end()) {
        ctx.Diags.diagnose(immortalParam->loc,
                           diag::lifetime_dependence_immortal_conflict_name);
        return;
      }
      // @_lifetime(target: immortal, copy source) is allowed for inout targets.
      if (!deps.isInout()) {
        if (deps.inheritIndices.any() || deps.scopeIndices.any()) {
          ctx.Diags.diagnose(immortalParam->loc,
                             diag::lifetime_dependence_immortal_alone);
        }
      }
      return;
    }

    const ParamInfo *paramInfo = getParamFromDescriptor(source);
    if (!paramInfo) {
      return;
    }
    unsigned sourceIndex = paramInfo->index;
    auto lifetimeKind =
      resolveSourceDescriptor(source, *paramInfo, targetIndex);
    if (!lifetimeKind.has_value()) {
      return;
    }
    // Don't allow an 'inout' parameter to 'borrow' itself because it is useless
    // and an easy mistake when 'inout' was intended.
    if (lifetimeKind == LifetimeDependenceKind::Scope &&
        paramInfo->param.isInOut() && sourceIndex == targetIndex) {
      diagnose(source.getLoc(),
               diag::lifetime_dependence_cannot_use_parsed_borrow_inout);
      ctx.Diags.diagnose(source.getLoc(),
                         diag::lifetime_dependence_cannot_infer_inout_suggest,
                         paramInfo->name());

      return;
    }
    addDescriptorIndices(deps, source, sourceIndex, *lifetimeKind);
  }

  void addDescriptorIndices(LifetimeDependenceBuilder::TargetDeps &deps,
                            LifetimeDescriptor descriptor,
                            unsigned paramIndexToSet,
                            LifetimeDependenceKind lifetimeKind) {
    // @_lifetime(target: immortal, copy source) is allowed for inout targets.
    if (deps.hasImmortalSpecifier && !deps.isInout()) {
      diagnose(descriptor.getLoc(), diag::lifetime_dependence_immortal_alone);
      return;
    }
    if (deps.inheritIndices.test(paramIndexToSet)
        || deps.scopeIndices.test(paramIndexToSet)) {
      diagnose(descriptor.getLoc(),
               diag::lifetime_dependence_duplicate_param_id);
      return;
    }
    if (lifetimeKind == LifetimeDependenceKind::Inherit) {
      deps.inheritIndices.set(paramIndexToSet);
    } else {
      assert(lifetimeKind == LifetimeDependenceKind::Scope);
      deps.scopeIndices.set(paramIndexToSet);
    }
  }

  // ==========================================================================
  // MARK: Inferrence rules
  // ==========================================================================

  // Infer the kind of dependence that makes sense for reading or writing a
  // stored property (for getters or initializers).
  std::optional<LifetimeDependenceKind>
  inferLifetimeDependenceKind(ParamInfo const &paramInfo) {
    Type paramType = paramInfo.typeInContext;
    if (!paramType->isEscapable()) {
      return LifetimeDependenceKind::Inherit;
    }
    // Lifetime dependence always propagates through temporary BitwiseCopyable
    // values, even if the dependence is scoped.
    if (isBitwiseCopyable(paramType, ctx)) {
      return LifetimeDependenceKind::Scope;
    }
    auto loweredOwnership = getLoweredOwnership(paramInfo.param);
    // It is impossible to depend on a consumed Escapable value (unless it is
    // BitwiseCopyable as checked above).
    if (loweredOwnership == ValueOwnership::Owned) {
      return std::nullopt;
    }
    return LifetimeDependenceKind::Scope;
  }

  // On returning, 'depBuilder' contains any inferred dependencies and
  // 'performedDiagnostics' indicates whether any specific diagnostics were
  // issued.
  void inferOrDiagnose() {
    if (auto accessor = dyn_cast_or_null<AccessorDecl>(afd)) {
      inferAccessor(accessor);
      // Aside from the special cases handled above, accessors are considered
      // regular methods...
    }

    // Infer non-Escapable results.
    if (isDiagnosedNonEscapable(resultTy)) {
      if (isInit && isImplicitOrSIL()) {
        inferImplicitInit();
      } else {
        // Apply the same-type rule before the single parameter rule. The
        // same-type rule does not trigger any diagnostics.
        inferNonEscapableResultOnSameTypeParam();

        if (hasImplicitSelfParam()) {
          // Methods that return a non-Escapable value - single parameter
          // default rule.
          inferNonEscapableResultOnSelf();
        } else {
          // Regular functions and initializers that return a non-Escapable
          // value - single parameter default rule.
          inferNonEscapableResultOnParam();
        }
      }
    }

    // Infer mutating non-Escapable methods (excluding initializers) -
    // `inout` parameter default rule.
    inferMutatingSelf();

    // Infer inout parameters - `inout` parameter default rule.
    inferInoutParams();
  }

  // Infer a dependency to the ~Escapable result from all parameters of the same
  // type. More generally, infer a dependency on any parameter type for which
  // Escapable conformance requires the result type to be Escapable.
  //
  //     @_lifetime(copy a) // OK: Optional<T>: Escapable requires T: Escapable
  //     func foo<T: ~Escapable>(a: T?) -> T {
  //
  void inferNonEscapableResultOnSameTypeParam() {
    // Check that no @_lifetime annotation is present for the function result.
    TargetDeps *targetDeps = depBuilder.getInferredTargetDeps(resultIndex);
    if (!targetDeps)
      return;

    LLVM_DEBUG(llvm::dbgs() << "\nTarget Type: " << resultTy << "\n");
    SmallVector<Type, 4> targetReqList;
    if (!collectRequiredTypesForNonEscapable(resultTy, targetReqList)) {
      // Unable to evaluate conformance requirements.
      return;
    }
    if (targetReqList.empty()) {
      // The target is unconditionally Escapable.
      return;
    }
    LLVM_DEBUG(llvm::dbgs() << "\nTarget reqs:\n";
               for (auto targetReq : targetReqList) {
                 targetReq.dump(llvm::dbgs());
               });
    llvm::SmallDenseSet<Type> targetReqs;
    for (Type targetReq : targetReqList) {
      targetReqs.insert(targetReq);
    }

    // Ignore mutating self. An 'inout' modifier effectively makes the parameter
    // a different type for lifetime inference.
    if (hasImplicitSelfParam() && !implicitSelfParamInfo->param.isInOut()) {
      if (hasSameTypeRequirement(implicitSelfParamInfo->typeInContext,
                                 targetReqs)) {
        targetDeps->inheritIndices.set(implicitSelfParamInfo->index);
      }
    }

    unsigned paramIndex = 0;
    for (auto const &paramInfo : parameterInfos) {
      SWIFT_DEFER { paramIndex++; };

      // Ignore 'inout' parameters. An 'inout' modifier effectively makes the
      // parameter a different type for lifetime inference. An 'inout' parameter
      // defaults to being the source and target of a self-dependency, as
      // covered by the 'inout' rule.
      if (paramInfo.param.isInOut())
        continue;

      if (hasSameTypeRequirement(paramInfo.typeInContext, targetReqs)) {
        targetDeps->inheritIndices.set(paramIndex);
      }
    }
  }

  // Infer dependence for an accessor whose non-escapable result depends on
  // self. This includes _read and _modify.
  //
  // Any accessors not handled here will be handled like a normal method.
  void inferAccessor(AccessorDecl *accessor) {
    if (!hasImplicitSelfParam()) {
      // global accessors have no 'self'.
      return;
    }
    bool nonEscapableSelf =
        isDiagnosedNonEscapable(implicitSelfParamInfo->typeInContext);
    if (nonEscapableSelf && accessor->getImplicitSelfDecl()->isInOut()) {
      // First, infer the dependency of the inout non-Escapable 'self'. This may
      // result in two inferred dependencies for accessors (one targetting
      // selfIndex here, and one targetting resultIndex below).
      inferMutatingAccessor(accessor);
    }
    // Handle synthesized wrappers...
    if (!isImplicitOrSIL() && !useLazyInference())
      return;

    // Infer the result dependency of the result or yielded value on 'self'
    // based on the kind of accessor called by this wrapper accessor.
    if (auto dependenceKind = getImplicitAccessorResultDependence(accessor)) {
      depBuilder.inferDependency(resultIndex, implicitSelfParamInfo->index,
                                 *dependenceKind);
    }
  }

  // Infer a mutating accessor's non-Escapable 'self' dependencies.
  void inferMutatingAccessor(AccessorDecl *accessor) {
    switch (accessor->getAccessorKind()) {
    case AccessorKind::Read:
    case AccessorKind::YieldingBorrow:
    case AccessorKind::Modify:
    case AccessorKind::YieldingMutate:
      // '_read' and '_modify' are inferred like regular methods. The yielded
      // value depends on the single 'self' parameter. Additionally, '_modify'
      // infers 'self' as an 'inout' parameter.
      //
      // The caller of _modify will ensure that the modified 'self', passed as
      // 'inout', depends on any value stored to the yielded address.
      //
      // Note that the AST generates a _modify for stored properties even though
      // it won't be emitted.
      break;
    case AccessorKind::Set: {
      const unsigned newValIdx = 0;
      auto const &paramInfo = parameterInfos[newValIdx];
      Type paramTypeInContext = paramInfo.typeInContext;
      if (paramTypeInContext->hasError()) {
        return;
      }
      depBuilder.inferInoutDependency(implicitSelfParamInfo->index);

      // The 'newValue' dependence kind must match the getter's dependence kind
      // because the generated '_modify' accessor composes the getter's result
      // with the setter's 'newValue'. In particular, if the getter's result is
      // Escapable then the getter does not have any lifetime dependency, so the
      // setter cannot depend on 'newValue'.
      if (!paramTypeInContext->isEscapable()) {
        depBuilder.inferDependency(implicitSelfParamInfo->index, newValIdx,
                                   LifetimeDependenceKind::Inherit);
      }
      break;
    }
    case AccessorKind::MutableAddress:
      if (useLazyInference()) {
        // Assume that a mutating method does not depend on its parameters.
        // Currently only for backward interface compatibility. Even though this
        // is the only useful dependence (a borrow of self is possible but not
        // useful), explicit annotation is required for now to confirm that the
        // mutated self cannot depend on anything stored at this address.
        depBuilder.inferInoutDependency(implicitSelfParamInfo->index);
      }
      break;
    default:
      // Unknown mutating accessor.
      break;
    }
  }

  // Implicit accessors must be consistent with the accessor that they
  // wrap. Otherwise, the sythesized implementation will report a diagnostic
  // error.
  std::optional<LifetimeDependenceKind>
  getImplicitAccessorResultDependence(AccessorDecl *accessor) {
    if (!isDiagnosedNonEscapable(resultTy))
      return std::nullopt;

    std::optional<AccessorKind> wrappedAccessorKind = std::nullopt;
    switch (accessor->getAccessorKind()) {
    case AccessorKind::Read:
    case AccessorKind::YieldingBorrow:
    case AccessorKind::Modify:
    case AccessorKind::YieldingMutate:
      // read/modify are syntesized as calls to the getter.
      wrappedAccessorKind = AccessorKind::Get;
      break;
    case AccessorKind::Get:
      // getters are synthesized as access to a stored property.
      break;
    default:
      // Unknown synthesized accessor.
      // Setters are handled in inferMutatingAccessor() because they don't
      // return a value.
      return std::nullopt;
    }
    if (wrappedAccessorKind) {
      auto *var = cast<AbstractStorageDecl>(accessor->getStorage());
      for (auto *wrappedAccessor : var->getAllAccessors()) {
        if (wrappedAccessor->isImplicit())
          continue;
        if (wrappedAccessor->getAccessorKind() == wrappedAccessorKind) {
          if (auto deps = wrappedAccessor->getLifetimeDependencies()) {
            for (auto &dep : *deps) {
              if (dep.getTargetIndex() != resultIndex)
                continue;
              if (dep.checkInherit(implicitSelfParamInfo->index))
                return LifetimeDependenceKind::Inherit;
              if (dep.checkScope(implicitSelfParamInfo->index))
                return LifetimeDependenceKind::Scope;
            }
          }
        }
      }
    }
    // Either a Get or Modify without any wrapped accessor. Handle these like a
    // read of the stored property.
    return inferLifetimeDependenceKind(*implicitSelfParamInfo);
  }

  // Infer implicit initialization. A non-Escapable initializer parameter can
  // always be inferred, similar to an implicit setter, because the
  // implementation is simply an assignment to stored property. Escapable
  // parameters are ambiguous: they may either be borrowed or
  // non-dependent. non-Escapable types often have incidental integer fields
  // that are unrelated to lifetime. Avoid inferring any dependency on Escapable
  // parameters unless it is the (unambiguously borrowed) sole parameter.
  void inferImplicitInit() {
    if (parameterInfos.size() == 0) {
      // Empty ~Escapable types can be implicitly initialized without any
      // dependencies. In SIL, implicit initializers become explicit. Set
      // performedDiagnostics here to bypass normal dependence checking without
      // raising an error.
      performedDiagnostics = true;
      return;
    }
    TargetDeps *resultDeps = depBuilder.getInferredTargetDeps(resultIndex);
    if (!resultDeps)
      return; // .sil implicit initializers may have been annotated.

    if (!resultDeps->empty())
      return; // same-type inferrence applied; don't issue diagnostics.

    unsigned paramIndex = 0;
    for (auto const &paramInfo : parameterInfos) {
      SWIFT_DEFER { paramIndex++; };
      Type paramTypeInContext = paramInfo.typeInContext;
      if (paramTypeInContext->hasError()) {
        return;
      }
      if (!paramTypeInContext->isEscapable()) {
        // An implicitly initialized non-Escapable value always copies its
        // dependency.
        resultDeps->addIfNew(paramIndex, LifetimeDependenceKind::Inherit);
        continue;
      }
      if (parameterInfos.size() > 1 && !useLazyInference()) {
        diagnose(paramInfo.loc,
                 diag::lifetime_dependence_cannot_infer_implicit_init);
        return;
      }
      // A single Escapable parameter must be borrowed.
      auto kind = inferLifetimeDependenceKind(paramInfo);
      if (!kind) {
        diagnose(
          returnLoc, diag::lifetime_dependence_cannot_infer_scope_ownership,
          paramInfo.name(), diagnosticQualifier());
      }
      resultDeps->addIfNew(paramIndex, LifetimeDependenceKind::Scope);
    }
  }

  // Infer method dependence of result on self for methods, getters, and _modify
  // accessors. Implements the single-parameter rule for methods and accessors
  // accessors (ignoring the subscript index parameter).
  void inferNonEscapableResultOnSelf() {
    TargetDeps *resultDeps = depBuilder.getInferredTargetDeps(resultIndex);
    if (!resultDeps)
      return;

    if (!resultDeps->empty())
      return; // same-type inferrence applied; don't issue diagnostics.

    bool nonEscapableSelf =
        isDiagnosedNonEscapable(implicitSelfParamInfo->typeInContext);
    // Do not infer the result's dependence when the method is mutating and
    // 'self' is non-Escapable. Independently, a missing dependence on inout
    // 'self' will be diagnosed. Since an explicit annotation will be needed for
    // 'self', we also require the method's result to have an explicit
    // annotation.
    if (nonEscapableSelf && implicitSelfParamInfo->param.isInOut()) {
      return;
    }
    // Methods with parameters only apply to lazy inference. This does not
    // include accessors because a subscript's index is assumed not to be the
    // source of the result's dependency.
    if (!(nullptr != afd && isa<AccessorDecl>(afd)) && !useLazyInference() &&
        parameterInfos.size() > 0) {
      return;
    }
    if (!useLazyInference() && !isImplicitOrSIL()) {
      // Require explicit @_lifetime(borrow self) for UnsafePointer-like self.
      if (!nonEscapableSelf &&
          isBitwiseCopyable(implicitSelfParamInfo->typeInContext, ctx)) {
        diagnose(returnLoc,
                 diag::lifetime_dependence_cannot_infer_bitwisecopyable,
                 diagnosticQualifier(), "self");
        return;
      }
      // Require explicit @_lifetime(copy or borrow) for non-Escapable self.
      if (nonEscapableSelf) {
        diagnose(returnLoc, diag::lifetime_dependence_cannot_infer_kind,
                 diagnosticQualifier(), "self");
        return;
      }
    }
    // Infer based on ownership if possible for either explicit accessors or
    // methods as long as they pass preceding ambiguity checks.
    auto kind = inferLifetimeDependenceKind(*implicitSelfParamInfo);
    if (!kind) {
      // Special diagnostic for an attempt to depend on a consuming parameter.
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_scope_ownership,
               "self", diagnosticQualifier());
      return;
    }
    resultDeps->addIfNew(implicitSelfParamInfo->index, *kind);
  }

  // Infer result dependence on a function or intitializer parameter.
  // Implements the single-parameter rule for functions.
  //
  // Note: for implicit initializers with parameters, consider inferring
  // Inherit dependency for each non-Escapable parameter. This would be
  // consistent with implicit stored property setters. This isn't done yet
  // because we also need to consider any Escapable parameters: either skip
  // inference if any exist, infer scoped dependency, or infer no
  // dependency. Implicit setters for Escapable properties are not inferred.
  void inferNonEscapableResultOnParam() {
    // This is only called when there is no 'self' argument that can be the
    // source of a dependence.
    assert(!hasImplicitSelfParam());

    if (useLazyInference()) {
      return lazillyInferNonEscapableResultOnParam();
    }
    TargetDeps *resultDeps = depBuilder.getInferredTargetDeps(resultIndex);
    if (!resultDeps)
      return;

    if (!resultDeps->empty())
      return; // same-type inferrence applied; don't issue diagnostics.

    // Strict inference only handles a single escapable parameter,
    // which is an unambiguous borrow dependence.
    if (parameterInfos.size() == 0) {
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_return_no_param,
               diagnosticQualifier());
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_return_immortal);
      return;
    }
    if (parameterInfos.size() > 1) {
      // The usual diagnostic check is sufficient.
      return;
    }
    // Do not infer non-escapable dependence kind -- it is ambiguous.
    auto const &paramInfo = parameterInfos[0];
    Type paramTypeInContext = paramInfo.typeInContext;
    if (paramTypeInContext->hasError()) {
      return;
    }
    if (!paramTypeInContext->isEscapable()) {
      diagnose(returnLoc, diag::lifetime_dependence_cannot_infer_kind,
               diagnosticQualifier(), paramInfo.name());
      return;
    }
    auto kind = LifetimeDependenceKind::Scope;
    if (!isCompatibleWithOwnership(kind, paramInfo)) {
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_scope_ownership,
               paramInfo.name(), diagnosticQualifier());
      return;
    }
    resultDeps->addIfNew(/*paramIndex*/ 0, kind);
  }

  // Lazy inference for .swiftinterface backward compatibility and
  // experimentation. Inference cases can be added but not removed.
  void lazillyInferNonEscapableResultOnParam() {
    TargetDeps *resultDeps = depBuilder.getInferredTargetDeps(resultIndex);
    if (!resultDeps)
      return;

    std::optional<unsigned> candidateParamIndex;
    std::optional<LifetimeDependenceKind> candidateLifetimeKind;
    unsigned paramIndex = 0;
    for (auto const &paramInfo : parameterInfos) {
      SWIFT_DEFER { paramIndex++; };
      Type paramTypeInContext = paramInfo.typeInContext;
      if (paramTypeInContext->hasError()) {
        return;
      }
      auto paramOwnership = paramInfo.param.getValueOwnership();
      if (paramTypeInContext->isEscapable()) {
        if (isBitwiseCopyable(paramTypeInContext, ctx)) {
          continue;
        }
        if (paramOwnership == ValueOwnership::Default) {
          continue;
        }
      }

      candidateLifetimeKind = inferLifetimeDependenceKind(paramInfo);
      if (!candidateLifetimeKind) {
        continue;
      }
      if (candidateParamIndex) {
        diagnose(returnLoc,
                 diag::lifetime_dependence_cannot_infer_ambiguous_candidate,
                 diagnosticQualifier());
        return;
      }
      candidateParamIndex = paramIndex;
    }
    if (!candidateParamIndex) {
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_no_candidates,
               diagnosticQualifier());
      return;
    }
    resultDeps->addIfNew(*candidateParamIndex, *candidateLifetimeKind);
  }

  // Infer a mutating 'self' dependency when 'self' is non-Escapable and the
  // result is 'void'.
  void inferMutatingSelf() {
    if (!hasImplicitSelfParam())
      return;

    if (!isDiagnosedNonEscapable(implicitSelfParamInfo->typeInContext))
      return;

    assert(!isInit && "class initializers have Escapable self");
    if (!implicitSelfParamInfo->param.isInOut())
      return;

    // Assume that a mutating method does not depend on its parameters.
    depBuilder.inferInoutDependency(implicitSelfParamInfo->index);
  }

  // Infer @_lifetime(param: copy param) for 'inout' non-Escapable parameters.
  //
  // This supports the common case in which the user of a non-Escapable type,
  // such as MutableSpan, wants to modify the span's contents without modifying
  // the span value itself. It should be possible to use MutableSpan this way
  // without requiring any knowledge of lifetime annotations. The tradeoff is
  // that it makes authoring non-Escapable types less safe. For example, a
  // MutableSpan method could update the underlying unsafe pointer and forget to
  // declare a dependence on the incoming pointer.
  //
  // This also allows programmers to make the easy mistake reassign the inout
  // parameter to another parameter:
  //
  //     func reassign(s: inout MutableSpan<Int>, a: MutableSpan) {
  //       s = a
  //     }
  //
  // But, even if that case were disallowed, they may derive another
  // non-Escapable value from an Escapable parameteter:
  //
  //     func reassign(s: inout MutableSpan<Int>, a: [Int]) {
  //       s = a.mutableSpan
  //     }
  //
  // In either case, a diagnostics on the `reassign` function's implementation
  // will catch the invalid reassignment. The only real danger is when the
  // implementation is uses unsafe constructs.
  //
  // Do not issue any diagnostics. This inference is triggered even when the
  // feature is disabled!
  void inferInoutParams() {
    for (unsigned paramIndex : range(parameterInfos.size())) {
      auto const &paramInfo = parameterInfos[paramIndex];
      if (!isDiagnosedNonEscapable(paramInfo.typeInContext)) {
        continue;
      }
      if (!paramInfo.param.isInOut())
        continue;

      depBuilder.inferInoutDependency(paramIndex);
    }
  }

  void inferUnambiguousInoutParams() {
    if (parameterInfos.size() != 1) {
      return;
    }
    const unsigned paramIndex = 0;
    auto const &paramInfo = parameterInfos[paramIndex];
    if (!paramInfo.param.isInOut()) {
      return;
    }
    if (!isDiagnosedNonEscapable(paramInfo.typeInContext)) {
      return;
    }
    depBuilder.inferInoutDependency(paramIndex);
  }

  void inferBuiltin() {
    // Only applicable to AbstractFunctionDecl.
    assert(nullptr != afd);

    // Normal inout parameter inference works for most generic Builtins.
    inferUnambiguousInoutParams();

    const DeclName &name = afd->getName();
    if (name.isSpecial()) {
      return;
    }
    // TODO: declare lifetime dependencies in Builtins.def. Until then, filter
    // the few that are not covered by general inference rules here. This is
    // safer than using a broader rule for implicit declarations. New Builtins
    // need to be considered as they are defined.
    auto id = name.getBaseIdentifier();
    if (id ==
        ctx.getIdentifier(getBuiltinName(BuiltinValueKind::InjectEnumTag))) {
      // ignore the tag parameter
      const unsigned inoutIdx = 0;
      depBuilder.inferInoutDependency(inoutIdx);
    } else if (id ==
        ctx.getIdentifier(
          getBuiltinName(BuiltinValueKind::ConvertUnownedUnsafeToGuaranteed))) {
      const unsigned baseIdx = 0;
      const unsigned inoutIdx = 1;
      depBuilder.inferInoutDependency(inoutIdx);
      depBuilder.inferDependency(inoutIdx, baseIdx,
                                 LifetimeDependenceKind::Scope);
    }
  }
};
} // anonymous namespace

std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
LifetimeDependenceInfo::get(ValueDecl *decl) {
  if (auto *afd = dyn_cast<AbstractFunctionDecl>(decl)) {
    return LifetimeDependenceChecker(afd).checkFuncDecl();
  }
  auto *eed = cast<EnumElementDecl>(decl);
  return LifetimeDependenceChecker::checkEnumElementDecl(eed);
}

std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
LifetimeDependenceInfo::getFromAST(
    FunctionTypeRepr *funcRepr, AnyFunctionType *funcType,
    ArrayRef<LifetimeTypeAttr *> lifetimeAttributes, DeclContext *dc,
    GenericEnvironment *env) {
  return LifetimeDependenceChecker(funcRepr, funcType, lifetimeAttributes, dc,
                                   env)
      .checkFuncType();
}

bool LifetimeDependenceInfo::convertibleTo(
    const LifetimeDependenceInfo &other) const {
  // Pointer equality is sufficient for checking lifetimes are equal because
  // they are canonicalized.
  //
  // We ignore the "isFromAnnotation" flag because it should not affect lifetime
  // checking.

  // The target must be the same.
  if (this->getTargetIndex() != other.getTargetIndex()) {
    return false;
  }

  // Immortal lifetimes are the least restrictive, so only immortal lifetimes
  // can convert to them.
  if (other.isImmortal()) {
    return this->isImmortal();
  }

  // Accordingly, immortal lifetimes can convert to any non-immortal lifetimes.
  if (this->isImmortal()) {
    return true;
  }

  // a ⊆ b. A nullptr is an empty set.
  const auto isSubset = [](IndexSubset *a, IndexSubset *b) {
    // The empty set is a subset of every set, and every set is a subset of
    // itself.
    if (!a || a == b)
      return true;
    // The set a is non-empty, so it cannot be a subset if b is empty.
    if (!b)
      return false;
    return a->isSubsetOf(b);
  };

  return isSubset(this->getInheritIndices(), other.getInheritIndices()) &&
         isSubset(this->getAddressableIndices(),
                  other.getAddressableIndices()) &&
         isSubset(this->getScopeIndices(), other.getScopeIndices());
}

void LifetimeDependenceInfo::dump() const {
  llvm::errs() << "target: " << getTargetIndex() << '\n';
  if (hasImmortalSpecifier()) {
    llvm::errs() << "  immortal\n";
  }
  if (auto scoped = getScopeIndices()) {
    llvm::errs() << "  scoped: ";
    scoped->dump();
  }
  if (auto inherited = getInheritIndices()) {
    llvm::errs() << "  inherited: ";
    inherited->dump();
  }
  if (auto addressable = getAddressableIndices()) {
    llvm::errs() << "  addressable: ";
    addressable->dump();
  }
}

// =============================================================================
// SIL parsing support
// =============================================================================

// This implements the logic for SIL type descriptors similar to source-level
// logic in LifetimeDependenceChecker::initializeAttributeDeps(). The SIL
// context is substantially different from Sema.
static std::optional<LifetimeDependenceInfo> checkSILTypeModifiers(
  LifetimeDependentTypeRepr *lifetimeDependentRepr, unsigned targetIndex,
  ArrayRef<SILParameterInfo> params, DeclContext *dc) {
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  auto capacity = params.size(); // SIL parameters include self

  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector scopeLifetimeParamIndices(capacity);
  SmallBitVector addressableLifetimeParamIndices(capacity);
  SmallBitVector conditionallyAddressableLifetimeParamIndices(capacity);

  auto updateLifetimeDependenceInfo =
    [&](LifetimeDescriptor descriptor,
        unsigned paramIndexToSet,
        ParameterConvention paramConvention) {
      auto loc = descriptor.getLoc();
      auto kind = descriptor.getParsedLifetimeDependenceKind();

      if (kind == ParsedLifetimeDependenceKind::Borrow &&
          isConsumedParameterInCallee(paramConvention)) {
        diags.diagnose(loc, diag::lifetime_dependence_cannot_use_kind, "_scope",
                       getStringForParameterConvention(paramConvention));
        return true;
      }

      if (inheritLifetimeParamIndices.test(paramIndexToSet) ||
          scopeLifetimeParamIndices.test(paramIndexToSet)) {
        diags.diagnose(loc, diag::lifetime_dependence_duplicate_param_id);
        return true;
      }
      if (kind == ParsedLifetimeDependenceKind::Inherit) {
        inheritLifetimeParamIndices.set(paramIndexToSet);
      } else {
        assert(kind == ParsedLifetimeDependenceKind::Borrow);
        scopeLifetimeParamIndices.set(paramIndexToSet);
      }
      return false;
    };

  bool hasImmortalSpecifier= false;
  for (auto source : lifetimeDependentRepr->getLifetimeEntry()->getSources())
  {
    switch (source.getDescriptorKind()) {
    case LifetimeDescriptor::DescriptorKind::Ordered: {
      auto index = source.getIndex();
      if (index > capacity) {
        diags.diagnose(source.getLoc(),
                       diag::lifetime_dependence_invalid_param_index, index);
        return std::nullopt;
      }
      auto param = params[index];
      auto paramConvention = param.getConvention();
      if (updateLifetimeDependenceInfo(source, index, paramConvention)) {
        return std::nullopt;
      }
      switch (source.isAddressable()) {
      case LifetimeDescriptor::IsNotAddressable:
        break;
      case LifetimeDescriptor::IsConditionallyAddressable:
        conditionallyAddressableLifetimeParamIndices.set(index);
        break;
      case LifetimeDescriptor::IsAddressable:
        addressableLifetimeParamIndices.set(index);
        break;
      }
      break;
    }
    case LifetimeDescriptor::DescriptorKind::Named: {
      assert(source.isImmortalSpecifier());
      hasImmortalSpecifier = true;
      break;
    }
    default:
      llvm_unreachable("SIL can only have ordered or immortal lifetime "
                       "dependence specifier kind");
    }
  }

  return LifetimeDependenceInfo(
    inheritLifetimeParamIndices.any()
    ? IndexSubset::get(ctx, inheritLifetimeParamIndices)
    : nullptr,
    scopeLifetimeParamIndices.any()
    ? IndexSubset::get(ctx, scopeLifetimeParamIndices)
    : nullptr,
    targetIndex,
    /*hasImmortalSpecifier*/ hasImmortalSpecifier,
    /*isFromAnnotation*/ true,
    addressableLifetimeParamIndices.any()
    ? IndexSubset::get(ctx, addressableLifetimeParamIndices)
    : nullptr,
    conditionallyAddressableLifetimeParamIndices.any()
    ? IndexSubset::get(ctx, conditionallyAddressableLifetimeParamIndices)
    : nullptr);
}

std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
LifetimeDependenceInfo::getFromSIL(FunctionTypeRepr *funcRepr,
                                   ArrayRef<SILParameterInfo> params,
                                   ArrayRef<SILResultInfo> results,
                                   DeclContext *dc) {
  SmallVector<LifetimeDependenceInfo, 1> lifetimeDependencies;

  auto getLifetimeDependenceFromTypeModifiers =
      [&](TypeRepr *typeRepr,
          unsigned targetIndex) -> std::optional<LifetimeDependenceInfo> {
    auto *lifetimeTypeRepr =
        dyn_cast_or_null<LifetimeDependentTypeRepr>(typeRepr);
    if (!lifetimeTypeRepr) {
      return std::nullopt;
    }
    return checkSILTypeModifiers(lifetimeTypeRepr, targetIndex, params, dc);
  };

  auto argsTypeRepr = funcRepr->getArgsTypeRepr()->getElements();
  for (unsigned targetIndex : indices(argsTypeRepr)) {
    if (auto result = getLifetimeDependenceFromTypeModifiers(
            argsTypeRepr[targetIndex].Type, targetIndex)) {
      lifetimeDependencies.push_back(*result);
    }
  }

  auto result = getLifetimeDependenceFromTypeModifiers(
    funcRepr->getResultTypeRepr(), params.size());
  if (result) {
    lifetimeDependencies.push_back(*result);
  }

  return dc->getASTContext().AllocateCopy(lifetimeDependencies);
}
