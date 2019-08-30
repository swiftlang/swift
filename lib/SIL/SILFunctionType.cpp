//===--- SILFunctionType.cpp - Giving SIL types to AST functions ----------===//
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
// This file defines the native Swift ownership transfer conventions
// and works in concert with the importer to give the correct
// conventions to imported functions and types.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "libsil"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/ForeignInfo.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Analysis/DomainSpecific/CocoaConventions.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace swift::Lowering;

SILType SILFunctionType::getDirectFormalResultsType() {
  CanType type;
  if (getNumDirectFormalResults() == 0) {
    type = getASTContext().TheEmptyTupleType;
  } else if (getNumDirectFormalResults() == 1) {
    type = getSingleDirectFormalResult().getType();
  } else {
    auto &cache = getMutableFormalResultsCache();
    if (cache) {
      type = cache;
    } else {
      SmallVector<TupleTypeElt, 4> elts;
      for (auto result : getResults())
        if (!result.isFormalIndirect())
          elts.push_back(result.getType());
      type = CanType(TupleType::get(elts, getASTContext()));
      cache = type;
    }
  }
  return SILType::getPrimitiveObjectType(type);
}

SILType SILFunctionType::getAllResultsType() {
  CanType type;
  if (getNumResults() == 0) {
    type = getASTContext().TheEmptyTupleType;
  } else if (getNumResults() == 1) {
    type = getResults()[0].getType();
  } else {
    auto &cache = getMutableAllResultsCache();
    if (cache) {
      type = cache;
    } else {
      SmallVector<TupleTypeElt, 4> elts;
      for (auto result : getResults())
        elts.push_back(result.getType());
      type = CanType(TupleType::get(elts, getASTContext()));
      cache = type;
    }
  }
  return SILType::getPrimitiveObjectType(type);
}

SILType SILFunctionType::getFormalCSemanticResult() {
  assert(getLanguage() == SILFunctionLanguage::C);
  assert(getNumResults() <= 1);
  return getDirectFormalResultsType();
}

CanType SILFunctionType::getSelfInstanceType() const {
  auto selfTy = getSelfParameter().getType();

  // If this is a static method, get the instance type.
  if (auto metaTy = dyn_cast<AnyMetatypeType>(selfTy))
    return metaTy.getInstanceType();

  return selfTy;
}

ClassDecl *
SILFunctionType::getWitnessMethodClass() const {
  auto selfTy = getSelfInstanceType();
  auto genericSig = getGenericSignature();
  if (auto paramTy = dyn_cast<GenericTypeParamType>(selfTy)) {
    assert(paramTy->getDepth() == 0 && paramTy->getIndex() == 0);
    auto superclass = genericSig->getSuperclassBound(paramTy);
    if (superclass)
      return superclass->getClassOrBoundGenericClass();
  }

  return nullptr;
}

static CanType getKnownType(Optional<CanType> &cacheSlot, ASTContext &C,
                            StringRef moduleName, StringRef typeName) {
  if (!cacheSlot) {
    cacheSlot = ([&] {
      ModuleDecl *mod = C.getLoadedModule(C.getIdentifier(moduleName));
      if (!mod)
        return CanType();

      // Do a general qualified lookup instead of a direct lookupValue because
      // some of the types we want are reexported through overlays and
      // lookupValue would only give us types actually declared in the overlays
      // themselves.
      SmallVector<ValueDecl *, 2> decls;
      mod->lookupQualified(mod, C.getIdentifier(typeName),
                           NL_QualifiedDefault | NL_KnownNonCascadingDependency,
                           decls);
      if (decls.size() != 1)
        return CanType();

      const auto *typeDecl = dyn_cast<TypeDecl>(decls.front());
      if (!typeDecl)
        return CanType();

      assert(typeDecl->hasInterfaceType() &&
             "bridged type must be type-checked");
      return typeDecl->getDeclaredInterfaceType()->getCanonicalType();
    })();
  }
  CanType t = *cacheSlot;

  // It is possible that we won't find a bridging type (e.g. String) when we're
  // parsing the stdlib itself.
  if (t) {
    LLVM_DEBUG(llvm::dbgs() << "Bridging type " << moduleName << '.' << typeName
                            << " mapped to ";
               if (t)
                 t->print(llvm::dbgs());
               else
                 llvm::dbgs() << "<null>";
               llvm::dbgs() << '\n');
  }
  return t;
}

#define BRIDGING_KNOWN_TYPE(BridgedModule,BridgedType) \
  CanType TypeConverter::get##BridgedType##Type() {         \
    return getKnownType(BridgedType##Ty, M.getASTContext(), \
                        #BridgedModule, #BridgedType);      \
  }
#include "swift/SIL/BridgedTypes.def"

/// Adjust a function type to have a slightly different type.
CanAnyFunctionType
Lowering::adjustFunctionType(CanAnyFunctionType t,
                             AnyFunctionType::ExtInfo extInfo) {
  if (t->getExtInfo() == extInfo)
    return t;
  return CanAnyFunctionType(t->withExtInfo(extInfo));
}

/// Adjust a function type to have a slightly different type.
CanSILFunctionType Lowering::adjustFunctionType(
    CanSILFunctionType type, SILFunctionType::ExtInfo extInfo,
    ParameterConvention callee,
    Optional<ProtocolConformanceRef> witnessMethodConformance) {
  if (type->getExtInfo() == extInfo && type->getCalleeConvention() == callee &&
      type->getWitnessMethodConformanceOrNone() == witnessMethodConformance)
    return type;

  return SILFunctionType::get(type->getGenericSignature(),
                              extInfo, type->getCoroutineKind(), callee,
                              type->getParameters(), type->getYields(),
                              type->getResults(),
                              type->getOptionalErrorResult(),
                              type->getASTContext(),
                              witnessMethodConformance);
}

CanSILFunctionType
SILFunctionType::getWithRepresentation(Representation repr) {
  return getWithExtInfo(getExtInfo().withRepresentation(repr));
}

CanSILFunctionType SILFunctionType::getWithExtInfo(ExtInfo newExt) {
  auto oldExt = getExtInfo();
  if (newExt == oldExt)
    return CanSILFunctionType(this);

  auto calleeConvention =
    (newExt.hasContext()
       ? (oldExt.hasContext()
            ? getCalleeConvention()
            : Lowering::DefaultThickCalleeConvention)
       : ParameterConvention::Direct_Unowned);

  return get(getGenericSignature(), newExt, getCoroutineKind(),
             calleeConvention, getParameters(), getYields(),
             getResults(), getOptionalErrorResult(), getASTContext(),
             getWitnessMethodConformanceOrNone());
}

namespace {

enum class ConventionsKind : uint8_t {
  Default = 0,
  DefaultBlock = 1,
  ObjCMethod = 2,
  CFunctionType = 3,
  CFunction = 4,
  ObjCSelectorFamily = 5,
  Deallocator = 6,
  Capture = 7,
  CXXMethod = 8,
};

class Conventions {
  ConventionsKind kind;

protected:
  virtual ~Conventions() = default;

public:
  Conventions(ConventionsKind k) : kind(k) {}

  ConventionsKind getKind() const { return kind; }

  virtual ParameterConvention
  getIndirectParameter(unsigned index,
                       const AbstractionPattern &type,
                       const TypeLowering &substTL) const = 0;
  virtual ParameterConvention
  getDirectParameter(unsigned index,
                     const AbstractionPattern &type,
                     const TypeLowering &substTL) const = 0;
  virtual ParameterConvention getCallee() const = 0;
  virtual ResultConvention getResult(const TypeLowering &resultTL) const = 0;
  virtual ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const = 0;
  virtual ParameterConvention
  getDirectSelfParameter(const AbstractionPattern &type) const = 0;

  // Helpers that branch based on a value ownership.
  ParameterConvention getIndirect(ValueOwnership ownership, bool forSelf,
                                  unsigned index,
                                  const AbstractionPattern &type,
                                  const TypeLowering &substTL) const {
    switch (ownership) {
    case ValueOwnership::Default:
      if (forSelf)
        return getIndirectSelfParameter(type);
      return getIndirectParameter(index, type, substTL);
    case ValueOwnership::InOut:
      return ParameterConvention::Indirect_Inout;
    case ValueOwnership::Shared:
      return ParameterConvention::Indirect_In_Guaranteed;
    case ValueOwnership::Owned:
      return ParameterConvention::Indirect_In;
    }
    llvm_unreachable("unhandled ownership");
  }

  ParameterConvention getDirect(ValueOwnership ownership, bool forSelf,
                                unsigned index, const AbstractionPattern &type,
                                const TypeLowering &substTL) const {
    switch (ownership) {
    case ValueOwnership::Default:
      if (forSelf)
        return getDirectSelfParameter(type);
      return getDirectParameter(index, type, substTL);
    case ValueOwnership::InOut:
      return ParameterConvention::Indirect_Inout;
    case ValueOwnership::Shared:
      return ParameterConvention::Direct_Guaranteed;
    case ValueOwnership::Owned:
      return ParameterConvention::Direct_Owned;
    }
    llvm_unreachable("unhandled ownership");
  }
};

/// A visitor for breaking down formal result types into a SILResultInfo
/// and possibly some number of indirect-out SILParameterInfos,
/// matching the abstraction patterns of the original type.
class DestructureResults {
  SILModule &M;
  const Conventions &Convs;
  SmallVectorImpl<SILResultInfo> &Results;
public:
  DestructureResults(SILModule &M, const Conventions &conventions,
                     SmallVectorImpl<SILResultInfo> &results)
    : M(M), Convs(conventions), Results(results) {}

  void destructure(AbstractionPattern origType, CanType substType) {
    // Recurse into tuples.
    if (origType.isTuple()) {
      auto substTupleType = cast<TupleType>(substType);
      for (auto eltIndex : indices(substTupleType.getElementTypes())) {
        AbstractionPattern origEltType =
          origType.getTupleElementType(eltIndex);
        CanType substEltType = substTupleType.getElementType(eltIndex);
        destructure(origEltType, substEltType);
      }
      return;
    }

    auto &substResultTL = M.Types.getTypeLowering(origType, substType,
                                                  ResilienceExpansion::Minimal);

    // Determine the result convention.
    ResultConvention convention;
    if (isFormallyReturnedIndirectly(origType, substType, substResultTL)) {
      convention = ResultConvention::Indirect;
    } else {
      convention = Convs.getResult(substResultTL);

      // Reduce conventions for trivial types to an unowned convention.
      if (substResultTL.isTrivial()) {
        switch (convention) {
        case ResultConvention::Indirect:
        case ResultConvention::Unowned:
        case ResultConvention::UnownedInnerPointer:
          // Leave these as-is.
          break;

        case ResultConvention::Autoreleased:
        case ResultConvention::Owned:
          // These aren't distinguishable from unowned for trivial types.
          convention = ResultConvention::Unowned;
          break;
        }
      }
    }

    SILResultInfo result(substResultTL.getLoweredType().getASTType(),
                         convention);
    Results.push_back(result);
  }

  /// Query whether the original type is returned indirectly for the purpose
  /// of reabstraction given complete lowering information about its
  /// substitution.
  bool isFormallyReturnedIndirectly(AbstractionPattern origType,
                                    CanType substType,
                                    const TypeLowering &substTL) {
    // If the substituted type is returned indirectly, so must the
    // unsubstituted type.
    if ((origType.isTypeParameter()
         && !origType.isConcreteType()
         && !origType.requiresClass())
        || substTL.isAddressOnly()) {
      return true;

    // If the substitution didn't change the type, then a negative
    // response to the above is determinative as well.
    } else if (origType.getType() == substType &&
               !origType.getType()->hasTypeParameter()) {
      return false;

    // Otherwise, query specifically for the original type.
    } else {
      return SILType::isFormallyReturnedIndirectly(
          origType.getType(), M, origType.getGenericSignature());
    }
  }
};

static bool isClangTypeMoreIndirectThanSubstType(SILModule &M,
                                                 const clang::Type *clangTy,
                                                 CanType substTy) {
  // A const pointer argument might have been imported as
  // UnsafePointer, COpaquePointer, or a CF foreign class.
  // (An ObjC class type wouldn't be const-qualified.)
  if (clangTy->isPointerType()
      && clangTy->getPointeeType().isConstQualified()) {
    // Peek through optionals.
    if (auto substObjTy = substTy.getOptionalObjectType())
      substTy = substObjTy;

    // Void pointers aren't usefully indirectable.
    if (clangTy->isVoidPointerType())
      return false;

    if (auto eltTy = substTy->getAnyPointerElementType())
      return isClangTypeMoreIndirectThanSubstType(M,
                    clangTy->getPointeeType().getTypePtr(), CanType(eltTy));

    if (substTy->getAnyNominal() ==
          M.getASTContext().getOpaquePointerDecl())
      // TODO: We could conceivably have an indirect opaque ** imported
      // as COpaquePointer. That shouldn't ever happen today, though,
      // since we only ever indirect the 'self' parameter of functions
      // imported as methods.
      return false;

    if (clangTy->getPointeeType()->getAs<clang::RecordType>()) {
      // CF type as foreign class
      if (substTy->getClassOrBoundGenericClass() &&
          substTy->getClassOrBoundGenericClass()->getForeignClassKind() ==
            ClassDecl::ForeignKind::CFType) {
        return false;
      }
    }

    // swift_newtypes are always passed directly
    if (auto typedefTy = clangTy->getAs<clang::TypedefType>()) {
      if (typedefTy->getDecl()->getAttr<clang::SwiftNewtypeAttr>())
        return false;
    }

    return true;
  }
  return false;
}

static bool isFormallyPassedIndirectly(SILModule &M,
                                       AbstractionPattern origType,
                                       CanType substType,
                                       const TypeLowering &substTL) {
  // If the C type of the argument is a const pointer, but the Swift type
  // isn't, treat it as indirect.
  if (origType.isClangType()
      && isClangTypeMoreIndirectThanSubstType(M, origType.getClangType(),
                                              substType)) {
    return true;
  }

  // If the substituted type is passed indirectly, so must the
  // unsubstituted type.
  if ((origType.isTypeParameter() && !origType.isConcreteType()
       && !origType.requiresClass())
      || substTL.isAddressOnly()) {
    return true;

  // If the substitution didn't change the type, then a negative
  // response to the above is determinative as well.
  } else if (origType.getType() == substType &&
             !origType.getType()->hasTypeParameter()) {
    return false;

  // Otherwise, query specifically for the original type.
  } else {
    return SILType::isFormallyPassedIndirectly(
        origType.getType(), M, origType.getGenericSignature());
  }
}

/// A visitor for turning formal input types into SILParameterInfos, matching
/// the abstraction patterns of the original type.
///
/// If the original abstraction pattern is fully opaque, we must pass the
/// function's parameters and results indirectly, as if the original type were
/// the most general function signature (expressed entirely in generic
/// parameters) which can be substituted to equal the given signature.
///
/// See the comment in AbstractionPattern.h for details.
class DestructureInputs {
  SILModule &M;
  const Conventions &Convs;
  const ForeignInfo &Foreign;
  Optional<llvm::function_ref<void()>> HandleForeignSelf;
  SmallVectorImpl<SILParameterInfo> &Inputs;
  unsigned NextOrigParamIndex = 0;
public:
  DestructureInputs(SILModule &M, const Conventions &conventions,
                    const ForeignInfo &foreign,
                    SmallVectorImpl<SILParameterInfo> &inputs)
    : M(M), Convs(conventions), Foreign(foreign), Inputs(inputs) {}

  void destructure(AbstractionPattern origType,
                   CanAnyFunctionType::CanParamArrayRef params,
                   AnyFunctionType::ExtInfo extInfo) {
    visitTopLevelParams(origType, params, extInfo);
  }

private:
  /// Query whether the original type is address-only given complete
  /// lowering information about its substitution.
  bool isFormallyPassedIndirectly(AbstractionPattern origType,
                                  CanType substType,
                                  const TypeLowering &substTL) {
    return ::isFormallyPassedIndirectly(M, origType, substType, substTL);
  }

  /// This is a special entry point that allows destructure inputs to handle
  /// self correctly.
  void visitTopLevelParams(AbstractionPattern origType,
                           CanAnyFunctionType::CanParamArrayRef params,
                           AnyFunctionType::ExtInfo extInfo) {
    unsigned numEltTypes = params.size();

    bool hasSelf = (extInfo.hasSelfParam() || Foreign.Self.isImportAsMember());
    unsigned numNonSelfParams = (hasSelf ? numEltTypes - 1 : numEltTypes);

    auto silRepresentation = extInfo.getSILRepresentation();

    // We have to declare this out here so that the lambda scope lasts for
    // the duration of the loop below.
    auto handleForeignSelf = [&] {
      // This is a "self", but it's not a Swift self, we handle it differently.
      auto selfParam = params[numNonSelfParams];
      visit(selfParam.getValueOwnership(),
            /*forSelf=*/false,
            origType.getFunctionParamType(numNonSelfParams),
            selfParam.getParameterType(), silRepresentation);
    };

    // If we have a foreign-self, install handleSelf as the handler.
    if (Foreign.Self.isInstance()) {
      assert(hasSelf && numEltTypes > 0);
      // This is safe because function_ref just stores a pointer to the
      // existing lambda object.
      HandleForeignSelf = handleForeignSelf;
    }

    // Add any leading foreign parameters.
    maybeAddForeignParameters();

    // Process all the non-self parameters.
    for (unsigned i = 0; i != numNonSelfParams; ++i) {
      auto ty = params[i].getParameterType();
      auto eltPattern = origType.getFunctionParamType(i);
      auto flags = params[i].getParameterFlags();

      visit(flags.getValueOwnership(), /*forSelf=*/false,
            eltPattern, ty, silRepresentation);
    }

    // Process the self parameter.  Note that we implicitly drop self
    // if this is a static foreign-self import.
    if (hasSelf && !Foreign.Self.isImportAsMember()) {
      auto selfParam = params[numNonSelfParams];
      auto ty = selfParam.getParameterType();
      auto eltPattern = origType.getFunctionParamType(numNonSelfParams);
      auto flags = selfParam.getParameterFlags();

      visit(flags.getValueOwnership(), /*forSelf=*/true,
            eltPattern, ty, silRepresentation);
    }

    // Clear the foreign-self handler for safety.
    HandleForeignSelf.reset();
  }

  void visit(ValueOwnership ownership, bool forSelf,
             AbstractionPattern origType, CanType substType,
             SILFunctionTypeRepresentation rep) {
    assert(!isa<InOutType>(substType));

    // Tuples get handled specially, in some cases:
    CanTupleType substTupleTy = dyn_cast<TupleType>(substType);
    if (substTupleTy && !origType.isTypeParameter()) {
      assert(origType.getNumTupleElements() == substTupleTy->getNumElements());
      switch (ownership) {
      case ValueOwnership::Default:
      case ValueOwnership::Owned:
      case ValueOwnership::Shared:
        // Expand the tuple.
        for (auto i : indices(substTupleTy.getElementTypes())) {
          auto &elt = substTupleTy->getElement(i);
          auto ownership = elt.getParameterFlags().getValueOwnership();
          // FIXME(swift3): Once the entire parameter list is no longer a
          // target for substitution, re-enable this.
          // assert(ownership == ValueOwnership::Default);
          // assert(!elt.isVararg());
          visit(ownership, forSelf,
                origType.getTupleElementType(i),
                CanType(elt.getRawType()), rep);
        }
        return;
      case ValueOwnership::InOut:
        // handled below
        break;
      }
    }

    unsigned origParamIndex = NextOrigParamIndex++;

    auto &substTL = M.Types.getTypeLowering(origType, substType,
                                            ResilienceExpansion::Minimal);
    ParameterConvention convention;
    if (ownership == ValueOwnership::InOut) {
      convention = ParameterConvention::Indirect_Inout;
    } else if (isFormallyPassedIndirectly(origType, substType, substTL)) {
      convention = Convs.getIndirect(ownership, forSelf, origParamIndex,
                                     origType, substTL);
      assert(isIndirectFormalParameter(convention));
    } else if (substTL.isTrivial()) {
      convention = ParameterConvention::Direct_Unowned;
    } else {
      convention = Convs.getDirect(ownership, forSelf, origParamIndex, origType,
                                   substTL);
      assert(!isIndirectFormalParameter(convention));
    }
    auto loweredType = substTL.getLoweredType().getASTType();

    Inputs.push_back(SILParameterInfo(loweredType, convention));

    maybeAddForeignParameters();
  }

  /// Given that we've just reached an argument index for the
  /// first time, add any foreign parameters.
  void maybeAddForeignParameters() {
    while (maybeAddForeignErrorParameter() ||
           maybeAddForeignSelfParameter()) {
      // Continue to see, just in case there are more parameters to add.
    }
  }

  bool maybeAddForeignErrorParameter() {
    if (!Foreign.Error ||
        NextOrigParamIndex != Foreign.Error->getErrorParameterIndex())
      return false;

    auto foreignErrorTy =
      M.Types.getLoweredRValueType(Foreign.Error->getErrorParameterType());

    // Assume the error parameter doesn't have interesting lowering.
    Inputs.push_back(SILParameterInfo(foreignErrorTy,
                                      ParameterConvention::Direct_Unowned));
    NextOrigParamIndex++;
    return true;
  }

  bool maybeAddForeignSelfParameter() {
    if (!Foreign.Self.isInstance() ||
        NextOrigParamIndex != Foreign.Self.getSelfIndex())
      return false;

    (*HandleForeignSelf)();
    return true;
  }
};

} // end anonymous namespace

static bool isPseudogeneric(SILDeclRef c) {
  // FIXME: should this be integrated in with the Sema check that prevents
  // illegal use of type arguments in pseudo-generic method bodies?

  // The implicitly-generated native initializer thunks for imported
  // initializers are never pseudo-generic, because they may need
  // to use their type arguments to bridge their value arguments.
  if (!c.isForeign &&
      (c.kind == SILDeclRef::Kind::Allocator ||
       c.kind == SILDeclRef::Kind::Initializer) &&
      c.getDecl()->hasClangNode())
    return false;

  // Otherwise, we have to look at the entity's context.
  DeclContext *dc;
  if (c.hasDecl()) {
    dc = c.getDecl()->getDeclContext();
  } else if (auto closure = c.getAbstractClosureExpr()) {
    dc = closure->getParent();
  } else {
    return false;
  }
  dc = dc->getInnermostTypeContext();
  if (!dc) return false;

  auto classDecl = dc->getSelfClassDecl();
  return (classDecl && classDecl->usesObjCGenericsModel());
}

/// Update the result type given the foreign error convention that we will be
/// using.
static std::pair<AbstractionPattern, CanType> updateResultTypeForForeignError(
    ForeignErrorConvention convention, CanGenericSignature genericSig,
    AbstractionPattern origResultType, CanType substFormalResultType) {
  switch (convention.getKind()) {
  // These conventions replace the result type.
  case ForeignErrorConvention::ZeroResult:
  case ForeignErrorConvention::NonZeroResult:
    assert(substFormalResultType->isVoid());
    substFormalResultType = convention.getResultType();
    origResultType = AbstractionPattern(genericSig, substFormalResultType);
    return {origResultType, substFormalResultType};

  // These conventions wrap the result type in a level of optionality.
  case ForeignErrorConvention::NilResult:
    assert(!substFormalResultType->getOptionalObjectType());
    substFormalResultType =
        OptionalType::get(substFormalResultType)->getCanonicalType();
    origResultType =
        AbstractionPattern::getOptional(origResultType);
    return {origResultType, substFormalResultType};

  // These conventions don't require changes to the formal error type.
  case ForeignErrorConvention::ZeroPreservedResult:
  case ForeignErrorConvention::NonNilError:
    return {origResultType, substFormalResultType};
  }
  llvm_unreachable("unhandled kind");
}

/// Lower any/all capture context parameters.
///
/// *NOTE* Currently default arg generators can not capture anything.
/// If we ever add that ability, it will be a different capture list
/// from the function to which the argument is attached.
static void
lowerCaptureContextParameters(SILModule &M, AnyFunctionRef function,
                              CanGenericSignature genericSig,
                              ResilienceExpansion expansion,
                              SmallVectorImpl<SILParameterInfo> &inputs) {

  // NB: The generic signature may be elided from the lowered function type
  // if the function is in a fully-specialized context, but we still need to
  // canonicalize references to the generic parameters that may appear in
  // non-canonical types in that context. We need the original generic
  // signature from the AST for that.
  auto origGenericSig = function.getGenericSignature();

  auto &Types = M.Types;
  auto loweredCaptures = Types.getLoweredLocalCaptures(function);

  for (auto capture : loweredCaptures.getCaptures()) {
    if (capture.isDynamicSelfMetadata()) {
      ParameterConvention convention = ParameterConvention::Direct_Unowned;
      auto dynamicSelfInterfaceType =
          loweredCaptures.getDynamicSelfType()->mapTypeOutOfContext();

      auto selfMetatype = MetatypeType::get(dynamicSelfInterfaceType,
                                            MetatypeRepresentation::Thick);

      auto canSelfMetatype = selfMetatype->getCanonicalType(origGenericSig);
      SILParameterInfo param(canSelfMetatype, convention);
      inputs.push_back(param);

      continue;
    }

    if (capture.isOpaqueValue()) {
      OpaqueValueExpr *opaqueValue = capture.getOpaqueValue();
      auto canType = opaqueValue->getType()->mapTypeOutOfContext()
          ->getCanonicalType(origGenericSig);
      auto &loweredTL =
          Types.getTypeLowering(AbstractionPattern(genericSig, canType),
                                canType, expansion);
      auto loweredTy = loweredTL.getLoweredType();

      ParameterConvention convention;
      if (loweredTL.isAddressOnly()) {
        convention = ParameterConvention::Indirect_In;
      } else {
        convention = ParameterConvention::Direct_Owned;
      }
      SILParameterInfo param(loweredTy.getASTType(), convention);
      inputs.push_back(param);

      continue;
    }

    auto *VD = capture.getDecl();
    auto type = VD->getInterfaceType();
    auto canType = type->getCanonicalType(origGenericSig);

    auto &loweredTL =
        Types.getTypeLowering(AbstractionPattern(genericSig, canType), canType,
                              expansion);
    auto loweredTy = loweredTL.getLoweredType();
    switch (Types.getDeclCaptureKind(capture, expansion)) {
    case CaptureKind::Constant: {
      // Constants are captured by value.
      ParameterConvention convention;
      if (loweredTL.isAddressOnly()) {
        convention = ParameterConvention::Indirect_In_Guaranteed;
      } else if (loweredTL.isTrivial()) {
        convention = ParameterConvention::Direct_Unowned;
      } else {
        convention = ParameterConvention::Direct_Guaranteed;
      }
      SILParameterInfo param(loweredTy.getASTType(), convention);
      inputs.push_back(param);
      break;
    }
    case CaptureKind::Box: {
      // Lvalues are captured as a box that owns the captured value.
      auto boxTy = Types.getInterfaceBoxTypeForCapture(
          VD, loweredTy.getASTType(),
          /*mutable*/ true);
      auto convention = ParameterConvention::Direct_Guaranteed;
      auto param = SILParameterInfo(boxTy, convention);
      inputs.push_back(param);
      break;
    }
    case CaptureKind::StorageAddress: {
      // Non-escaping lvalues are captured as the address of the value.
      SILType ty = loweredTy.getAddressType();
      auto param =
          SILParameterInfo(ty.getASTType(),
                           ParameterConvention::Indirect_InoutAliasable);
      inputs.push_back(param);
      break;
    }
    }
  }
}

static void destructureYieldsForReadAccessor(SILModule &M,
                                             AbstractionPattern origType,
                                             CanType valueType,
                                        SmallVectorImpl<SILYieldInfo> &yields) {
  // Recursively destructure tuples.
  if (origType.isTuple()) {
    auto valueTupleType = cast<TupleType>(valueType);
    for (auto i : indices(valueTupleType.getElementTypes())) {
      auto origEltType = origType.getTupleElementType(i);
      auto valueEltType = valueTupleType.getElementType(i);
      destructureYieldsForReadAccessor(M, origEltType, valueEltType, yields);
    }
    return;
  }

  auto &tl = M.Types.getTypeLowering(origType, valueType,
                                     ResilienceExpansion::Minimal);
  auto convention = [&] {
    if (isFormallyPassedIndirectly(M, origType, valueType, tl))
      return ParameterConvention::Indirect_In_Guaranteed;
    if (tl.isTrivial())
      return ParameterConvention::Direct_Unowned;
    return ParameterConvention::Direct_Guaranteed;
  }();
  yields.push_back(SILYieldInfo(tl.getLoweredType().getASTType(),
                                convention));
}

static void destructureYieldsForCoroutine(SILModule &M,
                                          Optional<SILDeclRef> origConstant,
                                          Optional<SILDeclRef> constant,
                                          Optional<SubstitutionMap> reqtSubs,
                                          SmallVectorImpl<SILYieldInfo> &yields,
                                          SILCoroutineKind &coroutineKind) {
  assert(coroutineKind == SILCoroutineKind::None);
  assert(yields.empty());

  if (!constant || !constant->hasDecl())
    return;

  auto accessor = dyn_cast<AccessorDecl>(constant->getDecl());
  if (!accessor || !accessor->isCoroutine())
    return;

  auto origAccessor = cast<AccessorDecl>(origConstant->getDecl());

  // Coroutine accessors are implicitly yield-once coroutines, despite
  // their function type.
  coroutineKind = SILCoroutineKind::YieldOnce;

  // Coroutine accessors are always native, so fetch the native
  // abstraction pattern.
  auto origStorage = origAccessor->getStorage();
  auto origType = M.Types.getAbstractionPattern(origStorage, /*nonobjc*/ true)
                         .getReferenceStorageReferentType();

  auto storage = accessor->getStorage();
  auto valueType = storage->getValueInterfaceType();
  if (reqtSubs) {
    valueType = valueType.subst(*reqtSubs);
  }

  auto canValueType = valueType->getCanonicalType(
    accessor->getGenericSignature());

  // 'modify' yields an inout of the target type.
  if (accessor->getAccessorKind() == AccessorKind::Modify) {
    auto loweredValueTy = M.Types.getLoweredRValueType(origType, canValueType);
    yields.push_back(SILYieldInfo(loweredValueTy,
                                  ParameterConvention::Indirect_Inout));
    return;
  }

  // 'read' yields a borrowed value of the target type, destructuring
  // tuples as necessary.
  assert(accessor->getAccessorKind() == AccessorKind::Read);
  destructureYieldsForReadAccessor(M, origType, canValueType, yields);
}

/// Create the appropriate SIL function type for the given formal type
/// and conventions.
///
/// The lowering of function types is generally sensitive to the
/// declared abstraction pattern.  We want to be able to take
/// advantage of declared type information in order to, say, pass
/// arguments separately and directly; but we also want to be able to
/// call functions from generic code without completely embarrassing
/// performance.  Therefore, different abstraction patterns induce
/// different argument-passing conventions, and we must introduce
/// implicit reabstracting conversions where necessary to map one
/// convention to another.
///
/// However, we actually can't reabstract arbitrary thin function
/// values while still leaving them thin, at least without costly
/// page-mapping tricks. Therefore, the representation must remain
/// consistent across all abstraction patterns.
///
/// We could reabstract block functions in theory, but (1) we don't
/// really need to and (2) doing so would be problematic because
/// stuffing something in an Optional currently forces it to be
/// reabstracted to the most general type, which means that we'd
/// expect the wrong abstraction conventions on bridged block function
/// types.
///
/// Therefore, we only honor abstraction patterns on thick or
/// polymorphic functions.
///
/// FIXME: we shouldn't just drop the original abstraction pattern
/// when we can't reabstract.  Instead, we should introduce
/// dynamic-indirect argument-passing conventions and map opaque
/// archetypes to that, then respect those conventions in IRGen by
/// using runtime call construction.
///
/// \param conventions - conventions as expressed for the original type
static CanSILFunctionType getSILFunctionType(
    SILModule &M, AbstractionPattern origType,
    CanAnyFunctionType substFnInterfaceType, AnyFunctionType::ExtInfo extInfo,
    const Conventions &conventions, const ForeignInfo &foreignInfo,
    Optional<SILDeclRef> origConstant, Optional<SILDeclRef> constant,
    Optional<SubstitutionMap> reqtSubs,
    Optional<ProtocolConformanceRef> witnessMethodConformance) {
  // Per above, only fully honor opaqueness in the abstraction pattern
  // for thick or polymorphic functions.  We don't need to worry about
  // non-opaque patterns because the type-checker forbids non-thick
  // function types from having generic parameters or results.
  if (origType.isTypeParameter() &&
      substFnInterfaceType->getExtInfo().getSILRepresentation()
        != SILFunctionType::Representation::Thick &&
      isa<FunctionType>(substFnInterfaceType)) {
    origType = AbstractionPattern(M.Types.getCurGenericContext(),
                                  substFnInterfaceType);
  }

  // Find the generic parameters.
  CanGenericSignature genericSig =
    substFnInterfaceType.getOptGenericSignature();

  // Lower the interface type in a generic context.
  GenericContextScope scope(M.Types, genericSig);

  // Map 'throws' to the appropriate error convention.
  Optional<SILResultInfo> errorResult;
  assert((!foreignInfo.Error || substFnInterfaceType->getExtInfo().throws()) &&
         "foreignError was set but function type does not throw?");
  if (substFnInterfaceType->getExtInfo().throws() && !foreignInfo.Error) {
    assert(!origType.isForeign() &&
           "using native Swift error convention for foreign type!");
    SILType exnType = SILType::getExceptionType(M.getASTContext());
    assert(exnType.isObject());
    errorResult = SILResultInfo(exnType.getASTType(),
                                ResultConvention::Owned);
  }

  // Lower the result type.
  AbstractionPattern origResultType = origType.getFunctionResultType();
  CanType substFormalResultType = substFnInterfaceType.getResult();

  // If we have a foreign error convention, restore the original result type.
  if (auto convention = foreignInfo.Error) {
    std::tie(origResultType, substFormalResultType) =
        updateResultTypeForForeignError(*convention, genericSig, origResultType,
                                        substFormalResultType);
  }

  // Destructure the result tuple type.
  SmallVector<SILResultInfo, 8> results;
  {
    DestructureResults destructurer(M, conventions, results);
    destructurer.destructure(origResultType, substFormalResultType);
  }

  // Destructure the input tuple type.
  SmallVector<SILParameterInfo, 8> inputs;
  {
    DestructureInputs destructurer(M, conventions, foreignInfo, inputs);
    destructurer.destructure(origType,
                             substFnInterfaceType.getParams(),
                             extInfo);
  }

  // Destructure the coroutine yields.
  SILCoroutineKind coroutineKind = SILCoroutineKind::None;
  SmallVector<SILYieldInfo, 8> yields;
  destructureYieldsForCoroutine(M, origConstant, constant, reqtSubs,
                                yields, coroutineKind);
  
  // Lower the capture context parameters, if any.
  //
  // *NOTE* Currently default arg generators can not capture anything.
  // If we ever add that ability, it will be a different capture list
  // from the function to which the argument is attached.
  if (constant && !constant->isDefaultArgGenerator()) {
    if (auto function = constant->getAnyFunctionRef()) {
      auto expansion = ResilienceExpansion::Maximal;
      if (constant->isSerialized())
        expansion = ResilienceExpansion::Minimal;
      lowerCaptureContextParameters(M, *function, genericSig, expansion,
                                    inputs);
    }
  }
  
  auto calleeConvention = ParameterConvention::Direct_Unowned;
  if (extInfo.hasContext())
    calleeConvention = conventions.getCallee();

  bool pseudogeneric = genericSig && constant
    ? isPseudogeneric(*constant)
    : false;

  // NOTE: SILFunctionType::ExtInfo doesn't track everything that
  // AnyFunctionType::ExtInfo tracks. For example: 'throws' or 'auto-closure'
  auto silExtInfo = SILFunctionType::ExtInfo()
    .withRepresentation(extInfo.getSILRepresentation())
    .withIsPseudogeneric(pseudogeneric)
    .withNoEscape(extInfo.isNoEscape());
  
  return SILFunctionType::get(genericSig, silExtInfo, coroutineKind,
                              calleeConvention, inputs, yields,
                              results, errorResult, M.getASTContext(),
                              witnessMethodConformance);
}

//===----------------------------------------------------------------------===//
//                        Deallocator SILFunctionTypes
//===----------------------------------------------------------------------===//

namespace {

// The convention for general deallocators.
struct DeallocatorConventions : Conventions {
  DeallocatorConventions() : Conventions(ConventionsKind::Deallocator) {}

  ParameterConvention getIndirectParameter(unsigned index,
                             const AbstractionPattern &type,
                             const TypeLowering &substTL) const override {
    llvm_unreachable("Deallocators do not have indirect parameters");
  }

  ParameterConvention getDirectParameter(unsigned index,
                             const AbstractionPattern &type,
                             const TypeLowering &substTL) const override {
    llvm_unreachable("Deallocators do not have non-self direct parameters");
  }

  ParameterConvention getCallee() const override {
    llvm_unreachable("Deallocators do not have callees");
  }

  ResultConvention getResult(const TypeLowering &tl) const override {
    // TODO: Put an unreachable here?
    return ResultConvention::Owned;
  }

  ParameterConvention
  getDirectSelfParameter(const AbstractionPattern &type) const override {
    // TODO: Investigate whether or not it is
    return ParameterConvention::Direct_Owned;
  }

  ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const override {
    llvm_unreachable("Deallocators do not have indirect self parameters");
  }

  static bool classof(const Conventions *C) {
    return C->getKind() == ConventionsKind::Deallocator;
  }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                      Default Convention FunctionTypes
//===----------------------------------------------------------------------===//

namespace {

enum class NormalParameterConvention { Owned, Guaranteed };

/// The default Swift conventions.
class DefaultConventions : public Conventions {
  NormalParameterConvention normalParameterConvention;

public:
  DefaultConventions(NormalParameterConvention normalParameterConvention)
      : Conventions(ConventionsKind::Default),
        normalParameterConvention(normalParameterConvention) {}

  bool isNormalParameterConventionGuaranteed() const {
    return normalParameterConvention == NormalParameterConvention::Guaranteed;
  }

  ParameterConvention getIndirectParameter(unsigned index,
                            const AbstractionPattern &type,
                            const TypeLowering &substTL) const override {
    if (isNormalParameterConventionGuaranteed()) {
      return ParameterConvention::Indirect_In_Guaranteed;
    }
    return ParameterConvention::Indirect_In;
  }

  ParameterConvention getDirectParameter(unsigned index,
                            const AbstractionPattern &type,
                            const TypeLowering &substTL) const override {
    if (isNormalParameterConventionGuaranteed())
      return ParameterConvention::Direct_Guaranteed;
    return ParameterConvention::Direct_Owned;
  }

  ParameterConvention getCallee() const override {
    return DefaultThickCalleeConvention;
  }

  ResultConvention getResult(const TypeLowering &tl) const override {
    return ResultConvention::Owned;
  }

  ParameterConvention
  getDirectSelfParameter(const AbstractionPattern &type) const override {
    return ParameterConvention::Direct_Guaranteed;
  }

  ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const override {
    return ParameterConvention::Indirect_In_Guaranteed;
  }

  static bool classof(const Conventions *C) {
    return C->getKind() == ConventionsKind::Default;
  }
};

/// The default conventions for Swift initializing constructors.
///
/// Initializing constructors take all parameters (including) self at +1. This
/// is because:
///
/// 1. We are likely to be initializing fields of self implying that the
///    parameters are likely to be forwarded into memory without further
///    copies.
/// 2. Initializers must take 'self' at +1, since they will return it back
///    at +1, and may chain onto Objective-C initializers that replace the
///    instance.
struct DefaultInitializerConventions : DefaultConventions {
  DefaultInitializerConventions()
      : DefaultConventions(NormalParameterConvention::Owned) {}

  /// Initializers must take 'self' at +1, since they will return it back at +1,
  /// and may chain onto Objective-C initializers that replace the instance.
  ParameterConvention
  getDirectSelfParameter(const AbstractionPattern &type) const override {
    return ParameterConvention::Direct_Owned;
  }
  
  ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const override {
    return ParameterConvention::Indirect_In;
  }
};

/// The convention used for allocating inits. Allocating inits take their normal
/// parameters at +1 and do not have a self parameter.
struct DefaultAllocatorConventions : DefaultConventions {
  DefaultAllocatorConventions()
      : DefaultConventions(NormalParameterConvention::Owned) {}

  ParameterConvention
  getDirectSelfParameter(const AbstractionPattern &type) const override {
    llvm_unreachable("Allocating inits do not have self parameters");
  }

  ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const override {
    llvm_unreachable("Allocating inits do not have self parameters");
  }
};

/// The default conventions for Swift setter acccessors.
///
/// These take self at +0, but all other parameters at +1. This is because we
/// assume that setter parameters are likely to be values to be forwarded into
/// memory. Thus by passing in the +1 value, we avoid a potential copy in that
/// case.
struct DefaultSetterConventions : DefaultConventions {
  DefaultSetterConventions()
      : DefaultConventions(NormalParameterConvention::Owned) {}
};

/// The default conventions for ObjC blocks.
struct DefaultBlockConventions : Conventions {
  DefaultBlockConventions() : Conventions(ConventionsKind::DefaultBlock) {}

  ParameterConvention getIndirectParameter(unsigned index,
                            const AbstractionPattern &type,
                            const TypeLowering &substTL) const override {
    llvm_unreachable("indirect block parameters unsupported");
  }

  ParameterConvention getDirectParameter(unsigned index,
                            const AbstractionPattern &type,
                            const TypeLowering &substTL) const override {
    return ParameterConvention::Direct_Unowned;
  }

  ParameterConvention getCallee() const override {
    return ParameterConvention::Direct_Unowned;
  }

  ResultConvention getResult(const TypeLowering &substTL) const override {
    return ResultConvention::Autoreleased;
  }

  ParameterConvention
  getDirectSelfParameter(const AbstractionPattern &type) const override {
    llvm_unreachable("objc blocks do not have a self parameter");
  }

  ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const override {
    llvm_unreachable("objc blocks do not have a self parameter");
  }

  static bool classof(const Conventions *C) {
    return C->getKind() == ConventionsKind::DefaultBlock;
  }
};

} // end anonymous namespace

static CanSILFunctionType
getSILFunctionTypeForAbstractCFunction(SILModule &M,
                                       AbstractionPattern origType,
                                       CanAnyFunctionType substType,
                                       AnyFunctionType::ExtInfo extInfo,
                                       Optional<SILDeclRef> constant);

static CanSILFunctionType getNativeSILFunctionType(
    SILModule &M, AbstractionPattern origType,
    CanAnyFunctionType substInterfaceType, AnyFunctionType::ExtInfo extInfo,
    Optional<SILDeclRef> origConstant,
    Optional<SILDeclRef> constant,
    Optional<SubstitutionMap> reqtSubs,
    Optional<ProtocolConformanceRef> witnessMethodConformance) {
  assert(bool(origConstant) == bool(constant));
  switch (extInfo.getSILRepresentation()) {
  case SILFunctionType::Representation::Block:
  case SILFunctionType::Representation::CFunctionPointer:
    return getSILFunctionTypeForAbstractCFunction(M, origType,
                                                  substInterfaceType,
                                                  extInfo, constant);

  case SILFunctionType::Representation::Thin:
  case SILFunctionType::Representation::ObjCMethod:
  case SILFunctionType::Representation::Thick:
  case SILFunctionType::Representation::Method:
  case SILFunctionType::Representation::Closure:
  case SILFunctionType::Representation::WitnessMethod: {
    switch (constant ? constant->kind : SILDeclRef::Kind::Func) {
    case SILDeclRef::Kind::Initializer:
    case SILDeclRef::Kind::EnumElement:
      return getSILFunctionType(M, origType, substInterfaceType, extInfo,
                                DefaultInitializerConventions(), ForeignInfo(),
                                origConstant, constant, reqtSubs,
                                witnessMethodConformance);
    case SILDeclRef::Kind::Allocator:
      return getSILFunctionType(M, origType, substInterfaceType, extInfo,
                                DefaultAllocatorConventions(), ForeignInfo(),
                                origConstant, constant, reqtSubs,
                                witnessMethodConformance);
    case SILDeclRef::Kind::Func:
      // If we have a setter, use the special setter convention. This ensures
      // that we take normal parameters at +1.
      if (constant && constant->isSetter()) {
        return getSILFunctionType(M, origType, substInterfaceType, extInfo,
                                  DefaultSetterConventions(), ForeignInfo(),
                                  origConstant, constant, reqtSubs,
                                  witnessMethodConformance);
      }
      LLVM_FALLTHROUGH;
    case SILDeclRef::Kind::Destroyer:
    case SILDeclRef::Kind::GlobalAccessor:
    case SILDeclRef::Kind::DefaultArgGenerator:
    case SILDeclRef::Kind::StoredPropertyInitializer:
    case SILDeclRef::Kind::IVarInitializer:
    case SILDeclRef::Kind::IVarDestroyer: {
      auto conv = DefaultConventions(NormalParameterConvention::Guaranteed);
      return getSILFunctionType(M, origType, substInterfaceType, extInfo, conv,
                                ForeignInfo(), origConstant, constant, reqtSubs,
                                witnessMethodConformance);
    }
    case SILDeclRef::Kind::Deallocator:
      return getSILFunctionType(M, origType, substInterfaceType, extInfo,
                                DeallocatorConventions(), ForeignInfo(),
                                origConstant, constant, reqtSubs,
                                witnessMethodConformance);
    }
  }
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

CanSILFunctionType swift::getNativeSILFunctionType(
    SILModule &M, AbstractionPattern origType, CanAnyFunctionType substType,
    Optional<SILDeclRef> origConstant,
    Optional<SILDeclRef> substConstant,
    Optional<SubstitutionMap> reqtSubs,
    Optional<ProtocolConformanceRef> witnessMethodConformance) {
  AnyFunctionType::ExtInfo extInfo;

  // Preserve type information from the original type if possible.
  if (auto origFnType = origType.getAs<AnyFunctionType>()) {
    extInfo = origFnType->getExtInfo();

  // Otherwise, preserve function type attributes from the substituted type.
  } else {
    extInfo = substType->getExtInfo();
  }

  return ::getNativeSILFunctionType(M, origType, substType, extInfo,
                                    origConstant, substConstant, reqtSubs,
                                    witnessMethodConformance);
}

//===----------------------------------------------------------------------===//
//                          Foreign SILFunctionTypes
//===----------------------------------------------------------------------===//

static bool isCFTypedef(const TypeLowering &tl, clang::QualType type) {
  // If we imported a C pointer type as a non-trivial type, it was
  // a foreign class type.
  return !tl.isTrivial() && type->isPointerType();
}

/// Given nothing but a formal C parameter type that's passed
/// indirectly, deduce the convention for it.
///
/// Generally, whether the parameter is +1 is handled before this.
static ParameterConvention getIndirectCParameterConvention(clang::QualType type) {
  // Non-trivial C++ types would be Indirect_Inout (at least in Itanium).
  // A trivial const * parameter in C should be considered @in.
  return ParameterConvention::Indirect_In;
}

/// Given a C parameter declaration whose type is passed indirectly,
/// deduce the convention for it.
///
/// Generally, whether the parameter is +1 is handled before this.
static ParameterConvention
getIndirectCParameterConvention(const clang::ParmVarDecl *param) {
  return getIndirectCParameterConvention(param->getType());
}

/// Given nothing but a formal C parameter type that's passed
/// directly, deduce the convention for it.
///
/// Generally, whether the parameter is +1 is handled before this.
static ParameterConvention getDirectCParameterConvention(clang::QualType type) {
  return ParameterConvention::Direct_Unowned;
}

/// Given a C parameter declaration whose type is passed directly,
/// deduce the convention for it.
static ParameterConvention
getDirectCParameterConvention(const clang::ParmVarDecl *param) {
  if (param->hasAttr<clang::NSConsumedAttr>() ||
      param->hasAttr<clang::CFConsumedAttr>())
    return ParameterConvention::Direct_Owned;
  return getDirectCParameterConvention(param->getType());
}

// FIXME: that should be Direct_Guaranteed
const auto ObjCSelfConvention = ParameterConvention::Direct_Unowned;

namespace {

class ObjCMethodConventions : public Conventions {
  const clang::ObjCMethodDecl *Method;

public:
  const clang::ObjCMethodDecl *getMethod() const { return Method; }

  ObjCMethodConventions(const clang::ObjCMethodDecl *method)
    : Conventions(ConventionsKind::ObjCMethod), Method(method) {}

  ParameterConvention getIndirectParameter(unsigned index,
                           const AbstractionPattern &type,
                           const TypeLowering &substTL) const override {
    return getIndirectCParameterConvention(Method->param_begin()[index]);
  }

  ParameterConvention getDirectParameter(unsigned index,
                           const AbstractionPattern &type,
                           const TypeLowering &substTL) const override {
    return getDirectCParameterConvention(Method->param_begin()[index]);
  }

  ParameterConvention getCallee() const override {
    // Always thin.
    return ParameterConvention::Direct_Unowned;
  }

  /// Given that a method returns a CF type, infer its method
  /// family.  Unfortunately, Clang's getMethodFamily() never
  /// considers a method to be in a special family if its result
  /// doesn't satisfy isObjCRetainable().
  clang::ObjCMethodFamily getMethodFamilyForCFResult() const {
    // Trust an explicit attribute.
    if (auto attr = Method->getAttr<clang::ObjCMethodFamilyAttr>()) {
      switch (attr->getFamily()) {
      case clang::ObjCMethodFamilyAttr::OMF_None:
        return clang::OMF_None;
      case clang::ObjCMethodFamilyAttr::OMF_alloc:
        return clang::OMF_alloc;
      case clang::ObjCMethodFamilyAttr::OMF_copy:
        return clang::OMF_copy;
      case clang::ObjCMethodFamilyAttr::OMF_init:
        return clang::OMF_init;
      case clang::ObjCMethodFamilyAttr::OMF_mutableCopy:
        return clang::OMF_mutableCopy;
      case clang::ObjCMethodFamilyAttr::OMF_new:
        return clang::OMF_new;
      }
      llvm_unreachable("bad attribute value");
    }

    return Method->getSelector().getMethodFamily();
  }

  bool isImplicitPlusOneCFResult() const {
    switch (getMethodFamilyForCFResult()) {
    case clang::OMF_None:
    case clang::OMF_dealloc:
    case clang::OMF_finalize:
    case clang::OMF_retain:
    case clang::OMF_release:
    case clang::OMF_autorelease:
    case clang::OMF_retainCount:
    case clang::OMF_self:
    case clang::OMF_initialize:
    case clang::OMF_performSelector:
      return false;

    case clang::OMF_alloc:
    case clang::OMF_new:
    case clang::OMF_mutableCopy:
    case clang::OMF_copy:
      return true;

    case clang::OMF_init:
      return Method->isInstanceMethod();
    }
    llvm_unreachable("bad method family");
  }

  ResultConvention getResult(const TypeLowering &tl) const override {
    // If we imported the result as something trivial, we need to
    // use one of the unowned conventions.
    if (tl.isTrivial()) {
      if (Method->hasAttr<clang::ObjCReturnsInnerPointerAttr>())
        return ResultConvention::UnownedInnerPointer;

      auto type = tl.getLoweredType();
      if (type.unwrapOptionalType().getStructOrBoundGenericStruct()
          == type.getASTContext().getUnmanagedDecl())
        return ResultConvention::UnownedInnerPointer;
      return ResultConvention::Unowned;
    }

    // Otherwise, the return type had better be a retainable object pointer.
    auto resultType = Method->getReturnType();
    assert(resultType->isObjCRetainableType() || isCFTypedef(tl, resultType));

    // If it's retainable for the purposes of ObjC ARC, we can trust
    // the presence of ns_returns_retained, because Clang will add
    // that implicitly based on the method family.
    if (resultType->isObjCRetainableType()) {
      if (Method->hasAttr<clang::NSReturnsRetainedAttr>())
        return ResultConvention::Owned;
      return ResultConvention::Autoreleased;
    }

    // Otherwise, it's a CF return type, which unfortunately means
    // we can't just trust getMethodFamily().  We should really just
    // change that, but that's an annoying change to make to Clang
    // right now.
    assert(isCFTypedef(tl, resultType));

    // Trust the explicit attributes.
    if (Method->hasAttr<clang::CFReturnsRetainedAttr>())
      return ResultConvention::Owned;
    if (Method->hasAttr<clang::CFReturnsNotRetainedAttr>())
      return ResultConvention::Autoreleased;

    // Otherwise, infer based on the method family.
    if (isImplicitPlusOneCFResult())
      return ResultConvention::Owned;
    return ResultConvention::Autoreleased;
  }

  ParameterConvention
  getDirectSelfParameter(const AbstractionPattern &type) const override {
    if (Method->hasAttr<clang::NSConsumesSelfAttr>())
      return ParameterConvention::Direct_Owned;

    // The caller is supposed to take responsibility for ensuring
    // that 'self' survives a method call.
    return ObjCSelfConvention;
  }

  ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const override {
    llvm_unreachable("objc methods do not support indirect self parameters");
  }

  static bool classof(const Conventions *C) {
    return C->getKind() == ConventionsKind::ObjCMethod;
  }
};

/// Conventions based on a C function type.
class CFunctionTypeConventions : public Conventions {
  const clang::FunctionType *FnType;

  clang::QualType getParamType(unsigned i) const {
    return FnType->castAs<clang::FunctionProtoType>()->getParamType(i);
  }

protected:
  /// Protected constructor for subclasses to override the kind passed to the
  /// super class.
  CFunctionTypeConventions(ConventionsKind kind,
                           const clang::FunctionType *type)
    : Conventions(kind), FnType(type) {}

public:
  CFunctionTypeConventions(const clang::FunctionType *type)
    : Conventions(ConventionsKind::CFunctionType), FnType(type) {}

  ParameterConvention getIndirectParameter(unsigned index,
                            const AbstractionPattern &type,
                           const TypeLowering &substTL) const override {
    return getIndirectCParameterConvention(getParamType(index));
  }

  ParameterConvention getDirectParameter(unsigned index,
                            const AbstractionPattern &type,
                           const TypeLowering &substTL) const override {
    if (cast<clang::FunctionProtoType>(FnType)->isParamConsumed(index))
      return ParameterConvention::Direct_Owned;
    return getDirectCParameterConvention(getParamType(index));
  }

  ParameterConvention getCallee() const override {
    // FIXME: blocks should be Direct_Guaranteed.
    return ParameterConvention::Direct_Unowned;
  }

  ResultConvention getResult(const TypeLowering &tl) const override {
    if (tl.isTrivial())
      return ResultConvention::Unowned;
    if (FnType->getExtInfo().getProducesResult())
      return ResultConvention::Owned;
    return ResultConvention::Autoreleased;
  }

  ParameterConvention
  getDirectSelfParameter(const AbstractionPattern &type) const override {
    llvm_unreachable("c function types do not have a self parameter");
  }

  ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const override {
    llvm_unreachable("c function types do not have a self parameter");
  }

  static bool classof(const Conventions *C) {
    return C->getKind() == ConventionsKind::CFunctionType;
  }
};

/// Conventions based on C function declarations.
class CFunctionConventions : public CFunctionTypeConventions {
  using super = CFunctionTypeConventions;
  const clang::FunctionDecl *TheDecl;
public:
  CFunctionConventions(const clang::FunctionDecl *decl)
    : CFunctionTypeConventions(ConventionsKind::CFunction,
                               decl->getType()->castAs<clang::FunctionType>()),
      TheDecl(decl) {}

  ParameterConvention getDirectParameter(unsigned index,
                            const AbstractionPattern &type,
                            const TypeLowering &substTL) const override {
    if (auto param = TheDecl->getParamDecl(index))
      if (param->hasAttr<clang::CFConsumedAttr>())
        return ParameterConvention::Direct_Owned;
    return super::getDirectParameter(index, type, substTL);
  }

  ResultConvention getResult(const TypeLowering &tl) const override {
    if (isCFTypedef(tl, TheDecl->getReturnType())) {
      // The CF attributes aren't represented in the type, so we need
      // to check them here.
      if (TheDecl->hasAttr<clang::CFReturnsRetainedAttr>()) {
        return ResultConvention::Owned;
      } else if (TheDecl->hasAttr<clang::CFReturnsNotRetainedAttr>()) {
        // Probably not actually autoreleased.
        return ResultConvention::Autoreleased;

      // The CF Create/Copy rule only applies to functions that return
      // a CF-runtime type; it does not apply to methods, and it does
      // not apply to functions returning ObjC types.
      } else if (clang::ento::coreFoundation::followsCreateRule(TheDecl)) {
        return ResultConvention::Owned;
      } else {
        return ResultConvention::Autoreleased;
      }
    }

    // Otherwise, fall back on the ARC annotations, which are part
    // of the type.
    return super::getResult(tl);
  }

  static bool classof(const Conventions *C) {
    return C->getKind() == ConventionsKind::CFunction;
  }
};

/// Conventions based on C++ method declarations.
class CXXMethodConventions : public CFunctionTypeConventions {
  using super = CFunctionTypeConventions;
  const clang::CXXMethodDecl *TheDecl;

public:
  CXXMethodConventions(const clang::CXXMethodDecl *decl)
      : CFunctionTypeConventions(
            ConventionsKind::CXXMethod,
            decl->getType()->castAs<clang::FunctionType>()),
        TheDecl(decl) {}
  ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const override {
    if (TheDecl->isConst())
      return ParameterConvention::Indirect_In_Guaranteed;
    return ParameterConvention::Indirect_Inout;
  }
  static bool classof(const Conventions *C) {
    return C->getKind() == ConventionsKind::CXXMethod;
  }
};

} // end anonymous namespace

/// Given that we have an imported Clang declaration, deduce the
/// ownership conventions for calling it and build the SILFunctionType.
static CanSILFunctionType
getSILFunctionTypeForClangDecl(SILModule &M, const clang::Decl *clangDecl,
                               CanAnyFunctionType origType,
                               CanAnyFunctionType substInterfaceType,
                               AnyFunctionType::ExtInfo extInfo,
                               const ForeignInfo &foreignInfo,
                               Optional<SILDeclRef> constant) {
  if (auto method = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
    auto origPattern =
      AbstractionPattern::getObjCMethod(origType, method, foreignInfo.Error);
    return getSILFunctionType(M, origPattern, substInterfaceType, extInfo,
                              ObjCMethodConventions(method), foreignInfo,
                              constant, constant, None,
                              /*witnessMethodConformance=*/None);
  }

  if (auto method = dyn_cast<clang::CXXMethodDecl>(clangDecl)) {
    AbstractionPattern origPattern =
        AbstractionPattern::getCXXMethod(origType, method);
    auto conventions = CXXMethodConventions(method);
    return getSILFunctionType(M, origPattern, substInterfaceType, extInfo,
                              conventions, foreignInfo, constant, constant,
                              None,
                              /*witnessMethodConformance=*/None);
  }

  if (auto func = dyn_cast<clang::FunctionDecl>(clangDecl)) {
    auto clangType = func->getType().getTypePtr();
    AbstractionPattern origPattern =
      foreignInfo.Self.isImportAsMember()
        ? AbstractionPattern::getCFunctionAsMethod(origType, clangType,
                                                   foreignInfo.Self)
        : AbstractionPattern(origType, clangType);
    return getSILFunctionType(M, origPattern, substInterfaceType, extInfo,
                              CFunctionConventions(func), foreignInfo,
                              constant, constant, None,
                              /*witnessMethodConformance=*/None);
  }

  llvm_unreachable("call to unknown kind of C function");
}

static CanSILFunctionType
getSILFunctionTypeForAbstractCFunction(SILModule &M,
                                       AbstractionPattern origType,
                                       CanAnyFunctionType substType,
                                       AnyFunctionType::ExtInfo extInfo,
                                       Optional<SILDeclRef> constant) {
  if (origType.isClangType()) {
    auto clangType = origType.getClangType();
    const clang::FunctionType *fnType;
    if (auto blockPtr = clangType->getAs<clang::BlockPointerType>()) {
      fnType = blockPtr->getPointeeType()->castAs<clang::FunctionType>();
    } else if (auto ptr = clangType->getAs<clang::PointerType>()) {
      fnType = ptr->getPointeeType()->getAs<clang::FunctionType>();
    } else if (auto ref = clangType->getAs<clang::ReferenceType>()) {
      fnType = ref->getPointeeType()->getAs<clang::FunctionType>();
    } else if (auto fn = clangType->getAs<clang::FunctionType>()) {
      fnType = fn;
    } else {
      llvm_unreachable("unexpected type imported as a function type");
    }
    if (fnType) {
      return getSILFunctionType(M, origType, substType, extInfo,
                                CFunctionTypeConventions(fnType), ForeignInfo(),
                                constant, constant, None,
                                /*witnessMethodConformance=*/None);
    }
  }

  // TODO: Ought to support captures in block funcs.
  return getSILFunctionType(M, origType, substType, extInfo,
                            DefaultBlockConventions(), ForeignInfo(),
                            constant, constant, None,
                            /*witnessMethodConformance=*/None);
}

/// Try to find a clang method declaration for the given function.
static const clang::Decl *findClangMethod(ValueDecl *method) {
  if (auto *methodFn = dyn_cast<FuncDecl>(method)) {
    if (auto *decl = methodFn->getClangDecl())
      return decl;

    if (auto overridden = methodFn->getOverriddenDecl())
      return findClangMethod(overridden);
  }

  if (auto *constructor = dyn_cast<ConstructorDecl>(method)) {
    if (auto *decl = constructor->getClangDecl())
      return decl;
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
//                      Selector Family SILFunctionTypes
//===----------------------------------------------------------------------===//

/// Derive the ObjC selector family from an identifier.
///
/// Note that this will never derive the Init family, which is too dangerous
/// to leave to chance. Swift functions starting with "init" are always
/// emitted as if they are part of the "none" family.
static ObjCSelectorFamily getObjCSelectorFamily(ObjCSelector name) {
  auto result = name.getSelectorFamily();

  if (result == ObjCSelectorFamily::Init)
    return ObjCSelectorFamily::None;

  return result;
}

/// Get the ObjC selector family a foreign SILDeclRef belongs to.
static ObjCSelectorFamily getObjCSelectorFamily(SILDeclRef c) {
  assert(c.isForeign);
  switch (c.kind) {
  case SILDeclRef::Kind::Func: {
    if (!c.hasDecl())
      return ObjCSelectorFamily::None;
      
    auto *FD = cast<FuncDecl>(c.getDecl());
    if (auto accessor = dyn_cast<AccessorDecl>(FD)) {
      switch (accessor->getAccessorKind()) {
      case AccessorKind::Get:
      case AccessorKind::Set:
        break;
#define OBJC_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
      case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
        llvm_unreachable("Unexpected AccessorKind of foreign FuncDecl");
      }
    }

    return getObjCSelectorFamily(FD->getObjCSelector());
  }
  case SILDeclRef::Kind::Initializer:
  case SILDeclRef::Kind::IVarInitializer:
    return ObjCSelectorFamily::Init;

  /// Currently IRGen wraps alloc/init methods into Swift constructors
  /// with Swift conventions.
  case SILDeclRef::Kind::Allocator:
  /// These constants don't correspond to method families we care about yet.
  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::Deallocator:
  case SILDeclRef::Kind::IVarDestroyer:
    return ObjCSelectorFamily::None;

  case SILDeclRef::Kind::EnumElement:
  case SILDeclRef::Kind::GlobalAccessor:
  case SILDeclRef::Kind::DefaultArgGenerator:
  case SILDeclRef::Kind::StoredPropertyInitializer:
    llvm_unreachable("Unexpected Kind of foreign SILDeclRef");
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

namespace {

class ObjCSelectorFamilyConventions : public Conventions {
  ObjCSelectorFamily Family;

public:
  ObjCSelectorFamilyConventions(ObjCSelectorFamily family)
    : Conventions(ConventionsKind::ObjCSelectorFamily), Family(family) {}

  ParameterConvention getIndirectParameter(unsigned index,
                                           const AbstractionPattern &type,
                                 const TypeLowering &substTL) const override {
    return ParameterConvention::Indirect_In;
  }

  ParameterConvention getDirectParameter(unsigned index,
                                         const AbstractionPattern &type,
                                 const TypeLowering &substTL) const override {
    return ParameterConvention::Direct_Unowned;
  }

  ParameterConvention getCallee() const override {
    // Always thin.
    return ParameterConvention::Direct_Unowned;
  }

  ResultConvention getResult(const TypeLowering &tl) const override {
    switch (Family) {
    case ObjCSelectorFamily::Alloc:
    case ObjCSelectorFamily::Copy:
    case ObjCSelectorFamily::Init:
    case ObjCSelectorFamily::MutableCopy:
    case ObjCSelectorFamily::New:
      return ResultConvention::Owned;

    case ObjCSelectorFamily::None:
      // Defaults below.
      break;
    }

    // Get the underlying AST type, potentially stripping off one level of
    // optionality while we do it.
    CanType type = tl.getLoweredType().unwrapOptionalType().getASTType();
    if (type->hasRetainablePointerRepresentation()
        || (type->getSwiftNewtypeUnderlyingType() && !tl.isTrivial()))
      return ResultConvention::Autoreleased;

    return ResultConvention::Unowned;
  }

  ParameterConvention
  getDirectSelfParameter(const AbstractionPattern &type) const override {
    if (Family == ObjCSelectorFamily::Init)
      return ParameterConvention::Direct_Owned;
    return ObjCSelfConvention;
  }

  ParameterConvention
  getIndirectSelfParameter(const AbstractionPattern &type) const override {
    llvm_unreachable("selector family objc function types do not support "
                     "indirect self parameters");
  }

  static bool classof(const Conventions *C) {
    return C->getKind() == ConventionsKind::ObjCSelectorFamily;
  }
};

} // end anonymous namespace

static CanSILFunctionType
getSILFunctionTypeForObjCSelectorFamily(SILModule &M, ObjCSelectorFamily family,
                                        CanAnyFunctionType origType,
                                        CanAnyFunctionType substInterfaceType,
                                        AnyFunctionType::ExtInfo extInfo,
                                        const ForeignInfo &foreignInfo,
                                        Optional<SILDeclRef> constant) {
  return getSILFunctionType(M, AbstractionPattern(origType), substInterfaceType,
                            extInfo, ObjCSelectorFamilyConventions(family),
                            foreignInfo, constant, constant,
                            /*requirement subs*/None,
                            /*witnessMethodConformance=*/None);
}

static bool isImporterGeneratedAccessor(const clang::Decl *clangDecl,
                                        SILDeclRef constant) {
  // Must be an accessor.
  auto accessor = dyn_cast<AccessorDecl>(constant.getDecl());
  if (!accessor)
    return false;

  // Must be a type member.
  if (constant.getParameterListCount() != 2)
    return false;

  // Must be imported from a function.
  if (!isa<clang::FunctionDecl>(clangDecl))
    return false;

  return true;
}

static CanSILFunctionType
getUncachedSILFunctionTypeForConstant(SILModule &M,
                                  SILDeclRef constant,
                                  CanAnyFunctionType origLoweredInterfaceType) {
  assert(origLoweredInterfaceType->getExtInfo().getSILRepresentation()
           != SILFunctionTypeRepresentation::Thick
         && origLoweredInterfaceType->getExtInfo().getSILRepresentation()
             != SILFunctionTypeRepresentation::Block);

  auto extInfo = origLoweredInterfaceType->getExtInfo();

  if (!constant.isForeign) {
    Optional<ProtocolConformanceRef> witnessMethodConformance;

    if (extInfo.getSILRepresentation() ==
        SILFunctionTypeRepresentation::WitnessMethod) {
      auto proto = constant.getDecl()->getDeclContext()->getSelfProtocolDecl();
      witnessMethodConformance = ProtocolConformanceRef(proto);
    }

    return ::getNativeSILFunctionType(
        M, AbstractionPattern(origLoweredInterfaceType),
        origLoweredInterfaceType, extInfo,
        constant, constant, None, witnessMethodConformance);
  }

  ForeignInfo foreignInfo;

  // If we have a clang decl associated with the Swift decl, derive its
  // ownership conventions.
  if (constant.hasDecl()) {
    auto decl = constant.getDecl();
    if (auto funcDecl = dyn_cast<AbstractFunctionDecl>(decl)) {
      foreignInfo.Error = funcDecl->getForeignErrorConvention();
      foreignInfo.Self = funcDecl->getImportAsMemberStatus();
    }

    if (auto clangDecl = findClangMethod(decl)) {
      // The importer generates accessors that are not actually
      // import-as-member but do involve the same gymnastics with the
      // formal type.  That's all that SILFunctionType cares about, so
      // pretend that it's import-as-member.
      if (!foreignInfo.Self.isImportAsMember() &&
          isImporterGeneratedAccessor(clangDecl, constant)) {
        unsigned selfIndex = cast<AccessorDecl>(decl)->isSetter() ? 1 : 0;
        foreignInfo.Self.setSelfIndex(selfIndex);
      }

      return getSILFunctionTypeForClangDecl(M, clangDecl,
                                            origLoweredInterfaceType,
                                            origLoweredInterfaceType,
                                            extInfo, foreignInfo, constant);
    }
  }

  // If the decl belongs to an ObjC method family, use that family's
  // ownership conventions.
  return getSILFunctionTypeForObjCSelectorFamily(
      M, getObjCSelectorFamily(constant),
      origLoweredInterfaceType, origLoweredInterfaceType,
      extInfo, foreignInfo, constant);
}

CanSILFunctionType TypeConverter::
getUncachedSILFunctionTypeForConstant(SILDeclRef constant,
                                      CanAnyFunctionType origInterfaceType) {
  auto origLoweredInterfaceType =
    getLoweredFormalTypes(constant, origInterfaceType).Uncurried;
  return ::getUncachedSILFunctionTypeForConstant(M, constant,
                                                 origLoweredInterfaceType);
}

static bool isClassOrProtocolMethod(ValueDecl *vd) {
  if (!vd->getDeclContext())
    return false;
  Type contextType = vd->getDeclContext()->getDeclaredInterfaceType();
  if (!contextType)
    return false;
  return contextType->getClassOrBoundGenericClass()
    || contextType->isClassExistentialType();
}

SILFunctionTypeRepresentation
TypeConverter::getDeclRefRepresentation(SILDeclRef c) {
  // Currying thunks always have freestanding CC.
  if (c.isCurried)
    return SILFunctionTypeRepresentation::Thin;

  // If this is a foreign thunk, it always has the foreign calling convention.
  if (c.isForeign) {
    if (!c.hasDecl() ||
        c.getDecl()->isImportAsMember())
      return SILFunctionTypeRepresentation::CFunctionPointer;

    if (isClassOrProtocolMethod(c.getDecl()) ||
        c.kind == SILDeclRef::Kind::IVarInitializer ||
        c.kind == SILDeclRef::Kind::IVarDestroyer)
      return SILFunctionTypeRepresentation::ObjCMethod;

    return SILFunctionTypeRepresentation::CFunctionPointer;
  }

  // Anonymous functions currently always have Freestanding CC.
  if (!c.hasDecl())
    return SILFunctionTypeRepresentation::Thin;

  // FIXME: Assert that there is a native entry point
  // available. There's no great way to do this.

  // Protocol witnesses are called using the witness calling convention.
  if (auto proto = dyn_cast<ProtocolDecl>(c.getDecl()->getDeclContext())) {
    // Use the regular method convention for foreign-to-native thunks.
    if (c.isForeignToNativeThunk())
      return SILFunctionTypeRepresentation::Method;
    assert(!c.isNativeToForeignThunk() && "shouldn't be possible");
    return getProtocolWitnessRepresentation(proto);
  }

  switch (c.kind) {
    case SILDeclRef::Kind::GlobalAccessor:
    case SILDeclRef::Kind::DefaultArgGenerator:
    case SILDeclRef::Kind::StoredPropertyInitializer:
      return SILFunctionTypeRepresentation::Thin;

    case SILDeclRef::Kind::Func:
      if (c.getDecl()->getDeclContext()->isTypeContext())
        return SILFunctionTypeRepresentation::Method;
      return SILFunctionTypeRepresentation::Thin;

    case SILDeclRef::Kind::Destroyer:
    case SILDeclRef::Kind::Deallocator:
    case SILDeclRef::Kind::Allocator:
    case SILDeclRef::Kind::Initializer:
    case SILDeclRef::Kind::EnumElement:
    case SILDeclRef::Kind::IVarInitializer:
    case SILDeclRef::Kind::IVarDestroyer:
      return SILFunctionTypeRepresentation::Method;
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

// Provide the ability to turn off the type converter cache to ease debugging.
static llvm::cl::opt<bool>
    DisableConstantInfoCache("sil-disable-typelowering-constantinfo-cache",
                             llvm::cl::init(false));

const SILConstantInfo &TypeConverter::getConstantInfo(SILDeclRef constant) {
  if (!DisableConstantInfoCache) {
    auto found = ConstantTypes.find(constant);
    if (found != ConstantTypes.end())
      return *found->second;
  }

  // First, get a function type for the constant.  This creates the
  // right type for a getter or setter.
  auto formalInterfaceType = makeConstantInterfaceType(constant);

  // The formal type is just that with the right representation.
  auto rep = getDeclRefRepresentation(constant);
  formalInterfaceType = adjustFunctionType(formalInterfaceType, rep);

  // The lowered type is the formal type, but uncurried and with
  // parameters automatically turned into their bridged equivalents.
  auto bridgedTypes = getLoweredFormalTypes(constant, formalInterfaceType);

  CanAnyFunctionType loweredInterfaceType = bridgedTypes.Uncurried;

  // The SIL type encodes conventions according to the original type.
  CanSILFunctionType silFnType =
    ::getUncachedSILFunctionTypeForConstant(M, constant,
                                            loweredInterfaceType);

  LLVM_DEBUG(llvm::dbgs() << "lowering type for constant ";
             constant.print(llvm::dbgs());
             llvm::dbgs() << "\n  formal type: ";
             formalInterfaceType.print(llvm::dbgs());
             llvm::dbgs() << "\n  lowered AST type: ";
             loweredInterfaceType.print(llvm::dbgs());
             llvm::dbgs() << "\n  SIL type: ";
             silFnType.print(llvm::dbgs());
             llvm::dbgs() << "\n");

  auto resultBuf = M.allocate(sizeof(SILConstantInfo),
                              alignof(SILConstantInfo));

  auto result = ::new (resultBuf) SILConstantInfo{formalInterfaceType,
                                                  bridgedTypes.Pattern,
                                                  loweredInterfaceType,
                                                  silFnType};
  if (DisableConstantInfoCache)
    return *result;

  auto inserted = ConstantTypes.insert({constant, result});
  assert(inserted.second);
  (void)inserted;
  return *result;
}

/// Returns the SILParameterInfo for the given declaration's `self` parameter.
/// `constant` must refer to a method.
SILParameterInfo TypeConverter::getConstantSelfParameter(SILDeclRef constant) {
  auto ty = getConstantFunctionType(constant);

  // In most cases the "self" parameter is lowered as the back parameter.
  // The exception is C functions imported as methods.
  if (!constant.isForeign)
    return ty->getParameters().back();
  if (!constant.hasDecl())
    return ty->getParameters().back();
  auto fn = dyn_cast<AbstractFunctionDecl>(constant.getDecl());
  if (!fn)
    return ty->getParameters().back();
  if (fn->isImportAsStaticMember())
    return SILParameterInfo();
  if (fn->isImportAsInstanceMember())
    return ty->getParameters()[fn->getSelfIndex()];
  return ty->getParameters().back();
}

// This check duplicates TypeConverter::checkForABIDifferences(),
// but on AST types. The issue is we only want to introduce a new
// vtable thunk if the AST type changes, but an abstraction change
// is OK; we don't want a new entry if an @in parameter became
// @guaranteed or whatever.
static bool checkASTTypeForABIDifferences(CanType type1,
                                          CanType type2) {
  return !type1->matches(type2, TypeMatchFlags::AllowABICompatible);
}

// FIXME: This makes me very upset. Can we do without this?
static CanType copyOptionalityFromDerivedToBase(TypeConverter &tc,
                                                CanType derived,
                                                CanType base) {
  // Unwrap optionals, but remember that we did.
  bool derivedWasOptional = false;
  if (auto object = derived.getOptionalObjectType()) {
    derivedWasOptional = true;
    derived = object;
  }
  if (auto object = base.getOptionalObjectType()) {
    base = object;
  }

  // T? +> S = (T +> S)?
  // T? +> S? = (T +> S)?
  if (derivedWasOptional) {
    base = copyOptionalityFromDerivedToBase(tc, derived, base);

    auto optDecl = tc.Context.getOptionalDecl();
    return CanType(BoundGenericEnumType::get(optDecl, Type(), base));
  }

  // (T1, T2, ...) +> (S1, S2, ...) = (T1 +> S1, T2 +> S2, ...)
  if (auto derivedTuple = dyn_cast<TupleType>(derived)) {
    if (auto baseTuple = dyn_cast<TupleType>(base)) {
      assert(derivedTuple->getNumElements() == baseTuple->getNumElements());
      SmallVector<TupleTypeElt, 4> elements;
      for (unsigned i = 0, e = derivedTuple->getNumElements(); i < e; i++) {
        elements.push_back(
          baseTuple->getElement(i).getWithType(
            copyOptionalityFromDerivedToBase(
              tc,
              derivedTuple.getElementType(i),
              baseTuple.getElementType(i))));
      }
      return CanType(TupleType::get(elements, tc.Context));
    }
  }

  // (T1 -> T2) +> (S1 -> S2) = (T1 +> S1) -> (T2 +> S2)
  if (auto derivedFunc = dyn_cast<AnyFunctionType>(derived)) {
    if (auto baseFunc = dyn_cast<AnyFunctionType>(base)) {
      SmallVector<FunctionType::Param, 8> params;

      auto derivedParams = derivedFunc.getParams();
      auto baseParams = baseFunc.getParams();
      assert(derivedParams.size() == baseParams.size());
      for (unsigned i = 0, e = derivedParams.size(); i < e; i++) {
        assert(derivedParams[i].getParameterFlags() ==
               baseParams[i].getParameterFlags());

        params.emplace_back(
          copyOptionalityFromDerivedToBase(
            tc,
            derivedParams[i].getPlainType(),
            baseParams[i].getPlainType()),
          Identifier(),
          baseParams[i].getParameterFlags());
      }

      auto result = copyOptionalityFromDerivedToBase(tc,
                                                     derivedFunc.getResult(),
                                                     baseFunc.getResult());
      return CanAnyFunctionType::get(baseFunc.getOptGenericSignature(),
                                     llvm::makeArrayRef(params), result,
                                     baseFunc->getExtInfo());
    }
  }

  return base;
}

/// Returns the ConstantInfo corresponding to the VTable thunk for overriding.
/// Will be the same as getConstantInfo if the declaration does not override.
const SILConstantInfo &
TypeConverter::getConstantOverrideInfo(SILDeclRef derived, SILDeclRef base) {
  // Foreign overrides currently don't need reabstraction.
  if (derived.isForeign)
    return getConstantInfo(derived);

  auto found = ConstantOverrideTypes.find({derived, base});
  if (found != ConstantOverrideTypes.end())
    return *found->second;

  assert(base.requiresNewVTableEntry() && "base must not be an override");

  auto baseInfo = getConstantInfo(base);
  auto derivedInfo = getConstantInfo(derived);

  // If the derived method is ABI-compatible with the base method, give the
  // vtable thunk the same signature as the derived method.
  auto basePattern = AbstractionPattern(baseInfo.LoweredType);

  auto baseInterfaceTy = baseInfo.FormalType;
  auto derivedInterfaceTy = derivedInfo.FormalType;

  auto params = derivedInterfaceTy.getParams();
  assert(params.size() == 1);
  auto selfInterfaceTy = params[0].getPlainType()->getMetatypeInstanceType();

  auto overrideInterfaceTy =
      selfInterfaceTy->adjustSuperclassMemberDeclType(
          base.getDecl(), derived.getDecl(), baseInterfaceTy);

  // Copy generic signature from derived to the override type, to handle
  // the case where the base member is not generic (because the base class
  // is concrete) but the derived member is generic (because the derived
  // class is generic).
  if (auto derivedInterfaceFnTy = derivedInterfaceTy->getAs<GenericFunctionType>()) {
    auto overrideInterfaceFnTy = overrideInterfaceTy->castTo<FunctionType>();
    overrideInterfaceTy =
        GenericFunctionType::get(derivedInterfaceFnTy->getGenericSignature(),
                                 overrideInterfaceFnTy->getParams(),
                                 overrideInterfaceFnTy->getResult(),
                                 overrideInterfaceFnTy->getExtInfo());
  }

  // Lower the formal AST type.
  auto bridgedTypes = getLoweredFormalTypes(derived,
      cast<AnyFunctionType>(overrideInterfaceTy->getCanonicalType()));
  auto overrideLoweredInterfaceTy = bridgedTypes.Uncurried;

  if (!checkASTTypeForABIDifferences(derivedInfo.LoweredType,
                                     overrideLoweredInterfaceTy)) {
    basePattern = AbstractionPattern(
      copyOptionalityFromDerivedToBase(
        *this,
        derivedInfo.LoweredType,
        baseInfo.LoweredType));
    overrideLoweredInterfaceTy = derivedInfo.LoweredType;
  }

  // Build the SILFunctionType for the vtable thunk.
  CanSILFunctionType fnTy = getNativeSILFunctionType(
      M, basePattern, overrideLoweredInterfaceTy, base, derived,
      /*reqt subs*/None, /*witnessMethodConformance=*/None);

  // Build the SILConstantInfo and cache it.
  auto resultBuf = M.allocate(sizeof(SILConstantInfo),
                              alignof(SILConstantInfo));
  auto result = ::new (resultBuf) SILConstantInfo{
    derivedInterfaceTy,
    bridgedTypes.Pattern,
    overrideLoweredInterfaceTy,
    fnTy};
  
  auto inserted = ConstantOverrideTypes.insert({{derived, base}, result});
  assert(inserted.second);
  (void)inserted;
  return *result;
}

namespace {

/// Given a lowered SIL type, apply a substitution to it to produce another
/// lowered SIL type which uses the same abstraction conventions.
class SILTypeSubstituter :
    public CanTypeVisitor<SILTypeSubstituter, CanType> {
  SILModule &TheSILModule;
  TypeSubstitutionFn Subst;
  LookupConformanceFn Conformances;
  // The signature for the original type.
  //
  // Replacement types are lowered with respect to the current
  // context signature.
  CanGenericSignature Sig;

  bool shouldSubstituteOpaqueArchetypes;

  ASTContext &getASTContext() { return TheSILModule.getASTContext(); }

public:
  SILTypeSubstituter(SILModule &silModule,
                     TypeSubstitutionFn Subst,
                     LookupConformanceFn Conformances,
                     CanGenericSignature Sig,
                     bool shouldSubstituteOpaqueArchetypes)
    : TheSILModule(silModule),
      Subst(Subst),
      Conformances(Conformances),
      Sig(Sig),
      shouldSubstituteOpaqueArchetypes(shouldSubstituteOpaqueArchetypes)
  {}

  // SIL type lowering only does special things to tuples and functions.

  // When a function appears inside of another type, we only perform
  // substitutions if it does not have a generic signature.
  CanSILFunctionType visitSILFunctionType(CanSILFunctionType origType) {
    if (origType->getGenericSignature())
      return origType;

    return substSILFunctionType(origType);
  }

  // Entry point for use by SILType::substGenericArgs().
  CanSILFunctionType substSILFunctionType(CanSILFunctionType origType) {
    SmallVector<SILResultInfo, 8> substResults;
    substResults.reserve(origType->getNumResults());
    for (auto origResult : origType->getResults()) {
      substResults.push_back(subst(origResult));
    }

    auto substErrorResult = origType->getOptionalErrorResult();
    assert(!substErrorResult ||
           (!substErrorResult->getType()->hasTypeParameter() &&
            !substErrorResult->getType()->hasArchetype()));

    SmallVector<SILParameterInfo, 8> substParams;
    substParams.reserve(origType->getParameters().size());
    for (auto &origParam : origType->getParameters()) {
      substParams.push_back(subst(origParam));
    }

    SmallVector<SILYieldInfo, 8> substYields;
    substYields.reserve(origType->getYields().size());
    for (auto &origYield : origType->getYields()) {
      substYields.push_back(subst(origYield));
    }

    Optional<ProtocolConformanceRef> witnessMethodConformance;
    if (auto conformance = origType->getWitnessMethodConformanceOrNone()) {
      assert(origType->getExtInfo().hasSelfParam());
      auto selfType = origType->getSelfParameter().getType();
      // The Self type can be nested in a few layers of metatypes (etc.).
      while (auto metatypeType = dyn_cast<MetatypeType>(selfType)) {
        auto next = metatypeType.getInstanceType();
        if (next == selfType)
          break;
        selfType = next;
      }
      witnessMethodConformance =
          conformance->subst(selfType, Subst, Conformances);
    }
    
    // The substituted type is no longer generic, so it'd never be
    // pseudogeneric.
    auto extInfo = origType->getExtInfo().withIsPseudogeneric(false);

    return SILFunctionType::get(nullptr, extInfo,
                                origType->getCoroutineKind(),
                                origType->getCalleeConvention(), substParams,
                                substYields, substResults, substErrorResult,
                                getASTContext(), witnessMethodConformance);
  }

  SILType subst(SILType type) {
    return SILType::getPrimitiveType(visit(type.getASTType()),
                                     type.getCategory());
  }

  SILResultInfo subst(SILResultInfo orig) {
    return SILResultInfo(visit(orig.getType()), orig.getConvention());
  }

  SILYieldInfo subst(SILYieldInfo orig) {
    return SILYieldInfo(visit(orig.getType()), orig.getConvention());
  }

  SILParameterInfo subst(SILParameterInfo orig) {
    return SILParameterInfo(visit(orig.getType()), orig.getConvention());
  }

  /// Tuples need to have their component types substituted by these
  /// same rules.
  CanType visitTupleType(CanTupleType origType) {
    // Fast-path the empty tuple.
    if (origType->getNumElements() == 0) return origType;

    SmallVector<TupleTypeElt, 8> substElts;
    substElts.reserve(origType->getNumElements());
    for (auto &origElt : origType->getElements()) {
      auto substEltType = visit(CanType(origElt.getType()));
      substElts.push_back(origElt.getWithType(substEltType));
    }
    return CanType(TupleType::get(substElts, getASTContext()));
  }
  // Block storage types need to substitute their capture type by these same
  // rules.
  CanType visitSILBlockStorageType(CanSILBlockStorageType origType) {
    auto substCaptureType = visit(origType->getCaptureType());
    return SILBlockStorageType::get(substCaptureType);
  }

  /// Optionals need to have their object types substituted by these rules.
  CanType visitBoundGenericEnumType(CanBoundGenericEnumType origType) {
    // Only use a special rule if it's Optional.
    if (!origType->getDecl()->isOptionalDecl()) {
      return visitType(origType);
    }

    CanType origObjectType = origType.getGenericArgs()[0];
    CanType substObjectType = visit(origObjectType);
    return CanType(BoundGenericType::get(origType->getDecl(), Type(),
                                         substObjectType));
  }

  /// Any other type would be a valid type in the AST. Just apply the
  /// substitution on the AST level and then lower that.
  CanType visitType(CanType origType) {
    assert(!isa<AnyFunctionType>(origType));
    assert(!isa<LValueType>(origType) && !isa<InOutType>(origType));

    SubstOptions substOptions(None);
    if (shouldSubstituteOpaqueArchetypes)
      substOptions = SubstFlags::SubstituteOpaqueArchetypes |
                     SubstFlags::AllowLoweredTypes;
    auto substType =
        origType.subst(Subst, Conformances, substOptions)->getCanonicalType();

    // If the substitution didn't change anything, we know that the
    // original type was a lowered type, so we're good.
    if (origType == substType) {
      return origType;
    }

    AbstractionPattern abstraction(Sig, origType);
    return TheSILModule.Types.getLoweredRValueType(abstraction, substType);
  }
};

} // end anonymous namespace

SILType SILType::subst(SILModule &silModule, TypeSubstitutionFn subs,
                       LookupConformanceFn conformances,
                       CanGenericSignature genericSig,
                       bool shouldSubstituteOpaqueArchetypes) const {
  if (!hasArchetype() && !hasTypeParameter() &&
      (!shouldSubstituteOpaqueArchetypes ||
       !getASTType()->hasOpaqueArchetype()))
    return *this;

  if (!genericSig)
    genericSig = silModule.Types.getCurGenericContext();
  SILTypeSubstituter STST(silModule, subs, conformances,
                          genericSig, shouldSubstituteOpaqueArchetypes);
  return STST.subst(*this);
}

SILType SILType::subst(SILModule &silModule, SubstitutionMap subs) const{
  return subst(silModule, QuerySubstitutionMap{subs},
               LookUpConformanceInSubstitutionMap(subs));
}

/// Apply a substitution to this polymorphic SILFunctionType so that
/// it has the form of the normal SILFunctionType for the substituted
/// type, except using the original conventions.
CanSILFunctionType
SILFunctionType::substGenericArgs(SILModule &silModule,
                                  SubstitutionMap subs) {
  if (!isPolymorphic()) {
    return CanSILFunctionType(this);
  }
  
  if (subs.empty()) {
    return CanSILFunctionType(this);
  }

  return substGenericArgs(silModule,
                          QuerySubstitutionMap{subs},
                          LookUpConformanceInSubstitutionMap(subs));
}

CanSILFunctionType
SILFunctionType::substGenericArgs(SILModule &silModule,
                                  TypeSubstitutionFn subs,
                                  LookupConformanceFn conformances) {
  if (!isPolymorphic()) return CanSILFunctionType(this);
  SILTypeSubstituter substituter(silModule, subs, conformances,
                                 getGenericSignature(),
                                 /*shouldSubstituteOpaqueTypes*/ false);
  return substituter.substSILFunctionType(CanSILFunctionType(this));
}

/// Fast path for bridging types in a function type without uncurrying.
CanAnyFunctionType
TypeConverter::getBridgedFunctionType(AbstractionPattern pattern,
                                      CanAnyFunctionType t,
                                      AnyFunctionType::ExtInfo extInfo,
                                      Bridgeability bridging) {
  // Pull out the generic signature.
  CanGenericSignature genericSig = t.getOptGenericSignature();

  switch (auto rep = t->getExtInfo().getSILRepresentation()) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::WitnessMethod: {
    // No bridging needed for native functions.
    if (t->getExtInfo() == extInfo)
      return t;
    return CanAnyFunctionType::get(genericSig, t.getParams(), t.getResult(),
                                   extInfo);
  }

  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::ObjCMethod: {
    SmallVector<AnyFunctionType::Param, 8> params;
    getBridgedParams(rep, pattern, t->getParams(), params, bridging);

    bool suppressOptional = pattern.hasForeignErrorStrippingResultOptionality();
    auto result = getBridgedResultType(rep,
                                       pattern.getFunctionResultType(),
                                       t.getResult(),
                                       bridging,
                                       suppressOptional);

    return CanAnyFunctionType::get(genericSig, llvm::makeArrayRef(params),
                                   result, extInfo);
  }
  }
  llvm_unreachable("bad calling convention");
}

static AbstractFunctionDecl *getBridgedFunction(SILDeclRef declRef) {
  switch (declRef.kind) {
  case SILDeclRef::Kind::Func:
  case SILDeclRef::Kind::Allocator:
  case SILDeclRef::Kind::Initializer:
    return (declRef.hasDecl()
            ? cast<AbstractFunctionDecl>(declRef.getDecl())
            : nullptr);

  case SILDeclRef::Kind::EnumElement:
  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::Deallocator:
  case SILDeclRef::Kind::GlobalAccessor:
  case SILDeclRef::Kind::DefaultArgGenerator:
  case SILDeclRef::Kind::StoredPropertyInitializer:
  case SILDeclRef::Kind::IVarInitializer:
  case SILDeclRef::Kind::IVarDestroyer:
    return nullptr;
  }
  llvm_unreachable("bad SILDeclRef kind");
}

static AbstractionPattern
getAbstractionPatternForConstant(ASTContext &ctx, SILDeclRef constant,
                                 CanAnyFunctionType fnType,
                                 unsigned numParameterLists) {
  if (!constant.isForeign)
    return AbstractionPattern(fnType);

  auto bridgedFn = getBridgedFunction(constant);
  if (!bridgedFn)
    return AbstractionPattern(fnType);
  const clang::Decl *clangDecl = bridgedFn->getClangDecl();
  if (!clangDecl)
    return AbstractionPattern(fnType);

  // Don't implicitly turn non-optional results to optional if
  // we're going to apply a foreign error convention that checks
  // for nil results.
  if (auto method = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
    assert(numParameterLists == 2 && "getting curried ObjC method type?");
    auto foreignError = bridgedFn->getForeignErrorConvention();
    return AbstractionPattern::getCurriedObjCMethod(fnType, method,
                                                    foreignError);
  } else if (auto value = dyn_cast<clang::ValueDecl>(clangDecl)) {
    if (numParameterLists == 1) {
      // C function imported as a function.
      return AbstractionPattern(fnType, value->getType().getTypePtr());
    } else {
      assert(numParameterLists == 2);
      if (auto method = dyn_cast<clang::CXXMethodDecl>(clangDecl)) {
        // C++ method.
        return AbstractionPattern::getCurriedCXXMethod(fnType, bridgedFn);
      } else {
        // C function imported as a method.
        return AbstractionPattern::getCurriedCFunctionAsMethod(fnType,
                                                               bridgedFn);
      }
    }
  }

  return AbstractionPattern(fnType);
}

TypeConverter::LoweredFormalTypes
TypeConverter::getLoweredFormalTypes(SILDeclRef constant,
                                     CanAnyFunctionType fnType) {
  // We always use full bridging when importing a constant because we can
  // directly bridge its arguments and results when calling it.
  auto bridging = Bridgeability::Full;

  unsigned numParameterLists = constant.getParameterListCount();
  auto extInfo = fnType->getExtInfo();

  // Form an abstraction pattern for bridging purposes.
  AbstractionPattern bridgingFnPattern =
    getAbstractionPatternForConstant(Context, constant, fnType,
                                     numParameterLists);

  // Fast path: no uncurrying required.
  if (numParameterLists == 1) {
    auto bridgedFnType =
      getBridgedFunctionType(bridgingFnPattern, fnType, extInfo, bridging);
    bridgingFnPattern.rewriteType(bridgingFnPattern.getGenericSignature(),
                                  bridgedFnType);
    return { bridgingFnPattern, bridgedFnType };
  }

  SILFunctionTypeRepresentation rep = extInfo.getSILRepresentation();
  assert(rep != SILFunctionType::Representation::Block
         && "objc blocks cannot be curried");

  // The dependent generic signature.
  CanGenericSignature genericSig = fnType.getOptGenericSignature();

  // The 'self' parameter.
  assert(fnType.getParams().size() == 1);
  AnyFunctionType::Param selfParam = fnType.getParams()[0];

  // The formal method parameters.
  // If we actually partially-apply this, assume we'll need a thick function.
  fnType = cast<FunctionType>(fnType.getResult());
  auto innerExtInfo =
    fnType->getExtInfo().withRepresentation(FunctionTypeRepresentation::Swift);
  auto methodParams = fnType->getParams();

  auto resultType = fnType.getResult();
  bool suppressOptionalResult =
    bridgingFnPattern.hasForeignErrorStrippingResultOptionality();

  // Bridge input and result types.
  SmallVector<AnyFunctionType::Param, 8> bridgedParams;
  CanType bridgedResultType;

  switch (rep) {
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::WitnessMethod:
    // Native functions don't need bridging.
    bridgedParams.append(methodParams.begin(), methodParams.end());
    bridgedResultType = resultType;
    break;

  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::CFunctionPointer: {
    if (rep == SILFunctionTypeRepresentation::ObjCMethod) {
      // The "self" parameter should not get bridged unless it's a metatype.
      if (selfParam.getPlainType()->is<AnyMetatypeType>()) {
        auto selfPattern = bridgingFnPattern.getFunctionParamType(0);
        selfParam = getBridgedParam(rep, selfPattern, selfParam, bridging);
      }
    }

    auto partialFnPattern = bridgingFnPattern.getFunctionResultType();
    getBridgedParams(rep, partialFnPattern, methodParams, bridgedParams,
                     bridging);

    bridgedResultType =
      getBridgedResultType(rep,
                           partialFnPattern.getFunctionResultType(),
                           resultType, bridging, suppressOptionalResult);
    break;
  }

  case SILFunctionTypeRepresentation::Block:
    llvm_unreachable("Cannot uncurry native representation");
  }

  // Build the curried function type.
  auto inner =
    CanFunctionType::get(llvm::makeArrayRef(bridgedParams),
                         bridgedResultType, innerExtInfo);

  auto curried =
    CanAnyFunctionType::get(genericSig, {selfParam}, inner, extInfo);

  // Replace the type in the abstraction pattern with the curried type.
  bridgingFnPattern.rewriteType(genericSig, curried);

  // Build the uncurried function type.
  if (innerExtInfo.throws())
    extInfo = extInfo.withThrows(true);

  bridgedParams.push_back(selfParam);

  auto uncurried =
    CanAnyFunctionType::get(genericSig,
                            llvm::makeArrayRef(bridgedParams),
                            bridgedResultType,
                            extInfo);

  return { bridgingFnPattern, uncurried };
}

// TODO: We should compare generic signatures. Class and witness methods
// allow variance in "self"-fulfilled parameters; other functions must
// match exactly.
// TODO: More sophisticated param and return ABI compatibility rules could
// diverge.
static bool areABICompatibleParamsOrReturns(SILType a, SILType b,
                                            SILFunction *inFunction) {
  // Address parameters are all ABI-compatible, though the referenced
  // values may not be. Assume whoever's doing this knows what they're
  // doing.
  if (a.isAddress() && b.isAddress())
    return true;

  // Addresses aren't compatible with values.
  // TODO: An exception for pointerish types?
  if (a.isAddress() || b.isAddress())
    return false;

  // Tuples are ABI compatible if their elements are.
  // TODO: Should destructure recursively.
  SmallVector<CanType, 1> aElements, bElements;
  if (auto tup = a.getAs<TupleType>()) {
    auto types = tup.getElementTypes();
    aElements.append(types.begin(), types.end());
  } else {
    aElements.push_back(a.getASTType());
  }
  if (auto tup = b.getAs<TupleType>()) {
    auto types = tup.getElementTypes();
    bElements.append(types.begin(), types.end());
  } else {
    bElements.push_back(b.getASTType());
  }

  if (aElements.size() != bElements.size())
    return false;

  for (unsigned i : indices(aElements)) {
    auto aa = SILType::getPrimitiveObjectType(aElements[i]);
    auto bb = SILType::getPrimitiveObjectType(bElements[i]);
    // Equivalent types are always ABI-compatible.
    if (aa == bb)
      continue;

    // Opaque types are compatible with their substitution.
    if (inFunction) {
      auto opaqueTypesSubsituted = aa;
      ReplaceOpaqueTypesWithUnderlyingTypes replacer(
          inFunction->getModule().getSwiftModule(),
          inFunction->getResilienceExpansion());
      if (aa.getASTType()->hasOpaqueArchetype())
        opaqueTypesSubsituted = aa.subst(inFunction->getModule(), replacer,
                                         replacer, CanGenericSignature(), true);

      auto opaqueTypesSubsituted2 = bb;
      if (bb.getASTType()->hasOpaqueArchetype())
        opaqueTypesSubsituted2 =
            bb.subst(inFunction->getModule(), replacer, replacer,
                     CanGenericSignature(), true);
      if (opaqueTypesSubsituted == opaqueTypesSubsituted2)
        continue;
    }

    // FIXME: If one or both types are dependent, we can't accurately assess
    // whether they're ABI-compatible without a generic context. We can
    // do a better job here when dependent types are related to their
    // generic signatures.
    if (aa.hasTypeParameter() || bb.hasTypeParameter())
      continue;

    // Bridgeable object types are interchangeable.
    if (aa.isBridgeableObjectType() && bb.isBridgeableObjectType())
      continue;

    // Optional and IUO are interchangeable if their elements are.
    auto aObject = aa.getOptionalObjectType();
    auto bObject = bb.getOptionalObjectType();
    if (aObject && bObject &&
        areABICompatibleParamsOrReturns(aObject, bObject, inFunction))
      continue;
    // Optional objects are ABI-interchangeable with non-optionals;
    // None is represented by a null pointer.
    if (aObject && aObject.isBridgeableObjectType() &&
        bb.isBridgeableObjectType())
      continue;
    if (bObject && bObject.isBridgeableObjectType() &&
        aa.isBridgeableObjectType())
      continue;

    // Optional thick metatypes are ABI-interchangeable with non-optionals
    // too.
    if (aObject)
      if (auto aObjMeta = aObject.getAs<MetatypeType>())
        if (auto bMeta = bb.getAs<MetatypeType>())
          if (aObjMeta->getRepresentation() == bMeta->getRepresentation() &&
              bMeta->getRepresentation() != MetatypeRepresentation::Thin)
            continue;
    if (bObject)
      if (auto aMeta = aa.getAs<MetatypeType>())
        if (auto bObjMeta = bObject.getAs<MetatypeType>())
          if (aMeta->getRepresentation() == bObjMeta->getRepresentation() &&
              aMeta->getRepresentation() != MetatypeRepresentation::Thin)
            continue;

    // Function types are interchangeable if they're also ABI-compatible.
    if (auto aFunc = aa.getAs<SILFunctionType>()) {
      if (auto bFunc = bb.getAs<SILFunctionType>()) {
        // *NOTE* We swallow the specific error here for now. We will still get
        // that the function types are incompatible though, just not more
        // specific information.
        return aFunc->isABICompatibleWith(bFunc, inFunction).isCompatible();
      }
    }

    // Metatypes are interchangeable with metatypes with the same
    // representation.
    if (auto aMeta = aa.getAs<MetatypeType>()) {
      if (auto bMeta = bb.getAs<MetatypeType>()) {
        if (aMeta->getRepresentation() == bMeta->getRepresentation())
          continue;
      }
    }
    // Other types must match exactly.
    return false;
  }

  return true;
}

namespace {
using ABICompatibilityCheckResult =
    SILFunctionType::ABICompatibilityCheckResult;
} // end anonymous namespace

ABICompatibilityCheckResult
SILFunctionType::isABICompatibleWith(CanSILFunctionType other,
                                     SILFunction *context) const {
  // The calling convention and function representation can't be changed.
  if (getRepresentation() != other->getRepresentation())
    return ABICompatibilityCheckResult::DifferentFunctionRepresentations;

  // Check the results.
  if (getNumResults() != other->getNumResults())
    return ABICompatibilityCheckResult::DifferentNumberOfResults;

  for (unsigned i : indices(getResults())) {
    auto result1 = getResults()[i];
    auto result2 = other->getResults()[i];

    if (result1.getConvention() != result2.getConvention())
      return ABICompatibilityCheckResult::DifferentReturnValueConventions;

    if (!areABICompatibleParamsOrReturns(result1.getSILStorageType(),
                                         result2.getSILStorageType(),
                                         context)) {
      return ABICompatibilityCheckResult::ABIIncompatibleReturnValues;
    }
  }

  // Our error result conventions are designed to be ABI compatible
  // with functions lacking error results.  Just make sure that the
  // actual conventions match up.
  if (hasErrorResult() && other->hasErrorResult()) {
    auto error1 = getErrorResult();
    auto error2 = other->getErrorResult();
    if (error1.getConvention() != error2.getConvention())
      return ABICompatibilityCheckResult::DifferentErrorResultConventions;

    if (!areABICompatibleParamsOrReturns(error1.getSILStorageType(),
                                         error2.getSILStorageType(), context))
      return ABICompatibilityCheckResult::ABIIncompatibleErrorResults;
  }

  // Check the parameters.
  // TODO: Could allow known-empty types to be inserted or removed, but SIL
  // doesn't know what empty types are yet.
  if (getParameters().size() != other->getParameters().size())
    return ABICompatibilityCheckResult::DifferentNumberOfParameters;

  for (unsigned i : indices(getParameters())) {
    auto param1 = getParameters()[i];
    auto param2 = other->getParameters()[i];

    if (param1.getConvention() != param2.getConvention())
      return {ABICompatibilityCheckResult::DifferingParameterConvention, i};
    if (!areABICompatibleParamsOrReturns(param1.getSILStorageType(),
                                         param2.getSILStorageType(), context))
      return {ABICompatibilityCheckResult::ABIIncompatibleParameterType, i};
  }

  // This needs to be checked last because the result implies everying else has
  // already been checked and this is the only difference.
  if (isNoEscape() != other->isNoEscape() &&
      (getRepresentation() == SILFunctionType::Representation::Thick))
    return ABICompatibilityCheckResult::ABIEscapeToNoEscapeConversion;

  return ABICompatibilityCheckResult::None;
}

StringRef SILFunctionType::ABICompatibilityCheckResult::getMessage() const {
  switch (kind) {
  case innerty::None:
    return "None";
  case innerty::DifferentFunctionRepresentations:
    return "Different function representations";
  case innerty::DifferentNumberOfResults:
    return "Different number of results";
  case innerty::DifferentReturnValueConventions:
    return "Different return value conventions";
  case innerty::ABIIncompatibleReturnValues:
    return "ABI incompatible return values";
  case innerty::DifferentErrorResultConventions:
    return "Different error result conventions";
  case innerty::ABIIncompatibleErrorResults:
    return "ABI incompatible error results";
  case innerty::DifferentNumberOfParameters:
    return "Different number of parameters";

  // These two have to do with specific parameters, so keep the error message
  // non-plural.
  case innerty::DifferingParameterConvention:
    return "Differing parameter convention";
  case innerty::ABIIncompatibleParameterType:
    return "ABI incompatible parameter type.";
  case innerty::ABIEscapeToNoEscapeConversion:
    return "Escape to no escape conversion";
  }
  llvm_unreachable("Covered switch isn't completely covered?!");
}
