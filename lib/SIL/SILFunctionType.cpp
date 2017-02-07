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
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILModule.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/Analysis/DomainSpecific/CocoaConventions.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/CharInfo.h"
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

ProtocolDecl *
SILFunctionType::getDefaultWitnessMethodProtocol(ModuleDecl &M) const {
  assert(getRepresentation() == SILFunctionTypeRepresentation::WitnessMethod);
  auto selfTy = getSelfInstanceType();
  if (auto paramTy = dyn_cast<GenericTypeParamType>(selfTy)) {
    assert(paramTy->getDepth() == 0 && paramTy->getIndex() == 0);
    auto protos = GenericSig->getConformsTo(paramTy, M);
    assert(protos.size() == 1);
    return protos[0];
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
      mod->lookupQualified(ModuleType::get(mod), C.getIdentifier(typeName),
                           NL_QualifiedDefault | NL_KnownNonCascadingDependency,
                           /*typeResolver=*/nullptr, decls);
      if (decls.size() != 1)
        return CanType();

      const TypeDecl *typeDecl = dyn_cast<TypeDecl>(decls.front());
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
    DEBUG(llvm::dbgs() << "Bridging type " << moduleName << '.' << typeName
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
CanAnyFunctionType Lowering::adjustFunctionType(CanAnyFunctionType t,
                                          AnyFunctionType::ExtInfo extInfo) {
  if (t->getExtInfo() == extInfo)
    return t;
  return CanAnyFunctionType(t->withExtInfo(extInfo));
}

/// Adjust a function type to have a slightly different type.
CanSILFunctionType Lowering::adjustFunctionType(CanSILFunctionType type,
                                               SILFunctionType::ExtInfo extInfo,
                                               ParameterConvention callee) {
  if (type->getExtInfo() == extInfo &&
      type->getCalleeConvention() == callee)
    return type;

  return SILFunctionType::get(type->getGenericSignature(), extInfo, callee,
                              type->getParameters(), type->getResults(),
                              type->getOptionalErrorResult(),
                              type->getASTContext());
}

namespace {

enum class ConventionsKind : uint8_t {
  Default = 0,
  DefaultBlock = 1,
  ObjCMethod = 2,
  CFunctionType = 3,
  CFunction = 4,
  SelectorFamily = 5,
  Deallocator = 6,
  Capture = 7,
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
                         const AbstractionPattern &type) const = 0;
    virtual ParameterConvention
    getDirectParameter(unsigned index,
                       const AbstractionPattern &type) const = 0;
    virtual ParameterConvention getCallee() const = 0;
    virtual ResultConvention getResult(const TypeLowering &resultTL) const = 0;
    virtual ParameterConvention
    getIndirectSelfParameter(const AbstractionPattern &type) const = 0;
    virtual ParameterConvention
    getDirectSelfParameter(const AbstractionPattern &type) const = 0;
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

      auto &substResultTL = M.Types.getTypeLowering(origType, substType);

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

      SILResultInfo result(substResultTL.getLoweredType().getSwiftRValueType(),
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
           && !origType.isConcreteType(*M.getSwiftModule())
           && !origType.requiresClass(*M.getSwiftModule()))
          || substTL.isAddressOnly()) {
        return true;

      // If the substitution didn't change the type, then a negative
      // response to the above is determinative as well.
      } else if (origType.getType() == substType &&
                 !origType.getType()->hasTypeParameter()) {
        return false;

      // Otherwise, query specifically for the original type.
      } else {
        // FIXME: Get expansion from SILDeclRef
        return SILType::isFormallyReturnedIndirectly(
            origType.getType(), M, origType.getGenericSignature(),
            ResilienceExpansion::Minimal);
      }
    }
  };

  /// A visitor for turning formal input types into SILParameterInfos,
  /// matching the abstraction patterns of the original type.
  ///
  /// If the original abstraction pattern is fully opaque, we must
  /// pass the function's inputs as if the original type were the most
  /// general function signature (expressed entirely in type
  /// variables) which can be substituted to equal the given
  /// signature.
  ///
  /// The goal of the most general type is to be (1) unambiguous to
  /// compute from the substituted type and (2) the same for every
  /// possible generalization of that type.  For example, suppose we
  /// have a Vector<(Int,Int)->Bool>.  Obviously, we would prefer to
  /// store optimal function pointers directly in this array; and if
  /// all uses of it are ungeneralized, we'd get away with that.  But
  /// suppose the vector is passed to a function like this:
  ///   func satisfiesAll<T>(v : Vector<(T,T)->Bool>, x : T, y : T) -> Bool
  /// That function will expect to be able to pull values out with the
  /// proper abstraction.  The only type we can possibly expect to agree
  /// upon is the most general form.
  ///
  /// The precise way this works is that Vector's subscript operation
  /// (assuming that's how it's being accessed) has this signature:
  ///   <X> Vector<X> -> Int -> X
  /// which 'satisfiesAll' is calling with this substitution:
  ///   X := (T, T) -> Bool
  /// Since 'satisfiesAll' has a function type substituting for an
  /// unrestricted archetype, it expects the value returned to have the
  /// most general possible form 'A -> B', which it will need to
  /// de-generalize (by thunking) if it needs to pass it around as
  /// a '(T, T) -> Bool' value.
  ///
  /// It is only this sort of direct substitution in types that forces
  /// the most general possible type to be selected; declarations will
  /// generally provide a target generalization level.  For example,
  /// in a Vector<IntPredicate>, where IntPredicate is a struct (not a
  /// tuple) with one field of type (Int, Int) -> Bool, all the
  /// function pointers will be stored ungeneralized.  Of course, such
  /// a vector couldn't be passed to 'satisfiesAll'.
  ///
  /// For most types, the most general type is simply a fresh,
  /// unrestricted type variable.  But unmaterializable types are not
  /// valid results of substitutions, so this does not apply.  The
  /// most general form of an unmaterializable type preserves the
  /// basic structure of the unmaterializable components, replacing
  /// any materializable components with fresh type variables.
  ///
  /// That is, if we have a substituted function type:
  ///   (UnicodeScalar, (Int, Float), Double) -> Bool
  /// then its most general form is
  ///   A -> B
  ///
  /// because there is a valid substitution
  ///   A := (UnicodeScalar, (Int, Float), Double)
  ///   B := Bool
  ///
  /// But if we have a substituted function type:
  ///   (UnicodeScalar, (Int, Float), inout Double) -> Bool
  /// then its most general form is
  ///   (A, B, inout C) -> D
  /// because the substitution
  ///   X := (UnicodeScalar, (Int, Float), inout Double)
  /// is invalid substitution, ultimately because 'inout Double'
  /// is not materializable.
  class DestructureInputs {
    SILModule &M;
    const Conventions &Convs;
    const Optional<ForeignErrorConvention> &ForeignError;
    SmallVectorImpl<SILParameterInfo> &Inputs;
    unsigned NextOrigParamIndex = 0;
  public:
    DestructureInputs(SILModule &M, const Conventions &conventions,
                      const Optional<ForeignErrorConvention> &foreignError,
                      SmallVectorImpl<SILParameterInfo> &inputs)
      : M(M), Convs(conventions), ForeignError(foreignError), Inputs(inputs) {}

    void destructure(AbstractionPattern origType, CanType substType,
                     AnyFunctionType::ExtInfo extInfo) {
      visitTopLevelType(origType, substType, extInfo);
      maybeAddForeignErrorParameter();
    }

  private:
    bool isClangTypeMoreIndirectThanSubstType(const clang::Type *clangTy,
                                              CanType substTy) {
      // A const pointer argument might have been imported as
      // UnsafePointer, COpaquePointer, or a CF foreign class.
      // (An ObjC class type wouldn't be const-qualified.)
      if (clangTy->isPointerType()
          && clangTy->getPointeeType().isConstQualified()) {
        // Peek through optionals.
        if (auto substObjTy = substTy.getAnyOptionalObjectType())
          substTy = substObjTy;

        // Void pointers aren't usefully indirectable.
        if (clangTy->isVoidPointerType())
          return false;
        
        if (auto eltTy = substTy->getAnyPointerElementType())
          return isClangTypeMoreIndirectThanSubstType(
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
          // swift_newtype-ed CF type as foreign class
          if (auto typedefTy = clangTy->getAs<clang::TypedefType>()) {
            if (typedefTy->getDecl()->getAttr<clang::SwiftNewtypeAttr>()) {
              // Make sure that we actually made the struct during import
              if (auto underlyingType =
                      substTy->getSwiftNewtypeUnderlyingType()) {
                if (auto underlyingClass =
                        underlyingType->getClassOrBoundGenericClass()) {
                  if (underlyingClass->getForeignClassKind() ==
                          ClassDecl::ForeignKind::CFType) {
                    return false;
                  }
                }
              }
            }
          }
        }

        return true;
      }
      return false;
    }
  
    /// Query whether the original type is address-only given complete
    /// lowering information about its substitution.
    bool isFormallyPassedIndirectly(AbstractionPattern origType,
                                    CanType substType,
                                    const TypeLowering &substTL) {
      auto &mod = *M.getSwiftModule();

      // If the C type of the argument is a const pointer, but the Swift type
      // isn't, treat it as indirect.
      if (origType.isClangType()
          && isClangTypeMoreIndirectThanSubstType(origType.getClangType(),
                                                  substType)) {
        return true;
      }

      // If the substituted type is passed indirectly, so must the
      // unsubstituted type.
      if ((origType.isTypeParameter() && !origType.isConcreteType(mod)
           && !origType.requiresClass(mod))
          || substTL.isAddressOnly()) {
        return true;

      // If the substitution didn't change the type, then a negative
      // response to the above is determinative as well.
      } else if (origType.getType() == substType &&
                 !origType.getType()->hasTypeParameter()) {
        return false;

      // Otherwise, query specifically for the original type.
      } else {
        // FIXME: Get expansion from SILDeclRef
        return SILType::isFormallyPassedIndirectly(
            origType.getType(), M, origType.getGenericSignature(),
            ResilienceExpansion::Minimal);
      }
    }

    void visitSelfType(AbstractionPattern origType, CanType substType,
                       SILFunctionTypeRepresentation rep) {
      auto &substTL =
        M.Types.getTypeLowering(origType, substType);
      ParameterConvention convention;
      if (origType.getAs<InOutType>()) {
        convention = ParameterConvention::Indirect_Inout;
      } else if (isFormallyPassedIndirectly(origType, substType, substTL)) {
        if (rep == SILFunctionTypeRepresentation::WitnessMethod)
          convention = ParameterConvention::Indirect_In_Guaranteed;
        else
          convention = Convs.getIndirectSelfParameter(origType);
        assert(isIndirectFormalParameter(convention));

      } else if (substTL.isTrivial()) {
        convention = ParameterConvention::Direct_Unowned;
      } else {
        convention = Convs.getDirectSelfParameter(origType);
        assert(!isIndirectFormalParameter(convention));
      }

      maybeAddForeignErrorParameter();

      auto loweredType = substTL.getLoweredType().getSwiftRValueType();
      Inputs.push_back(SILParameterInfo(loweredType, convention));
    }

    /// This is a special entry point that allows destructure inputs to handle
    /// self correctly.
    void visitTopLevelType(AbstractionPattern origType, CanType substType,
                           AnyFunctionType::ExtInfo extInfo) {
      // If we don't have 'self', we don't need to do anything special.
      if (!extInfo.hasSelfParam()) {
        return visit(origType, substType);
      }

      // Okay, handle 'self'.
      if (CanTupleType substTupleType = dyn_cast<TupleType>(substType)) {
        unsigned numEltTypes = substTupleType.getElementTypes().size();
        assert(numEltTypes > 0);

        // Process all the non-self parameters.
        unsigned numNonSelfParams = numEltTypes - 1;
        for (unsigned i = 0; i != numNonSelfParams; ++i) {
          visit(origType.getTupleElementType(i),
                substTupleType.getElementType(i));
        }

        // Process the self parameter.
        visitSelfType(origType.getTupleElementType(numNonSelfParams),
                      substTupleType.getElementType(numNonSelfParams),
                      extInfo.getSILRepresentation());
      } else {
        visitSelfType(origType, substType,
                      extInfo.getSILRepresentation());
      }
    }

    void visit(AbstractionPattern origType, CanType substType) {
      // Expand tuples.  But if the abstraction pattern is opaque, and
      // the tuple type is materializable -- if it doesn't contain an
      // l-value type -- then it's a valid target for substitution and
      // we should not expand it.
      if (isa<TupleType>(substType) &&
          (!origType.isTypeParameter() ||
           !substType->isMaterializable())) {
        auto substTuple = cast<TupleType>(substType);
        assert(origType.isTypeParameter() ||
               origType.getNumTupleElements() == substTuple->getNumElements());
        for (auto i : indices(substTuple.getElementTypes())) {
          visit(origType.getTupleElementType(i),
                substTuple.getElementType(i));
        }
        return;
      }

      maybeAddForeignErrorParameter();

      unsigned origParamIndex = NextOrigParamIndex++;

      auto &substTL = M.Types.getTypeLowering(origType, substType);
      ParameterConvention convention;
      if (isa<InOutType>(substType)) {
        assert(origType.isTypeParameter() || origType.getAs<InOutType>());
        convention = ParameterConvention::Indirect_Inout;
      } else if (isFormallyPassedIndirectly(origType, substType, substTL)) {
        convention = Convs.getIndirectParameter(origParamIndex, origType);
        assert(isIndirectFormalParameter(convention));
      } else if (substTL.isTrivial()) {
        convention = ParameterConvention::Direct_Unowned;
      } else {
        convention = Convs.getDirectParameter(origParamIndex, origType);
        assert(!isIndirectFormalParameter(convention));
      }
      auto loweredType = substTL.getLoweredType().getSwiftRValueType();
      
      Inputs.push_back(SILParameterInfo(loweredType, convention));
    }

    void maybeAddForeignErrorParameter() {
      if (!ForeignError ||
          NextOrigParamIndex != ForeignError->getErrorParameterIndex())
        return;

      auto foreignErrorTy =
        M.Types.getLoweredType(ForeignError->getErrorParameterType());

      // Assume the error parameter doesn't have interesting lowering.
      Inputs.push_back(SILParameterInfo(foreignErrorTy.getSwiftRValueType(),
                                        ParameterConvention::Direct_Unowned));
      NextOrigParamIndex++;
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

  auto classDecl = dc->getAsClassOrClassExtensionContext();
  return (classDecl && classDecl->usesObjCGenericsModel());
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
static CanSILFunctionType getSILFunctionType(SILModule &M,
                        AbstractionPattern origType,
                        CanAnyFunctionType substFnInterfaceType,
                        AnyFunctionType::ExtInfo extInfo,
                        const Conventions &conventions,
                        const Optional<ForeignErrorConvention> &foreignError,
                        Optional<SILDeclRef> constant) {
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
  CanGenericSignature genericSig = nullptr;
  if (auto genFnType = dyn_cast<GenericFunctionType>(substFnInterfaceType)) {
    genericSig = genFnType.getGenericSignature();
  }

  // Lower the interface type in a generic context.
  GenericContextScope scope(M.Types, genericSig);

  // Map 'throws' to the appropriate error convention.
  Optional<SILResultInfo> errorResult;
  assert((!foreignError || substFnInterfaceType->getExtInfo().throws()) &&
         "foreignError was set but function type does not throw?");
  if (substFnInterfaceType->getExtInfo().throws() && !foreignError) {
    assert(!origType.isForeign() &&
           "using native Swift error convention for foreign type!");
    SILType exnType = SILType::getExceptionType(M.getASTContext());
    assert(exnType.isObject());
    errorResult = SILResultInfo(exnType.getSwiftRValueType(),
                                ResultConvention::Owned);
  }

  // Lower the result type.

  AbstractionPattern origResultType = origType.getFunctionResultType();
  CanType substFormalResultType = substFnInterfaceType.getResult();

  // If we have a foreign error convention, restore the original result type.
  if (foreignError) {
    switch (foreignError->getKind()) {
    // These conventions replace the result type.
    case ForeignErrorConvention::ZeroResult:
    case ForeignErrorConvention::NonZeroResult:
      assert(substFormalResultType->isVoid());
      substFormalResultType = foreignError->getResultType();
      origResultType = AbstractionPattern(genericSig, substFormalResultType);
      break;

    // These conventions wrap the result type in a level of optionality.
    case ForeignErrorConvention::NilResult:
      assert(!substFormalResultType->getAnyOptionalObjectType());
      substFormalResultType =
        OptionalType::get(substFormalResultType)->getCanonicalType();
      origResultType =
        AbstractionPattern::getOptional(origResultType, OTK_Optional);
      break;

    // These conventions don't require changes to the formal error type.
    case ForeignErrorConvention::ZeroPreservedResult:
    case ForeignErrorConvention::NonNilError:
      break;
    }
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
    DestructureInputs destructurer(M, conventions, foreignError, inputs);
    destructurer.destructure(origType.getFunctionInputType(),
                             substFnInterfaceType.getInput(),
                             extInfo);
  }
  
  // Lower the capture context parameters, if any.
  // But note that default arg generators can't capture anything right now,
  // and if we ever add that ability, it will be a different capture list
  // from the function to which the argument is attached.
  if (constant && !constant->isDefaultArgGenerator())
  if (auto function = constant->getAnyFunctionRef()) {
    // NB: The generic signature may be elided from the lowered function type
    // if the function is in a fully-specialized context, but we still need to
    // canonicalize references to the generic parameters that may appear in
    // non-canonical types in that context. We need the original generic
    // signature from the AST for that.
    auto origGenericSig
      = function->getGenericSignature();
    auto getCanonicalType = [origGenericSig, &M](Type t) -> CanType {
      if (origGenericSig)
        return origGenericSig->getCanonicalTypeInContext(t,
                                                         *M.getSwiftModule());
      return t->getCanonicalType();
    };

    auto &Types = M.Types;
    auto loweredCaptures = Types.getLoweredLocalCaptures(*function);
    
    for (auto capture : loweredCaptures.getCaptures()) {
      if (capture.isDynamicSelfMetadata()) {
        ParameterConvention convention = ParameterConvention::Direct_Unowned;
        auto selfMetatype = MetatypeType::get(
            loweredCaptures.getDynamicSelfType()->getSelfType(),
            MetatypeRepresentation::Thick);
        auto canSelfMetatype = getCanonicalType(selfMetatype);
        SILParameterInfo param(canSelfMetatype, convention);
        inputs.push_back(param);

        continue;
      }

      auto *VD = capture.getDecl();
      auto type = VD->getInterfaceType();
      auto canType = getCanonicalType(type);

      auto &loweredTL = Types.getTypeLowering(
                              AbstractionPattern(genericSig, canType), canType);
      auto loweredTy = loweredTL.getLoweredType();
      switch (Types.getDeclCaptureKind(capture)) {
      case CaptureKind::None:
        break;
      case CaptureKind::Constant: {
        // Constants are captured by value.
        ParameterConvention convention;
        if (loweredTL.isAddressOnly()) {
          convention = M.getOptions().EnableGuaranteedClosureContexts
            ? ParameterConvention::Indirect_In_Guaranteed
            : ParameterConvention::Indirect_In;
        } else if (loweredTL.isTrivial()) {
          convention = ParameterConvention::Direct_Unowned;
        } else {
          convention = M.getOptions().EnableGuaranteedClosureContexts
            ? ParameterConvention::Direct_Guaranteed
            : ParameterConvention::Direct_Owned;
        }
        SILParameterInfo param(loweredTy.getSwiftRValueType(), convention);
        inputs.push_back(param);
        break;
      }
      case CaptureKind::Box: {
        // Lvalues are captured as a box that owns the captured value.
        auto boxTy = Types.getInterfaceBoxTypeForCapture(VD,
                                                 loweredTy.getSwiftRValueType(),
                                                 /*mutable*/ true);
        auto convention = M.getOptions().EnableGuaranteedClosureContexts
          ? ParameterConvention::Direct_Guaranteed
          : ParameterConvention::Direct_Owned;
        auto param = SILParameterInfo(boxTy, convention);
        inputs.push_back(param);
        break;
      }
      case CaptureKind::StorageAddress: {
        // Non-escaping lvalues are captured as the address of the value.
        SILType ty = loweredTy.getAddressType();
        auto param = SILParameterInfo(ty.getSwiftRValueType(),
                                  ParameterConvention::Indirect_InoutAliasable);
        inputs.push_back(param);
        break;
      }
      }
    }
  }
  
  auto calleeConvention = ParameterConvention::Direct_Unowned;
  if (extInfo.hasContext())
    calleeConvention = conventions.getCallee();

  bool pseudogeneric = (constant ? isPseudogeneric(*constant) : false);

  // Always strip the auto-closure and no-escape bit.
  // TODO: The noescape bit could be of interest to SIL optimizations.
  //   We should bring it back when we have those optimizations.
  auto silExtInfo = SILFunctionType::ExtInfo()
    .withRepresentation(extInfo.getSILRepresentation())
    .withIsPseudogeneric(pseudogeneric);
  
  return SILFunctionType::get(genericSig,
                              silExtInfo, calleeConvention,
                              inputs, results, errorResult,
                              M.getASTContext());
}

//===----------------------------------------------------------------------===//
//                        Deallocator SILFunctionTypes
//===----------------------------------------------------------------------===//

namespace {

// The convention for general deallocators.
struct DeallocatorConventions : Conventions {
  DeallocatorConventions() : Conventions(ConventionsKind::Deallocator) {}

  ParameterConvention getIndirectParameter(unsigned index,
                             const AbstractionPattern &type) const override {
    llvm_unreachable("Deallocators do not have indirect parameters");
  }

  ParameterConvention getDirectParameter(unsigned index,
                             const AbstractionPattern &type) const override {
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
  /// The default Swift conventions.
  struct DefaultConventions : Conventions {

    DefaultConventions()
      : Conventions(ConventionsKind::Default) {}

    ParameterConvention getIndirectParameter(unsigned index,
                              const AbstractionPattern &type) const override {
      return ParameterConvention::Indirect_In;
    }

    ParameterConvention getDirectParameter(unsigned index,
                              const AbstractionPattern &type) const override {
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
  struct DefaultInitializerConventions : DefaultConventions {
    using DefaultConventions::DefaultConventions;
  
    /// Initializers must take 'self' at +1, since they will return it back
    /// at +1, and may chain onto Objective-C initializers that replace the
    /// instance.
    ParameterConvention
    getDirectSelfParameter(const AbstractionPattern &type) const override {
      return ParameterConvention::Direct_Owned;
    }
    
    ParameterConvention
    getIndirectSelfParameter(const AbstractionPattern &type) const override {
      return ParameterConvention::Indirect_In;
    }
  };

  /// The default conventions for ObjC blocks.
  struct DefaultBlockConventions : Conventions {
    DefaultBlockConventions() : Conventions(ConventionsKind::DefaultBlock) {}

    ParameterConvention getIndirectParameter(unsigned index,
                              const AbstractionPattern &type) const override {
      llvm_unreachable("indirect block parameters unsupported");
    }

    ParameterConvention getDirectParameter(unsigned index,
                              const AbstractionPattern &type) const override {
      return ParameterConvention::Direct_Unowned;
    }

    ParameterConvention getCallee() const override {
      return ParameterConvention::Direct_Unowned;
    }

    ResultConvention getResult(const TypeLowering &tl) const override {
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

static CanSILFunctionType getNativeSILFunctionType(SILModule &M,
                                         AbstractionPattern origType,
                                         CanAnyFunctionType substInterfaceType,
                                         AnyFunctionType::ExtInfo extInfo,
                                         Optional<SILDeclRef> constant,
                                         SILDeclRef::Kind kind) {
  switch (extInfo.getSILRepresentation()) {
  case SILFunctionType::Representation::Block:
  case SILFunctionType::Representation::CFunctionPointer:
    // TODO: Ought to support captures in block funcs.
    return getSILFunctionType(M, origType, substInterfaceType,
                              extInfo, DefaultBlockConventions(),
                              None, constant);

  case SILFunctionType::Representation::Thin:
  case SILFunctionType::Representation::ObjCMethod:
  case SILFunctionType::Representation::Thick:
  case SILFunctionType::Representation::Method:
  case SILFunctionType::Representation::Closure:
  case SILFunctionType::Representation::WitnessMethod: {
    switch (kind) {
    case SILDeclRef::Kind::Initializer:
      return getSILFunctionType(M, origType, substInterfaceType,
                                extInfo, DefaultInitializerConventions(),
                                None, constant);
    
    case SILDeclRef::Kind::Func:
    case SILDeclRef::Kind::Allocator:
    case SILDeclRef::Kind::Destroyer:
    case SILDeclRef::Kind::GlobalAccessor:
    case SILDeclRef::Kind::GlobalGetter:
    case SILDeclRef::Kind::DefaultArgGenerator:
    case SILDeclRef::Kind::StoredPropertyInitializer:
    case SILDeclRef::Kind::IVarInitializer:
    case SILDeclRef::Kind::IVarDestroyer:
    case SILDeclRef::Kind::EnumElement:
      return getSILFunctionType(M, origType, substInterfaceType,
                                extInfo, DefaultConventions(),
                                None, constant);
    case SILDeclRef::Kind::Deallocator:
      return getSILFunctionType(M, origType, substInterfaceType,
                                extInfo, DeallocatorConventions(), None,
                                constant);
    }
  }
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

CanSILFunctionType swift::getNativeSILFunctionType(SILModule &M,
                                       AbstractionPattern origType,
                                       CanAnyFunctionType substInterfaceType,
                                       Optional<SILDeclRef> constant,
                                       SILDeclRef::Kind kind) {
  AnyFunctionType::ExtInfo extInfo;

  // Preserve type information from the original type if possible.
  if (auto origFnType = origType.getAs<AnyFunctionType>()) {
    extInfo = origFnType->getExtInfo();

  // Otherwise, preserve function type attributes from the substituted type.
  } else {
    extInfo = substInterfaceType->getExtInfo();
  }
  return ::getNativeSILFunctionType(M, origType, substInterfaceType,
                                    extInfo, constant, kind);
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
                             const AbstractionPattern &type) const override {
      return getIndirectCParameterConvention(Method->param_begin()[index]);
    }

    ParameterConvention getDirectParameter(unsigned index,
                             const AbstractionPattern &type) const override {
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
                              const AbstractionPattern &type) const override {
      return getIndirectCParameterConvention(getParamType(index));
    }

    ParameterConvention getDirectParameter(unsigned index,
                              const AbstractionPattern &type) const override {
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
                              const AbstractionPattern &type) const override {
      if (auto param = TheDecl->getParamDecl(index))
        if (param->hasAttr<clang::CFConsumedAttr>())
          return ParameterConvention::Direct_Owned;
      return super::getDirectParameter(index, type);
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
} // end anonymous namespace

/// Given that we have an imported Clang declaration, deduce the
/// ownership conventions for calling it and build the SILFunctionType.
static CanSILFunctionType
getSILFunctionTypeForClangDecl(SILModule &M, const clang::Decl *clangDecl,
                               CanAnyFunctionType origType,
                               CanAnyFunctionType substInterfaceType,
                               AnyFunctionType::ExtInfo extInfo,
                         const Optional<ForeignErrorConvention> &foreignError,
                               Optional<SILDeclRef> constant) {
  if (auto method = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
    auto origPattern =
      AbstractionPattern::getObjCMethod(origType, method, foreignError);
    return getSILFunctionType(M, origPattern, substInterfaceType,
                              extInfo, ObjCMethodConventions(method),
                              foreignError, constant);
  }

  if (auto func = dyn_cast<clang::FunctionDecl>(clangDecl)) {
    AbstractionPattern origPattern(origType,
                                   func->getType().getTypePtr());
    return getSILFunctionType(M, origPattern, substInterfaceType,
                              extInfo, CFunctionConventions(func),
                              foreignError, constant);
  }

  llvm_unreachable("call to unknown kind of C function");
}

/// Try to find a clang method declaration for the given function.
static const clang::Decl *findClangMethod(ValueDecl *method) {
  if (FuncDecl *methodFn = dyn_cast<FuncDecl>(method)) {
    if (auto *decl = methodFn->getClangDecl())
      return decl;

    if (auto overridden = methodFn->getOverriddenDecl())
      return findClangMethod(overridden);
  }

  if (ConstructorDecl *constructor = dyn_cast<ConstructorDecl>(method)) {
    if (auto *decl = constructor->getClangDecl())
      return decl;
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
//                      Selector Family SILFunctionTypes
//===----------------------------------------------------------------------===//

/// Apply a macro FAMILY(Name, Prefix) to all ObjC selector families.
#define FOREACH_FAMILY(FAMILY)       \
  FAMILY(Alloc, "alloc")             \
  FAMILY(Copy, "copy")               \
  FAMILY(Init, "init")               \
  FAMILY(MutableCopy, "mutableCopy") \
  FAMILY(New, "new")

namespace {
  enum class SelectorFamily : unsigned {
    None,
#define GET_LABEL(LABEL, PREFIX) LABEL,
FOREACH_FAMILY(GET_LABEL)
#undef GET_LABEL
  };
} // end anonymous namespace

/// Derive the ObjC selector family from an identifier.
///
/// Note that this will never derive the Init family, which is too dangerous
/// to leave to chance. Swift functions starting with "init" are always
/// emitted as if they are part of the "none" family.
static SelectorFamily getSelectorFamily(Identifier name) {
  StringRef text = name.get();
  while (!text.empty() && text[0] == '_') text = text.substr(1);

  /// Does the given selector start with the given string as a
  /// prefix, in the sense of the selector naming conventions?
  auto hasPrefix = [](StringRef text, StringRef prefix) {
    if (!text.startswith(prefix)) return false;
    if (text.size() == prefix.size()) return true;
    assert(text.size() > prefix.size());
    return !clang::isLowercase(text[prefix.size()]);
  };

  auto result = SelectorFamily::None;
  if (false) /*for #define purposes*/;
#define CHECK_PREFIX(LABEL, PREFIX) \
  else if (hasPrefix(text, PREFIX)) result = SelectorFamily::LABEL;
  FOREACH_FAMILY(CHECK_PREFIX)
#undef CHECK_PREFIX

  if (result == SelectorFamily::Init)
    return SelectorFamily::None;
  return result;
}

/// Get the ObjC selector family a SILDeclRef implicitly belongs to.
static SelectorFamily getSelectorFamily(SILDeclRef c) {
  switch (c.kind) {
  case SILDeclRef::Kind::Func: {
    if (!c.hasDecl())
      return SelectorFamily::None;
      
    auto *FD = cast<FuncDecl>(c.getDecl());
    switch (FD->getAccessorKind()) {
    case AccessorKind::NotAccessor:
      return getSelectorFamily(FD->getName());
    case AccessorKind::IsGetter:
      // Getter selectors can belong to families if their name begins with the
      // wrong thing.
      if (FD->getAccessorStorageDecl()->isObjC() || c.isForeign)
        return getSelectorFamily(FD->getAccessorStorageDecl()->getName());
      return SelectorFamily::None;

      // Other accessors are never selector family members.
    case AccessorKind::IsSetter:
    case AccessorKind::IsWillSet:
    case AccessorKind::IsDidSet:
    case AccessorKind::IsAddressor:
    case AccessorKind::IsMutableAddressor:
    case AccessorKind::IsMaterializeForSet:
      return SelectorFamily::None;
    }
  }
  case SILDeclRef::Kind::Initializer:
    case SILDeclRef::Kind::IVarInitializer:
    return SelectorFamily::Init;

  /// Currently IRGen wraps alloc/init methods into Swift constructors
  /// with Swift conventions.
  case SILDeclRef::Kind::Allocator:
  /// These constants don't correspond to method families we care about yet.
  case SILDeclRef::Kind::EnumElement:
  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::Deallocator:
  case SILDeclRef::Kind::GlobalAccessor:
  case SILDeclRef::Kind::GlobalGetter:
  case SILDeclRef::Kind::IVarDestroyer:
  case SILDeclRef::Kind::DefaultArgGenerator:
  case SILDeclRef::Kind::StoredPropertyInitializer:
    return SelectorFamily::None;
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

namespace {
  class SelectorFamilyConventions : public Conventions {
    SelectorFamily Family;

  public:
    SelectorFamilyConventions(SelectorFamily family)
      : Conventions(ConventionsKind::SelectorFamily), Family(family) {}

    ParameterConvention getIndirectParameter(unsigned index,
                              const AbstractionPattern &type) const override {
      return ParameterConvention::Indirect_In;
    }

    ParameterConvention getDirectParameter(unsigned index,
                              const AbstractionPattern &type) const override {
      return ParameterConvention::Direct_Unowned;
    }

    ParameterConvention getCallee() const override {
      // Always thin.
      return ParameterConvention::Direct_Unowned;
    }

    ResultConvention getResult(const TypeLowering &tl) const override {
      switch (Family) {
      case SelectorFamily::Alloc:
      case SelectorFamily::Copy:
      case SelectorFamily::Init:
      case SelectorFamily::MutableCopy:
      case SelectorFamily::New:
        return ResultConvention::Owned;

      case SelectorFamily::None:
        // Defaults below.
        break;
      }

      auto type = tl.getLoweredType().getSwiftRValueType();
      if (type->hasRetainablePointerRepresentation())
        return ResultConvention::Autoreleased;

      return ResultConvention::Unowned;
    }

    ParameterConvention
    getDirectSelfParameter(const AbstractionPattern &type) const override {
      if (Family == SelectorFamily::Init)
        return ParameterConvention::Direct_Owned;
      return ObjCSelfConvention;
    }

    ParameterConvention
    getIndirectSelfParameter(const AbstractionPattern &type) const override {
      llvm_unreachable("selector family objc function types do not support "
                       "indirect self parameters");
    }

    static bool classof(const Conventions *C) {
      return C->getKind() == ConventionsKind::SelectorFamily;
    }
  };
} // end anonymous namespace

static CanSILFunctionType
getSILFunctionTypeForSelectorFamily(SILModule &M, SelectorFamily family,
                                    CanAnyFunctionType origType,
                                    CanAnyFunctionType substInterfaceType,
                                    AnyFunctionType::ExtInfo extInfo,
                     const Optional<ForeignErrorConvention> &foreignError,
                                    Optional<SILDeclRef> constant) {
  return getSILFunctionType(M, AbstractionPattern(origType),
                            substInterfaceType,
                            extInfo,
                            SelectorFamilyConventions(family),
                            foreignError, constant);
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
    return ::getNativeSILFunctionType(M,
                  AbstractionPattern(origLoweredInterfaceType),
                  origLoweredInterfaceType,
                  extInfo,
                  constant,
                  constant.kind);
  }

  Optional<ForeignErrorConvention> foreignError;

  // If we have a clang decl associated with the Swift decl, derive its
  // ownership conventions.
  if (constant.hasDecl()) {
    auto decl = constant.getDecl();
    if (auto funcDecl = dyn_cast<AbstractFunctionDecl>(decl)) {
      foreignError = funcDecl->getForeignErrorConvention();
    }

    if (auto clangDecl = findClangMethod(decl))
      return getSILFunctionTypeForClangDecl(M, clangDecl,
                                            origLoweredInterfaceType,
                                            origLoweredInterfaceType,
                                            extInfo, foreignError, constant);
  }

  // If the decl belongs to an ObjC method family, use that family's
  // ownership conventions.
  return getSILFunctionTypeForSelectorFamily(M, getSelectorFamily(constant),
                                             origLoweredInterfaceType,
                                             origLoweredInterfaceType,
                                             extInfo, foreignError, constant);
}

CanSILFunctionType TypeConverter::
getUncachedSILFunctionTypeForConstant(SILDeclRef constant,
                                      CanAnyFunctionType origInterfaceType) {
  auto origLoweredInterfaceType = getLoweredASTFunctionType(origInterfaceType,
                                                            constant.uncurryLevel,
                                                            constant);
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
  if (auto proto = dyn_cast<ProtocolDecl>(c.getDecl()->getDeclContext()))
    return getProtocolWitnessRepresentation(proto);

  switch (c.kind) {
    case SILDeclRef::Kind::GlobalAccessor:
    case SILDeclRef::Kind::GlobalGetter:
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

SILConstantInfo TypeConverter::getConstantInfo(SILDeclRef constant) {
  auto found = ConstantTypes.find(constant);
  if (found != ConstantTypes.end())
    return found->second;

  // First, get a function type for the constant.  This creates the
  // right type for a getter or setter.
  auto formalInterfaceType = makeConstantInterfaceType(constant);
  auto *genericEnv = getConstantGenericEnvironment(constant);

  // The formal type is just that with the right representation.
  auto rep = getDeclRefRepresentation(constant);
  formalInterfaceType = adjustFunctionType(formalInterfaceType, rep);
  // The lowered type is the formal type, but uncurried and with
  // parameters automatically turned into their bridged equivalents.
  auto loweredInterfaceType =
    getLoweredASTFunctionType(formalInterfaceType, constant.uncurryLevel,
                              constant);

  // The SIL type encodes conventions according to the original type.
  CanSILFunctionType silFnType =
    ::getUncachedSILFunctionTypeForConstant(M, constant,
                                            loweredInterfaceType);

  DEBUG(llvm::dbgs() << "lowering type for constant ";
        constant.print(llvm::dbgs());
        llvm::dbgs() << "\n  formal type: ";
        formalInterfaceType.print(llvm::dbgs());
        llvm::dbgs() << "\n  lowered AST type: ";
        loweredInterfaceType.print(llvm::dbgs());
        llvm::dbgs() << "\n  SIL type: ";
        silFnType.print(llvm::dbgs());
        llvm::dbgs() << "\n");

  SILConstantInfo result = {
    formalInterfaceType,
    loweredInterfaceType,
    silFnType,
    genericEnv
  };
  ConstantTypes[constant] = result;
  return result;
}

namespace {
  class SILFunctionTypeSubstituter {
    TypeConverter &TC;
    CanSILFunctionType OrigFnType;
    ArrayRef<SILParameterInfo> OrigParams;
    ArrayRef<SILResultInfo> OrigResults;
    unsigned NextOrigParamIndex = 0;
    unsigned NextOrigResultIndex = 0;
    SmallVector<SILParameterInfo, 8> SubstParams;
    SmallVector<SILResultInfo, 8> SubstResults;
    const Optional<ForeignErrorConvention> &ForeignError;

  public:
    SILFunctionTypeSubstituter(
        TypeConverter &TC, CanSILFunctionType origFnType,
        const Optional<ForeignErrorConvention> &foreignError)
        : TC(TC), OrigFnType(origFnType),
          OrigParams(origFnType->getParameters()),
          OrigResults(origFnType->getResults()), ForeignError(foreignError) {}

    ArrayRef<SILResultInfo> getSubstResults() const {
      assert(NextOrigResultIndex == OrigResults.size() &&
             "didn't claim all results?!");
      return SubstResults;
    }

    void substResults(AbstractionPattern origType, CanType substType);

    ArrayRef<SILParameterInfo> getSubstParams() const {
      assert(NextOrigParamIndex == OrigParams.size() &&
             "didn't claim all parameters?!");
      return SubstParams;
    }

    void substInputs(AbstractionPattern origType, CanType substType) {
      maybeSkipForeignErrorParameter();

      // Decompose tuples.
      if (origType.isTuple()) {
        auto substTuple = cast<TupleType>(substType);
        assert(origType.getNumTupleElements() == substTuple->getNumElements());
        for (auto i : indices(substTuple.getElementTypes())) {
          substInputs(origType.getTupleElementType(i),
                      substTuple.getElementType(i));
        }
        return;
      }

      // Every other type corresponds to a single parameter in the
      // original signature, since we're dealing with like-uncurried
      // types and thus don't have to worry about expanding archetypes
      // to unmaterializable parameter clauses in result function types.
      auto origParam = claimNextOrigParam();

      // If the type hasn't changed and doesn't rely on context, just use the
      // original parameter.
      if (origType.isExactType(substType) &&
          !origParam.getType()->hasTypeParameter()) {
        SubstParams.push_back(origParam);
        return;
      }

      // Otherwise, lower the substituted type using the abstraction
      // patterns of the original.
      auto &substTL = TC.getTypeLowering(origType, substType);
      auto substConvention = getSubstConvention(origParam.getConvention(),
                                                substTL.isTrivial());
      assert(isIndirectFormalParameter(substConvention)
             || !substTL.isAddressOnly());
      addSubstParam(substTL.getLoweredType().getSwiftRValueType(),
                    substConvention);
    }

  private:
    void decomposeResult(AbstractionPattern origType, CanType substType);

    SILParameterInfo claimNextOrigParam() {
      maybeSkipForeignErrorParameter();
      return OrigParams[NextOrigParamIndex++];
    }

    SILResultInfo claimNextOrigResult() {
      return OrigResults[NextOrigResultIndex++];
    }

    void maybeSkipForeignErrorParameter() {
      if (!ForeignError ||
          NextOrigParamIndex != ForeignError->getErrorParameterIndex())
        return;
      SubstParams.push_back(OrigParams[NextOrigParamIndex++]);
    }

    void addSubstParam(CanType type, ParameterConvention conv) {
      SubstParams.push_back(SILParameterInfo(type, conv));
    }

    ParameterConvention getSubstConvention(ParameterConvention orig,
                                           bool isTrivial) {
      // We use the original convention, except that we have an
      // invariant that direct trivial parameters are always unowned.
      switch (orig) {
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Guaranteed:
        if (isTrivial) return ParameterConvention::Direct_Unowned;
        SWIFT_FALLTHROUGH;
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_InoutAliasable:
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed:
        return orig;
      }
      llvm_unreachable("bad parameter convention");
    }

    ResultConvention getSubstConvention(ResultConvention orig,
                                        bool isTrivial) {
      // We use the original convention, except that we have an
      // invariant that direct trivial results are always unowned.
      switch (orig) {
      case ResultConvention::Owned:
      case ResultConvention::Autoreleased:
        if (isTrivial) return ResultConvention::Unowned;
        SWIFT_FALLTHROUGH;
      case ResultConvention::Indirect:
      case ResultConvention::Unowned:
      case ResultConvention::UnownedInnerPointer:
        return orig;
      }
      llvm_unreachable("bad parameter convention");
    }
  };
} // end anonymous namespace

void SILFunctionTypeSubstituter::substResults(AbstractionPattern origResultType,
                                              CanType substResultType) {
  // Fast path: if the results of the original type are not type-dependent,
  // we can just copy them over.
  auto allResults = OrigFnType->getResults();
  if (std::find_if(allResults.begin(), allResults.end(),
                   [&](SILResultInfo result) { 
                     return result.getType()->hasTypeParameter();
                   }) == allResults.end()) {
    SubstResults.append(allResults.begin(), allResults.end());
    return;
  }

  // Okay, we need to walk the types and re-lower.

  // If we have a foreign-error convention that strips result
  // optionality, we need to wrap both the original and
  // substituted types in a level of optionality.
  if (ForeignError && ForeignError->stripsResultOptionality()) {
    origResultType =
      AbstractionPattern::getOptional(origResultType, OTK_Optional);
    substResultType =
      OptionalType::get(substResultType)->getCanonicalType();
  }

  decomposeResult(origResultType, substResultType);
}

void
SILFunctionTypeSubstituter::decomposeResult(AbstractionPattern origResultType,
                                            CanType substResultType) {
  // If the result is a tuple, we need to expand it.
  if (origResultType.isTuple()) {
    auto substResultTupleType = cast<TupleType>(substResultType);
    for (auto eltIndex : indices(substResultTupleType.getElementTypes())) {
      auto origEltType = origResultType.getTupleElementType(eltIndex);
      auto substEltType = substResultTupleType.getElementType(eltIndex);
      decomposeResult(origEltType, substEltType);
    }
    return;
  }

  // Okay, the result is a single value, which will either be an
  // indirect result or not.

  // Grab the next result.
  SILResultInfo origResult = claimNextOrigResult();

  // If substitution is trivial, fast path.
  if (!origResult.getType()->hasTypeParameter()) {
    SubstResults.push_back(origResult);
    return;
  }

  // Lower the substituted result using the abstraction patterns
  // of the original result.
  auto &substResultTL = TC.getTypeLowering(origResultType, substResultType);
  auto loweredResultTy = substResultTL.getLoweredType().getSwiftRValueType();

  // Return the new type with the old convention.
  SILResultInfo substResult(loweredResultTy,
                            getSubstConvention(origResult.getConvention(),
                                               substResultTL.isTrivial()));
  SubstResults.push_back(substResult);
}

/// Apply a substitution to the given SILFunctionType so that it has
/// the form of the normal SILFunctionType for the substituted type,
/// except using the original conventions.
///
/// This is equivalent to
///    getLoweredType(origLoweredType,
///                   substLoweredType).castTo<SILFunctionType>()
/// except that origFnType's conventions may not correspond to the
/// standard conventions of the lowered type.
CanSILFunctionType
TypeConverter::substFunctionType(CanSILFunctionType origFnType,
                                 CanAnyFunctionType origLoweredType,
                                 CanAnyFunctionType substLoweredInterfaceType,
                         const Optional<ForeignErrorConvention> &foreignError) {
  // FIXME: is this inefficient now?
  if (origLoweredType == substLoweredInterfaceType)
    return origFnType;

  // Use the generic parameters from the substituted type.
  CanGenericSignature genericSig;
  if (auto genSubstFn = dyn_cast<GenericFunctionType>(substLoweredInterfaceType))
    genericSig = genSubstFn.getGenericSignature();

  GenericContextScope scope(*this, genericSig);
  SILFunctionTypeSubstituter substituter(*this, origFnType, foreignError);

  AbstractionPattern origLoweredPattern(origLoweredType);

  // Map the results.
  substituter.substResults(origLoweredPattern.getFunctionResultType(),
                           substLoweredInterfaceType.getResult());

  // Map the error result.  Currently this is never dependent.
  Optional<SILResultInfo> substErrorResult
    = origFnType->getOptionalErrorResult();
  assert(!substErrorResult ||
         (!substErrorResult->getType()->hasTypeParameter() &&
          !substErrorResult->getType()->hasArchetype()));

  // Map the inputs.
  substituter.substInputs(origLoweredPattern.getFunctionInputType(),
                          substLoweredInterfaceType.getInput());

  // Allow the substituted type to add thick-ness, but not remove it.
  assert(!origFnType->getExtInfo().hasContext()
           || substLoweredInterfaceType->getExtInfo().hasContext());
  assert(substLoweredInterfaceType->getExtInfo().getSILRepresentation()
           == substLoweredInterfaceType->getExtInfo().getSILRepresentation());

  auto rep = substLoweredInterfaceType->getExtInfo().getSILRepresentation();
  auto extInfo = origFnType->getExtInfo().withRepresentation(rep);

  // FIXME: Map into archetype context.
  return SILFunctionType::get(genericSig,
                              extInfo,
                              origFnType->getCalleeConvention(),
                              substituter.getSubstParams(),
                              substituter.getSubstResults(),
                              substErrorResult,
                              Context);
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

/// Returns the ConstantInfo corresponding to the VTable thunk for overriding.
/// Will be the same as getConstantInfo if the declaration does not override.
SILConstantInfo TypeConverter::getConstantOverrideInfo(SILDeclRef derived,
                                                       SILDeclRef base) {
  // Foreign overrides currently don't need reabstraction.
  if (derived.isForeign)
    return getConstantInfo(derived);

  auto found = ConstantOverrideTypes.find({derived, base});
  if (found != ConstantOverrideTypes.end())
    return found->second;

  assert(base.getNextOverriddenVTableEntry().isNull()
         && "base must not be an override");

  auto baseInfo = getConstantInfo(base);
  auto derivedInfo = getConstantInfo(derived);

  // If the derived method is ABI-compatible with the base method, give the
  // vtable thunk the same signature as the derived method.
  auto basePattern = AbstractionPattern(baseInfo.LoweredInterfaceType);

  auto baseInterfaceTy = makeConstantInterfaceType(base);
  auto derivedInterfaceTy = makeConstantInterfaceType(derived);

  auto selfInterfaceTy = derivedInterfaceTy.getInput()->getRValueInstanceType();

  auto overrideInterfaceTy =
      selfInterfaceTy->adjustSuperclassMemberDeclType(
          base.getDecl(), derived.getDecl(), baseInterfaceTy,
          /*resolver=*/nullptr);

  // Copy generic signature from derived to the override type, to handle
  // the case where the base member is not generic (because the base class
  // is concrete) but the derived member is generic (because the derived
  // class is generic).
  if (auto derivedInterfaceFnTy = derivedInterfaceTy->getAs<GenericFunctionType>()) {
    auto overrideInterfaceFnTy = overrideInterfaceTy->castTo<AnyFunctionType>();
    overrideInterfaceTy =
        GenericFunctionType::get(derivedInterfaceFnTy->getGenericSignature(),
                                 overrideInterfaceFnTy->getInput(),
                                 overrideInterfaceFnTy->getResult(),
                                 overrideInterfaceFnTy->getExtInfo());
  }

  // Lower the formal AST type.
  auto overrideLoweredInterfaceTy = getLoweredASTFunctionType(
      cast<AnyFunctionType>(overrideInterfaceTy->getCanonicalType()),
      derived.uncurryLevel, derived);

  // Build the SILFunctionType for the vtable thunk.
  CanSILFunctionType fnTy = getNativeSILFunctionType(M, basePattern,
                                                     overrideLoweredInterfaceTy,
                                                     derived,
                                                     derived.kind);

  {
    GenericContextScope scope(*this, fnTy->getGenericSignature());

    // Is the vtable thunk type actually ABI compatible with the derived type?
    // Then we don't need a thunk.
    if (checkFunctionForABIDifferences(derivedInfo.SILFnType, fnTy)
          == ABIDifference::Trivial) {
      ConstantOverrideTypes[{derived, base}] = derivedInfo;
      return derivedInfo;
    }
  }

  // Build the SILConstantInfo and cache it.
  SILConstantInfo overrideInfo;
  overrideInfo.LoweredInterfaceType = overrideLoweredInterfaceTy;
  overrideInfo.SILFnType = fnTy;
  overrideInfo.GenericEnv = derivedInfo.GenericEnv;
  ConstantOverrideTypes[{derived, base}] = overrideInfo;

  return overrideInfo;
}

namespace {
  /// Given a lowered SIL type, apply a substitution to it to produce another
  /// lowered SIL type which uses the same abstraction conventions.
  class SILTypeSubstituter :
      public CanTypeVisitor<SILTypeSubstituter, CanType> {
    SILModule &TheSILModule;
    // Order dependency - Context must initialize before Subst and Conformances
    Optional<std::pair<QueryTypeSubstitutionMap,
                       LookUpConformanceInSubstitutionMap>> Context;
    TypeSubstitutionFn Subst;
    LookupConformanceFn Conformances;

    ASTContext &getASTContext() { return TheSILModule.getASTContext(); }

  public:
    SILTypeSubstituter(SILModule &silModule, const SubstitutionMap &subs)
      : TheSILModule(silModule),
        Context({QueryTypeSubstitutionMap{subs.getMap()},
                 LookUpConformanceInSubstitutionMap(subs)}),
        Subst(Context->first),
        Conformances(Context->second)
    {}

    SILTypeSubstituter(SILModule &silModule,
                       TypeSubstitutionFn Subst,
                       LookupConformanceFn Conformances)
      : TheSILModule(silModule),
        Subst(Subst),
        Conformances(Conformances)
    {}

    // SIL type lowering only does special things to tuples and functions.

    /// Functions need to preserve their abstraction structure.
    CanSILFunctionType visitSILFunctionType(CanSILFunctionType origType,
                                            bool dropGenerics = false)
    {
      GenericContextScope scope(TheSILModule.Types,
                                origType->getGenericSignature());

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

      auto genericSig
        = (dropGenerics ? nullptr : origType->getGenericSignature());

      return SILFunctionType::get(genericSig,
                                  origType->getExtInfo(),
                                  origType->getCalleeConvention(),
                                  substParams, substResults,
                                  substErrorResult,
                                  getASTContext());
    }

    SILType subst(SILType type) {
      return SILType::getPrimitiveType(visit(type.getSwiftRValueType()),
                                       type.getCategory());
    }

    SILResultInfo subst(SILResultInfo orig) {
      return SILResultInfo(visit(orig.getType()), orig.getConvention());
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
      if (!origType->getDecl()->classifyAsOptionalType()) {
        return visitType(origType);
      }

      CanType origObjectType = origType.getGenericArgs()[0];
      CanType substObjectType = visit(origObjectType);
      return CanType(BoundGenericType::get(origType->getDecl(), Type(),
                                           substObjectType));
    }

    /// Any other type is would be a valid type in the AST.  Just
    /// apply the substitution on the AST level and then lower that.
    CanType visitType(CanType origType) {
      assert(!isa<AnyFunctionType>(origType));
      assert(!isa<LValueType>(origType) && !isa<InOutType>(origType));

      CanGenericSignature genericSig =
          TheSILModule.Types.getCurGenericContext();
      AbstractionPattern abstraction(genericSig, origType);

      assert(TheSILModule.Types.getLoweredType(abstraction, origType)
               .getSwiftRValueType() == origType);

      CanType substType =
        origType.subst(Subst, Conformances, None)->getCanonicalType();

      // If the substitution didn't change anything, we know that the
      // original type was a lowered type, so we're good.
      if (origType == substType) {
        return origType;
      }

      return TheSILModule.Types.getLoweredType(abstraction, substType)
               .getSwiftRValueType();
    }
  };
} // end anonymous namespace

SILType SILType::subst(SILModule &silModule, const SubstitutionMap &subs) const{
  SILTypeSubstituter STST(silModule, subs);
  return STST.subst(*this);
}

CanSILFunctionType SILType::substFuncType(SILModule &silModule,
                                          const SubstitutionMap &subs,
                                          CanSILFunctionType SrcTy,
                                          bool dropGenerics) {
  SILTypeSubstituter STST(silModule, subs);
  return STST.visitSILFunctionType(SrcTy, dropGenerics);
}

/// Apply a substitution to this polymorphic SILFunctionType so that
/// it has the form of the normal SILFunctionType for the substituted
/// type, except using the original conventions.
CanSILFunctionType
SILFunctionType::substGenericArgs(SILModule &silModule,
                                  ArrayRef<Substitution> subs) {
  if (subs.empty()) {
    assert(!isPolymorphic() && "no args for polymorphic substitution");
    return CanSILFunctionType(this);
  }

  assert(isPolymorphic());
  auto map = GenericSig->getSubstitutionMap(subs);
  SILTypeSubstituter substituter(silModule, map);

  return substituter.visitSILFunctionType(CanSILFunctionType(this),
                                          /*dropGenerics*/ true);
}

/// Apply a substitution to this polymorphic SILFunctionType so that
/// it has the form of the normal SILFunctionType for the substituted
/// type, except using the original conventions.
CanSILFunctionType
SILFunctionType::substGenericArgs(SILModule &silModule,
                                  const SubstitutionMap &subs) {
  if (subs.empty()) {
    assert(!isPolymorphic() && "no args for polymorphic substitution");
    return CanSILFunctionType(this);
  }

  assert(isPolymorphic());
  SILTypeSubstituter substituter(silModule, subs);

  return substituter.visitSILFunctionType(CanSILFunctionType(this),
                                          /*dropGenerics*/ true);
}

CanSILFunctionType
SILFunctionType::substGenericArgs(SILModule &silModule,
                                  TypeSubstitutionFn subs,
                                  LookupConformanceFn conformances) {
  if (!isPolymorphic()) return CanSILFunctionType(this);
  SILTypeSubstituter substituter(silModule, subs, conformances);
  return substituter.visitSILFunctionType(CanSILFunctionType(this),
                                          /*dropGenerics*/ true);
}

/// Fast path for bridging types in a function type without uncurrying.
CanAnyFunctionType
TypeConverter::getBridgedFunctionType(AbstractionPattern pattern,
                                      CanAnyFunctionType t,
                                      AnyFunctionType::ExtInfo extInfo) {
  // Pull out the generic signature.
  CanGenericSignature genericSig;
  if (auto gft = dyn_cast<GenericFunctionType>(t)) {
    genericSig = gft.getGenericSignature();
  }

  auto rebuild = [&](CanType input, CanType result) -> CanAnyFunctionType {
    if (genericSig) {
      return CanGenericFunctionType::get(genericSig, input, result, extInfo);
    } else {
      return CanFunctionType::get(input, result, extInfo);
    }
  };

  switch (auto rep = t->getExtInfo().getSILRepresentation()) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::WitnessMethod:
    // No bridging needed for native functions.
    if (t->getExtInfo() == extInfo)
      return t;
    return rebuild(t.getInput(), t.getResult());

  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::ObjCMethod:
    return rebuild(getBridgedInputType(rep, pattern.getFunctionInputType(),
                                       t.getInput()),
                   getBridgedResultType(rep, pattern.getFunctionResultType(),
                                        t.getResult(),
                        pattern.hasForeignErrorStrippingResultOptionality()));
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
  case SILDeclRef::Kind::GlobalGetter:
  case SILDeclRef::Kind::DefaultArgGenerator:
  case SILDeclRef::Kind::StoredPropertyInitializer:
  case SILDeclRef::Kind::IVarInitializer:
  case SILDeclRef::Kind::IVarDestroyer:
    return nullptr;
  }
  llvm_unreachable("bad SILDeclRef kind");
}

static CanType createProductType(CanType type1, CanType type2,
                                 ASTContext &Context) {
  if (auto tupleType = dyn_cast<TupleType>(type1))
    if (tupleType->getNumElements() == 0)
      return type2;
  return CanType(TupleType::get({type1, type2}, Context));
}

CanAnyFunctionType
TypeConverter::getLoweredASTFunctionType(CanAnyFunctionType fnType,
                                         unsigned uncurryLevel,
                                         AnyFunctionType::ExtInfo extInfo,
                                         Optional<SILDeclRef> constant) {
  // Form an abstraction pattern for bridging purposes.
  // Foreign functions are only available at very specific uncurry levels.
  auto bridgingFnPattern = AbstractionPattern::getOpaque();
  if (constant && constant->isForeign) {
    auto bridgedFn = getBridgedFunction(*constant);

    const clang::Decl *clangDecl;
    if (bridgedFn && (clangDecl = bridgedFn->getClangDecl())) {

      // Clang-generated accessors are "uncurried" here and not in the
      // below loop because ClangType AbstractionPatterns don't support
      // currying.
      if (uncurryLevel == 1 &&
          isa<FuncDecl>(bridgedFn) &&
          !cast<FuncDecl>(bridgedFn)->isImportAsMember() &&
          cast<FuncDecl>(bridgedFn)->isAccessor() &&
          extInfo.getSILRepresentation() == SILFunctionTypeRepresentation::CFunctionPointer) {

        // Can't be polymorphic
        assert(isa<FunctionType>(fnType));
        auto resultFnType = cast<FunctionType>(fnType.getResult());

        CanType inputType = createProductType(resultFnType.getInput(),
                                              fnType.getInput(),
                                              Context);
        CanType resultType = resultFnType.getResult();

        // Rebuild the uncurried accessor type.
        fnType = CanFunctionType::get(inputType, resultType, extInfo);

        // Hit the fast path below.
        uncurryLevel = 0;
      }

      // Don't implicitly turn non-optional results to optional if
      // we're going to apply a foreign error convention that checks
      // for nil results.
      if (auto method = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
        assert(uncurryLevel == 1 && "getting curried ObjC method type?");
        auto foreignError = bridgedFn->getForeignErrorConvention();
        bridgingFnPattern =
          AbstractionPattern::getCurriedObjCMethod(fnType, method,
                                                   foreignError);
      } else if (auto value = dyn_cast<clang::ValueDecl>(clangDecl)) {
        if (uncurryLevel == 0) {
          // C function imported as a function.
          bridgingFnPattern =
            AbstractionPattern(fnType, value->getType().getTypePtr());
        } else {
          // C function imported as a method.
          assert(uncurryLevel == 1);
          bridgingFnPattern =
            AbstractionPattern::getCurriedCFunctionAsMethod(fnType, bridgedFn);
        }
      }
    }
  }

  // Fast path: no uncurrying required.
  if (uncurryLevel == 0)
    return getBridgedFunctionType(bridgingFnPattern, fnType, extInfo);

  SILFunctionTypeRepresentation rep = extInfo.getSILRepresentation();
  assert(!extInfo.isAutoClosure() && "autoclosures cannot be curried");
  assert(rep != SILFunctionType::Representation::Block
         && "objc blocks cannot be curried");

  // The dependent generic signature.
  CanGenericSignature genericSig;
  if (auto gft = dyn_cast<GenericFunctionType>(fnType)) {
    genericSig = gft.getGenericSignature();
  }

  // The uncurried input types.
  SmallVector<TupleTypeElt, 4> inputs;

  // Merge inputs and generic parameters from the uncurry levels.
  for (;;) {
    inputs.push_back(TupleTypeElt(fnType->getInput()));

    // The uncurried function calls all of the intermediate function
    // levels and so throws if any of them do.
    if (fnType->getExtInfo().throws())
      extInfo = extInfo.withThrows();

    if (uncurryLevel-- == 0)
      break;
    fnType = cast<AnyFunctionType>(fnType.getResult());
  }

  CanType resultType = fnType.getResult();
  bool suppressOptionalResult =
    bridgingFnPattern.hasForeignErrorStrippingResultOptionality();

  // Bridge input and result types.
  switch (rep) {
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::WitnessMethod:
    // Native functions don't need bridging.
    break;

  case SILFunctionTypeRepresentation::ObjCMethod: {
    assert(inputs.size() == 2);
    // The "self" parameter should not get bridged unless it's a metatype.
    if (inputs.front().getType()->is<AnyMetatypeType>()) {
      auto inputPattern = bridgingFnPattern.getFunctionInputType();
      inputs[0] = inputs[0].getWithType(
        getBridgedInputType(rep, inputPattern, CanType(inputs[0].getType())));
    }

    auto partialFnPattern = bridgingFnPattern.getFunctionResultType();
    inputs[1] = inputs[1].getWithType(
        getBridgedInputType(rep, partialFnPattern.getFunctionInputType(),
                            CanType(inputs[1].getType())));

    resultType = getBridgedResultType(rep,
                                   partialFnPattern.getFunctionResultType(),
                                   resultType, suppressOptionalResult);
    break;
  }

  case SILFunctionTypeRepresentation::CFunctionPointer: {
    // A C function imported as a method.
    assert(inputs.size() == 2);

    // Bridge the parameters.
    auto partialFnPattern = bridgingFnPattern.getFunctionResultType();
    inputs[1] = inputs[1].getWithType(
                getBridgedInputType(rep, partialFnPattern.getFunctionInputType(),
                                    CanType(inputs[1].getType())));

    // If the 'self' parameter is a metatype, we'll throw it away.
    // Otherwise, splice it in to the parameters at the right position.
    if (!inputs[0].getType()->is<AnyMetatypeType>()) {
      auto memberStatus = bridgingFnPattern.getImportAsMemberStatus();
      assert(memberStatus.isInstance() &&
             "static method with non-metatype self?!");
      SmallVector<TupleTypeElt, 4> fields;
      if (auto tuple = inputs[1].getType()->getAs<TupleType>()) {
        fields.append(tuple->getElements().begin(), tuple->getElements().end());
      } else {
        fields.push_back(inputs[1].getType());
      }
      fields.insert(fields.begin() + memberStatus.getSelfIndex(),
                    inputs[0].getType());
      inputs[1] = inputs[1].getWithType(TupleType::get(fields, Context));
    }
    inputs.erase(inputs.begin());
    
    resultType = getBridgedResultType(rep,
                                      partialFnPattern.getFunctionResultType(),
                                      resultType, suppressOptionalResult);
    break;
  }

  case SILFunctionTypeRepresentation::Block:
    llvm_unreachable("Cannot uncurry native representation");
  }

  // Put the inputs in the order expected by the calling convention.
  std::reverse(inputs.begin(), inputs.end());

  // Create the new function type.
  CanType inputType = TupleType::get(inputs, Context)->getCanonicalType();
  if (genericSig) {
    return CanGenericFunctionType::get(genericSig,
                                       inputType, resultType, extInfo);
  } else {
    return CanFunctionType::get(inputType, resultType, extInfo);
  }
}
