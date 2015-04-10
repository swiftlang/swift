//===--- SILFunctionType.cpp - Giving SIL types to AST functions ----------===//
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
// This file defines the native Swift ownership transfer conventions
// and works in concert with the importer to give the correct
// conventions to imported functions and types.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "libsil"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILModule.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/SubstTypeVisitor.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/Analysis/DomainSpecific/CocoaConventions.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;
using namespace swift::Lowering;

static CanType getKnownType(Optional<CanType> &cacheSlot, ASTContext &C,
                            StringRef moduleName, StringRef typeName) {
  if (!cacheSlot) {
    cacheSlot = ([&] {
      Module *mod = C.getLoadedModule(C.getIdentifier(moduleName));
      if (!mod)
        return CanType();

      // Do a general qualified lookup instead of a direct lookupValue because
      // some of the types we want are reexported through overlays and
      // lookupValue would only give us types actually declared in the overlays
      // themselves.
      SmallVector<ValueDecl *, 2> decls;
      mod->lookupQualified(ModuleType::get(mod), C.getIdentifier(typeName),
                           NL_QualifiedDefault | NL_KnownNonCascadingDependency,
                           /*resolver=*/nullptr, decls);
      if (decls.size() != 1)
        return CanType();

      const TypeDecl *typeDecl = dyn_cast<TypeDecl>(decls.front());
      if (!typeDecl)
        return CanType();

      assert(typeDecl->getDeclaredType() &&
             "bridged type must be type-checked");
      return typeDecl->getDeclaredType()->getCanonicalType();
    })();
  }
  CanType t = *cacheSlot;

  // It is possible that we won't find a briding type (e.g. String) when we're
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

  SIL_FUNCTION_TYPE_IGNORE_DEPRECATED_BEGIN
  return SILFunctionType::get(type->getGenericSignature(),
                              extInfo,
                              callee,
                              type->getParameters(),
                              type->getResult(),
                              type->getOptionalErrorResult(),
                              type->getASTContext());
  SIL_FUNCTION_TYPE_IGNORE_DEPRECATED_END
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

  template<typename F>
  class DestructuredArgumentTypeVisitor
    : public CanTypeVisitor<DestructuredArgumentTypeVisitor<F>> {
    const F &fn;
  public:
    DestructuredArgumentTypeVisitor(const F &fn) : fn(fn) { }

    void visitType(CanType t) {
      fn(t);
    }

    void visitTupleType(CanTupleType tt) {
      for (auto eltType : tt.getElementTypes()) {
        this->visit(eltType);
      }
    }
  };

  template <typename F>
  void visitDestructuredArgumentTypes(const F &fn, CanType type) {
    DestructuredArgumentTypeVisitor<F>(fn).visit(type);
  }

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
    /// Query whether the original type is address-only given complete
    /// lowering information about its substitution.
    bool isPassedIndirectly(AbstractionPattern origType, CanType substType,
                            const TypeLowering &substTL) {
      // If the substituted type is passed indirectly, so must the
      // unsubstituted type.
      if (origType.isOpaque() || substTL.isPassedIndirectly()) {
        return true;

      // If the substitution didn't change the type, then a negative
      // response to the above is determinative as well.
      } else if (origType.getType() == substType) {
        return false;

      // Otherwise, query specifically for the original type.
      } else {
        return SILType::isPassedIndirectly(origType.getType(), M);
      }
    }

    void visitSelfType(AbstractionPattern origType, CanType substType,
                       SILFunctionTypeRepresentation rep) {
      auto &substTL =
        M.Types.getTypeLowering(origType, substType);
      ParameterConvention convention;
      if (origType.getAs<InOutType>()) {
        convention = ParameterConvention::Indirect_Inout;
      } else if (isPassedIndirectly(origType, substType, substTL)) {
        if (rep == SILFunctionTypeRepresentation::WitnessMethod)
          convention = ParameterConvention::Indirect_In_Guaranteed;
        else
          convention = Convs.getIndirectSelfParameter(origType);
        assert(isIndirectParameter(convention));

      } else if (substTL.isTrivial()) {
        convention = ParameterConvention::Direct_Unowned;
      } else {
        convention = Convs.getDirectSelfParameter(origType);
        assert(!isIndirectParameter(convention));
      }

      auto loweredType = substTL.getLoweredType().getSwiftRValueType();
      addParameter(SILParameterInfo(loweredType, convention));
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
        visitSelfType(origType, substType, extInfo.getSILRepresentation());
      }
    }

    void visit(AbstractionPattern origType, CanType substType) {
      // Expand tuples.  But if the abstraction pattern is opaque, and
      // the tuple type is materializable -- if it doesn't contain an
      // l-value type -- then it's a valid target for substitution and
      // we should not expand it.
      if (isa<TupleType>(substType) &&
          (!origType.isOpaque() || !substType->isMaterializable())) {
        auto substTuple = cast<TupleType>(substType);
        assert(origType.isOpaque() ||
               origType.getNumTupleElements() == substTuple->getNumElements());
        for (auto i : indices(substTuple.getElementTypes())) {
          visit(origType.getTupleElementType(i), substTuple.getElementType(i));
        }
        return;
      }

      unsigned origParamIndex = NextOrigParamIndex++;

      auto &substTL = M.Types.getTypeLowering(origType, substType);
      ParameterConvention convention;
      if (origType.getAs<InOutType>()) {
        convention = ParameterConvention::Indirect_Inout;
      } else if (isPassedIndirectly(origType, substType, substTL)) {
        convention = Convs.getIndirectParameter(origParamIndex, origType);
        assert(isIndirectParameter(convention));
      } else if (substTL.isTrivial()) {
        convention = ParameterConvention::Direct_Unowned;
      } else {
        convention = Convs.getDirectParameter(origParamIndex, origType);
        assert(!isIndirectParameter(convention));
      }
      auto loweredType = substTL.getLoweredType().getSwiftRValueType();
      addParameter(SILParameterInfo(loweredType, convention));
    }

    void addParameter(SILParameterInfo param) {
      maybeAddForeignErrorParameter();
      Inputs.push_back(param);
    }

    void maybeAddForeignErrorParameter() {
      if (!ForeignError ||
          NextOrigParamIndex != ForeignError->getErrorParameterIndex())
        return;

      // Assume the error parameter doesn't have interesting lowering.
      Inputs.push_back(SILParameterInfo(ForeignError->getErrorParameterType(),
                                        ParameterConvention::Direct_Unowned));
      NextOrigParamIndex++;
    }
  };
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
                                             CanAnyFunctionType substFnOldType,
                                             CanAnyFunctionType substFnInterfaceType,
                                             AnyFunctionType::ExtInfo extInfo,
                                             const Conventions &conventions,
                        const Optional<ForeignErrorConvention> &foreignError) {
  SmallVector<SILParameterInfo, 8> inputs;

  // Per above, only honor the most general abstraction pattern for thick
  // or polymorphic functions.
  if (origType.isOpaque() &&
      substFnOldType->getExtInfo().getSILRepresentation()
        != SILFunctionType::Representation::Thick &&
      isa<FunctionType>(substFnOldType)) {
    origType = AbstractionPattern(substFnOldType);
  }

  // Find the generic parameters.
  GenericParamList *genericParams = nullptr;
  if (auto polyFnType = dyn_cast<PolymorphicFunctionType>(substFnOldType)) {
    genericParams = &polyFnType->getGenericParams();
  }
  GenericSignature *genericSig = nullptr;
  if (auto genFnType = dyn_cast<GenericFunctionType>(substFnInterfaceType)) {
    genericSig = GenericSignature::get(genFnType->getGenericParams(),
                                       genFnType->getRequirements());
  }
  assert(bool(genericParams) == bool(genericSig));

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
      origResultType = AbstractionPattern(substFormalResultType);
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
    case ForeignErrorConvention::NonNilError:
      break;
    }
  }

  auto &substResultTL = M.Types.getTypeLowering(origResultType,
                                                substFormalResultType);
  bool hasIndirectResult;

  // If the substituted result type is returned indirectly, then the
  // original type must be as well.
  if (substResultTL.isReturnedIndirectly()) {
    hasIndirectResult = true;

  // If the unsubstituted type is dependent, then we use the most
  // general type for the function, which involves an indirect result.
  } else if (origResultType.isOpaque()) {
    hasIndirectResult = true;

  // If the substitution didn't change the result type, we can use the
  // lowering that we already fetched.
  } else if (origResultType.getType() == substFormalResultType) {
    assert(!substResultTL.isReturnedIndirectly());
    hasIndirectResult = false;

  // Otherwise, ask whether the original result type was address-only.
  } else {
    hasIndirectResult =
      SILType::isReturnedIndirectly(origResultType.getType(), M);
  }

  // Okay, with that we can actually construct the result type.
  CanType loweredResultType = substResultTL.getLoweredType().getSwiftRValueType();
  SILResultInfo result;
  if (hasIndirectResult) {
    inputs.push_back(SILParameterInfo(loweredResultType,
                                      ParameterConvention::Indirect_Out));
    result = SILResultInfo(TupleType::getEmpty(M.getASTContext()),
                           ResultConvention::Unowned);
  } else {
    ResultConvention convention;
    convention = conventions.getResult(substResultTL);
    if (substResultTL.isTrivial()) {
      // Reduce conventions for trivial types to an unowned convention.
      switch (convention) {
      case ResultConvention::Unowned:
      case ResultConvention::UnownedInnerPointer:
        // Already unowned.
        break;

      case ResultConvention::Autoreleased:
      case ResultConvention::Owned:
        // These aren't distinguishable from unowned for trivial types.
        convention = ResultConvention::Unowned;
        break;
      }
    }
    result = SILResultInfo(loweredResultType, convention);
  }

  // Destructure the input tuple type.
  {
    DestructureInputs InputDestructurer(M, conventions, foreignError, inputs);
    InputDestructurer.destructure(origType.getFunctionInputType(),
                                  substFnInterfaceType.getInput(),
                                  extInfo);
  }

  auto calleeConvention = ParameterConvention::Direct_Unowned;
  if (extInfo.hasContext())
    calleeConvention = conventions.getCallee();

  // Always strip the auto-closure and no-escape bit.
  // TODO: The noescape bit could be of interest to SIL optimizations.
  //   We should bring it back when we have those optimizations.
  auto silExtInfo = SILFunctionType::ExtInfo()
    .withRepresentation(extInfo.getSILRepresentation())
    .withIsNoReturn(extInfo.isNoReturn());
  
  return SILFunctionType::get(genericSig,
                              silExtInfo, calleeConvention,
                              inputs, result, errorResult,
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
    bool hasGuaranteedSelf;

    DefaultConventions(bool guaranteedSelf)
      : Conventions(ConventionsKind::Default),
        hasGuaranteedSelf(guaranteedSelf) {}

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
      if (hasGuaranteedSelf)
        return ParameterConvention::Direct_Guaranteed;
      return ParameterConvention::Direct_Owned;
    }

    ParameterConvention
    getIndirectSelfParameter(const AbstractionPattern &type) const override {
      if (hasGuaranteedSelf)
        return ParameterConvention::Indirect_In_Guaranteed;
      return ParameterConvention::Indirect_In;
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
}

static CanSILFunctionType getNativeSILFunctionType(SILModule &M,
                                           AbstractionPattern origType,
                                           CanAnyFunctionType substType,
                                           CanAnyFunctionType substInterfaceType,
                                           AnyFunctionType::ExtInfo extInfo,
                                           SILDeclRef::Kind kind) {
  switch (extInfo.getSILRepresentation()) {
  case SILFunctionType::Representation::Block:
  case SILFunctionType::Representation::CFunctionPointer:
    return getSILFunctionType(M, origType, substType, substInterfaceType,
                              extInfo, DefaultBlockConventions(),
                              None);

  case SILFunctionType::Representation::Thin:
  case SILFunctionType::Representation::ObjCMethod:
  case SILFunctionType::Representation::Thick:
  case SILFunctionType::Representation::Method:
  case SILFunctionType::Representation::WitnessMethod: {
    bool enableGuaranteedSelf = M.getOptions().EnableGuaranteedSelf;
    switch (kind) {
    case SILDeclRef::Kind::Initializer:
      return getSILFunctionType(M, origType, substType, substInterfaceType,
                  extInfo, DefaultInitializerConventions(enableGuaranteedSelf),
                                None);
    
    case SILDeclRef::Kind::Func:
    case SILDeclRef::Kind::Allocator:
    case SILDeclRef::Kind::Destroyer:
    case SILDeclRef::Kind::GlobalAccessor:
    case SILDeclRef::Kind::GlobalGetter:
    case SILDeclRef::Kind::DefaultArgGenerator:
    case SILDeclRef::Kind::IVarInitializer:
    case SILDeclRef::Kind::IVarDestroyer:
    case SILDeclRef::Kind::EnumElement:
      return getSILFunctionType(M, origType, substType, substInterfaceType,
                            extInfo, DefaultConventions(enableGuaranteedSelf),
                                None);
    case SILDeclRef::Kind::Deallocator:
      return getSILFunctionType(M, origType, substType, substInterfaceType,
                                extInfo, DeallocatorConventions(), None);
    }
  }
  }
}

CanSILFunctionType swift::getNativeSILFunctionType(SILModule &M,
                                       AbstractionPattern origType,
                                       CanAnyFunctionType substType,
                                       CanAnyFunctionType substInterfaceType) {
  AnyFunctionType::ExtInfo extInfo;

  // Preserve type information from the original type if possible.
  if (auto origFnType = origType.getAs<AnyFunctionType>()) {
    extInfo = origFnType->getExtInfo();

  // Otherwise, preserve function type attributes from the substituted type.
  } else {
    extInfo = substType->getExtInfo();
  }
  return ::getNativeSILFunctionType(M, origType, substType, substInterfaceType,
                                    extInfo, SILDeclRef::Kind::Func);
}

//===----------------------------------------------------------------------===//
//                          Foreign SILFunctionTypes
//===----------------------------------------------------------------------===//

/// Given nothing but a formal C parameter type that's passed
/// indirectly, deduce the convention for it.
///
/// Generally, whether the parameter is +1 is handled before this.
static ParameterConvention getIndirectCParameterConvention(clang::QualType type) {
  // Non-trivial C++ types are Indirect_Inout (at least in Itanium).
  llvm_unreachable("no address-only C types currently supported!");
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

/// Given nothing but a formal C result type, deduce the return
/// convention for it.
///
/// Generally, whether the result is +1 is handled before ending up here.
static ResultConvention getCResultConvention(clang::QualType type) {
  if (type->isObjCRetainableType())
    return ResultConvention::Autoreleased;
  return ResultConvention::Unowned;
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

    ResultConvention getResult(const TypeLowering &tl) const override {
      assert((!Method->hasAttr<clang::NSReturnsRetainedAttr>()
              || !Method->hasAttr<clang::ObjCReturnsInnerPointerAttr>())
             && "cannot be both returns_retained and returns_inner_pointer");
      if (Method->hasAttr<clang::NSReturnsRetainedAttr>() ||
          Method->hasAttr<clang::CFReturnsRetainedAttr>())
        return ResultConvention::Owned;

      if (Method->hasAttr<clang::CFReturnsNotRetainedAttr>())
        return ResultConvention::Autoreleased;

      // Make sure we apply special lifetime-extension behavior to
      // inner pointer results, unless we managed to import them with a
      // managed type.
      if (Method->hasAttr<clang::ObjCReturnsInnerPointerAttr>()) {
        if (tl.isTrivial())
          return ResultConvention::UnownedInnerPointer;
        else
          return ResultConvention::Autoreleased;
      }
      return getCResultConvention(Method->getReturnType());
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
      if (FnType->getExtInfo().getProducesResult())
        return ResultConvention::Owned;
      return getCResultConvention(FnType->getReturnType());
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

    static bool isCFTypedef(const TypeLowering &tl, clang::QualType type) {
      // If we imported a C pointer type as a non-trivial type, it was
      // a foreign class type.
      return !tl.isTrivial() && type->isPointerType();
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
}

/// Given that we have an imported Clang declaration, deduce the
/// ownership conventions for calling it and build the SILFunctionType.
static CanSILFunctionType
getSILFunctionTypeForClangDecl(SILModule &M, const clang::Decl *clangDecl,
                               CanAnyFunctionType origType,
                               CanAnyFunctionType substType,
                               CanAnyFunctionType substInterfaceType,
                               AnyFunctionType::ExtInfo extInfo,
                         const Optional<ForeignErrorConvention> &foreignError) {
  if (auto method = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
    auto origPattern = AbstractionPattern::getObjCMethod(origType, method);
    return getSILFunctionType(M, origPattern, substType, substInterfaceType,
                              extInfo, ObjCMethodConventions(method),
                              foreignError);
  }

  if (auto func = dyn_cast<clang::FunctionDecl>(clangDecl)) {
    AbstractionPattern origPattern(origType, func->getType().getTypePtr());
    return getSILFunctionType(M, origPattern, substType, substInterfaceType,
                              extInfo, CFunctionConventions(func),
                              foreignError);
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
}

/// Derive the ObjC selector family from an identifier.
static SelectorFamily getSelectorFamily(Identifier name) {
  StringRef text = name.get();
  while (!text.empty() && text[0] == '_') text = text.substr(1);

  /// Does the given selector start with the given string as a
  /// prefix, in the sense of the selector naming conventions?
  auto hasPrefix = [](StringRef text, StringRef prefix) {
    if (!text.startswith(prefix)) return false;
    if (text.size() == prefix.size()) return true;
    assert(text.size() > prefix.size());
    return !islower(text[prefix.size()]);
  };

  #define CHECK_PREFIX(LABEL, PREFIX) \
    if (hasPrefix(text, PREFIX)) return SelectorFamily::LABEL;
  FOREACH_FAMILY(CHECK_PREFIX)
  #undef CHECK_PREFIX

  return SelectorFamily::None;
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
    return SelectorFamily::None;
  }
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
}

static CanSILFunctionType
getSILFunctionTypeForSelectorFamily(SILModule &M, SelectorFamily family,
                                    CanAnyFunctionType origType,
                                    CanAnyFunctionType substType,
                                    CanAnyFunctionType substInterfaceType,
                                    AnyFunctionType::ExtInfo extInfo,
                     const Optional<ForeignErrorConvention> &foreignError) {
  return getSILFunctionType(M, AbstractionPattern(origType),
                            substType, substInterfaceType, extInfo,
                            SelectorFamilyConventions(family),
                            foreignError);
}

static CanSILFunctionType
getUncachedSILFunctionTypeForConstant(SILModule &M, SILDeclRef constant,
                                  CanAnyFunctionType origLoweredType,
                                  CanAnyFunctionType origLoweredInterfaceType,
                                  CanAnyFunctionType substFormalType,
                                  CanAnyFunctionType substInterfaceType) {
  assert(origLoweredType->getExtInfo().getSILRepresentation()
           != SILFunctionTypeRepresentation::Thick
         && origLoweredType->getExtInfo().getSILRepresentation()
             != SILFunctionTypeRepresentation::Block);

  auto extInfo = origLoweredType->getExtInfo();

  CanAnyFunctionType substLoweredType;
  CanAnyFunctionType substLoweredInterfaceType;
  if (substFormalType) {
    assert(substInterfaceType);
    substLoweredType = M.Types.getLoweredASTFunctionType(substFormalType,
                                                         constant.uncurryLevel,
                                                         extInfo,
                                                         constant);
    substLoweredInterfaceType
                    = M.Types.getLoweredASTFunctionType(substInterfaceType,
                                                        constant.uncurryLevel,
                                                        extInfo,
                                                        constant);
  } else {
    assert(!substInterfaceType);
    substLoweredType = origLoweredType;
    substLoweredInterfaceType = origLoweredInterfaceType;
  }

  if (!constant.isForeign) {
    return getNativeSILFunctionType(M, AbstractionPattern(origLoweredType),
                                    substLoweredType,
                                    substLoweredInterfaceType,
                                    extInfo,
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
                                            origLoweredType, substLoweredType,
                                            substLoweredInterfaceType,
                                            extInfo, foreignError);
  }

  // If the decl belongs to an ObjC method family, use that family's
  // ownership conventions.
  return getSILFunctionTypeForSelectorFamily(M, getSelectorFamily(constant),
                                             origLoweredType, substLoweredType,
                                             substLoweredInterfaceType,
                                             extInfo, foreignError);
}

static bool isClassOrProtocolMethod(ValueDecl *vd) {
  if (!vd->getDeclContext())
    return false;
  Type contextType = vd->getDeclContext()->getDeclaredTypeInContext();
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
  if (c.isForeign)
    return c.hasDecl() &&
      (isClassOrProtocolMethod(c.getDecl()) ||
       c.kind == SILDeclRef::Kind::IVarInitializer ||
       c.kind == SILDeclRef::Kind::IVarDestroyer)
      ? SILFunctionTypeRepresentation::ObjCMethod
      : SILFunctionTypeRepresentation::CFunctionPointer;

  // Anonymous functions currently always have Freestanding CC.
  if (!c.hasDecl())
    return SILFunctionTypeRepresentation::Thin;

  // FIXME: Assert that there is a native entry point
  // available. There's no great way to do this.

  // Protocol witnesses are called using the witness calling convention.
  if (auto proto = dyn_cast<ProtocolDecl>(c.getDecl()->getDeclContext()))
    return getProtocolWitnessRepresentation(proto);

  switch (c.kind) {
    case SILDeclRef::Kind::Func:
    case SILDeclRef::Kind::Allocator:
    case SILDeclRef::Kind::EnumElement:
    case SILDeclRef::Kind::Destroyer:
    case SILDeclRef::Kind::Deallocator:
    case SILDeclRef::Kind::GlobalAccessor:
    case SILDeclRef::Kind::GlobalGetter:
      if (c.getDecl()->isInstanceMember())
        return SILFunctionTypeRepresentation::Method;
      return SILFunctionTypeRepresentation::Thin;

    case SILDeclRef::Kind::DefaultArgGenerator:
      return SILFunctionTypeRepresentation::Thin;

    case SILDeclRef::Kind::Initializer:
    case SILDeclRef::Kind::IVarInitializer:
    case SILDeclRef::Kind::IVarDestroyer:
      return SILFunctionTypeRepresentation::Method;
  }
}

SILConstantInfo TypeConverter::getConstantInfo(SILDeclRef constant) {
  auto found = ConstantTypes.find(constant);
  if (found != ConstantTypes.end())
    return found->second;

  // First, get a function type for the constant.  This creates the
  // right type for a getter or setter.
  auto formalType = makeConstantType(constant, /*withCaptures*/ true);
  auto formalInterfaceType = makeConstantInterfaceType(constant,
                                                       /*withCaptures*/ true);
  GenericParamList *contextGenerics, *innerGenerics;
  std::tie(contextGenerics, innerGenerics)
    = getConstantContextGenericParams(constant, /*withCaptures*/true);

  // The formal type is just that with the right representation.
  auto rep = getDeclRefRepresentation(constant);
  formalType = adjustFunctionType(formalType, rep);
  formalInterfaceType = adjustFunctionType(formalInterfaceType, rep);
  // The lowered type is the formal type, but uncurried and with
  // parameters automatically turned into their bridged equivalents.
  auto loweredType =
    getLoweredASTFunctionType(formalType, constant.uncurryLevel, constant);
  auto loweredInterfaceType =
    getLoweredASTFunctionType(formalInterfaceType, constant.uncurryLevel,
                              constant);

  // The SIL type encodes conventions according to the original type.
  CanSILFunctionType silFnType =
    getUncachedSILFunctionTypeForConstant(M, constant, loweredType,
                                          loweredInterfaceType,
                                          CanAnyFunctionType(),
                                          CanAnyFunctionType());

  DEBUG(llvm::dbgs() << "lowering type for constant ";
        constant.print(llvm::dbgs());
        llvm::dbgs() << "\n  formal type: ";
        formalType.print(llvm::dbgs());
        llvm::dbgs() << "\n  lowered AST type: ";
        loweredType.print(llvm::dbgs());
        llvm::dbgs() << "\n  SIL type: ";
        silFnType.print(llvm::dbgs());
        llvm::dbgs() << "\n");

  SILConstantInfo result = {
    formalType, formalInterfaceType,
    loweredType, loweredInterfaceType,
    silFnType,
    contextGenerics,
    innerGenerics,
  };
  ConstantTypes[constant] = result;
  return result;
}


namespace {
  class SILFunctionTypeSubstituter {
    TypeConverter &TC;
    CanSILFunctionType OrigFnType;
    ArrayRef<SILParameterInfo> OrigParams;
    SmallVector<SILParameterInfo, 8> SubstParams;

  public:
    SILFunctionTypeSubstituter(TypeConverter &TC,
                               CanSILFunctionType origFnType)
      : TC(TC), OrigFnType(origFnType),
        OrigParams(origFnType->getParameters())
    {}

    SILResultInfo substResult(AbstractionPattern origResultType,
                              CanType substResultType) {
      SILResultInfo origResult = OrigFnType->getResult();
      bool origHasIndirectResult = OrigFnType->hasIndirectResult();
      bool resultIsDependent;

      // Claim the implicit indirect result parameter.
      SILParameterInfo origIndirectResult;
      if (origHasIndirectResult) {
        origIndirectResult = claimNextOrigParam();
        resultIsDependent = origIndirectResult.getType()->isDependentType();
        assert(origIndirectResult.isIndirectResult());
      } else {
        resultIsDependent = origResult.getType()->isDependentType();
      }

      // If the result type didn't change and doesn't depend on context, we can
      // just use the original result.
      if (origResultType.isExactType(substResultType)
          && !resultIsDependent) {
        // That includes the implicit indirect result parameter.
        if (origHasIndirectResult)
          SubstParams.push_back(origIndirectResult);
        return origResult;
      }

      // Otherwise, we'll need to SIL-lower the substituted result
      // using the abstraction patterns of the original result.
      auto &substResultTL = TC.getTypeLowering(origResultType, substResultType);
      auto loweredResultTy =substResultTL.getLoweredType().getSwiftRValueType();

      // If the original result was returned indirectly, we need to
      // preserve that.
      if (origHasIndirectResult) {
        // We intentionally ignore origIndirectResult here.
        addSubstParam(loweredResultTy, ParameterConvention::Indirect_Out);

        assert(origResult == SILResultInfo(TupleType::getEmpty(TC.Context),
                                           ResultConvention::Unowned));
        return origResult;
      }

      assert(!substResultTL.isReturnedIndirectly() &&
             "substitution yielded an indirect result when the "
             "original function type didn't have one!");

      // Otherwise, return the new type with the old conventions.
      return SILResultInfo(loweredResultTy, origResult.getConvention());
    }

    ArrayRef<SILParameterInfo> getSubstParams() const {
      assert(OrigParams.empty() && "didn't claim all parameters?!");
      return SubstParams;
    }

    void substInputs(AbstractionPattern origType, CanType substType) {
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
      assert(!origParam.isIndirectResult());

      // If the type hasn't changed and doesn't rely on context, just use the
      // original parameter.
      if (origType.isExactType(substType) &&
          !origParam.getType()->isDependentType()) {
        SubstParams.push_back(origParam);
        return;
      }

      // Otherwise, lower the substituted type using the abstraction
      // patterns of the original.
      auto &substTL = TC.getTypeLowering(origType, substType);
      auto substConvention = getSubstConvention(origParam.getConvention(),
                                                substTL.isTrivial());
      assert(isIndirectParameter(substConvention) ||
             !substTL.isPassedIndirectly());
      addSubstParam(substTL.getLoweredType().getSwiftRValueType(),
                    substConvention);
    }

  private:
    SILParameterInfo claimNextOrigParam() {
      assert(!OrigParams.empty());
      auto temp = OrigParams[0];
      OrigParams = OrigParams.slice(1);
      return temp;
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
      case ParameterConvention::Direct_Deallocating:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed:
        return orig;

      case ParameterConvention::Indirect_Out:
        llvm_unreachable("should've been filtered out before");
      }
      llvm_unreachable("bad parameter convention");
    }
  };
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
                                 CanAnyFunctionType substLoweredType,
                                 CanAnyFunctionType substLoweredInterfaceType) {
  if (origLoweredType == substLoweredType)
    return origFnType;

  // Use the generic parameters from the substituted type.
  GenericParamList *genericParams = nullptr;
  GenericSignature *genericSig = nullptr;
  if (auto polySubstFn = dyn_cast<PolymorphicFunctionType>(substLoweredType))
    genericParams = &polySubstFn->getGenericParams();
  if (auto genSubstFn = dyn_cast<GenericFunctionType>(substLoweredInterfaceType))
    genericSig = GenericSignature::get(genSubstFn->getGenericParams(),
                                       genSubstFn->getRequirements());
  assert(bool(genericParams) == bool(genericSig));

  GenericContextScope scope(*this, genericSig);
  SILFunctionTypeSubstituter substituter(*this, origFnType);

  // Map the result.
  SILResultInfo substResult =
    substituter.substResult(AbstractionPattern(origLoweredType.getResult()),
                            substLoweredInterfaceType.getResult());

  // Map the error result.  Currently this is never dependent.
  Optional<SILResultInfo> substErrorResult
    = origFnType->getOptionalErrorResult();
  assert(!substErrorResult ||
         (!substErrorResult->getType()->isDependentType() &&
          !substErrorResult->getType()->hasArchetype()));

  // Map the inputs.
  substituter.substInputs(AbstractionPattern(origLoweredType.getInput()),
                          substLoweredInterfaceType.getInput());

  // Allow the substituted type to add thick-ness, but not remove it.
  assert(!origFnType->getExtInfo().hasContext()
           || substLoweredType->getExtInfo().hasContext());
  assert(substLoweredType->getExtInfo().getSILRepresentation()
           == substLoweredInterfaceType->getExtInfo().getSILRepresentation());

  auto rep = substLoweredType->getExtInfo().getSILRepresentation();
  auto extInfo = origFnType->getExtInfo().withRepresentation(rep);

  // FIXME: Map into archetype context.
  return SILFunctionType::get(genericSig,
                              extInfo,
                              origFnType->getCalleeConvention(),
                              substituter.getSubstParams(),
                              substResult,
                              substErrorResult,
                              Context);
}

namespace {
  /// Given a lowered SIL type, apply a substitution to it to produce another
  /// lowered SIL type which uses the same abstraction conventions.
  class SILTypeSubstituter :
      public CanTypeVisitor<SILTypeSubstituter, CanType> {
    SILModule &TheSILModule;
    Module *TheASTModule;
    TypeSubstitutionMap &Subs;

    ASTContext &getASTContext() { return TheSILModule.getASTContext(); }
  public:
    SILTypeSubstituter(SILModule &silModule, Module *astModule,
                       TypeSubstitutionMap &subs)
      : TheSILModule(silModule), TheASTModule(astModule), Subs(subs)
    {}

    // SIL type lowering only does special things to tuples and functions.

    /// Functions need to preserve their abstraction structure.
    CanSILFunctionType visitSILFunctionType(CanSILFunctionType origType,
                                            bool dropGenerics = false)
    {
      GenericContextScope scope(TheSILModule.Types,
                                origType->getGenericSignature());

      SILResultInfo substResult = subst(origType->getResult());

      auto substErrorResult = origType->getOptionalErrorResult();
      assert(!substErrorResult ||
             (!substErrorResult->getType()->isDependentType() &&
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
                                  substParams, substResult,
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

    /// Any other type is would be a valid type in the AST.  Just
    /// apply the substitution on the AST level and then lower that.
    CanType visitType(CanType origType) {
      assert(!isa<AnyFunctionType>(origType));
      assert(!isa<LValueType>(origType) && !isa<InOutType>(origType));

      assert(TheSILModule.Types.getLoweredType(origType).getSwiftRValueType()
               == origType);

      CanType substType =
        origType.subst(TheASTModule, Subs, true, nullptr)->getCanonicalType();

      // If the substitution didn't change anything, we know that the
      // original type was a lowered type, so we're good.
      if (origType == substType) {
        return origType;
      }

      return TheSILModule.Types.getLoweredType(AbstractionPattern(origType),
                                               substType)
               .getSwiftRValueType();
    }
  };
}

SILType SILType::substType(SILModule &silModule, Module *astModule,
                           TypeSubstitutionMap &subs, SILType SrcTy) {
  return SrcTy.subst(silModule, astModule, subs);
}

SILType SILType::subst(SILModule &silModule, Module *astModule,
                       TypeSubstitutionMap &subs) const {
  SILTypeSubstituter STST(silModule, astModule, subs);
  return STST.subst(*this);
}

CanSILFunctionType SILType::substFuncType(SILModule &silModule,
                                          Module *astModule,
                                          TypeSubstitutionMap &subs,
                                          CanSILFunctionType SrcTy,
                                          bool dropGenerics) {
  SILTypeSubstituter STST(silModule, astModule, subs);
  return STST.visitSILFunctionType(SrcTy, dropGenerics);
}

/// Apply a substitution to this polymorphic SILFunctionType so that
/// it has the form of the normal SILFunctionType for the substituted
/// type, except using the original conventions.
CanSILFunctionType
SILFunctionType::substGenericArgs(SILModule &silModule,
                                           Module *astModule,
                                           ArrayRef<Substitution> subs) {
  if (subs.empty()) {
    assert(!isPolymorphic() && "no args for polymorphic substitution");
    return CanSILFunctionType(this);
  }

  assert(isPolymorphic());
  TypeSubstitutionMap map = GenericSig->getSubstitutionMap(subs);
  SILTypeSubstituter substituter(silModule, astModule, map);

  return substituter.visitSILFunctionType(CanSILFunctionType(this),
                                          /*dropGenerics*/ true);
}
