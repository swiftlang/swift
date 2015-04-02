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
#include "swift/AST/SubstTypeVisitor.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/Analysis/DomainSpecific/CocoaConventions.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
using namespace swift;
using namespace swift::Lowering;

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

    virtual ParameterConvention getIndirectParameter(unsigned index,
                                                     CanType type) const = 0;
    virtual ParameterConvention getDirectParameter(unsigned index,
                                                   CanType type) const = 0;
    virtual ParameterConvention getCallee() const = 0;
    virtual ResultConvention getResult(const TypeLowering &resultTL) const = 0;
    virtual ParameterConvention
    getIndirectSelfParameter(CanType type) const = 0;
    virtual ParameterConvention getDirectSelfParameter(CanType type) const = 0;
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
  class DestructureInputs : public SubstTypeVisitor<DestructureInputs> {
    SILModule &M;
    const Conventions &Convs;
    SmallVectorImpl<SILParameterInfo> &Inputs;
    unsigned NextOrigParamIndex = 0;
  public:
    DestructureInputs(SILModule &M, const Conventions &conventions,
                      SmallVectorImpl<SILParameterInfo> &inputs)
      : M(M), Convs(conventions), Inputs(inputs) {}

    /// Expand tuples.
    void visitTupleType(CanTupleType origType, CanTupleType substType) {
      assert(origType->getNumElements() == substType->getNumElements());
      for (auto i : indices(substType.getElementTypes())) {
        visit(origType.getElementType(i), substType.getElementType(i));
      }
    }

    /// Query whether the original type is address-only given complete
    /// lowering information about its substitution.
    bool isPassedIndirectly(CanType origType, CanType substType,
                            const TypeLowering &substTL) {
      // If the substituted type is passed indirectly, so must the
      // unsubstituted type.
      if (substTL.isPassedIndirectly()) {
        return true;

      // If the substitution didn't change the type, then a negative
      // response to the above is determinative as well.
      } else if (origType == substType) {
        return false;

      // Otherwise, query specifically for the original type.
      } else {
        return SILType::isPassedIndirectly(origType, M);
      }
    }

    void visitType(CanType origType, CanType substType) {
      unsigned origParamIndex = NextOrigParamIndex++;

      auto &substTL =
        M.Types.getTypeLowering(AbstractionPattern(origType), substType);
      ParameterConvention convention;
      if (isa<InOutType>(origType)) {
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
      Inputs.push_back(SILParameterInfo(loweredType, convention));
    }

    void visitSelfType(CanType origType, CanType substType, AbstractCC CC) {
      auto &substTL =
        M.Types.getTypeLowering(AbstractionPattern(origType), substType);
      ParameterConvention convention;
      if (isa<InOutType>(origType)) {
        convention = ParameterConvention::Indirect_Inout;
      } else if (isPassedIndirectly(origType, substType, substTL)) {
        if (CC == AbstractCC::WitnessMethod)
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
      Inputs.push_back(SILParameterInfo(loweredType, convention));
    }

    /// This is a special entry point that allows destructure inputs to handle
    /// self correctly.
    void visitTopLevelType(CanType origType, CanType substType,
                           AnyFunctionType::ExtInfo extInfo) {
      bool hasSelfParam = extInfo.hasSelfParam();
      // If we don't have a tuple type, then we have a one element argument.
      if (!isa<TupleType>(origType)) {       
        if (hasSelfParam) {
          visitSelfType(origType, substType, extInfo.getCC());
          return;
        }
        
        visit(origType, substType);
        return;
      }

      CanTupleType origTupleType = dyn_cast<TupleType>(origType);
      CanTupleType substTupleType = dyn_cast<TupleType>(substType);

      unsigned numEltTypes = substTupleType.getElementTypes().size();
      if (numEltTypes == 0)
        return;

      // Handle all types except for the self param if we have a self parameter.
      unsigned numNonSelfParams = numEltTypes - unsigned(hasSelfParam);
      for (unsigned i = 0; i != numNonSelfParams; ++i) {
        visit(origTupleType.getElementType(i),
              substTupleType.getElementType(i));
      }

      // If we don't have a self param, exit now.
      if (!hasSelfParam)
        return;

      // Otherwise, process the self parameter.
      visitSelfType(origTupleType.getElementType(numNonSelfParams),
                    substTupleType.getElementType(numNonSelfParams),
                    extInfo.getCC());
    }
  };

  /// A visitor for generating the most general possible input types for
  /// the given function signature.
  ///
  /// That is, passing the function's inputs as if the original type
  /// were the most general function signature (expressed entirely in
  /// type variables) which can be substituted to equal the given
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
  class DestructureGeneralizedInputs
      : public CanTypeVisitor<DestructureGeneralizedInputs> {
    SILModule &M;
    AbstractionPattern OrigType;
    SmallVectorImpl<SILParameterInfo> &Inputs;
  public:
    DestructureGeneralizedInputs(SILModule &M,
                                 AbstractionPattern origType,
                                 SmallVectorImpl<SILParameterInfo> &inputs)
      : M(M), OrigType(origType), Inputs(inputs) {
      assert(origType.isOpaque());
    }

    void visitTupleType(CanTupleType type) {
      // If a tuple type is not materializable -- if it contains an l-value
      // type -- then it's not a valid target for substitution and we should
      // destructure it.
      if (!type->isMaterializable()) {
        for (auto elt : type.getElementTypes())
          visit(elt);
        return;
      }

      // Otherwise, it's a valid target for substitution, so the most
      // general form is to pass it indirectly.
      addInput(type, ParameterConvention::Indirect_In);
    }

    void visitInOutType(CanInOutType type) {
      // inout types aren't valid targets for substitution.
      addInput(type.getObjectType(), ParameterConvention::Indirect_Inout);
    }

    void visitType(CanType type) {
      // Every other type is materializable.
      assert(type->isMaterializable());
      addInput(type, ParameterConvention::Indirect_In);
    }

    void addInput(CanType unlowered, ParameterConvention convention) {
      auto lowered = M.Types.getLoweredType(OrigType, unlowered);
      Inputs.push_back(SILParameterInfo(lowered.getSwiftRValueType(),
                                        convention));
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
                                             CanType origType,
                                             CanAnyFunctionType substFnOldType,
                                             CanAnyFunctionType substFnInterfaceType,
                                             AnyFunctionType::ExtInfo extInfo,
                                             const Conventions &conventions) {
  SmallVector<SILParameterInfo, 8> inputs;

  // If the original type is dependent, we'll want the most general type.
  auto origFnType = dyn_cast<AnyFunctionType>(origType);
  assert(origFnType
         || isa<SubstitutableType>(origType)
         || isa<DependentMemberType>(origType));

  // Per above, use reabstraction only on thick or polymorphic functions.
  if (substFnOldType->getRepresentation()
        != AnyFunctionType::Representation::Thick &&
      (!origFnType || isa<FunctionType>(origFnType))) {
    origFnType = substFnOldType;
  }

  // Get an abstraction pattern to apply against the result.
  // If the unsubstituted type is dependent, use the most general type for
  // the result; otherwise, match the original abstraction level.
  AbstractionPattern origResultPattern = origFnType
    ? AbstractionPattern(origFnType.getResult())
    : AbstractionPattern(origType);

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

  CanType substFormalResultType = substFnInterfaceType.getResult();
  auto &substResultTL = M.Types.getTypeLowering(origResultPattern,
                                                substFormalResultType);
  bool hasIndirectResult;

  // If the substituted result type is returned indirectly, then the
  // original type must be as well.
  if (substResultTL.isReturnedIndirectly()) {
    hasIndirectResult = true;

  // If the unsubstituted type is dependent, then we use the most
  // general type for the function, which involves an indirect result.
  } else if (!origFnType) {
    hasIndirectResult = true;

  // If the substitution didn't change the result type, we can use the
  // lowering that we already fetched.
  } else if (origFnType.getResult() == substFormalResultType) {
    assert(!substResultTL.isReturnedIndirectly());
    hasIndirectResult = false;

  // Otherwise, ask whether the original result type was address-only.
  } else {
    hasIndirectResult = SILType::isReturnedIndirectly(origFnType.getResult(), M);
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

  // TODO: map native 'throws' to an error result type.
  Optional<SILResultInfo> errorResult;

  // Destructure the input tuple type.
  if (origFnType) {
    DestructureInputs InputDestructurer(M, conventions, inputs);
    InputDestructurer.visitTopLevelType(origFnType.getInput(),
                                        substFnInterfaceType.getInput(),
                                        extInfo);
  } else {
    DestructureGeneralizedInputs InputDestructurer(M,
                                                   AbstractionPattern(origType),
                                                   inputs);
    InputDestructurer.visit(substFnInterfaceType.getInput());
  }

  auto calleeConvention = ParameterConvention::Direct_Unowned;
  if (extInfo.hasContext())
    calleeConvention = conventions.getCallee();

  // Always strip the auto-closure and no-escape bit.
  // TODO: The noescape bit could be of interesting to SIL optimizations.
  //   We should bring it back when we have those optimizations.
  extInfo = extInfo.withIsAutoClosure(false)
                   .withNoEscape(false);

  return SILFunctionType::get(genericSig,
                              extInfo, calleeConvention,
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
                                           CanType type) const override {
    llvm_unreachable("Dealloactors do not have indirect parameters");
  }

  ParameterConvention getDirectParameter(unsigned index,
                                         CanType type) const override {
    llvm_unreachable("Deallocators do not have non-self direct parameters");
  }

  ParameterConvention getCallee() const override {
    llvm_unreachable("Deallocators do not have callees");
  }

  ResultConvention getResult(const TypeLowering &tl) const override {
    // TODO: Put an unreachable here?
    return ResultConvention::Owned;
  }

  ParameterConvention getDirectSelfParameter(CanType type) const override {
    // TODO: Investigate whether or not it is
    return ParameterConvention::Direct_Owned;
  }

  ParameterConvention getIndirectSelfParameter(CanType type) const override {
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
                                             CanType type) const override {
      return ParameterConvention::Indirect_In;
    }

    ParameterConvention getDirectParameter(unsigned index,
                                           CanType type) const override {
      return ParameterConvention::Direct_Owned;
    }

    ParameterConvention getCallee() const override {
      return DefaultThickCalleeConvention;
    }

    ResultConvention getResult(const TypeLowering &tl) const override {
      return ResultConvention::Owned;
    }

    ParameterConvention getDirectSelfParameter(CanType type) const override {
      if (hasGuaranteedSelf)
        return ParameterConvention::Direct_Guaranteed;
      return ParameterConvention::Direct_Owned;
    }

    ParameterConvention getIndirectSelfParameter(CanType type) const override {
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
    ParameterConvention getDirectSelfParameter(CanType type) const override {
      return ParameterConvention::Direct_Owned;
    }
    
    ParameterConvention getIndirectSelfParameter(CanType type) const override {
      return ParameterConvention::Indirect_In;
    }
  };

  /// The default conventions for ObjC blocks.
  struct DefaultBlockConventions : Conventions {
    DefaultBlockConventions() : Conventions(ConventionsKind::DefaultBlock) {}

    ParameterConvention getIndirectParameter(unsigned index,
                                             CanType type) const override {
      llvm_unreachable("indirect block parameters unsupported");
    }

    ParameterConvention getDirectParameter(unsigned index,
                                           CanType type) const override {
      return ParameterConvention::Direct_Unowned;
    }

    ParameterConvention getCallee() const override {
      return ParameterConvention::Direct_Unowned;
    }

    ResultConvention getResult(const TypeLowering &tl) const override {
      return ResultConvention::Autoreleased;
    }

    ParameterConvention getDirectSelfParameter(CanType type) const override {
      llvm_unreachable("objc blocks do not have a self parameter");
    }

    ParameterConvention getIndirectSelfParameter(CanType type) const override {
      llvm_unreachable("objc blocks do not have a self parameter");
    }

    static bool classof(const Conventions *C) {
      return C->getKind() == ConventionsKind::DefaultBlock;
    }
  };
}

static CanSILFunctionType getNativeSILFunctionType(SILModule &M,
                                           CanType origType,
                                           CanAnyFunctionType substType,
                                           CanAnyFunctionType substInterfaceType,
                                           AnyFunctionType::ExtInfo extInfo,
                                           SILDeclRef::Kind kind) {
  switch (extInfo.getRepresentation()) {
  case AnyFunctionType::Representation::Block:
    return getSILFunctionType(M, origType, substType, substInterfaceType,
                              extInfo, DefaultBlockConventions());

  case AnyFunctionType::Representation::Thin:
  case AnyFunctionType::Representation::Thick: {
    // TODO: Proper representation for C function pointer types.
    if (extInfo.getCC() == AbstractCC::C)
      return getSILFunctionType(M, origType, substType, substInterfaceType,
                                extInfo, DefaultBlockConventions());
    
  
    bool enableGuaranteedSelf = M.getOptions().EnableGuaranteedSelf;
    switch (kind) {
    case SILDeclRef::Kind::Initializer:
      return getSILFunctionType(M, origType, substType, substInterfaceType,
                  extInfo, DefaultInitializerConventions(enableGuaranteedSelf));
    
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
                            extInfo, DefaultConventions(enableGuaranteedSelf));
    case SILDeclRef::Kind::Deallocator:
      return getSILFunctionType(M, origType, substType, substInterfaceType,
                                extInfo, DeallocatorConventions());
    }
  }
  }
}

CanSILFunctionType swift::getNativeSILFunctionType(SILModule &M,
                                       CanType origType,
                                       CanAnyFunctionType substType,
                                       CanAnyFunctionType substInterfaceType) {
  AnyFunctionType::ExtInfo extInfo;

  // Preserve type information from the original type if possible.
  if (auto origFnType = dyn_cast<AnyFunctionType>(origType)) {
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
                                             CanType type) const override {
      return getIndirectCParameterConvention(Method->param_begin()[index]);
    }

    ParameterConvention getDirectParameter(unsigned index,
                                           CanType type) const override {
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

    ParameterConvention getDirectSelfParameter(CanType type) const override {
      if (Method->hasAttr<clang::NSConsumesSelfAttr>())
        return ParameterConvention::Direct_Owned;

      // The caller is supposed to take responsibility for ensuring
      // that 'self' survives a method call.
      return ObjCSelfConvention;
    }

    ParameterConvention getIndirectSelfParameter(CanType type) const override {
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
                                             CanType type) const override {
      return getIndirectCParameterConvention(getParamType(index));
    }

    ParameterConvention getDirectParameter(unsigned index,
                                           CanType type) const override {
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

    ParameterConvention getDirectSelfParameter(CanType type) const override {
      llvm_unreachable("c function types do not have a self parameter");
    }

    ParameterConvention getIndirectSelfParameter(CanType type) const override {
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
                                           CanType type) const override {
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
                               AnyFunctionType::ExtInfo extInfo) {
  if (auto method = dyn_cast<clang::ObjCMethodDecl>(clangDecl))
    return getSILFunctionType(M, origType, substType, substInterfaceType,
                              extInfo,
                              ObjCMethodConventions(method));

  if (auto func = dyn_cast<clang::FunctionDecl>(clangDecl)) {
    return getSILFunctionType(M, origType, substType, substInterfaceType,
                              extInfo, CFunctionConventions(func));
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
                                             CanType type) const override {
      return ParameterConvention::Indirect_In;
    }

    ParameterConvention getDirectParameter(unsigned index,
                                           CanType type) const override {
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

    ParameterConvention getDirectSelfParameter(CanType type) const override {
      if (Family == SelectorFamily::Init)
        return ParameterConvention::Direct_Owned;
      return ObjCSelfConvention;
    }

    ParameterConvention getIndirectSelfParameter(CanType type) const override {
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
                                    AnyFunctionType::ExtInfo extInfo) {
  return getSILFunctionType(M, origType, substType, substInterfaceType, extInfo,
                            SelectorFamilyConventions(family));
}

static CanSILFunctionType
getUncachedSILFunctionTypeForConstant(SILModule &M, SILDeclRef constant,
                                  CanAnyFunctionType origLoweredType,
                                  CanAnyFunctionType origLoweredInterfaceType,
                                  CanAnyFunctionType substFormalType,
                                  CanAnyFunctionType substInterfaceType,
                                  AnyFunctionType::Representation rep) {
  assert(origLoweredType->getRepresentation()
           == FunctionType::Representation::Thin);

  auto extInfo = origLoweredType->getExtInfo().withRepresentation(rep);

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
    return getNativeSILFunctionType(M, origLoweredType, substLoweredType,
                                    substLoweredInterfaceType,
                                    extInfo,
                                    constant.kind);
  }

  // If we have a clang decl associated with the Swift decl, derive its
  // ownership conventions.
  if (constant.hasDecl()) {
    auto decl = constant.getDecl();

    if (auto clangDecl = findClangMethod(decl))
      return getSILFunctionTypeForClangDecl(M, clangDecl,
                                            origLoweredType, substLoweredType,
                                            substLoweredInterfaceType,
                                            extInfo);
  }

  // If the decl belongs to an ObjC method family, use that family's
  // ownership conventions.
  return getSILFunctionTypeForSelectorFamily(M, getSelectorFamily(constant),
                                             origLoweredType, substLoweredType,
                                             substLoweredInterfaceType,
                                             extInfo);
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

AbstractCC TypeConverter::getAbstractCC(SILDeclRef c) {
  // Currying thunks always have freestanding CC.
  if (c.isCurried)
    return AbstractCC::Freestanding;

  // If this is a foreign thunk, it always has the foreign calling convention.
  if (c.isForeign)
    return c.hasDecl() &&
      (isClassOrProtocolMethod(c.getDecl()) ||
       c.kind == SILDeclRef::Kind::IVarInitializer ||
       c.kind == SILDeclRef::Kind::IVarDestroyer)
      ? AbstractCC::ObjCMethod
      : AbstractCC::C;

  // Anonymous functions currently always have Freestanding CC.
  if (!c.hasDecl())
    return AbstractCC::Freestanding;

  // FIXME: Assert that there is a native entry point
  // available. There's no great way to do this.

  // Protocol witnesses are called using the witness calling convention.
  if (auto proto = dyn_cast<ProtocolDecl>(c.getDecl()->getDeclContext()))
    return getProtocolWitnessCC(proto);

  switch (c.kind) {
    case SILDeclRef::Kind::Func:
    case SILDeclRef::Kind::Allocator:
    case SILDeclRef::Kind::EnumElement:
    case SILDeclRef::Kind::Destroyer:
    case SILDeclRef::Kind::Deallocator:
    case SILDeclRef::Kind::GlobalAccessor:
    case SILDeclRef::Kind::GlobalGetter:
      if (c.getDecl()->isInstanceMember())
        return AbstractCC::Method;
      return AbstractCC::Freestanding;

    case SILDeclRef::Kind::DefaultArgGenerator:
      return AbstractCC::Freestanding;

    case SILDeclRef::Kind::Initializer:
    case SILDeclRef::Kind::IVarInitializer:
    case SILDeclRef::Kind::IVarDestroyer:
      return AbstractCC::Method;
  }
}

CanSILFunctionType TypeConverter::getConstantFunctionType(SILDeclRef constant,
                                   CanAnyFunctionType substFormalType,
                                   CanAnyFunctionType substFormalInterfaceType,
                                   AnyFunctionType::Representation rep) {
  auto cachedInfo = getConstantInfo(constant);
  if (!substFormalType || substFormalType == cachedInfo.FormalType) {
    assert(bool(substFormalType) == bool(substFormalInterfaceType)
           && (!substFormalInterfaceType
               || substFormalInterfaceType == cachedInfo.FormalInterfaceType));
    auto extInfo = cachedInfo.SILFnType->getExtInfo()
      .withRepresentation(FunctionType::Representation::Thin);
    return adjustFunctionType(cachedInfo.SILFnType, extInfo);
  }

  return getUncachedSILFunctionTypeForConstant(M, constant,
                                               cachedInfo.LoweredType,
                                               cachedInfo.LoweredInterfaceType,
                                               substFormalType,
                                               substFormalInterfaceType,
                                               rep);
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

  // The formal type is just that with the right CC and thin-ness.
  AbstractCC cc = getAbstractCC(constant);
  formalType = adjustFunctionType(formalType, cc,
                                  AnyFunctionType::Representation::Thin);
  formalInterfaceType = adjustFunctionType(formalInterfaceType, cc,
                                         AnyFunctionType::Representation::Thin);
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
                                          CanAnyFunctionType(),
                                          FunctionType::Representation::Thin);

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

static CanType getBridgedInputType(TypeConverter &tc,
                                   AbstractCC cc,
                                   CanType input,
                                   const clang::Decl *clangDecl) {

  auto getClangParamType = [&](unsigned i) -> const clang::Type * {
    if (!clangDecl)
      return nullptr;
    if (auto methodDecl = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
      if (i < methodDecl->param_size())
        return methodDecl->param_begin()[i]->getType()
          ->getUnqualifiedDesugaredType();
      return nullptr;
    } else if (auto fnTy = dyn_cast_or_null<clang::FunctionProtoType>
                 (clangDecl->getFunctionType())) {
      return fnTy->getParamType(i)->getUnqualifiedDesugaredType();
    } else {
      return nullptr;
    }
  };

  if (auto tuple = dyn_cast<TupleType>(input)) {
    SmallVector<TupleTypeElt, 4> bridgedFields;
    bool changed = false;

    for (unsigned i : indices(tuple->getElements())) {
      auto &elt = tuple->getElement(i);

      auto clangInputTy = getClangParamType(i);

      Type bridged = tc.getLoweredBridgedType(elt.getType(), cc, clangInputTy,
                                              TypeConverter::ForArgument);
      if (!bridged) {
        tc.Context.Diags.diagnose(SourceLoc(), diag::could_not_find_bridge_type,
                                  elt.getType());

        llvm::report_fatal_error("unable to set up the ObjC bridge!");
      }

      CanType canBridged = bridged->getCanonicalType();
      if (canBridged != CanType(elt.getType())) {
        changed = true;
        bridgedFields.push_back(elt.getWithType(canBridged));
      } else {
        bridgedFields.push_back(elt);
      }
    }

    if (!changed)
      return input;
    return CanType(TupleType::get(bridgedFields, input->getASTContext()));
  }

  auto clangInputTy = getClangParamType(0);
  auto loweredBridgedType = tc.getLoweredBridgedType(input, cc, clangInputTy,
                                                    TypeConverter::ForArgument);

  if (!loweredBridgedType) {
    tc.Context.Diags.diagnose(SourceLoc(), diag::could_not_find_bridge_type,
                              input);

    llvm::report_fatal_error("unable to set up the ObjC bridge!");
  }

  return loweredBridgedType->getCanonicalType();
}

/// Bridge a result type.
static CanType getBridgedResultType(TypeConverter &tc,
                                    AbstractCC cc,
                                    CanType result,
                                    const clang::Decl *clangDecl) {
  const clang::Type *clangResultTy = nullptr;
  if (clangDecl) {
    if (auto methodDecl = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
      clangResultTy = methodDecl->getReturnType()->getUnqualifiedDesugaredType();
    } else if (auto fnTy = clangDecl->getFunctionType()) {
      clangResultTy = fnTy->getReturnType()->getUnqualifiedDesugaredType();
    }
  }

  auto loweredType = tc.getLoweredBridgedType(result, cc, clangResultTy,
                                              TypeConverter::ForResult);

  if (!loweredType) {
    tc.Context.Diags.diagnose(SourceLoc(), diag::could_not_find_bridge_type,
                              result);

    llvm::report_fatal_error("unable to set up the ObjC bridge!");
  }

  return loweredType->getCanonicalType();
}

/// Fast path for bridging types in a function type without uncurrying.
static CanAnyFunctionType getBridgedFunctionType(TypeConverter &tc,
                                            CanAnyFunctionType t,
                                            AnyFunctionType::ExtInfo extInfo,
                                            const clang::Decl *decl) {
  // C functions are always thin.
  // FIXME: Should have an AST-level Representation enumeration for
  // "C function pointer".
  AbstractCC effectiveCC = t->getAbstractCC();
  if (effectiveCC == AbstractCC::C
      && t->getRepresentation() != FunctionType::Representation::Block) {
    extInfo = extInfo.withRepresentation(FunctionType::Representation::Thin);
  }
  
  // Blocks are always cdecl.
  if (t->getRepresentation() == FunctionType::Representation::Block) {
    effectiveCC = AbstractCC::C;
    extInfo = extInfo.withCallingConv(AbstractCC::C);
  }

  // Pull out the generic signature.
  CanGenericSignature genericSig;
  if (auto gft = dyn_cast<GenericFunctionType>(t)) {
    genericSig = gft.getGenericSignature();
  }

  // Pull the innermost generic parameter list in the type out.
  Optional<GenericParamList *> genericParams;
  {
    CanAnyFunctionType innerTy = t;
    while (innerTy) {
      if (auto pft = dyn_cast<PolymorphicFunctionType>(innerTy)) {
        assert(!genericParams
           || pft->getGenericParams().getOuterParameters() == *genericParams);
        genericParams = &pft->getGenericParams();
      }
      innerTy = dyn_cast<AnyFunctionType>(innerTy.getResult());
    }
  }
  GenericParamList *innerGenericParams
    = genericParams ? *genericParams : nullptr;

  auto rebuild = [&](CanType input, CanType result) -> CanAnyFunctionType {
    if (genericParams) {
      assert(!genericSig && "got mix of poly/generic function type?!");
      return CanPolymorphicFunctionType::get(input, result,
                                             innerGenericParams,
                                             extInfo);
    } else if (genericSig) {
      return CanGenericFunctionType::get(genericSig, input, result, extInfo);
    } else {
      return CanFunctionType::get(input, result, extInfo);
    }
  };

  switch (effectiveCC) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    // No bridging needed for native functions.
    if (t->getExtInfo() == extInfo && !innerGenericParams)
      return t;
    return rebuild(t.getInput(), t.getResult());

  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    // FIXME: Should consider the originating Clang types of e.g. blocks.
    return rebuild(getBridgedInputType(tc, effectiveCC, t.getInput(), decl),
                   getBridgedResultType(tc, effectiveCC, t.getResult(), decl));
  }
  llvm_unreachable("bad calling convention");
}

CanAnyFunctionType
TypeConverter::getLoweredASTFunctionType(CanAnyFunctionType t,
                                         unsigned uncurryLevel,
                                         AnyFunctionType::ExtInfo extInfo,
                                         Optional<SILDeclRef> constant) {
  // Get the original Clang type of a node, if we have one.
  const clang::Decl *clangDecl = nullptr;
  if (constant && constant->hasDecl())
    clangDecl = constant->getDecl()->getClangDecl();

  // Fast path: no uncurrying required.
  if (uncurryLevel == 0)
    return getBridgedFunctionType(*this, t, extInfo, clangDecl);

  AbstractCC cc = extInfo.getCC();
  assert(!extInfo.isAutoClosure() && "autoclosures cannot be curried");
  assert(extInfo.getRepresentation() != FunctionType::Representation::Block
         && "objc blocks cannot be curried");

  // The uncurried input types.
  SmallVector<TupleTypeElt, 4> inputs;

  // The innermost generic parameter list.
  // FIXME: Interface types make this unnecessary.
  Optional<GenericParamList *> genericParams;

  // The dependent generic signature.
  CanGenericSignature genericSig;
  if (auto gft = dyn_cast<GenericFunctionType>(t)) {
    genericSig = gft.getGenericSignature();
  }

  // Merge inputs and generic parameters from the uncurry levels.
  for (;;) {
    inputs.push_back(TupleTypeElt(t->getInput()));

    if (auto pft = dyn_cast<PolymorphicFunctionType>(t)) {
      assert(!genericParams
             || pft->getGenericParams().getOuterParameters() == *genericParams);
      genericParams = &pft->getGenericParams();
    }

    if (uncurryLevel-- == 0)
      break;
    t = cast<AnyFunctionType>(t.getResult());
  }

  CanType resultType = t.getResult();

  // Bridge input and result types.
  switch (cc) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    // Native functions don't need bridging.
    break;

  case AbstractCC::C:
    for (auto &input : inputs)
      input = input.getWithType(
               getBridgedInputType(*this, cc, CanType(input.getType()),
                                   clangDecl));
    resultType = getBridgedResultType(*this, cc, resultType,
                                      clangDecl);
    break;
  case AbstractCC::ObjCMethod: {
    // The "self" parameter should not get bridged unless it's a metatype.
    unsigned skip = 1;
    if (inputs.front().getType()->is<AnyMetatypeType>())
      skip = 0;

    for (auto &input : make_range(inputs.begin() + skip, inputs.end()))
      input = input.getWithType(
                getBridgedInputType(*this, cc, CanType(input.getType()),
                                    clangDecl));
    resultType = getBridgedResultType(*this, cc, resultType, clangDecl);
    break;
  }
  }

  // Put the inputs in the order expected by the calling convention.
  std::reverse(inputs.begin(), inputs.end());

  // Create the new function type.
  CanType inputType = CanType(TupleType::get(inputs, Context));
  GenericParamList *innerGenericParams
    = genericParams ? *genericParams : nullptr;
  if (genericSig) {
    assert(!innerGenericParams && "got mix of Polymorphic/Generic FunctionType?!");
    return CanGenericFunctionType::get(genericSig,
                                       inputType, resultType, extInfo);
  }

  if (innerGenericParams) {
    return CanPolymorphicFunctionType::get(inputType, resultType,
                                           innerGenericParams,
                                           extInfo);
  } else {
    return CanFunctionType::get(inputType, resultType, extInfo);
  }
}

namespace {
  class SILFunctionTypeSubstituter :
      SubstTypeVisitor<SILFunctionTypeSubstituter> {
    TypeConverter &TC;
    CanSILFunctionType OrigFnType;
    ArrayRef<SILParameterInfo> OrigParams;
    SmallVector<SILParameterInfo, 8> SubstParams;

    friend class SubstTypeVisitor<SILFunctionTypeSubstituter>;

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
      if (origResultType.getAsType() == substResultType
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

    void substInputs(CanType origInputType, CanType substInputType) {
      visit(origInputType, substInputType);
    }

    ArrayRef<SILParameterInfo> getSubstParams() const {
      assert(OrigParams.empty() && "didn't claim all parameters?!");
      return SubstParams;
    }

  private:
    /// Decompose tuples.
    void visitTupleType(CanTupleType origTuple, CanTupleType substTuple) {
      assert(origTuple->getNumElements() == substTuple->getNumElements());
      for (auto i : indices(origTuple.getElementTypes()))
        visit(origTuple.getElementType(i), substTuple.getElementType(i));
    }

    /// Every other type corresponds to a single parameter in the
    /// original signature, since we're dealing with like-uncurried
    /// types and thus don't have to worry about expanding archetypes
    /// to unmaterializable parameter clauses in result function types.
    void visitType(CanType origType, CanType substType) {
      auto origParam = claimNextOrigParam();
      assert(!origParam.isIndirectResult());

      // If the type hasn't changed and doesn't rely on context, just use the
      // original parameter.
      if (origType == substType && !origParam.getType()->isDependentType()) {
        SubstParams.push_back(origParam);
        return;
      }

      // Otherwise, lower the substituted type using the abstraction
      // patterns of the original.
      auto &substTL =
        TC.getTypeLowering(AbstractionPattern(origType), substType);
      auto substConvention = getSubstConvention(origParam.getConvention(),
                                                substTL.isTrivial());
      assert(isIndirectParameter(substConvention) ||
             !substTL.isPassedIndirectly());
      addSubstParam(substTL.getLoweredType().getSwiftRValueType(),
                    substConvention);
    }

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
  substituter.substInputs(origLoweredType.getInput(),
                          substLoweredInterfaceType.getInput());

  // Allow the substituted type to add thick-ness, but not remove it.
  assert(!origFnType->getExtInfo().hasContext()
           || substLoweredType->getExtInfo().hasContext());
  assert(substLoweredType->getRepresentation()
           == substLoweredInterfaceType->getRepresentation());

  // FIXME: Map into archetype context.
  return SILFunctionType::get(genericSig,
                              substLoweredType->getExtInfo(),
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
