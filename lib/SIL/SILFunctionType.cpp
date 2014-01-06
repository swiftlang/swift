//===--- SILFunctionType.cpp - Lowering for SILFunctionType ---------------===//
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

#include "swift/SIL/SILType.h"
#include "swift/SIL/SILModule.h"
#include "swift/AST/Decl.h"
#include "swift/AST/SubstTypeVisitor.h"
#include "swift/Basic/Fallthrough.h"
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

  if (auto ft = dyn_cast<FunctionType>(t))
    return CanFunctionType::get(ft.getInput(), ft.getResult(), extInfo);
  
  if (auto pft = dyn_cast<PolymorphicFunctionType>(t))
    return CanPolymorphicFunctionType::get(pft.getInput(), pft.getResult(),
                                           &pft->getGenericParams(),
                                           extInfo);

  llvm_unreachable("bad type to pass to adjustFunctionType");
}

/// Adjust a function type to have a slightly different type.
CanSILFunctionType Lowering::adjustFunctionType(CanSILFunctionType type,
                                             SILFunctionType::ExtInfo extInfo,
                                                ParameterConvention callee) {
  if (type->getExtInfo() == extInfo &&
      type->getCalleeConvention() == callee)
    return type;

  return SILFunctionType::get(type->getGenericParams(),
                              extInfo,
                              callee,
                              type->getParameters(),
                              type->getResult(),
                              type->getASTContext());
}

namespace {
  class Conventions {
  protected:
    virtual ~Conventions() = default;
  public:
    virtual ParameterConvention getIndirectParameter(unsigned index,
                                                     CanType type) const = 0;
    virtual ParameterConvention getDirectParameter(unsigned index,
                                                   CanType type) const = 0;
    virtual ParameterConvention getCallee() const = 0;
    virtual ResultConvention getResult(CanType type) const = 0;
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
  ///   (UnicodeScalar, (Int, Float), @inout Double) -> Bool
  /// then its most general form is
  ///   (A, B, @inout C) -> D
  /// because the substitution
  ///   X := (UnicodeScalar, (Int, Float), @inout Double)
  /// is invalid substitution, ultimately because '@inout Double'
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
      // @inout types aren't valid targets for substitution.
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
/// \param conventions - conventions as expressed for the original type
static CanSILFunctionType getSILFunctionType(SILModule &M,
                                             CanType origType,
                                             CanAnyFunctionType substFnType,
                                             AnyFunctionType::ExtInfo extInfo,
                                             const Conventions &conventions) {
  SmallVector<SILParameterInfo, 8> inputs;

  // If the original type is an archetype, we'll want the most general type.
  auto origFnType = dyn_cast<AnyFunctionType>(origType);
  assert(origFnType || isa<ArchetypeType>(origType));
  
  // Get an abstraction pattern to apply against the result.
  // If the unsubstituted type is an archetype, use the most general type for
  // the result; otherwise, match the original abstraction level.
  AbstractionPattern origResultPattern = origFnType
    ? AbstractionPattern(origFnType.getResult())
    : AbstractionPattern(origType);

  CanType substFormalResultType = substFnType.getResult();
  auto &substResultTL = M.Types.getTypeLowering(origResultPattern,
                                                substFormalResultType);
  bool hasIndirectResult;

  // If the substituted result type is returned indirectly, then the
  // original type must be as well.
  if (substResultTL.isReturnedIndirectly()) {
    hasIndirectResult = true;

  // If the unsubstituted type is an archetype, then we use the most
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
    if (substResultTL.isTrivial()) {
      convention = ResultConvention::Unowned;
    } else {
      convention = conventions.getResult(loweredResultType);
    }
    result = SILResultInfo(loweredResultType, convention);
  }
  
  // Destructure the input tuple type.
  if (origFnType) {
    DestructureInputs(M, conventions, inputs).visit(origFnType.getInput(),
                                                    substFnType.getInput());
  } else {
    DestructureGeneralizedInputs(M, AbstractionPattern(origType), inputs)
      .visit(substFnType.getInput());
  }

  // Find the generic parameters.
  GenericParamList *genericParams = nullptr;
  if (auto polyFnType = dyn_cast<PolymorphicFunctionType>(substFnType)) {
    genericParams = &polyFnType->getGenericParams();
  }

  auto calleeConvention = ParameterConvention::Direct_Unowned;
  if (!extInfo.isThin()) calleeConvention = conventions.getCallee();

  // Always strip the auto-closure bit.
  extInfo = extInfo.withIsAutoClosure(false);

  return SILFunctionType::get(genericParams, extInfo, calleeConvention,
                              inputs, result, M.getASTContext());  
}

namespace {
  /// The default Swift conventions.
  struct DefaultConventions : Conventions {
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
    ResultConvention getResult(CanType type) const override {
      return ResultConvention::Owned;
    }
  };
}

static CanSILFunctionType getNativeSILFunctionType(SILModule &M,
                                                   CanType origType,
                                                   CanAnyFunctionType substType,
                                                AnyFunctionType::ExtInfo extInfo) {
  return getSILFunctionType(M, origType, substType,
                            extInfo, DefaultConventions());
}

CanSILFunctionType swift::getNativeSILFunctionType(SILModule &M,
                                                   CanType origType,
                                                CanAnyFunctionType substType) {
  AnyFunctionType::ExtInfo extInfo;

  // Preserve type information from the original type if possible.
  if (auto origFnType = dyn_cast<AnyFunctionType>(origType)) {
    extInfo = origFnType->getExtInfo();

  // Otherwise, preserve function type attributes from the substituted type.
  } else {
    extInfo = substType->getExtInfo();
  }
  return ::getNativeSILFunctionType(M, origType, substType, extInfo);
}

/*****************************************************************************/
/****************************** Clang imports ********************************/
/*****************************************************************************/

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
  if (param->hasAttr<clang::NSConsumedAttr>())
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
    ObjCMethodConventions(const clang::ObjCMethodDecl *method) : Method(method) {}

    bool isSelfParameter(unsigned index) const {
      assert(index <= Method->param_size());
      return index == Method->param_size();
    }

    ParameterConvention getIndirectParameter(unsigned index,
                                             CanType type) const override {
      assert(!isSelfParameter(index));
      return getIndirectCParameterConvention(Method->param_begin()[index]);
    }

    ParameterConvention getDirectParameter(unsigned index,
                                           CanType type) const override {
      if (!isSelfParameter(index))
        return getDirectCParameterConvention(Method->param_begin()[index]);

      if (Method->hasAttr<clang::NSConsumesSelfAttr>())
        return ParameterConvention::Direct_Owned;

      // The caller is supposed to take responsibility for ensuring
      // that 'self' survives a method call.
      return ObjCSelfConvention;
    }

    ParameterConvention getCallee() const override {
      // Always thin.
      return ParameterConvention::Direct_Unowned;
    }

    ResultConvention getResult(CanType type) const override {
      if (Method->hasAttr<clang::NSReturnsRetainedAttr>())
        return ResultConvention::Owned;
      return getCResultConvention(Method->getResultType());
    }
  };

  /// Conventions based on a C function type.
  class CFunctionTypeConventions : public Conventions {
    const clang::FunctionProtoType *FnType;
  public:
    CFunctionTypeConventions(const clang::FunctionProtoType *type)
      : FnType(type) {}

    ParameterConvention getIndirectParameter(unsigned index,
                                             CanType type) const override {
      return getIndirectCParameterConvention(FnType->getArgType(index));
    }

    ParameterConvention getDirectParameter(unsigned index,
                                           CanType type) const override {
      return getDirectCParameterConvention(FnType->getArgType(index));
    }

    ParameterConvention getCallee() const override {
      // FIXME: blocks should be Direct_Guaranteed.
      return ParameterConvention::Direct_Unowned;
    }

    ResultConvention getResult(CanType type) const override {
      if (FnType->getExtInfo().getProducesResult())
        return ResultConvention::Owned;
      return getCResultConvention(FnType->getResultType());
    }    
  };
}

/// Given that we have an imported Clang declaration, deduce the
/// ownership conventions for calling it and build the SILFunctionType.
static CanSILFunctionType
getSILFunctionTypeForClangDecl(SILModule &M, const clang::Decl *clangDecl,
                               CanAnyFunctionType origType,
                               CanAnyFunctionType substType,
                               AnyFunctionType::ExtInfo extInfo) {
  if (auto method = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
    return getSILFunctionType(M, origType, substType, extInfo,
                              ObjCMethodConventions(method));
  }

  if (auto func = dyn_cast<clang::FunctionDecl>(clangDecl)) {
    auto fnType = func->getType()->castAs<clang::FunctionProtoType>();
    return getSILFunctionType(M, origType, substType, extInfo,
                              CFunctionTypeConventions(fnType));
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

/*****************************************************************************/
/******************************* Selectors ***********************************/
/*****************************************************************************/

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
  case SILDeclRef::Kind::Func:
    return getSelectorFamily(c.getDecl()->getName());

  case SILDeclRef::Kind::Initializer:
    return SelectorFamily::Init;

  case SILDeclRef::Kind::Getter:
    // Getter selectors can belong to families if their name begins with the
    // wrong thing.
    if (c.getDecl()->isObjC() || c.isForeign)
      return getSelectorFamily(c.getDecl()->getName());
    return SelectorFamily::None;

  // Setter selectors shouldn't belong to any family we care about.
  case SILDeclRef::Kind::Setter:
  /// Currently IRGen wraps alloc/init methods into Swift constructors
  /// with Swift conventions.
  case SILDeclRef::Kind::Allocator:
  /// These constants don't correspond to method families we care about yet.
  case SILDeclRef::Kind::EnumElement:
  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::GlobalAccessor:
  case SILDeclRef::Kind::DefaultArgGenerator:
    return SelectorFamily::None;
  }
}

namespace {
  class SelectorFamilyConventions : public Conventions {
    SelectorFamily Family;

    /// The index of the 'self' parameter.
    unsigned SelfParamIndex;
    bool isSelfParameter(unsigned index) const {
      return index == SelfParamIndex;
    }

  public:
    SelectorFamilyConventions(SelectorFamily family, unsigned selfIndex)
      : Family(family), SelfParamIndex(selfIndex) {}

    ParameterConvention getIndirectParameter(unsigned index,
                                             CanType type) const override {
      assert(!isSelfParameter(index));
      return ParameterConvention::Indirect_In;
    }

    ParameterConvention getDirectParameter(unsigned index,
                                           CanType type) const override {
      if (isSelfParameter(index)) {
        if (Family == SelectorFamily::Init)
          return ParameterConvention::Direct_Owned;
        return ObjCSelfConvention;
      }
      return ParameterConvention::Direct_Unowned;
    }

    ParameterConvention getCallee() const override {
      // Always thin.
      return ParameterConvention::Direct_Unowned;
    }

    ResultConvention getResult(CanType type) const override {
      if (Family != SelectorFamily::None)
        return ResultConvention::Owned;
      if (type.hasReferenceSemantics())
        return ResultConvention::Autoreleased;
      return ResultConvention::Unowned;
    }
  };
}

static CanSILFunctionType
getSILFunctionTypeForSelectorFamily(SILModule &M, SelectorFamily family,
                                    CanAnyFunctionType origType,
                                    CanAnyFunctionType substType,
                                    AnyFunctionType::ExtInfo extInfo) {
  unsigned numParams = 0;
  visitDestructuredArgumentTypes([&](CanType type) { numParams++; },
                                 origType.getInput());

  return getSILFunctionType(M, origType, substType, extInfo,
                            SelectorFamilyConventions(family, numParams - 1));
}

static CanSILFunctionType
getUncachedSILFunctionTypeForConstant(SILModule &M, SILDeclRef constant,
                                      CanAnyFunctionType origLoweredType,
                                      CanAnyFunctionType substFormalType,
                                      bool thin) {
  assert(origLoweredType->isThin());

  auto extInfo = origLoweredType->getExtInfo().withIsThin(thin);

  CanAnyFunctionType substLoweredType;
  if (substFormalType) {
    substLoweredType = M.Types.getLoweredASTFunctionType(substFormalType,
                                                         constant.uncurryLevel,
                                                         extInfo);
  } else {
    substLoweredType = origLoweredType;
  }

  if (!constant.isForeign) {
    return getNativeSILFunctionType(M, origLoweredType, substLoweredType,
                                    extInfo);
  }

  // If we have a clang decl associated with the Swift decl, derive its
  // ownership conventions.
  // FIXME: When we support calling ObjC blocks, we'll need to handle anonymous
  // SILDeclRefs here too.

  auto decl = constant.loc.get<ValueDecl*>();

  if (auto clangDecl = findClangMethod(decl))
    return getSILFunctionTypeForClangDecl(M, clangDecl,
                                          origLoweredType, substLoweredType,
                                          extInfo);

  // If the decl belongs to an ObjC method family, use that family's
  // ownership conventions.
  return getSILFunctionTypeForSelectorFamily(M, getSelectorFamily(constant),
                                             origLoweredType, substLoweredType,
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
    return c.hasDecl() && isClassOrProtocolMethod(c.getDecl())
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
  
  if (c.getDecl()->isInstanceMember() ||
      c.kind == SILDeclRef::Kind::Initializer)
    return AbstractCC::Method;
  return AbstractCC::Freestanding;
}

CanSILFunctionType TypeConverter::getConstantFunctionType(SILDeclRef constant,
                                             CanAnyFunctionType substFormalType,
                                                          bool thin) {
  auto cachedInfo = getConstantInfo(constant);
  if (!substFormalType || substFormalType == cachedInfo.FormalType) {
    auto extInfo = cachedInfo.SILFnType->getExtInfo().withIsThin(thin);
    return adjustFunctionType(cachedInfo.SILFnType, extInfo);
  }

  return getUncachedSILFunctionTypeForConstant(M, constant,
                                               cachedInfo.LoweredType,
                                               substFormalType,
                                               thin);
}

SILConstantInfo TypeConverter::getConstantInfo(SILDeclRef constant) {
  auto found = ConstantTypes.find(constant);
  if (found != ConstantTypes.end())
    return found->second;

  // First, get a function type for the constant.  This creates the
  // right type for a getter or setter.
  auto formalType = makeConstantType(constant, /*withCaptures*/ true);

  // The formal type is just that with the right CC and thin-ness.
  AbstractCC cc = getAbstractCC(constant);
  formalType = getThinFunctionType(formalType, cc);

  // The lowered type is the formal type, but uncurried and with
  // parameters automatically turned into their bridged equivalents.
  auto loweredType =
    getLoweredASTFunctionType(formalType, constant.uncurryLevel);

  // The SIL type encodes conventions according to the original type.
  CanSILFunctionType silFnType =
    getUncachedSILFunctionTypeForConstant(M, constant, loweredType,
                                          CanAnyFunctionType(),
                                          /*thin*/ true);

  DEBUG(llvm::dbgs() << "lowering type for constant ";
        constant.print(llvm::dbgs());
        llvm::dbgs() << "\n  formal type: ";
        formalType.print(llvm::dbgs());
        llvm::dbgs() << "\n  lowered AST type: ";
        loweredType.print(llvm::dbgs());
        llvm::dbgs() << "\n  SIL type: ";
        silFnType.print(llvm::dbgs());
        llvm::dbgs() << "\n");

  SILConstantInfo result = { formalType, loweredType, silFnType };
  ConstantTypes[constant] = result;
  return result;
}

/// Bridge the elements of an input tuple type.
static CanType getBridgedInputType(TypeConverter &tc,
                                   AbstractCC cc,
                                   CanType input) {
  if (auto tuple = dyn_cast<TupleType>(input)) {
    SmallVector<TupleTypeElt, 4> bridgedFields;
    bool changed = false;
    for (auto &elt : tuple->getFields()) {
      CanType bridged = CanType(tc.getLoweredBridgedType(elt.getType(), cc));
      if (bridged != CanType(elt.getType())) {
        changed = true;
        bridgedFields.push_back(elt.getWithType(bridged));
      } else {
        bridgedFields.push_back(elt);
      }
    }
    
    if (!changed)
      return input;
    return CanType(TupleType::get(bridgedFields, input->getASTContext()));
  }
  
  return tc.getLoweredBridgedType(input, cc)->getCanonicalType();
}

/// Bridge a result type.
static CanType getBridgedResultType(TypeConverter &tc,
                                    AbstractCC cc,
                                    CanType result) {
  return tc.getLoweredBridgedType(result, cc)->getCanonicalType();
}

/// Fast path for bridging types in a function type without uncurrying.
static CanAnyFunctionType getBridgedFunctionType(TypeConverter &tc,
                                            CanAnyFunctionType t,
                                            AnyFunctionType::ExtInfo extInfo) {
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
      return CanPolymorphicFunctionType::get(input, result,
                                             innerGenericParams,
                                             extInfo);
    } else {
      return CanFunctionType::get(input, result, extInfo);
    }
  };

  switch (t->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    // No bridging needed for native functions.
    if (t->getExtInfo() == extInfo && !innerGenericParams)
      return t;
    return rebuild(t.getInput(), t.getResult());

  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    return rebuild(getBridgedInputType(tc, t->getAbstractCC(), t.getInput()),
                   getBridgedResultType(tc, t->getAbstractCC(), t.getResult()));
  }
  llvm_unreachable("bad calling convention");
}

CanAnyFunctionType
TypeConverter::getLoweredASTFunctionType(CanAnyFunctionType t,
                                         unsigned uncurryLevel,
                                         AnyFunctionType::ExtInfo extInfo) {
  // Fast path: no uncurrying required.
  if (uncurryLevel == 0)
    return getBridgedFunctionType(*this, t, extInfo);

  AbstractCC cc = extInfo.getCC();
  assert(!extInfo.isAutoClosure() && "auto_closures cannot be curried");
  assert(!extInfo.isBlock() && "objc blocks cannot be curried");
  
  // The uncurried input types.
  SmallVector<TupleTypeElt, 4> inputs;
  
  // The uncurried generic parameter list components.
  bool isPolymorphic = false;
  SmallVector<GenericParam, 4> genericParams;
  SmallVector<RequirementRepr, 4> requirements;
  SmallVector<ArchetypeType *, 4> allArchetypes;
  GenericParamList *outerParameters = nullptr;

  // Merge inputs and generic parameters from the uncurry levels.
  for (;;) {
    inputs.push_back(TupleTypeElt(t->getInput()));
    
    if (auto pft = dyn_cast<PolymorphicFunctionType>(t)) {
      isPolymorphic = true;
      GenericParamList &params = pft->getGenericParams();
      if (GenericParamList *outer = params.getOuterParameters()) {
        if (!outerParameters)
          outerParameters = outer;
      }
      
      genericParams.append(params.getParams().begin(),
                           params.getParams().end());
      requirements.append(params.getRequirements().begin(),
                          params.getRequirements().end());
      allArchetypes.append(params.getAllArchetypes().begin(),
                           params.getAllArchetypes().end());
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
               getBridgedInputType(*this, cc, CanType(input.getType())));
    resultType = getBridgedResultType(*this, cc, resultType);
    break;
  case AbstractCC::ObjCMethod:
    // The "self" parameter should not get bridged.
    for (auto &input : make_range(inputs.begin() + 1, inputs.end()))
      input = input.getWithType(
                getBridgedInputType(*this, cc, CanType(input.getType())));
    resultType = getBridgedResultType(*this, cc, resultType);
    break;
  }
  
  // Put the inputs in the order expected by the calling convention.
  std::reverse(inputs.begin(), inputs.end());
  
  // Create the new function type.
  CanType inputType = CanType(TupleType::get(inputs, Context));
  if (isPolymorphic) {
    auto *curriedGenericParams = GenericParamList::create(Context,
                                                          SourceLoc(),
                                                          genericParams,
                                                          SourceLoc(),
                                                          requirements,
                                                          SourceLoc());
    curriedGenericParams->setAllArchetypes(Context.AllocateCopy(allArchetypes));
    curriedGenericParams->setOuterParameters(outerParameters);
    
    return CanPolymorphicFunctionType::get(inputType, resultType,
                                           curriedGenericParams,
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
      : TC(TC), OrigFnType(origFnType), OrigParams(origFnType->getParameters())
    {}

    SILResultInfo substResult(AbstractionPattern origResultType,
                              CanType substResultType) {
      SILResultInfo origResult = OrigFnType->getResult();
      bool origHasIndirectResult = OrigFnType->hasIndirectResult();

      // Claim the implicit indirect result parameter.
      SILParameterInfo origIndirectResult;
      if (origHasIndirectResult) {
        origIndirectResult = claimNextOrigParam();
        assert(origIndirectResult.isIndirectResult());
      }

      // If the result type didn't change, we can just use the
      // original result.
      if (origResultType.getAsType() == substResultType) {
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

      // If the type hasn't changed, just use the original parameter.
      if (origType == substType) {
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
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_In:
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
                                 CanAnyFunctionType substLoweredType) {
  if (origLoweredType == substLoweredType)
    return origFnType;

  SILFunctionTypeSubstituter substituter(*this, origFnType);

  // Map the result.
  SILResultInfo substResult =
    substituter.substResult(AbstractionPattern(origLoweredType.getResult()),
                            substLoweredType.getResult());

  // Map the inputs.
  substituter.substInputs(origLoweredType.getInput(),
                          substLoweredType.getInput());

  // Use the generic parameters from the substituted type.
  GenericParamList *genericParams = nullptr;
  if (auto polySubstFn = dyn_cast<PolymorphicFunctionType>(substLoweredType))
    genericParams = &polySubstFn->getGenericParams();

  // Allow the substituted type to add thick-ness, but not remove it.
  assert(origFnType->isThin() || !substLoweredType->isThin());

  return SILFunctionType::get(genericParams,
                              substLoweredType->getExtInfo(),
                              origFnType->getCalleeConvention(),
                              substituter.getSubstParams(),
                              substResult,
                              Context);
}

static void getReplacementTypes(GenericParamList &genericParams,
                                ArrayRef<Substitution> subs,
                                SmallVectorImpl<Type> &replacements) {
  (void) genericParams;

  // Add a substitution for Self if present.
  if (genericParams.hasSelfArchetype()) {
    assert(subs[0].Archetype->getSelfProtocol()
           && "first substitution is not for Self");
    replacements.push_back(subs[0].Replacement);
    subs = subs.slice(1);
  }
  
#ifndef NDEBUG
  // FIXME: The AST sets up substitutions for secondary archetypes that are
  // strictly unnecessary.
  auto archetypes = genericParams.getAllArchetypes();
  assert(subs.size() == archetypes.size() &&
         "substitutions don't match archetypes");
  auto ai = archetypes.begin();
#endif
  
  for (auto &sub : subs) {
    assert(*ai++ == sub.Archetype && "substitution doesn't match archetype");
    replacements.push_back(sub.Replacement);
  }  
}

/// Apply a substitution to this polymorphic SILFunctionType so that
/// it has the form of the normal SILFunctionType for the substituted
/// type, except using the original conventions.
CanSILFunctionType SILFunctionType::substGenericArgs(SILModule &silModule,
                                                     Module *astModule,
                                                  ArrayRef<Substitution> subs) {
  assert(isPolymorphic());
  auto &params = *getGenericParams();

  SmallVector<Type, 4> replacements;
  getReplacementTypes(params, subs, replacements);

  // FIXME: Only substitute the primary archetypes.
  return substGenericArgs(silModule, astModule,
      llvm::makeArrayRef(replacements).slice(0, params.getParams().size()));
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
      : TheSILModule(silModule), TheASTModule(astModule), Subs(subs) {}

    // SIL type lowering only does special things to tuples and functions.

    /// Functions need to preserve their abstraction structure.
    CanSILFunctionType visitSILFunctionType(CanSILFunctionType origType,
                                            bool dropGenerics = false,
                                            bool interfaceTypes = false)
    {
      auto result = interfaceTypes ? origType->getInterfaceResult()
                                   : origType->getResult();
      SILResultInfo substResult = subst(result);

      SmallVector<SILParameterInfo, 8> substParams;
      substParams.reserve(origType->getParameters().size());
      auto params = interfaceTypes ? origType->getInterfaceParameters()
                                   : origType->getParameters();
      for (auto &origParam : params) {
        substParams.push_back(subst(origParam));
      }

      auto generics = (dropGenerics ? nullptr : origType->getGenericParams());
      return SILFunctionType::get(generics, origType->getExtInfo(),
                                  origType->getCalleeConvention(),
                                  substParams, substResult, getASTContext());
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
      for (auto &origElt : origType->getFields()) {
        auto substEltType = visit(CanType(origElt.getType()));
        substElts.push_back(origElt.getWithType(substEltType));
      }
      return CanType(TupleType::get(substElts, getASTContext()));
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
  SILTypeSubstituter STST(silModule, astModule, subs);
  return STST.subst(SrcTy);
}

CanSILFunctionType SILType::substFuncType(SILModule &silModule,
                                          Module *astModule,
                                          TypeSubstitutionMap &subs,
                                          CanSILFunctionType SrcTy,
                                          bool dropGenerics) {
  SILTypeSubstituter STST(silModule, astModule, subs);
  return STST.visitSILFunctionType(SrcTy, dropGenerics);
}

CanSILFunctionType SILFunctionType::substGenericArgs(SILModule &silModule,
                                                     Module *astModule,
                                                     ArrayRef<Type> args) {
  assert(isPolymorphic());
  auto genericParams = getGenericParams()->getParams();
  assert(args.size() <= genericParams.size());

  TypeSubstitutionMap subs;
  for (size_t i = 0, e = args.size(); i != e; ++i) {
    auto archetype = genericParams[i].getAsTypeParam()->getArchetype();
    subs.insert(std::make_pair(archetype, args[i]));
  }

  SILTypeSubstituter substituter(silModule, astModule, subs);
  return substituter.visitSILFunctionType(CanSILFunctionType(this),
                                          /*dropGenerics*/ true);
}

/// Apply a substitution to this polymorphic SILFunctionType so that
/// it has the form of the normal SILFunctionType for the substituted
/// type, except using the original conventions.
CanSILFunctionType
SILFunctionType::substInterfaceGenericArgs(SILModule &silModule,
                                           Module *astModule,
                                           ArrayRef<Substitution> subs) {
  assert(isPolymorphic());
  TypeSubstitutionMap map = GenericSig->getSubstitutionMap(subs);
  SILTypeSubstituter substituter(silModule, astModule, map);
  
  GenericContextScope scope(silModule.Types,
                            getGenericSignature()->getGenericParams(),
                            getGenericSignature()->getRequirements());
  
  return substituter.visitSILFunctionType(CanSILFunctionType(this),
                                          /*dropGenerics*/ true,
                                          /*interfaceTypes*/ true);
}
