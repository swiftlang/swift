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
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
using namespace swift;
using namespace swift::Lowering;

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
}

/// Create the appropriate SIL function type for the given formal type
/// and conventions.
static SILFunctionType *getSILFunctionType(SILModule &M,
                                           CanAnyFunctionType formalType,
                                           const Conventions &conventions) {
  SmallVector<SILParameterInfo, 8> inputs;
  
  // If the result type lowers to an address-only type, add it as an indirect
  // return argument.
  CanType formalResultType = formalType.getResult();
  auto &resultTL = M.Types.getTypeLowering(formalResultType);
  bool hasIndirectReturn = resultTL.isAddressOnly();
  CanType resultType = resultTL.getLoweredType().getSwiftRValueType();
  SILResultInfo result;
  if (hasIndirectReturn) {
    inputs.push_back(SILParameterInfo(resultType,
                                   ParameterConvention::Indirect_Out));
    resultType = TupleType::getEmpty(M.getASTContext());    
    result = SILResultInfo(resultType, ResultConvention::Unowned);
  } else {
    ResultConvention convention;
    if (resultTL.isTrivial()) {
      convention = ResultConvention::Unowned;
    } else {
      convention = conventions.getResult(formalResultType);
    }
    result = SILResultInfo(resultType, convention);
  }
  
  // Destructure the input tuple type.
  unsigned nextParamIndex = 0;
  auto visitFn = [&](CanType type) {
    unsigned paramIndex = nextParamIndex++;

    auto &paramTL = M.Types.getTypeLowering(type);
    ParameterConvention convention;
    if (isa<LValueType>(type)) {
      convention = ParameterConvention::Indirect_Inout;
    } else if (paramTL.isAddressOnly()) {
      convention = conventions.getIndirectParameter(paramIndex, type);
      assert(isIndirectParameter(convention));
    } else if (paramTL.isTrivial()) {
      convention = ParameterConvention::Direct_Unowned;
    } else {
      convention = conventions.getDirectParameter(paramIndex, type);
      assert(!isIndirectParameter(convention));
    }
    assert(isIndirectParameter(convention)
             == paramTL.getLoweredType().isAddress());
    auto loweredType = paramTL.getLoweredType().getSwiftRValueType();
    inputs.push_back(SILParameterInfo(loweredType, convention));
  };
  visitDestructuredArgumentTypes(visitFn, formalType.getInput());

  // Find the generic parameters.
  GenericParamList *genericParams = nullptr;
  if (auto polyFnType = dyn_cast<PolymorphicFunctionType>(formalType)) {
    genericParams = &polyFnType->getGenericParams();
  }

  return SILFunctionType::get(genericParams, formalType->getExtInfo(),
                              conventions.getCallee(), inputs, result,
                              M.getASTContext());  
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
      return ParameterConvention::Direct_Owned;
    }
    ResultConvention getResult(CanType type) const override {
      return ResultConvention::Owned;
    }
  };
}

SILFunctionType *SILType::getFunctionTypeInfo(SILModule &M) const {
  CanAnyFunctionType fnType = cast<AnyFunctionType>(getSwiftRValueType());
  
  auto found = M.FunctionTypeInfoCache.find(fnType);
  if (found != M.FunctionTypeInfoCache.end())
    return found->second;

  auto silFnType = getSILFunctionType(M, fnType, DefaultConventions());

  M.FunctionTypeInfoCache[fnType] = silFnType;
  return silFnType;
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
      return ParameterConvention::Direct_Guaranteed;
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
static SILFunctionType *getSILFunctionTypeForClangDecl(SILModule &M,
                                                       const clang::Decl *clangDecl,
                                                       CanAnyFunctionType formalType) {
  if (auto method = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
    return getSILFunctionType(M, formalType, ObjCMethodConventions(method));
  }

  if (auto func = dyn_cast<clang::FunctionDecl>(clangDecl)) {
    auto fnType = func->getType()->castAs<clang::FunctionProtoType>();
    return getSILFunctionType(M, formalType, CFunctionTypeConventions(fnType));
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
        return ParameterConvention::Direct_Guaranteed;
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

static SILFunctionType *
getSILFunctionTypeForSelectorFamily(SILModule &M, SelectorFamily family,
                                    CanAnyFunctionType formalType) {
  unsigned numParams = 0;
  visitDestructuredArgumentTypes([&](CanType type) { numParams++; },
                                 formalType.getInput());

  return getSILFunctionType(M, formalType,
                            SelectorFamilyConventions(family, numParams - 1));
}

SILFunctionType *SILDeclRef::getSILFunctionType(SILModule &M) const {
  auto formalType = M.Types.getConstantType(*this);
  if (!isForeign) {
    return formalType.getFunctionTypeInfo(M);
  }

  // If we have a clang decl associated with the Swift decl, derive its
  // ownership conventions.
  // FIXME: When we support calling ObjC blocks, we'll need to handle anonymous
  // SILDeclRefs here too.

  auto formalFnType = formalType.castTo<AnyFunctionType>();
  auto decl = loc.get<ValueDecl*>();

  if (auto clangDecl = findClangMethod(decl))
    return getSILFunctionTypeForClangDecl(M, clangDecl, formalFnType);

  // If the decl belongs to an ObjC method family, use that family's
  // ownership conventions.
  return getSILFunctionTypeForSelectorFamily(M, getSelectorFamily(*this),
                                             formalFnType);
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

static AbstractCC getAbstractCC(SILDeclRef c) {
  // Currying thunks always have freestanding CC.
  if (c.isCurried)
    return AbstractCC::Freestanding;

  // If this is an ObjC thunk, it always has ObjC calling convention.
  if (c.isForeign)
    return c.hasDecl() && isClassOrProtocolMethod(c.getDecl())
      ? AbstractCC::ObjCMethod
      : AbstractCC::C;

  // Anonymous functions currently always have Freestanding CC.
  if (!c.hasDecl())
    return AbstractCC::Freestanding;

  // FIXME: Assert that there is a native entry point
  // available. There's no great way to do this.

  if (c.getDecl()->isInstanceMember() ||
      c.kind == SILDeclRef::Kind::Initializer)
    return AbstractCC::Method;
  return AbstractCC::Freestanding;
}

SILType TypeConverter::getConstantType(SILDeclRef constant) {
  auto found = constantTypes.find(constant);
  if (found != constantTypes.end())
    return found->second;

  AbstractCC cc = getAbstractCC(constant);
  Type swiftTy = getThinFunctionType(makeConstantType(constant), cc);
  SILType loweredTy
    = getTypeLowering(swiftTy, constant.uncurryLevel).getLoweredType();
  DEBUG(llvm::dbgs() << "constant ";
        constant.print(llvm::dbgs());
        llvm::dbgs() << " has type ";
        loweredTy.print(llvm::dbgs());
        llvm::dbgs() << " cc " << unsigned(cc) << "\n");
  constantTypes[constant] = loweredTy;
  return loweredTy;
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
                                                 CanAnyFunctionType t) {
  switch (t->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // No bridging needed for native functions.
    return t;

  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    if (auto pft = dyn_cast<PolymorphicFunctionType>(t)) {
      return CanAnyFunctionType(PolymorphicFunctionType::get(
                  getBridgedInputType(tc, t->getAbstractCC(), t.getInput()),
                  getBridgedResultType(tc, t->getAbstractCC(), t.getResult()),
                  &pft->getGenericParams(),
                  t->getExtInfo(),
                  t->getASTContext()));
    }
    return CanAnyFunctionType(FunctionType::get(
                getBridgedInputType(tc, t->getAbstractCC(), t.getInput()),
                getBridgedResultType(tc, t->getAbstractCC(), t.getResult()),
                t->getExtInfo(),
                t->getASTContext()));
  }
}

CanAnyFunctionType TypeConverter::getUncurriedFunctionType(CanAnyFunctionType t,
                                                        unsigned uncurryLevel) {
  if (uncurryLevel == 0)
    return getBridgedFunctionType(*this, t);

  AnyFunctionType::ExtInfo outerInfo = t->getExtInfo();
  AbstractCC outerCC = outerInfo.getCC();
  assert(!outerInfo.isAutoClosure() && "auto_closures cannot be curried");
  assert(!outerInfo.isBlock() && "objc blocks cannot be curried");

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
  switch (outerCC) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // Native functions don't need bridging.
    break;
  
  case AbstractCC::C:
    for (auto &input : inputs)
      input = input.getWithType(
               getBridgedInputType(*this, outerCC, CanType(input.getType())));
    resultType = getBridgedResultType(*this, outerCC, resultType);
    break;
  case AbstractCC::ObjCMethod:
    // The "self" parameter should not get bridged.
    for (auto &input : make_range(inputs.begin() + 1, inputs.end()))
      input = input.getWithType(
                getBridgedInputType(*this, outerCC, CanType(input.getType())));
    resultType = getBridgedResultType(*this, outerCC, resultType);
    break;
  }
  
  // Put the inputs in the order expected by the calling convention.
  std::reverse(inputs.begin(), inputs.end());
  
  // Create the new function type.
  const ASTContext &C = t->getASTContext();
  Type inputType = TupleType::get(inputs, C);
  if (isPolymorphic) {
    auto *curriedGenericParams = GenericParamList::create(C,
                                                          SourceLoc(),
                                                          genericParams,
                                                          SourceLoc(),
                                                          requirements,
                                                          SourceLoc());
    curriedGenericParams->setAllArchetypes(C.AllocateCopy(allArchetypes));
    curriedGenericParams->setOuterParameters(outerParameters);
    
    return CanPolymorphicFunctionType(
           PolymorphicFunctionType::get(inputType, resultType,
                                        curriedGenericParams,
                                        outerInfo, C));
  } else {
    return CanFunctionType(FunctionType::get(inputType, resultType,
                                             outerInfo, C));
  }    
}

namespace {
  struct SILFunctionTypeSubstituter :
      SubstTypeVisitor<SILFunctionTypeSubstituter> {
    SILModule &M;
    ArrayRef<SILParameterInfo> OldParams;
    SmallVector<SILParameterInfo, 8> NewParams;

    SILFunctionTypeSubstituter(SILModule &M) : M(M) {}

    SILParameterInfo claimNext() {
      assert(!OldParams.empty());
      auto temp = OldParams[0];
      OldParams = OldParams.slice(1);
      return temp;
    }

    // Decompose tuples.
    void visitTupleType(CanTupleType orig, CanTupleType subst) {
      assert(orig->getNumElements() == subst->getNumElements());
      for (unsigned i = 0, e = orig->getNumElements(); i != e; ++i) 
        visit(orig.getElementType(i), subst.getElementType(i));
    }

    void visitType(CanType orig, CanType subst) {
      auto oldParam = claimNext();

      if (orig == subst) {
        NewParams.push_back(oldParam);
        return;
      }

      switch (oldParam.getConvention()) {
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Indirect_Inout: {
        auto &substTL = M.Types.getTypeLowering(subst);
        auto loweredSubstType = substTL.getLoweredType().getSwiftRValueType();
        NewParams.push_back(SILParameterInfo(loweredSubstType,
                                             oldParam.getConvention()));
        return;
      }

      case ParameterConvention::Indirect_In:
        addOwnedTupleElements(subst);
        return;

      case ParameterConvention::Indirect_Out:
        llvm_unreachable("should've been filtered out before");
      }
      llvm_unreachable("bad parameter convention");
    }

    void addOwnedTupleElements(CanType type) {
      if (auto tuple = dyn_cast<TupleType>(type)) {
        for (auto elt : tuple.getElementTypes())
          addOwnedTupleElements(elt);
        return;
      }

      auto &paramTL = M.Types.getTypeLowering(type);
      ParameterConvention convention;
      if (isa<LValueType>(type)) {
        convention = ParameterConvention::Indirect_Inout;
      } else if (paramTL.isAddressOnly()) {
        convention = ParameterConvention::Indirect_In;
      } else if (paramTL.isTrivial()) {
        convention = ParameterConvention::Direct_Unowned;
      } else {
        convention = ParameterConvention::Direct_Owned;
      }
      assert(isIndirectParameter(convention)
               == paramTL.getLoweredType().isAddress());
      auto loweredType = paramTL.getLoweredType().getSwiftRValueType();
      NewParams.push_back(SILParameterInfo(loweredType, convention));
    }
  };
}

/// Apply a substitution to the given SILFunctionType so that it has
/// the form of the normal SILFunctionType for the substituted type,
/// except using the original conventions.
///
/// All of this code should disappear when we're tracking abstraction
/// difference properly.
SILFunctionType *swift::substituteSILFunctionType(SILModule &M,
                                                  SILFunctionType *origInfo,
                                                  SILType origType,
                                                  SILType substType) {
  SILFunctionTypeSubstituter substituter(M);

  auto origFnType = origType.castTo<AnyFunctionType>();
  auto substFnType = substType.castTo<FunctionType>();

  // Handle the result.
  auto &substResultTL = M.Types.getTypeLowering(substFnType.getResult());
  auto substResultTy = substResultTL.getLoweredType().getSwiftRValueType();
  SILResultInfo result;
  if (origInfo->hasIndirectResult()) {
    if (substResultTL.isAddressOnly()) {
      result = origInfo->getResult();
      SILParameterInfo param(substResultTy, ParameterConvention::Indirect_Out);
      substituter.NewParams.push_back(param);
    } else {
      result = SILResultInfo(substResultTy, ResultConvention::Owned);
    }
  } else {
    result = SILResultInfo(substResultTy, origInfo->getResult().getConvention());
  }

  // Map the inputs.
  substituter.OldParams = origInfo->getNonReturnParameters();
  substituter.visit(origFnType.getInput(), substFnType.getInput());

  return SILFunctionType::get(nullptr, origInfo->getExtInfo(),
                              origInfo->getCalleeConvention(),
                              substituter.NewParams, result, M.getASTContext());
}
