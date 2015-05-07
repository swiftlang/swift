//===--- SILGenType.cpp - SILGen for types and their members --------------===//
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
// This file contains code for emitting code associated with types:
//   - methods
//   - ObjC dispatch thunks
//   - SIL v-tables
//   - etc.
//
//===----------------------------------------------------------------------===//

#include "SILGenFunction.h"
#include "Scope.h"
#include "ManagedValue.h"
#include "swift/AST/AST.h"
#include "swift/AST/TypeMemberVisitor.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

SILFunction *SILGenModule::getDynamicThunk(SILDeclRef constant,
                                           SILConstantInfo constantInfo) {
  // Mangle the constant with a _TTD header.
  llvm::SmallString<32> name;
  constant.mangle(name, "_TTD");

  auto F = M.getOrCreateFunction(constant.getDecl(), name, SILLinkage::Shared,
                            constantInfo.getSILType().castTo<SILFunctionType>(),
                            IsBare, IsTransparent,
                            makeModuleFragile ? IsFragile : IsNotFragile);

  if (F->empty()) {
    // Emit the thunk if we haven't yet.
    // Currently a dynamic thunk looks just like a foreign-to-native thunk around
    // an ObjC method. This would change if we introduced a native
    // runtime-hookable mechanism.
    SILGenFunction SGF(*this, *F);
    SGF.emitForeignToNativeThunk(constant);
  }

  return F;
}

static constexpr unsigned OTKPair(OptionalTypeKind a, OptionalTypeKind b) {
  return unsigned(a) << 8 | unsigned(b);
}

SILFunction *
SILGenModule::emitVTableMethod(SILDeclRef derived, SILDeclRef base) {
  // As a fast path, if there is no override, definitely no thunk is necessary.
  if (derived == base)
    return getFunction(derived, NotForDefinition);

  // Generate the thunk name.
  // TODO: If we allocated a new vtable slot for the derived method, then
  // further derived methods would potentially need multiple thunks, and we
  // would need to mangle the base method into the symbol as well.
  llvm::SmallString<32> name;
  derived.mangle(name, "_TTV");

  // If we already emitted this thunk, reuse it.
  // TODO: Allocating new vtable slots for derived methods with different ABIs
  // would invalidate the assumption that the same thunk is correct, as above.
  if (auto existingThunk = M.lookUpFunction(name))
    return existingThunk;

  auto origDerivedTy = getConstantType(derived).castTo<SILFunctionType>();
  auto baseTy = getConstantType(base).castTo<SILFunctionType>();

  bool needsThunk = false;

  // Introduce optionality into the derived signature to match the base.
  // TODO: Handle other forms of reabstraction.
  SmallVector<SILParameterInfo, 4> vtableParams;
  SILResultInfo vtableResult;
  SmallVector<VTableParamThunk, 4> paramActions;
  VTableResultThunk resultAction;

  // The derived result may be less optional than the base.
  {
    SILType vtableResultTy;
    OptionalTypeKind baseOTK, derivedOTK;
    auto baseObj = baseTy->getSemanticResultSILType().getSwiftRValueType()
      ->getAnyOptionalObjectType(baseOTK);
    origDerivedTy->getSemanticResultSILType().getSwiftRValueType()
      ->getAnyOptionalObjectType(derivedOTK);

    switch (OTKPair(baseOTK, derivedOTK)) {
    case OTKPair(OTK_ImplicitlyUnwrappedOptional, OTK_None):
    case OTKPair(OTK_Optional, OTK_None): {
      // Optionalize the return value.
      // This only requires a thunk if the underlying type isn't an object
      // reference.
      auto baseResTy = baseTy->getSemanticResultSILType().getSwiftRValueType();
      auto origDerivedResTy = origDerivedTy
        ->getSemanticResultSILType().getSwiftRValueType();
      auto covariantTy = baseResTy->replaceCovariantResultType(origDerivedResTy,
                                                               0);
      auto canTy = covariantTy->getCanonicalType();
      vtableResultTy = SILType::getPrimitiveObjectType(canTy);
      resultAction = VTableResultThunk::MakeOptional;
      needsThunk |= !baseObj->isBridgeableObjectType();
      break;
    }

    case OTKPair(OTK_None, OTK_None):
    case OTKPair(OTK_Optional, OTK_Optional):
    case OTKPair(OTK_Optional, OTK_ImplicitlyUnwrappedOptional):
    case OTKPair(OTK_ImplicitlyUnwrappedOptional, OTK_Optional):
    case OTKPair(OTK_ImplicitlyUnwrappedOptional,
                 OTK_ImplicitlyUnwrappedOptional):
      // The return value doesn't need to change.
      vtableResultTy = origDerivedTy->getSemanticResultSILType();
      resultAction = VTableResultThunk::None;
      break;

    default:
      llvm_unreachable("derived return can't be more optional than base");
    }

    assert(origDerivedTy->hasIndirectResult() == baseTy->hasIndirectResult()
           && "return type reabstraction for override not implemented");
    if (origDerivedTy->hasIndirectResult()) {
      vtableParams.push_back(
                           SILParameterInfo(vtableResultTy.getSwiftRValueType(),
                                            ParameterConvention::Indirect_Out));
      vtableResult = SILResultInfo(TupleType::getEmpty(getASTContext()),
                                   ResultConvention::Unowned);
    } else {
      vtableResult = SILResultInfo(vtableResultTy.getSwiftRValueType(),
                                   origDerivedTy->getResult().getConvention());
    }
  }

  // We want to call 'throws' class methods assuming that they return
  // an error result, rather than paying the additional overhead
  // for an indirect call.  That means thunking.
  Optional<SILResultInfo> vtableErrorResult;
  if (baseTy->hasErrorResult() && !origDerivedTy->hasErrorResult()) {
    needsThunk = true;
    vtableErrorResult = baseTy->getErrorResult();
  } else {
    assert(origDerivedTy->hasErrorResult() == baseTy->hasErrorResult());
    assert(!origDerivedTy->hasErrorResult() ||
           origDerivedTy->getErrorResult() == baseTy->getErrorResult());
  }

  // The parameters may be either more optional than the base, or if the base
  // is IUO, may force off optionality.
  auto baseParams = baseTy->getParametersWithoutIndirectResult();
  auto derivedParams = origDerivedTy->getParametersWithoutIndirectResult();
  assert(baseParams.size() == derivedParams.size()
         && "explosion reabstraction for override not implemented");
  for (unsigned i : indices(baseParams)) {
    OptionalTypeKind baseOTK, derivedOTK;
    baseParams[i].getSILType().getSwiftRValueType()
      ->getAnyOptionalObjectType(baseOTK);
    auto derivedObj = derivedParams[i].getSILType().getSwiftRValueType()
      ->getAnyOptionalObjectType(derivedOTK);

    VTableParamThunk paramAction;

    switch (OTKPair(baseOTK, derivedOTK)) {
    case OTKPair(OTK_None, OTK_Optional):
    case OTKPair(OTK_None, OTK_ImplicitlyUnwrappedOptional):
      // De-optionalize the parameter.
      // This only requires a thunk if the underlying type isn't an object
      // reference.
      vtableParams.push_back(baseParams[i]);
      paramAction = VTableParamThunk::MakeOptional;
      needsThunk |= !derivedObj->isBridgeableObjectType();
      break;

    case OTKPair(OTK_ImplicitlyUnwrappedOptional, OTK_None):
      // Optionalize the parameter. We'll force it in the thunk.
      vtableParams.push_back(baseParams[i]);
      paramAction = VTableParamThunk::ForceIUO;
      needsThunk = true;
      break;

    case OTKPair(OTK_None, OTK_None):
    case OTKPair(OTK_Optional, OTK_Optional):
    case OTKPair(OTK_Optional, OTK_ImplicitlyUnwrappedOptional):
    case OTKPair(OTK_ImplicitlyUnwrappedOptional, OTK_Optional):
    case OTKPair(OTK_ImplicitlyUnwrappedOptional,
                 OTK_ImplicitlyUnwrappedOptional):
      // The parameter doesn't need to change.
      vtableParams.push_back(derivedParams[i]);
      paramAction = VTableParamThunk::None;
      break;

    default:
      llvm_unreachable("derived param can't be less optional than base");
    }

    paramActions.push_back(paramAction);
  }

  // If no thunk action was necessary, just emit the method as is.
  if (!needsThunk)
    return getFunction(derived, NotForDefinition);

  auto vtableDerivedTy = SILFunctionType::get(
                                          origDerivedTy->getGenericSignature(),
                                          origDerivedTy->getExtInfo(),
                                          origDerivedTy->getCalleeConvention(),
                                          vtableParams, vtableResult,
                                          vtableErrorResult,
                                          getASTContext());
  auto *derivedDecl = cast<AbstractFunctionDecl>(derived.getDecl());
  SILLocation loc(derivedDecl);
  auto thunk = SILFunction::create(M, SILLinkage::Private, name,
                                   vtableDerivedTy,
                                   derivedDecl->getGenericParams(),
                                   loc, IsBare, IsNotTransparent, IsNotFragile);
  thunk->setDebugScope(new (M) SILDebugScope(loc, *thunk));

  SILGenFunction(*this, *thunk)
    .emitVTableThunk(derived, base, paramActions, resultAction);

  return thunk;
}

SILValue SILGenFunction::emitDynamicMethodRef(SILLocation loc,
                                              SILDeclRef constant,
                                              SILConstantInfo constantInfo) {
  // If the method is foreign, its foreign thunk will handle the dynamic
  // dispatch for us.
  if (constant.isForeignToNativeThunk()) {
    if (!SGM.hasFunction(constant))
      SGM.emitForeignToNativeThunk(constant);
    return B.createFunctionRef(loc, SGM.getFunction(constant, NotForDefinition));
  }

  // Otherwise, we need a dynamic dispatch thunk.
  SILFunction *F = SGM.getDynamicThunk(constant, constantInfo);

  return B.createFunctionRef(loc, F);
}

void
SILGenFunction::emitVTableThunk(SILDeclRef derived, SILDeclRef base,
                                ArrayRef<VTableParamThunk> paramThunks,
                                VTableResultThunk resultThunk) {
  auto fd = cast<AbstractFunctionDecl>(derived.getDecl());

  SILLocation loc(fd);
  loc.markAutoGenerated();
  CleanupLocation cleanupLoc(fd);
  cleanupLoc.markAutoGenerated();
  Scope scope(Cleanups, cleanupLoc);

  // Collect the parameters.
  SmallVector<ManagedValue, 8> thunkArgs;
  SILValue indirectReturn;
  for (auto param : F.getLoweredFunctionType()->getParameters()) {
    auto paramTy = F.mapTypeIntoContext(param.getSILType());
    auto arg = new (F.getModule()) SILArgument(F.begin(), paramTy);
    if (param.getConvention() == ParameterConvention::Indirect_Out)
      indirectReturn = arg;
    else
      thunkArgs.push_back(emitManagedRValueWithCleanup(arg));
  }

  auto origFn = SGM.getFunction(derived, NotForDefinition);
  auto origTy = origFn->getLoweredFunctionType();

  // Process the arguments.
  SmallVector<SILValue, 8> origArgs;

  // Handle the indirect return, if we have one.
  if (indirectReturn.isValid()) {
    switch (resultThunk) {
    case VTableResultThunk::None:
      // Emit into the indirect return directly.
      origArgs.push_back(indirectReturn);
      break;

    case VTableResultThunk::MakeOptional:
      // Emit into the payload of the optional indirect return.
      OptionalTypeKind OTK;
      indirectReturn.getType().getSwiftRValueType()
        ->getAnyOptionalObjectType(OTK);
      auto payloadTy = F.mapTypeIntoContext(origTy->getSemanticResultSILType());
      auto returnPayload = B.createInitEnumDataAddr(loc, indirectReturn,
                                      getASTContext().getOptionalSomeDecl(OTK),
                                      payloadTy);
      origArgs.push_back(returnPayload);
      break;
    }
  }

  auto origParams = origTy->getParametersWithoutIndirectResult();
  for (unsigned i : indices(paramThunks)) {
    switch (paramThunks[i]) {
    // Forward this argument as-is.
    case VTableParamThunk::None:
      origArgs.push_back(thunkArgs[i].forward(*this));
      break;

    // Wrap this argument up in an optional.
    case VTableParamThunk::MakeOptional: {
      OptionalTypeKind OTK;
      origParams[i].getType()->getAnyOptionalObjectType(OTK);
      auto someDecl = getASTContext().getOptionalSomeDecl(OTK);
      auto optTy = F.mapTypeIntoContext(origParams[i].getSILType());
      if (thunkArgs[i].getType().isAddress()) {
        auto buf = emitTemporaryAllocation(loc, optTy);
        auto payload = B.createInitEnumDataAddr(loc, buf, someDecl,
                                                thunkArgs[i].getType());
        B.createCopyAddr(loc, thunkArgs[i].forward(*this), payload,
                         IsTake, IsInitialization);
        B.createInjectEnumAddr(loc, buf, someDecl);
        origArgs.push_back(buf);
      } else {
        auto some = B.createEnum(loc, thunkArgs[i].forward(*this),
                                 someDecl, optTy);
        origArgs.push_back(some);
      }
      break;
    }

    // Force-unwrap this optional argument.
    case VTableParamThunk::ForceIUO: {
      auto &tl = getTypeLowering(thunkArgs[i].getType());
      auto payload =
        emitCheckedGetOptionalValueFrom(loc, thunkArgs[i], tl, SGFContext());
      origArgs.push_back(payload.forward(*this));
      break;
    }
    }
  }

  // Call the underlying function.
  auto origFnRef = B.createFunctionRef(loc, origFn);
  auto subs = getForwardingSubstitutions();
  auto substTy = origTy->substGenericArgs(SGM.M, SGM.M.getSwiftModule(), subs);
  SILValue result = B.createApply(loc, origFnRef,
                              SILType::getPrimitiveObjectType(substTy),
                              origTy->getResult().getSILType(), subs, origArgs);

  // Process the result.
  switch (resultThunk) {
  case VTableResultThunk::None:
    break;
  case VTableResultThunk::MakeOptional:
    // Wrap the result in an optional.
    OptionalTypeKind OTK;
    if (indirectReturn) {
      indirectReturn.getType().getSwiftRValueType()
        ->getAnyOptionalObjectType(OTK);

      // We emitted the payload in-place above. We just need to inject
      // the tag.
      B.createInjectEnumAddr(loc, indirectReturn,
                             getASTContext().getOptionalSomeDecl(OTK));
    } else {
      // Wrap up the immediate value in an optional.
      auto thunkResultTy
        = F.getLoweredFunctionType()->getResult().getSILType();

      thunkResultTy.getSwiftRValueType()
        ->getAnyOptionalObjectType(OTK);

      result = B.createEnum(loc, result,
                            getASTContext().getOptionalSomeDecl(OTK),
                            thunkResultTy);
    }
    break;
  }

  scope.pop();

  B.createReturn(loc, result);
}

bool SILGenModule::requiresObjCMethodEntryPoint(FuncDecl *method) {
  // Property accessors should be generated alongside the property unless
  // the @NSManagedAttr attribute is present.
  if (method->isGetterOrSetter()) {
    auto asd = method->getAccessorStorageDecl();
    return asd->hasObjCGetterAndSetter() &&
           !asd->getAttrs().hasAttribute<NSManagedAttr>();
  }

  return method->isObjC() || method->getAttrs().hasAttribute<IBActionAttr>();
}

bool SILGenModule::requiresObjCMethodEntryPoint(ConstructorDecl *constructor) {
  return constructor->isObjC();
}

bool SILGenModule::requiresObjCDispatch(ValueDecl *vd) {
  // Final functions never require ObjC dispatch.
  if (vd->isFinal())
    return false;

  // If the decl is an @objc protocol requirement, then the only witness is
  // objc.
  if (auto *proto = dyn_cast<ProtocolDecl>(vd->getDeclContext()))
    if (proto->isObjC())
      return true;

  if (auto *fd = dyn_cast<FuncDecl>(vd)) {
    // If a function has an associated Clang node, it's foreign and only has
    // an ObjC entry point.
    if (vd->hasClangNode())
      return true;

    // Property accessors should be generated alongside the property.
    if (fd->isGetterOrSetter())
      return requiresObjCDispatch(fd->getAccessorStorageDecl());

    return fd->getAttrs().hasAttribute<DynamicAttr>();
  }
  if (auto *cd = dyn_cast<ConstructorDecl>(vd)) {
    // If a function has an associated Clang node, it's foreign and only has
    // an ObjC entry point.
    if (vd->hasClangNode())
      return true;
    
    return cd->getAttrs().hasAttribute<DynamicAttr>();
  }
  if (auto *asd = dyn_cast<AbstractStorageDecl>(vd))
    return asd->requiresObjCGetterAndSetter();

  return vd->getAttrs().hasAttribute<DynamicAttr>();
}

bool SILGenModule::requiresObjCSuperDispatch(ValueDecl *vd) {
  return requiresObjCDispatch(vd);
}

namespace {

/// An ASTVisitor for populating SILVTable entries from ClassDecl members.
class SILGenVTable : public Lowering::ASTVisitor<SILGenVTable> {
public:
  SILGenModule &SGM;
  ClassDecl *theClass;
  std::vector<SILVTable::Pair> vtableEntries;

  SILGenVTable(SILGenModule &SGM, ClassDecl *theClass)
    : SGM(SGM), theClass(theClass)
  {
    // Populate the superclass members, if any.
    Type super = theClass->getSuperclass();
    if (super && super->getClassOrBoundGenericClass())
      visitAncestor(super->getClassOrBoundGenericClass());
  }

  ~SILGenVTable() {
    // Create the vtable.
    SILVTable::create(SGM.M, theClass, vtableEntries);
  }

  void visitAncestor(ClassDecl *ancestor) {
    // Recursively visit all our ancestors.
    Type super = ancestor->getSuperclass();
    if (super && super->getClassOrBoundGenericClass())
      visitAncestor(super->getClassOrBoundGenericClass());

    // Only visit the members for a class defined natively.
    if (!ancestor->hasClangNode()) {
      for (auto member : ancestor->getMembers())
        visit(member);
    }
  }

  // Add an entry to the vtable.
  void addEntry(SILDeclRef member) {
    /// Get the function to reference from the vtable.
    auto getVtableEntryFn = [&](SILDeclRef entry) -> SILFunction* {
      // If the member is dynamic, reference its dynamic dispatch thunk so that
      // it will be redispatched, funneling the method call through the runtime
      // hook point.
      // TODO: Dynamic thunks could conceivably require reabstraction too.
      if (member.getDecl()->getAttrs().hasAttribute<DynamicAttr>())
        return SGM.getDynamicThunk(member, SGM.Types.getConstantInfo(member));

      // The derived method may require thunking to match up to the ABI of the
      // base method.
      return SGM.emitVTableMethod(member, entry);
    };

    // Try to find an overridden entry.
    // NB: Mutates vtableEntries in-place
    // FIXME: O(n^2)
    if (auto overridden = member.getOverriddenVTableEntry()) {
      for (SILVTable::Pair &entry : vtableEntries) {
        SILDeclRef ref = overridden;

        do {
          // Replace the overridden member.
          if (entry.first == ref) {
            // The entry is keyed by the least derived method.
            entry = {ref, getVtableEntryFn(ref)};
            return;
          }
        } while ((ref = ref.getOverridden()));
      }
      llvm_unreachable("no overridden vtable entry?!");
    }

    // If this is a final member and isn't overriding something, we don't need
    // to add it to the vtable.
    if (member.getDecl()->isFinal())
      return;
    // If this is dynamic and isn't overriding a non-dynamic method, it'll
    // always be accessed by objc_msgSend, so we don't need to add it to the
    // vtable.
    if (member.getDecl()->getAttrs().hasAttribute<DynamicAttr>())
      return;

    // Otherwise, introduce a new vtable entry.
    vtableEntries.emplace_back(member, getVtableEntryFn(member));
  }

  // Default for members that don't require vtable entries.
  void visitDecl(Decl*) {}

  void visitFuncDecl(FuncDecl *fd) {
    // ObjC decls don't go in vtables.
    if (fd->hasClangNode())
      return;

    // Observers don't get separate vtable entries.
    if (fd->isObservingAccessor())
      return;

    addEntry(SILDeclRef(fd));
  }

  void visitConstructorDecl(ConstructorDecl *cd) {
    // Stub constructors don't get an entry.
    if (cd->hasStubImplementation())
      return;

    // Required constructors (or overrides thereof) have their allocating entry
    // point in the vtable.
    bool isRequired = false;
    auto override = cd;
    while (override) {
      if (override->isRequired()) {
        isRequired = true;
        break;
      }
      override = override->getOverriddenDecl();
    }
    if (isRequired) {
      addEntry(SILDeclRef(cd, SILDeclRef::Kind::Allocator));
    }

    // All constructors have their initializing constructor in the
    // vtable, which can be used by a convenience initializer.
    addEntry(SILDeclRef(cd, SILDeclRef::Kind::Initializer));
  }

  void visitVarDecl(VarDecl *vd) {
    // Note: dynamically-dispatched properties have their getter and setter
    // added to the vtable when they are visited.
  }

  void visitDestructorDecl(DestructorDecl *dd) {
    if (dd->getParent()->isClassOrClassExtensionContext() == theClass) {
      // Add the deallocating destructor to the vtable just for the purpose
      // that it is referenced and cannot be eliminated by dead function removal.
      addEntry(SILDeclRef(dd, SILDeclRef::Kind::Deallocator));
    }
  }

  void visitSubscriptDecl(SubscriptDecl *sd) {
    // Note: dynamically-dispatched properties have their getter and setter
    // added to the vtable when they are visited.
  }
};

static void emitTypeMemberGlobalVariable(SILGenModule &SGM,
                                         GenericParamList *generics,
                                         NominalTypeDecl *theType,
                                         VarDecl *var) {
  assert(!generics && "generic static properties not implemented");
  if (var->getDeclContext()->isClassOrClassExtensionContext()) {
    assert(var->isFinal() && "only 'static' ('class final') stored properties are implemented in classes");
  }

  SGM.addGlobalVariable(var);
}


/// An ASTVisitor for generating SIL from method declarations
/// inside nominal types.
class SILGenType : public TypeMemberVisitor<SILGenType> {
public:
  SILGenModule &SGM;
  NominalTypeDecl *theType;
  Optional<SILGenVTable> genVTable;

  SILGenType(SILGenModule &SGM, NominalTypeDecl *theType)
    : SGM(SGM), theType(theType) {}

  /// Emit SIL functions for all the members of the type.
  void emitType() {
    // Start building a vtable if this is a class.
    if (auto theClass = dyn_cast<ClassDecl>(theType))
      genVTable.emplace(SGM, theClass);

    for (Decl *member : theType->getMembers()) {
      if (genVTable)
        genVTable->visit(member);

      visit(member);
    }

    for (Decl *member : theType->getDerivedGlobalDecls()) {
      SGM.visit(member);
    }

    // Emit witness tables for conformances of concrete types. Protocol types
    // are existential and do not have witness tables.
    if (isa<ProtocolDecl>(theType))
      return;

    for (auto *conformance : theType->getAllConformances(/*sorted=*/true)) {
      if (conformance->isComplete() &&
          isa<NormalProtocolConformance>(conformance))
        SGM.getWitnessTable(conformance);
    }
  }

  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitTypeAliasDecl(TypeAliasDecl *tad) {}
  void visitAbstractTypeParamDecl(AbstractTypeParamDecl *tpd) {}
  void visitModuleDecl(ModuleDecl *md) {}
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    ProfilerRAII Profiler(SGM, fd);
    SGM.emitFunction(fd);
    // FIXME: Default implementations in protocols.
    if (SGM.requiresObjCMethodEntryPoint(fd) &&
        !isa<ProtocolDecl>(fd->getDeclContext()))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
    ProfilerRAII Profiler(SGM, cd);
    SGM.emitConstructor(cd);

    if (SGM.requiresObjCMethodEntryPoint(cd) &&
        !isa<ProtocolDecl>(cd->getDeclContext()))
      SGM.emitObjCConstructorThunk(cd);
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    assert(isa<ClassDecl>(theType) && "destructor in a non-class type");
    ProfilerRAII Profiler(SGM, dd);
    SGM.emitDestructor(cast<ClassDecl>(theType), dd);
  }

  void visitEnumCaseDecl(EnumCaseDecl *ecd) {}
  void visitEnumElementDecl(EnumElementDecl *ued) {
    assert(isa<EnumDecl>(theType));
    SGM.emitEnumConstructor(ued);
  }

  void visitPatternBindingDecl(PatternBindingDecl *pd) {
    // Emit initializers for static variables.
    if (!pd->isStatic()) return;

    for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i)
      if (pd->getInit(i))
        SGM.emitGlobalInitialization(pd, i);
  }

  void visitVarDecl(VarDecl *vd) {
    // Collect global variables for static properties.
    // FIXME: We can't statically emit a global variable for generic properties.
    if (vd->isStatic() && vd->hasStorage()) {
      return emitTypeMemberGlobalVariable(SGM, theType->getGenericParams(),
                                          theType, vd);
    }

    visitAbstractStorageDecl(vd);
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *asd) {
    // FIXME: Default implementations in protocols.
    if (asd->hasObjCGetterAndSetter() &&
        !isa<ProtocolDecl>(asd->getDeclContext()))
      SGM.emitObjCPropertyMethodThunks(asd);
  }
};

} // end anonymous namespace

void SILGenModule::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(*this, ntd).emitType();
}

void SILGenFunction::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(SGM, ntd).emitType();
}

/// SILGenExtension - an ASTVisitor for generating SIL from method declarations
/// and protocol conformances inside type extensions.
class SILGenExtension : public TypeMemberVisitor<SILGenExtension> {
public:
  SILGenModule &SGM;

  SILGenExtension(SILGenModule &SGM)
    : SGM(SGM) {}

  /// Emit SIL functions for all the members of the extension.
  void emitExtension(ExtensionDecl *e) {
    for (Decl *member : e->getMembers())
      visit(member);

    if (!e->getExtendedType()->isExistentialType()) {
      // Emit witness tables for protocol conformances introduced by the
      // extension.
      for (auto *conformance : e->getLocalConformances()) {
        if (conformance->isComplete() &&
            isa<NormalProtocolConformance>(conformance))
          SGM.getWitnessTable(conformance);
      }
    }
  }

  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitTypeAliasDecl(TypeAliasDecl *tad) {}
  void visitAbstractTypeParamDecl(AbstractTypeParamDecl *tpd) {}
  void visitModuleDecl(ModuleDecl *md) {}
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    ProfilerRAII Profiler(SGM, fd);
    SGM.emitFunction(fd);
    if (SGM.requiresObjCMethodEntryPoint(fd))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
    ProfilerRAII Profiler(SGM, cd);
    SGM.emitConstructor(cd);
    if (SGM.requiresObjCMethodEntryPoint(cd))
      SGM.emitObjCConstructorThunk(cd);
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    llvm_unreachable("destructor in extension?!");
  }

  void visitPatternBindingDecl(PatternBindingDecl *pd) {
    // Emit initializers for static variables.
    if (!pd->isStatic()) return;

    for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i)
      if (pd->getInit(i))
        SGM.emitGlobalInitialization(pd, i);
  }

  void visitVarDecl(VarDecl *vd) {
    if (vd->isStatic() && vd->hasStorage()) {
      ExtensionDecl *ext = cast<ExtensionDecl>(vd->getDeclContext());
      NominalTypeDecl *theType = ext->getExtendedType()->getAnyNominal();
      return emitTypeMemberGlobalVariable(SGM, ext->getGenericParams(),
                                          theType, vd);
    }
    visitAbstractStorageDecl(vd);
  }

  void visitEnumCaseDecl(EnumCaseDecl *ecd) {}
  void visitEnumElementDecl(EnumElementDecl *ed) {
    llvm_unreachable("enum elements aren't allowed in extensions");
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *vd) {
    if (vd->hasObjCGetterAndSetter())
      SGM.emitObjCPropertyMethodThunks(vd);
  }
};

void SILGenModule::visitExtensionDecl(ExtensionDecl *ed) {
  SILGenExtension(*this).emitExtension(ed);
}

