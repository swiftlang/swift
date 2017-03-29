//===--- SILGenType.cpp - SILGen for types and their members --------------===//
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
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeMemberVisitor.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILVTableVisitor.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;


SILVTable::Entry
SILGenModule::emitVTableMethod(SILDeclRef derived, SILDeclRef base) {
  assert(base.kind == derived.kind);

  SILFunction *implFn;
  SILLinkage implLinkage;

  // If the member is dynamic, reference its dynamic dispatch thunk so that
  // it will be redispatched, funneling the method call through the runtime
  // hook point.
  if (derived.getDecl()->isDynamic()) {
    implFn = getDynamicThunk(derived, Types.getConstantInfo(derived));
    implLinkage = SILLinkage::Public;
  } else {
    implFn = getFunction(derived, NotForDefinition);
    implLinkage = stripExternalFromLinkage(implFn->getLinkage());
  }

  // As a fast path, if there is no override, definitely no thunk is necessary.
  if (derived == base)
    return {base, implFn, implLinkage};

  // Determine the derived thunk type by lowering the derived type against the
  // abstraction pattern of the base.
  auto baseInfo = Types.getConstantInfo(base);
  auto derivedInfo = Types.getConstantInfo(derived);
  auto basePattern = AbstractionPattern(baseInfo.LoweredInterfaceType);
  
  auto overrideInfo = M.Types.getConstantOverrideInfo(derived, base);

  // The override member type is semantically a subtype of the base
  // member type. If the override is ABI compatible, we do not need
  // a thunk.
  if (M.Types.checkFunctionForABIDifferences(derivedInfo.SILFnType,
                                             overrideInfo.SILFnType)
      == TypeConverter::ABIDifference::Trivial)
    return {base, implFn, implLinkage};

  // Generate the thunk name.
  std::string name;
  {
    Mangle::ASTMangler mangler;
    if (isa<FuncDecl>(base.getDecl())) {
      name = mangler.mangleVTableThunk(
        cast<FuncDecl>(base.getDecl()),
        cast<FuncDecl>(derived.getDecl()));
    } else {
      name = mangler.mangleConstructorVTableThunk(
        cast<ConstructorDecl>(base.getDecl()),
        cast<ConstructorDecl>(derived.getDecl()),
        base.kind == SILDeclRef::Kind::Allocator);
    }
  }

  // If we already emitted this thunk, reuse it.
  if (auto existingThunk = M.lookUpFunction(name))
    return {base, existingThunk, implLinkage};

  // Emit the thunk.
  auto *derivedDecl = cast<AbstractFunctionDecl>(derived.getDecl());
  SILLocation loc(derivedDecl);
  auto thunk =
      M.createFunction(SILLinkage::Private,
                       name, overrideInfo.SILFnType,
                       derivedDecl->getGenericEnvironment(), loc, IsBare,
                       IsNotTransparent, IsNotFragile);
  thunk->setDebugScope(new (M) SILDebugScope(loc, thunk));

  SILGenFunction(*this, *thunk)
    .emitVTableThunk(derived, implFn, basePattern,
                     overrideInfo.LoweredInterfaceType,
                     derivedInfo.LoweredInterfaceType);

  return {base, thunk, implLinkage};
}

bool SILGenModule::requiresObjCMethodEntryPoint(FuncDecl *method) {
  // Property accessors should be generated alongside the property unless
  // the @NSManaged attribute is present.
  if (method->isGetterOrSetter()) {
    auto asd = method->getAccessorStorageDecl();
    return asd->isObjC() && !asd->getAttrs().hasAttribute<NSManagedAttr>();
  }

  if (method->getAttrs().hasAttribute<NSManagedAttr>())
    return false;

  return method->isObjC() || method->getAttrs().hasAttribute<IBActionAttr>();
}

bool SILGenModule::requiresObjCMethodEntryPoint(ConstructorDecl *constructor) {
  return constructor->isObjC();
}

namespace {

/// An ASTVisitor for populating SILVTable entries from ClassDecl members.
class SILGenVTable : public SILVTableVisitor<SILGenVTable> {
public:
  SILGenModule &SGM;
  ClassDecl *theClass;

  // Map a base SILDeclRef to the corresponding element in vtableMethods.
  llvm::DenseMap<SILDeclRef, unsigned> baseToIndexMap;

  // For each base method, store the corresponding override.
  SmallVector<std::pair<SILDeclRef, SILDeclRef>, 8> vtableMethods;

  SILGenVTable(SILGenModule &SGM, ClassDecl *theClass)
    : SILVTableVisitor(SGM.Types), SGM(SGM), theClass(theClass)
  { }

  void emitVTable() {
    // Populate the superclass members, if any.
    visitAncestor(theClass);

    auto *dtor = theClass->getDestructor();
    assert(dtor);

    // Add the deallocating destructor to the vtable just for the purpose
    // that it is referenced and cannot be eliminated by dead function removal.
    // In reality, the deallocating destructor is referenced directly from
    // the HeapMetadata for the class.
    if (!dtor->hasClangNode())
      addMethod(SILDeclRef(dtor, SILDeclRef::Kind::Deallocator));

    if (SGM.requiresIVarDestroyer(theClass))
      addMethod(SILDeclRef(theClass, SILDeclRef::Kind::IVarDestroyer));

    SmallVector<SILVTable::Entry, 8> vtableEntries;
    vtableEntries.reserve(vtableMethods.size());

    for (auto method : vtableMethods) {
      SILDeclRef baseRef, derivedRef;
      std::tie(baseRef, derivedRef) = method;

      vtableEntries.push_back(SGM.emitVTableMethod(derivedRef, baseRef));
    }

    // Create the vtable.
    SILVTable::create(SGM.M, theClass, vtableEntries);
  }

  void visitAncestor(ClassDecl *ancestor) {
    auto superTy = ancestor->getSuperclass();
    if (superTy)
      visitAncestor(superTy->getClassOrBoundGenericClass());

    addVTableEntries(ancestor);
  }

  // Try to find an overridden entry.
  void addMethodOverride(SILDeclRef baseRef, SILDeclRef declRef) {
    auto found = baseToIndexMap.find(baseRef);
    assert(found != baseToIndexMap.end());
    auto &method = vtableMethods[found->second];
    assert(method.first == baseRef);
    method.second = declRef;
  }

  // Add an entry to the vtable.
  void addMethod(SILDeclRef member) {
    unsigned index = vtableMethods.size();
    vtableMethods.push_back(std::make_pair(member, member));
    auto result = baseToIndexMap.insert(std::make_pair(member, index));
    assert(result.second);
    (void) result;
  }
};

static void emitTypeMemberGlobalVariable(SILGenModule &SGM,
                                         VarDecl *var) {
  if (var->getDeclContext()->isGenericContext()) {
    assert(var->getDeclContext()->getGenericSignatureOfContext()
              ->areAllParamsConcrete()
           && "generic static vars are not implemented yet");
  }

  if (var->getDeclContext()->getAsClassOrClassExtensionContext()) {
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

  SILGenType(SILGenModule &SGM, NominalTypeDecl *theType)
    : SGM(SGM), theType(theType) {}

  /// Emit SIL functions for all the members of the type.
  void emitType() {
    for (Decl *member : theType->getMembers())
      visit(member);

    // Build a vtable if this is a class.
    if (auto theClass = dyn_cast<ClassDecl>(theType)) {
      SILGenVTable genVTable(SGM, theClass);
      genVTable.emitVTable();
    }

    // Build a default witness table if this is a protocol.
    if (auto protocol = dyn_cast<ProtocolDecl>(theType)) {
      if (!protocol->isObjC())
        SGM.emitDefaultWitnessTable(protocol);
      return;
    }

    // Emit witness tables for conformances of concrete types. Protocol types
    // are existential and do not have witness tables.
    for (auto *conformance : theType->getLocalConformances(
                               ConformanceLookupKind::All,
                               nullptr, /*sorted=*/true)) {
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
  void visitEnumElementDecl(EnumElementDecl *ued) {}

  void visitPatternBindingDecl(PatternBindingDecl *pd) {
    // Emit initializers.
    for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i) {
      if (pd->getInit(i)) {
        if (pd->isStatic())
          SGM.emitGlobalInitialization(pd, i);
        else
          SGM.emitStoredPropertyInitialization(pd, i);
      }
    }
  }

  void visitVarDecl(VarDecl *vd) {
    if (vd->hasBehavior())
      SGM.emitPropertyBehavior(vd);

    // Collect global variables for static properties.
    // FIXME: We can't statically emit a global variable for generic properties.
    if (vd->isStatic() && vd->hasStorage()) {
      return emitTypeMemberGlobalVariable(SGM, vd);
    }

    visitAbstractStorageDecl(vd);
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *asd) {
    // FIXME: Default implementations in protocols.
    if (asd->isObjC() && !isa<ProtocolDecl>(asd->getDeclContext()))
      SGM.emitObjCPropertyMethodThunks(asd);
  }
};

} // end anonymous namespace

void SILGenModule::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(*this, ntd).emitType();
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
      for (auto *conformance : e->getLocalConformances(
                                 ConformanceLookupKind::All,
                                 nullptr, /*sorted=*/true)) {
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
    for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i) {
      if (pd->getInit(i)) {
        assert(pd->isStatic() && "stored property in extension?!");
        SGM.emitGlobalInitialization(pd, i);
      }
    }
  }

  void visitVarDecl(VarDecl *vd) {
    if (vd->hasBehavior())
      SGM.emitPropertyBehavior(vd);
    if (vd->hasStorage()) {
      assert(vd->isStatic() && "stored property in extension?!");
      return emitTypeMemberGlobalVariable(SGM, vd);
    }
    visitAbstractStorageDecl(vd);
  }

  void visitEnumCaseDecl(EnumCaseDecl *ecd) {}
  void visitEnumElementDecl(EnumElementDecl *ed) {
    llvm_unreachable("enum elements aren't allowed in extensions");
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *vd) {
    if (vd->isObjC())
      SGM.emitObjCPropertyMethodThunks(vd);
  }
};

void SILGenModule::visitExtensionDecl(ExtensionDecl *ed) {
  SILGenExtension(*this).emitExtension(ed);
}
