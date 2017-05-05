//===--- TBDGen.cpp - Swift TBD Generation --------------------------------===//
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
//  This file implements the entrypoints into TBD file generation.
//
//===----------------------------------------------------------------------===//

#include "swift/TBDGen/TBDGen.h"

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/Basic/LLVM.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/StringSet.h"

using namespace swift;
using namespace swift::irgen;
using StringSet = llvm::StringSet<>;

static bool isPrivateDecl(ValueDecl *VD) {
  return getDeclLinkage(VD) != FormalLinkage::PublicUnique;
}

namespace {
class TBDGenVisitor : public ASTVisitor<TBDGenVisitor> {
  StringSet &Symbols;
  const UniversalLinkageInfo &UniversalLinkInfo;
  ModuleDecl *SwiftModule;
  bool FileHasEntryPoint;

  void addSymbol(StringRef name) {
    auto isNewValue = Symbols.insert(name).second;
    (void)isNewValue;
    assert(isNewValue && "already inserted");
  }

  void addSymbol(SILDeclRef declRef, bool checkSILOnly = true);

  void addSymbol(LinkEntity entity) {
    auto linkage =
        LinkInfo::get(UniversalLinkInfo, SwiftModule, entity, ForDefinition);

    auto externallyVisible =
        llvm::GlobalValue::isExternalLinkage(linkage.getLinkage()) &&
        linkage.getVisibility() != llvm::GlobalValue::HiddenVisibility;

    if (externallyVisible)
      addSymbol(linkage.getName());
  }

  void addConformances(DeclContext *DC) {
    for (auto conformance : DC->getLocalConformances()) {
      auto needsWTable = Lowering::TypeConverter::protocolRequiresWitnessTable(
          conformance->getProtocol());
      if (!needsWTable)
        continue;

      // Only normal conformances get symbols; the others get any public symbols
      // from their parent normal conformance.
      if (conformance->getKind() != ProtocolConformanceKind::Normal)
        continue;

      addSymbol(LinkEntity::forDirectProtocolWitnessTable(conformance));
      addSymbol(LinkEntity::forProtocolWitnessTableAccessFunction(conformance));
    }
  }

public:
  TBDGenVisitor(StringSet &symbols,
                const UniversalLinkageInfo &universalLinkInfo,
                ModuleDecl *swiftModule, bool fileHasEntryPoint)
      : Symbols(symbols), UniversalLinkInfo(universalLinkInfo),
        SwiftModule(swiftModule), FileHasEntryPoint(fileHasEntryPoint) {}

  void visitMembers(Decl *D) {
    SmallVector<Decl *, 4> members;
    auto addMembers = [&](DeclRange range) {
      for (auto member : range)
        members.push_back(member);
    };
    if (auto ED = dyn_cast<ExtensionDecl>(D))
      addMembers(ED->getMembers());
    else if (auto NTD = dyn_cast<NominalTypeDecl>(D))
      addMembers(NTD->getMembers());

    for (auto member : members) {
      ASTVisitor::visit(member);
    }
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD);

  void visitValueDecl(ValueDecl *VD);

  void visitAbstractFunctionDecl(AbstractFunctionDecl *AFD);

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    // any information here is encoded elsewhere
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    // Any getters and setters etc. exist as independent FuncDecls in the AST,
    // so get processed elsewhere; subscripts don't have any symbols other than
    // these.
  }

  void visitNominalTypeDecl(NominalTypeDecl *NTD);

  void visitClassDecl(ClassDecl *CD);

  void visitExtensionDecl(ExtensionDecl *ED);

  void visitProtocolDecl(ProtocolDecl *PD);

  void visitVarDecl(VarDecl *VD);

  void visitDecl(Decl *D) { visitMembers(D); }
};
} // end anonymous namespace

static bool isGlobalOrStaticVar(VarDecl *VD) {
  return VD->isStatic() || VD->getDeclContext()->isModuleScopeContext();
}

void TBDGenVisitor::visitPatternBindingDecl(PatternBindingDecl *PBD) {
  for (auto &entry : PBD->getPatternList()) {
    auto *var = entry.getAnchoringVarDecl();
    if (isPrivateDecl(var))
      return;

    // Non-global variables might have an explicit initializer symbol.
    if (entry.getInit() && !isGlobalOrStaticVar(var)) {
      auto declRef =
          SILDeclRef(var, SILDeclRef::Kind::StoredPropertyInitializer);
      // Stored property initializers for public properties are currently
      // public, even when the initializer is marked as SIL only (transparent).
      addSymbol(declRef, /*checkSILOnly=*/false);
    }
  }
}

void TBDGenVisitor::addSymbol(SILDeclRef declRef, bool checkSILOnly) {
  bool isPrivate = !hasPublicVisibility(declRef.getLinkage(ForDefinition));
  // Even private methods of open classes (specifically, private methods that
  // are in the vtable) have public symbols, because external subclasses
  // currently need to refer to them by symbol for their own vtable.
  switch (declRef.getSubclassScope()) {
  case SubclassScope::External:
    // Allocating constructors retain their normal linkage behavior.
    if (declRef.kind == SILDeclRef::Kind::Allocator)
      break;

    // Unlike the "truly" public things, private things have public symbols
    // unconditionally, even if they're theoretically SIL only.
    if (isPrivate) {
      isPrivate = false;
      checkSILOnly = false;
    }
    break;
  case SubclassScope::Internal:
  case SubclassScope::NotApplicable:
    break;
  }
  if (isPrivate)
    return;

  // (Most) transparent things don't exist, even if they're public.
  // FIXME: isTransparent should really be "is SIL only".
  if (checkSILOnly && declRef.isTransparent())
    return;

  addSymbol(declRef.mangle());
}

void TBDGenVisitor::visitValueDecl(ValueDecl *VD) {
  addSymbol(SILDeclRef(VD));
  visitMembers(VD);
}

void TBDGenVisitor::visitAbstractFunctionDecl(AbstractFunctionDecl *AFD) {
  // Default arguments (of public functions) are public symbols, as the default
  // values are computed at the call site.
  auto index = 0;
  auto paramLists = AFD->getParameterLists();
  // Skip the first arguments, which contains Self (etc.), can't be defaulted,
  // and are ignored for the purposes of default argument indices.
  if (AFD->getDeclContext()->isTypeContext())
    paramLists = paramLists.slice(1);
  for (auto *paramList : paramLists) {
    for (auto *param : *paramList) {
      if (param->getDefaultValue())
        addSymbol(SILDeclRef::getDefaultArgGenerator(AFD, index));
      index++;
    }
  }

  visitValueDecl(AFD);
}

void TBDGenVisitor::visitVarDecl(VarDecl *VD) {
  if (isPrivateDecl(VD))
    return;

  // statically/globally stored variables have some special handling.
  if (VD->hasStorage() && isGlobalOrStaticVar(VD)) {
    // The actual variable has a symbol.
    Mangle::ASTMangler mangler;
    addSymbol(mangler.mangleEntity(VD, false));

    // Variables in the main file don't get accessors, despite otherwise looking
    // like globals.
    if (!FileHasEntryPoint)
      addSymbol(SILDeclRef(VD, SILDeclRef::Kind::GlobalAccessor));
  }

  visitMembers(VD);
}

void TBDGenVisitor::visitNominalTypeDecl(NominalTypeDecl *NTD) {
  auto declaredType = NTD->getDeclaredType()->getCanonicalType();

  addSymbol(LinkEntity::forNominalTypeDescriptor(NTD));

  // Generic types do not get metadata directly, only through the function.
  if (!NTD->isGenericContext()) {
    addSymbol(LinkEntity::forTypeMetadata(declaredType,
                                          TypeMetadataAddress::AddressPoint,
                                          /*isPattern=*/false));
  }
  addSymbol(LinkEntity::forTypeMetadataAccessFunction(declaredType));

  // There are symbols associated with any protocols this type conforms to.
  addConformances(NTD);

  visitMembers(NTD);
}

void TBDGenVisitor::visitClassDecl(ClassDecl *CD) {
  if (isPrivateDecl(CD))
    return;

  auto &ctxt = CD->getASTContext();
  auto isGeneric = CD->isGenericContext();
  auto objCCompatible = ctxt.LangOpts.EnableObjCInterop && !isGeneric;
  auto isObjC = objCCompatible && CD->isObjC();

  // Metaclasses and ObjC class (duh) are a ObjC thing, and so are not needed in
  // build artifacts/for classes which can't touch ObjC.
  if (objCCompatible) {
    if (isObjC)
      addSymbol(LinkEntity::forObjCClass(CD));

    if (CD->getMetaclassKind() == ClassDecl::MetaclassKind::ObjC)
      addSymbol(LinkEntity::forObjCMetaclass(CD));
    else
      addSymbol(LinkEntity::forSwiftMetaclassStub(CD));
  }

  // Some members of classes get extra handling, beyond members of struct/enums,
  // so let's walk over them manually.
  for (auto *member : CD->getMembers()) {
    auto value = dyn_cast<ValueDecl>(member);
    if (!value)
      continue;

    auto var = dyn_cast<VarDecl>(value);
    auto hasFieldOffset =
        !isGeneric && var && var->hasStorage() && !var->isStatic();
    if (hasFieldOffset) {
      // Field are only direct if the class's internals are completely known.
      auto isIndirect = !CD->hasFixedLayout();
      addSymbol(LinkEntity::forFieldOffset(var, isIndirect));
    }

    // The non-allocating forms of the constructors and destructors.
    if (auto ctor = dyn_cast<ConstructorDecl>(value)) {
      addSymbol(SILDeclRef(ctor, SILDeclRef::Kind::Initializer));
    } else if (auto dtor = dyn_cast<DestructorDecl>(value)) {
      // ObjC classes don't have a symbol for their destructor.
      if (!isObjC)
        addSymbol(SILDeclRef(dtor, SILDeclRef::Kind::Destroyer));
    }
  }

  visitNominalTypeDecl(CD);
}

void TBDGenVisitor::visitExtensionDecl(ExtensionDecl *ED) {
  if (!ED->getExtendedType()->isExistentialType()) {
    addConformances(ED);
  }

  visitMembers(ED);
}

void TBDGenVisitor::visitProtocolDecl(ProtocolDecl *PD) {
  if (!PD->isObjC())
    addSymbol(LinkEntity::forProtocolDescriptor(PD));

#ifndef NDEBUG
  // There's no (currently) relevant information about members of a protocol
  // at individual protocols, each conforming type has to handle them
  // individually. Let's assert this fact:
  for (auto *member : PD->getMembers()) {
    auto isExpectedKind =
        isa<TypeAliasDecl>(member) || isa<AssociatedTypeDecl>(member) ||
        isa<AbstractStorageDecl>(member) || isa<PatternBindingDecl>(member) ||
        isa<AbstractFunctionDecl>(member);
    assert(isExpectedKind &&
           "unexpected member of protocol during TBD generation");
  }
#endif
}

void swift::enumeratePublicSymbols(FileUnit *file, StringSet &symbols,
                                   bool hasMultipleIRGenThreads,
                                   bool isWholeModule) {
  UniversalLinkageInfo linkInfo(file->getASTContext().LangOpts.Target,
                                hasMultipleIRGenThreads, isWholeModule);

  SmallVector<Decl *, 16> decls;
  file->getTopLevelDecls(decls);

  auto hasEntryPoint = file->hasEntryPoint();

  TBDGenVisitor visitor(symbols, linkInfo, file->getParentModule(),
                        hasEntryPoint);
  for (auto d : decls)
    visitor.visit(d);

  if (hasEntryPoint)
    symbols.insert("main");
}
