//===--- TypeCheckCOM.cpp - Type checking for COM interop -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TypeCheckCOM.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ArgumentList.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/StorageImpl.h"
#include "swift/AST/SynthesizedDeclBuilder.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/UUID.h"
#include "llvm/ADT/StringExtras.h"


namespace {
namespace com {

using namespace swift;

/// The protocol an `@com` class conforms to as its COM root under \p model, or
/// none when no model is in effect.
std::optional<KnownProtocolKind>
getRootProtocol(std::optional<LangOptions::COMInteropModel> model) {
  if (!model)
    return std::nullopt;

  switch (*model) {
  case LangOptions::COMInteropModel::Microsoft:
    return KnownProtocolKind::IUnknown;
  case LangOptions::COMInteropModel::CoreFoundation:
    return std::nullopt;
  }
  llvm_unreachable("unhandled COMInteropModel");
}

/// Look up a type by name from the \c COM module.  Emits a diagnostic on
/// failure.
TypeDecl *lookup(ASTContext &ASTContext, DeclContext *DC, Identifier name,
                 SourceLoc loc) {
  auto *COM = ASTContext.getLoadedModule(ASTContext.Id_COM);
  if (COM && !ASTContext.getImportCache().isImportedBy(COM, DC))
    COM = nullptr;
  if (!COM) {
    // When building the COM module itself, look up types locally.
    if (ASTContext.MainModule &&
        ASTContext.MainModule->getName() == ASTContext.Id_COM)
      COM = ASTContext.MainModule;
  }
  if (!COM) {
    ASTContext.Diags.diagnose(loc, diag::attr_com_missing_module);
    return nullptr;
  }

  SmallVector<ValueDecl *, 1> results;
  ASTContext.lookupInModule(COM, name.str(), results);
  if (results.empty() || !isa<TypeDecl>(results.front())) {
    ASTContext.Diags.diagnose(loc, diag::com_module_missing_type, name.str());
    return nullptr;
  }

  return cast<TypeDecl>(results.front());
}

/// Body synthesizer for a GUID getter.  Produces:
/// \code
///   return ID(data1: 0x…, data2: 0x…, data3: 0x…,
///             data4: (0x…, 0x…, 0x…, 0x…, 0x…, 0x…, 0x…, 0x…))
/// \endcode
std::pair<BraceStmt *, bool>
materializeGUID(AbstractFunctionDecl *AFD, void *context) {
  auto &ASTContext = AFD->getASTContext();

  std::optional<UUID> uuid =
      UUID::fromString(static_cast<const char *>(context));
  ASSERT(uuid && "GUID string should have been validated by Sema");
  // Not uuid->Value: it is native-endian on a Windows host, which would bake
  // byte-swapped field literals. The field values must not depend on the host.
  unsigned char V[UUID::Size];
  uuid->getCanonicalBytes(V);

  auto hex = [&](uint64_t value) -> IntegerLiteralExpr * {
    auto literal = ASTContext.AllocateCopy("0x" + llvm::utohexstr(value));
    return new (ASTContext) IntegerLiteralExpr(literal, {}, /*implicit=*/true);
  };

  // RFC 4122 network byte order -> COM GUID fields.
  Expr *data1 = hex((uint32_t(V[0]) << 24) | (V[1] << 16) | (V[2] << 8) | V[3]);
  Expr *data2 = hex((uint16_t(V[4]) << 8) | V[5]);
  Expr *data3 = hex((uint16_t(V[6]) << 8) | V[7]);

  SmallVector<Expr *, 8> elements;
  for (unsigned i = 0; i < 8; ++i)
    elements.push_back(hex(V[8 + i]));
  Expr *data4 = TupleExpr::createImplicit(ASTContext, elements, {});

  Argument args[] = {
      {SourceLoc(), ASTContext.getIdentifier("data1"), data1},
      {SourceLoc(), ASTContext.getIdentifier("data2"), data2},
      {SourceLoc(), ASTContext.getIdentifier("data3"), data3},
      {SourceLoc(), ASTContext.getIdentifier("data4"), data4},
  };

  Type DeclTy = cast<AccessorDecl>(AFD)->getResultInterfaceType();
  CallExpr *call =
      CallExpr::createImplicit(ASTContext,
                               TypeExpr::createImplicit(DeclTy, ASTContext),
                               ArgumentList::createImplicit(ASTContext, args));

  return {BraceStmt::create(ASTContext, SourceLoc(),
                            ASTNode(ReturnStmt::createImplicit(ASTContext, call)),
                            SourceLoc()),
          /*isTypeChecked=*/false};
}

/// Synthesize a computed GUID property named \p identifier holding \p value;
/// its type is the same-named \c GUID specialization (\c IID or \c CLSID).
/// The property is created in \p DC and copies its formal access from \p decl,
/// the interface or class the ID belongs to. For a \c CLSID these coincide;
/// for an \c IID the property lives in a metatype extension, so \p DC is that
/// extension while \p decl is the protocol.
VarDecl *generateIDAccessor(ASTContext &ASTContext, DeclContext *DC,
                            ValueDecl *decl, Identifier identifier,
                            StringRef value, bool isStatic, bool aeic) {
  TypeDecl *Ty =
      lookup(ASTContext, decl->getDeclContext(), identifier, decl->getLoc());
  if (!Ty)
    return nullptr;
  Type GUIDTy = Ty->getDeclaredInterfaceType();

  // Null-terminate: UUID::fromString expects a C string.
  std::string str = value.str();
  StringRef uuid{str.c_str(), str.size() + 1};
  StringRef stored = ASTContext.AllocateCopy(uuid);

  // `decl` is the property's parent context only when the property is created
  // directly in it (the CLSID case); the IID's parent is the extension.
  bool enclosing = decl == DC->getAsDecl();

  return VarDeclBuilder(DC, identifier)
      .introducer(VarDecl::Introducer::Var)
      .static_(isStatic)
      .type(GUIDTy)
      .readAccess(formalAccessForSynthesizedMember(decl, enclosing))
      .usableFromInline(decl->getAttrs().hasAttribute<UsableFromInlineAttr>())
      .getter(materializeGUID, const_cast<char *>(stored.data()))
      .alwaysEmitIntoClient(aeic);
}

/// Synthesize \c var \c IID: \c GUID in an \c extension \c P.Protocol for a
/// \c @com protocol.  The getter is \c @_alwaysEmitIntoClient.
///
/// The IID lives on the protocol metatype rather than on conforming types:
/// it is the identity of the interface itself, so it is reached as
/// `P.IID` and is not inherited by types that conform to `P`.  This is
/// exactly what a protocol metatype extension provides, so the synthesized
/// extension is marked as one.  Because the extended type is `P.Protocol`
/// (the metatype), the accessor is a non-static instance member of the
/// metatype, which the relaxed static-member rule for metatype extensions
/// permits.
VarDecl *synthesizeIIDProperty(ProtocolDecl *PD, ASTContext &ASTContext,
                               StringRef value) {
  // Create an implicit `extension P.Protocol`.
  ExtensionDecl *ext =
      ExtensionDecl::create(ASTContext, SourceLoc(), nullptr, {},
                            PD->getModuleScopeContext(), nullptr);
  ext->setImplicit();
  // The extended type is the protocol metatype `(any P).Type`; that is what
  // marks this as a protocol metatype extension; there is no separate flag.
  ASTContext.evaluator.cacheOutput(
      ExtendedTypeRequest{ext},
      MetatypeType::get(ExistentialType::get(PD->getDeclaredInterfaceType())));
  ext->setExtendedNominal(PD);
  PD->addExtension(ext);

  // Route the extension through the synthesized file unit so it is code-gen'd
  // and serialized: the `IID` accessor must be emitted for the current module
  // and reachable by name lookup in modules that import it.  Synthesis only
  // happens for source-file protocols (see SynthesizeCOMInterfaceIDRequest);
  // imported protocols recover the extension from the deserialized module.
  if (auto *file = dyn_cast<FileUnit>(PD->getModuleScopeContext()))
    file->getOrCreateSynthesizedFile().addTopLevelDecl(ext);

  VarDecl *property =
      generateIDAccessor(ASTContext, /*DC=*/ext, /*decl=*/PD,
                         /*identifier=*/ASTContext.Id_IID, value,
                         /*isStatic=*/false, /*aeic=*/true);
  if (!property)
    return nullptr;

  PatternBindingDecl *binding = PatternBindingDeclBuilder(property);
  ext->addMember(property);
  ext->addMember(binding);
  return property;
}

/// Synthesize \c static \c var \c CLSID: \c GUID on a \c @com class.
VarDecl *synthesizeCLSIDProperty(ClassDecl *CD, ASTContext &ASTContext,
                                 StringRef value) {
  VarDecl *property =
      generateIDAccessor(ASTContext, /*DC=*/CD, /*decl=*/CD,
                         /*identifier=*/ASTContext.Id_CLSID, value,
                         /*isStatic=*/true, /*aeic=*/false);
  if (!property)
    return nullptr;

  PatternBindingDecl *binding = PatternBindingDeclBuilder(property);
  CD->addMember(property);
  CD->addMember(binding);
  return property;
}

/// Recover the synthesized `IID` accessor for an imported or swiftinterface
/// `@com` protocol: it lives in the deserialized metatype extension, so find it
/// by name lookup rather than re-synthesizing.
VarDecl *lookupCOMInterfaceID(ProtocolDecl *PD) {
  for (auto *ref : PD->lookupDirect(PD->getASTContext().Id_IID)) {
    auto *prop = dyn_cast<VarDecl>(ref);
    if (!prop)
      continue;
    if (auto *ext = dyn_cast<ExtensionDecl>(prop->getDeclContext()))
      if (ext->isMetatypeExtension())
        return prop;
  }
  return nullptr;
}

/// Recover the synthesized `CLSID` accessor for an imported or swiftinterface
/// `@com` class.
VarDecl *lookupCOMImplementationID(ClassDecl *CD) {
  for (auto *ref : CD->lookupDirect(CD->getASTContext().Id_CLSID)) {
    auto *prop = dyn_cast<VarDecl>(ref);
    if (prop && prop->getDeclContext() == CD)
      return prop;
  }
  return nullptr;
}

const COMAttr *getAttribute(const ClassDecl *CD) {
  auto *attr = CD->getAttrs().getAttribute<COMAttr>();
  if (!attr)
    return nullptr;

  ASSERT(attr->IID.empty());
  ASSERT(!attr->CLSID || !attr->CLSID->empty());
  return attr;
}

const COMAttr *getAttribute(const ProtocolDecl *PD) {
  auto *attr = PD->getAttrs().getAttribute<COMAttr>();
  if (!attr)
    return nullptr;

  ASSERT(!attr->IID.empty());
  ASSERT(!attr->CLSID);
  return attr;
}

/// Select the unique most-derived interface among a set of comparable COM
/// bases. Unrelated bases cannot share one interface pointer and are invalid.
std::optional<ProtocolDecl *>
resolveABIBase(ProtocolDecl *protocol,
               ArrayRef<std::pair<ProtocolDecl *, SourceLoc>> bases) {
  ASSERT(!bases.empty());
  ProtocolDecl *selection = nullptr;
  bool invalid = false;
  ASTContext &C = protocol->getASTContext();

  for (auto [base, loc] : bases) {
    auto *hierarchy = base->getCOMInterfaceHierarchy();
    if (!hierarchy || hierarchy->isInvalid()) {
      invalid = true;
    } else if (!selection || base->inheritsFrom(selection)) {
      selection = base;
    } else if (selection != base && !selection->inheritsFrom(base)) {
      C.Diags.diagnose(loc, diag::com_interface_multiple_abi_bases,
                       protocol->getName(), selection->getName(),
                       base->getName());
      invalid = true;
    }
  }

  if (invalid)
    return std::nullopt;
  ASSERT(selection);
  return selection;
}

ProtocolDecl *findInterface(ArrayRef<ProtocolDecl *> chain,
                            StringRef iid) {
  for (auto *interface : chain) {
    auto *info = interface->getCOMDeclInfo();
    ASSERT(info && info->isInterface());
    if (iid.equals_insensitive(info->getInterfaceID()))
      return interface;
  }
  return nullptr;
}

}
}

namespace swift {
const COMDeclInfo *
COMDeclInfoRequest::evaluate(Evaluator &evaluator,
                             NominalTypeDecl *nominal) const {
  auto &ctx = nominal->getASTContext();

  if (auto *PD = dyn_cast<ProtocolDecl>(nominal)) {
    auto *attr = ::com::getAttribute(PD);
    if (!attr)
      return nullptr;

    return ctx.AllocateObjectCopy(COMDeclInfo::forInterface(attr->IID));
  } else if (auto *CD = dyn_cast<ClassDecl>(nominal)) {
    ProtocolDecl *root = nullptr;
    if (auto interface = ::com::getRootProtocol(ctx.LangOpts.COMModel))
      root = ctx.getProtocol(*interface);

    SmallVector<ProtocolDecl *, 2> interfaces;
    for (auto *proto : CD->getAllProtocols(/*sorted=*/true)) {
      if (proto->isCOMInterface() && proto != root &&
          !proto->isSpecificProtocol(KnownProtocolKind::ISwiftObject))
        interfaces.push_back(proto);
    }

    const COMDeclInfo *superInfo = nullptr;
    if (auto *superclass = CD->getSuperclassDecl()) {
      auto *info = superclass->getCOMDeclInfo();
      if (info && info->isImplementation())
        superInfo = info;
    }

    auto *attr = ::com::getAttribute(CD);
    if (!attr && interfaces.empty() && !superInfo)
      return nullptr;

    SmallVector<ProtocolDecl *, 2> antichain;
    if (superInfo) {
      llvm::append_range(antichain, superInfo->getInterfaceSlots());
    } else {
      // Keep the compiler-managed Swift identity interface at the stable
      // position closest to the Swift heap-object address point.
      ProtocolDecl *ISO = ctx.getProtocol(KnownProtocolKind::ISwiftObject);
      ASSERT(ISO);
      antichain.push_back(ISO);
    }

    // Preserve every superclass interface position. A more-derived interface
    // in an existing refinement chain replaces that position; a new
    // independent chain is appended. This keeps an interface value formed from
    // a statically typed superclass reference valid for every subclass
    // instance.
    for (ProtocolDecl *interface : interfaces) {
      if (llvm::any_of(interfaces, [&](const ProtocolDecl *PD) {
                        return PD != interface && PD->inheritsFrom(interface);
                       }))
          continue;

      bool inserted = false;
      for (auto &PD : drop_begin(antichain)) {
        if (interface == PD || PD->inheritsFrom(interface)) {
          inserted = true;
          break;
        }
        if (interface->inheritsFrom(PD)) {
          PD = interface;
          inserted = true;
          break;
        }
      }

      if (!inserted)
        antichain.push_back(interface);
    }

    StringRef implementationID;
    std::optional<COMThreadingModel> threadingModel;
    if (attr) {
      if (attr->CLSID)
        implementationID = *attr->CLSID;
      threadingModel = attr->getThreadingModel();
    }

    return ctx.AllocateObjectCopy(COMDeclInfo::forImplementation(
        implementationID, threadingModel, root, ctx.AllocateCopy(interfaces),
        ctx.AllocateCopy(antichain)));
  }

  return nullptr;
}

const COMInterfaceHierarchy *
COMInterfaceHierarchyRequest::evaluate(Evaluator &evaluator,
                                       ProtocolDecl *protocol) const {
  auto &C = protocol->getASTContext();
  auto *info = protocol->getCOMDeclInfo();

  // Ordinary circular-inheritance checking owns the diagnostic. Do not start
  // a recursive COM hierarchy walk in that case.
  if (protocol->hasCircularInheritedProtocols()) {
    if (!info)
      return nullptr;
    return C.AllocateObjectCopy(COMInterfaceHierarchy::invalid());
  }

  SmallVector<InheritedNominalEntry, 4> inherited;
  if (protocol->wasDeserialized()) {
    for (auto *base : protocol->getInheritedProtocols()) {
      inherited.emplace_back(base, SourceLoc(), /*inheritedTypeRepr=*/nullptr,
                             ConformanceAttributes(),
                             /*isSuppressed=*/false);
    }
  } else {
    // Class constraints do not participate in the COM interface hierarchy.
    bool ignoredAnyObject = false;
    InvertibleProtocolSet inverses;
    inherited = getDirectlyInheritedNominalTypeDecls(protocol, inverses,
                                                      ignoredAnyObject);
  }

  // A protocol that refines a COM interface must itself introduce a COM
  // identity. A malformed @com attribute already has its own diagnostic, so
  // do not obscure it with this follow-on error.
  if (!info) {
    if (protocol->getAttrs().hasAttribute<COMAttr>(
            /*AllowInvalid=*/true))
      return nullptr;

    for (const auto &entry : inherited) {
      auto *PD = dyn_cast<ProtocolDecl>(entry.Item);
      if (PD && PD->isCOMInterface()) {
        C.Diags.diagnose(entry.Loc,
                         diag::com_interface_refinement_requires_identity,
                         protocol->getName(), PD->getName());
        break;
      }
    }

    return nullptr;
  }

  bool invalid = false;

  SmallVector<ProtocolDecl *, 4> markers;
  auto record = [&](ProtocolDecl *marker) {
    if (!llvm::is_contained(markers, marker))
      markers.push_back(marker);
  };

  SmallVector<std::pair<ProtocolDecl *, SourceLoc>, 2> bases;

  for (const auto &entry : inherited) {
    auto *PD = dyn_cast<ProtocolDecl>(entry.Item);
    if (!PD) {
      C.Diags.diagnose(entry.Loc, diag::com_interface_inherits_non_protocol,
                       protocol->getName(),
                       entry.Item->getDeclaredInterfaceType());
      invalid = true;
    } else if (PD->isCOMInterface()) {
      bases.emplace_back(PD, entry.Loc);
    } else if (PD->isMarkerProtocol()) {
      record(PD);
      for (auto *marker : PD->getAllInheritedProtocols()) {
        if (marker->isMarkerProtocol())
          record(marker);
      }
    } else {
      C.Diags.diagnose(entry.Loc, diag::com_interface_inherits_non_marker,
                       protocol->getName(), PD->getName());
      invalid = true;
    }
  }

  ProtocolDecl *base = nullptr;
  if (!bases.empty()) {
    if (const auto resolved = ::com::resolveABIBase(protocol, bases))
      base = *resolved;
    else
      invalid = true;
  }

  if (invalid)
    return C.AllocateObjectCopy(COMInterfaceHierarchy::invalid());

  SmallVector<ProtocolDecl *, 4> chain;
  // A root interface has no COM base; its ABI chain starts with the current
  // interface when it is appended below.
  if (base) {
    auto *hierarchy = base->getCOMInterfaceHierarchy();
    ASSERT(hierarchy && !hierarchy->isInvalid());
    for (auto *marker : hierarchy->getMarkerProtocols())
      record(marker);
    llvm::append_range(chain, hierarchy->getABIChain());
  }
  llvm::sort(markers, [](ProtocolDecl *lhs, ProtocolDecl *rhs) {
    return TypeDecl::compare(lhs, rhs) < 0;
  });

  // An IID denotes one logical interface in an ABI chain. Reusing it for a
  // derived declaration would make QueryInterface unable to distinguish the
  // two layouts.
  if (auto *interface = ::com::findInterface(chain, info->getInterfaceID())) {
    auto *attr = protocol->getAttrs().getAttribute<COMAttr>();
    SourceLoc loc = attr ? attr->getLocation() : protocol->getLoc();
    C.Diags.diagnose(loc, diag::com_interface_repeated_iid, protocol->getName(),
                     interface->getName(), info->getInterfaceID());
    return C.AllocateObjectCopy(COMInterfaceHierarchy::invalid());
  }

  chain.push_back(protocol);
  return C.AllocateObjectCopy(
      COMInterfaceHierarchy(C.AllocateCopy(markers), C.AllocateCopy(chain)));
}

void com::validateImplementation(ClassDecl *CD) {
  auto *info = CD->getCOMDeclInfo();
  if (!info)
    return;

  if (CD->isActor())
    CD->diagnose(diag::com_actor_implementation, CD->getName());

  if (CD->getObjectModel() != ReferenceCounting::Native)
    CD->diagnose(diag::com_non_native_implementation, CD->getName());

  if (CD->isGenericContext() && info->getImplementationID())
    CD->diagnose(diag::com_generic_activatable_implementation, CD->getName());
}

void com::validateConformance(ProtocolConformance *conformance) {
  auto *normal = dyn_cast<NormalProtocolConformance>(conformance);
  if (!normal ||
      normal->getSourceKind() != ConformanceEntryKind::Explicit)
    return;

  auto &diagnostics = normal->getDeclContext()->getASTContext().Diags;

  auto *protocol = normal->getProtocol();
  if (protocol->isSpecificProtocol(KnownProtocolKind::COMInterface) ||
      protocol->isSpecificProtocol(KnownProtocolKind::COMActivatable)) {
    diagnostics.diagnose(normal->getLoc(),
                         diag::com_identity_explicit_conformance,
                         protocol->getName());
    normal->setInvalid();
    return;
  }

  if (!protocol->isCOMInterface())
    return;

  Type type = normal->getType();
  auto *nominal = type->getAnyNominal();
  auto *CD = dyn_cast_or_null<ClassDecl>(nominal);
  if (!CD) {
    diagnostics.diagnose(normal->getLoc(), diag::com_conformance_requires_class,
                         type, protocol->getDeclaredInterfaceType());
    return;
  }

  auto *typeModule = CD->getParentModule();
  auto *conformanceModule = normal->getDeclContext()->getParentModule();
  if (!typeModule->isSameModuleLookingThroughOverlays(conformanceModule))
    diagnostics.diagnose(normal->getLoc(),
                         diag::com_conformance_must_be_in_type_module, type,
                         protocol->getDeclaredInterfaceType());

  if (!normal->getConditionalRequirements().empty())
    diagnostics.diagnose(normal->getLoc(), diag::com_conditional_conformance,
                         type, protocol->getDeclaredInterfaceType());
}

void com::validateIdentityProtocol(ProtocolDecl *PD) {
  bool interface = PD->isSpecificProtocol(KnownProtocolKind::COMInterface);
  bool activatable = PD->isSpecificProtocol(KnownProtocolKind::COMActivatable);
  if (!interface && !activatable)
    return;

  VarDecl *identity = nullptr;
  for (auto *requirement : PD->getProtocolRequirements()) {
    auto kind = classifyCOMIdentityRequirement(requirement);
    if (!kind) {
      requirement->diagnose(diag::com_identity_unsupported_requirement,
                            requirement->getName(), PD->getName());
      PD->setInvalid();
      continue;
    }

    switch (*kind) {
    case COMIdentityRequirementKind::InterfaceID:
    case COMIdentityRequirementKind::ClassID:
      identity = dyn_cast<VarDecl>(requirement);
      break;
    }
  }

  auto &context = PD->getASTContext();
  Identifier ident = interface ? context.Id_IID : context.Id_CLSID;
  auto *decl = ::com::lookup(context, PD->getDeclContext(), ident, PD->getLoc());

  bool hasValidIdentity =
      identity && !identity->isStatic() && !identity->isSettable(nullptr) &&
      decl && identity->getValueInterfaceType()->isEqual(decl->getDeclaredInterfaceType());
  if (!hasValidIdentity) {
    PD->diagnose(diag::com_identity_invalid_requirement,
                 PD->getName().str(), ident.str());
    PD->setInvalid();
  }
}

ProtocolConformance *
com::deriveImplicitConformance(NominalTypeDecl *NTD, KnownProtocolKind KP) {
  const auto *CD = dyn_cast<ClassDecl>(NTD);
  if (CD == nullptr)
    return nullptr;

  auto *info = CD->getCOMDeclInfo();
  if (!info || !info->isImplementation())
    return nullptr;

  ASTContext &context = NTD->getASTContext();
  auto *protocol = context.getProtocol(KP);
  if (protocol == nullptr)
    return nullptr;

  // Synthesize the COM root conformance the interop model selects.
  // `ISwiftObject` is compiler-managed and synthesized regardless.
  llvm::SmallVector<KnownProtocolKind, 2> supported;
  if (auto RP = ::com::getRootProtocol(context.LangOpts.COMModel))
    supported = { *RP, KnownProtocolKind::ISwiftObject };
  else
    supported = { KnownProtocolKind::ISwiftObject };

  if (llvm::none_of(supported,
                    [KP](const KnownProtocolKind P) { return KP == P; }))
    return nullptr;

  // Ensure that `ISwiftObject` is always compiler managed.
  if (KP == KnownProtocolKind::ISwiftObject) {
    llvm::SmallVector<ProtocolConformance *, 2> conformances;
    NTD->lookupConformance(protocol, conformances);
    if (!conformances.empty()) {
      context.Diags.diagnose(CD->getLoc(), diag::attr_com_explicit_iswiftobject);
      if (const swift::COMAttr *A = CD->getAttrs().getAttribute<COMAttr>())
        context.Diags.diagnose(A->getLocation(),
                               diag::attr_com_iswiftobject_implied);
      return conformances.front();
    }
  }

  auto conformance =
      context.getNormalConformance(NTD->getDeclaredInterfaceType(), protocol,
                                   NTD->getLoc(), /*inheritedTypeRepr=*/nullptr,
                                   /*conformanceDC=*/NTD,
                                   ProtocolConformanceState::Complete,
                                   ProtocolConformanceOptions());
  conformance->setSourceKindAndImplyingConformance(ConformanceEntryKind::Synthesized,
                                                   nullptr);
  NTD->registerProtocolConformance(conformance, /*synthesized=*/true);
  return conformance;
}

VarDecl *SynthesizeCOMInterfaceIDRequest::evaluate(Evaluator &evaluator,
                                                   ProtocolDecl *PD) const {
  auto &ASTContext = PD->getASTContext();
  auto *info = PD->getCOMDeclInfo();
  if (!info)
    return nullptr;
  // Only synthesize for a protocol defined in a source file of the module being
  // compiled.  A deserialized module or swiftinterface already carries the
  // synthesized extension, so find its `IID` by name lookup; re-synthesizing
  // would duplicate it, or fabricate a member absent from an older interface.
  if (!PD->isInSwiftSourceFile())
    return ::com::lookupCOMInterfaceID(PD);
  return ::com::synthesizeIIDProperty(PD, ASTContext, info->getInterfaceID());
}

VarDecl *SynthesizeCOMCLSIDRequest::evaluate(Evaluator &evaluator,
                                             ClassDecl *CD) const {
  auto &ASTContext = CD->getASTContext();

  // CLSID is the Microsoft model's spelling and activation surface. Other
  // models may consume the implementation UUID through their own policy, but
  // must not acquire a synthetic Microsoft-named member.
  if (ASTContext.LangOpts.COMModel != LangOptions::COMInteropModel::Microsoft)
    return nullptr;

  auto *info = CD->getCOMDeclInfo();
  if (!info)
    return nullptr;
  auto implementationID = info->getImplementationID();
  if (!implementationID)
    return nullptr;
  // Synthesize only for a source-file class; recover the imported member by
  // name lookup (see SynthesizeCOMInterfaceIDRequest).
  if (!CD->isInSwiftSourceFile())
    return ::com::lookupCOMImplementationID(CD);
  return ::com::synthesizeCLSIDProperty(CD, ASTContext, *implementationID);
}
}
