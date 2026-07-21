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

#include "swift/AST/ASTContext.h"
#include "swift/AST/ArgumentList.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/StorageImpl.h"
#include "swift/AST/SynthesizedDeclBuilder.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/UUID.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

namespace {
namespace com {

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

}
}

VarDecl *SynthesizeCOMInterfaceIDRequest::evaluate(Evaluator &evaluator,
                                                   ProtocolDecl *PD) const {
  auto &ASTContext = PD->getASTContext();
  if (!ASTContext.LangOpts.EnableCOMInterop)
    return nullptr;
  auto *attr = PD->getAttrs().getAttribute<COMAttr>();
  if (!attr || attr->isInvalid() || attr->IID.empty())
    return nullptr;
  // Only synthesize for a protocol defined in a source file of the module being
  // compiled.  A deserialized module or swiftinterface already carries the
  // synthesized extension, so find its `IID` by name lookup; re-synthesizing
  // would duplicate it, or fabricate a member absent from an older interface.
  auto *DC = PD->getDeclContext();
  if (!DC->getParentSourceFile() || DC->isInSwiftinterface())
    return com::lookupCOMInterfaceID(PD);
  return com::synthesizeIIDProperty(PD, ASTContext, attr->IID);
}

VarDecl *SynthesizeCOMImplementationIDRequest::evaluate(Evaluator &evaluator,
                                                        ClassDecl *CD) const {
  auto &ASTContext = CD->getASTContext();
  if (!ASTContext.LangOpts.EnableCOMInterop)
    return nullptr;
  auto *attr = CD->getAttrs().getAttribute<COMAttr>();
  if (!attr || attr->isInvalid() || !attr->CLSID || attr->CLSID->empty())
    return nullptr;
  // Synthesize only for a source-file class; recover the imported member by
  // name lookup (see SynthesizeCOMInterfaceIDRequest).
  auto *DC = CD->getDeclContext();
  if (!DC->getParentSourceFile() || DC->isInSwiftinterface())
    return com::lookupCOMImplementationID(CD);
  return com::synthesizeCLSIDProperty(CD, ASTContext, *attr->CLSID);
}
