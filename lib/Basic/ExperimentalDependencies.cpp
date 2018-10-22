//===--- ExperimentalDependencies.cpp - Generates swiftdeps files ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <stdio.h>

#include "swift/Basic/ExperimentalDependencies.h"

// may not all be needed
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;
using namespace ExperimentalDependencies;

template<ProvidesKind kind, typename DeclT>
static HashOrUnimpLoc getProvidesHash(const DeclT*);

template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::topLevel, Decl>(const Decl *);
template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::nominal, NominalTypeDecl>(const NominalTypeDecl*);
template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::dynamicLookup, ValueDecl>(const ValueDecl*);
template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::memberHolder, NominalTypeDecl>(const NominalTypeDecl*);
template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::member, ValueDecl>(const ValueDecl*);

template<ProvidesKind kind, typename DeclT>
std::string ExperimentalDependencies::getCombinedNameAndProvidesHash(StringRef name, const DeclT* D) {
  return CompoundProvides(name, getProvidesHash<kind, DeclT>(D)).combined();
}

template
std::string ExperimentalDependencies::getCombinedNameAndProvidesHash<ProvidesKind::topLevel, Decl>(StringRef, const Decl*);
template
std::string ExperimentalDependencies::getCombinedNameAndProvidesHash<ProvidesKind::nominal, NominalTypeDecl>(StringRef, const NominalTypeDecl*);
template
std::string ExperimentalDependencies::getCombinedNameAndProvidesHash<ProvidesKind::dynamicLookup, ValueDecl>(StringRef, const ValueDecl*);
template
std::string ExperimentalDependencies::getCombinedNameAndProvidesHash<ProvidesKind::memberHolder, NominalTypeDecl>(StringRef, const NominalTypeDecl*);
template
std::string ExperimentalDependencies::getCombinedNameAndProvidesHash<ProvidesKind::member, ValueDecl>(StringRef, const ValueDecl*);






static std::string scrubOne(StringRef input, const char* prefix, const char endChar);
static std::string scrubAll(StringRef input);


template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::topLevel, Decl>(const Decl *D) {
  
  //  llvm::MD5 DeclHash;
  //  if (ExperimentalDependencies::unimpLocation_t r  = ExperimentalDependencies::updateExpDepDeclHash(DeclHash, D))
  //    return make_pair(std::string(), r);
  //
  //  llvm::MD5::MD5Result result;
  //  DeclHash.final(result);
  //  llvm::SmallString<32> str;
  //  llvm::MD5::stringifyResult(result, str);
  //  return std::make_pair(str.str().str(), nullptr);
  std::string buf;
  llvm::raw_string_ostream OS(buf);
  D->dump(OS);
  return ExperimentalDependencies::HashOrUnimpLoc(scrubAll(OS.str()), std::string());
}

template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::nominal, NominalTypeDecl>(const NominalTypeDecl* NTD) {
  Mangle::ASTMangler Mangler;
  return HashOrUnimpLoc::forHash(Mangler.mangleTypeAsContextUSR(NTD));
  //  return HashOrUnimpLoc::forUnimpLoc(UNIMP_HASH);
}

template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::dynamicLookup, ValueDecl>(const ValueDecl* VD) {
  Mangle::ASTMangler Mangler;
  std::string MangledName = Mangler.mangleDeclType(VD);
  return HashOrUnimpLoc::forHash(MangledName);
}

template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::memberHolder, NominalTypeDecl>(const NominalTypeDecl* NTD) {
  Mangle::ASTMangler Mangler;
  return HashOrUnimpLoc::forHash(Mangler.mangleTypeAsContextUSR(NTD));
//  return HashOrUnimpLoc::forUnimpLoc(UNIMP_HASH);
}

template<>
HashOrUnimpLoc getProvidesHash<ProvidesKind::member, ValueDecl>(const ValueDecl* VD) {
  Mangle::ASTMangler Mangler;
  std::string MangledName = Mangler.mangleDeclType(VD);
  return HashOrUnimpLoc::forHash(MangledName);
//  return HashOrUnimpLoc::forUnimpLoc(UNIMP_HASH);
}

static std::string scrubOne(StringRef input, const char* prefix, const char endChar) {
  std::string result;

  for (size_t pos = 0;;) {
    size_t i = input.find(prefix, pos);
    result += input.slice(pos, i);
    if (i == StringRef::npos)
      break;
    pos = input.find(endChar, i) + 1;
  }
  return result;
}

static std::string scrubAll(StringRef input) {
  return scrubOne(scrubOne( scrubOne(input, "range=[", ']'), "location=", ' '), "@/", ' ');
}


//qqq unused?
//void ExperimentalDependencies::updateExpDepFromBits(llvm::MD5 &hash, const void *bits, size_t size) {
//  hash.update(
//              ArrayRef<u_int8_t>(reinterpret_cast<u_int8_t*>(const_cast<void*>(bits)), size)
//              );
//}

//namespace {
//  // copied from PrintDecl
//class UpdateExpDepDeclHash : public DeclVisitor<UpdateExpDepDeclHash> {
//public:
//  llvm::MD5 &hash;
//  unimpLocation_t unimpLocation;
//
//  explicit UpdateExpDepDeclHash(llvm::MD5 &hash) : hash(hash), unimpLocation("never set in UpdateExpDepDeclHash")
//{}
//
//  void updateExpDepHashRec(Decl *D) { UpdateExpDepDeclHash(hash).visit(D); }
//  void updateExpDepHashRec(Expr *E) { E->print(OS, Indent+2); }
//  void updateExpDepHashRec(Stmt *S, const ASTContext &Ctx) { S->print(OS, &Ctx, Indent+2); }
//  void updateExpDepHashRec(Pattern *P) { PrintPattern(OS, Indent+2).visit(P); }
//  void updateExpDepHashRec(TypeRepr *T); // was printRec
//
//  // Print a field with a value.
//  template<typename T>
//  unimpLocation_t updateExpDepHashField(StringRef name, const T &value) {
////     snort( value );
//    return UNIMP_HASH;
//  }
//
//  void updateExpDepHashCommon(Decl *D, const char *Name) {
//    hash.update(Name);
//
//    if (D->isImplicit())
//      hash.update("implicit");
//
//    if (D->TrailingSemiLoc.isValid())
//      hash.update("trailing_semi");
//  }
//
//  void updateExpDepHashInherited(ArrayRef<TypeLoc> Inherited) {
//    if (Inherited.empty())
//      return;
//    hash.update("inherits: ");
//    for (auto *s in Super) {
//      // snort(s.getType()); // print
//      return UNIMP_HASH;
//    }
//  }
//
//  void visitImportDecl(ImportDecl *ID) {
//    updateExpDepHashCommon(ID, "import_decl");
//
//    if (ID->isExported())
//      hash.update("exported");
//
//    if (ID->getImportKind() != ImportKind::Module) {
//      hash.update("kind=");
//      hash.update(getImportKindString(ID->getImportKind()));
//    }
//    for (auto *ap: ID->getFullAccessPath()) {
//      snort(ap.first);
//      return UNIMP_HASH;
//    }
//  }
//
//  void visitExtensionDecl(ExtensionDecl *ED) {
//    updateExpDepHashCommon(ED, "extension_decl");
//    snort(ED->getExtendedType());
//    return UNIMP_HASH;
//    snortInherited(ED->getInherited());
//    return UNIMP_HASH;
//    for (Decl *Member : ED->getMembers()) {
//      updateExpDepHashRec(Member);
//    }
//  }
//
//  void updateExpDepHashName(const ValueDecl *D) {
//    if (D->getFullName()) {
//      hash.update(D->getFullName();
//    } else {
//      hash.update("anonname");
//      return UNIMP_HASH;
//    }
//  }
//
//  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
//    updateExpDepHashCommon(TAD, "typealias");
//    hash.update("type=");
//    if (TAD->getUnderlyingTypeLoc().getType()) {
//      hash.update(TAD->getUnderlyingTypeLoc().getType().getString());
//    } else {
//      hash.update("<<<unresolved>>>");
//    }
//    printInherited(TAD->getInherited());
//    OS << "')";
//  }
//
//  void updateExpDepHashAbstractTypeParamCommon(AbstractTypeParamDecl *decl,
//                                    const char *name) {
//    updateExpDepHashCommon(decl, name);
//    if (decl->getDeclContext()->getGenericEnvironmentOfContext()) {
//      if (auto superclassTy = decl->getSuperclass()) {
//        hash.update("superclass=");
//        hash.update("superclassTy->getString()");
//      }
//    }
//  }
//
//  void visitGenericTypeParamDecl(GenericTypeParamDecl *decl) {
//    updateExpDepHashAbstractTypeParamCommon(decl, "generic_type_param");
//    hash.update("depth=");
//    ExperimentalDependencies::updateExpDepBits(decl->getDepth());
//    hash.update("index=");
//    ExperimentalDependencies::updateExpDepBits(decl.getIndex());
//  }
//
//  void visitAssociatedTypeDecl(AssociatedTypeDecl *decl) {
//    updateExpDepHashAbstractTypeParamCommon(decl, "associated_type_decl");
//    if (auto defaultDef = decl->getDefaultDefinitionType()) {
//      hash.update("default=");
//      snortMprint(defaultDef);
//      return UNIMP_HASH;
//    }
//    if (auto whereClause = decl->getTrailingWhereClause()) {
//      hash.update("where requirements: ");
//      for (auto *req = whereClause->getRequirements()) {
//        snortMprint(req);
//        return UNIMP_HASH;
//      }
//    }
//    if (decl->overriddenDeclsComputed()) {
//      hash.update("overridden=");
//      for (auto *overridden: decl->getOverriddenDecl()) {
//        hash.update(overridden->getProtocol()->getName());
//      }
//    }
//  }
//
//  void visitProtocolDecl(ProtocolDecl *PD) {
//    printCommon(PD, "protocol");
//
//    OS << " requirement signature=";
//    if (PD->isRequirementSignatureComputed()) {
//      OS << GenericSignature::get({PD->getProtocolSelfType()} ,
//                                  PD->getRequirementSignature())
//      ->getAsString();
//    } else {
//      OS << "<null>";
//    }
//    printInherited(PD->getInherited());
//    if (auto whereClause = PD->getTrailingWhereClause()) {
//      OS << " where requirements: ";
//      interleave(whereClause->getRequirements(),
//                 [&](const RequirementRepr &req) { req.print(OS); },
//                 [&] { OS << ", "; });
//    }
//
//    for (auto VD : PD->getMembers()) {
//      OS << '\n';
//      printRec(VD);
//    }
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void updateExpDepHashCommon(ValueDecl *VD, const char *Name) {
//    updateExpDepHashCommon((Decl*)VD, Name);
//
//    updateExpDepHashDeclName(VD);
//    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(VD))
//      updateExpDepHashGenericParameters(AFD->getGenericParams());
//    if (auto *GTD = dyn_cast<GenericTypeDecl>(VD))
//      updateExpDepHashGenericParameters(GTD->getGenericParams());
//
//    if (auto *var = dyn_cast<VarDecl>(VD)) {
//      hash.update("type=");
//      if (var->hasType()) {
//        snortMprint(var->getType())
//        return UNIMP_HASH;
//      }
//      else
//        hash.update("<null type>");
//    }
//
//    if (VD->hasInterfaceType()) {
//      hash.update("interface type");
//      snortMprint(VD->getInterfaceType());
//    }
//
//    if (VD->hasAccess()) {
//      hash.update("access=");
//      hash.update(getAccessLevelSpelling(VD->getFormalAccess()));
//    }
//
//    if (VD->overriddenDeclsComputed()) {
//      auto overridden = VD->getOverriddenDecls();
//      if (!overridden.empty()) {
//        hash.update("override=");
//        for (auto *ov: overridden) {
//          snortMref(ov);
//          return UNIMP_HASH;
//        }
//      }
//    }
//
//    if (VD->isFinal())
//      hash.update("final");
//    if (VD->isObjC())
//      hash.update("@objc");
//    if (VD->isDynamic())
//      hash.update("dynamic");
//  }
//
//  void updateExpDepHashCommon(NominalTypeDecl *NTD, const char *Name) {
//    updateExpDepHashCommon((ValueDecl *)NTD, Name);
//
//    if (NTD->hasInterfaceType()) {
//      hash.update(NTD->isResilient() ? "resilient" : "non-resilient");
//  }
//
//  void visitSourceFile(const SourceFile &SF) {
//    hash.update("source_file");
//    hash.update(SF.getFilename());
//
//    for (Decl *D : SF.Decls) {
//      if (!D->isImplicit())
//        updateExpDepHashRec(D);
//    }
//  }
//
//  void visitVarDecl(VarDecl *VD) {
//    updateExpDepHashCommon(VD, "var_decl");
//    if (VD->isStatic())
//      hash.update("type");
//    if (VD->isLet())
//      hash.update("let");
//    if (VD->hasNonPatternBindingInit())
//      hash.update("non_pattern_init");
//    if (VD->getAttrs().hasAttribute<LazyAttr>())
//      hash.update("lazy");
//    updateExpDepHashStorageImpl(VD);
//    updateExpDepHashAccessors(VD);
//  }
//
//  void updateExpDepHashStorageImpl(AbstractStorageDecl *D) {
//    auto impl = D->getImplInfo();
//    hash.update("readImpl=");
//    hash.update(getReadImplKindName(impl.getReadImpl()));
//    if (!impl.supportsMutation()) {
//      hash.update("immutable");
//    } else {
//      hash.update("writeImpl=");
//      hash.update(getWriteImplKindName(impl.getWriteImpl()));
//      hash.update("readWriteImpl=");
//      hash.update(getReadWriteImplKindName(impl.getReadWriteImpl()));
//    }
//  }
//
//  void updateExpDepHashAccessors(AbstractStorageDecl *D) {
//    for (auto accessor : D->getAllAccessors()) {
//      updateExpDepHashRec(accessor);
//    }
//  }
//
//  void visitParamDecl(ParamDecl *PD) {
//    updateExpDepHashParameter(PD);
//  }
//
//  void visitEnumCaseDecl(EnumCaseDecl *ECD) {
//    updateExpDepHashCommon(ECD, "enum_case_decl");
//    for (EnumElementDecl *D : ECD->getElements()) {
//      updateExpDepHashRec(D);
//    }
//  }
//
//  void visitEnumDecl(EnumDecl *ED) {
//    updateExpDepHashCommon(ED, "enum_decl");
//   updateExpDepHashInherited(ED->getInherited());
//    for (Decl *D : ED->getMembers()) {
//      updateExpDepHashRec(D);
//    }
//  }
//
//  void visitEnumElementDecl(EnumElementDecl *EED) {
//    updateExpDepHashCommon(EED, "enum_element_decl");
//  }
//
//  void visitStructDecl(StructDecl *SD) {
//    updateExpDepHashCommon(SD, "struct_decl");
//    updateExpDepHashInherited(SD->getInherited());
//    for (Decl *D : SD->getMembers()) {
//      updateExpDepHashRec(D);
//    }
//  }
//
//  void visitClassDecl(ClassDecl *CD) {
//    updateExpDepHashCommon(CD, "class_decl");
//    if (CD->getAttrs().hasAttribute<StaticInitializeObjCMetadataAttr>())
//      hash.update("@_staticInitializeObjCMetadata");
//    updateExpDepHashInherited(CD->getInherited());
//    for (Decl *D : CD->getMembers()) {
//      updateExpDepHashRec(D);
//    }
//  }
//
//  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
//    updateExpDepHashCommon(PBD, "pattern_binding_decl");
//
//    for (auto entry : PBD->getPatternList()) {
//      updateExpDepHashRec(entry.getPattern());
//      if (entry.getInit()) {
//        updateExpDepHashRec(entry.getInit());
//      }
//    }
//  }
//
//  void visitSubscriptDecl(SubscriptDecl *SD) {
//    updateExpDepHashCommon(SD, "subscript_decl");
//    updateExpDepHashStorageImpl(SD);
//    updateExpDepHashAccessors(SD);
//  }
//
//  void updateExpDepHashCommonAFD(AbstractFunctionDecl *D, const char *Type) {
//    updateExpDepHashCommon(D, Type);
//    if (!D->getCaptureInfo().isTrivial()) {
//      snortMPrint(D->getCaptureInfo());
//    }
//
//    if (auto fec = D->getForeignErrorConvention()) {
//      hash.update("foreign_error=");
//      hash.update(getForeignErrorConventionKindString(fec->getKind()));
//      bool wantResultType = (
//                             fec->getKind() == ForeignErrorConvention::ZeroResult ||
//                             fec->getKind() == ForeignErrorConvention::NonZeroResult);
//
//      hash.update((fec->isErrorOwned() == ForeignErrorConvention::IsOwned)
//                  ? ",owned"
//                  : ",unowned");
//      hash.update("param=");
//      hash.update(llvm::utostr(fec->getErrorParameterIndex()));
//      hash.update("paramtype=");
//      hash.update(fec->getErrorParameterType().getString());
//      if (wantResultType) {
//        hash.update("resulttype=");
//        hash.update(fec->getResultType().getString());
//      }
//    }
//  }
//
//  void updateExpDepHashParameter(const ParamDecl *P) {
//    hash.update("parameter");
//    updateExpDepHashDeclName(P);
//    if (!P->getArgumentName().empty()) {
//      hash.update("apiName=");
//      hash.update(P->getArgumentName());
//    }
//
//    if (P->hasType()) {
//      hash.update("type=");
//      snortMprint(P->getType());
//      return UNIMP_HASH;
//    }
//
//    if (P->hasInterfaceType()) {
//      hash.update("interface type=");
//      snortMprint(P->getInterfaceType());
//      return UNIMP_HASH;
//    }
////    UP TO HERE
//    switch (P->getSpecifier()) {
//      case VarDecl::Specifier::Let:
//        /* nothing */
//        break;
//      case VarDecl::Specifier::Var:
//        OS << " mutable";
//        break;
//      case VarDecl::Specifier::InOut:
//        OS << " inout";
//        break;
//      case VarDecl::Specifier::Shared:
//        OS << " shared";
//        break;
//      case VarDecl::Specifier::Owned:
//        OS << " owned";
//        break;
//    }
//
//    if (P->isVariadic())
//      OS << " variadic";
//
//    if (P->getDefaultArgumentKind() != DefaultArgumentKind::None)
//      printField("default_arg",
//                 getDefaultArgumentKindString(P->getDefaultArgumentKind()));
//
//    if (auto init = P->getDefaultValue()) {
//      OS << " expression=\n";
//      printRec(init);
//    }
//
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void printParameterList(const ParameterList *params, const ASTContext *ctx = nullptr) {
//    OS.indent(Indent);
//    PrintWithColorRAII(OS, ParenthesisColor) << '(';
//    PrintWithColorRAII(OS, ParameterColor) << "parameter_list";
//    Indent += 2;
//    for (auto P : *params) {
//      OS << '\n';
//      printParameter(P);
//    }
//
//    if (!ctx && params->size() != 0 && params->get(0))
//      ctx = &params->get(0)->getASTContext();
//
//    if (ctx) {
//      auto R = params->getSourceRange();
//      if (R.isValid()) {
//        PrintWithColorRAII(OS, RangeColor) << " range=";
//        R.print(PrintWithColorRAII(OS, RangeColor).getOS(),
//                ctx->SourceMgr, /*PrintText=*/false);
//      }
//    }
//
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//    Indent -= 2;
//  }
//
//  void printAbstractFunctionDecl(AbstractFunctionDecl *D) {
//    Indent += 2;
//    if (auto *P = D->getImplicitSelfDecl()) {
//      OS << '\n';
//      printParameter(P);
//    }
//
//    OS << '\n';
//    printParameterList(D->getParameters(), &D->getASTContext());
//    Indent -= 2;
//
//    if (auto FD = dyn_cast<FuncDecl>(D)) {
//      if (FD->getBodyResultTypeLoc().getTypeRepr()) {
//        OS << '\n';
//        Indent += 2;
//        OS.indent(Indent);
//        PrintWithColorRAII(OS, ParenthesisColor) << '(';
//        OS << "result\n";
//        printRec(FD->getBodyResultTypeLoc().getTypeRepr());
//        PrintWithColorRAII(OS, ParenthesisColor) << ')';
//        Indent -= 2;
//      }
//    }
//    if (auto Body = D->getBody(/*canSynthesize=*/false)) {
//      OS << '\n';
//      printRec(Body, D->getASTContext());
//    }
//  }
//
//  void printCommonFD(FuncDecl *FD, const char *type) {
//    printCommonAFD(FD, type);
//    if (FD->isStatic())
//      OS << " type";
//  }
//
//  void visitFuncDecl(FuncDecl *FD) {
//    printCommonFD(FD, "func_decl");
//    printAbstractFunctionDecl(FD);
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitAccessorDecl(AccessorDecl *AD) {
//    printCommonFD(AD, "accessor_decl");
//    OS << " " << getAccessorKindString(AD->getAccessorKind());
//    OS << "_for=" << AD->getStorage()->getFullName();
//    printAbstractFunctionDecl(AD);
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitConstructorDecl(ConstructorDecl *CD) {
//    printCommonAFD(CD, "constructor_decl");
//    if (CD->isRequired())
//      hash.update("required";
//    PrintWithColorRAII(OS, DeclModifierColor) << " "
//    << getCtorInitializerKindString(CD->getInitKind());
//    if (CD->getFailability() != OTK_None)
//      hash.update("failable="
//      << getOptionalTypeKindString(CD->getFailability());
//    printAbstractFunctionDecl(CD);
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitDestructorDecl(DestructorDecl *DD) {
//    printCommonAFD(DD, "destructor_decl");
//    printAbstractFunctionDecl(DD);
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
//    printCommon(TLCD, "top_level_code_decl");
//    if (TLCD->getBody()) {
//      OS << "\n";
//      printRec(TLCD->getBody(), static_cast<Decl *>(TLCD)->getASTContext());
//    }
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void printASTNodes(const ArrayRef<ASTNode> &Elements, const ASTContext &Ctx, StringRef Name) {
//    OS.indent(Indent);
//    PrintWithColorRAII(OS, ParenthesisColor) << "(";
//    PrintWithColorRAII(OS, ASTNodeColor) << Name;
//    for (auto Elt : Elements) {
//      OS << '\n';
//      if (auto *SubExpr = Elt.dyn_cast<Expr*>())
//        printRec(SubExpr);
//      else if (auto *SubStmt = Elt.dyn_cast<Stmt*>())
//        printRec(SubStmt, Ctx);
//      else
//        printRec(Elt.get<Decl*>());
//    }
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitIfConfigDecl(IfConfigDecl *ICD) {
//    printCommon(ICD, "if_config_decl");
//    Indent += 2;
//    for (auto &Clause : ICD->getClauses()) {
//      OS << '\n';
//      OS.indent(Indent);
//      PrintWithColorRAII(OS, StmtColor) << (Clause.Cond ? "#if:" : "#else:");
//      if (Clause.isActive)
//        hash.update("active";
//      if (Clause.Cond) {
//        OS << "\n";
//        printRec(Clause.Cond);
//      }
//
//      OS << '\n';
//      Indent += 2;
//      printASTNodes(Clause.Elements, ICD->getASTContext(), "elements");
//      Indent -= 2;
//    }
//
//    Indent -= 2;
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitPoundDiagnosticDecl(PoundDiagnosticDecl *PDD) {
//    printCommon(PDD, "pound_diagnostic_decl");
//    auto kind = PDD->isError() ? "error" : "warning";
//    OS << " kind=" << kind << "\n";
//    Indent += 2;
//    printRec(PDD->getMessage());
//    Indent -= 2;
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitPrecedenceGroupDecl(PrecedenceGroupDecl *PGD) {
//    printCommon(PGD, "precedence_group_decl ");
//    OS << PGD->getName() << "\n";
//
//    OS.indent(Indent+2);
//    OS << "associativity "
//    << getAssociativityString(PGD->getAssociativity()) << "\n";
//
//    OS.indent(Indent+2);
//    OS << "assignment " << (PGD->isAssignment() ? "true" : "false");
//
//    auto printRelations =
//    [&](StringRef label, ArrayRef<PrecedenceGroupDecl::Relation> rels) {
//      if (rels.empty()) return;
//      OS << '\n';
//      OS.indent(Indent+2);
//      OS << label << ' ' << rels[0].Name;
//      for (auto &rel : rels.slice(1))
//        OS << ", " << rel.Name;
//    };
//    printRelations("higherThan", PGD->getHigherThan());
//    printRelations("lowerThan", PGD->getLowerThan());
//
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void printOperatorIdentifiers(OperatorDecl *OD) {
//    auto identifiers = OD->getIdentifiers();
//    for (auto index : indices(identifiers)) {
//      OS.indent(Indent + 2);
//      OS << "identifier #" << index << " " << identifiers[index];
//      if (index != identifiers.size() - 1)
//        OS << "\n";
//    }
//  }
//
//  void visitInfixOperatorDecl(InfixOperatorDecl *IOD) {
//    printCommon(IOD, "infix_operator_decl");
//    OS << " " << IOD->getName();
//    if (!IOD->getIdentifiers().empty()) {
//      OS << "\n";
//      printOperatorIdentifiers(IOD);
//    }
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitPrefixOperatorDecl(PrefixOperatorDecl *POD) {
//    printCommon(POD, "prefix_operator_decl");
//    OS << " " << POD->getName();
//    if (!POD->getIdentifiers().empty()) {
//      OS << "\n";
//      printOperatorIdentifiers(POD);
//    }
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitPostfixOperatorDecl(PostfixOperatorDecl *POD) {
//    printCommon(POD, "postfix_operator_decl");
//    OS << " " << POD->getName();
//    if (!POD->getIdentifiers().empty()) {
//      OS << "\n";
//      printOperatorIdentifiers(POD);
//    }
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitModuleDecl(ModuleDecl *MD) {
//    printCommon(MD, "module");
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//
//  void visitMissingMemberDecl(MissingMemberDecl *MMD) {
//    printCommon(MMD, "missing_member_decl ");
//    PrintWithColorRAII(OS, IdentifierColor)
//    << '\"' << MMD->getFullName() << '\"';
//    PrintWithColorRAII(OS, ParenthesisColor) << ')';
//  }
//};
//} // end anonymous namespace
//
//
//
//unimpLocation_t ExperimentalDependencies::updateExpDepDeclHash(llvm::MD5& hash, const Decl* D) {
//  //qqq
//  //  D->dump();
//  UpdateExpDepDeclHash u(hash);
//  u.visit(const_cast<Decl *>(D));
//  return u.unimpLocation;
//}


//unimpLocation_t ExperimentalDependencies::updateExpDepDeclHash(llvm::MD5& hash, const Decl* D) {
//  //qqq
//  //  D->dump();
//  UpdateExpDepDeclHash u(hash);
//  u.visit(const_cast<Decl *>(D));
//  return u.unimpLocation;
//}
