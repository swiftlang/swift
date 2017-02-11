//===--- SwiftSourceDocInfo.cpp -------------------------------------------===//
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

#include "SwiftASTManager.h"
#include "SwiftLangSupport.h"
#include "SourceKit/Support/UIdent.h"
#include "SourceKit/Support/ImmutableTextBuffer.h"
#include "SourceKit/Support/Logging.h"

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SourceEntityWalker.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/IDE/Utils.h"
#include "swift/Markup/XMLUtils.h"
#include "swift/Sema/IDETypeChecking.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/Module.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/Lexer.h"

#include "llvm/Support/MemoryBuffer.h"

#include <numeric>

using namespace SourceKit;
using namespace swift;
using namespace swift::ide;

namespace {
class AnnotatedDeclarationPrinter : public XMLEscapingPrinter {
public:
  AnnotatedDeclarationPrinter(raw_ostream &OS)
    :XMLEscapingPrinter(OS) { }

private:
  void printTypeRef(Type T, const TypeDecl *TD, Identifier Name) override {
    printXML("<Type usr=\"");
    SwiftLangSupport::printUSR(TD, OS);
    printXML("\">");
    StreamPrinter::printTypeRef(T, TD, Name);
    printXML("</Type>");
  }
};
} // end anonymous namespace

static StringRef getTagForDecl(const Decl *D, bool isRef) {
  auto UID = SwiftLangSupport::getUIDForDecl(D, isRef);
  static const char *prefix = "source.lang.swift.";
  assert(UID.getName().startswith(prefix));
  return UID.getName().drop_front(strlen(prefix));
}

static StringRef ExternalParamNameTag = "decl.var.parameter.argument_label";
static StringRef LocalParamNameTag = "decl.var.parameter.name";
static StringRef GenericParamNameTag = "decl.generic_type_param.name";
static StringRef SyntaxKeywordTag = "syntaxtype.keyword";

static StringRef getTagForParameter(PrintStructureKind context) {
  switch (context) {
  case PrintStructureKind::FunctionParameter:
    return "decl.var.parameter";
  case PrintStructureKind::FunctionReturnType:
    return "decl.function.returntype";
  case PrintStructureKind::FunctionType:
    return "";
  case PrintStructureKind::TupleType:
    return "tuple";
  case PrintStructureKind::TupleElement:
    return "tuple.element";
  case PrintStructureKind::GenericParameter:
    return "decl.generic_type_param";
  case PrintStructureKind::GenericRequirement:
    return "decl.generic_type_requirement";
  case PrintStructureKind::BuiltinAttribute:
    return "syntaxtype.attribute.builtin";
  case PrintStructureKind::NumberLiteral:
    return "syntaxtype.number";
  case PrintStructureKind::StringLiteral:
    return "syntaxtype.string";
  }
  llvm_unreachable("unexpected parameter kind");
}

static StringRef getDeclNameTagForDecl(const Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Param:
    // When we're examining the parameter itself, it is the local name that is
    // the name of the variable.
    return LocalParamNameTag;
  case DeclKind::GenericTypeParam:
    return ""; // Handled by printName.
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::Subscript:
    // The names 'init'/'deinit'/'subscript' are actually keywords.
    return SyntaxKeywordTag;
  default:
    return "decl.name";
  }
}

namespace {
/// A typesafe union of contexts that the printer can be inside.
/// Currently: Decl, PrintStructureKind
class PrintContext {
  // Use the low bit to determine the type; store the enum value shifted left
  // to leave the low bit free.
  const uintptr_t value;
  static constexpr unsigned declTag = 0;
  static constexpr unsigned PrintStructureKindTag = 1;
  static constexpr unsigned typeTag = 2;
  static constexpr unsigned tagMask = 3;
  static constexpr unsigned tagShift = 2;
  bool hasTag(unsigned tag) const { return (value & tagMask) == tag; }

public:
  PrintContext(const Decl *D) : value(uintptr_t(D)) {
    static_assert(llvm::PointerLikeTypeTraits<Decl *>::NumLowBitsAvailable >=
                      tagShift,
                  "missing spare bit in Decl *");
  }
  PrintContext(PrintStructureKind K)
      : value((uintptr_t(K) << tagShift) | PrintStructureKindTag) {}
  PrintContext(TypeLoc unused) : value(typeTag) {}

  /// Get the context as a Decl, or nullptr.
  const Decl *getDecl() const {
    return hasTag(declTag) ? (const Decl *)value : nullptr;
  }
  /// Get the context as a PrintStructureKind, or None.
  Optional<PrintStructureKind> getPrintStructureKind() const {
    if (!hasTag(PrintStructureKindTag))
      return None;
    return PrintStructureKind(value >> tagShift);
  }
  /// Whether this is a PrintStructureKind context of the given \p kind.
  bool is(PrintStructureKind kind) const {
    auto storedKind = getPrintStructureKind();
    return storedKind && *storedKind == kind;
  }
  bool isType() const { return hasTag(typeTag); }
};

/// An ASTPrinter for annotating declarations with XML tags that describe the
/// key substructure of the declaration for CursorInfo/DocInfo.
///
/// Prints declarations with decl- and type-specific tags derived from the
/// UIDs used for decl/refs. For example (including newlines purely for ease of
/// reading):
///
/// \verbatim
///   <decl.function.free>
///     func <decl.name>foo</decl.name>
///     (
///     <decl.var.parameter>
///       <decl.var.parameter.name>x</decl.var.parameter.name>:
///       <ref.struct usr="Si">Int</ref.struct>
///     </decl.var.parameter>
///     ) -> <decl.function.returntype>
///            <ref.struct usr="Si">Int</ref.struct></decl.function.returntype>
///  </decl.function.free>
/// \endverbatim
class FullyAnnotatedDeclarationPrinter final : public XMLEscapingPrinter {
public:
  FullyAnnotatedDeclarationPrinter(raw_ostream &OS) : XMLEscapingPrinter(OS) {}

private:

  // MARK: The ASTPrinter callback interface.

  void printDeclPre(const Decl *D, Optional<BracketOptions> Bracket) override {
    contextStack.emplace_back(PrintContext(D));
    openTag(getTagForDecl(D, /*isRef=*/false));
  }
  void printDeclPost(const Decl *D, Optional<BracketOptions> Bracket) override {
    assert(contextStack.back().getDecl() == D && "unmatched printDeclPre");
    contextStack.pop_back();
    closeTag(getTagForDecl(D, /*isRef=*/false));
  }

  void printDeclLoc(const Decl *D) override {
    auto tag = getDeclNameTagForDecl(D);
    if (!tag.empty())
      openTag(tag);
  }
  void printDeclNameEndLoc(const Decl *D) override {
    auto tag = getDeclNameTagForDecl(D);
    if (!tag.empty())
      closeTag(tag);
  }

  void printTypePre(const TypeLoc &TL) override {
    auto tag = getTypeTagForCurrentContext();
    contextStack.emplace_back(PrintContext(TL));
    if (!tag.empty())
      openTag(tag);
  }
  void printTypePost(const TypeLoc &TL) override {
    assert(contextStack.back().isType());
    contextStack.pop_back();
    auto tag = getTypeTagForCurrentContext();
    if (!tag.empty())
      closeTag(tag);
  }

  void printStructurePre(PrintStructureKind kind, const Decl *D) override {
    if (kind == PrintStructureKind::TupleElement ||
        kind == PrintStructureKind::TupleType)
      fixupTuple(kind);

    contextStack.emplace_back(PrintContext(kind));
    auto tag = getTagForParameter(kind);
    if (tag.empty())
      return;

    if (D && kind == PrintStructureKind::GenericParameter) {
      assert(isa<ValueDecl>(D) && "unexpected non-value decl for param");
      openTagWithUSRForDecl(tag, cast<ValueDecl>(D));
    } else {
      openTag(tag);
    }
  }
  void printStructurePost(PrintStructureKind kind, const Decl *D) override {
    if (kind == PrintStructureKind::TupleElement ||
        kind == PrintStructureKind::TupleType) {
      auto prev = contextStack.pop_back_val();
      (void)prev;
      fixupTuple(kind);
      assert(prev.is(kind) && "unmatched printStructurePre");
    } else {
      assert(contextStack.back().is(kind) && "unmatched printStructurePre");
      contextStack.pop_back();
    }

    auto tag = getTagForParameter(kind);
    if (!tag.empty())
      closeTag(tag);
  }

  void printNamePre(PrintNameContext context) override {
    auto tag = getTagForPrintNameContext(context);
    if (!tag.empty())
      openTag(tag);
  }
  void printNamePost(PrintNameContext context) override {
    auto tag = getTagForPrintNameContext(context);
    if (!tag.empty())
      closeTag(tag);
  }

  void printTypeRef(Type T, const TypeDecl *TD, Identifier name) override {
    auto tag = getTagForDecl(TD, /*isRef=*/true);
    openTagWithUSRForDecl(tag, TD);
    insideRef = true;
    XMLEscapingPrinter::printTypeRef(T, TD, name);
    insideRef = false;
    closeTag(tag);
  }

  // MARK: Convenience functions for printing.

  void openTag(StringRef tag) { OS << "<" << tag << ">"; }
  void closeTag(StringRef tag) { OS << "</" << tag << ">"; }

  void openTagWithUSRForDecl(StringRef tag, const ValueDecl *VD) {
    OS << "<" << tag << " usr=\"";
    SwiftLangSupport::printUSR(VD, OS);
    OS << "\">";
  }

  // MARK: Misc.

  StringRef getTypeTagForCurrentContext() const {
    if (contextStack.empty())
      return "";

    static StringRef parameterTypeTag = "decl.var.parameter.type";
    static StringRef genericParamTypeTag = "decl.generic_type_param.constraint";

    auto context = contextStack.back();
    if (context.is(PrintStructureKind::FunctionParameter))
      return parameterTypeTag;
    if (context.is(PrintStructureKind::GenericParameter))
      return genericParamTypeTag;
    if (context.is(PrintStructureKind::TupleElement))
      return "tuple.element.type";
    if (context.getPrintStructureKind().hasValue() || context.isType())
      return "";

    assert(context.getDecl() && "unexpected context kind");
    switch (context.getDecl()->getKind()) {
    case DeclKind::Param:
      return parameterTypeTag;
    case DeclKind::GenericTypeParam:
      return genericParamTypeTag;
    case DeclKind::Var:
      return "decl.var.type";
    case DeclKind::Subscript:
    case DeclKind::Func:
    default:
      return "";
    }
  }

  StringRef getTagForPrintNameContext(PrintNameContext context) {
    if (insideRef)
      return "";

    bool insideParam =
        !contextStack.empty() &&
        contextStack.back().is(PrintStructureKind::FunctionParameter);

    switch (context) {
    case PrintNameContext::FunctionParameterExternal:
      return ExternalParamNameTag;
    case PrintNameContext::FunctionParameterLocal:
      return LocalParamNameTag;
    case PrintNameContext::TupleElement:
      if (insideParam)
        return ExternalParamNameTag;
      return "tuple.element.argument_label";
    case PrintNameContext::Keyword:
      return SyntaxKeywordTag;
    case PrintNameContext::GenericParameter:
      return GenericParamNameTag;
    case PrintNameContext::Attribute:
      return "syntaxtype.attribute.name";
    default:
      return "";
    }
  }

  /// 'Fix' a tuple or tuple element structure kind to be a function parameter
  /// or function type if we are currently inside a function type. This
  /// simplifies functions that need to differentiate a tuple from the input
  /// part of a function type.
  void fixupTuple(PrintStructureKind &kind) {
    assert(kind == PrintStructureKind::TupleElement ||
           kind == PrintStructureKind::TupleType);
    // Skip over 'type's in the context stack.
    for (auto I = contextStack.rbegin(), E = contextStack.rend(); I != E; ++I) {
      if (I->is(PrintStructureKind::FunctionType)) {
        if (kind == PrintStructureKind::TupleElement)
          kind = PrintStructureKind::FunctionParameter;
        else
          kind = PrintStructureKind::FunctionType;
        break;
      } else if (!I->isType()) {
        break;
      }
    }
  }

private:
  /// A stack of contexts being printed, used to determine the context for
  /// subsequent ASTPrinter callbacks.
  llvm::SmallVector<PrintContext, 3> contextStack;
  bool insideRef = false;
};
} // end anonymous namespace

static Type findBaseTypeForReplacingArchetype(const ValueDecl *VD, const Type Ty) {
  if (Ty.isNull())
    return Type();

  // Find the nominal type decl related to VD.
  NominalTypeDecl *NTD = VD->getDeclContext()->
    getAsNominalTypeOrNominalTypeExtensionContext();
  if (!NTD)
    return Type();

  return Ty->getRValueType()->getRValueInstanceType();
}

static void printAnnotatedDeclaration(const ValueDecl *VD,
                                      const Type BaseTy,
                                      raw_ostream &OS) {
  AnnotatedDeclarationPrinter Printer(OS);
  PrintOptions PO = PrintOptions::printQuickHelpDeclaration();
  if (BaseTy) {
    PO.setBaseType(BaseTy);
    PO.PrintAsMember = true;
  }

  // If it's implicit, try to find an overridden ValueDecl that's not implicit.
  // This will ensure we can properly annotate TypeRepr with a usr
  // in AnnotatedDeclarationPrinter.
  while (VD->isImplicit() && VD->getOverriddenDecl())
    VD = VD->getOverriddenDecl();

  // Wrap this up in XML, as that's what we'll use for documentation comments.
  OS<<"<Declaration>";
  VD->print(Printer, PO);
  OS<<"</Declaration>";
}

void SwiftLangSupport::printFullyAnnotatedDeclaration(const ValueDecl *VD,
                                                      Type BaseTy,
                                                      raw_ostream &OS) {
  FullyAnnotatedDeclarationPrinter Printer(OS);
  PrintOptions PO = PrintOptions::printQuickHelpDeclaration();
  if (BaseTy) {
    PO.setBaseType(BaseTy);
    PO.PrintAsMember = true;
  }

  // If it's implicit, try to find an overridden ValueDecl that's not implicit.
  // This will ensure we can properly annotate TypeRepr with a usr
  // in AnnotatedDeclarationPrinter.
  while (VD->isImplicit() && VD->getOverriddenDecl())
    VD = VD->getOverriddenDecl();

  VD->print(Printer, PO);
}

void SwiftLangSupport::
printFullyAnnotatedSynthesizedDeclaration(const swift::ValueDecl *VD,
                                          swift::NominalTypeDecl *Target,
                                          llvm::raw_ostream &OS) {
  // FIXME: Mutable global variable - gross!
  static llvm::SmallDenseMap<swift::ValueDecl*,
    std::unique_ptr<swift::SynthesizedExtensionAnalyzer>> TargetToAnalyzerMap;
  FullyAnnotatedDeclarationPrinter Printer(OS);
  PrintOptions PO = PrintOptions::printQuickHelpDeclaration();
  if (TargetToAnalyzerMap.count(Target) == 0) {
    std::unique_ptr<SynthesizedExtensionAnalyzer> Analyzer(
      new SynthesizedExtensionAnalyzer(Target, PO));
    TargetToAnalyzerMap.insert({Target, std::move(Analyzer)});
  }
  PO.initForSynthesizedExtension(Target);
  PO.PrintAsMember = true;
  VD->print(Printer, PO);
}

template <typename FnTy>
void walkRelatedDecls(const ValueDecl *VD, const FnTy &Fn) {
  llvm::SmallDenseMap<DeclName, unsigned, 16> NamesSeen;
  ++NamesSeen[VD->getFullName()];
  SmallVector<ValueDecl *, 8> RelatedDecls;

  if (isa<ParamDecl>(VD))
    return; // Parameters don't have interesting related declarations.

  // FIXME: Extract useful related declarations, overloaded functions,
  // if VD is an initializer, we should extract other initializers etc.
  // For now we use UnqualifiedLookup to fetch other declarations with the same
  // base name.
  auto TypeResolver = VD->getASTContext().getLazyResolver();
  UnqualifiedLookup Lookup(VD->getName(), VD->getDeclContext(), TypeResolver);
  for (auto result : Lookup.Results) {
    ValueDecl *RelatedVD = result.getValueDecl();
    if (RelatedVD->getAttrs().isUnavailable(VD->getASTContext()))
      continue;

    if (RelatedVD != VD) {
      ++NamesSeen[RelatedVD->getFullName()];
      RelatedDecls.push_back(RelatedVD);
    }
  }

  // Now provide the results along with whether the name is duplicate or not.
  for (auto RelatedVD : RelatedDecls) {
    Fn(RelatedVD, NamesSeen[RelatedVD->getFullName()] > 1);
  }
}

//===----------------------------------------------------------------------===//
// SwiftLangSupport::getCursorInfo
//===----------------------------------------------------------------------===//

static StringRef getSourceToken(unsigned Offset,
                                ImmutableTextSnapshotRef Snap) {
  auto MemBuf = Snap->getBuffer()->getInternalBuffer();
  SourceManager SM;
  auto MemBufRef = llvm::MemoryBuffer::getMemBuffer(MemBuf->getBuffer(),
                                                 MemBuf->getBufferIdentifier());
  auto BufId = SM.addNewSourceBuffer(std::move(MemBufRef));
  SourceLoc Loc = SM.getLocForOffset(BufId, Offset);

  // Use fake language options; language options only affect validity
  // and the exact token produced.
  LangOptions FakeLangOpts;
  Lexer L(FakeLangOpts, SM, BufId, nullptr, /*InSILMode=*/ false,
          CommentRetentionMode::ReturnAsTokens);
  return L.getTokenAt(Loc).getText();
}

static llvm::Optional<unsigned>
mapOffsetToOlderSnapshot(unsigned Offset,
                         ImmutableTextSnapshotRef NewSnap,
                         ImmutableTextSnapshotRef OldSnap) {
  SmallVector<ReplaceImmutableTextUpdateRef, 16> Updates;
  OldSnap->foreachReplaceUntil(NewSnap,
    [&](ReplaceImmutableTextUpdateRef Upd)->bool {
      Updates.push_back(Upd);
      return true;
    });

  // Walk the updates backwards and "undo" them.
  for (auto I = Updates.rbegin(), E = Updates.rend(); I != E; ++I) {
    auto Upd = *I;
    if (Upd->getByteOffset() <= Offset &&
        Offset < Upd->getByteOffset() + Upd->getText().size())
      return None; // Offset is part of newly inserted text.

    if (Upd->getByteOffset() <= Offset) {
      Offset += Upd->getLength(); // "bring back" what was removed.
      Offset -= Upd->getText().size(); // "remove" what was added.
    }
  }
  return Offset;
}

static llvm::Optional<unsigned>
mapOffsetToNewerSnapshot(unsigned Offset,
                         ImmutableTextSnapshotRef OldSnap,
                         ImmutableTextSnapshotRef NewSnap) {
  bool Completed = OldSnap->foreachReplaceUntil(NewSnap,
    [&](ReplaceImmutableTextUpdateRef Upd)->bool {
      if (Upd->getByteOffset() <= Offset &&
          Offset < Upd->getByteOffset() + Upd->getLength())
        return false; // Offset is part of removed text.

      if (Upd->getByteOffset() <= Offset) {
        Offset += Upd->getText().size();
        Offset -= Upd->getLength();
      }
      return true;
    });

  if (Completed)
    return Offset;
  return None;
}

/// Tries to remap the location from a previous snapshot to the latest one.
static llvm::Optional<std::pair<unsigned, unsigned>>
tryRemappingLocToLatestSnapshot(SwiftLangSupport &Lang,
                                std::pair<unsigned, unsigned> Range,
                                StringRef Filename,
                         ArrayRef<ImmutableTextSnapshotRef> PreviousASTSnaps) {
  ImmutableTextSnapshotRef LatestSnap;
  if (auto EditorDoc = Lang.getEditorDocuments().findByPath(Filename))
    LatestSnap = EditorDoc->getLatestSnapshot();
  if (!LatestSnap)
    return Range;

  for (auto &PrevSnap : PreviousASTSnaps) {
    if (PrevSnap->isFromSameBuffer(LatestSnap)) {
      if (PrevSnap->getStamp() == LatestSnap->getStamp())
        return Range;

      auto OptBegin = mapOffsetToNewerSnapshot(Range.first,
                                               PrevSnap, LatestSnap);
      if (!OptBegin.hasValue())
        return None;

      auto OptEnd = mapOffsetToNewerSnapshot(Range.first+Range.second,
                                             PrevSnap, LatestSnap);
      if (!OptEnd.hasValue())
        return None;

      return std::make_pair(*OptBegin, *OptEnd-*OptBegin);
    }
  }

  return Range;
}


/// Returns true for error.
static bool passCursorInfoForModule(ModuleEntity Mod,
                                    SwiftInterfaceGenMap &IFaceGenContexts,
                                    const CompilerInvocation &Invok,
                             std::function<void(const CursorInfo &)> Receiver) {
  std::string Name = Mod.getName();
  std::string FullName = Mod.getFullName();
  CursorInfo Info;
  Info.Kind = SwiftLangSupport::getUIDForModuleRef();
  Info.Name = Name;
  Info.ModuleName = FullName;
  if (auto IFaceGenRef = IFaceGenContexts.find(Info.ModuleName, Invok))
    Info.ModuleInterfaceName = IFaceGenRef->getDocumentName();
  Info.IsSystem = Mod.isSystemModule();
  std::vector<StringRef> Groups;
  if (auto MD = Mod.getAsSwiftModule()) {
    Info.ModuleGroupArray = ide::collectModuleGroups(const_cast<ModuleDecl*>(MD),
                                                     Groups);
  }
  Receiver(Info);
  return false;
}

/// Returns true for failure to resolve.
static bool passCursorInfoForDecl(const ValueDecl *VD,
                                  const ModuleDecl *MainModule,
                                  const Type Ty,
                                  const Type ContainerTy,
                                  bool IsRef,
                                  Optional<unsigned> OrigBufferID,
                                  SwiftLangSupport &Lang,
                                  const CompilerInvocation &Invok,
                            ArrayRef<ImmutableTextSnapshotRef> PreviousASTSnaps,
                             std::function<void(const CursorInfo &)> Receiver) {
  if (AvailableAttr::isUnavailable(VD))
    return true;

  SmallString<64> SS;
  auto BaseType = findBaseTypeForReplacingArchetype(VD, Ty);
  bool InSynthesizedExtension = false;
  if (BaseType) {
    if (auto Target = BaseType->getAnyNominal()) {
      SynthesizedExtensionAnalyzer Analyzer(Target,
                                            PrintOptions::printInterface());
      InSynthesizedExtension = Analyzer.isInSynthesizedExtension(VD);
    }
  }

  unsigned NameBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    SwiftLangSupport::printDisplayName(VD, OS);
  }
  unsigned NameEnd = SS.size();

  unsigned USRBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    SwiftLangSupport::printUSR(VD, OS);
    if (InSynthesizedExtension) {
        OS << LangSupport::SynthesizedUSRSeparator;
        SwiftLangSupport::printUSR(BaseType->getAnyNominal(), OS);
    }
  }
  unsigned USREnd = SS.size();

  unsigned TypenameBegin = SS.size();
  if (VD->hasInterfaceType()) {
    llvm::raw_svector_ostream OS(SS);
    PrintOptions Options;
    Options.PrintNameAliasUnderlyingType = true;
    VD->getInterfaceType().print(OS, Options);
  }
  unsigned TypenameEnd = SS.size();

  unsigned MangledTypeStart = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    SwiftLangSupport::printDeclTypeUSR(VD, OS);
  }
  unsigned MangledTypeEnd = SS.size();

  unsigned MangledContainerTypeStart = SS.size();
  if (ContainerTy && !ContainerTy->hasArchetype()) {
    llvm::raw_svector_ostream OS(SS);
    SwiftLangSupport::printTypeUSR(ContainerTy, OS);
  }
  unsigned MangledContainerTypeEnd = SS.size();

  unsigned DocCommentBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    ide::getDocumentationCommentAsXML(VD, OS);
  }
  unsigned DocCommentEnd = SS.size();

  if (DocCommentEnd == DocCommentBegin) {
    if (auto *Req = ASTPrinter::findConformancesWithDocComment(
        const_cast<ValueDecl*>(VD))) {
      llvm::raw_svector_ostream OS(SS);
      ide::getDocumentationCommentAsXML(Req, OS);
    }
    DocCommentEnd = SS.size();
  }

  unsigned DeclBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    printAnnotatedDeclaration(VD, BaseType, OS);
  }
  unsigned DeclEnd = SS.size();

  unsigned FullDeclBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    SwiftLangSupport::printFullyAnnotatedDeclaration(VD, BaseType, OS);
  }
  unsigned FullDeclEnd = SS.size();

  unsigned GroupBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    auto *GroupVD = InSynthesizedExtension ? BaseType->getAnyNominal() : VD;
    if (auto OP = GroupVD->getGroupName())
      OS << OP.getValue();
  }
  unsigned GroupEnd = SS.size();

  SmallVector<std::pair<unsigned, unsigned>, 4> OverUSROffs;

  ide::walkOverriddenDecls(VD,
    [&](llvm::PointerUnion<const ValueDecl*, const clang::NamedDecl*> D) {
      unsigned OverUSRBegin = SS.size();
      {
        llvm::raw_svector_ostream OS(SS);
        if (auto VD = D.dyn_cast<const ValueDecl*>()) {
          if (SwiftLangSupport::printUSR(VD, OS))
            return;
        } else {
          llvm::SmallString<128> Buf;
          if (clang::index::generateUSRForDecl(
              D.get<const clang::NamedDecl*>(), Buf))
            return;
          OS << Buf.str();
        }
      }
      unsigned OverUSREnd = SS.size();
      OverUSROffs.push_back(std::make_pair(OverUSRBegin, OverUSREnd));
  });

  SmallVector<std::pair<unsigned, unsigned>, 4> RelDeclOffs;
  walkRelatedDecls(VD, [&](const ValueDecl *RelatedDecl, bool DuplicateName) {
    unsigned RelatedDeclBegin = SS.size();
    {
      llvm::raw_svector_ostream OS(SS);
      OS<<"<RelatedName usr=\"";
      SwiftLangSupport::printUSR(RelatedDecl, OS);
      OS<<"\">";
      if (isa<AbstractFunctionDecl>(RelatedDecl) && DuplicateName) {
        // Related decls are generally overloads, so print parameter types to
        // differentiate them.
        PrintOptions PO;
        PO.SkipAttributes = true;
        PO.SkipIntroducerKeywords = true;
        PO.ArgAndParamPrinting = PrintOptions::ArgAndParamPrintingMode::ArgumentOnly;
        XMLEscapingPrinter Printer(OS);
        if (BaseType) {
          PO.setBaseType(BaseType);
          PO.PrintAsMember = true;
        }
        RelatedDecl->print(Printer, PO);
      } else {
        llvm::SmallString<128> Buf;
        {
          llvm::raw_svector_ostream OSBuf(Buf);
          SwiftLangSupport::printDisplayName(RelatedDecl, OSBuf);
        }
        swift::markup::appendWithXMLEscaping(OS, Buf);
      }
      OS<<"</RelatedName>";
    }
    unsigned RelatedDeclEnd = SS.size();
    RelDeclOffs.push_back(std::make_pair(RelatedDeclBegin, RelatedDeclEnd));
  });

  ASTContext &Ctx = VD->getASTContext();

  ClangImporter *Importer = static_cast<ClangImporter*>(
      Ctx.getClangModuleLoader());
  std::string ModuleName;
  auto ClangNode = VD->getClangNode();
  if (ClangNode) {
    auto ClangMod = Importer->getClangOwningModule(ClangNode);
    ModuleName = ClangMod->getFullModuleName();
  } else if (VD->getLoc().isInvalid() && VD->getModuleContext() != MainModule) {
    ModuleName = VD->getModuleContext()->getName().str();
  }
  StringRef ModuleInterfaceName;
  if (auto IFaceGenRef = Lang.getIFaceGenContexts().find(ModuleName, Invok))
    ModuleInterfaceName = IFaceGenRef->getDocumentName();

  UIdent Kind = SwiftLangSupport::getUIDForDecl(VD, IsRef);
  StringRef Name = StringRef(SS.begin()+NameBegin, NameEnd-NameBegin);
  StringRef USR = StringRef(SS.begin()+USRBegin, USREnd-USRBegin);
  StringRef TypeName = StringRef(SS.begin()+TypenameBegin,
                                 TypenameEnd-TypenameBegin);
  StringRef TypeUsr = StringRef(SS.begin()+MangledTypeStart,
                                MangledTypeEnd - MangledTypeStart);

  StringRef ContainerTypeUsr = StringRef(SS.begin()+MangledContainerTypeStart,
                            MangledContainerTypeEnd - MangledContainerTypeStart);
  StringRef DocComment = StringRef(SS.begin()+DocCommentBegin,
                                   DocCommentEnd-DocCommentBegin);
  StringRef AnnotatedDecl = StringRef(SS.begin()+DeclBegin,
                                      DeclEnd-DeclBegin);
  StringRef FullyAnnotatedDecl =
      StringRef(SS.begin() + FullDeclBegin, FullDeclEnd - FullDeclBegin);
  StringRef GroupName = StringRef(SS.begin() + GroupBegin, GroupEnd - GroupBegin);

  llvm::Optional<std::pair<unsigned, unsigned>> DeclarationLoc;
  StringRef Filename;
  getLocationInfo(VD, DeclarationLoc, Filename);
  if (DeclarationLoc.hasValue()) {
    DeclarationLoc = tryRemappingLocToLatestSnapshot(Lang,
                                                     *DeclarationLoc,
                                                     Filename,
                                                     PreviousASTSnaps);
    if (!DeclarationLoc.hasValue())
      return true; // failed to remap.
  }

  SmallVector<StringRef, 4> OverUSRs;
  for (auto Offs : OverUSROffs) {
    OverUSRs.push_back(StringRef(SS.begin()+Offs.first,
                                 Offs.second-Offs.first));
  }

  SmallVector<StringRef, 4> AnnotatedRelatedDecls;
  for (auto Offs : RelDeclOffs) {
    AnnotatedRelatedDecls.push_back(StringRef(SS.begin() + Offs.first,
                                              Offs.second - Offs.first));
  }

  bool IsSystem = VD->getModuleContext()->isSystemModule();
  std::string TypeInterface;

  CursorInfo Info;
  Info.Kind = Kind;
  Info.Name = Name;
  Info.USR = USR;
  Info.TypeName = TypeName;
  Info.TypeUSR = TypeUsr;
  Info.ContainerTypeUSR = ContainerTypeUsr;
  Info.DocComment = DocComment;
  Info.AnnotatedDeclaration = AnnotatedDecl;
  Info.FullyAnnotatedDeclaration = FullyAnnotatedDecl;
  Info.ModuleName = ModuleName;
  Info.ModuleInterfaceName = ModuleInterfaceName;
  Info.DeclarationLoc = DeclarationLoc;
  Info.Filename = Filename;
  Info.OverrideUSRs = OverUSRs;
  Info.AnnotatedRelatedDeclarations = AnnotatedRelatedDecls;
  Info.GroupName = GroupName;
  Info.IsSystem = IsSystem;
  Info.TypeInterface = StringRef();
  Receiver(Info);
  return false;
}

static clang::DeclarationName
getClangDeclarationName(clang::ASTContext &Ctx, NameTranslatingInfo &Info) {
  assert(SwiftLangSupport::getNameKindForUID(Info.NameKind) == NameKind::ObjC);
  if (Info.BaseName.empty() == Info.ArgNames.empty()) {
    // cannot have both.
    return clang::DeclarationName();
  }
  if (!Info.BaseName.empty()) {
    return clang::DeclarationName(&Ctx.Idents.get(Info.BaseName));
  } else {
    ArrayRef<StringRef> Args = llvm::makeArrayRef(Info.ArgNames);
    std::vector<clang::IdentifierInfo *> Pieces(Args.size(), nullptr);
    std::transform(Args.begin(), Args.end(), Pieces.begin(),
      [&](StringRef T) { return &Ctx.Idents.get(T.endswith(":") ?
                                                T.drop_back() : T); });
    return clang::DeclarationName(Ctx.Selectors.getSelector(
      /*Calculate Args*/std::accumulate(Args.begin(), Args.end(), (unsigned)0,
        [](unsigned I, StringRef T) { return I + (T.endswith(":") ? 1 : 0); }),
      Pieces.data()));
  }
}

/// Returns true for failure to resolve.
static bool passNameInfoForDecl(const ValueDecl *VD, NameTranslatingInfo &Info,
                    std::function<void(const NameTranslatingInfo &)> Receiver) {
  switch (SwiftLangSupport::getNameKindForUID(Info.NameKind)) {
  case NameKind::Swift: {

    // FIXME: Implement the swift to objc name translation.
    return true;
  }
  case NameKind::ObjC: {
    ClangImporter *Importer = static_cast<ClangImporter *>(VD->getDeclContext()->
      getASTContext().getClangModuleLoader());

    if (auto *Named = dyn_cast_or_null<clang::NamedDecl>(VD->getClangDecl())) {
      DeclName Name = Importer->importName(Named,
        getClangDeclarationName(Named->getASTContext(), Info));
      NameTranslatingInfo Result;
      Result.NameKind = SwiftLangSupport::getUIDForNameKind(NameKind::Swift);
      Result.BaseName = Name.getBaseName().str();
      Result.ArgNames.resize(Name.getArgumentNames().size());
      std::transform(Name.getArgumentNames().begin(),
                     Name.getArgumentNames().end(),
                     Result.ArgNames.begin(),
                     [](Identifier Id) { return Id.str(); });
      Receiver(Result);
      return false;
    }
    return true;
  }
  }
}

class CursorRangeInfoConsumer : public SwiftASTConsumer {
protected:
  SwiftLangSupport &Lang;
  SwiftInvocationRef ASTInvok;
  StringRef InputFile;
  unsigned Offset;
  unsigned Length;

private:
  const bool TryExistingAST;
  SmallVector<ImmutableTextSnapshotRef, 4> PreviousASTSnaps;

protected:
  ArrayRef<ImmutableTextSnapshotRef> getPreviousASTSnaps() {
    return llvm::makeArrayRef(PreviousASTSnaps);
  }

public:
  CursorRangeInfoConsumer(StringRef InputFile, unsigned Offset, unsigned Length,
                          SwiftLangSupport &Lang, SwiftInvocationRef ASTInvok,
                          bool TryExistingAST)
    : Lang(Lang), ASTInvok(ASTInvok),InputFile(InputFile), Offset(Offset),
      Length(Length), TryExistingAST(TryExistingAST) { }

  bool canUseASTWithSnapshots(ArrayRef<ImmutableTextSnapshotRef> Snapshots) override {
    if (!TryExistingAST) {
      LOG_INFO_FUNC(High, "will resolve using up-to-date AST");
      return false;
    }

    // If there is an existing AST and the offset can be mapped back to the
    // document snapshot that was used to create it, then use that AST.
    // The downside is that we may return stale information, but we get the
    // benefit of increased responsiveness, since the request will not be
    // blocked waiting on the AST to be fully typechecked.

    ImmutableTextSnapshotRef InputSnap;
    if (auto EditorDoc = Lang.getEditorDocuments().findByPath(InputFile))
      InputSnap = EditorDoc->getLatestSnapshot();
      if (!InputSnap)
        return false;

    auto mappedBackOffset = [&]()->llvm::Optional<unsigned> {
      for (auto &Snap : Snapshots) {
        if (Snap->isFromSameBuffer(InputSnap)) {
          if (Snap->getStamp() == InputSnap->getStamp())
            return Offset;

          auto OptOffset = mapOffsetToOlderSnapshot(Offset, InputSnap, Snap);
          if (!OptOffset.hasValue())
            return None;

          // Check that the new and old offset still point to the same token.
          StringRef NewTok = getSourceToken(Offset, InputSnap);
          if (NewTok.empty())
            return None;
          if (NewTok == getSourceToken(OptOffset.getValue(), Snap))
            return OptOffset;

          return None;
        }
      }
      return None;
    };

    auto OldOffsetOpt = mappedBackOffset();
    if (OldOffsetOpt.hasValue()) {
      Offset = *OldOffsetOpt;
      PreviousASTSnaps.append(Snapshots.begin(), Snapshots.end());
      LOG_INFO_FUNC(High, "will try existing AST");
      return true;
    }

    LOG_INFO_FUNC(High, "will resolve using up-to-date AST");
    return false;
  }
};
static void resolveCursor(SwiftLangSupport &Lang,
                          StringRef InputFile, unsigned Offset,
                          unsigned Length, bool Actionables,
                          SwiftInvocationRef Invok,
                          bool TryExistingAST,
                          std::function<void(const CursorInfo &)> Receiver) {
  assert(Invok);

  class CursorInfoConsumer : public CursorRangeInfoConsumer {
    bool Actionables;
    std::function<void(const CursorInfo &)> Receiver;

  public:
    CursorInfoConsumer(StringRef InputFile, unsigned Offset,
                       unsigned Length, bool Actionables,
                       SwiftLangSupport &Lang,
                       SwiftInvocationRef ASTInvok,
                       bool TryExistingAST,
                       std::function<void(const CursorInfo &)> Receiver)
    : CursorRangeInfoConsumer(InputFile, Offset, Length, Lang, ASTInvok,
                              TryExistingAST), Actionables(Actionables),
      Receiver(std::move(Receiver)){ }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompIns = AstUnit->getCompilerInstance();
      ModuleDecl *MainModule = CompIns.getMainModule();

      unsigned BufferID = AstUnit->getPrimarySourceFile().getBufferID().getValue();
      SourceLoc Loc =
        Lexer::getLocForStartOfToken(CompIns.getSourceMgr(), BufferID, Offset);
      if (Loc.isInvalid()) {
        Receiver({});
        return;
      }

      trace::TracedOperation TracedOp;
      if (trace::enabled()) {
        trace::SwiftInvocation SwiftArgs;
        ASTInvok->raw(SwiftArgs.Args.Args, SwiftArgs.Args.PrimaryFile);
        trace::initTraceFiles(SwiftArgs, CompIns);
        TracedOp.start(trace::OperationKind::CursorInfoForSource, SwiftArgs,
                       {std::make_pair("Offset", std::to_string(Offset))});
      }

      SemaLocResolver Resolver(AstUnit->getPrimarySourceFile());
      SemaToken SemaTok = Resolver.resolve(Loc);
      if (SemaTok.isInvalid()) {
        Receiver({});
        return;
      }

      CompilerInvocation CompInvok;
      ASTInvok->applyTo(CompInvok);

      if (SemaTok.Mod) {
        passCursorInfoForModule(SemaTok.Mod, Lang.getIFaceGenContexts(),
                                CompInvok, Receiver);
      } else {
        ValueDecl *VD = SemaTok.CtorTyRef ? SemaTok.CtorTyRef : SemaTok.ValueD;
        bool Failed = passCursorInfoForDecl(VD, MainModule,
                                            SemaTok.ContainerType,
                                            SemaTok.ContainerType,
                                            SemaTok.IsRef, BufferID, Lang,
                                            CompInvok, getPreviousASTSnaps(),
                                            Receiver);
        if (Failed) {
          if (!getPreviousASTSnaps().empty()) {
            // Attempt again using the up-to-date AST.
            resolveCursor(Lang, InputFile, Offset, Length, Actionables, ASTInvok,
                          /*TryExistingAST=*/false, Receiver);
          } else {
            Receiver({});
          }
        }
      }
    }

    void cancelled() override {
      CursorInfo Info;
      Info.IsCancelled = true;
      Receiver(Info);
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("cursor info failed: " << Error);
      Receiver({});
    }
  };

  auto Consumer = std::make_shared<CursorInfoConsumer>(
    InputFile, Offset, Length, Actionables, Lang, Invok, TryExistingAST,
    Receiver);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  Lang.getASTManager().processASTAsync(Invok, std::move(Consumer), &OncePerASTToken);
}

static void resolveName(SwiftLangSupport &Lang, StringRef InputFile,
                        unsigned Offset, SwiftInvocationRef Invok,
                        bool TryExistingAST,
                        NameTranslatingInfo &Input,
                        std::function<void(const NameTranslatingInfo &)> Receiver) {
  assert(Invok);

  class NameInfoConsumer : public CursorRangeInfoConsumer {
    NameTranslatingInfo Input;
    std::function<void(const NameTranslatingInfo &)> Receiver;

  public:
    NameInfoConsumer(StringRef InputFile, unsigned Offset,
                     SwiftLangSupport &Lang, SwiftInvocationRef ASTInvok,
                     bool TryExistingAST, NameTranslatingInfo Input,
                     std::function<void(const NameTranslatingInfo &)> Receiver)
    : CursorRangeInfoConsumer(InputFile, Offset, 0, Lang, ASTInvok,
                              TryExistingAST), Input(std::move(Input)),
      Receiver(std::move(Receiver)){ }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompIns = AstUnit->getCompilerInstance();

      unsigned BufferID = AstUnit->getPrimarySourceFile().getBufferID().getValue();
      SourceLoc Loc =
        Lexer::getLocForStartOfToken(CompIns.getSourceMgr(), BufferID, Offset);
      if (Loc.isInvalid()) {
        Receiver({});
        return;
      }

      trace::TracedOperation TracedOp;
      if (trace::enabled()) {
        trace::SwiftInvocation SwiftArgs;
        ASTInvok->raw(SwiftArgs.Args.Args, SwiftArgs.Args.PrimaryFile);
        trace::initTraceFiles(SwiftArgs, CompIns);
        TracedOp.start(trace::OperationKind::CursorInfoForSource, SwiftArgs,
                       {std::make_pair("Offset", std::to_string(Offset))});
      }

      SemaLocResolver Resolver(AstUnit->getPrimarySourceFile());
      SemaToken SemaTok = Resolver.resolve(Loc);
      if (SemaTok.isInvalid()) {
        Receiver({});
        return;
      }

      CompilerInvocation CompInvok;
      ASTInvok->applyTo(CompInvok);

      if (SemaTok.Mod) {

      } else {
        bool Failed = passNameInfoForDecl(SemaTok.ValueD, Input, Receiver);
        if (Failed) {
          if (!getPreviousASTSnaps().empty()) {
            // Attempt again using the up-to-date AST.
            resolveName(Lang, InputFile, Offset, ASTInvok,
                        /*TryExistingAST=*/false, Input, Receiver);
          } else {
            Receiver({});
          }
        }
      }
    }

    void cancelled() override {
      NameTranslatingInfo Info;
      Info.IsCancelled = true;
      Receiver(Info);
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("name info failed: " << Error);
      Receiver({});
    }
  };

  auto Consumer = std::make_shared<NameInfoConsumer>(
    InputFile, Offset, Lang, Invok, TryExistingAST, Input, Receiver);

  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  Lang.getASTManager().processASTAsync(Invok, std::move(Consumer),
                                       &OncePerASTToken);
}

static void resolveRange(SwiftLangSupport &Lang,
                          StringRef InputFile, unsigned Offset, unsigned Length,
                          SwiftInvocationRef Invok,
                          bool TryExistingAST,
                          std::function<void(const RangeInfo&)> Receiver) {
  assert(Invok);

  class RangeInfoConsumer : public CursorRangeInfoConsumer {
    std::function<void(const RangeInfo&)> Receiver;

  public:
    RangeInfoConsumer(StringRef InputFile, unsigned Offset, unsigned Length,
                       SwiftLangSupport &Lang, SwiftInvocationRef ASTInvok,
                       bool TryExistingAST,
                       std::function<void(const RangeInfo&)> Receiver)
    : CursorRangeInfoConsumer(InputFile, Offset, Length, Lang, ASTInvok,
                              TryExistingAST), Receiver(std::move(Receiver)){ }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      if (trace::enabled()) {
        // FIXME: Implement tracing
      }
      RangeResolver Resolver(AstUnit->getPrimarySourceFile(), Offset, Length);
      ResolvedRangeInfo Info = Resolver.resolve();

      CompilerInvocation CompInvok;
      ASTInvok->applyTo(CompInvok);
      RangeInfo Result;
      Result.RangeKind = Lang.getUIDForRangeKind(Info.Kind);
      Result.RangeContent = Info.Content;
      switch (Info.Kind) {
      case RangeKind::SingleExpression: {
        SmallString<64> SS;
        llvm::raw_svector_ostream OS(SS);
        Info.Ty.print(OS);
        Result.ExprType = OS.str();
        Receiver(Result);
        return;
      }
      case RangeKind::SingleDecl:
      case RangeKind::MultiStatement:
      case RangeKind::SingleStatement: {
        Receiver(Result);
        return;
      }
      case RangeKind::Invalid:
        if (!getPreviousASTSnaps().empty()) {
          // Attempt again using the up-to-date AST.
          resolveRange(Lang, InputFile, Offset, Length, ASTInvok,
                      /*TryExistingAST=*/false, Receiver);
        } else {
          Receiver(Result);
        }
        return;
      }
    }

    void cancelled() override {
      RangeInfo Info;
      Info.IsCancelled = true;
      Receiver(Info);
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("range info failed: " << Error);
      Receiver({});
    }
  };

  auto Consumer = std::make_shared<RangeInfoConsumer>(
    InputFile, Offset, Length, Lang, Invok, TryExistingAST, Receiver);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  Lang.getASTManager().processASTAsync(Invok, std::move(Consumer),
                                       &OncePerASTToken);
}

void SwiftLangSupport::getCursorInfo(
    StringRef InputFile, unsigned Offset, unsigned Length, bool Actionables,
    ArrayRef<const char *> Args,
    std::function<void(const CursorInfo &)> Receiver) {

  if (auto IFaceGenRef = IFaceGenContexts.get(InputFile)) {
    trace::TracedOperation TracedOp;
    if (trace::enabled()) {
      trace::SwiftInvocation SwiftArgs;
      trace::initTraceInfo(SwiftArgs, InputFile, Args);
      // Do we need to record any files? If yes -- which ones?
      trace::StringPairs OpArgs {
        std::make_pair("DocumentName", IFaceGenRef->getDocumentName()),
        std::make_pair("ModuleOrHeaderName", IFaceGenRef->getModuleOrHeaderName()),
        std::make_pair("Offset", std::to_string(Offset))};
      TracedOp.start(trace::OperationKind::CursorInfoForIFaceGen,
                     SwiftArgs, OpArgs);
    }

    IFaceGenRef->accessASTAsync([this, IFaceGenRef, Offset, Receiver] {
      SwiftInterfaceGenContext::ResolvedEntity Entity;
      Entity = IFaceGenRef->resolveEntityForOffset(Offset);
      if (Entity.isResolved()) {
        CompilerInvocation Invok;
        IFaceGenRef->applyTo(Invok);
        if (Entity.Mod) {
          passCursorInfoForModule(Entity.Mod, IFaceGenContexts, Invok,
                                  Receiver);
        } else {
          // FIXME: Should pass the main module for the interface but currently
          // it's not necessary.
          passCursorInfoForDecl(
              Entity.Dcl, /*MainModule*/ nullptr, Type(), Type(), Entity.IsRef,
              /*OrigBufferID=*/None, *this, Invok, {}, Receiver);
        }
      } else {
        Receiver({});
      }
    });
    return;
  }

  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, InputFile, Error);
  if (!Invok) {
    // FIXME: Report it as failed request.
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver({});
    return;
  }

  resolveCursor(*this, InputFile, Offset, Length, Actionables, Invok,
                /*TryExistingAST=*/true, Receiver);
}

void SwiftLangSupport::
getRangeInfo(StringRef InputFile, unsigned Offset, unsigned Length,
             ArrayRef<const char *> Args,
             std::function<void(const RangeInfo&)> Receiver) {
  if (IFaceGenContexts.get(InputFile)) {
    // FIXME: return range info for generated interfaces.
    Receiver(RangeInfo());
    return;
  }
  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, InputFile, Error);
  if (!Invok) {
    // FIXME: Report it as failed request.
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver(RangeInfo());
    return;
  }
  if (Length == 0) {
    Receiver(RangeInfo());
    return;
  }
  resolveRange(*this, InputFile, Offset, Length, Invok, /*TryExistingAST=*/true,
                Receiver);
}

void SwiftLangSupport::
getNameInfo(StringRef InputFile, unsigned Offset, NameTranslatingInfo &Input,
            ArrayRef<const char *> Args,
            std::function<void(const NameTranslatingInfo &)> Receiver) {

  if (auto IFaceGenRef = IFaceGenContexts.get(InputFile)) {
    trace::TracedOperation TracedOp;
    if (trace::enabled()) {
      trace::SwiftInvocation SwiftArgs;
      trace::initTraceInfo(SwiftArgs, InputFile, Args);
      // Do we need to record any files? If yes -- which ones?
      trace::StringPairs OpArgs {
        std::make_pair("DocumentName", IFaceGenRef->getDocumentName()),
        std::make_pair("ModuleOrHeaderName", IFaceGenRef->getModuleOrHeaderName()),
        std::make_pair("Offset", std::to_string(Offset))};
      TracedOp.start(trace::OperationKind::CursorInfoForIFaceGen,
                     SwiftArgs, OpArgs);
    }

    IFaceGenRef->accessASTAsync([this, IFaceGenRef, Offset, Input, Receiver] {
      SwiftInterfaceGenContext::ResolvedEntity Entity;
      Entity = IFaceGenRef->resolveEntityForOffset(Offset);
      if (Entity.isResolved()) {
        CompilerInvocation Invok;
        IFaceGenRef->applyTo(Invok);
        if (Entity.Mod) {
          // Module is ignored
        } else {
          NameTranslatingInfo NewInput = Input;
          // FIXME: Should pass the main module for the interface but currently
          // it's not necessary.
          passNameInfoForDecl(Entity.Dcl, NewInput, Receiver);
        }
      } else {
        Receiver({});
      }
    });
    return;
  }

  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, InputFile, Error);
  if (!Invok) {
    // FIXME: Report it as failed request.
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver({});
    return;
  }

  resolveName(*this, InputFile, Offset, Invok, /*TryExistingAST=*/true, Input,
              Receiver);
}

static void
resolveCursorFromUSR(SwiftLangSupport &Lang, StringRef InputFile, StringRef USR,
                     SwiftInvocationRef Invok, bool TryExistingAST,
                     std::function<void(const CursorInfo &)> Receiver) {
  assert(Invok);

  class CursorInfoConsumer : public SwiftASTConsumer {
    std::string InputFile;
    StringRef USR;
    SwiftLangSupport &Lang;
    SwiftInvocationRef ASTInvok;
    const bool TryExistingAST;
    std::function<void(const CursorInfo &)> Receiver;
    SmallVector<ImmutableTextSnapshotRef, 4> PreviousASTSnaps;

  public:
    CursorInfoConsumer(StringRef InputFile, StringRef USR,
                       SwiftLangSupport &Lang, SwiftInvocationRef ASTInvok,
                       bool TryExistingAST,
                       std::function<void(const CursorInfo &)> Receiver)
        : InputFile(InputFile), USR(USR), Lang(Lang),
          ASTInvok(std::move(ASTInvok)), TryExistingAST(TryExistingAST),
          Receiver(std::move(Receiver)) {}

    bool canUseASTWithSnapshots(
        ArrayRef<ImmutableTextSnapshotRef> Snapshots) override {
      if (!TryExistingAST) {
        LOG_INFO_FUNC(High, "will resolve using up-to-date AST");
        return false;
      }

      if (!Snapshots.empty()) {
        PreviousASTSnaps.append(Snapshots.begin(), Snapshots.end());
        LOG_INFO_FUNC(High, "will try existing AST");
        return true;
      }

      LOG_INFO_FUNC(High, "will resolve using up-to-date AST");
      return false;
    }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompIns = AstUnit->getCompilerInstance();
      ModuleDecl *MainModule = CompIns.getMainModule();

      unsigned BufferID =
          AstUnit->getPrimarySourceFile().getBufferID().getValue();

      trace::TracedOperation TracedOp;
      if (trace::enabled()) {
        trace::SwiftInvocation SwiftArgs;
        ASTInvok->raw(SwiftArgs.Args.Args, SwiftArgs.Args.PrimaryFile);
        trace::initTraceFiles(SwiftArgs, CompIns);
        TracedOp.start(trace::OperationKind::CursorInfoForSource, SwiftArgs,
                       {std::make_pair("USR", USR)});
      }

      if (USR.startswith("c:")) {
        LOG_WARN_FUNC("lookup for C/C++/ObjC USRs not implemented");
        Receiver({});
        return;
      }

      auto &context = CompIns.getASTContext();
      std::string error;
      Decl *D = ide::getDeclFromUSR(context, USR, error);

      if (!D) {
        Receiver({});
        return;
      }

      CompilerInvocation CompInvok;
      ASTInvok->applyTo(CompInvok);

      if (auto *M = dyn_cast<ModuleDecl>(D)) {
        passCursorInfoForModule(M, Lang.getIFaceGenContexts(), CompInvok,
                                Receiver);
      } else if (auto *VD = dyn_cast<ValueDecl>(D)) {
        auto *DC = VD->getDeclContext();
        Type selfTy;
        if (DC->isTypeContext()) {
          selfTy = DC->getSelfInterfaceType();
          selfTy = VD->getInnermostDeclContext()->mapTypeIntoContext(selfTy);
        }
        bool Failed =
            passCursorInfoForDecl(VD, MainModule, selfTy, Type(),
                                  /*isRef=*/false, BufferID, Lang, CompInvok,
                                  PreviousASTSnaps, Receiver);
        if (Failed) {
          if (!PreviousASTSnaps.empty()) {
            // Attempt again using the up-to-date AST.
            resolveCursorFromUSR(Lang, InputFile, USR, ASTInvok,
                                 /*TryExistingAST=*/false, Receiver);
          } else {
            Receiver({});
          }
        }
      }
    }

    void cancelled() override {
      CursorInfo Info;
      Info.IsCancelled = true;
      Receiver(Info);
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("cursor info failed: " << Error);
      Receiver({});
    }
  };

  auto Consumer = std::make_shared<CursorInfoConsumer>(
      InputFile, USR, Lang, Invok, TryExistingAST, Receiver);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  Lang.getASTManager().processASTAsync(Invok, std::move(Consumer),
                                       &OncePerASTToken);
}

void SwiftLangSupport::getCursorInfoFromUSR(
    StringRef filename, StringRef USR, ArrayRef<const char *> args,
    std::function<void(const CursorInfo &)> receiver) {
  if (auto IFaceGenRef = IFaceGenContexts.get(filename)) {
    LOG_WARN_FUNC("info from usr for generated interface not implemented yet");
    receiver({});
    return;
  }

  std::string error;
  SwiftInvocationRef invok = ASTMgr->getInvocation(args, filename, error);
  if (!invok) {
    // FIXME: Report it as failed request.
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << error);
    receiver({});
    return;
  }

  resolveCursorFromUSR(*this, filename, USR, invok, /*TryExistingAST=*/true,
                       receiver);
}

//===----------------------------------------------------------------------===//
// SwiftLangSupport::findUSRRange
//===----------------------------------------------------------------------===//

llvm::Optional<std::pair<unsigned, unsigned>>
SwiftLangSupport::findUSRRange(StringRef DocumentName, StringRef USR) {
  if (auto IFaceGenRef = IFaceGenContexts.get(DocumentName))
    return IFaceGenRef->findUSRRange(USR);

  // Only works for a module interface document currently.
  // FIXME: Report it as failed request.
  return None;
}

//===----------------------------------------------------------------------===//
// SwiftLangSupport::findRelatedIdentifiersInFile
//===----------------------------------------------------------------------===//

namespace {
class RelatedIdScanner : public SourceEntityWalker {
  ValueDecl *Dcl;
  llvm::SmallVectorImpl<std::pair<unsigned, unsigned>> &Ranges;
  SourceManager &SourceMgr;
  unsigned BufferID = -1;
  bool Cancelled = false;

public:
  explicit RelatedIdScanner(SourceFile &SrcFile, unsigned BufferID,
                            ValueDecl *D,
      llvm::SmallVectorImpl<std::pair<unsigned, unsigned>> &Ranges)
    : Dcl(D), Ranges(Ranges),
      SourceMgr(SrcFile.getASTContext().SourceMgr),
      BufferID(BufferID) {
  }

private:
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    if (Cancelled)
      return false;
    if (D == Dcl)
      return passId(Range);
    return true;
  }
  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                          SemaReferenceKind Kind) override {
    if (Cancelled)
      return false;
    if (CtorTyRef)
      D = CtorTyRef;
    if (D == Dcl)
      return passId(Range);
    return true;
  }

  bool passId(CharSourceRange Range) {
    unsigned Offset = SourceMgr.getLocOffsetInBuffer(Range.getStart(),BufferID);
    Ranges.push_back({ Offset, Range.getByteLength() });
    return !Cancelled;
  }
};

}

void SwiftLangSupport::findRelatedIdentifiersInFile(
    StringRef InputFile, unsigned Offset,
    ArrayRef<const char *> Args,
    std::function<void(const RelatedIdentsInfo &)> Receiver) {

  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, InputFile, Error);
  if (!Invok) {
    // FIXME: Report it as failed request.
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver({});
    return;
  }

  class RelatedIdConsumer : public SwiftASTConsumer {
    unsigned Offset;
    std::function<void(const RelatedIdentsInfo &)> Receiver;
    SwiftInvocationRef Invok;

  public:
    RelatedIdConsumer(unsigned Offset,
                      std::function<void(const RelatedIdentsInfo &)> Receiver,
                      SwiftInvocationRef Invok)
      : Offset(Offset), Receiver(std::move(Receiver)), Invok(Invok) { }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompInst = AstUnit->getCompilerInstance();
      auto &SrcFile = AstUnit->getPrimarySourceFile();

      trace::TracedOperation TracedOp;

      SmallVector<std::pair<unsigned, unsigned>, 8> Ranges;

      auto Action = [&]() {
        if (trace::enabled()) {
          trace::SwiftInvocation SwiftArgs;
          Invok->raw(SwiftArgs.Args.Args, SwiftArgs.Args.PrimaryFile);
          trace::initTraceFiles(SwiftArgs, CompInst);
          TracedOp.start(trace::OperationKind::RelatedIdents, SwiftArgs,
                        {std::make_pair("Offset", std::to_string(Offset))});
        }

        unsigned BufferID = SrcFile.getBufferID().getValue();
        SourceLoc Loc =
          Lexer::getLocForStartOfToken(CompInst.getSourceMgr(), BufferID, Offset);
        if (Loc.isInvalid())
          return;

        SemaLocResolver Resolver(SrcFile);
        SemaToken SemaTok = Resolver.resolve(Loc);
        if (SemaTok.isInvalid())
          return;
        if (SemaTok.IsKeywordArgument)
          return;

        ValueDecl *VD = SemaTok.CtorTyRef ? SemaTok.CtorTyRef : SemaTok.ValueD;
        if (!VD)
          return; // This was a module reference.

        // Only accept pointing to an identifier.
        if (!SemaTok.IsRef &&
            (isa<ConstructorDecl>(VD) ||
             isa<DestructorDecl>(VD) ||
             isa<SubscriptDecl>(VD)))
          return;
        if (VD->getName().isOperator())
          return;

        RelatedIdScanner Scanner(SrcFile, BufferID, VD, Ranges);
        if (DeclContext *LocalDC = VD->getDeclContext()->getLocalContext()) {
          Scanner.walk(LocalDC);
        } else {
          Scanner.walk(SrcFile);
        }
      };
      Action();

      RelatedIdentsInfo Info;
      Info.Ranges = Ranges;
      Receiver(Info);
    }

    void cancelled() override {
      RelatedIdentsInfo Info;
      Info.IsCancelled = true;
      Receiver(Info);
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("related idents failed: " << Error);
      Receiver({});
    }
  };

  auto Consumer = std::make_shared<RelatedIdConsumer>(Offset, Receiver, Invok);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  ASTMgr->processASTAsync(Invok, std::move(Consumer), &OncePerASTToken);
}
