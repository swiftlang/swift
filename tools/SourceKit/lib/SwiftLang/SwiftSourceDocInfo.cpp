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
#include "SourceKit/Support/FileSystemProvider.h"
#include "SourceKit/Support/ImmutableTextBuffer.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/UIdent.h"

#include "swift/AST/ASTDemangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LookupKinds.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SwiftNameTranslation.h"
#include "swift/AST/GenericSignature.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/IDE/Utils.h"
#include "swift/IDE/Refactoring.h"
#include "swift/IDE/IDERequests.h"
#include "swift/Markup/XMLUtils.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/SymbolGraphGen/SymbolGraphGen.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/SourceManager.h"
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
  void printTypeRef(
      Type T, const TypeDecl *TD, Identifier Name,
      PrintNameContext NameContext = PrintNameContext::Normal) override {
    printXML("<Type usr=\"");
    SwiftLangSupport::printUSR(TD, OS);
    printXML("\">");
    StreamPrinter::printTypeRef(T, TD, Name, NameContext);
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

  void printTypeRef(
      Type T, const TypeDecl *TD, Identifier name,
      PrintNameContext NameContext = PrintNameContext::Normal) override {
    auto tag = getTagForDecl(TD, /*isRef=*/true);
    openTagWithUSRForDecl(tag, TD);
    insideRef = true;
    XMLEscapingPrinter::printTypeRef(T, TD, name, NameContext);
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
  if (!VD->getDeclContext()->isTypeContext())
    return Type();

  return Ty->getRValueType()->getInOutObjectType()->getMetatypeInstanceType();
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

  // VD may be a compiler synthesized member, constructor, or shorthand argument
  // so always print it even if it's implicit.
  //
  // FIXME: Update PrintOptions::printQuickHelpDeclaration to print implicit
  // decls by default. That causes issues due to newlines being printed before
  // implicit OpaqueTypeDecls at time of writing.
  PO.TreatAsExplicitDeclList.push_back(VD);

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

  // VD may be a compiler synthesized member, constructor, or shorthand argument
  // so always print it even if it's implicit.
  //
  // FIXME: Update PrintOptions::printQuickHelpDeclaration to print implicit
  // decls by default. That causes issues due to newlines being printed before
  // implicit OpaqueTypeDecls at time of writing.
  PO.TreatAsExplicitDeclList.push_back(VD);

  VD->print(Printer, PO);
}

void SwiftLangSupport::printFullyAnnotatedDeclaration(const ExtensionDecl *ED,
                                                      raw_ostream &OS) {
  FullyAnnotatedDeclarationPrinter Printer(OS);
  PrintOptions PO = PrintOptions::printQuickHelpDeclaration();
  ED->print(Printer, PO);
}

void SwiftLangSupport::printFullyAnnotatedSynthesizedDeclaration(
    const swift::ValueDecl *VD, TypeOrExtensionDecl Target,
    llvm::raw_ostream &OS) {
  FullyAnnotatedDeclarationPrinter Printer(OS);
  PrintOptions PO = PrintOptions::printQuickHelpDeclaration();
  PO.initForSynthesizedExtension(Target);
  PO.PrintAsMember = true;
  VD->print(Printer, PO);
}

void SwiftLangSupport::printFullyAnnotatedSynthesizedDeclaration(
    const swift::ExtensionDecl *ED, TypeOrExtensionDecl Target,
    llvm::raw_ostream &OS) {
  FullyAnnotatedDeclarationPrinter Printer(OS);
  PrintOptions PO = PrintOptions::printQuickHelpDeclaration();
  PO.initForSynthesizedExtension(Target);
  ED->print(Printer, PO);
}

template <typename FnTy>
static void walkRelatedDecls(const ValueDecl *VD, const FnTy &Fn) {
  if (isa<ParamDecl>(VD))
    return; // Parameters don't have interesting related declarations.

  auto &ctx = VD->getASTContext();

  llvm::SmallDenseMap<DeclName, unsigned, 16> NamesSeen;
  ++NamesSeen[VD->getName()];


  auto *DC = VD->getDeclContext();
  bool typeLookup = DC->isTypeContext();

  SmallVector<ValueDecl *, 4> results;

  if (typeLookup) {
    auto type = DC->getDeclaredInterfaceType();
    if (!type->is<ErrorType>()) {
      DC->lookupQualified(type, DeclNameRef(VD->getBaseName()),
                          NL_QualifiedDefault, results);
    }
  } else {
    namelookup::lookupInModule(DC->getModuleScopeContext(),
                               VD->getBaseName(), results,
                               NLKind::UnqualifiedLookup,
                               namelookup::ResolutionKind::Overloadable,
                               DC->getModuleScopeContext(),
                               NL_UnqualifiedDefault);
  }

  SmallVector<ValueDecl *, 8> RelatedDecls;
  for (auto result : results) {
    if (result->getAttrs().isUnavailable(ctx))
      continue;

    if (result != VD) {
      ++NamesSeen[result->getName()];
      RelatedDecls.push_back(result);
    }
  }

  // Now provide the results along with whether the name is duplicate or not.
  for (auto result : RelatedDecls)
    Fn(result, typeLookup, NamesSeen[result->getName()] > 1);
}

//===----------------------------------------------------------------------===//
// SwiftLangSupport::getCursorInfo
//===----------------------------------------------------------------------===//

static StringRef getSourceToken(unsigned Offset,
                                ImmutableTextSnapshotRef Snap) {
  auto MemBuf = Snap->getBuffer()->getInternalBuffer();

  // FIXME: Invalid offset shouldn't reach here.
  if (Offset >= MemBuf->getBufferSize())
    return StringRef();

  SourceManager SM;
  auto MemBufRef = llvm::MemoryBuffer::getMemBuffer(MemBuf->getBuffer(),
                                                 MemBuf->getBufferIdentifier());
  auto BufId = SM.addNewSourceBuffer(std::move(MemBufRef));
  SourceLoc Loc = SM.getLocForOffset(BufId, Offset);
  return Lexer::getTokenAtLocation(SM, Loc).getText();
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

/// Tries to remap the location from a previous snapshot to the latest one and
/// then sets the location's line and column.
static void mapLocToLatestSnapshot(
    SwiftLangSupport &Lang, LocationInfo &Location,
    ArrayRef<ImmutableTextSnapshotRef> PreviousASTSnaps) {
  auto EditorDoc = Lang.getEditorDocuments()->findByPath(Location.Filename);
  if (!EditorDoc)
    return;

  ImmutableTextSnapshotRef LatestSnap = EditorDoc->getLatestSnapshot();
  if (!LatestSnap)
    return;

  for (auto &PrevSnap : PreviousASTSnaps) {
    if (PrevSnap->isFromSameBuffer(LatestSnap)) {
      if (PrevSnap->getStamp() == LatestSnap->getStamp())
        break;

      auto OptBegin = mapOffsetToNewerSnapshot(Location.Offset,
                                               PrevSnap, LatestSnap);
      if (!OptBegin.hasValue()) {
        Location.Filename = StringRef();
        return;
      }

      auto OptEnd = mapOffsetToNewerSnapshot(Location.Offset +
                                             Location.Length,
                                             PrevSnap, LatestSnap);
      if (!OptEnd.hasValue()) {
        Location.Filename = StringRef();
        return;
      }

      Location.Offset = *OptBegin;
      Location.Length = *OptEnd - *OptBegin;
    }
  }

  std::tie(Location.Line, Location.Column) =
      LatestSnap->getBuffer()->getLineAndColumn(Location.Offset);
}


/// Returns true for error.
static bool passCursorInfoForModule(ModuleEntity Mod,
                                    SwiftInterfaceGenMap &IFaceGenContexts,
                                    const CompilerInvocation &Invok,
                       std::function<void(const RequestResult<CursorInfoData> &)> Receiver) {
  std::string FullName = Mod.getFullName();
  SmallVector<CursorSymbolInfo, 1> Symbols;
  SmallVector<StringRef, 4> ModuleGroups;

  CursorSymbolInfo &Symbol = Symbols.emplace_back();
  Symbol.Kind = SwiftLangSupport::getUIDForModuleRef();
  Symbol.Name = Mod.getName();
  Symbol.ModuleName = FullName;
  if (auto IFaceGenRef = IFaceGenContexts.find(Symbol.ModuleName, Invok))
    Symbol.ModuleInterfaceName = IFaceGenRef->getDocumentName();
  Symbol.IsSystem = Mod.isSystemModule();
  if (auto MD = Mod.getAsSwiftModule()) {
    ide::collectModuleGroups(const_cast<ModuleDecl *>(MD), ModuleGroups);
    Symbol.ModuleGroupArray = llvm::makeArrayRef(ModuleGroups);
  }

  CursorInfoData Data;
  Data.Symbols = llvm::makeArrayRef(Symbols);
  Receiver(RequestResult<CursorInfoData>::fromResult(Data));
  return false;
}

static void
collectAvailableRenameInfo(const ValueDecl *VD, Optional<RenameRefInfo> RefInfo,
                           SmallVectorImpl<RefactoringInfo> &Refactorings) {
  SmallVector<RenameAvailabilityInfo, 2> Renames;
  collectRenameAvailabilityInfo(VD, RefInfo, Renames);
  for (auto Info : Renames) {
    Refactorings.emplace_back(
        SwiftLangSupport::getUIDForRefactoringKind(Info.Kind),
        ide::getDescriptiveRefactoringKindName(Info.Kind),
        ide::getDescriptiveRenameUnavailableReason(Info.AvailableKind));
  }
}

static void collectAvailableRefactoringsOtherThanRename(
    ResolvedCursorInfo CursorInfo,
    SmallVectorImpl<RefactoringInfo> &Refactorings) {
  SmallVector<RefactoringKind, 8> Kinds;
  collectAvailableRefactorings(CursorInfo, Kinds, /*ExcludeRename*/ true);
  for (auto Kind : Kinds) {
    Refactorings.emplace_back(SwiftLangSupport::getUIDForRefactoringKind(Kind),
                              ide::getDescriptiveRefactoringKindName(Kind),
                              StringRef());
  }
}

static Optional<unsigned>
getParamParentNameOffset(const ValueDecl *VD, SourceLoc Cursor) {
  if (Cursor.isInvalid())
    return None;
  SourceLoc Loc;
  if (auto PD = dyn_cast<ParamDecl>(VD)) {

    // Avoid returning parent loc for internal-only names.
    if (PD->getArgumentNameLoc().isValid() && PD->getArgumentNameLoc() != Cursor)
      return None;
    auto *DC = PD->getDeclContext();
    switch (DC->getContextKind()) {
      case DeclContextKind::SubscriptDecl:
        Loc = cast<SubscriptDecl>(DC)->getNameLoc();
        break;
      case DeclContextKind::AbstractFunctionDecl:
        Loc = cast<AbstractFunctionDecl>(DC)->getNameLoc();
        break;
      default:
        break;
    }
  }
  if (Loc.isInvalid())
    return None;
  auto &SM = VD->getASTContext().SourceMgr;
  return SM.getLocOffsetInBuffer(Loc, SM.findBufferContainingLoc(Loc));
}

static StringRef
getModuleName(const ValueDecl *VD, llvm::BumpPtrAllocator &Allocator,
              ModuleDecl *IgnoreModule = nullptr) {
  ASTContext &Ctx = VD->getASTContext();
  ClangImporter *Importer =
      static_cast<ClangImporter *>(Ctx.getClangModuleLoader());
  auto ClangNode = VD->getClangNode();
  if (ClangNode) {
    auto ClangMod = Importer->getClangOwningModule(ClangNode);
    if (ClangMod)
      return copyString(Allocator, ClangMod->getFullModuleName());
  } else if (VD->getModuleContext() != IgnoreModule) {
    ModuleDecl *MD = VD->getModuleContext();
    // If the decl is from a cross-import overlay module, report the
    // overlay's declaring module as the owning module.
    if (ModuleDecl *Declaring = MD->getDeclaringModuleIfCrossImportOverlay())
      MD = Declaring;
    return MD->getNameStr();
  }
  return "";
}

struct DeclInfo {
  const ValueDecl *VD;
  Type ContainerType;
  bool IsRef;
  bool IsDynamic;
  ArrayRef<NominalTypeDecl *> ReceiverTypes;

  /// If VD is a synthesized property wrapper backing storage (_foo) or
  /// projected value ($foo) of a property (foo), the property instead.
  /// Otherwise, VD.
  const ValueDecl *OriginalProperty = nullptr;
  bool Unavailable = true;
  Type BaseType;
  bool InSynthesizedExtension = false;

  DeclInfo(const ValueDecl *VD, Type ContainerType, bool IsRef, bool IsDynamic,
           ArrayRef<NominalTypeDecl *> ReceiverTypes,
           const CompilerInvocation &Invoc)
      : VD(VD), ContainerType(ContainerType), IsRef(IsRef),
        IsDynamic(IsDynamic), ReceiverTypes(ReceiverTypes) {
    if (VD == nullptr)
      return;

    // The synthesized properties $foo and _foo aren't unavailable even if
    // the original property foo is, so check them rather than the original
    // property.
    Unavailable = AvailableAttr::isUnavailable(VD);
    // No point computing the rest since they won't be used anyway.
    if (Unavailable)
      return;

    OriginalProperty = VD;
    if (auto *VarD = dyn_cast<VarDecl>(VD)) {
      if (auto *Wrapped = VarD->getOriginalWrappedProperty())
        OriginalProperty = Wrapped;
    }

    BaseType = findBaseTypeForReplacingArchetype(VD, ContainerType);
    InSynthesizedExtension = false;
    if (BaseType) {
      if (auto Target = BaseType->getAnyNominal()) {
        SynthesizedExtensionAnalyzer Analyzer(
            Target, PrintOptions::printModuleInterface(
                        Invoc.getFrontendOptions().PrintFullConvention));
        InSynthesizedExtension = Analyzer.isInSynthesizedExtension(VD);
      }
    }
  }
};

static StringRef copyAndClearString(llvm::BumpPtrAllocator &Allocator,
                                    SmallVectorImpl<char> &Str) {
  auto Ref = copyString(Allocator, StringRef(Str.data(), Str.size()));
  Str.clear();
  return Ref;
}

template <typename T>
static ArrayRef<T> copyAndClearArray(llvm::BumpPtrAllocator &Allocator,
                                     SmallVectorImpl<T> &Array) {
  auto Ref = copyArray(Allocator, llvm::makeArrayRef(Array));
  Array.clear();
  return Ref;
}

static void setLocationInfoForClangNode(ClangNode ClangNode,
                                        ClangImporter *Importer,
                                        LocationInfo &Location) {
  clang::ASTContext &ClangCtx = Importer->getClangASTContext();
  clang::SourceManager &ClangSM = ClangCtx.getSourceManager();

  clang::SourceRange SR = ClangNode.getLocation();
  if (auto MD =
          dyn_cast_or_null<clang::ObjCMethodDecl>(ClangNode.getAsDecl())) {
    SR = clang::SourceRange(MD->getSelectorStartLoc(),
                            MD->getDeclaratorEndLoc());
  }

  clang::CharSourceRange CharRange =
      clang::Lexer::makeFileCharRange(clang::CharSourceRange::getTokenRange(SR),
                                      ClangSM, ClangCtx.getLangOpts());
  if (CharRange.isInvalid())
    return;

  std::pair<clang::FileID, unsigned> Decomp =
      ClangSM.getDecomposedLoc(CharRange.getBegin());
  if (!Decomp.first.isInvalid()) {
    if (auto FE = ClangSM.getFileEntryForID(Decomp.first)) {
      Location.Filename = FE->getName();

      std::pair<clang::FileID, unsigned> EndDecomp =
          ClangSM.getDecomposedLoc(CharRange.getEnd());

      Location.Offset = Decomp.second;
      Location.Length = EndDecomp.second - Decomp.second;
      Location.Line = ClangSM.getLineNumber(Decomp.first, Decomp.second);
      Location.Column = ClangSM.getColumnNumber(Decomp.first, Decomp.second);
    }
  }
}

static unsigned getCharLength(SourceManager &SM, SourceRange TokenRange) {
  SourceLoc CharEndLoc = Lexer::getLocForEndOfToken(SM, TokenRange.End);
  return SM.getByteDistance(TokenRange.Start, CharEndLoc);
}

static void setLocationInfo(const ValueDecl *VD,
                            LocationInfo &Location) {
  ASTContext &Ctx = VD->getASTContext();
  SourceManager &SM = Ctx.SourceMgr;

  auto ClangNode = VD->getClangNode();

  auto Loc = VD->getLoc(/*SerializedOK=*/true);
  if (Loc.isValid()) {
    auto getSignatureRange = [&](const ValueDecl *VD) -> Optional<unsigned> {
      if (auto FD = dyn_cast<AbstractFunctionDecl>(VD)) {
        SourceRange R = FD->getSignatureSourceRange();
        if (R.isValid())
          return getCharLength(SM, R);
      }
      return None;
    };
    unsigned NameLen;
    if (auto SigLen = getSignatureRange(VD)) {
      NameLen = SigLen.getValue();
    } else if (VD->hasName()) {
      NameLen = VD->getBaseName().userFacingName().size();
    } else {
      NameLen = getCharLength(SM, Loc);
    }

    unsigned DeclBufID = SM.findBufferContainingLoc(Loc);
    Location.Filename = SM.getIdentifierForBuffer(DeclBufID);
    Location.Offset = SM.getLocOffsetInBuffer(Loc, DeclBufID);
    Location.Length = NameLen;
    std::tie(Location.Line, Location.Column) = SM.getLineAndColumnInBuffer(
        Loc, DeclBufID);
  } else if (ClangNode) {
    ClangImporter *Importer =
        static_cast<ClangImporter*>(Ctx.getClangModuleLoader());
    setLocationInfoForClangNode(ClangNode, Importer, Location);
  }
}

static llvm::Error
fillSymbolInfo(CursorSymbolInfo &Symbol, const DeclInfo &DInfo,
               ModuleDecl *MainModule, SourceLoc CursorLoc, bool AddSymbolGraph,
               SwiftLangSupport &Lang, const CompilerInvocation &Invoc,
               ArrayRef<ImmutableTextSnapshotRef> PreviousSnaps,
               llvm::BumpPtrAllocator &Allocator) {
  SmallString<256> Buffer;
  SmallVector<StringRef, 4> Strings;
  llvm::raw_svector_ostream OS(Buffer);

  Symbol.DeclarationLang = SwiftLangSupport::getUIDForDeclLanguage(DInfo.VD);
  Symbol.Kind = SwiftLangSupport::getUIDForDecl(DInfo.VD, DInfo.IsRef);

  SwiftLangSupport::printDisplayName(DInfo.VD, OS);
  Symbol.Name = copyAndClearString(Allocator, Buffer);

  SwiftLangSupport::printUSR(DInfo.OriginalProperty, OS);
  if (DInfo.InSynthesizedExtension) {
    OS << LangSupport::SynthesizedUSRSeparator;
    SwiftLangSupport::printUSR(DInfo.BaseType->getAnyNominal(), OS);
  }
  Symbol.USR = copyAndClearString(Allocator, Buffer);

  {
    PrintOptions Options;
    Options.PrintTypeAliasUnderlyingType = true;
    DInfo.VD->getInterfaceType().print(OS, Options);
  }
  Symbol.TypeName = copyAndClearString(Allocator, Buffer);

  SwiftLangSupport::printDeclTypeUSR(DInfo.VD, OS);
  Symbol.TypeUSR = copyAndClearString(Allocator, Buffer);

  if (DInfo.ContainerType && !DInfo.ContainerType->hasArchetype()) {
    SwiftLangSupport::printTypeUSR(DInfo.ContainerType, OS);
  }
  Symbol.ContainerTypeUSR = copyAndClearString(Allocator, Buffer);

  ide::getDocumentationCommentAsXML(DInfo.OriginalProperty, OS);
  Symbol.DocComment = copyAndClearString(Allocator, Buffer);

  {
    auto *Group = DInfo.InSynthesizedExtension ? DInfo.BaseType->getAnyNominal()
                                               : DInfo.VD;
    if (auto Name = Group->getGroupName())
      Symbol.GroupName = Name.getValue();
  }

  ide::getLocalizationKey(DInfo.VD, OS);
  Symbol.LocalizationKey = copyAndClearString(Allocator, Buffer);

  printAnnotatedDeclaration(DInfo.VD, DInfo.BaseType, OS);
  Symbol.AnnotatedDeclaration = copyAndClearString(Allocator, Buffer);

  SwiftLangSupport::printFullyAnnotatedDeclaration(DInfo.VD, DInfo.BaseType,
                                                   OS);
  Symbol.FullyAnnotatedDeclaration = copyAndClearString(Allocator, Buffer);

  if (AddSymbolGraph) {
    SmallVector<symbolgraphgen::PathComponent, 4> PathComponents;
    SmallVector<symbolgraphgen::FragmentInfo, 8> FragmentInfos;
    symbolgraphgen::SymbolGraphOptions Options{
        "",
        Invoc.getLangOptions().Target,
        /*PrettyPrint=*/false,
        AccessLevel::Private,
        /*EmitSynthesizedMembers*/ false,
        /*PrintMessages*/ false,
        /*SkipInheritedDocs*/ false,
    };

    symbolgraphgen::printSymbolGraphForDecl(DInfo.VD, DInfo.BaseType,
                                            DInfo.InSynthesizedExtension,
                                            Options, OS, PathComponents,
                                            FragmentInfos);
    Symbol.SymbolGraph = copyAndClearString(Allocator, Buffer);

    SmallVector<ParentInfo, 4> Parents;
    for (auto &Component : PathComponents) {
      SwiftLangSupport::printUSR(Component.VD, OS);
      Parents.emplace_back(copyString(Allocator, Component.Title),
                           Component.Kind,
                           copyAndClearString(Allocator, Buffer));
    };
    Symbol.ParentContexts = copyArray(Allocator, llvm::makeArrayRef(Parents));

    SmallVector<ReferencedDeclInfo, 8> ReferencedDecls;
    for (auto &FI: FragmentInfos) {
      SmallVector<ParentInfo, 4> FIParents;
      for (auto &Component: FI.ParentContexts) {
        SwiftLangSupport::printUSR(Component.VD, OS);
        FIParents.emplace_back(copyString(Allocator, Component.Title),
                               Component.Kind,
                               copyAndClearString(Allocator, Buffer));
      }

      ASTContext &Ctx = FI.VD->getASTContext();
      StringRef Filename = "";
      if (auto Loc = FI.VD->getLoc(/*SerializedOK=*/true)) {
        Filename = Ctx.SourceMgr.getDisplayNameForLoc(Loc);
      } else if (auto ClangNode = FI.VD->getClangNode()) {
        auto Loc = ClangNode.getLocation();
        if (Loc.isValid()) {
          Filename = Ctx.getClangModuleLoader()->getClangASTContext()
              .getSourceManager()
              .getFilename(Loc);
        }
      }

      SwiftLangSupport::printUSR(FI.VD, OS);
      ReferencedDecls.emplace_back(
          copyAndClearString(Allocator, Buffer),
          SwiftLangSupport::getUIDForDeclLanguage(FI.VD),
          swift::getAccessLevelSpelling(FI.VD->getFormalAccess()), Filename,
          getModuleName(FI.VD, Allocator),
          FI.VD->getModuleContext()->isSystemModule(),
          FI.VD->isSPI(),
          copyArray(Allocator, llvm::makeArrayRef(FIParents)));
    }
    Symbol.ReferencedSymbols = copyArray(Allocator,
                                         llvm::makeArrayRef(ReferencedDecls));
  }

  Symbol.ModuleName = copyString(Allocator,
                                 getModuleName(DInfo.VD, Allocator,
                                               /*ModuleToIgnore=*/MainModule));
  if (auto IFaceGenRef =
          Lang.getIFaceGenContexts().find(Symbol.ModuleName, Invoc))
    Symbol.ModuleInterfaceName = IFaceGenRef->getDocumentName();

  setLocationInfo(DInfo.OriginalProperty, Symbol.Location);
  if (!Symbol.Location.Filename.empty()) {
    mapLocToLatestSnapshot(Lang, Symbol.Location, PreviousSnaps);
    if (Symbol.Location.Filename.empty()) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          "Failed to remap declaration to latest snapshot.");
    }
  }

  ide::walkOverriddenDecls(
      DInfo.VD,
      [&](llvm::PointerUnion<const ValueDecl *, const clang::NamedDecl *> D) {
        // Could have junk in from previous failing USR print
        Buffer.clear();
        if (auto VD = D.dyn_cast<const ValueDecl *>()) {
          if (SwiftLangSupport::printUSR(VD, OS))
            return;
        } else {
          if (clang::index::generateUSRForDecl(
                  D.get<const clang::NamedDecl *>(), Buffer))
            return;
        }
        Strings.push_back(copyAndClearString(Allocator, Buffer));
      });
  Symbol.OverrideUSRs = copyAndClearArray(Allocator, Strings);

  walkRelatedDecls(DInfo.VD, [&](const ValueDecl *RelatedDecl,
                                 bool UseOriginalBase, bool DuplicateName) {
    OS << "<RelatedName usr=\"";
    SwiftLangSupport::printUSR(RelatedDecl, OS);
    OS << "\">";
    if (isa<AbstractFunctionDecl>(RelatedDecl) && DuplicateName) {
      // Related decls are generally overloads, so print parameter types to
      // differentiate them.
      PrintOptions PO;
      PO.SkipAttributes = true;
      PO.SkipIntroducerKeywords = true;
      PO.ArgAndParamPrinting =
          PrintOptions::ArgAndParamPrintingMode::ArgumentOnly;
      XMLEscapingPrinter Printer(OS);
      if (UseOriginalBase && DInfo.BaseType) {
        PO.setBaseType(DInfo.BaseType);
        PO.PrintAsMember = true;
      }
      RelatedDecl->print(Printer, PO);
    } else {
      SmallString<128> RelatedBuffer;
      llvm::raw_svector_ostream RelatedOS(RelatedBuffer);
      SwiftLangSupport::printDisplayName(RelatedDecl, RelatedOS);
      swift::markup::appendWithXMLEscaping(OS, RelatedBuffer);
    }
    OS << "</RelatedName>";

    Strings.push_back(copyAndClearString(Allocator, Buffer));
  });
  Symbol.AnnotatedRelatedDeclarations = copyAndClearArray(Allocator, Strings);

  for (auto *ReceiverTy : DInfo.ReceiverTypes) {
    if (!SwiftLangSupport::printUSR(ReceiverTy, OS))
      Strings.push_back(copyAndClearString(Allocator, Buffer));
  }
  Symbol.ReceiverUSRs = copyAndClearArray(Allocator, Strings);

  Symbol.IsSystem = DInfo.VD->getModuleContext()->isSystemModule();
  Symbol.IsDynamic = DInfo.IsDynamic;
  Symbol.ParentNameOffset = getParamParentNameOffset(DInfo.VD, CursorLoc);

  return llvm::Error::success();
}

/// Returns true on success, false on error (and sets `Diagnostic` accordingly).
static bool passCursorInfoForDecl(
    const ResolvedCursorInfo &Info, ModuleDecl *MainModule,
    bool AddRefactorings, bool AddSymbolGraph,
    ArrayRef<RefactoringInfo> KnownRefactoringInfo, SwiftLangSupport &Lang,
    const CompilerInvocation &Invoc, std::string &Diagnostic,
    ArrayRef<ImmutableTextSnapshotRef> PreviousSnaps,
    std::function<void(const RequestResult<CursorInfoData> &)> Receiver) {
  DeclInfo OrigInfo(Info.ValueD, Info.ContainerType, Info.IsRef, Info.IsDynamic,
                    Info.ReceiverTypes, Invoc);
  DeclInfo CtorTypeInfo(Info.CtorTyRef, Type(), true, false,
                        ArrayRef<NominalTypeDecl *>(), Invoc);
  DeclInfo &MainInfo = CtorTypeInfo.VD ? CtorTypeInfo : OrigInfo;
  if (MainInfo.Unavailable) {
    Diagnostic = "Unavailable in the current compilation context.";
    return false;
  }

  llvm::BumpPtrAllocator Allocator;

  SmallVector<CursorSymbolInfo, 2> Symbols;
  CursorSymbolInfo &MainSymbol = Symbols.emplace_back();
  // The primary result for constructor calls, eg. `MyType()` should be
  // the type itself, rather than the constructor. The constructor will be
  // added as a secondary result.
  if (auto Err = fillSymbolInfo(MainSymbol, MainInfo, MainModule, Info.Loc,
                                AddSymbolGraph, Lang, Invoc, PreviousSnaps,
                                Allocator)) {
    llvm::handleAllErrors(std::move(Err), [&](const llvm::StringError &E) {
      Diagnostic = E.message();
    });
    return false;
  }
  if (MainInfo.VD != OrigInfo.VD && !OrigInfo.Unavailable) {
    CursorSymbolInfo &CtorSymbol = Symbols.emplace_back();
    if (auto Err = fillSymbolInfo(CtorSymbol, OrigInfo, MainModule, Info.Loc,
                                  AddSymbolGraph, Lang, Invoc, PreviousSnaps,
                                  Allocator)) {
      // Ignore but make sure to remove the partially-filled symbol
      llvm::handleAllErrors(std::move(Err), [&](const llvm::StringError &E) {});
      Symbols.pop_back();
    }
  }

  SmallVector<RefactoringInfo, 8> Refactorings;
  if (AddRefactorings) {
    Optional<RenameRefInfo> RefInfo;
    if (Info.IsRef)
      RefInfo = {Info.SF, Info.Loc, Info.IsKeywordArgument};
    collectAvailableRenameInfo(MainInfo.VD, RefInfo, Refactorings);
    collectAvailableRefactoringsOtherThanRename(Info, Refactorings);
  }
  Refactorings.insert(Refactorings.end(), KnownRefactoringInfo.begin(),
                      KnownRefactoringInfo.end());

  CursorInfoData Data;
  Data.Symbols = llvm::makeArrayRef(Symbols);
  Data.AvailableActions = llvm::makeArrayRef(Refactorings);
  Receiver(RequestResult<CursorInfoData>::fromResult(Data));
  return true;
}

static clang::DeclarationName
getClangDeclarationName(const clang::NamedDecl *ND, NameTranslatingInfo &Info) {
  auto &Ctx = ND->getASTContext();
  auto OrigName = ND->getDeclName();
  assert(SwiftLangSupport::getNameKindForUID(Info.NameKind) == NameKind::ObjC);
  if (Info.BaseName.empty() == Info.ArgNames.empty()) {
    // cannot have both.
    return clang::DeclarationName();
  }
  if (!Info.BaseName.empty()) {
    return clang::DeclarationName(&Ctx.Idents.get(Info.BaseName));
  } else {
    switch (OrigName.getNameKind()) {
    case clang::DeclarationName::ObjCZeroArgSelector:
    case clang::DeclarationName::ObjCOneArgSelector:
    case clang::DeclarationName::ObjCMultiArgSelector:
      break;
    default:
      return clang::DeclarationName();
    }

    auto OrigSel = OrigName.getObjCSelector();
    unsigned NumPieces = OrigSel.isUnarySelector() ? 1 : OrigSel.getNumArgs();
    if (Info.ArgNames.size() > NumPieces)
      return clang::DeclarationName();

    ArrayRef<StringRef> Args = llvm::makeArrayRef(Info.ArgNames);
    std::vector<clang::IdentifierInfo *> Pieces;
    for (unsigned i = 0; i < NumPieces; ++i) {
      if (i >= Info.ArgNames.size() || Info.ArgNames[i].empty()) {
        Pieces.push_back(OrigSel.getIdentifierInfoForSlot(i));
      } else {
        StringRef T = Args[i];
        Pieces.push_back(&Ctx.Idents.get(T.endswith(":") ? T.drop_back() : T));
      }
    }
    return clang::DeclarationName(
        Ctx.Selectors.getSelector(OrigSel.getNumArgs(), Pieces.data()));
  }
}

static DeclName getSwiftDeclName(const ValueDecl *VD,
                                 NameTranslatingInfo &Info) {
  auto &Ctx = VD->getDeclContext()->getASTContext();
  assert(SwiftLangSupport::getNameKindForUID(Info.NameKind) == NameKind::Swift);
  const DeclName OrigName = VD->getName();
  DeclBaseName BaseName = Info.BaseName.empty()
                              ? OrigName.getBaseName()
                              : DeclBaseName(
                                Info.BaseName == "init"
                                  ? DeclBaseName::createConstructor()
                                  : Ctx.getIdentifier(Info.BaseName));
  auto OrigArgs = OrigName.getArgumentNames();
  SmallVector<Identifier, 8> Args(OrigArgs.begin(), OrigArgs.end());
  if (Info.ArgNames.size() > OrigArgs.size())
    return DeclName();
  for (unsigned i = 0; i < OrigArgs.size(); ++i) {
    if (i < Info.ArgNames.size() && !Info.ArgNames[i].empty()) {
      StringRef Arg = Info.ArgNames[i];
      Args[i] = Ctx.getIdentifier(Arg == "_" ? StringRef() : Arg);
    }
  }
  return DeclName(Ctx, BaseName, llvm::makeArrayRef(Args));
}

/// Returns true on success, false on error (and sets `Diagnostic` accordingly).
static bool passNameInfoForDecl(ResolvedCursorInfo CursorInfo,
                                NameTranslatingInfo &Info,
                                std::string &Diagnostic,
                    std::function<void(const RequestResult<NameTranslatingInfo> &)> Receiver) {
  auto *VD = CursorInfo.ValueD;

  // If the given name is not a function name, and the cursor points to
  // a contructor call, we use the type declaration instead of the init
  // declaration to translate the name.
  if (Info.ArgNames.empty() && !Info.IsZeroArgSelector) {
    if (auto *TD = CursorInfo.CtorTyRef) {
      VD = TD;
    }
  }
  switch (SwiftLangSupport::getNameKindForUID(Info.NameKind)) {
  case NameKind::Swift: {
    NameTranslatingInfo Result;
    auto DeclName = getSwiftDeclName(VD, Info);
    if (!DeclName) {
      Diagnostic = "Unable to resolve Swift declaration name.";
      return false;
    }
    auto ResultPair =
        swift::objc_translation::getObjCNameForSwiftDecl(VD, DeclName);
    Identifier Name = ResultPair.first;
    if (!Name.empty()) {
      Result.NameKind = SwiftLangSupport::getUIDForNameKind(NameKind::ObjC);
      Result.BaseName = Name.str();
      Receiver(RequestResult<NameTranslatingInfo>::fromResult(Result));
    } else if (ObjCSelector Selector = ResultPair.second) {
      Result.NameKind = SwiftLangSupport::getUIDForNameKind(NameKind::ObjC);
      SmallString<64> Buffer;
      StringRef Total = Selector.getString(Buffer);
      SmallVector<StringRef, 4> Pieces;
      Total.split(Pieces, ":");
      if (Selector.getNumArgs()) {
        assert(Pieces.back().empty());
        Pieces.pop_back();
      } else {
        Result.IsZeroArgSelector = true;
      }
      Result.ArgNames.insert(Result.ArgNames.begin(), Pieces.begin(), Pieces.end());
      Receiver(RequestResult<NameTranslatingInfo>::fromResult(Result));
    } else {
      Diagnostic = "Unable to resolve name info.";
      return false;
    }
    return true;
  }
  case NameKind::ObjC: {
    ClangImporter *Importer = static_cast<ClangImporter *>(VD->getDeclContext()->
      getASTContext().getClangModuleLoader());

    const clang::NamedDecl *Named = nullptr;
    auto *BaseDecl = VD;

    while (!Named && BaseDecl) {
      Named = dyn_cast_or_null<clang::NamedDecl>(BaseDecl->getClangDecl());
      BaseDecl = BaseDecl->getOverriddenDecl();
    }
    if (!Named) {
      Diagnostic = "Unable to resolve a named declaration.";
      return false;
    }

    auto ObjCName = getClangDeclarationName(Named, Info);
    if (!ObjCName) {
      Diagnostic = "Unable to resolve ObjC declaration name.";
      return false;
    }

    DeclName Name = Importer->importName(Named, ObjCName);
    NameTranslatingInfo Result;
    Result.NameKind = SwiftLangSupport::getUIDForNameKind(NameKind::Swift);
    Result.BaseName = Name.getBaseName().userFacingName();
    llvm::transform(Name.getArgumentNames(),
                    std::back_inserter(Result.ArgNames),
                    [](Identifier Id) { return Id.str(); });
    Receiver(RequestResult<NameTranslatingInfo>::fromResult(Result));
    return true;
  }
  }
}

class CursorRangeInfoConsumer : public SwiftASTConsumer {
protected:
  SwiftLangSupport &Lang;
  SwiftInvocationRef ASTInvok;
  std::string InputFile;
  unsigned Offset;
  unsigned Length;

private:
  const bool TryExistingAST;
  SmallVector<ImmutableTextSnapshotRef, 4> PreviousASTSnaps;

protected:
  bool CancelOnSubsequentRequest;
protected:
  ArrayRef<ImmutableTextSnapshotRef> getPreviousASTSnaps() {
    return llvm::makeArrayRef(PreviousASTSnaps);
  }

public:
  CursorRangeInfoConsumer(StringRef InputFile, unsigned Offset, unsigned Length,
                          SwiftLangSupport &Lang, SwiftInvocationRef ASTInvok,
                          bool TryExistingAST, bool CancelOnSubsequentRequest)
    : Lang(Lang), ASTInvok(ASTInvok),InputFile(InputFile.str()), Offset(Offset),
      Length(Length), TryExistingAST(TryExistingAST),
      CancelOnSubsequentRequest(CancelOnSubsequentRequest) {}

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
    if (auto EditorDoc = Lang.getEditorDocuments()->findByPath(InputFile))
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

static void resolveCursor(
    SwiftLangSupport &Lang, StringRef InputFile, unsigned Offset,
    unsigned Length, bool Actionables, bool SymbolGraph,
    SwiftInvocationRef Invok, bool TryExistingAST,
    bool CancelOnSubsequentRequest,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fileSystem,
    std::function<void(const RequestResult<CursorInfoData> &)> Receiver) {
  assert(Invok);
  assert(fileSystem);

  class CursorInfoConsumer : public CursorRangeInfoConsumer {
    bool Actionables;
    bool SymbolGraph;
    std::function<void(const RequestResult<CursorInfoData> &)> Receiver;

  public:
    CursorInfoConsumer(StringRef InputFile, unsigned Offset,
                       unsigned Length, bool Actionables, bool SymbolGraph,
                       SwiftLangSupport &Lang,
                       SwiftInvocationRef ASTInvok,
                       bool TryExistingAST,
                       bool CancelOnSubsequentRequest,
                       std::function<void(const RequestResult<CursorInfoData> &)> Receiver)
    : CursorRangeInfoConsumer(InputFile, Offset, Length, Lang, ASTInvok,
                              TryExistingAST, CancelOnSubsequentRequest),
      Actionables(Actionables),
      SymbolGraph(SymbolGraph),
      Receiver(std::move(Receiver)){ }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompIns = AstUnit->getCompilerInstance();
      ModuleDecl *MainModule = CompIns.getMainModule();
      SourceManager &SM = CompIns.getSourceMgr();
      unsigned BufferID = AstUnit->getPrimarySourceFile().getBufferID().getValue();
      SourceLoc Loc =
        Lexer::getLocForStartOfToken(SM, BufferID, Offset);
      if (Loc.isInvalid()) {
        Receiver(RequestResult<CursorInfoData>::fromError(
          "Unable to resolve the start of the token."));
        return;
      }

      // Sanitize length.
      if (Length) {
        SourceLoc TokEnd = Lexer::getLocForEndOfToken(SM, Loc);
        SourceLoc EndLoc = SM.getLocForOffset(BufferID, Offset + Length);

        // If TokEnd is not before the given EndLoc, the EndLoc contains no
        // more stuff than this token, so set the length to 0.
        if (SM.isBeforeInBuffer(EndLoc, TokEnd) || TokEnd == EndLoc)
          Length = 0;
      }

      // Retrieve relevant actions on the code under selection.
      llvm::SmallVector<RefactoringInfo, 8> Actions;
      if (Actionables && Length) {
        SmallVector<RefactoringKind, 8> Kinds;
        RangeConfig Range;
        Range.BufferId = BufferID;
        auto Pair = SM.getLineAndColumnInBuffer(Loc);
        Range.Line = Pair.first;
        Range.Column = Pair.second;
        Range.Length = Length;
        bool RangeStartMayNeedRename = false;
        collectAvailableRefactorings(&AstUnit->getPrimarySourceFile(), Range,
                                     RangeStartMayNeedRename, Kinds, {});
        for (RefactoringKind Kind : Kinds) {
          Actions.emplace_back(SwiftLangSupport::getUIDForRefactoringKind(Kind),
                               getDescriptiveRefactoringKindName(Kind),
                               /*UnavailableReason*/ StringRef());
        }
        if (!RangeStartMayNeedRename) {
          // If Length is given, then the cursor-info request should only about
          // collecting available refactorings for the range.
          CursorInfoData Data;
          Data.AvailableActions = llvm::makeArrayRef(Actions);
          Receiver(RequestResult<CursorInfoData>::fromResult(Data));
          return;
        }
        // If the range start may need rename, we fall back to a regular cursor
        // info request to get the available rename kinds.
      }

      auto *File = &AstUnit->getPrimarySourceFile();
      ResolvedCursorInfo CursorInfo =
        evaluateOrDefault(File->getASTContext().evaluator,
                          CursorInfoRequest{CursorInfoOwner(File, Loc)},
                          ResolvedCursorInfo());

      if (CursorInfo.isInvalid()) {
        CursorInfoData Info;
        Info.InternalDiagnostic = "Unable to resolve cursor info.";
        Receiver(RequestResult<CursorInfoData>::fromResult(Info));
        return;
      }
      CompilerInvocation CompInvok;
      ASTInvok->applyTo(CompInvok);

      switch (CursorInfo.Kind) {
      case CursorInfoKind::ModuleRef:
        passCursorInfoForModule(CursorInfo.Mod, Lang.getIFaceGenContexts(),
                                CompInvok, Receiver);
        return;
      case CursorInfoKind::ValueRef: {
        std::string Diagnostic;
        bool Success = passCursorInfoForDecl(
            CursorInfo, MainModule, Actionables, SymbolGraph, Actions, Lang,
            CompInvok, Diagnostic, getPreviousASTSnaps(), Receiver);
        if (!Success) {
          if (!getPreviousASTSnaps().empty()) {
            // Attempt again using the up-to-date AST.
            resolveCursor(Lang, InputFile, Offset, Length, Actionables,
                          SymbolGraph, ASTInvok, /*TryExistingAST=*/false,
                          CancelOnSubsequentRequest, SM.getFileSystem(),
                          Receiver);
          } else {
            CursorInfoData Info;
            Info.InternalDiagnostic = Diagnostic;
            Receiver(RequestResult<CursorInfoData>::fromResult(Info));
          }
        }
        return;
      }
      case CursorInfoKind::ExprStart:
      case CursorInfoKind::StmtStart: {
        if (Actionables) {
          collectAvailableRefactoringsOtherThanRename(CursorInfo, Actions);
          if (!Actions.empty()) {
            CursorInfoData Data;
            Data.AvailableActions = llvm::makeArrayRef(Actions);
            Receiver(RequestResult<CursorInfoData>::fromResult(Data));
            return;
          }
        }

        CursorInfoData Info;
        Info.InternalDiagnostic =
            "Resolved to incomplete expression or statement.";
        Receiver(RequestResult<CursorInfoData>::fromResult(Info));
        return;
      }
      case CursorInfoKind::Invalid: {
        llvm_unreachable("bad sema token kind");
      }
      }
    }

    void cancelled() override {
      Receiver(RequestResult<CursorInfoData>::cancelled());
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("cursor info failed: " << Error);
      Receiver(RequestResult<CursorInfoData>::fromError(Error));
    }
  };

  auto Consumer = std::make_shared<CursorInfoConsumer>(
    InputFile, Offset, Length, Actionables, SymbolGraph, Lang, Invok,
    TryExistingAST, CancelOnSubsequentRequest, Receiver);

  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  static const char OncePerASTTokenWithActionables = 0;
  const void *Once = nullptr;
  if (CancelOnSubsequentRequest)
    Once = Actionables ? &OncePerASTTokenWithActionables : &OncePerASTToken;
  Lang.getASTManager()->processASTAsync(Invok, std::move(Consumer), Once,
                                        fileSystem);
}

static void resolveName(SwiftLangSupport &Lang, StringRef InputFile,
                        unsigned Offset, SwiftInvocationRef Invok,
                        bool TryExistingAST,
                        NameTranslatingInfo &Input,
                        std::function<void(const RequestResult<NameTranslatingInfo> &)> Receiver) {
  assert(Invok);

  class NameInfoConsumer : public CursorRangeInfoConsumer {
    NameTranslatingInfo Input;
    std::function<void(const RequestResult<NameTranslatingInfo> &)> Receiver;

  public:
    NameInfoConsumer(StringRef InputFile, unsigned Offset,
                     SwiftLangSupport &Lang, SwiftInvocationRef ASTInvok,
                     bool TryExistingAST, NameTranslatingInfo Input,
                     std::function<void(const RequestResult<NameTranslatingInfo> &)> Receiver)
    : CursorRangeInfoConsumer(InputFile, Offset, 0, Lang, ASTInvok,
                              TryExistingAST,
                              /*CancelOnSubsequentRequest=*/false),
      Input(std::move(Input)), Receiver(std::move(Receiver)){ }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompIns = AstUnit->getCompilerInstance();

      unsigned BufferID = AstUnit->getPrimarySourceFile().getBufferID().getValue();
      SourceLoc Loc =
        Lexer::getLocForStartOfToken(CompIns.getSourceMgr(), BufferID, Offset);
      if (Loc.isInvalid()) {
        Receiver(RequestResult<NameTranslatingInfo>::fromError(
          "Unable to resolve the start of the token."));
        return;
      }

      auto *File = &AstUnit->getPrimarySourceFile();
      ResolvedCursorInfo CursorInfo =
        evaluateOrDefault(File->getASTContext().evaluator,
                          CursorInfoRequest{CursorInfoOwner(File, Loc)},
                          ResolvedCursorInfo());
      if (CursorInfo.isInvalid()) {
        NameTranslatingInfo Info;
        Info.InternalDiagnostic = "Unable to resolve cursor info.";
        Receiver(RequestResult<NameTranslatingInfo>::fromResult(Info));
        return;
      }

      CompilerInvocation CompInvok;
      ASTInvok->applyTo(CompInvok);

      switch(CursorInfo.Kind) {
      case CursorInfoKind::ModuleRef:
        return;

      case CursorInfoKind::ValueRef: {
        std::string Diagnostic;
        bool Success = passNameInfoForDecl(CursorInfo, Input, Diagnostic,
                                           Receiver);
        if (!Success) {
          if (!getPreviousASTSnaps().empty()) {
            // Attempt again using the up-to-date AST.
            resolveName(Lang, InputFile, Offset, ASTInvok,
                        /*TryExistingAST=*/false, Input, Receiver);
          } else {
            NameTranslatingInfo Info;
            Info.InternalDiagnostic = Diagnostic;
            Receiver(RequestResult<NameTranslatingInfo>::fromResult(Info));
          }
        }
        return;
      }
      case CursorInfoKind::ExprStart:
      case CursorInfoKind::StmtStart: {
        NameTranslatingInfo Info;
        Info.InternalDiagnostic =
            "Resolved to incomplete expression or statement.";
        Receiver(RequestResult<NameTranslatingInfo>::fromResult(Info));
        return;
      }
      case CursorInfoKind::Invalid:
        llvm_unreachable("bad sema token kind.");
      }
    }

    void cancelled() override {
      Receiver(RequestResult<NameTranslatingInfo>::cancelled());
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("name info failed: " << Error);
      Receiver(RequestResult<NameTranslatingInfo>::fromError(Error));
    }
  };

  auto Consumer = std::make_shared<NameInfoConsumer>(
    InputFile, Offset, Lang, Invok, TryExistingAST, Input, Receiver);

  Lang.getASTManager()->processASTAsync(Invok, std::move(Consumer), nullptr,
                                        llvm::vfs::getRealFileSystem());
}

static void resolveRange(SwiftLangSupport &Lang,
                          StringRef InputFile, unsigned Offset, unsigned Length,
                          SwiftInvocationRef Invok,
                          bool TryExistingAST, bool CancelOnSubsequentRequest,
                          std::function<void(const RequestResult<RangeInfo> &)> Receiver) {
  assert(Invok);

  class RangeInfoConsumer : public CursorRangeInfoConsumer {
    std::function<void(const RequestResult<RangeInfo> &)> Receiver;

  public:
    RangeInfoConsumer(StringRef InputFile, unsigned Offset, unsigned Length,
                       SwiftLangSupport &Lang, SwiftInvocationRef ASTInvok,
                       bool TryExistingAST, bool CancelOnSubsequentRequest,
                       std::function<void(const RequestResult<RangeInfo> &)> Receiver)
    : CursorRangeInfoConsumer(InputFile, Offset, Length, Lang, ASTInvok,
                              TryExistingAST, CancelOnSubsequentRequest),
      Receiver(std::move(Receiver)){ }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      // FIXME: Implement tracing
      auto *File = &AstUnit->getPrimarySourceFile();
      ResolvedRangeInfo Info = evaluateOrDefault(File->getASTContext().evaluator,
        RangeInfoRequest(RangeInfoOwner({File, Offset, Length})),
                                                 ResolvedRangeInfo());

      CompilerInvocation CompInvok;
      ASTInvok->applyTo(CompInvok);

      RangeInfo Result;
      Result.RangeKind = Lang.getUIDForRangeKind(Info.Kind);
      if (Info.Kind == RangeKind::Invalid) {
        Result.RangeContent = "";
      } else {
        assert(Info.ContentRange.isValid());
        Result.RangeContent = Info.ContentRange.str();
      }

      switch (Info.Kind) {
      case RangeKind::SingleExpression: {
        SmallString<64> SS;
        llvm::raw_svector_ostream OS(SS);
        Info.ExitInfo.ReturnType->print(OS);
        Result.ExprType = OS.str();
        Receiver(RequestResult<RangeInfo>::fromResult(Result));
        return;
      }
      case RangeKind::SingleDecl:
      case RangeKind::MultiTypeMemberDecl:
      case RangeKind::MultiStatement:
      case RangeKind::SingleStatement: {
        Receiver(RequestResult<RangeInfo>::fromResult(Result));
        return;
      }
      case RangeKind::PartOfExpression:
      case RangeKind::Invalid:
        if (!getPreviousASTSnaps().empty()) {
          // Attempt again using the up-to-date AST.
          resolveRange(Lang, InputFile, Offset, Length, ASTInvok,
                      /*TryExistingAST=*/false, CancelOnSubsequentRequest,
                      Receiver);
        } else {
          Receiver(RequestResult<RangeInfo>::fromResult(Result));
        }
        return;
      }
    }

    void cancelled() override {
      Receiver(RequestResult<RangeInfo>::cancelled());
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("range info failed: " << Error);
      Receiver(RequestResult<RangeInfo>::fromError(Error));
    }
  };

  auto Consumer = std::make_shared<RangeInfoConsumer>(
    InputFile, Offset, Length, Lang, Invok, TryExistingAST,
    CancelOnSubsequentRequest, Receiver);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  const void *Once = CancelOnSubsequentRequest ? &OncePerASTToken : nullptr;
  Lang.getASTManager()->processASTAsync(Invok, std::move(Consumer), Once,
                                        llvm::vfs::getRealFileSystem());
}

void SwiftLangSupport::getCursorInfo(
    StringRef InputFile, unsigned Offset, unsigned Length, bool Actionables,
    bool SymbolGraph, bool CancelOnSubsequentRequest,
    ArrayRef<const char *> Args, Optional<VFSOptions> vfsOptions,
    std::function<void(const RequestResult<CursorInfoData> &)> Receiver) {

  std::string error;
  auto fileSystem = getFileSystem(vfsOptions, InputFile, error);
  if (!fileSystem)
    return Receiver(RequestResult<CursorInfoData>::fromError(error));

  if (auto IFaceGenRef = IFaceGenContexts.get(InputFile)) {
    IFaceGenRef->accessASTAsync([this, IFaceGenRef, Offset, Actionables,
                                 SymbolGraph, Receiver] {
      SwiftInterfaceGenContext::ResolvedEntity Entity;
      Entity = IFaceGenRef->resolveEntityForOffset(Offset);
      if (Entity.isResolved()) {
        CompilerInvocation Invok;
        IFaceGenRef->applyTo(Invok);
        if (Entity.Mod) {
          passCursorInfoForModule(Entity.Mod, IFaceGenContexts, Invok,
                                  Receiver);
        } else {
          std::string Diagnostic;  // Unused.
          ModuleDecl *MainModule = IFaceGenRef->getModuleDecl();
          ResolvedCursorInfo Info;
          Info.ValueD = const_cast<ValueDecl *>(Entity.Dcl);
          Info.IsRef = Entity.IsRef;
          passCursorInfoForDecl(Info, MainModule, Actionables, SymbolGraph, {},
                                *this, Invok, Diagnostic, {}, Receiver);
        }
      } else {
        CursorInfoData Info;
        Info.InternalDiagnostic =
            "Unable to resolve entity from generated interface.";
        Receiver(RequestResult<CursorInfoData>::fromResult(Info));
      }
    });
    return;
  }

  std::string Error;
  SwiftInvocationRef Invok =
      ASTMgr->getInvocation(Args, InputFile, fileSystem, Error);
  if (!Error.empty()) {
    LOG_WARN_FUNC("error creating ASTInvocation: " << Error);
  }
  if (!Invok) {
    Receiver(RequestResult<CursorInfoData>::fromError(Error));
    return;
  }

  resolveCursor(*this, InputFile, Offset, Length, Actionables, SymbolGraph,
                Invok, /*TryExistingAST=*/true, CancelOnSubsequentRequest,
                fileSystem, Receiver);
}

void SwiftLangSupport::
getRangeInfo(StringRef InputFile, unsigned Offset, unsigned Length,
             bool CancelOnSubsequentRequest, ArrayRef<const char *> Args,
             std::function<void(const RequestResult<RangeInfo> &)> Receiver) {
  if (IFaceGenContexts.get(InputFile)) {
    // FIXME: return range info for generated interfaces.
    Receiver(RequestResult<RangeInfo>::fromError(
        "Range info for generated interfaces is not implemented."));
    return;
  }
  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, InputFile, Error);
  if (!Invok) {
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver(RequestResult<RangeInfo>::fromError(Error));
    return;
  }
  if (Length == 0) {
    Receiver(RequestResult<RangeInfo>::fromError("Invalid range length."));
    return;
  }
  resolveRange(*this, InputFile, Offset, Length, Invok, /*TryExistingAST=*/true,
               CancelOnSubsequentRequest, Receiver);
}

void SwiftLangSupport::
getNameInfo(StringRef InputFile, unsigned Offset, NameTranslatingInfo &Input,
            ArrayRef<const char *> Args,
            std::function<void(const RequestResult<NameTranslatingInfo> &)> Receiver) {

  if (auto IFaceGenRef = IFaceGenContexts.get(InputFile)) {
    IFaceGenRef->accessASTAsync([IFaceGenRef, Offset, Input, Receiver] {
      SwiftInterfaceGenContext::ResolvedEntity Entity;
      Entity = IFaceGenRef->resolveEntityForOffset(Offset);
      if (Entity.isResolved()) {
        CompilerInvocation Invok;
        IFaceGenRef->applyTo(Invok);
        if (Entity.Mod) {
          // Module is ignored
        } else {
          // FIXME: Should pass the main module for the interface but currently
          // it's not necessary.
        }
      } else {
        NameTranslatingInfo Info;
        Info.InternalDiagnostic =
            "Unable to resolve entity from generated interface.";
        Receiver(RequestResult<NameTranslatingInfo>::fromResult(Info));
      }
    });
    return;
  }

  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, InputFile, Error);
  if (!Invok) {
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver(RequestResult<NameTranslatingInfo>::fromError(Error));
    return;
  }

  resolveName(*this, InputFile, Offset, Invok, /*TryExistingAST=*/true, Input,
              Receiver);
}

static void resolveCursorFromUSR(
    SwiftLangSupport &Lang, StringRef InputFile, StringRef USR,
    SwiftInvocationRef Invok, bool TryExistingAST,
    bool CancelOnSubsequentRequest,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fileSystem,
    std::function<void(const RequestResult<CursorInfoData> &)> Receiver) {
  assert(Invok);

  class CursorInfoConsumer : public SwiftASTConsumer {
    std::string InputFile;
    StringRef USR;
    SwiftLangSupport &Lang;
    SwiftInvocationRef ASTInvok;
    const bool TryExistingAST;
    bool CancelOnSubsequentRequest;
    std::function<void(const RequestResult<CursorInfoData> &)> Receiver;
    SmallVector<ImmutableTextSnapshotRef, 4> PreviousASTSnaps;

  public:
    CursorInfoConsumer(StringRef InputFile, StringRef USR,
                       SwiftLangSupport &Lang, SwiftInvocationRef ASTInvok,
                       bool TryExistingAST, bool CancelOnSubsequentRequest,
                       std::function<void(const RequestResult<CursorInfoData> &)> Receiver)
        : InputFile(InputFile), USR(USR), Lang(Lang),
          ASTInvok(std::move(ASTInvok)), TryExistingAST(TryExistingAST),
          CancelOnSubsequentRequest(CancelOnSubsequentRequest),
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

      if (USR.startswith("c:")) {
        LOG_WARN_FUNC("lookup for C/C++/ObjC USRs not implemented");
        CursorInfoData Info;
        Info.InternalDiagnostic = "Lookup for C/C++/ObjC USRs not implemented.";
        Receiver(RequestResult<CursorInfoData>::fromResult(Info));
        return;
      }

      auto &context = CompIns.getASTContext();
      TypeDecl *D = Demangle::getTypeDeclForUSR(context, USR);

      if (!D) {
        CursorInfoData Info;
        Info.InternalDiagnostic = "Unable to resolve type from USR.";
        Receiver(RequestResult<CursorInfoData>::fromResult(Info));
        return;
      }

      CompilerInvocation CompInvok;
      ASTInvok->applyTo(CompInvok);

      if (auto *M = dyn_cast<ModuleDecl>(D)) {
        passCursorInfoForModule(M, Lang.getIFaceGenContexts(), CompInvok,
                                Receiver);
      } else {
        ResolvedCursorInfo Info;
        Info.ValueD = D;
        Info.IsRef = false;

        auto *DC = D->getDeclContext();
        Type selfTy;
        if (DC->isTypeContext()) {
          Info.ContainerType = DC->getSelfInterfaceType();
          Info.ContainerType = D->getInnermostDeclContext()->mapTypeIntoContext(
              Info.ContainerType);
        }

        std::string Diagnostic;
        bool Success =
            passCursorInfoForDecl(Info, MainModule, /*AddRefactorings*/ false,
                                  /*AddSymbolGraph*/ false, {}, Lang, CompInvok,
                                  Diagnostic, PreviousASTSnaps, Receiver);
        if (!Success) {
          if (!PreviousASTSnaps.empty()) {
            // Attempt again using the up-to-date AST.
            resolveCursorFromUSR(
                Lang, InputFile, USR, ASTInvok,
                /*TryExistingAST=*/false, CancelOnSubsequentRequest,
                CompIns.getSourceMgr().getFileSystem(), Receiver);
          } else {
            CursorInfoData Info;
            Info.InternalDiagnostic = Diagnostic;
            Receiver(RequestResult<CursorInfoData>::fromResult(Info));
          }
        }
      }
    }

    void cancelled() override {
      Receiver(RequestResult<CursorInfoData>::cancelled());
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("cursor info failed: " << Error);
      Receiver(RequestResult<CursorInfoData>::fromError(Error));
    }
  };

  auto Consumer = std::make_shared<CursorInfoConsumer>(
      InputFile, USR, Lang, Invok, TryExistingAST, CancelOnSubsequentRequest,
      Receiver);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  const void *Once = CancelOnSubsequentRequest ? &OncePerASTToken : nullptr;
  Lang.getASTManager()->processASTAsync(Invok, std::move(Consumer), Once,
                                        fileSystem);
}

void SwiftLangSupport::getCursorInfoFromUSR(
    StringRef filename, StringRef USR, bool CancelOnSubsequentRequest,
    ArrayRef<const char *> Args, Optional<VFSOptions> vfsOptions,
    std::function<void(const RequestResult<CursorInfoData> &)> Receiver) {
    std::string error;

    auto fileSystem = getFileSystem(vfsOptions, filename, error);
    if (!fileSystem)
      return Receiver(RequestResult<CursorInfoData>::fromError(error));

    if (auto IFaceGenRef = IFaceGenContexts.get(filename)) {
      LOG_WARN_FUNC(
          "Info from usr for generated interface not implemented yet.");
      CursorInfoData Info;
      Info.InternalDiagnostic =
          "Info for generated interfaces not implemented.";
      Receiver(RequestResult<CursorInfoData>::fromResult(Info));
      return;
  }

  std::string Error;
  SwiftInvocationRef Invok =
      ASTMgr->getInvocation(Args, filename, fileSystem, Error);
  if (!Invok) {
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver(RequestResult<CursorInfoData>::fromError(Error));
    return;
  }

  resolveCursorFromUSR(*this, filename, USR, Invok, /*TryExistingAST=*/true,
                       CancelOnSubsequentRequest, fileSystem, Receiver);
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
    : Ranges(Ranges), SourceMgr(SrcFile.getASTContext().SourceMgr),
      BufferID(BufferID) {
    if (auto *V = dyn_cast<VarDecl>(D)) {
      // Always use the canonical var decl for comparison. This is so we
      // pick up all occurrences of x in case statements like the below:
      //   case .first(let x), .second(let x)
      //     fallthrough
      //   case .third(let x)
      //     print(x)
      Dcl = V->getCanonicalVarDecl();

      // If we have a prioperty wrapper backing property or projected value, use
      // the wrapped property instead (i.e. if this is _foo or $foo, pretend
      // it's foo).
      if (auto *Wrapped = V->getOriginalWrappedProperty()) {
        Dcl = Wrapped;
      }
    } else {
      Dcl = D;
    }
  }

private:
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    if (Cancelled)
      return false;
    if (auto *V = dyn_cast<VarDecl>(D)) {
      // Handle references to the implicitly generated vars in case statements
      // matching multiple patterns
      D = V->getCanonicalVarDecl();
    }
    if (D == Dcl)
      return passId(Range);
    return true;
  }

  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                          ReferenceMetaData Data) override {
    if (Cancelled)
      return false;

    if (auto *V = dyn_cast<VarDecl>(D)) {
      D = V->getCanonicalVarDecl();

      // If we have a prioperty wrapper backing property or projected value, use
      // the wrapped property for comparison instead (i.e. if this is _foo or
      // $foo, pretend it's foo).
      if (auto *Wrapped = V->getOriginalWrappedProperty()) {
        assert(Range.getByteLength() > 1 &&
               (Range.str().front() == '_' || Range.str().front() == '$'));
        D = Wrapped;
        Range = CharSourceRange(Range.getStart().getAdvancedLoc(1), Range.getByteLength() - 1);
      }
    } else if (CtorTyRef) {
      D = CtorTyRef;
    }

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

} // end anonymous namespace

void SwiftLangSupport::findRelatedIdentifiersInFile(
    StringRef InputFile, unsigned Offset,
    bool CancelOnSubsequentRequest,
    ArrayRef<const char *> Args,
    std::function<void(const RequestResult<RelatedIdentsInfo> &)> Receiver) {

  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, InputFile, Error);
  if (!Invok) {
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver(RequestResult<RelatedIdentsInfo>::fromError(Error));
    return;
  }

  class RelatedIdConsumer : public SwiftASTConsumer {
    unsigned Offset;
    std::function<void(const RequestResult<RelatedIdentsInfo> &)> Receiver;
    SwiftInvocationRef Invok;

  public:
    RelatedIdConsumer(unsigned Offset,
                      std::function<void(const RequestResult<RelatedIdentsInfo> &)> Receiver,
                      SwiftInvocationRef Invok)
      : Offset(Offset), Receiver(std::move(Receiver)), Invok(Invok) { }

    // FIXME: Don't silently eat errors here.
    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompInst = AstUnit->getCompilerInstance();
      auto &SrcFile = AstUnit->getPrimarySourceFile();


      SmallVector<std::pair<unsigned, unsigned>, 8> Ranges;

      auto Action = [&]() {
        unsigned BufferID = SrcFile.getBufferID().getValue();
        SourceLoc Loc =
          Lexer::getLocForStartOfToken(CompInst.getSourceMgr(), BufferID, Offset);
        if (Loc.isInvalid())
          return;

        ResolvedCursorInfo CursorInfo =
          evaluateOrDefault(SrcFile.getASTContext().evaluator,
                            CursorInfoRequest{CursorInfoOwner(&SrcFile, Loc)},
                            ResolvedCursorInfo());
        if (CursorInfo.isInvalid())
          return;
        if (CursorInfo.IsKeywordArgument)
          return;

        ValueDecl *VD = CursorInfo.typeOrValue();
        if (!VD)
          return; // This was a module reference.

        // Only accept pointing to an identifier.
        if (!CursorInfo.IsRef &&
            (isa<ConstructorDecl>(VD) ||
             isa<DestructorDecl>(VD) ||
             isa<SubscriptDecl>(VD)))
          return;
        if (VD->isOperator())
          return;

        RelatedIdScanner Scanner(SrcFile, BufferID, VD, Ranges);

        if (auto *Case = getCaseStmtOfCanonicalVar(VD)) {
          Scanner.walk(Case);
          while ((Case = Case->getFallthroughDest().getPtrOrNull())) {
            Scanner.walk(Case);
          }
        } else if (DeclContext *LocalDC = VD->getDeclContext()->getLocalContext()) {
          Scanner.walk(LocalDC);
        } else {
          Scanner.walk(SrcFile);
        }
      };
      Action();

      RelatedIdentsInfo Info;
      Info.Ranges = Ranges;
      Receiver(RequestResult<RelatedIdentsInfo>::fromResult(Info));
    }

    void cancelled() override {
      Receiver(RequestResult<RelatedIdentsInfo>::cancelled());
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("related idents failed: " << Error);
      Receiver(RequestResult<RelatedIdentsInfo>::fromError(Error));
    }

    static CaseStmt *getCaseStmtOfCanonicalVar(Decl *D) {
      assert(D);
      if (auto *VD = dyn_cast<VarDecl>(D)) {
        if (auto *Canonical = VD->getCanonicalVarDecl()) {
          return dyn_cast_or_null<CaseStmt>(Canonical->getRecursiveParentPatternStmt());
        }
      }
      return nullptr;
    }
  };

  auto Consumer = std::make_shared<RelatedIdConsumer>(Offset, Receiver, Invok);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  const void *Once = CancelOnSubsequentRequest ? &OncePerASTToken : nullptr;
  ASTMgr->processASTAsync(Invok, std::move(Consumer), Once,
                          llvm::vfs::getRealFileSystem());
}

//===----------------------------------------------------------------------===//
// SwiftLangSupport::semanticRefactoring
//===----------------------------------------------------------------------===//

static RefactoringKind getIDERefactoringKind(SemanticRefactoringInfo Info) {
  switch(Info.Kind) {
    case SemanticRefactoringKind::None: return RefactoringKind::None;
#define SEMANTIC_REFACTORING(KIND, NAME, ID)                                   \
    case SemanticRefactoringKind::KIND: return RefactoringKind::KIND;
#include "swift/IDE/RefactoringKinds.def"
  }
}

void SwiftLangSupport::
semanticRefactoring(StringRef Filename, SemanticRefactoringInfo Info,
                    ArrayRef<const char*> Args,
                    CategorizedEditsReceiver Receiver) {
  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, Filename, Error);
  if (!Invok) {
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver(RequestResult<ArrayRef<CategorizedEdits>>::fromError(Error));
    return;
  }
  assert(Invok);

  class SemaRefactoringConsumer : public SwiftASTConsumer {
    SemanticRefactoringInfo Info;
    CategorizedEditsReceiver Receiver;

  public:
    SemaRefactoringConsumer(SemanticRefactoringInfo Info,
                            CategorizedEditsReceiver Receiver) : Info(Info),
                                                Receiver(std::move(Receiver)) {}

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompIns = AstUnit->getCompilerInstance();
      ModuleDecl *MainModule = CompIns.getMainModule();
      RefactoringOptions Opts(getIDERefactoringKind(Info));
      Opts.Range.BufferId =  AstUnit->getPrimarySourceFile().getBufferID().
        getValue();
      Opts.Range.Line = Info.Line;
      Opts.Range.Column = Info.Column;
      Opts.Range.Length = Info.Length;
      Opts.PreferredName = Info.PreferredName.str();

      RequestRefactoringEditConsumer EditConsumer(Receiver);
      refactorSwiftModule(MainModule, Opts, EditConsumer, EditConsumer);
    }

    void cancelled() override {
      Receiver(RequestResult<ArrayRef<CategorizedEdits>>::cancelled());
    }

    void failed(StringRef Error) override {
      Receiver(RequestResult<ArrayRef<CategorizedEdits>>::fromError(Error));
    }
  };

  auto Consumer = std::make_shared<SemaRefactoringConsumer>(Info, Receiver);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  getASTManager()->processASTAsync(Invok, std::move(Consumer), &OncePerASTToken,
                                   llvm::vfs::getRealFileSystem());
}

void SwiftLangSupport::collectExpressionTypes(StringRef FileName,
                                              ArrayRef<const char *> Args,
                                    ArrayRef<const char *> ExpectedProtocols,
                                              bool CanonicalType,
                  std::function<void(const RequestResult<ExpressionTypesInFile> &)> Receiver) {
  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, FileName, Error);
  if (!Invok) {
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver(RequestResult<ExpressionTypesInFile>::fromError(Error));
    return;
  }
  assert(Invok);
  class ExpressionTypeCollector: public SwiftASTConsumer {
    std::function<void(const RequestResult<ExpressionTypesInFile> &)> Receiver;
    std::vector<const char *> ExpectedProtocols;
    bool CanonicalType;
  public:
    ExpressionTypeCollector(
        std::function<void(const RequestResult<ExpressionTypesInFile> &)> Receiver,
        ArrayRef<const char *> ExpectedProtocols,
        bool CanonicalType):
          Receiver(std::move(Receiver)),
          ExpectedProtocols(ExpectedProtocols.vec()),
          CanonicalType(CanonicalType) {}
    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto *SF = AstUnit->getCompilerInstance().getPrimarySourceFile();
      std::vector<ExpressionTypeInfo> Scratch;
      llvm::SmallString<256> TypeBuffer;
      llvm::raw_svector_ostream OS(TypeBuffer);
      ExpressionTypesInFile Result;
      for (auto Item: collectExpressionType(*SF, ExpectedProtocols, Scratch,
                                            CanonicalType, OS)) {
        Result.Results.push_back({Item.offset, Item.length, Item.typeOffset, {}});
        for (auto P: Item.protocols) {
          Result.Results.back().ProtocolOffsets.push_back(P.first);
        }
      }
      Result.TypeBuffer = OS.str();
      Receiver(RequestResult<ExpressionTypesInFile>::fromResult(Result));
    }

    void cancelled() override {
      Receiver(RequestResult<ExpressionTypesInFile>::cancelled());
    }

    void failed(StringRef Error) override {
      Receiver(RequestResult<ExpressionTypesInFile>::fromError(Error));
    }
  };
  auto Collector = std::make_shared<ExpressionTypeCollector>(Receiver,
                                                             ExpectedProtocols,
                                                             CanonicalType);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  getASTManager()->processASTAsync(Invok, std::move(Collector),
                                   &OncePerASTToken,
                                   llvm::vfs::getRealFileSystem());
}
