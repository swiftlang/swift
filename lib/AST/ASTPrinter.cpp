//===--- ASTPrinter.cpp - Swift Language AST Printer---------------------===//
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
//  This file implements printing for the Swift ASTs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Strings.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>

using namespace swift;

void ASTPrinter::anchor() {}

void ASTPrinter::printIndent() {
  llvm::SmallString<16> Str;
  for (unsigned i = 0; i != CurrentIndentation; ++i)
    Str += ' ';
  
  printText(Str);
}

void ASTPrinter::printTextImpl(StringRef Text) {
  if (PendingNewlines != 0) {
    llvm::SmallString<16> Str;
    for (unsigned i = 0; i != PendingNewlines; ++i)
      Str += '\n';
    PendingNewlines = 0;

    printText(Str);
    printIndent();
  }
  
  const Decl *PreD = PendingDeclPreCallback;
  const Decl *LocD = PendingDeclLocCallback;
  PendingDeclPreCallback = nullptr;
  PendingDeclLocCallback = nullptr;
  
  if (PreD) {
    printDeclPre(PreD);
  }
  if (LocD) {
    printDeclLoc(LocD);
  }

  printText(Text);
}

void ASTPrinter::printModuleRef(ModuleEntity Mod, Identifier Name) {
  printName(Name);
}

ASTPrinter &ASTPrinter::operator<<(unsigned long long N) {
  llvm::SmallString<32> Str;
  llvm::raw_svector_ostream OS(Str);
  OS << N;
  printTextImpl(OS.str());
  return *this;
}

ASTPrinter &ASTPrinter::operator<<(UUID UU) {
  llvm::SmallString<UUID::StringBufferSize> Str;
  UU.toString(Str);
  printTextImpl(Str);
  return *this;
}

void ASTPrinter::printName(Identifier Name) {
  if (Name.empty()) {
    *this << "_";
    return;
  }
  bool IsKeyword = llvm::StringSwitch<bool>(Name.str())
#define KEYWORD(KW) \
      .Case(#KW, true)
#include "swift/Parse/Tokens.def"
      .Default(false);
  if (IsKeyword)
    *this << "`";
  *this << Name.str();
  if (IsKeyword)
    *this << "`";
}

void StreamPrinter::printText(StringRef Text) {
  OS << Text;
}

namespace {
/// \brief AST pretty-printer.
class PrintAST : public ASTVisitor<PrintAST> {
  ASTPrinter &Printer;
  PrintOptions Options;
  unsigned IndentLevel = 0;

  friend DeclVisitor<PrintAST>;

  /// \brief RAII object that increases the indentation level.
  class IndentRAII {
    PrintAST &Self;
    bool DoIndent;

  public:
    IndentRAII(PrintAST &self, bool DoIndent = true)
        : Self(self), DoIndent(DoIndent) {
      if (DoIndent)
        Self.IndentLevel += Self.Options.Indent;
    }

    ~IndentRAII() {
      if (DoIndent)
        Self.IndentLevel -= Self.Options.Indent;
    }
  };

  /// \brief Indent the current number of indentation spaces.
  void indent() {
    Printer.setIndent(IndentLevel);
  }

  /// \brief Record the location of this declaration, which is about to
  /// be printed.
  template<typename FnTy>
  void recordDeclLoc(Decl *decl, const FnTy &Fn) {
    Printer.callPrintDeclLoc(decl);
    Fn();
    Printer.printDeclNameEndLoc(decl);
  }

  void printClangDocumentationComment(const clang::Decl *D) {
    const auto &ClangContext = D->getASTContext();
    const clang::RawComment *RC = ClangContext.getRawCommentForAnyRedecl(D);
    if (!RC)
      return;

    if (!Options.PrintRegularClangComments) {
      Printer.printNewline();
      indent();
    }

    bool Invalid;
    unsigned StartLocCol =
        ClangContext.getSourceManager().getSpellingColumnNumber(
            RC->getLocStart(), &Invalid);
    if (Invalid)
      StartLocCol = 0;

    unsigned WhitespaceToTrim = StartLocCol ? StartLocCol - 1 : 0;

    SmallVector<StringRef, 8> Lines;

    StringRef RawText =
        RC->getRawText(ClangContext.getSourceManager()).rtrim("\n\r");
    trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);

    for (auto Line : Lines) {
      Printer << Line;
      Printer.printNewline();
    }
  }

  void printSwiftDocumentationComment(const Decl *D) {
    auto RC = D->getRawComment();
    if (RC.isEmpty())
      return;

    Printer.printNewline();
    indent();

    SmallVector<StringRef, 8> Lines;
    for (const auto &SRC : RC.Comments) {
      Lines.clear();

      StringRef RawText = SRC.RawText.rtrim("\n\r");
      unsigned WhitespaceToTrim = SRC.StartColumn - 1;
      trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);

      for (auto Line : Lines) {
        Printer << Line;
        Printer.printNewline();
      }
    }
  }

  void printDocumentationComment(Decl *D) {
    if (!Options.PrintDocumentationComments)
      return;

    // Try to print a comment from Clang.
    auto MaybeClangNode = D->getClangNode();
    if (MaybeClangNode) {
      if (auto *CD = MaybeClangNode.getAsDecl())
        printClangDocumentationComment(CD);
      return;
    }

    printSwiftDocumentationComment(D);
  }

  void printStaticKeyword(StaticSpellingKind StaticSpelling) {
    switch (StaticSpelling) {
    case StaticSpellingKind::None:
      llvm_unreachable("should not be called for non-static decls");
    case StaticSpellingKind::KeywordStatic:
      Printer << "static ";
      break;
    case StaticSpellingKind::KeywordClass:
      Printer<< "class ";
      break;
    }
  }

  void printAccessibility(Accessibility access, StringRef suffix = "") {
    Printer << "/*";
    switch (access) {
    case Accessibility::Private:
      Printer << "private";
      break;
    case Accessibility::Internal:
      Printer << "internal";
      break;
    case Accessibility::Public:
      Printer << "public";
      break;
    }
    Printer << suffix << "*/ ";
  }

  void printAccessibility(const ValueDecl *D) {
    if (!Options.PrintAccessibility || !D->hasAccessibility() ||
        D->getAttrs().hasAttribute<AccessibilityAttr>())
      return;

    printAccessibility(D->getFormalAccess());

    if (auto storageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      if (auto setter = storageDecl->getSetter()) {
        Accessibility setterAccess = setter->getFormalAccess();
        if (setterAccess != D->getFormalAccess())
          printAccessibility(setterAccess, "(set)");
      }
    }
  }

  void printTypeLoc(const TypeLoc &TL) {
    // Print a TypeRepr if instructed to do so by options, or if the type
    // is null.
    if ((Options.PreferTypeRepr && TL.hasLocation()) ||
        TL.getType().isNull()) {
      TL.getTypeRepr()->print(Printer, Options);
      return;
    }
    TL.getType().print(Printer, Options);
  }

  void printAttributes(const Decl *D);
  void printTypedPattern(const TypedPattern *TP,
                         bool StripOuterSliceType = false);

public:
  void printPattern(const Pattern *pattern);

  void printGenericParams(GenericParamList *params);

private:
  bool shouldPrint(const Decl *D);
  void printAccessors(AbstractStorageDecl *ASD);
  void printMembers(DeclRange members, bool needComma = false);
  void printNominalDeclName(NominalTypeDecl *decl);
  void printInherited(const Decl *decl,
                      ArrayRef<TypeLoc> inherited,
                      ArrayRef<ProtocolDecl *> protos,
                      Type superclass = {},
                      bool explicitClass = false,
                      bool PrintAsProtocolComposition = false);

  void printInherited(const TypeDecl *decl, bool explicitClass = false);
  void printInherited(const EnumDecl *D);
  void printInherited(const ExtensionDecl *decl);
  void printInherited(const GenericTypeParamDecl *D);

  void printEnumElement(EnumElementDecl *elt);

  /// \returns true if anything was printed.
  bool printASTNodes(const ArrayRef<ASTNode> &Elements, bool NeedIndent = true);

  void printOneParameter(const Pattern *BodyPattern,
                         bool ArgNameIsAPIByDefault,
                         bool StripOuterSliceType,
                         bool Curried);

  /// \brief Print the function parameters in curried or selector style,
  /// to match the original function declaration.
  void printFunctionParameters(AbstractFunctionDecl *AFD);

#define DECL(Name,Parent) void visit##Name##Decl(Name##Decl *decl);
#define ABSTRACT_DECL(Name, Parent)
#define DECL_RANGE(Name,Start,End)
#include "swift/AST/DeclNodes.def"

#define STMT(Name, Parent) void visit##Name##Stmt(Name##Stmt *stmt);
#include "swift/AST/StmtNodes.def"

public:
  PrintAST(ASTPrinter &Printer, const PrintOptions &Options)
      : Printer(Printer), Options(Options) {}

  using ASTVisitor::visit;

  bool visit(Decl *D) {
    if (!shouldPrint(D))
      return false;

    Printer.callPrintDeclPre(D);
    ASTVisitor::visit(D);
    Printer.printDeclPost(D);
    return true;
  }
};
} // unnamed namespace

void PrintAST::printAttributes(const Decl *D) {
  if (Options.SkipAttributes)
    return;
  D->getAttrs().print(Printer, Options);
}

void PrintAST::printTypedPattern(const TypedPattern *TP,
                                 bool StripOuterSliceType) {
  auto TheTypeLoc = TP->getTypeLoc();
  if (TheTypeLoc.hasLocation()) {
    // If the outer typeloc is an InOutTypeRepr, print the inout before the
    // subpattern.
    if (auto *IOT = dyn_cast<InOutTypeRepr>(TheTypeLoc.getTypeRepr())) {
      TheTypeLoc = TypeLoc(IOT->getBase());
      Type T = TheTypeLoc.getType();
      if (T) {
        if (auto *IOT = T->getAs<InOutType>()) {
          T = IOT->getObjectType();
          TheTypeLoc.setType(T);
        }
      }

      Printer << "inout ";
    }

    printPattern(TP->getSubPattern());
    Printer << ": ";
    if (StripOuterSliceType) {
      Type T = TP->getType();
      if (auto *BGT = T->getAs<BoundGenericType>()) {
        BGT->getGenericArgs()[0].print(Printer, Options);
        return;
      }
    }
    printTypeLoc(TheTypeLoc);
    return;
  }

  Type T = TP->getType();
  if (auto *IOT = T->getAs<InOutType>()) {
    T = IOT->getObjectType();
    Printer << "inout ";
  }

  printPattern(TP->getSubPattern());
  Printer << ": ";
  if (StripOuterSliceType) {
    if (auto *BGT = T->getAs<BoundGenericType>()) {
      BGT->getGenericArgs()[0].print(Printer, Options);
      return;
    }
  }
  T.print(Printer, Options);
}

void PrintAST::printPattern(const Pattern *pattern) {
  switch (pattern->getKind()) {
  case PatternKind::Any:
    Printer << "_";
    break;

  case PatternKind::Named: {
    auto named = cast<NamedPattern>(pattern);
    recordDeclLoc(named->getDecl(),
      [&]{
        Printer.printName(named->getBodyName());
      });
    break;
  }

  case PatternKind::Paren:
    Printer << "(";
    printPattern(cast<ParenPattern>(pattern)->getSubPattern());
    Printer << ")";
    break;

  case PatternKind::Tuple: {
    Printer << "(";
    auto TP = cast<TuplePattern>(pattern);
    auto Fields = TP->getFields();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      const auto &Elt = Fields[i];
      if (i != 0)
        Printer << ", ";

      if (i == e - 1 && TP->hasVararg()) {
        printTypedPattern(cast<TypedPattern>(Elt.getPattern()),
                          /*StripOuterSliceType=*/true);
      } else {
        printPattern(Elt.getPattern());
      }
      if (Elt.getDefaultArgKind() != DefaultArgumentKind::None) {
        if (Options.PrintDefaultParameterPlaceholder)
          Printer << " = default";
        else if (Options.VarInitializers) {
          // FIXME: Print initializer here.
        }
      }
    }
    if (TP->hasVararg())
      Printer << "...";
    Printer << ")";
    break;
  }

  case PatternKind::Typed:
    printTypedPattern(cast<TypedPattern>(pattern));
    break;

  case PatternKind::Is: {
    auto isa = cast<IsPattern>(pattern);
    Printer << "is ";
    isa->getCastTypeLoc().getType().print(Printer, Options);
    break;
  }

  case PatternKind::NominalType: {
    auto type = cast<NominalTypePattern>(pattern);
    type->getCastTypeLoc().getType().print(Printer, Options);
    Printer << "(";
    interleave(type->getElements().begin(), type->getElements().end(),
               [&](const NominalTypePattern::Element &elt) {
                 Printer << elt.getPropertyName().str() << ":";
                 printPattern(elt.getSubPattern());
               }, [&] {
                 Printer << ", ";
               });
    break;
  }

  case PatternKind::EnumElement: {
    auto elt = cast<EnumElementPattern>(pattern);
    // FIXME: Print element expr.
    if (elt->hasSubPattern())
      printPattern(elt->getSubPattern());
    break;
  }

  case PatternKind::OptionalSome:
    printPattern(cast<OptionalSomePattern>(pattern)->getSubPattern());
    Printer << '?';
    break;

  case PatternKind::Expr:
    // FIXME: Print expr.
    break;

  case PatternKind::Var:
    if (!Options.SkipIntroducerKeywords)
      Printer << (cast<VarPattern>(pattern)->isLet() ? "let " : "var ");
    printPattern(cast<VarPattern>(pattern)->getSubPattern());
  }
}

void PrintAST::printGenericParams(GenericParamList *Params) {
  if (!Params)
    return;

  Printer << "<";
  bool IsFirst = true;
  for (auto GP : Params->getParams()) {
    if (IsFirst) {
      IsFirst = false;
    } else {
      Printer << ", ";
    }

    Printer.printName(GP->getName());
    printInherited(GP);
  }

  auto Requirements = Params->getRequirements();
  if (!Requirements.empty()) {
    bool IsFirst = true;
    for (auto &Req : Requirements) {
      if (Req.isInvalid() ||
          Req.getKind() == RequirementKind::WitnessMarker)
        continue;

      if (IsFirst) {
        Printer << " where ";
        IsFirst = false;
      } else {
        Printer << ", ";
      }

      switch (Req.getKind()) {
      case RequirementKind::Conformance:
        printTypeLoc(Req.getSubjectLoc());
        Printer << " : ";
        printTypeLoc(Req.getConstraintLoc());
        break;
      case RequirementKind::SameType:
        printTypeLoc(Req.getFirstTypeLoc());
        Printer << " == ";
        printTypeLoc(Req.getSecondTypeLoc());
        break;
      case RequirementKind::WitnessMarker:
        llvm_unreachable("Handled above");
      }
    }
  }
  Printer << ">";
}

bool PrintAST::shouldPrint(const Decl *D) {
  if (Options.SkipDeinit && isa<DestructorDecl>(D)) {
    return false;
  }

  if (Options.SkipImplicit && D->isImplicit())
    return false;

  if (Options.SkipUnavailable &&
      D->getAttrs().isUnavailable(D->getASTContext()))
    return false;

  // Skip declarations that are not accessible.
  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    if (Options.AccessibilityFilter > Accessibility::Private &&
        VD->hasAccessibility() &&
        VD->getFormalAccess() < Options.AccessibilityFilter)
      return false;
  }

  if (Options.SkipPrivateStdlibDecls && D->isPrivateStdlibDecl())
      return false;

  if (Options.SkipEmptyExtensionDecls && isa<ExtensionDecl>(D)) {
    auto Ext = cast<ExtensionDecl>(D);
    // If the extension doesn't add protocols or has no members that we should
    // print then skip printing it.
    if (Ext->getProtocols().empty()) {
      bool HasMemberToPrint = false;
      for (auto Member : Ext->getMembers()) {
        if (shouldPrint(Member)) {
          HasMemberToPrint = true;
          break;
        }
      }
      if (!HasMemberToPrint)
        return false;
    }
  }
  return true;
}

static bool isAccessorAssumedNonMutating(FuncDecl *accessor) {
  switch (accessor->getAccessorKind()) {
  case AccessorKind::IsGetter:
  case AccessorKind::IsAddressor:
    return true;

  case AccessorKind::IsSetter:
  case AccessorKind::IsWillSet:
  case AccessorKind::IsDidSet:
  case AccessorKind::IsMaterializeForSet:
  case AccessorKind::IsMutableAddressor:
    return false;

  case AccessorKind::NotAccessor:
    llvm_unreachable("not an addressor!");
  }
  llvm_unreachable("bad addressor kind");
}

static StringRef getAddressorLabel(FuncDecl *addressor) {
  switch (addressor->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("addressor claims not to be an addressor");
  case AddressorKind::Unsafe:
    return "unsafeAddress";
  case AddressorKind::Owning:
    return "addressWithOwner";
  case AddressorKind::NativeOwning:
    return "addressWithNativeOwner";
  case AddressorKind::NativePinning:
    return "addressWithPinnedNativeOwner";
  }
  llvm_unreachable("bad addressor kind");
}

static StringRef getMutableAddressorLabel(FuncDecl *addressor) {
  switch (addressor->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("addressor claims not to be an addressor");
  case AddressorKind::Unsafe:
    return "unsafeMutableAddress";
  case AddressorKind::Owning:
    return "mutableAddressWithOwner";
  case AddressorKind::NativeOwning:
    return "mutableAddressWithNativeOwner";
  case AddressorKind::NativePinning:
    return "mutableAddressWithPinnedNativeOwner";
  }
  llvm_unreachable("bad addressor kind");
}

/// Is the getter-like accessor for this storage explicitly mutating?
static bool hasMutatingGetter(AbstractStorageDecl *storage) {
  switch (storage->getStorageKind()) {
  case AbstractStorageDecl::Stored:
    return false;

  case AbstractStorageDecl::StoredWithTrivialAccessors:
  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
  case AbstractStorageDecl::Computed:
    assert(storage->getGetter());
    return storage->getGetter()->isMutating();

  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
    assert(storage->getAddressor());
    return storage->getAddressor()->isMutating();
  }
  llvm_unreachable("bad storage kind");
}

/// Is the setter-like accessor for this storage explicitly nonmutating?
static bool hasExplicitNonMutatingSetter(AbstractStorageDecl *storage) {
  assert(storage->isSettable(nullptr));
  switch (storage->getStorageKind()) {
  case AbstractStorageDecl::Stored:
    return false;

  case AbstractStorageDecl::StoredWithTrivialAccessors:
  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::Computed:
    assert(storage->getSetter());
    return storage->getSetter()->isExplicitNonMutating();

  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
    assert(storage->getMutableAddressor());
    return storage->getMutableAddressor()->isExplicitNonMutating();
  }
  llvm_unreachable("bad storage kind");
}

void PrintAST::printAccessors(AbstractStorageDecl *ASD) {
  auto storageKind = ASD->getStorageKind();

  // Never print anything for stored properties.
  if (storageKind == AbstractStorageDecl::Stored)
    return;

  // Treat StoredWithTrivialAccessors the same as Stored unless
  // we're printing for SIL, in which case we want to distinguish it
  // from a pure stored property.
  if (storageKind == AbstractStorageDecl::StoredWithTrivialAccessors) {
    if (!Options.PrintForSIL) return;

    // Don't print an accessor for a let; the parser can't handle it.
    if (isa<VarDecl>(ASD) && cast<VarDecl>(ASD)->isLet())
      return;
  }

  // We sometimes want to print the accessors abstractly
  // instead of listing out how they're actually implemented.
  bool inProtocol = isa<ProtocolDecl>(ASD->getDeclContext());
  if (inProtocol ||
      (Options.AbstractAccessors && !Options.FunctionDefinitions)) {
    bool mutatingGetter = hasMutatingGetter(ASD);

    bool settable = ASD->isSettable(nullptr);
    bool nonmutatingSetter =
      (settable ? hasExplicitNonMutatingSetter(ASD) : false);

    // We're about to print something like this:
    //   { mutating? get (nonmutating? set)? }
    // But don't print "{ get set }" if we don't have to.
    if (!inProtocol && !Options.PrintGetSetOnRWProperties &&
        settable && !mutatingGetter && !nonmutatingSetter) {
      return;
    }

    Printer << " {";
    if (mutatingGetter) Printer << " mutating";
    Printer << " get";
    if (settable) {
      if (nonmutatingSetter) Printer << " nonmutating";
      Printer << " set";
    }
    Printer << " }";
    return;
  }

  // Honor !Options.PrintGetSetOnRWProperties in the only remaining
  // case where we could end up printing { get set }.
  if (storageKind == AbstractStorageDecl::StoredWithTrivialAccessors ||
      storageKind == AbstractStorageDecl::Computed) {
    if (!Options.PrintGetSetOnRWProperties &&
        !Options.FunctionDefinitions &&
        ASD->getSetter() &&
        !ASD->getGetter()->isMutating() &&
        !ASD->getSetter()->isExplicitNonMutating()) {
      return;
    }
  }

  // Otherwise, print all the concrete defining accessors.

  bool PrintAccessorBody = Options.FunctionDefinitions;

  auto PrintAccessor = [&](FuncDecl *Accessor, StringRef Label) {
    if (!Accessor)
      return;
    if (!PrintAccessorBody) {
      if (isAccessorAssumedNonMutating(Accessor)) {
        if (Accessor->isMutating())
          Printer << " mutating";
      } else {
        if (Accessor->isExplicitNonMutating()) {
          Printer << " nonmutating";
        }
      }
      Printer << " " << Label;
    } else {
      Printer.printNewline();
      IndentRAII IndentMore(*this);
      indent();
      visit(Accessor);
    }
  };

  auto PrintAddressor = [&](FuncDecl *accessor) {
    if (!accessor) return;
    PrintAccessor(accessor, getAddressorLabel(accessor));
  };

  auto PrintMutableAddressor = [&](FuncDecl *accessor) {
    if (!accessor) return;
    PrintAccessor(accessor, getMutableAddressorLabel(accessor));
  };

  Printer << " {";
  switch (storageKind) {
  case AbstractStorageDecl::Stored:
    llvm_unreachable("filtered out above!");
    
  case AbstractStorageDecl::StoredWithTrivialAccessors:
  case AbstractStorageDecl::Computed:
    PrintAccessor(ASD->getGetter(), "get");
    PrintAccessor(ASD->getSetter(), "set");
    break;

  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
    PrintAccessor(ASD->getWillSetFunc(), "willSet");
    PrintAccessor(ASD->getDidSetFunc(), "didSet");
    break;

  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
    PrintAddressor(ASD->getAddressor());
    PrintMutableAddressor(ASD->getMutableAddressor());
    if (ASD->hasObservers()) {
      PrintAccessor(ASD->getWillSetFunc(), "willSet");
      PrintAccessor(ASD->getDidSetFunc(), "didSet");
    }
    break;

  case AbstractStorageDecl::ComputedWithMutableAddress:
    PrintAccessor(ASD->getGetter(), "get");
    PrintMutableAddressor(ASD->getMutableAddressor());
    break;
  }
  if (PrintAccessorBody) {
    Printer.printNewline();
    indent();
  } else
    Printer << " ";
  Printer << "}";
}

void PrintAST::printMembers(DeclRange members, bool needComma) {
  Printer << " {";
  Printer.printNewline();
  {
    IndentRAII indentMore(*this);
    for (auto i = members.begin(), iEnd = members.end(); i != iEnd; ++i) {
      auto member = *i;

      if (!shouldPrint(member))
        continue;

      if (!member->shouldPrintInContext(Options))
        continue;

      indent();
      visit(member);
      if (needComma && std::next(i) != iEnd)
        Printer << ",";
      Printer.printNewline();
    }
  }
  indent();
  Printer << "}";
}

void PrintAST::printNominalDeclName(NominalTypeDecl *decl) {
  Printer.printName(decl->getName());
  if (auto gp = decl->getGenericParams()) {
    if (!isa<ProtocolDecl>(decl))
      printGenericParams(gp);
  }
}

void PrintAST::printInherited(const Decl *decl,
                              ArrayRef<TypeLoc> inherited,
                              ArrayRef<ProtocolDecl *> protos,
                              Type superclass,
                              bool explicitClass,
                              bool PrintAsProtocolComposition) {
  if (inherited.empty() && superclass.isNull() && !explicitClass) {
    if (protos.empty())
      return;
    // If only conforms to AnyObject protocol, nothing to print.
    if (protos.size() == 1) {
      if (protos.front()->isSpecificProtocol(KnownProtocolKind::AnyObject))
        return;
    }
  }

  if (inherited.empty()) {
    bool PrintedColon = false;
    bool PrintedInherited = false;

    if (explicitClass) {
      Printer << " : class";
      PrintedInherited = true;
    } else if (superclass) {
      bool ShouldPrintSuper = true;
      if (auto NTD = superclass->getAnyNominal()) {
        ShouldPrintSuper = shouldPrint(NTD);
      }
      if (ShouldPrintSuper) {
        Printer << " : ";
        superclass.print(Printer, Options);
        PrintedInherited = true;
      }
    }

    bool UseProtocolCompositionSyntax =
        PrintAsProtocolComposition && protos.size() > 1;
    if (UseProtocolCompositionSyntax) {
      Printer << " : protocol<";
      PrintedColon = true;
    }
    for (auto Proto : protos) {
      if (!shouldPrint(Proto))
        continue;
      if (Proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
        continue;
      if (auto Enum = dyn_cast<EnumDecl>(decl)) {
        // Conformance to RawRepresentable is implied by having a raw type.
        if (Enum->hasRawType()
            && Proto->isSpecificProtocol(KnownProtocolKind::RawRepresentable))
          continue;
        // Conformance to Equatable and Hashable is implied by being a "simple"
        // no-payload enum.
        if (Enum->isSimpleEnum()
            && (Proto->isSpecificProtocol(KnownProtocolKind::Equatable)
                || Proto->isSpecificProtocol(KnownProtocolKind::Hashable)))
          continue;
      }

      if (PrintedInherited)
        Printer << ", ";
      else if (!PrintedColon)
        Printer << " : ";
      Proto->getDeclaredType()->print(Printer, Options);
      PrintedInherited = true;
      PrintedColon = true;
    }
    if (UseProtocolCompositionSyntax)
      Printer << ">";
  } else {
    SmallVector<TypeLoc, 6> TypesToPrint;
    for (auto TL : inherited) {
      if (auto Ty = TL.getType()) {
        if (auto NTD = Ty->getAnyNominal())
          if (!shouldPrint(NTD))
            continue;
      }
      TypesToPrint.push_back(TL);
    }
    if (TypesToPrint.empty())
      return;

    Printer << " : ";

    if (explicitClass)
      Printer << " class, ";

    interleave(TypesToPrint, [&](TypeLoc TL) {
      printTypeLoc(TL);
    }, [&]() {
      Printer << ", ";
    });
  }
}

void PrintAST::printInherited(const TypeDecl *decl, bool explicitClass) {
  printInherited(decl, decl->getInherited(), decl->getProtocols(), nullptr,
                 explicitClass);
}

void PrintAST::printInherited(const EnumDecl *D) {
  printInherited(D, D->getInherited(), D->getProtocols(), D->getRawType());
}

void PrintAST::printInherited(const ExtensionDecl *decl) {
  printInherited(decl, decl->getInherited(), decl->getProtocols());
}

void PrintAST::printInherited(const GenericTypeParamDecl *D) {
  printInherited(D, D->getInherited(), D->getConformingProtocols(nullptr),
                 D->getSuperclass(), false, true);
}

static void getModuleEntities(const clang::Module *ClangMod,
                              SmallVectorImpl<ModuleEntity> &ModuleEnts) {
  if (!ClangMod)
    return;

  getModuleEntities(ClangMod->Parent, ModuleEnts);
  ModuleEnts.push_back(ClangMod);
}

static void getModuleEntities(ImportDecl *Import,
                              SmallVectorImpl<ModuleEntity> &ModuleEnts) {
  if (auto *ClangMod = Import->getClangModule()) {
    getModuleEntities(ClangMod, ModuleEnts);
    return;
  }

  auto Mod = Import->getModule();
  if (!Mod)
    return;

  if (auto *ClangMod = Mod->findUnderlyingClangModule()) {
    getModuleEntities(ClangMod, ModuleEnts);
  } else {
    ModuleEnts.push_back(Mod);
  }
}

void PrintAST::visitImportDecl(ImportDecl *decl) {
  printAttributes(decl);
  Printer << "import ";

  switch (decl->getImportKind()) {
  case ImportKind::Module:
    break;
  case ImportKind::Type:
    Printer << "typealias ";
    break;
  case ImportKind::Struct:
    Printer << "struct ";
    break;
  case ImportKind::Class:
    Printer << "class ";
    break;
  case ImportKind::Enum:
    Printer << "enum ";
    break;
  case ImportKind::Protocol:
    Printer << "protocol ";
    break;
  case ImportKind::Var:
    Printer << "var ";
    break;
  case ImportKind::Func:
    Printer << "func ";
    break;
  }

  SmallVector<ModuleEntity, 4> ModuleEnts;
  getModuleEntities(decl, ModuleEnts);

  ArrayRef<ModuleEntity> Mods = ModuleEnts;
  interleave(decl->getFullAccessPath(),
             [&](const ImportDecl::AccessPathElement &Elem) {
               if (!Mods.empty()) {
                 Printer.printModuleRef(Mods.front(), Elem.first);
                 Mods = Mods.slice(1);
               } else {
                 Printer << Elem.first.str();
               }
             },
             [&] { Printer << "."; });
}

void PrintAST::visitExtensionDecl(ExtensionDecl *decl) {
  printAttributes(decl);
  Printer << "extension ";
  recordDeclLoc(decl,
    [&]{
      // We cannot extend sugared types.
      auto *nominal = decl->getExtendedType()->getAnyNominal();
      assert(nominal && "extension of non-nominal type");
      
      if (auto ct = decl->getExtendedType()->getAs<ClassType>()) {
        if (auto ParentType = ct->getParent()) {
          ParentType.print(Printer, Options);
          Printer << ".";
        }
      }
      if (auto st = decl->getExtendedType()->getAs<StructType>()) {
        if (auto ParentType = st->getParent()) {
          ParentType.print(Printer, Options);
          Printer << ".";
        }
      }
      Printer.printTypeRef(nominal, nominal->getName());
    });
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitPatternBindingDecl(PatternBindingDecl *decl) {
  if (decl->isStatic())
    printStaticKeyword(decl->getCorrectStaticSpelling());

  // FIXME: We're not printing proper "{ get set }" annotations in pattern
  // binding decls.  As a hack, scan the decl to find out if any of the
  // variables are immutable, and if so, we print as 'let'.  This allows us to
  // handle the 'let x = 4' case properly at least.
  const VarDecl *anyVar = nullptr;
  for (auto entry : decl->getPatternList()) {
    entry.ThePattern->forEachVariable([&](VarDecl *V) {
      anyVar = V;
    });
    if (anyVar) break;
  }

  // FIXME: PatternBindingDecls don't have attributes themselves, so just assume
  // the variables all have the same attributes. This isn't exactly true
  // after type-checking, but it's close enough for now.
  if (anyVar) {
    printAttributes(anyVar);
    printAccessibility(anyVar);
    Printer << (anyVar->isSettable(anyVar->getDeclContext()) ? "var " : "let ");
  } else {
    Printer << "let ";
  }
  
  bool isFirst = true;
  for (auto entry : decl->getPatternList()) {
    if (isFirst)
      isFirst = false;
    else
      Printer << ", ";
    
    printPattern(entry.ThePattern);
    if (Options.VarInitializers) {
      // FIXME: Implement once we can pretty-print expressions.
    }
  }
}

void PrintAST::visitTopLevelCodeDecl(TopLevelCodeDecl *decl) {
  printASTNodes(decl->getBody()->getElements(), /*NeedIndent=*/false);
}

void PrintAST::visitIfConfigDecl(IfConfigDecl *ICD) {
  // FIXME: Pretty print #if decls
}

void PrintAST::visitTypeAliasDecl(TypeAliasDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "typealias ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  bool ShouldPrint = true;
  Type Ty = decl->getUnderlyingType();
  // If the underlying type is private, don't print it.
  if (Options.SkipPrivateStdlibDecls && Ty && Ty.isPrivateStdlibType())
    ShouldPrint = false;

  if (ShouldPrint) {
    Printer << " = ";
    printTypeLoc(decl->getUnderlyingTypeLoc());
  }
}

void PrintAST::visitGenericTypeParamDecl(GenericTypeParamDecl *decl) {
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });

  printInherited(decl, decl->getInherited(),
                 decl->getConformingProtocols(nullptr), decl->getSuperclass());
}

void PrintAST::visitAssociatedTypeDecl(AssociatedTypeDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "typealias ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });

  printInherited(decl, decl->getInherited(),
                 decl->getConformingProtocols(nullptr), decl->getSuperclass());

  if (!decl->getDefaultDefinitionLoc().isNull()) {
    Printer << " = ";
    decl->getDefaultDefinitionLoc().getType().print(Printer, Options);
  }
}

void PrintAST::visitEnumDecl(EnumDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "enum ";
  recordDeclLoc(decl,
    [&]{
      printNominalDeclName(decl);
    });
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitStructDecl(StructDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "struct ";
  recordDeclLoc(decl,
    [&]{
      printNominalDeclName(decl);
    });
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitClassDecl(ClassDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "class ";
  recordDeclLoc(decl,
    [&]{
      printNominalDeclName(decl);
    });

  printInherited(decl, decl->getInherited(), decl->getProtocols(),
                 decl->getSuperclass());

  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitProtocolDecl(ProtocolDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "protocol ";
  recordDeclLoc(decl,
    [&]{
      printNominalDeclName(decl);
    });

  // Figure out whether we need an explicit 'class' in the inheritance.
  bool explicitClass = false;
  if (decl->requiresClass() && !decl->isObjC()) {
    bool inheritsRequiresClass = false;
    for (auto proto : decl->getInheritedProtocols(nullptr)) {
      if (proto->requiresClass()) {
        inheritsRequiresClass = true;
        break;
      }
    }

    if (!inheritsRequiresClass)
      explicitClass = true;
  }

  printInherited(decl, explicitClass);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

static bool isStructOrClassContext(DeclContext *dc) {
  if (auto ctx = dc->getDeclaredTypeInContext())
    return ctx->getClassOrBoundGenericClass() ||
           ctx->getStructOrBoundGenericStruct();
  return false;
}

void PrintAST::visitVarDecl(VarDecl *decl) {
  printDocumentationComment(decl);
  // Print @sil_stored when the attribute is not already
  // on, decl has storage and it is on a class.
  if (Options.PrintForSIL && decl->hasStorage() &&
      isStructOrClassContext(decl->getDeclContext()) &&
      !decl->getAttrs().hasAttribute<SILStoredAttr>())
    Printer << "@sil_stored ";
  printAttributes(decl);
  printAccessibility(decl);
  if (!Options.SkipIntroducerKeywords) {
    if (decl->isStatic())
      printStaticKeyword(decl->getCorrectStaticSpelling());
    Printer << (decl->isLet() ? "let " : "var ");
  }
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  if (decl->hasType()) {
    Printer << ": ";
    decl->getType().print(Printer, Options);
  }

  printAccessors(decl);
}

void PrintAST::visitParamDecl(ParamDecl *decl) {
  return visitVarDecl(decl);
}

void PrintAST::printOneParameter(const Pattern *BodyPattern,
                                 bool ArgNameIsAPIByDefault,
                                 bool StripOuterSliceType,
                                 bool Curried) {
  auto printArgName = [&]() {
    // Print argument name.
    auto ArgName = BodyPattern->getBoundName();
    auto BodyName = BodyPattern->getBodyName();
    auto printArg = [&]{
      if (!ArgName.empty() && !ArgNameIsAPIByDefault)
        Printer << "#";
      Printer.printName(ArgName);
    };

    switch (Options.ArgAndParamPrinting) {
    case PrintOptions::ArgAndParamPrintingMode::ArgumentOnly:
      printArg();
      break;
    case PrintOptions::ArgAndParamPrintingMode::BothIfDifferent:
      if (ArgName == BodyName) {
        printArg();
        break;
      }
      if (ArgName.empty() && !ArgNameIsAPIByDefault) {
        Printer.printName(BodyName);
        break;
      }
      SWIFT_FALLTHROUGH;
    case PrintOptions::ArgAndParamPrintingMode::BothAlways:
      Printer.printName(ArgName);
      Printer << " ";
      Printer.printName(BodyName);
      break;
    }
    Printer << ": ";
  };

  if (auto *VP = dyn_cast<VarPattern>(BodyPattern))
    BodyPattern = VP->getSubPattern();
  auto *TypedBodyPattern = dyn_cast<TypedPattern>(BodyPattern);
  if (!TypedBodyPattern) {
    // It was a syntax error.
    printArgName();
    return;
  }
  auto TheTypeLoc = TypedBodyPattern->getTypeLoc();
  if (TheTypeLoc.hasLocation()) {
    // If the outer typeloc is an InOutTypeRepr, print the 'inout' before the
    // subpattern.
    if (auto *IOTR = dyn_cast<InOutTypeRepr>(TheTypeLoc.getTypeRepr())) {
      TheTypeLoc = TypeLoc(IOTR->getBase());
      if (Type T = TheTypeLoc.getType()) {
        if (auto *IOT = T->getAs<InOutType>()) {
          TheTypeLoc.setType(IOT->getObjectType());
        }
      }

      Printer << "inout ";
    }
  } else {
    if (Type T = TheTypeLoc.getType()) {
      if (auto *IOT = T->getAs<InOutType>()) {
        Printer << "inout ";
        TheTypeLoc.setType(IOT->getObjectType());
      }
    }
  }

  // If the parameter is autoclosure, or noescape, print it.  This is stored
  // on the type of the pattern, not on the typerepr.
  if (BodyPattern->hasType()) {
    auto bodyCanType = BodyPattern->getType()->getCanonicalType();
    if (auto patternType = dyn_cast<AnyFunctionType>(bodyCanType)) {
      switch (patternType->isAutoClosure()*2 + patternType->isNoEscape()) {
      case 0: break; // neither.
      case 1: Printer << "@noescape "; break;
      case 2: Printer << "@autoclosure(escaping) "; break;
      case 3: Printer << "@autoclosure "; break;
      }
    }
  }

  printArgName();

  if (StripOuterSliceType && !TheTypeLoc.hasLocation()) {
    if (auto *BGT = TypedBodyPattern->getType()->getAs<BoundGenericType>()) {
      BGT->getGenericArgs()[0].print(Printer, Options);
      return;
    }
  }
  printTypeLoc(TheTypeLoc);
}

void PrintAST::printFunctionParameters(AbstractFunctionDecl *AFD) {
  ArrayRef<Pattern *> BodyPatterns = AFD->getBodyParamPatterns();

  // Skip over the implicit 'self'.
  if (AFD->getImplicitSelfDecl()) {
    BodyPatterns = BodyPatterns.slice(1);
  }

  for (unsigned CurrPattern = 0, NumPatterns = BodyPatterns.size();
       CurrPattern != NumPatterns; ++CurrPattern) {
    if (auto *BodyTuple = dyn_cast<TuplePattern>(BodyPatterns[CurrPattern])) {
      Printer << "(";
      for (unsigned i = 0, e = BodyTuple->getFields().size(); i != e; ++i) {
        if (i > 0)
          Printer << ", ";

        // Determine whether the argument name is API by default.
        bool ArgNameIsAPIByDefault = (CurrPattern == 0 &&
                                      AFD->argumentNameIsAPIByDefault(i)) ||
          CurrPattern > 0 ||
          (BodyTuple->getFields()[i].getDefaultArgKind() != 
             DefaultArgumentKind::None &&
           Options.PrintDefaultParameterPlaceholder);

        printOneParameter(BodyTuple->getFields()[i].getPattern(),
                          ArgNameIsAPIByDefault,
                          /*StripOuterSliceType=*/i == e - 1 &&
                            BodyTuple->hasVararg(),
                          /*Curried=*/CurrPattern > 0);
        if (Options.PrintDefaultParameterPlaceholder &&
            BodyTuple->getFields()[i].getDefaultArgKind() !=
                DefaultArgumentKind::None)
          Printer << " = default";
      }
      if (BodyTuple->hasVararg())
        Printer << "...";
      Printer << ")";
      continue;
    }
    bool ArgNameIsAPIByDefault = (CurrPattern == 0 &&
                                  AFD->argumentNameIsAPIByDefault(0));
    auto *BodyParen = cast<ParenPattern>(BodyPatterns[CurrPattern]);
    Printer << "(";
    printOneParameter(BodyParen->getSubPattern(),
                      ArgNameIsAPIByDefault,
                      /*StripOuterSliceType=*/false,
                      /*Curried=*/CurrPattern > 0);
    Printer << ")";
  }
}

bool PrintAST::printASTNodes(const ArrayRef<ASTNode> &Elements,
                             bool NeedIndent) {
  IndentRAII IndentMore(*this, NeedIndent);
  bool PrintedSomething = false;
  for (auto element : Elements) {
    PrintedSomething = true;
    Printer.printNewline();
    indent();
    if (auto decl = element.dyn_cast<Decl*>()) {
      if (decl->shouldPrintInContext(Options))
        visit(decl);
    } else if (auto stmt = element.dyn_cast<Stmt*>()) {
      visit(stmt);
    } else {
      // FIXME: print expression
      // visit(element.get<Expr*>());
    }
  }
  return PrintedSomething;
}

void PrintAST::visitFuncDecl(FuncDecl *decl) {
  if (decl->isAccessor()) {
    printDocumentationComment(decl);
    printAttributes(decl);
    switch (auto kind = decl->getAccessorKind()) {
    case AccessorKind::NotAccessor: break;
    case AccessorKind::IsGetter:
    case AccessorKind::IsAddressor:
      recordDeclLoc(decl,
        [&]{
          if (decl->isMutating())
            Printer << "mutating ";
          Printer << (kind == AccessorKind::IsGetter
                        ? "get" : getAddressorLabel(decl));
        });
      Printer << " {";
      break;
    case AccessorKind::IsDidSet:
    case AccessorKind::IsMaterializeForSet:
    case AccessorKind::IsMutableAddressor:
      recordDeclLoc(decl,
        [&]{
          if (decl->isExplicitNonMutating())
            Printer << "nonmutating ";
          Printer << (kind == AccessorKind::IsDidSet ? "didSet" :
                      kind == AccessorKind::IsMaterializeForSet
                        ? "materializeForSet"
                        : getMutableAddressorLabel(decl));
        });
      Printer << " {";
      break;
    case AccessorKind::IsSetter:
    case AccessorKind::IsWillSet:
      recordDeclLoc(decl,
        [&]{
          if (decl->isExplicitNonMutating())
            Printer << "nonmutating ";
          Printer << (decl->isSetter() ? "set" : "willSet");

          auto BodyParams = decl->getBodyParamPatterns();
          auto ValueParam = BodyParams.back()->getSemanticsProvidingPattern();
          if (auto *TP = dyn_cast<TuplePattern>(ValueParam)) {
            if (!TP->isImplicit() && !TP->getFields().empty()) {
              auto *P = TP->getFields().begin()->getPattern()->
                            getSemanticsProvidingPattern();
              Identifier Name = P->getBodyName();
              if (!Name.empty() && !P->isImplicit()) {
                Printer << "(";
                Printer.printName(Name);
                Printer << ")";
              }
            }
          }
        });
      Printer << " {";
    }
    if (Options.FunctionDefinitions && decl->getBody()) {
      if (printASTNodes(decl->getBody()->getElements())) {
        Printer.printNewline();
        indent();
      }
    }
    Printer << "}";
  } else {
    printDocumentationComment(decl);
    printAttributes(decl);
    printAccessibility(decl);
    if (!Options.SkipIntroducerKeywords) {
      if (decl->isStatic() && !decl->isOperator())
        printStaticKeyword(decl->getCorrectStaticSpelling());
      if (decl->isMutating() && !decl->getAttrs().hasAttribute<MutatingAttr>())
        Printer << "mutating ";
      Printer << "func ";
    }
    recordDeclLoc(decl,
      [&]{
        if (!decl->hasName())
          Printer << "<anonymous>";
        else
          Printer.printName(decl->getName());
        if (decl->isGeneric()) {
          printGenericParams(decl->getGenericParams());
        }

        printFunctionParameters(decl);
      });

    auto &Context = decl->getASTContext();
    Type ResultTy = decl->getResultType();
    if (ResultTy && !ResultTy->isEqual(TupleType::getEmpty(Context))) {
      Printer << " -> ";
      ResultTy->print(Printer, Options);
    }

    if (!Options.FunctionDefinitions || !decl->getBody()) {
      return;
    }

    Printer << " ";
    visit(decl->getBody());
  }
}

void PrintAST::printEnumElement(EnumElementDecl *elt) {
  recordDeclLoc(elt,
    [&]{
      Printer.printName(elt->getName());
    });

  if (elt->hasArgumentType()) {
    Type Ty = elt->getArgumentType();
    if (!Options.SkipPrivateStdlibDecls || !Ty.isPrivateStdlibType())
      Ty.print(Printer, Options);
  }
}

void PrintAST::visitEnumCaseDecl(EnumCaseDecl *decl) {
  printAttributes(decl);
  Printer << "case ";

  interleave(decl->getElements().begin(), decl->getElements().end(),
    [&](EnumElementDecl *elt) {
      printEnumElement(elt);
    },
    [&] { Printer << ", "; });
}

void PrintAST::visitEnumElementDecl(EnumElementDecl *decl) {
  if (!decl->shouldPrintInContext(Options))
    return;

  // In cases where there is no parent EnumCaseDecl (such as imported or
  // deserialized elements), print the element independently.
  printAttributes(decl);
  Printer << "case ";
  printEnumElement(decl);
}

void PrintAST::visitSubscriptDecl(SubscriptDecl *decl) {
  printAttributes(decl);
  printAccessibility(decl);
  recordDeclLoc(decl,
    [&]{
      Printer << "subscript ";
      printPattern(decl->getIndices());
    });
  Printer << " -> ";
  decl->getElementType().print(Printer, Options);

  printAccessors(decl);
}

void PrintAST::visitConstructorDecl(ConstructorDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);

  if ((decl->getInitKind() == CtorInitializerKind::Convenience ||
       decl->getInitKind() == CtorInitializerKind::ConvenienceFactory) &&
      !decl->getAttrs().hasAttribute<ConvenienceAttr>())
    Printer << "convenience ";
  
  recordDeclLoc(decl,
    [&]{
      Printer << "init";
      switch (decl->getFailability()) {
      case OTK_None:
        break;
        
      case OTK_Optional:
        Printer << "?";
        break;

      case OTK_ImplicitlyUnwrappedOptional:
        Printer << "!";
        break;
      }

      if (decl->isGeneric())
        printGenericParams(decl->getGenericParams());

      printFunctionParameters(decl);
    });
  
  if (decl->getInitKind() == CtorInitializerKind::Factory) {
    Printer << " -> ";
    Printer.printName(decl->getExtensionType()->getAnyNominal()->getName());
  }

  if (!Options.FunctionDefinitions || !decl->getBody()) {
    return;
  }

  Printer << " ";
  visit(decl->getBody());
}

void PrintAST::visitDestructorDecl(DestructorDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  recordDeclLoc(decl,
    [&]{
      Printer << "deinit ";
    });

  if (!Options.FunctionDefinitions || !decl->getBody()) {
    return;
  }

  Printer << " ";
  visit(decl->getBody());
}

void PrintAST::visitInfixOperatorDecl(InfixOperatorDecl *decl) {
  Printer << "infix operator ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  Printer << " {";
  Printer.printNewline();
  {
    IndentRAII indentMore(*this);
    if (!decl->isAssociativityImplicit()) {
      indent();
      Printer << "associativity ";
      switch (decl->getAssociativity()) {
      case Associativity::None:
        Printer << "none";
        break;
      case Associativity::Left:
        Printer << "left";
        break;
      case Associativity::Right:
        Printer << "right";
        break;
      }
      Printer.printNewline();
    }
    if (!decl->isPrecedenceImplicit()) {
      indent();
      Printer << "precedence " << decl->getPrecedence();
      Printer.printNewline();
    }
    if (!decl->isAssignmentImplicit()) {
      indent();
      if (decl->isAssignment())
        Printer << "assignment";
      else
        Printer << "/* not assignment */";
      Printer.printNewline();
    }
  }
  indent();
  Printer << "}";
}

void PrintAST::visitPrefixOperatorDecl(PrefixOperatorDecl *decl) {
  Printer << "prefix operator ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  Printer << " {";
  Printer.printNewline();
  Printer << "}";
}

void PrintAST::visitPostfixOperatorDecl(PostfixOperatorDecl *decl) {
  Printer << "postfix operator ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  Printer << " {";
  Printer.printNewline();
  Printer << "}";
}

void PrintAST::visitBraceStmt(BraceStmt *stmt) {
  Printer << "{";
  printASTNodes(stmt->getElements());
  Printer.printNewline();
  indent();
  Printer << "}";
}

void PrintAST::visitReturnStmt(ReturnStmt *stmt) {
  Printer << "return";
  if (stmt->hasResult()) {
    Printer << " ";
    // FIXME: print expression.
  }
}

void PrintAST::visitIfStmt(IfStmt *stmt) {
  Printer << "if ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getThenStmt());
  if (auto elseStmt = stmt->getElseStmt()) {
    Printer << " else ";
    visit(elseStmt);
  }
}

void PrintAST::visitIfConfigStmt(IfConfigStmt *stmt) {
  for (auto &Clause : stmt->getClauses()) {
    if (&Clause == &*stmt->getClauses().begin())
      Printer << "#if "; // FIXME: print condition
    else if (Clause.Cond)
      Printer << "#elseif"; // FIXME: print condition
    else
      Printer << "#else";
    Printer.printNewline();
    if (printASTNodes(Clause.Elements)) {
      Printer.printNewline();
      indent();
    }
  }
  Printer.printNewline();
  Printer << "#endif";
}

void PrintAST::visitWhileStmt(WhileStmt *stmt) {
  Printer << "while ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitDoWhileStmt(DoWhileStmt *stmt) {
  Printer << "do ";
  visit(stmt->getBody());
  Printer << " while ";
  // FIXME: print condition
}

void PrintAST::visitDoStmt(DoStmt *stmt) {
  Printer << "do ";
  visit(stmt->getBody());
}

void PrintAST::visitDoCatchStmt(DoCatchStmt *stmt) {
  Printer << "do ";
  visit(stmt->getBody());
  for (auto clause : stmt->getCatches()) {
    visitCatchStmt(clause);
  }
}

void PrintAST::visitCatchStmt(CatchStmt *stmt) {
  Printer << "catch ";
  printPattern(stmt->getErrorPattern());
  if (auto guard = stmt->getGuardExpr()) {
    Printer << " where ";
    // FIXME: print guard expression
    (void) guard;
  }
  Printer << ' ';
  visit(stmt->getBody());
}

void PrintAST::visitForStmt(ForStmt *stmt) {
  Printer << "for (";
  // FIXME: print initializer
  Printer << "; ";
  if (stmt->getCond().isNonNull()) {
    // FIXME: print cond
  }
  Printer << "; ";
  // FIXME: print increment
  Printer << ") ";
  visit(stmt->getBody());
}

void PrintAST::visitForEachStmt(ForEachStmt *stmt) {
  Printer << "for ";
  printPattern(stmt->getPattern());
  Printer << " in ";
  // FIXME: print container
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitBreakStmt(BreakStmt *stmt) {
  Printer << "break";
}

void PrintAST::visitContinueStmt(ContinueStmt *stmt) {
  Printer << "continue";
}

void PrintAST::visitFallthroughStmt(FallthroughStmt *stmt) {
  Printer << "fallthrough";
}

void PrintAST::visitSwitchStmt(SwitchStmt *stmt) {
  Printer << "switch ";
  // FIXME: print subject
  Printer << "{";
  Printer.printNewline();
  for (CaseStmt *C : stmt->getCases()) {
    visit(C);
  }
  Printer.printNewline();
  indent();
  Printer << "}";
}

void PrintAST::visitCaseStmt(CaseStmt *CS) {
  if (CS->isDefault()) {
    Printer << "default";
  } else {
    auto PrintCaseLabelItem = [&](const CaseLabelItem &CLI) {
      if (auto *P = CLI.getPattern())
        printPattern(P);
      if (CLI.getGuardExpr()) {
        Printer << " where ";
        // FIXME: print guard expr
      }
    };
    Printer << "case ";
    interleave(CS->getCaseLabelItems(), PrintCaseLabelItem,
               [&] { Printer << ", "; });
  }
  Printer << ":";
  Printer.printNewline();

  printASTNodes((cast<BraceStmt>(CS->getBody())->getElements()));
}

void PrintAST::visitFailStmt(FailStmt *stmt) {
  Printer << "return nil";
}

void Decl::print(raw_ostream &os) const {
  PrintOptions options;
  options.FunctionDefinitions = true;
  options.TypeDefinitions = true;
  options.VarInitializers = true;

  print(os, options);
}

void Decl::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}

bool Decl::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  PrintAST printer(Printer, Opts);
  return printer.visit(const_cast<Decl *>(this));
}

bool Decl::shouldPrintInContext(const PrintOptions &PO) const {
  // Skip getters/setters. They are part of the variable or subscript.
  if (isa<FuncDecl>(this) && cast<FuncDecl>(this)->isAccessor())
    return false;

  if (PO.ExplodePatternBindingDecls) {
    if (isa<VarDecl>(this))
      return true;
    if (isa<PatternBindingDecl>(this))
      return false;
  } else {
    // Try to preserve the PatternBindingDecl structure.

    // Skip stored variables, unless they came from a Clang module.
    // Stored variables in Swift source will be picked up by the
    // PatternBindingDecl.
    if (auto *VD = dyn_cast<VarDecl>(this)) {
      if (!VD->hasClangNode() && VD->hasStorage() &&
          VD->getStorageKind() != VarDecl::StoredWithObservers)
        return false;
    }

    // Skip pattern bindings that consist of just one computed variable.
    if (auto pbd = dyn_cast<PatternBindingDecl>(this)) {
      if (pbd->getPatternList().size() == 1) {
        auto pattern =
          pbd->getPatternList()[0].ThePattern->getSemanticsProvidingPattern();
        if (auto named = dyn_cast<NamedPattern>(pattern)) {
          auto StorageKind = named->getDecl()->getStorageKind();
          if (StorageKind == VarDecl::Computed ||
              StorageKind == VarDecl::StoredWithObservers)
            return false;
        }
      }
    }
  }

  if (auto EED = dyn_cast<EnumElementDecl>(this)) {
    // Enum elements are printed as part of the EnumCaseDecl, unless they were
    // imported without source info.
    return !EED->getSourceRange().isValid();
  }

  // Print everything else.
  return true;
}

void Pattern::print(llvm::raw_ostream &OS, const PrintOptions &Options) const {
  StreamPrinter StreamPrinter(OS);
  PrintAST Printer(StreamPrinter, Options);
  Printer.printPattern(this);
}

//===----------------------------------------------------------------------===//
//  Type Printing
//===----------------------------------------------------------------------===//

namespace {
class TypePrinter : public TypeVisitor<TypePrinter> {
  ASTPrinter &Printer;
  const PrintOptions &Options;
  Optional<std::vector<GenericParamList *>> UnwrappedGenericParams;

  void printDeclContext(DeclContext *DC) {
    switch (DC->getContextKind()) {
    case DeclContextKind::Module: {
      Module *M = cast<Module>(DC);

      if (auto Parent = M->getParent())
        printDeclContext(Parent);
      Printer.printModuleRef(M, M->Name);
      return;
    }

    case DeclContextKind::FileUnit:
      printDeclContext(DC->getParent());
      return;

    case DeclContextKind::AbstractClosureExpr:
      // FIXME: print closures somehow.
      return;

    case DeclContextKind::NominalTypeDecl:
      visit(cast<NominalTypeDecl>(DC)->getType());
      return;

    case DeclContextKind::ExtensionDecl:
      visit(cast<ExtensionDecl>(DC)->getExtendedType());
      return;

    case DeclContextKind::Initializer:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::SerializedLocal:
      llvm_unreachable("bad decl context");

    case DeclContextKind::AbstractFunctionDecl:
      visit(cast<AbstractFunctionDecl>(DC)->getType());
      return;
    }
  }

  void printGenericArgs(ArrayRef<Type> Args) {
    if (Args.empty())
      return;

    Printer << "<";
    bool First = true;
    for (Type Arg : Args) {
      if (First)
        First = false;
      else
        Printer << ", ";
      visit(Arg);
    }
    Printer << ">";
  }

  static bool isSimple(Type type) {
    switch (type->getKind()) {
    case TypeKind::Function:
    case TypeKind::PolymorphicFunction:
    case TypeKind::GenericFunction:
      return false;

    case TypeKind::Metatype:
    case TypeKind::ExistentialMetatype:
      return !cast<AnyMetatypeType>(type.getPointer())->hasRepresentation();

    default:
      return true;
    }
  }

  /// Helper function for printing a type that is embedded within a larger type.
  ///
  /// This is necessary whenever the inner type may not normally be represented
  /// as a 'type-simple' production in the type grammar.
  void printWithParensIfNotSimple(Type T) {
    if (T.isNull()) {
      visit(T);
      return;
    }

    if (!isSimple(T)) {
      Printer << "(";
      visit(T);
      Printer << ")";
    } else {
      visit(T);
    }
  }

  void printGenericParams(GenericParamList *Params) {
    PrintAST(Printer, Options).printGenericParams(Params);
  }

  template <typename T>
  void printModuleContext(T *Ty) {
    Module *Mod = Ty->getDecl()->getModuleContext();
    Printer.printModuleRef(Mod, Mod->Name);
    Printer << ".";
  }

  template <typename T>
  void printTypeDeclName(T *Ty) {
    TypeDecl *TD = Ty->getDecl();
    Printer.printTypeRef(TD, TD->getName());
  }

  // FIXME: we should have a callback that would tell us
  // whether it's kosher to print a module name or not
  bool isLLDBExpressionModule(Module *M) {
    if (!M)
      return false;
    return M->Name.str().startswith(LLDB_EXPRESSIONS_MODULE_NAME_PREFIX);
  }

  bool shouldPrintFullyQualified(TypeBase *T) {
    if (Options.FullyQualifiedTypes)
      return true;

    if (!Options.FullyQualifiedTypesIfAmbiguous)
      return false;

    Decl *D = nullptr;
    if (auto *NAT = dyn_cast<NameAliasType>(T))
      D = NAT->getDecl();
    else
      D = T->getAnyNominal();

    // If we can not find the declaration, be extra careful and print
    // the type qualified.
    if (!D)
      return true;

    Module *M = D->getDeclContext()->getParentModule();

    // Don't print qualifiers for types from the standard library.
    if (M->isStdlibModule() ||
        M->Name == T->getASTContext().ObjCModuleName ||
        M->isSystemModule() ||
        isLLDBExpressionModule(M))
      return false;

    // Don't print qualifiers for imported types.
    for (auto File : M->getFiles()) {
      if (File->getKind() == FileUnitKind::ClangModule)
        return false;
    }

    return true;
  }

public:
  TypePrinter(ASTPrinter &Printer, const PrintOptions &PO)
      : Printer(Printer), Options(PO) {}

  void visitErrorType(ErrorType *T) {
    Printer << "<<error type>>";
  }

  void visitBuiltinRawPointerType(BuiltinRawPointerType *T) {
    Printer << "Builtin.RawPointer";
  }

  void visitBuiltinNativeObjectType(BuiltinNativeObjectType *T) {
    Printer << "Builtin.NativeObject";
  }

  void visitBuiltinUnknownObjectType(BuiltinUnknownObjectType *T) {
    Printer << "Builtin.UnknownObject";
  }

  void visitBuiltinBridgeObjectType(BuiltinBridgeObjectType *T) {
    Printer << "Builtin.BridgeObject";
  }
  
  void visitBuiltinUnsafeValueBufferType(BuiltinUnsafeValueBufferType *T) {
    Printer << "Builtin.UnsafeValueBuffer";
  }

  void visitBuiltinVectorType(BuiltinVectorType *T) {
    llvm::SmallString<32> UnderlyingStrVec;
    StringRef UnderlyingStr;
    {
      // FIXME: Ugly hack: remove the .Builtin from the element type.
      llvm::raw_svector_ostream UnderlyingOS(UnderlyingStrVec);
      visit(T->getElementType());
      if (UnderlyingStrVec.startswith("Builtin."))
        UnderlyingStr = UnderlyingStrVec.substr(9);
      else
        UnderlyingStr = UnderlyingStrVec;
    }

    Printer << "Builtin.Vec" << T->getNumElements() << "x" << UnderlyingStr;
  }

  void visitBuiltinIntegerType(BuiltinIntegerType *T) {
    auto width = T->getWidth();
    if (width.isFixedWidth()) {
      Printer << "Builtin.Int" << width.getFixedWidth();
    } else if (width.isPointerWidth()) {
      Printer << "Builtin.Word";
    } else {
      llvm_unreachable("impossible bit width");
    }
  }

  void visitBuiltinFloatType(BuiltinFloatType *T) {
    switch (T->getFPKind()) {
    case BuiltinFloatType::IEEE16:  Printer << "Builtin.FPIEEE16"; return;
    case BuiltinFloatType::IEEE32:  Printer << "Builtin.FPIEEE32"; return;
    case BuiltinFloatType::IEEE64:  Printer << "Builtin.FPIEEE64"; return;
    case BuiltinFloatType::IEEE80:  Printer << "Builtin.FPIEEE80"; return;
    case BuiltinFloatType::IEEE128: Printer << "Builtin.FPIEEE128"; return;
    case BuiltinFloatType::PPC128:  Printer << "Builtin.FPPPC128"; return;
    }
  }

  void visitNameAliasType(NameAliasType *T) {
    if (Options.PrintForSIL) {
      visit(T->getSinglyDesugaredType());
      return;
    }

    if (shouldPrintFullyQualified(T)) {
      if (auto ParentDC = T->getDecl()->getDeclContext()) {
        printDeclContext(ParentDC);
        Printer << ".";
      }
    }
    printTypeDeclName(T);
  }

  void visitParenType(ParenType *T) {
    Printer << "(";
    visit(T->getUnderlyingType());
    Printer << ")";
  }

  void visitTupleType(TupleType *T) {
    Printer << "(";

    auto Fields = T->getFields();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      if (i)
        Printer << ", ";
      const TupleTypeElt &TD = Fields[i];
      Type EltType = TD.getType();
      if (auto *IOT = EltType->getAs<InOutType>()) {
        Printer << "inout ";
        EltType = IOT->getObjectType();
      }

      if (TD.hasName()) {
        Printer.printName(TD.getName());
        Printer << ": ";
      }

      if (TD.isVararg()) {
        visit(TD.getVarargBaseTy());
        Printer << "...";
      } else
        visit(EltType);
    }
    Printer << ")";
  }

  void visitUnboundGenericType(UnboundGenericType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }
    printTypeDeclName(T);
  }

  void visitBoundGenericType(BoundGenericType *T) {
    if (Options.SynthesizeSugarOnTypes) {
      auto *NT = T->getDecl();
      auto &Ctx = T->getASTContext();
      if (NT == Ctx.getArrayDecl()) {
        Printer << "[";
        visit(T->getGenericArgs()[0]);
        Printer << "]";
        return;
      }
      if (NT == Ctx.getDictionaryDecl()) {
        Printer << "[";
        visit(T->getGenericArgs()[0]);
        Printer << " : ";
        visit(T->getGenericArgs()[1]);
        Printer << "]";
        return;
      }
      if (NT == Ctx.getOptionalDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        Printer << "?";
        return;
      }
      if (NT == Ctx.getImplicitlyUnwrappedOptionalDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        Printer << "!";
        return;
      }
    }
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
    printGenericArgs(T->getGenericArgs());
  }

  void visitEnumType(EnumType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitStructType(StructType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitClassType(ClassType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitAnyMetatypeType(AnyMetatypeType *T) {
    if (T->hasRepresentation()) {
      switch (T->getRepresentation()) {
      case MetatypeRepresentation::Thin:  Printer << "@thin ";  break;
      case MetatypeRepresentation::Thick: Printer << "@thick "; break;
      case MetatypeRepresentation::ObjC:  Printer << "@objc_metatype "; break;
      }
    }
    printWithParensIfNotSimple(T->getInstanceType());

    // We spell normal metatypes of existential types as .Protocol.
    if (isa<MetatypeType>(T) && T->getInstanceType()->isAnyExistentialType()) {
      Printer << ".Protocol";
    } else {
      Printer << ".Type";
    }
  }

  void visitModuleType(ModuleType *T) {
    Printer << "module<";
    Printer.printModuleRef(T->getModule(), T->getModule()->Name);
    Printer << ">";
  }

  void visitDynamicSelfType(DynamicSelfType *T) {
    Printer << "Self";
  }

  void printFunctionExtInfo(AnyFunctionType::ExtInfo info) {
    if(Options.SkipAttributes)
      return;
    if (info.isAutoClosure())
      Printer << "@autoclosure ";
    else if (info.isNoEscape())    // autoclosure implies noescape.
      Printer << "@noescape ";
    
    switch (info.getCC()) {
    case AbstractCC::Freestanding: break;
    case AbstractCC::Method:
      Printer << "@cc(method) ";
      break;
    case AbstractCC::C:
      Printer << "@cc(cdecl) ";
      break;
    case AbstractCC::ObjCMethod:
      Printer << "@cc(objc_method) ";
      break;
    case AbstractCC::WitnessMethod:
      Printer << "@cc(witness_method) ";
      break;
    }

    if (Options.PrintFunctionRepresentationAttrs) {
      switch (info.getRepresentation()) {
      case AnyFunctionType::Representation::Thick:
        break;
      case AnyFunctionType::Representation::Thin:
        Printer << "@thin ";
        break;
      case AnyFunctionType::Representation::Block:
        Printer << "@objc_block ";
        break;
      }
    }

    if (info.isNoReturn())
      Printer << "@noreturn ";
  }

  void visitFunctionType(FunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printWithParensIfNotSimple(T->getInput());
    Printer << " -> ";
    T->getResult().print(Printer, Options);
  }

  void visitPolymorphicFunctionType(PolymorphicFunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printGenericParams(&T->getGenericParams());
    Printer << " ";
    printWithParensIfNotSimple(T->getInput());
    Printer << " -> ";
    T->getResult().print(Printer, Options);
  }

  /// If we can't find the depth of a type, return ErrorDepth.
  const unsigned ErrorDepth = ~0U;
  /// A helper function to return the depth of a type.
  unsigned getDepthOfType(Type ty) {
    if (auto paramTy = ty->getAs<GenericTypeParamType>())
      return paramTy->getDepth();

    if (auto depMemTy = dyn_cast<DependentMemberType>(ty->getCanonicalType())) {
      CanType rootTy;
      do {
        rootTy = depMemTy.getBase();
      } while ((depMemTy = dyn_cast<DependentMemberType>(rootTy)));
      if (auto rootParamTy = dyn_cast<GenericTypeParamType>(rootTy))
        return rootParamTy->getDepth();
      return ErrorDepth;
    }

    return ErrorDepth;
  }

  /// A helper function to return the depth of a requirement.
  unsigned getDepthOfRequirement(const Requirement &req) {
    switch (req.getKind()) {
    case RequirementKind::Conformance:
    case RequirementKind::WitnessMarker:
      return getDepthOfType(req.getFirstType());

    case RequirementKind::SameType:
      // Return the max depth of firstType and secondType.
      return std::max(getDepthOfType(req.getFirstType()),
                      getDepthOfType(req.getSecondType()));
    }
    llvm_unreachable("bad RequirementKind");
  }

  void printGenericSignature(ArrayRef<GenericTypeParamType *> genericParams,
                             ArrayRef<Requirement> requirements) {
    if (!Options.PrintInSILBody) {
      printSingleDepthOfGenericSignature(genericParams, requirements, 0);
      return;
    }

    // In order to recover the nested GenericParamLists, we divide genericParams
    // and requirements according to depth.
    unsigned paramIdx = 0, numParam = genericParams.size();
    unsigned reqIdx = 0, numReq = requirements.size();
    while (paramIdx < numParam) {
      unsigned depth = genericParams[paramIdx]->getDepth();

      // Move index to genericParams.
      unsigned lastParamIdx = paramIdx;
      do {
        lastParamIdx++;
      } while (lastParamIdx < numParam &&
               genericParams[lastParamIdx]->getDepth() == depth);

      // Move index to requirements as well.
      unsigned lastReqIdx = reqIdx;
      while (lastReqIdx < numReq) {
        unsigned currentDepth = getDepthOfRequirement(requirements[lastReqIdx]);
        if (currentDepth != ErrorDepth && currentDepth != depth)
          break;
        lastReqIdx++;
      }

      printSingleDepthOfGenericSignature(
        ArrayRef<GenericTypeParamType *>(genericParams.begin() + paramIdx,
                                          lastParamIdx - paramIdx),
        ArrayRef<Requirement>(requirements.begin() + reqIdx,
                              lastReqIdx - reqIdx), reqIdx);
      paramIdx = lastParamIdx;
      reqIdx = lastReqIdx;
    }
  }

  void printSingleDepthOfGenericSignature(
           ArrayRef<GenericTypeParamType *> genericParams,
           ArrayRef<Requirement> requirements, unsigned reqIdx) {
    // Print the generic parameters.
    Printer << "<";
    bool isFirstParam = true;
    for (auto param : genericParams) {
      if (isFirstParam)
        isFirstParam = false;
      else
        Printer << ", ";

      visit(param);
    }

    // Print the requirements.
    bool isFirstReq = true;
    for (const auto &req : requirements) {
      if (req.getKind() == RequirementKind::WitnessMarker)
        continue;

      if (isFirstReq) {
        Printer << " where ";
        isFirstReq = false;
      } else {
        Printer << ", ";
      }

      visit(req.getFirstType());
      switch (req.getKind()) {
      case RequirementKind::Conformance:
        Printer << " : ";
        break;

      case RequirementKind::SameType:
        Printer << " == ";
        break;

      case RequirementKind::WitnessMarker:
        llvm_unreachable("Handled above");
      }
      visit(req.getSecondType());
    }
    Printer << ">";
  }

  void visitGenericFunctionType(GenericFunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printGenericSignature(T->getGenericParams(), T->getRequirements());
    Printer << " ";
    printWithParensIfNotSimple(T->getInput());

    Printer << " -> ";
    T->getResult().print(Printer, Options);
  }

  void printCalleeConvention(ParameterConvention conv) {
    switch (conv) {
    case ParameterConvention::Direct_Unowned:
      return;
    case ParameterConvention::Direct_Owned:
      Printer << "@callee_owned ";
      return;
    case ParameterConvention::Direct_Guaranteed:
      Printer << "@callee_guaranteed ";
      return;
    case ParameterConvention::Direct_Deallocating:
      // Closures do not have destructors.
      llvm_unreachable("callee convention cannot be deallocating");
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_Out:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_In_Guaranteed:
      llvm_unreachable("callee convention cannot be indirect");
    }
    llvm_unreachable("bad convention");
  }

  void visitSILFunctionType(SILFunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printCalleeConvention(T->getCalleeConvention());
    if (auto sig = T->getGenericSignature()) {
      printGenericSignature(sig->getGenericParams(), sig->getRequirements());
      Printer << " ";
    }
    Printer << "(";
    bool first = true;
    for (auto param : T->getParameters()) {
      if (first) {
        first = false;
      } else {
        Printer << ", ";
      }
      param.print(Printer, Options);
    }
    Printer << ") -> ";

    T->getResult().print(Printer, Options);
  }
  
  void visitSILBlockStorageType(SILBlockStorageType *T) {
    Printer << "@block_storage ";
    printWithParensIfNotSimple(T->getCaptureType());
  }

  void visitArraySliceType(ArraySliceType *T) {
    Printer << "[";
    visit(T->getBaseType());
    Printer << "]";
  }

  void visitDictionaryType(DictionaryType *T) {
    Printer << "[";
    visit(T->getKeyType());
    Printer << " : ";
    visit(T->getValueType());
    Printer << "]";
  }

  void visitOptionalType(OptionalType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    Printer << "?";
  }

  void visitImplicitlyUnwrappedOptionalType(ImplicitlyUnwrappedOptionalType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    Printer <<  "!";
  }

  void visitProtocolType(ProtocolType *T) {
    printTypeDeclName(T);
  }

  void visitProtocolCompositionType(ProtocolCompositionType *T) {
    Printer << "protocol<";
    bool First = true;
    for (auto Proto : T->getProtocols()) {
      if (First)
        First = false;
      else
        Printer << ", ";
      visit(Proto);
    }
    Printer << ">";
  }

  void visitLValueType(LValueType *T) {
    Printer << "@lvalue ";
    visit(T->getObjectType());
  }

  void visitInOutType(InOutType *T) {
    Printer << "inout ";
    visit(T->getObjectType());
  }

  void visitArchetypeType(ArchetypeType *T) {
    if (auto existentialTy = T->getOpenedExistentialType()) {
      Printer << "@opened(\"" << T->getOpenedExistentialID() << "\") ";
      visit(existentialTy);
    } else {
      Printer << T->getFullName();
    }
  }

  GenericParamList *getGenericParamListAtDepth(unsigned depth) {
    assert(Options.ContextGenericParams);
    if (!UnwrappedGenericParams) {
      std::vector<GenericParamList *> paramLists;
      for (auto *params = Options.ContextGenericParams;
           params;
           params = params->getOuterParameters()) {
        paramLists.push_back(params);
      }
      UnwrappedGenericParams = std::move(paramLists);
    }
    return UnwrappedGenericParams->rbegin()[depth];
  }

  void visitGenericTypeParamType(GenericTypeParamType *T) {
    // Substitute a context archetype if we have context generic params.
    if (Options.ContextGenericParams) {
      return visit(getGenericParamListAtDepth(T->getDepth())
                     ->getPrimaryArchetypes()[T->getIndex()]);
    }

    auto Name = T->getName();
    if (Name.empty())
      Printer << "<anonymous>";
    else
      Printer.printName(Name);
  }

  void visitAssociatedTypeType(AssociatedTypeType *T) {
    auto Name = T->getDecl()->getName();
    if (Name.empty())
      Printer << "<anonymous>";
    else
      Printer.printName(Name);
  }

  void visitSubstitutedType(SubstitutedType *T) {
    visit(T->getReplacementType());
  }

  void visitDependentMemberType(DependentMemberType *T) {
    visit(T->getBase());
    Printer << ".";
    Printer.printName(T->getName());
  }

  void visitUnownedStorageType(UnownedStorageType *T) {
    if (Options.PrintStorageRepresentationAttrs)
      Printer << "@sil_unowned ";
    visit(T->getReferentType());
  }

  void visitUnmanagedStorageType(UnmanagedStorageType *T) {
    if (Options.PrintStorageRepresentationAttrs)
      Printer << "@sil_unmanaged ";
    visit(T->getReferentType());
  }

  void visitWeakStorageType(WeakStorageType *T) {
    if (Options.PrintStorageRepresentationAttrs)
      Printer << "@sil_weak ";
    visit(T->getReferentType());
  }

  void visitTypeVariableType(TypeVariableType *T) {
    auto Base = T->getBaseBeingSubstituted();
    
    if (T->getASTContext().LangOpts.DebugConstraintSolver) {
      Printer << "$T" << T->getID();
      return;
    }
    
    if (T->isEqual(Base) || T->isPrinting) {
      Printer << "_";
      return;
    }
    
    llvm::SaveAndRestore<bool> isPrinting(T->isPrinting, true);
    
    visit(Base);
  }
};
} // unnamed namespace

void Type::print(raw_ostream &OS, const PrintOptions &PO) const {
  StreamPrinter Printer(OS);
  print(Printer, PO);
}
void Type::print(ASTPrinter &Printer, const PrintOptions &PO) const {
  if (isNull())
    Printer << "<null>";
  else
    TypePrinter(Printer, PO).visit(*this);
}

void GenericSignature::print(raw_ostream &OS) const {
  StreamPrinter Printer(OS);
  TypePrinter(Printer, PrintOptions())
    .printGenericSignature(getGenericParams(), getRequirements());
}
void GenericSignature::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

std::string GenericSignature::getAsString() const {
  std::string result;
  llvm::raw_string_ostream out(result);
  print(out);
  return out.str();
}

static StringRef getStringForParameterConvention(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Indirect_In: return "@in ";
  case ParameterConvention::Indirect_Out: return "@out ";
  case ParameterConvention::Indirect_In_Guaranteed:  return "@in_guaranteed ";
  case ParameterConvention::Indirect_Inout: return "@inout ";
  case ParameterConvention::Direct_Owned: return "@owned ";
  case ParameterConvention::Direct_Unowned: return "";
  case ParameterConvention::Direct_Guaranteed: return "@guaranteed ";
  case ParameterConvention::Direct_Deallocating: return "@deallocating ";
  }
  llvm_unreachable("bad parameter convention");
}

StringRef swift::getCheckedCastKindName(CheckedCastKind kind) {
  switch (kind) {
  case CheckedCastKind::Unresolved:
    return "unresolved";
  case CheckedCastKind::Coercion:
    return "coercion";
  case CheckedCastKind::ValueCast:
    return "value_cast";
  case CheckedCastKind::ArrayDowncast:
    return "array_downcast";
  case CheckedCastKind::DictionaryDowncast:
    return "dictionary_downcast";
  case CheckedCastKind::DictionaryDowncastBridged:
    return "dictionary_downcast_bridged";
  case CheckedCastKind::SetDowncast:
    return "set_downcast";
  case CheckedCastKind::SetDowncastBridged:
    return "set_downcast_bridged";
  case CheckedCastKind::BridgeFromObjectiveC:
    return "bridge_from_objc";
  }
  llvm_unreachable("bad checked cast name");
}

void SILParameterInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void SILParameterInfo::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}
void SILParameterInfo::print(ASTPrinter &Printer,
                             const PrintOptions &Opts) const {
  Printer << getStringForParameterConvention(getConvention());
  getType().print(Printer, Opts);
}

static StringRef getStringForResultConvention(ResultConvention conv) {
  switch (conv) {
  case ResultConvention::Owned: return "@owned ";
  case ResultConvention::Unowned: return "";
  case ResultConvention::UnownedInnerPointer: return "@unowned_inner_pointer ";
  case ResultConvention::Autoreleased: return "@autoreleased ";
  }
  llvm_unreachable("bad result convention");
}

void SILResultInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void SILResultInfo::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}
void SILResultInfo::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  Printer << getStringForResultConvention(getConvention());
  getType().print(Printer, Opts);
}

std::string Type::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

std::string TypeBase::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

void TypeBase::dumpPrint() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void TypeBase::print(raw_ostream &OS, const PrintOptions &PO) const {
  Type(const_cast<TypeBase *>(this)).print(OS, PO);
}
void TypeBase::print(ASTPrinter &Printer, const PrintOptions &PO) const {
  Type(const_cast<TypeBase *>(this)).print(Printer, PO);
}


void ProtocolConformance::printName(llvm::raw_ostream &os,
                                    const PrintOptions &PO) const {
  if (PO.PrintForSIL) {
    if (auto genericSig = getGenericSignature()) {
      StreamPrinter sPrinter(os);
      TypePrinter typePrinter(sPrinter, PO);
      typePrinter.printGenericSignature(genericSig->getGenericParams(),
                                        genericSig->getRequirements());
      os << ' ';
    }
  } else if (auto gp = getGenericParams()) {
    StreamPrinter SPrinter(os);
    PrintAST Printer(SPrinter, PO);
    Printer.printGenericParams(gp);
    os << ' ';
  }
 
  getType()->print(os, PO);
  os << ": ";
 
  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto normal = cast<NormalProtocolConformance>(this);
    os << normal->getProtocol()->getName()
       << " module " << normal->getDeclContext()->getParentModule()->Name;
    break;
  }
  case ProtocolConformanceKind::Specialized: {
    auto spec = cast<SpecializedProtocolConformance>(this);
    os << "specialize <";
    interleave(spec->getGenericSubstitutions(),
               [&](const Substitution &s) { s.print(os, PO); },
               [&] { os << ", "; });
    os << "> (";
    spec->getGenericConformance()->printName(os);
    os << ")";
    break;
  }
  case ProtocolConformanceKind::Inherited: {
    auto inherited = cast<InheritedProtocolConformance>(this);
    os << "inherit (";
    inherited->getInheritedConformance()->printName(os);
    os << ")";
    break;
  }
  }
}
