//===--- ASTDumper.cpp - Swift Language AST Dumper ------------------------===//
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
//
//  This file implements dumping for the Swift ASTs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ForeignAsyncConvention.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/QuotedString.h"
#include "swift/Basic/STLExtras.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

struct TerminalColor {
  llvm::raw_ostream::Colors Color;
  bool Bold;
};

#define DEF_COLOR(NAME, COLOR, BOLD) \
static const TerminalColor NAME##Color = { llvm::raw_ostream::COLOR, BOLD };

DEF_COLOR(Func, YELLOW, false)
DEF_COLOR(Range, YELLOW, false)
DEF_COLOR(AccessLevel, YELLOW, false)
DEF_COLOR(ASTNode, YELLOW, true)
DEF_COLOR(Parameter, YELLOW, false)
DEF_COLOR(Extension, MAGENTA, false)
DEF_COLOR(Pattern, RED, true)
DEF_COLOR(Override, RED, false)
DEF_COLOR(Stmt, RED, true)
DEF_COLOR(Captures, RED, false)
DEF_COLOR(Arguments, RED, false)
DEF_COLOR(TypeRepr, GREEN, false)
DEF_COLOR(LiteralValue, GREEN, false)
DEF_COLOR(Decl, GREEN, true)
DEF_COLOR(Parenthesis, BLUE, false)
DEF_COLOR(Type, BLUE, false)
DEF_COLOR(Discriminator, BLUE, false)
DEF_COLOR(InterfaceType, GREEN, false)
DEF_COLOR(Identifier, GREEN, false)
DEF_COLOR(Expr, MAGENTA, true)
DEF_COLOR(ExprModifier, CYAN, false)
DEF_COLOR(DeclModifier, CYAN, false)
DEF_COLOR(ClosureModifier, CYAN, false)
DEF_COLOR(TypeField, CYAN, false)
DEF_COLOR(Location, CYAN, false)
DEF_COLOR(Label, WHITE, true)

#undef DEF_COLOR

namespace {
  /// RAII object that prints with the given color, if color is supported on the
  /// given stream.
  class PrintWithColorRAII {
    raw_ostream &OS;
    bool ShowColors;

  public:
    PrintWithColorRAII(raw_ostream &os, TerminalColor color)
    : OS(os), ShowColors(false)
    {
      ShowColors = os.has_colors();

      if (ShowColors)
        OS.changeColor(color.Color, color.Bold);
    }

    ~PrintWithColorRAII() {
      if (ShowColors) {
        OS.resetColor();
      }
    }

    raw_ostream &getOS() const { return OS; }

    template<typename T>
    friend raw_ostream &operator<<(PrintWithColorRAII &&printer,
                                   const T &value){
      printer.OS << value;
      return printer.OS;
    }

  };

/// Contains helpers for looking up types in expressions. Used by PrintExpr,
/// but threaded through the other printers too.
struct ExprTypeDelegate {
  llvm::function_ref<Type(Expr *)> GetTypeOfExpr;
  llvm::function_ref<Type(TypeRepr *)> GetTypeOfTypeRepr;
  llvm::function_ref<Type(KeyPathExpr *E, unsigned index)>
      GetTypeOfKeyPathComponent;

  ExprTypeDelegate(llvm::function_ref<Type(Expr *)> getTypeOfExpr,
                   llvm::function_ref<Type(TypeRepr *)> getTypeOfTypeRepr,
                   llvm::function_ref<Type(KeyPathExpr *E, unsigned index)>
                       getTypeOfKeyPathComponent)
    : GetTypeOfExpr(getTypeOfExpr), GetTypeOfTypeRepr(getTypeOfTypeRepr),
      GetTypeOfKeyPathComponent(getTypeOfKeyPathComponent) { }

  ExprTypeDelegate()
    : ExprTypeDelegate([](Expr *E) -> Type { return E->getType(); }, nullptr,
                       [](KeyPathExpr *E, unsigned index) -> Type {
                         return E->getComponents()[index].getComponentType();
                       }) { }
};
} // end anonymous namespace

static void printGenericParameters(raw_ostream &OS, GenericParamList *Params) {
  if (!Params)
    return;
  OS << ' ';
  Params->print(OS);
}


static StringRef
getSILFunctionTypeRepresentationString(SILFunctionType::Representation value) {
  switch (value) {
  case SILFunctionType::Representation::Thick: return "thick";
  case SILFunctionType::Representation::Block: return "block";
  case SILFunctionType::Representation::CFunctionPointer: return "c";
  case SILFunctionType::Representation::Thin: return "thin";
  case SILFunctionType::Representation::Method: return "method";
  case SILFunctionType::Representation::ObjCMethod: return "objc_method";
  case SILFunctionType::Representation::WitnessMethod: return "witness_method";
  case SILFunctionType::Representation::Closure: return "closure";
  }

  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}

StringRef swift::getReadImplKindName(ReadImplKind kind) {
  switch (kind) {
  case ReadImplKind::Stored:
    return "stored";
  case ReadImplKind::Inherited:
    return "inherited";
  case ReadImplKind::Get:
    return "getter";
  case ReadImplKind::Address:
    return "addressor";
  case ReadImplKind::Read:
    return "read_coroutine";
  }
  llvm_unreachable("bad kind");
}

StringRef swift::getWriteImplKindName(WriteImplKind kind) {
  switch (kind) {
  case WriteImplKind::Immutable:
    return "immutable";
  case WriteImplKind::Stored:
    return "stored";
  case WriteImplKind::StoredWithObservers:
    return "stored_with_observers";
  case WriteImplKind::InheritedWithObservers:
    return "inherited_with_observers";
  case WriteImplKind::Set:
    return "setter";
  case WriteImplKind::MutableAddress:
    return "mutable_addressor";
  case WriteImplKind::Modify:
    return "modify_coroutine";
  }
  llvm_unreachable("bad kind");
}

StringRef swift::getReadWriteImplKindName(ReadWriteImplKind kind) {
  switch (kind) {
  case ReadWriteImplKind::Immutable:
    return "immutable";
  case ReadWriteImplKind::Stored:
    return "stored";
  case ReadWriteImplKind::MutableAddress:
    return "mutable_addressor";
  case ReadWriteImplKind::MaterializeToTemporary:
    return "materialize_to_temporary";
  case ReadWriteImplKind::Modify:
    return "modify_coroutine";
  case ReadWriteImplKind::StoredWithDidSet:
    return "stored_with_didset";
  case ReadWriteImplKind::InheritedWithDidSet:
    return "inherited_with_didset";
  }
  llvm_unreachable("bad kind");
}

static StringRef getImportKindString(ImportKind value) {
  switch (value) {
  case ImportKind::Module: return "module";
  case ImportKind::Type: return "type";
  case ImportKind::Struct: return "struct";
  case ImportKind::Class: return "class";
  case ImportKind::Enum: return "enum";
  case ImportKind::Protocol: return "protocol";
  case ImportKind::Var: return "var";
  case ImportKind::Func: return "func";
  }
  
  llvm_unreachable("Unhandled ImportKind in switch.");
}

static StringRef
getForeignErrorConventionKindString(ForeignErrorConvention::Kind value) {
  switch (value) {
  case ForeignErrorConvention::ZeroResult: return "ZeroResult";
  case ForeignErrorConvention::NonZeroResult: return "NonZeroResult";
  case ForeignErrorConvention::ZeroPreservedResult: return "ZeroPreservedResult";
  case ForeignErrorConvention::NilResult: return "NilResult";
  case ForeignErrorConvention::NonNilError: return "NonNilError";
  }

  llvm_unreachable("Unhandled ForeignErrorConvention in switch.");
}
static StringRef getDefaultArgumentKindString(DefaultArgumentKind value) {
  switch (value) {
    case DefaultArgumentKind::None: return "none";
#define MAGIC_IDENTIFIER(NAME, STRING, SYNTAX_KIND) \
    case DefaultArgumentKind::NAME: return STRING;
#include "swift/AST/MagicIdentifierKinds.def"
    case DefaultArgumentKind::Inherited: return "inherited";
    case DefaultArgumentKind::NilLiteral: return "nil";
    case DefaultArgumentKind::EmptyArray: return "[]";
    case DefaultArgumentKind::EmptyDictionary: return "[:]";
    case DefaultArgumentKind::Normal: return "normal";
    case DefaultArgumentKind::StoredProperty: return "stored property";
  }

  llvm_unreachable("Unhandled DefaultArgumentKind in switch.");
}
static StringRef
getObjCSelectorExprKindString(ObjCSelectorExpr::ObjCSelectorKind value) {
  switch (value) {
    case ObjCSelectorExpr::Method: return "method";
    case ObjCSelectorExpr::Getter: return "getter";
    case ObjCSelectorExpr::Setter: return "setter";
  }

  llvm_unreachable("Unhandled ObjCSelectorExpr in switch.");
}
static StringRef getAccessSemanticsString(AccessSemantics value) {
  switch (value) {
    case AccessSemantics::Ordinary: return "ordinary";
    case AccessSemantics::DirectToStorage: return "direct_to_storage";
    case AccessSemantics::DirectToImplementation: return "direct_to_impl";
  }

  llvm_unreachable("Unhandled AccessSemantics in switch.");
}
static StringRef getMetatypeRepresentationString(MetatypeRepresentation value) {
  switch (value) {
    case MetatypeRepresentation::Thin: return "thin";
    case MetatypeRepresentation::Thick: return "thick";
    case MetatypeRepresentation::ObjC: return "@objc";
  }

  llvm_unreachable("Unhandled MetatypeRepresentation in switch.");
}
static StringRef
getStringLiteralExprEncodingString(StringLiteralExpr::Encoding value) {
  switch (value) {
    case StringLiteralExpr::UTF8: return "utf8";
    case StringLiteralExpr::OneUnicodeScalar: return "unicodeScalar";
  }

  llvm_unreachable("Unhandled StringLiteral in switch.");
}
static StringRef getCtorInitializerKindString(CtorInitializerKind value) {
  switch (value) {
    case CtorInitializerKind::Designated: return "designated";
    case CtorInitializerKind::Convenience: return "convenience";
    case CtorInitializerKind::ConvenienceFactory: return "convenience_factory";
    case CtorInitializerKind::Factory: return "factory";
  }

  llvm_unreachable("Unhandled CtorInitializerKind in switch.");
}
static StringRef getAssociativityString(Associativity value) {
  switch (value) {
    case Associativity::None: return "none";
    case Associativity::Left: return "left";
    case Associativity::Right: return "right";
  }

  llvm_unreachable("Unhandled Associativity in switch.");
}

//===----------------------------------------------------------------------===//
//  Decl printing.
//===----------------------------------------------------------------------===//

// Print a name.
static void printName(raw_ostream &os, DeclName name) {
  if (!name)
    os << "<anonymous>";
  else
    os << name;
}

static void dumpSubstitutionMapRec(
    SubstitutionMap map, llvm::raw_ostream &out,
    SubstitutionMap::DumpStyle style, unsigned indent,
    llvm::SmallPtrSetImpl<const ProtocolConformance *> &visited);

namespace {
  class PrintPattern : public PatternVisitor<PrintPattern, void, StringRef> {
  public:
    ASTContext *Ctx;
    raw_ostream &OS;
    ExprTypeDelegate &Delegate;
    unsigned Indent;

    explicit PrintPattern(ASTContext *Ctx, raw_ostream &os,
                          ExprTypeDelegate &delegate, unsigned indent = 0)
      : Ctx(Ctx), OS(os), Delegate(delegate), Indent(indent) { }

    void printRec(Decl *D, StringRef Label = "");
    void printRec(Expr *E, StringRef Label = "");
    void printRec(Stmt *S, StringRef Label = "");
    void printRec(TypeRepr *T, StringRef Label = "");
    void printRec(const Pattern *P, StringRef Label = "");

    void printCommon(const char *name, StringRef label, TerminalColor Color) {
      OS.indent(Indent);
      PrintWithColorRAII(OS, ParenthesisColor) << '(';
      PrintWithColorRAII(OS, LabelColor) << label;
      if (!label.empty()) {  // not null/empty
        OS << "=";
      }

      PrintWithColorRAII(OS, Color) << name;
    }

    raw_ostream &printCommon(Pattern *P, const char *Name, StringRef Label) {
      printCommon(Name, Label, PatternColor);

      if (P->isImplicit())
        PrintWithColorRAII(OS, ExprModifierColor) << " implicit";

      if (P->hasType()) {
        PrintWithColorRAII(OS, TypeColor) << " type='";
        P->getType().print(PrintWithColorRAII(OS, TypeColor).getOS());
        PrintWithColorRAII(OS, TypeColor) << "'";
      }
      return OS;
    }

    void printCommonPost() {
      PrintWithColorRAII(OS, ParenthesisColor) << ')';
    }

    void visitParenPattern(ParenPattern *P, StringRef Label) {
      printCommon(P, "pattern_paren", Label);
      OS << '\n';
      printRec(P->getSubPattern());
      printCommonPost();
    }
    void visitTuplePattern(TuplePattern *P, StringRef Label) {
      printCommon(P, "pattern_tuple", Label);

      OS << " names=";
      interleave(P->getElements(),
                 [&](const TuplePatternElt &elt) {
                   auto name = elt.getLabel();
                   OS << (name.empty() ? "''" : name.str());
                 },
                 [&] { OS << ","; });

      for (auto &elt : P->getElements()) {
        OS << '\n';
        printRec(elt.getPattern());
      }
      printCommonPost();
    }
    void visitNamedPattern(NamedPattern *P, StringRef Label) {
      printCommon(P, "pattern_named", Label);
      PrintWithColorRAII(OS, IdentifierColor) << " '" << P->getNameStr() << "'";
      printCommonPost();
    }
    void visitAnyPattern(AnyPattern *P, StringRef Label) {
      printCommon(P, "pattern_any", Label);
      printCommonPost();
    }
    void visitTypedPattern(TypedPattern *P, StringRef Label) {
      printCommon(P, "pattern_typed", Label);
      OS << '\n';
      printRec(P->getSubPattern());
      if (auto *repr = P->getTypeRepr()) {
        OS << '\n';
        printRec(repr);
      }
      printCommonPost();
    }

    void visitIsPattern(IsPattern *P, StringRef Label) {
      printCommon(P, "pattern_is", Label);
      OS << ' ' << getCheckedCastKindName(P->getCastKind()) << ' ';
      P->getCastType().print(OS);
      if (auto sub = P->getSubPattern()) {
        OS << '\n';
        printRec(sub);
      }
      printCommonPost();
    }
    void visitExprPattern(ExprPattern *P, StringRef Label) {
      printCommon(P, "pattern_expr", Label);
      OS << '\n';
      if (auto m = P->getMatchExpr())
        printRec(m);
      else
        printRec(P->getSubExpr());
      printCommonPost();
    }
    void visitBindingPattern(BindingPattern *P, StringRef Label) {
      printCommon(P, P->isLet() ? "pattern_let" : "pattern_var", Label);
      OS << '\n';
      printRec(P->getSubPattern());
      printCommonPost();
    }
    void visitEnumElementPattern(EnumElementPattern *P, StringRef Label) {
      printCommon(P, "pattern_enum_element", Label);
      OS << ' ';
      P->getParentType().print(PrintWithColorRAII(OS, TypeColor).getOS());
      PrintWithColorRAII(OS, IdentifierColor) << '.' << P->getName();
      if (P->hasSubPattern()) {
        OS << '\n';
        printRec(P->getSubPattern());
      }
      printCommonPost();
    }
    void visitOptionalSomePattern(OptionalSomePattern *P, StringRef Label) {
      printCommon(P, "pattern_optional_some", Label);
      OS << '\n';
      printRec(P->getSubPattern());
      printCommonPost();
    }
    void visitBoolPattern(BoolPattern *P, StringRef Label) {
      printCommon(P, "pattern_bool", Label);
      OS << (P->getValue() ? " true" : " false");
      printCommonPost();
    }

  };

  /// PrintDecl - Visitor implementation of Decl::print.
  class PrintDecl : public DeclVisitor<PrintDecl, void, StringRef> {
  public:
    ASTContext *Ctx;
    raw_ostream &OS;
    ExprTypeDelegate &Delegate;
    unsigned Indent;

    explicit PrintDecl(ASTContext *Ctx, raw_ostream &os,
                       ExprTypeDelegate &delegate, unsigned indent = 0)
      : Ctx(Ctx), OS(os), Delegate(delegate), Indent(indent) { }

  private:
    void printRec(Decl *D, StringRef Label = "");
    void printRec(Expr *E, StringRef Label = "");
    void printRec(Stmt *S, StringRef Label = "");
    void printRec(Pattern *P, StringRef Label = "");
    void printRec(TypeRepr *T, StringRef Label = "");

    void printWhereRequirements(
        PointerUnion<const AssociatedTypeDecl *, const GenericContext *> Owner)
        const {
      const auto printWhere = [&](const TrailingWhereClause *Where) {
        if (Where) {
          OS << " where requirements: ";
          Where->print(OS, /*printWhereKeyword*/ false);
        }
      };

      if (const auto GC = Owner.dyn_cast<const GenericContext *>()) {
        printWhere(GC->getTrailingWhereClause());
      } else {
        const auto ATD = Owner.get<const AssociatedTypeDecl *>();
        printWhere(ATD->getTrailingWhereClause());
      }
    }

    void printLabel(StringRef label, TerminalColor Color = DeclModifierColor) {
      OS << " ";
      PrintWithColorRAII(OS, Color) << label;
      if (!label.empty())
        PrintWithColorRAII(OS, Color) << "=";
    }

    // Print a field with a value.
    template<typename T>
    void print(const T &value, StringRef label = "") {
      printLabel(label);
      OS << value;
    }

    // Print a single flag.
    void printFlag(StringRef label) {
      PrintWithColorRAII(OS, DeclModifierColor) << " " << label;
    }

    // Print a single flag if it is set.
    void printFlag(bool isSet, StringRef label) {
      if (isSet)
        printFlag(label);
    }

    void printCommon(const char *name, StringRef label, TerminalColor Color) {
      OS.indent(Indent);
      PrintWithColorRAII(OS, ParenthesisColor) << '(';
      PrintWithColorRAII(OS, LabelColor) << label;
      if (!label.empty()) {  // not null/empty
        OS << "=";
      }

      PrintWithColorRAII(OS, Color) << name;
    }

    void printCommon(Decl *D, const char *Name, StringRef Label,
                     TerminalColor Color = DeclColor) {
      printCommon(Name, Label, Color);

      if (D->isImplicit())
        PrintWithColorRAII(OS, DeclModifierColor) << " implicit";

      if (D->isHoisted())
        PrintWithColorRAII(OS, DeclModifierColor) << " hoisted";

      auto R = D->getSourceRange();
      if (Ctx && R.isValid()) {
        PrintWithColorRAII(OS, RangeColor) << " range=";
        R.print(PrintWithColorRAII(OS, RangeColor).getOS(),
                Ctx->SourceMgr, /*PrintText=*/false);
      }

      if (D->TrailingSemiLoc.isValid())
        PrintWithColorRAII(OS, DeclModifierColor) << " trailing_semi";
    }

    void printCommonPost() {
      PrintWithColorRAII(OS, ParenthesisColor) << ')';
    }

    void printInherited(ArrayRef<TypeLoc> Inherited) {
      if (Inherited.empty())
        return;
      OS << " inherits: ";
      interleave(Inherited, [&](TypeLoc Super) { Super.getType().print(OS); },
                 [&] { OS << ", "; });
    }

  public:
    void visitImportDecl(ImportDecl *ID, StringRef Label) {
      printCommon(ID, "import_decl", Label);

      printFlag(ID->isExported(), "exported");

      if (ID->getImportKind() != ImportKind::Module)
        print(getImportKindString(ID->getImportKind()), "kind");

      SmallString<64> scratch;
      ID->getImportPath().getString(scratch);
      print(QuotedString(scratch));

      printCommonPost();
    }

    void visitExtensionDecl(ExtensionDecl *ED, StringRef Label) {
      printCommon(ED, "extension_decl", Label, ExtensionColor);
      OS << ' ';
      if (ED->hasBeenBound())
        ED->getExtendedType().print(OS);
      else
        ED->getExtendedTypeRepr()->print(OS);
      printCommonPost(ED);
    }

    void printDeclName(DeclName name, StringRef label = "") {
      printLabel(label, IdentifierColor);
      PrintWithColorRAII(OS, IdentifierColor)
        << '\"' << name << '\"';
    }

    void printDeclName(const ValueDecl *D, StringRef label = "") {
      if (D->getName()) {
        printDeclName(D->getName(), label);
      } else {
        printLabel(label);
        PrintWithColorRAII(OS, IdentifierColor)
          << "'anonymous @ " << (const void*)D << '\'';
      }
    }

    void visitTypeAliasDecl(TypeAliasDecl *TAD, StringRef Label) {
      printCommon(TAD, "typealias", Label);
      PrintWithColorRAII(OS, TypeColor) << " type=";
      if (auto underlying = TAD->getCachedUnderlyingType()) {
        PrintWithColorRAII(OS, TypeColor)
          << "'" << underlying.getString() << "'";
      } else {
        PrintWithColorRAII(OS, TypeColor) << "<<<unresolved>>>";
      }
      printWhereRequirements(TAD);
      printCommonPost();
    }

    void visitOpaqueTypeDecl(OpaqueTypeDecl *OTD, StringRef Label) {
      printCommon(OTD, "opaque_type", Label);
      printDeclName(OTD->getNamingDecl(), "naming_decl");
      PrintWithColorRAII(OS, TypeColor) << " opaque_interface="
        << Type(OTD->getUnderlyingInterfaceType()).getString();
      OS << " in "
         << OTD->getOpaqueInterfaceGenericSignature()->getAsString();
      if (auto underlyingSubs = OTD->getUnderlyingTypeSubstitutions()) {
        OS << " underlying:\n";
        SmallPtrSet<const ProtocolConformance *, 4> Dumped;
        dumpSubstitutionMapRec(*underlyingSubs, OS,
                               SubstitutionMap::DumpStyle::Full,
                               Indent + 2, Dumped);
      }
      printCommonPost();
    }

    void printAbstractTypeParamCommon(AbstractTypeParamDecl *decl,
                                      const char *name, StringRef Label) {
      printCommon(decl, name, Label);
      if (decl->getDeclContext()->getGenericEnvironmentOfContext()) {
        if (auto superclassTy = decl->getSuperclass()) {
          OS << " superclass='" << superclassTy->getString() << "'";
        }
      }
    }

    void visitGenericTypeParamDecl(GenericTypeParamDecl *decl, StringRef Label) {
      printAbstractTypeParamCommon(decl, "generic_type_param", Label);
      OS << " depth=" << decl->getDepth() << " index=" << decl->getIndex();
      printCommonPost();
    }

    void visitAssociatedTypeDecl(AssociatedTypeDecl *decl, StringRef Label) {
      printAbstractTypeParamCommon(decl, "associated_type_decl", Label);
      if (auto defaultDef = decl->getDefaultDefinitionType()) {
        OS << " default=";
        defaultDef.print(OS);
      }
      printWhereRequirements(decl);
      if (decl->overriddenDeclsComputed()) {
        OS << " overridden=";
        interleave(decl->getOverriddenDecls(),
                   [&](AssociatedTypeDecl *overridden) {
                     OS << overridden->getProtocol()->getName();
                   }, [&]() {
                     OS << ", ";
                   });
      }

      printCommonPost();
    }

    void visitProtocolDecl(ProtocolDecl *PD, StringRef Label) {
      printCommon(PD, "protocol", Label);

      OS << " requirement signature=";
      if (PD->isRequirementSignatureComputed()) {
        OS << GenericSignature::get({PD->getProtocolSelfType()} ,
                                    PD->getRequirementSignature())
                ->getAsString();
      } else {
        OS << "<null>";
      }
      printCommonPost(PD);
    }

    void printCommon(ValueDecl *VD, const char *Name, StringRef Label,
                     TerminalColor Color = DeclColor) {
      printCommon((Decl*)VD, Name, Label, Color);

      OS << ' ';
      printDeclName(VD);
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(VD))
        printGenericParameters(OS, AFD->getParsedGenericParams());
      if (auto *GTD = dyn_cast<GenericTypeDecl>(VD))
        printGenericParameters(OS, GTD->getParsedGenericParams());

      if (auto *var = dyn_cast<VarDecl>(VD)) {
        PrintWithColorRAII(OS, TypeColor) << " type='";
        if (var->hasInterfaceType())
          var->getType().print(PrintWithColorRAII(OS, TypeColor).getOS());
        else
          PrintWithColorRAII(OS, TypeColor) << "<null type>";
        PrintWithColorRAII(OS, TypeColor) << "'";
      }

      if (VD->hasInterfaceType()) {
        PrintWithColorRAII(OS, InterfaceTypeColor) << " interface type='";
        VD->getInterfaceType()->print(
            PrintWithColorRAII(OS, InterfaceTypeColor).getOS());
        PrintWithColorRAII(OS, InterfaceTypeColor) << "'";
      }

      if (VD->hasAccess()) {
        PrintWithColorRAII(OS, AccessLevelColor) << " access="
          << getAccessLevelSpelling(VD->getFormalAccess());
      }

      if (VD->overriddenDeclsComputed()) {
        auto overridden = VD->getOverriddenDecls();
        if (!overridden.empty()) {
          PrintWithColorRAII(OS, OverrideColor) << " override=";
          interleave(overridden,
                     [&](ValueDecl *overridden) {
                       overridden->dumpRef(
                                PrintWithColorRAII(OS, OverrideColor).getOS());
                     }, [&]() {
                       OS << ", ";
                     });
        }
      }

      auto VarD = dyn_cast<VarDecl>(VD);
      const auto &attrs = VD->getAttrs();
      printFlag(attrs.hasAttribute<FinalAttr>() && !(VarD && VarD->isLet()),
                "final");
      printFlag(attrs.hasAttribute<ObjCAttr>(), "@objc");
      printFlag(attrs.hasAttribute<DynamicAttr>(), " dynamic");
      if (auto *attr = attrs.getAttribute<DynamicReplacementAttr>()) {
        OS << " @_dynamicReplacement(for: \"";
        OS << attr->getReplacedFunctionName();
        OS << "\")";
      }
    }

    void printCommon(NominalTypeDecl *NTD, const char *Name, StringRef Label,
                     TerminalColor Color = DeclColor) {
      printCommon((ValueDecl *)NTD, Name, Label, Color);

      if (NTD->hasInterfaceType()) {
        if (NTD->isResilient())
          printFlag("resilient");
        else
          printFlag("non-resilient");
      }
    }

    void printCommonPost(const IterableDeclContext *IDC) {
      switch (IDC->getIterableContextKind()) {
      case IterableDeclContextKind::NominalTypeDecl: {
        const auto NTD = cast<NominalTypeDecl>(IDC);
        printInherited(NTD->getInherited());
        printWhereRequirements(NTD);
        break;
      }
      case IterableDeclContextKind::ExtensionDecl:
        const auto ED = cast<ExtensionDecl>(IDC);
        printInherited(ED->getInherited());
        printWhereRequirements(ED);
        break;
      }

      for (Decl *D : IDC->getMembers()) {
        OS << '\n';
        printRec(D);
      }
      printCommonPost();
    }

    void visitSourceFile(const SourceFile &SF, StringRef Label) {
      printCommon("source_file", Label, ASTNodeColor);
      PrintWithColorRAII(OS, LocationColor) << " \"" << SF.getFilename() << '"';

      if (auto decls = SF.getCachedTopLevelDecls()) {
        for (Decl *D : *decls) {
          if (D->isImplicit())
            continue;

          OS << '\n';
          printRec(D);
        }
      }
      printCommonPost();
    }

    void visitVarDecl(VarDecl *VD, StringRef Label) {
      printCommon(VD, "var_decl", Label);
      if (VD->isLet())
        PrintWithColorRAII(OS, DeclModifierColor) << " let";
      if (VD->getAttrs().hasAttribute<LazyAttr>())
        PrintWithColorRAII(OS, DeclModifierColor) << " lazy";
      printStorageImpl(VD);
      printAccessors(VD);
      printCommonPost();
    }

    void printStorageImpl(AbstractStorageDecl *D) {
      if (D->isStatic())
        PrintWithColorRAII(OS, DeclModifierColor) << " type";

      if (D->hasInterfaceType()) {
        auto impl = D->getImplInfo();
        PrintWithColorRAII(OS, DeclModifierColor)
          << " readImpl="
          << getReadImplKindName(impl.getReadImpl());
        if (!impl.supportsMutation()) {
          PrintWithColorRAII(OS, DeclModifierColor)
            << " immutable";
        } else {
          PrintWithColorRAII(OS, DeclModifierColor)
            << " writeImpl="
            << getWriteImplKindName(impl.getWriteImpl());
          PrintWithColorRAII(OS, DeclModifierColor)
            << " readWriteImpl="
            << getReadWriteImplKindName(impl.getReadWriteImpl());
        }
      }
    }

    void printAccessors(AbstractStorageDecl *D) {
      for (auto accessor : D->getAllAccessors()) {
        OS << "\n";
        printRec(accessor);
      }
    }

    void visitParamDecl(ParamDecl *PD, StringRef Label) {
      printParameter(PD, Label);
    }

    void visitEnumCaseDecl(EnumCaseDecl *ECD, StringRef Label) {
      printCommon(ECD, "enum_case_decl", Label);
      for (EnumElementDecl *D : ECD->getElements()) {
        OS << '\n';
        printRec(D);
      }
      printCommonPost();
    }

    void visitEnumDecl(EnumDecl *ED, StringRef Label) {
      printCommon(ED, "enum_decl", Label);
      printCommonPost(ED);
    }

    void visitEnumElementDecl(EnumElementDecl *EED, StringRef Label) {
      printCommon(EED, "enum_element_decl", Label);
      if (auto *paramList = EED->getParameterList()) {
        Indent += 2;
        OS << "\n";
        printParameterList(paramList);
        Indent -= 2;
      }
      printCommonPost();
    }

    void visitStructDecl(StructDecl *SD, StringRef Label) {
      printCommon(SD, "struct_decl", Label);
      printCommonPost(SD);
    }

    void visitClassDecl(ClassDecl *CD, StringRef Label) {
      printCommon(CD, "class_decl", Label);
      if (CD->getAttrs().hasAttribute<StaticInitializeObjCMetadataAttr>())
        OS << " @_staticInitializeObjCMetadata";
      printCommonPost(CD);
    }

    void visitPatternBindingDecl(PatternBindingDecl *PBD, StringRef Label) {
      printCommon(PBD, "pattern_binding_decl", Label);

      for (auto idx : range(PBD->getNumPatternEntries())) {
        OS << '\n';
        printRec(PBD->getPattern(idx));
        if (PBD->getOriginalInit(idx)) {
          OS << '\n';
          OS.indent(Indent + 2);
          OS << "Original init:\n";
          printRec(PBD->getOriginalInit(idx));
        }
        if (PBD->getInit(idx)) {
          OS << '\n';
          OS.indent(Indent + 2);
          OS << "Processed init:\n";
          printRec(PBD->getInit(idx));
        }
      }
      printCommonPost();
    }

    void visitSubscriptDecl(SubscriptDecl *SD, StringRef Label) {
      printCommon(SD, "subscript_decl", Label);
      printStorageImpl(SD);
      printAccessors(SD);
      printCommonPost();
    }

    void printCommonAFD(AbstractFunctionDecl *D, const char *Type,
                        StringRef Label) {
      printCommon(D, Type, Label, FuncColor);
      if (!D->getCaptureInfo().isTrivial()) {
        OS << " ";
        D->getCaptureInfo().print(OS);
      }

      if (auto fac = D->getForeignAsyncConvention()) {
        OS << " foreign_async=";
        if (auto type = fac->completionHandlerType())
          type.print(OS);
        OS << ",completion_handler_param="
           << fac->completionHandlerParamIndex();
        if (auto errorParamIndex = fac->completionHandlerErrorParamIndex())
          OS << ",error_param=" << *errorParamIndex;
      }

      if (auto fec = D->getForeignErrorConvention()) {
        OS << " foreign_error=";
        OS << getForeignErrorConventionKindString(fec->getKind());
        bool wantResultType = (
          fec->getKind() == ForeignErrorConvention::ZeroResult ||
          fec->getKind() == ForeignErrorConvention::NonZeroResult);

        OS << ((fec->isErrorOwned() == ForeignErrorConvention::IsOwned)
                ? ",owned"
                : ",unowned");
        OS << ",param=" << llvm::utostr(fec->getErrorParameterIndex());
        OS << ",paramtype=" << fec->getErrorParameterType().getString();
        if (wantResultType)
          OS << ",resulttype=" << fec->getResultType().getString();
      }
    }

    void printParameter(const ParamDecl *P, StringRef Label) {
      printCommon("parameter", Label, ParameterColor);
      OS << " ";
      printDeclName(P);
      if (!P->getArgumentName().empty())
        PrintWithColorRAII(OS, IdentifierColor)
          << " apiName=" << P->getArgumentName();

      if (P->hasInterfaceType()) {
        PrintWithColorRAII(OS, TypeColor) << " type='";
        P->getType().print(PrintWithColorRAII(OS, TypeColor).getOS());
        PrintWithColorRAII(OS, TypeColor) << "'";

        PrintWithColorRAII(OS, InterfaceTypeColor) << " interface type='";
        P->getInterfaceType().print(
            PrintWithColorRAII(OS, InterfaceTypeColor).getOS());
        PrintWithColorRAII(OS, InterfaceTypeColor) << "'";
      }

      if (auto specifier = P->getCachedSpecifier()) {
        switch (*specifier) {
        case ParamDecl::Specifier::Default:
          /* nothing */
          break;
        case ParamDecl::Specifier::InOut:
          OS << " inout";
          break;
        case ParamDecl::Specifier::Shared:
          OS << " shared";
          break;
        case ParamDecl::Specifier::Owned:
          OS << " owned";
          break;
        }
      }

      printFlag(P->isVariadic(), "variadic");
      printFlag(P->isAutoClosure(), "autoclosure");
      printFlag(P->getAttrs().hasAttribute<NonEphemeralAttr>(), "nonEphemeral");

      if (P->getDefaultArgumentKind() != DefaultArgumentKind::None) {
        print(getDefaultArgumentKindString(P->getDefaultArgumentKind()),
              "default_arg");
      }

      if (P->hasDefaultExpr() &&
          !P->getDefaultArgumentCaptureInfo().isTrivial()) {
        OS << " ";
        P->getDefaultArgumentCaptureInfo().print(
          PrintWithColorRAII(OS, CapturesColor).getOS());
      }

      if (auto init = P->getStructuralDefaultExpr()) {
        OS << '\n';
        printRec(init, "expression");
      }

      printCommonPost();
    }

    void printParameterList(const ParameterList *params) {
      printCommon("parameter_list", "", ParameterColor);

      if (!Ctx && params->size() != 0 && params->get(0))
        Ctx = &params->get(0)->getASTContext();

      if (Ctx) {
        auto R = params->getSourceRange();
        if (R.isValid()) {
          PrintWithColorRAII(OS, RangeColor) << " range=";
          R.print(PrintWithColorRAII(OS, RangeColor).getOS(),
                  Ctx->SourceMgr, /*PrintText=*/false);
        }
      }

      Indent += 2;
      for (auto P : *params) {
        OS << '\n';
        printParameter(P, "");
      }
      Indent -= 2;

      printCommonPost();
    }

    void printAbstractFunctionDecl(AbstractFunctionDecl *D) {
      Indent += 2;
      if (auto *P = D->getImplicitSelfDecl()) {
        OS << '\n';
        printParameter(P, "self");
      }

      OS << '\n';
      printParameterList(D->getParameters());
      Indent -= 2;

      if (auto FD = dyn_cast<FuncDecl>(D)) {
        if (FD->getResultTypeRepr()) {
          OS << '\n';
          printRec(FD->getResultTypeRepr(), "result");
          if (auto opaque = FD->getOpaqueResultTypeDecl()) {
            OS << '\n';
            printRec(opaque, "opaque_result_decl");
          }
        }
      }
      if (D->hasSingleExpressionBody()) {
        OS << '\n';
        printRec(D->getSingleExpressionBody(), "single_expression_body");
      } else if (auto Body = D->getBody(/*canSynthesize=*/false)) {
        OS << '\n';
        printRec(Body, "body");
      }
    }

    void printCommonFD(FuncDecl *FD, const char *type, StringRef Label) {
      printCommonAFD(FD, type, Label);
      printFlag(FD->isStatic(), "type");
    }

    void visitFuncDecl(FuncDecl *FD, StringRef Label) {
      printCommonFD(FD, "func_decl", Label);
      printAbstractFunctionDecl(FD);
      printCommonPost();
    }

    void visitAccessorDecl(AccessorDecl *AD, StringRef Label) {
      printCommonFD(AD, "accessor_decl", Label);
      OS << " " << getAccessorKindString(AD->getAccessorKind());
      OS << "_for=" << AD->getStorage()->getName();
      printAbstractFunctionDecl(AD);
      printCommonPost();
    }

    void visitConstructorDecl(ConstructorDecl *CD, StringRef Label) {
      printCommonAFD(CD, "constructor_decl", Label);
      if (CD->isRequired())
        PrintWithColorRAII(OS, DeclModifierColor) << " required";
      PrintWithColorRAII(OS, DeclModifierColor) << " "
        << getCtorInitializerKindString(CD->getInitKind());
      if (CD->isFailable())
        PrintWithColorRAII(OS, DeclModifierColor) << " failable="
          << (CD->isImplicitlyUnwrappedOptional()
              ? "ImplicitlyUnwrappedOptional"
              : "Optional");
      printAbstractFunctionDecl(CD);
      printCommonPost();
    }

    void visitDestructorDecl(DestructorDecl *DD, StringRef Label) {
      printCommonAFD(DD, "destructor_decl", Label);
      printAbstractFunctionDecl(DD);
      printCommonPost();
    }

    void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD, StringRef Label) {
      printCommon(TLCD, "top_level_code_decl", Label);
      if (TLCD->getBody()) {
        OS << "\n";
        printRec(TLCD->getBody());
      }
      printCommonPost();
    }
    
    void printASTNodes(const ArrayRef<ASTNode> &Elements, const char *Name,
                       StringRef Label) {
      printCommon(Name, Label, ASTNodeColor);
      for (auto Elt : Elements) {
        OS << '\n';
        if (auto *SubExpr = Elt.dyn_cast<Expr*>())
          printRec(SubExpr);
        else if (auto *SubStmt = Elt.dyn_cast<Stmt*>())
          printRec(SubStmt);
        else
          printRec(Elt.get<Decl*>());
      }
      printCommonPost();
    }

    void visitIfConfigDecl(IfConfigDecl *ICD, StringRef Label) {
      printCommon(ICD, "if_config_decl", Label);
      Indent += 2;
      for (auto &Clause : ICD->getClauses()) {
        OS << '\n';
        OS.indent(Indent);
        PrintWithColorRAII(OS, StmtColor) << (Clause.Cond ? "#if:" : "#else:");
        if (Clause.isActive)
          PrintWithColorRAII(OS, DeclModifierColor) << " active";
        if (Clause.Cond) {
          OS << "\n";
          printRec(Clause.Cond);
        }

        OS << '\n';
        Indent += 2;
        printASTNodes(Clause.Elements, "", "elements");
        Indent -= 2;
      }

      Indent -= 2;
      printCommonPost();
    }

    void visitPoundDiagnosticDecl(PoundDiagnosticDecl *PDD, StringRef Label) {
      printCommon(PDD, "pound_diagnostic_decl", Label);
      print(PDD->isError() ? "error" : "warning", "kind");
      OS << "\n";
      Indent += 2;
      printRec(PDD->getMessage());
      Indent -= 2;
      printCommonPost();
    }

    void visitPrecedenceGroupDecl(PrecedenceGroupDecl *PGD, StringRef Label) {
      printCommon(PGD, "precedence_group_decl", Label);

      printDeclName(PGD->getName());
      print(getAssociativityString(PGD->getAssociativity()), "associativity");
      printFlag(PGD->isAssignment(), "assignment");

      auto printRelations =
          [&](StringRef label, ArrayRef<PrecedenceGroupDecl::Relation> rels) {
        if (rels.empty()) return;
        OS << '\n';
        OS.indent(Indent+2);
        OS << label << ' ' << rels[0].Name;
        for (auto &rel : rels.slice(1))
          OS << ", " << rel.Name;
      };
      printRelations("higherThan", PGD->getHigherThan());
      printRelations("lowerThan", PGD->getLowerThan());

      printCommonPost();
    }

    void printOperatorIdentifiers(OperatorDecl *OD) {
      auto identifiers = OD->getIdentifiers();
      for (auto index : indices(identifiers)) {
        OS.indent(Indent + 2);
        OS << "identifier #" << index << " " << identifiers[index].Item;
        if (index != identifiers.size() - 1)
          OS << "\n";
      }
    }

    void visitInfixOperatorDecl(InfixOperatorDecl *IOD, StringRef Label) {
      printCommon(IOD, "infix_operator_decl", Label);
      printDeclName(IOD->getName());
      if (!IOD->getIdentifiers().empty()) {
        OS << "\n";
        printOperatorIdentifiers(IOD);
      }
      printCommonPost();
    }

    void visitPrefixOperatorDecl(PrefixOperatorDecl *POD, StringRef Label) {
      printCommon(POD, "prefix_operator_decl", Label);
      printDeclName(POD->getName());
      if (!POD->getIdentifiers().empty()) {
        OS << "\n";
        printOperatorIdentifiers(POD);
      }
      printCommonPost();
    }

    void visitPostfixOperatorDecl(PostfixOperatorDecl *POD, StringRef Label) {
      printCommon(POD, "postfix_operator_decl", Label);
      printDeclName(POD->getName());
      if (!POD->getIdentifiers().empty()) {
        OS << "\n";
        printOperatorIdentifiers(POD);
      }
      printCommonPost();
    }

    void visitModuleDecl(ModuleDecl *MD, StringRef Label) {
      printCommon(MD, "module", Label);

      printFlag(MD->isNonSwiftModule(), "non_swift");
      
      printCommonPost();
    }

    void visitMissingMemberDecl(MissingMemberDecl *MMD, StringRef Label) {
      printCommon(MMD, "missing_member_decl", Label);
      PrintWithColorRAII(OS, IdentifierColor)
          << " \"" << MMD->getName() << '\"';
      printCommonPost();
    }
  };
} // end anonymous namespace

void ParameterList::dump() const {
  dump(llvm::errs(), 0);
}

void ParameterList::dump(raw_ostream &OS, unsigned Indent) const {
  ExprTypeDelegate delegate;
  PrintDecl(nullptr, OS, delegate, Indent).printParameterList(this);
  llvm::errs() << '\n';
}



void Decl::dump() const {
  dump(llvm::errs(), 0);
}

void Decl::dump(const char *filename) const {
  std::error_code ec;
  llvm::raw_fd_ostream stream(filename, ec, llvm::sys::fs::FA_Read |
                              llvm::sys::fs::FA_Write);
  // In assert builds, we blow up. Otherwise, we just return.
  assert(!ec && "Failed to open file for dumping?!");
  if (ec)
    return;
  dump(stream, 0);
}

void Decl::dump(raw_ostream &OS, unsigned Indent) const {
  ExprTypeDelegate delegate;
  PrintDecl(&getASTContext(), OS, delegate, Indent)
      .visit(const_cast<Decl *>(this), "");
  OS << '\n';
}

/// Print the given declaration context (with its parents).
void swift::printContext(raw_ostream &os, DeclContext *dc) {
  if (auto parent = dc->getParent()) {
    printContext(os, parent);
    os << '.';
  }

  switch (dc->getContextKind()) {
  case DeclContextKind::Module:
    printName(os, cast<ModuleDecl>(dc)->getName());
    break;

  case DeclContextKind::FileUnit:
    // FIXME: print the file's basename?
    os << "(file)";
    break;

  case DeclContextKind::SerializedLocal:
    os << "local context";
    break;

  case DeclContextKind::AbstractClosureExpr: {
    auto *ACE = cast<AbstractClosureExpr>(dc);
    if (isa<ClosureExpr>(ACE)) {
      PrintWithColorRAII(os, DiscriminatorColor)
        << "explicit closure discriminator=";
    }
    if (isa<AutoClosureExpr>(ACE)) {
      PrintWithColorRAII(os, DiscriminatorColor)
        << "autoclosure discriminator=";
    }
    PrintWithColorRAII(os, DiscriminatorColor) << ACE->getDiscriminator();
    break;
  }

  case DeclContextKind::GenericTypeDecl:
    printName(os, cast<GenericTypeDecl>(dc)->getName());
    break;

  case DeclContextKind::ExtensionDecl:
    if (auto extendedNominal = cast<ExtensionDecl>(dc)->getExtendedNominal()) {
      printName(os, extendedNominal->getName());
    }
    os << " extension";
    break;

  case DeclContextKind::Initializer:
    switch (cast<Initializer>(dc)->getInitializerKind()) {
    case InitializerKind::PatternBinding:
      os << "pattern binding initializer";
      break;
    case InitializerKind::DefaultArgument:
      os << "default argument initializer";
      break;
    case InitializerKind::PropertyWrapper:
      os << "property wrapper initializer";
      break;
    }
    break;

  case DeclContextKind::TopLevelCodeDecl:
    os << "top-level code";
    break;

  case DeclContextKind::AbstractFunctionDecl:
    printName(os, cast<AbstractFunctionDecl>(dc)->getName());
    break;

  case DeclContextKind::SubscriptDecl:
    printName(os, cast<SubscriptDecl>(dc)->getName());
    break;

  case DeclContextKind::EnumElementDecl:
    printName(os, cast<EnumElementDecl>(dc)->getName());
    break;
  }
}

std::string ValueDecl::printRef() const {
  std::string result;
  llvm::raw_string_ostream os(result);
  dumpRef(os);
  return os.str();
}

void ValueDecl::dumpRef(raw_ostream &os) const {
  // Print the context.
  printContext(os, getDeclContext());
  os << ".";

  // Print name.
  getName().printPretty(os);

  // Print location.
  auto &srcMgr = getASTContext().SourceMgr;
  if (getLoc().isValid()) {
    os << '@';
    getLoc().print(os, srcMgr);
  }
}

void LLVM_ATTRIBUTE_USED ValueDecl::dumpRef() const {
  dumpRef(llvm::errs());
}

void SourceFile::dump() const {
  dump(llvm::errs());
}

void SourceFile::dump(llvm::raw_ostream &OS, bool parseIfNeeded) const {
  // If we're allowed to parse the SourceFile, do so now. We need to force the
  // parsing request as by default the dumping logic tries not to kick any
  // requests.
  if (parseIfNeeded)
    (void)getTopLevelDecls();

  ExprTypeDelegate delegate;
  PrintDecl(&getASTContext(), OS, delegate).visitSourceFile(*this, "");
  llvm::errs() << '\n';
}

void Pattern::dump() const {
  dump(llvm::errs());
}

static ASTContext *getASTContextFromType(Type ty) {
  if (ty)
    return &ty->getASTContext();
  return nullptr;
}

void Pattern::dump(raw_ostream &OS, unsigned Indent) const {
  ExprTypeDelegate delegate;
  PrintPattern(getASTContextFromType(getType()), OS, delegate, Indent)
    .visit(const_cast<Pattern*>(this), "");
  OS << '\n';
}

//===----------------------------------------------------------------------===//
// Printing for Stmt and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
/// PrintStmt - Visitor implementation of Stmt::dump.
class PrintStmt : public StmtVisitor<PrintStmt, void, StringRef> {
public:
  ASTContext *Ctx;
  raw_ostream &OS;
  ExprTypeDelegate &Delegate;
  unsigned Indent;

  explicit PrintStmt(ASTContext *ctx, raw_ostream &os,
                     ExprTypeDelegate &delegate, unsigned indent)
    : Ctx(ctx), OS(os), Delegate(delegate), Indent(indent) { }

  void visit(Stmt *S, StringRef Label) {
    if (S)
      StmtVisitor<PrintStmt, void, StringRef>::visit(S, Label);
    else {
      printCommon("<<null>>", Label, StmtColor);
      printCommonPost();
    }
  }

  void printRec(Stmt *S, StringRef Label = "");
  void printRec(Decl *D, StringRef Label = "");
  void printRec(Expr *E, StringRef Label = "");
  void printRec(const Pattern *P, StringRef Label = "");

  void printRec(StmtConditionElement C, StringRef Label = "") {
    switch (C.getKind()) {
    case StmtConditionElement::CK_Boolean:
      return printRec(C.getBoolean(), Label);
    case StmtConditionElement::CK_PatternBinding:
      Indent += 2;
      printCommon("pattern", Label, PatternColor);
      OS << "\n";
      printRec(C.getPattern());
      OS << "\n";
      printRec(C.getInitializer());
      printCommonPost();
      Indent -= 2;
      break;
    case StmtConditionElement::CK_Availability:
      Indent += 2;
      printCommon("#available", Label, ASTNodeColor);
      OS << "\n";
      for (auto *Query : C.getAvailability()->getQueries()) {
        OS << '\n';
        switch (Query->getKind()) {
        case AvailabilitySpecKind::PlatformVersionConstraint:
          cast<PlatformVersionConstraintAvailabilitySpec>(Query)->print(OS, Indent + 2);
          break;
        case AvailabilitySpecKind::LanguageVersionConstraint:
        case AvailabilitySpecKind::PackageDescriptionVersionConstraint:
          cast<PlatformVersionConstraintAvailabilitySpec>(Query)->print(OS, Indent + 2);
          break;
        case AvailabilitySpecKind::OtherPlatform:
          cast<OtherPlatformAvailabilitySpec>(Query)->print(OS, Indent + 2);
          break;
        }
      }
      printCommonPost();
      Indent -= 2;
      break;
    }
  }

  // Print a single flag.
  void printFlag(StringRef label) {
    PrintWithColorRAII(OS, ExprModifierColor) << " " << label;
  }

  // Print a single flag if it is set.
  void printFlag(bool isSet, StringRef label) {
    if (isSet)
      printFlag(label);
  }

  void printCommon(const char *name, StringRef label, TerminalColor Color) {
    OS.indent(Indent);
    PrintWithColorRAII(OS, ParenthesisColor) << '(';
    PrintWithColorRAII(OS, LabelColor) << label;
    if (!label.empty()) {  // not null/empty
      OS << "=";
    }

    PrintWithColorRAII(OS, Color) << name;
  }

  raw_ostream &printCommon(Stmt *S, const char *Name, StringRef Label) {
    printCommon(Name, Label, StmtColor);

    printFlag(S->isImplicit(), "implicit");

    if (Ctx) {
      auto R = S->getSourceRange();
      if (R.isValid()) {
        PrintWithColorRAII(OS, RangeColor) << " range=";
        R.print(PrintWithColorRAII(OS, RangeColor).getOS(),
                Ctx->SourceMgr, /*PrintText=*/false);
      }
    }

    printFlag(S->TrailingSemiLoc.isValid(), "trailing_semi");

    return OS;
  }

  void printCommonPost() {
    PrintWithColorRAII(OS, ParenthesisColor) << ')';
  }

  void visitBraceStmt(BraceStmt *S, StringRef Label) {
    printCommon(S, "brace_stmt", Label);
    printASTNodes(S->getElements());
    printCommonPost();
  }

  void printASTNodes(const ArrayRef<ASTNode> &Elements) {
    for (auto Elt : Elements) {
      OS << '\n';
      if (auto *SubExpr = Elt.dyn_cast<Expr*>())
        printRec(SubExpr);
      else if (auto *SubStmt = Elt.dyn_cast<Stmt*>())
        printRec(SubStmt);
      else
        printRec(Elt.get<Decl*>());
    }
  }

  void visitReturnStmt(ReturnStmt *S, StringRef Label) {
    printCommon(S, "return_stmt", Label);
    if (S->hasResult()) {
      OS << '\n';
      printRec(S->getResult());
    }
    printCommonPost();
  }

  void visitYieldStmt(YieldStmt *S, StringRef Label) {
    printCommon(S, "yield_stmt", Label);
    for (auto yield : S->getYields()) {
      OS << '\n';
      printRec(yield);
    }
    printCommonPost();
  }

  void visitDeferStmt(DeferStmt *S, StringRef Label) {
    printCommon(S, "defer_stmt", Label) << '\n';
    printRec(S->getTempDecl());
    OS << '\n';
    printRec(S->getCallExpr());
    printCommonPost();
  }

  void visitIfStmt(IfStmt *S, StringRef Label) {
    printCommon(S, "if_stmt", Label) << '\n';
    for (auto elt : S->getCond())
      printRec(elt);
    OS << '\n';
    printRec(S->getThenStmt());
    if (S->getElseStmt()) {
      OS << '\n';
      printRec(S->getElseStmt());
    }
    printCommonPost();
  }

  void visitGuardStmt(GuardStmt *S, StringRef Label) {
    printCommon(S, "guard_stmt", Label) << '\n';
    for (auto elt : S->getCond())
      printRec(elt);
    OS << '\n';
    printRec(S->getBody());
    printCommonPost();
  }

  void visitDoStmt(DoStmt *S, StringRef Label) {
    printCommon(S, "do_stmt", Label) << '\n';
    printRec(S->getBody());
    printCommonPost();
  }

  void visitWhileStmt(WhileStmt *S, StringRef Label) {
    printCommon(S, "while_stmt", Label) << '\n';
    for (auto elt : S->getCond())
      printRec(elt);
    OS << '\n';
    printRec(S->getBody());
    printCommonPost();
  }

  void visitRepeatWhileStmt(RepeatWhileStmt *S, StringRef Label) {
    printCommon(S, "repeat_while_stmt", Label) << '\n';
    printRec(S->getBody());
    OS << '\n';
    printRec(S->getCond());
    printCommonPost();
  }
  void visitForEachStmt(ForEachStmt *S, StringRef Label) {
    printCommon(S, "for_each_stmt", Label);

    OS << '\n';
    printRec(S->getPattern(), "pattern");

    if (S->getWhere()) {
      OS << '\n';
      printRec(S->getWhere(), "where_clause");
    }

    OS << '\n';
    printRec(S->getSequence(), "sequence");

    if (S->getIteratorVar()) {
      OS << '\n';
      printRec(S->getIteratorVar(), "iterator_var");
    }

    if (S->getIteratorVarRef()) {
      OS << '\n';
      printRec(S->getIteratorVarRef(), "iterator_var_ref");
    }

    if (S->getConvertElementExpr()) {
      OS << '\n';
      printRec(S->getConvertElementExpr(), "convert_element_expr");
    }

    if (S->getElementExpr()) {
      OS << '\n';
      printRec(S->getElementExpr(), "element_expr");
    }

    OS << '\n';
    printRec(S->getBody(), "body");

    printCommonPost();
  }
  void visitBreakStmt(BreakStmt *S, StringRef Label) {
    printCommon(S, "break_stmt", Label);
    printCommonPost();
  }
  void visitContinueStmt(ContinueStmt *S, StringRef Label) {
    printCommon(S, "continue_stmt", Label);
    printCommonPost();
  }
  void visitFallthroughStmt(FallthroughStmt *S, StringRef Label) {
    printCommon(S, "fallthrough_stmt", Label);
    printCommonPost();
  }
  void visitSwitchStmt(SwitchStmt *S, StringRef Label) {
    printCommon(S, "switch_stmt", Label);

    OS << '\n';
    printRec(S->getSubjectExpr());

    for (auto N : S->getRawCases()) {
      OS << '\n';
      if (N.is<Stmt*>())
        printRec(N.get<Stmt*>());
      else
        printRec(N.get<Decl*>());
    }
    printCommonPost();
  }
  void visitCaseStmt(CaseStmt *S, StringRef Label) {
    printCommon(S, "case_stmt", Label);
    printFlag(S->hasUnknownAttr(), "@unknown");

    Indent += 2;
    if (S->hasCaseBodyVariables()) {
      OS << '\n';
      printCommon("case_body_variables", "", StmtColor);
      OS << '\n';
      for (auto *vd : S->getCaseBodyVariables()) {
        // TODO: Printing a var decl does an Indent ... dump(vd) ... '\n'. We
        // should see if we can factor this dumping so that the caller of
        // printRec(VarDecl) has more control over the printing.
        printRec(vd);
      }
      // FIXME: Is this correct?
      OS.indent(Indent);
      printCommonPost();
    }

    for (const auto &LabelItem : S->getCaseLabelItems()) {
      OS << '\n';
      printCommon("case_label_item", "", StmtColor);
      printFlag(LabelItem.isDefault(), "default");
      if (auto *CasePattern = LabelItem.getPattern()) {
        OS << '\n';
        printRec(CasePattern, "pattern");
      }
      if (auto *Guard = LabelItem.getGuardExpr()) {
        OS << '\n';
        printRec(const_cast<Expr *>(Guard), "where_clause");
      }
      printCommonPost();
    }
    Indent -= 2;

    OS << '\n';
    printRec(S->getBody(), "body");

    printCommonPost();
  }

  void visitFailStmt(FailStmt *S, StringRef Label) {
    printCommon(S, "fail_stmt", Label);
    printCommonPost();
  }

  void visitThrowStmt(ThrowStmt *S, StringRef Label) {
    printCommon(S, "throw_stmt", Label);
    OS << '\n';
    printRec(S->getSubExpr());
    printCommonPost();
  }

  void visitPoundAssertStmt(PoundAssertStmt *S, StringRef Label) {
    printCommon(S, "pound_assert", Label);
    OS << " message=" << QuotedString(S->getMessage()) << "\n";
    printRec(S->getCondition());
    printCommonPost();
  }

  void visitDoCatchStmt(DoCatchStmt *S, StringRef Label) {
    printCommon(S, "do_catch_stmt", Label);

    OS << '\n';
    printRec(S->getBody(), "do_body");

    for (auto clause : S->getCatches()) {
      OS << '\n';
      printRec(clause);
    }

    printCommonPost();
  }
};

} // end anonymous namespace

void Stmt::dump() const {
  dump(llvm::errs());
  llvm::errs() << '\n';
}

void Stmt::dump(raw_ostream &OS, const ASTContext *Ctx, unsigned Indent) const {
  ExprTypeDelegate delegate;
  PrintStmt(const_cast<ASTContext *>(Ctx), OS, delegate, Indent)
      .visit(const_cast<Stmt*>(this), "");
}

//===----------------------------------------------------------------------===//
// Printing for Expr and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
/// PrintExpr - Visitor implementation of Expr::dump.
class PrintExpr : public ExprVisitor<PrintExpr, void, StringRef> {
public:
  ASTContext *Ctx;
  raw_ostream &OS;
  ExprTypeDelegate &Delegate;
  unsigned Indent;

  Type getTypeOfExpr(Expr *E) {
    return Delegate.GetTypeOfExpr(E);
  }
  bool canGetTypeOfTypeRepr() {
    return (bool)Delegate.GetTypeOfTypeRepr;
  }
  Type getTypeOfTypeRepr(TypeRepr *T) {
    return Delegate.GetTypeOfTypeRepr(T);
  }
  Type getTypeOfKeyPathComponent(KeyPathExpr *E, unsigned index) {
    return Delegate.GetTypeOfKeyPathComponent(E, index);
  }

  PrintExpr(ASTContext *ctx, raw_ostream &os, ExprTypeDelegate &delegate,
            unsigned indent)
      : Ctx(ctx), OS(os), Delegate(delegate), Indent(indent) {}

  void visit(Expr *E, StringRef Label) {
    if (E)
      ExprVisitor<PrintExpr, void, StringRef>::visit(E, Label);
    else {
      printCommon("<<null>>", Label, ExprColor);
      printCommonPost();
    }
  }

  void printRec(Expr *E, StringRef Label = "");

  /// FIXME: This should use ExprWalker to print children.

  void printRec(Decl *D, StringRef Label = "");
  void printRec(Stmt *S, StringRef Label = "");
  void printRec(const Pattern *P, StringRef Label = "");
  void printRec(TypeRepr *T, StringRef Label = "");

  template<typename T, typename... Args>
  void printRec(ArrayRef<T> elems, StringRef Label, Args... args) {
    Indent += 2;
    printCommon("array", Label, ExprModifierColor);
    for (auto elt : elems) {
      OS << "\n";
      printRec(elt, ::std::forward<Args>(args)..., "");
    }
    printCommonPost();
    Indent -= 2;
  }

  void printRec(ProtocolConformanceRef conf) {
    conf.dump(OS, Indent + 2);
  }

  void printDeclRef(ConcreteDeclRef declRef) {
    declRef.dump(PrintWithColorRAII(OS, DeclColor).getOS());
  }

  // Print a single flag.
  void printFlag(StringRef label) {
    PrintWithColorRAII(OS, ExprModifierColor) << " " << label;
  }

  // Print a single flag if it is set.
  void printFlag(bool isSet, StringRef label) {
    if (isSet)
      printFlag(label);
  }

  void printCommon(const char *name, StringRef label, TerminalColor Color) {
    OS.indent(Indent);
    PrintWithColorRAII(OS, ParenthesisColor) << '(';
    PrintWithColorRAII(OS, LabelColor) << label;
    if (!label.empty()) {  // not null/empty
      OS << "=";
    }

    PrintWithColorRAII(OS, Color) << name;
  }

  raw_ostream &printCommon(Expr *E, const char *C, StringRef Label) {
    PrintOptions PO;
    PO.PrintTypesForDebugging = true;

    printCommon(C, Label, ExprColor);

    if (E->isImplicit())
      PrintWithColorRAII(OS, ExprModifierColor) << " implicit";
    PrintWithColorRAII(OS, TypeColor) << " type='";
    PrintWithColorRAII(OS, TypeColor) << getTypeOfExpr(E).getString(PO) << '\'';

    // If we have a source range and an ASTContext, print the source range.
    if (!Ctx)
      if (auto Ty = getTypeOfExpr(E))
        Ctx = &Ty->getASTContext();

    if (Ctx) {
      auto L = E->getLoc();
      if (L.isValid()) {
        PrintWithColorRAII(OS, LocationColor) << " location=";
        L.print(PrintWithColorRAII(OS, LocationColor).getOS(), Ctx->SourceMgr);
      }

      auto R = E->getSourceRange();
      if (R.isValid()) {
        PrintWithColorRAII(OS, RangeColor) << " range=";
        R.print(PrintWithColorRAII(OS, RangeColor).getOS(),
                Ctx->SourceMgr, /*PrintText=*/false);
      }
    }

    printFlag(E->TrailingSemiLoc.isValid(), "trailing_semi");

    return OS;
  }

  void printCommonPost() {
    PrintWithColorRAII(OS, ParenthesisColor) << ')';
  }

  void printSemanticExpr(Expr * semanticExpr) {
    if (semanticExpr == nullptr) {
      return;
    }
    
    OS << '\n';
    printRec(semanticExpr, "semantic_expr");
  }

  void visitErrorExpr(ErrorExpr *E, StringRef Label) {
    printCommon(E, "error_expr", Label);
    printCommonPost();
  }

  void visitCodeCompletionExpr(CodeCompletionExpr *E, StringRef Label) {
    printCommon(E, "code_completion_expr", Label);
    if (E->getBase()) {
      OS << '\n';
      printRec(E->getBase());
    }
    printCommonPost();
  }

  void visitNilLiteralExpr(NilLiteralExpr *E, StringRef Label) {
    printCommon(E, "nil_literal_expr", Label);
    PrintWithColorRAII(OS, LiteralValueColor) << " initializer=";
    E->getInitializer().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    printCommonPost();
  }

  void visitIntegerLiteralExpr(IntegerLiteralExpr *E, StringRef Label) {
    printCommon(E, "integer_literal_expr", Label);
    if (E->isNegative())
      PrintWithColorRAII(OS, LiteralValueColor) << " negative";
    PrintWithColorRAII(OS, LiteralValueColor) << " value=";
    Type T = getTypeOfExpr(E);
    if (T.isNull() || !T->is<BuiltinIntegerType>())
      PrintWithColorRAII(OS, LiteralValueColor) << E->getDigitsText();
    else
      PrintWithColorRAII(OS, LiteralValueColor) << E->getValue();
    PrintWithColorRAII(OS, LiteralValueColor) << " builtin_initializer=";
    E->getBuiltinInitializer().dump(
        PrintWithColorRAII(OS, LiteralValueColor).getOS());
    PrintWithColorRAII(OS, LiteralValueColor) << " initializer=";
    E->getInitializer().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    printCommonPost();
  }
  void visitFloatLiteralExpr(FloatLiteralExpr *E, StringRef Label) {
    printCommon(E, "float_literal_expr", Label);
    if (E->isNegative())
      PrintWithColorRAII(OS, LiteralValueColor) << " negative";
    PrintWithColorRAII(OS, LiteralValueColor)
      << " value=" << E->getDigitsText();
    PrintWithColorRAII(OS, LiteralValueColor) << " builtin_initializer=";
    E->getBuiltinInitializer().dump(
        PrintWithColorRAII(OS, LiteralValueColor).getOS());
    PrintWithColorRAII(OS, LiteralValueColor) << " initializer=";
    E->getInitializer().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    if (!E->getBuiltinType().isNull()) {
      PrintWithColorRAII(OS, TypeColor) << " builtin_type='";
      E->getBuiltinType().print(PrintWithColorRAII(OS, TypeColor).getOS());
      PrintWithColorRAII(OS, TypeColor) << "'";
    }
    printCommonPost();
  }

  void visitBooleanLiteralExpr(BooleanLiteralExpr *E, StringRef Label) {
    printCommon(E, "boolean_literal_expr", Label);
    PrintWithColorRAII(OS, LiteralValueColor)
      << " value=" << (E->getValue() ? "true" : "false")
      << " builtin_initializer=";
    E->getBuiltinInitializer().dump(
      PrintWithColorRAII(OS, LiteralValueColor).getOS());
    PrintWithColorRAII(OS, LiteralValueColor) << " initializer=";
    E->getInitializer().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    printCommonPost();
  }

  void visitStringLiteralExpr(StringLiteralExpr *E, StringRef Label) {
    printCommon(E, "string_literal_expr", Label);
    PrintWithColorRAII(OS, LiteralValueColor) << " encoding="
      << getStringLiteralExprEncodingString(E->getEncoding())
      << " value=" << QuotedString(E->getValue())
      << " builtin_initializer=";
    E->getBuiltinInitializer().dump(
      PrintWithColorRAII(OS, LiteralValueColor).getOS());
    PrintWithColorRAII(OS, LiteralValueColor) << " initializer=";
    E->getInitializer().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    printCommonPost();
  }
  void visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E, StringRef Label) {
    printCommon(E, "interpolated_string_literal_expr", Label);
    
    // Print the trailing quote location
    if (Ctx) {
      auto TQL = E->getTrailingQuoteLoc();
      if (TQL.isValid()) {
        PrintWithColorRAII(OS, LocationColor) << " trailing_quote_loc=";
        TQL.print(PrintWithColorRAII(OS, LocationColor).getOS(),
                  Ctx->SourceMgr);
      }
    }
    PrintWithColorRAII(OS, LiteralValueColor)
      << " literal_capacity="
      << E->getLiteralCapacity() << " interpolation_count="
      << E->getInterpolationCount();
    PrintWithColorRAII(OS, LiteralValueColor) << " builder_init=";
    E->getBuilderInit().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    PrintWithColorRAII(OS, LiteralValueColor) << " result_init=";
    E->getInitializer().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    OS << "\n";
    printRec(E->getAppendingExpr());
    printCommonPost();
  }
  void visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E, StringRef Label) {
    printCommon(E, "magic_identifier_literal_expr", Label);
    OS << " kind=" << MagicIdentifierLiteralExpr::getKindString(E->getKind());

    if (E->isString()) {
      OS << " encoding="
         << getStringLiteralExprEncodingString(E->getStringEncoding());
    }
    OS << " builtin_initializer=";
    E->getBuiltinInitializer().dump(OS);
    OS << " initializer=";
    E->getInitializer().dump(OS);
    printCommonPost();
  }

  void visitObjectLiteralExpr(ObjectLiteralExpr *E, StringRef Label) {
    printCommon(E, "object_literal", Label);
    OS << " kind='" << E->getLiteralKindPlainName() << "'";
    PrintWithColorRAII(OS, LiteralValueColor) << " initializer=";
    E->getInitializer().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    printArgumentLabels(E->getArgumentLabels());
    OS << "\n";
    printRec(E->getArg());
    printCommonPost();
  }

  void visitDiscardAssignmentExpr(DiscardAssignmentExpr *E, StringRef Label) {
    printCommon(E, "discard_assignment_expr", Label);
    printCommonPost();
  }

  void visitDeclRefExpr(DeclRefExpr *E, StringRef Label) {
    printCommon(E, "declref_expr", Label);
    PrintWithColorRAII(OS, DeclColor) << " decl=";
    printDeclRef(E->getDeclRef());
    if (E->getAccessSemantics() != AccessSemantics::Ordinary)
      PrintWithColorRAII(OS, AccessLevelColor)
        << " " << getAccessSemanticsString(E->getAccessSemantics());
    PrintWithColorRAII(OS, ExprModifierColor)
      << " function_ref=" << getFunctionRefKindStr(E->getFunctionRefKind());
    printCommonPost();
  }
  void visitSuperRefExpr(SuperRefExpr *E, StringRef Label) {
    printCommon(E, "super_ref_expr", Label);
    printCommonPost();
  }

  void visitTypeExpr(TypeExpr *E, StringRef Label) {
    printCommon(E, "type_expr", Label);
    PrintWithColorRAII(OS, TypeReprColor) << " typerepr='";
    if (E->getTypeRepr())
      E->getTypeRepr()->print(PrintWithColorRAII(OS, TypeReprColor).getOS());
    else
      PrintWithColorRAII(OS, TypeReprColor) << "<<NULL>>";
    PrintWithColorRAII(OS, TypeReprColor) << "'";
    printCommonPost();
  }

  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E, StringRef Label) {
    printCommon(E, "other_constructor_ref_expr", Label);
    PrintWithColorRAII(OS, DeclColor) << " decl=";
    printDeclRef(E->getDeclRef());
    printCommonPost();
  }
  void visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *E, StringRef Label) {
    printCommon(E, "overloaded_decl_ref_expr", Label);
    PrintWithColorRAII(OS, IdentifierColor) << " name="
      << E->getDecls()[0]->getBaseName();
    PrintWithColorRAII(OS, ExprModifierColor)
      << " number_of_decls=" << E->getDecls().size()
      << " function_ref=" << getFunctionRefKindStr(E->getFunctionRefKind())
      << " decls=[\n";
    interleave(E->getDecls(),
               [&](ValueDecl *D) {
                 OS.indent(Indent + 2);
                 D->dumpRef(PrintWithColorRAII(OS, DeclModifierColor).getOS());
               },
               [&] { PrintWithColorRAII(OS, DeclModifierColor) << ",\n"; });
    PrintWithColorRAII(OS, ExprModifierColor) << "]";
    printCommonPost();
  }
  void visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E, StringRef Label) {
    printCommon(E, "unresolved_decl_ref_expr", Label);
    PrintWithColorRAII(OS, IdentifierColor) << " name=" << E->getName();
    PrintWithColorRAII(OS, ExprModifierColor)
      << " function_ref=" << getFunctionRefKindStr(E->getFunctionRefKind());
    printCommonPost();
  }
  void visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *E, StringRef Label) {
    printCommon(E, "unresolved_specialize_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    for (TypeLoc T : E->getUnresolvedParams()) {
      OS << '\n';
      printRec(T.getTypeRepr());
    }
    printCommonPost();
  }

  void visitMemberRefExpr(MemberRefExpr *E, StringRef Label) {
    printCommon(E, "member_ref_expr", Label);
    PrintWithColorRAII(OS, DeclColor) << " decl=";
    printDeclRef(E->getMember());
    if (E->getAccessSemantics() != AccessSemantics::Ordinary)
      PrintWithColorRAII(OS, AccessLevelColor)
        << " " << getAccessSemanticsString(E->getAccessSemantics());
    printFlag(E->isSuper(), "super");

    OS << '\n';
    printRec(E->getBase());
    printCommonPost();
  }
  void visitDynamicMemberRefExpr(DynamicMemberRefExpr *E, StringRef Label) {
    printCommon(E, "dynamic_member_ref_expr", Label);
    PrintWithColorRAII(OS, DeclColor) << " decl=";
    E->getMember().dump(OS);
    OS << '\n';
    printRec(E->getBase());
    printCommonPost();
  }
  void visitUnresolvedMemberExpr(UnresolvedMemberExpr *E, StringRef Label) {
    printCommon(E, "unresolved_member_expr", Label);
    OS << " name='" << E->getName() << "'";
    PrintWithColorRAII(OS, ExprModifierColor)
      << " function_ref=" << getFunctionRefKindStr(E->getFunctionRefKind());
    printCommonPost();
  }
  void visitDotSelfExpr(DotSelfExpr *E, StringRef Label) {
    printCommon(E, "dot_self_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitParenExpr(ParenExpr *E, StringRef Label) {
    printCommon(E, "paren_expr", Label);
    if (E->hasTrailingClosure())
      OS << " trailing-closure";
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitAwaitExpr(AwaitExpr *E, StringRef Label) {
    printCommon(E, "await_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitUnresolvedMemberChainResultExpr(UnresolvedMemberChainResultExpr *E,
                                            StringRef Label){
    printCommon(E, "unresolved_member_chain_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitTupleExpr(TupleExpr *E, StringRef Label) {
    printCommon(E, "tuple_expr", Label);
    if (E->hasTrailingClosure())
      OS << " trailing-closure";

    if (E->hasElementNames()) {
      PrintWithColorRAII(OS, IdentifierColor) << " names=";

      interleave(E->getElementNames(),
                 [&](Identifier name) {
                   PrintWithColorRAII(OS, IdentifierColor)
                     << (name.empty()?"''":name.str());
                 },
                 [&] { PrintWithColorRAII(OS, IdentifierColor) << ","; });
    }

    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i) {
      OS << '\n';
      if (E->getElement(i))
        printRec(E->getElement(i));
      else
        OS.indent(Indent+2) << "<<tuple element default value>>";
    }
    printCommonPost();
  }
  void visitArrayExpr(ArrayExpr *E, StringRef Label) {
    printCommon(E, "array_expr", Label);
    PrintWithColorRAII(OS, LiteralValueColor) << " initializer=";
    E->getInitializer().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    for (auto elt : E->getElements()) {
      OS << '\n';
      printRec(elt);
    }
    printCommonPost();
  }
  void visitDictionaryExpr(DictionaryExpr *E, StringRef Label) {
    printCommon(E, "dictionary_expr", Label);
    PrintWithColorRAII(OS, LiteralValueColor) << " initializer=";
    E->getInitializer().dump(PrintWithColorRAII(OS, LiteralValueColor).getOS());
    for (auto elt : E->getElements()) {
      OS << '\n';
      printRec(elt);
    }
    printCommonPost();
  }
  void visitSubscriptExpr(SubscriptExpr *E, StringRef Label) {
    printCommon(E, "subscript_expr", Label);
    if (E->getAccessSemantics() != AccessSemantics::Ordinary)
      PrintWithColorRAII(OS, AccessLevelColor)
        << " " << getAccessSemanticsString(E->getAccessSemantics());
    printFlag(E->isSuper(), "super");
    if (E->hasDecl()) {
      PrintWithColorRAII(OS, DeclColor) << " decl=";
      printDeclRef(E->getDecl());
    }
    printArgumentLabels(E->getArgumentLabels());
    OS << '\n';
    printRec(E->getBase());
    OS << '\n';
    printRec(E->getIndex());
    printCommonPost();
  }
  void visitKeyPathApplicationExpr(KeyPathApplicationExpr *E, StringRef Label) {
    printCommon(E, "keypath_application_expr", Label);
    OS << '\n';
    printRec(E->getBase());
    OS << '\n';
    printRec(E->getKeyPath());
    printCommonPost();
  }
  void visitDynamicSubscriptExpr(DynamicSubscriptExpr *E, StringRef Label) {
    printCommon(E, "dynamic_subscript_expr", Label);
    PrintWithColorRAII(OS, DeclColor) << " decl=";
    printDeclRef(E->getMember());
    printArgumentLabels(E->getArgumentLabels());
    OS << '\n';
    printRec(E->getBase());
    OS << '\n';
    printRec(E->getIndex());
    printCommonPost();
  }
  void visitUnresolvedDotExpr(UnresolvedDotExpr *E, StringRef Label) {
    printCommon(E, "unresolved_dot_expr", Label);
    OS << " field '" << E->getName() << "'";
    PrintWithColorRAII(OS, ExprModifierColor)
      << " function_ref=" << getFunctionRefKindStr(E->getFunctionRefKind());
    if (E->getBase()) {
      OS << '\n';
      printRec(E->getBase());
    }
    printCommonPost();
  }
  void visitTupleElementExpr(TupleElementExpr *E, StringRef Label) {
    printCommon(E, "tuple_element_expr", Label);
    OS << " field #" << E->getFieldNumber() << '\n';
    printRec(E->getBase());
    printCommonPost();
  }
  void visitDestructureTupleExpr(DestructureTupleExpr *E, StringRef Label) {
    printCommon(E, "destructure_tuple_expr", Label);
    OS << "\n";
    printRec(E->getDestructuredElements(), "destructured");
    OS << "\n";
    printRec(E->getSubExpr());
    OS << "\n";
    printRec(E->getResultExpr());
    printCommonPost();
  }
  void visitUnresolvedTypeConversionExpr(UnresolvedTypeConversionExpr *E, StringRef Label) {
    printCommon(E, "unresolvedtype_conversion_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitFunctionConversionExpr(FunctionConversionExpr *E, StringRef Label) {
    printCommon(E, "function_conversion_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitCovariantFunctionConversionExpr(CovariantFunctionConversionExpr *E,
                                            StringRef Label) {
    printCommon(E, "covariant_function_conversion_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitCovariantReturnConversionExpr(CovariantReturnConversionExpr *E,
                                          StringRef Label) {
    printCommon(E, "covariant_return_conversion_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitImplicitlyUnwrappedFunctionConversionExpr(
      ImplicitlyUnwrappedFunctionConversionExpr *E, StringRef Label) {
    printCommon(E, "implicitly_unwrapped_function_conversion_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitUnderlyingToOpaqueExpr(UnderlyingToOpaqueExpr *E, StringRef Label) {
    printCommon(E, "underlying_to_opaque_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitErasureExpr(ErasureExpr *E, StringRef Label) {
    printCommon(E, "erasure_expr", Label);
    OS << '\n';
    for (auto conf : E->getConformances()) {
      printRec(conf);
      OS << '\n';
    }
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitAnyHashableErasureExpr(AnyHashableErasureExpr *E, StringRef Label) {
    printCommon(E, "any_hashable_erasure_expr", Label);
    OS << '\n';
    printRec(E->getConformance());
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitConditionalBridgeFromObjCExpr(ConditionalBridgeFromObjCExpr *E,
                                          StringRef Label) {
    printCommon(E, "conditional_bridge_from_objc_expr", Label);
    OS << " conversion=";
    printDeclRef(E->getConversion());
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitBridgeFromObjCExpr(BridgeFromObjCExpr *E, StringRef Label) {
    printCommon(E, "bridge_from_objc_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitBridgeToObjCExpr(BridgeToObjCExpr *E, StringRef Label) {
    printCommon(E, "bridge_to_objc_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitLoadExpr(LoadExpr *E, StringRef Label) {
    printCommon(E, "load_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitMetatypeConversionExpr(MetatypeConversionExpr *E, StringRef Label) {
    printCommon(E, "metatype_conversion_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitCollectionUpcastConversionExpr(CollectionUpcastConversionExpr *E,
                                           StringRef Label) {
    printCommon(E, "collection_upcast_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    if (auto keyConversion = E->getKeyConversion()) {
      OS << '\n';
      printRec(keyConversion.Conversion, "key_conversion");
    }
    if (auto valueConversion = E->getValueConversion()) {
      OS << '\n';
      printRec(valueConversion.Conversion, "value_conversion");
    }
    printCommonPost();
  }
  void visitDerivedToBaseExpr(DerivedToBaseExpr *E, StringRef Label) {
    printCommon(E, "derived_to_base_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E, StringRef Label) {
    printCommon(E, "archetype_to_super_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E, StringRef Label) {
    printCommon(E, "inject_into_optional", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitClassMetatypeToObjectExpr(ClassMetatypeToObjectExpr *E,
                                      StringRef Label) {
    printCommon(E, "class_metatype_to_object", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitExistentialMetatypeToObjectExpr(ExistentialMetatypeToObjectExpr *E,
                                            StringRef Label) {
    printCommon(E, "existential_metatype_to_object", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitProtocolMetatypeToObjectExpr(ProtocolMetatypeToObjectExpr *E,
                                         StringRef Label) {
    printCommon(E, "protocol_metatype_to_object", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitInOutToPointerExpr(InOutToPointerExpr *E, StringRef Label) {
    printCommon(E, "inout_to_pointer", Label);
    OS << (E->isNonAccessing() ? " nonaccessing" : "") << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitArrayToPointerExpr(ArrayToPointerExpr *E, StringRef Label) {
    printCommon(E, "array_to_pointer", Label);
    OS << (E->isNonAccessing() ? " nonaccessing" : "") << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitStringToPointerExpr(StringToPointerExpr *E, StringRef Label) {
    printCommon(E, "string_to_pointer", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitPointerToPointerExpr(PointerToPointerExpr *E, StringRef Label) {
    printCommon(E, "pointer_to_pointer", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitForeignObjectConversionExpr(ForeignObjectConversionExpr *E, StringRef Label) {
    printCommon(E, "foreign_object_conversion", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitUnevaluatedInstanceExpr(UnevaluatedInstanceExpr *E, StringRef Label) {
    printCommon(E, "unevaluated_instance", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitDifferentiableFunctionExpr(DifferentiableFunctionExpr *E, StringRef Label) {
    printCommon(E, "differentiable_function", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitLinearFunctionExpr(LinearFunctionExpr *E, StringRef Label) {
    printCommon(E, "linear_function", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitDifferentiableFunctionExtractOriginalExpr(
      DifferentiableFunctionExtractOriginalExpr *E, StringRef Label) {
    printCommon(E, "differentiable_function_extract_original", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitLinearFunctionExtractOriginalExpr(
      LinearFunctionExtractOriginalExpr *E, StringRef Label) {
    printCommon(E, "linear_function_extract_original", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitLinearToDifferentiableFunctionExpr(
      LinearToDifferentiableFunctionExpr *E, StringRef Label) {
    printCommon(E, "linear_to_differentiable_function", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }

  void visitInOutExpr(InOutExpr *E, StringRef Label) {
    printCommon(E, "inout_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }

  void visitVarargExpansionExpr(VarargExpansionExpr *E, StringRef Label) {
    printCommon(E, "vararg_expansion_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }

  void visitForceTryExpr(ForceTryExpr *E, StringRef Label) {
    printCommon(E, "force_try_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }

  void visitOptionalTryExpr(OptionalTryExpr *E, StringRef Label) {
    printCommon(E, "optional_try_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }

  void visitTryExpr(TryExpr *E, StringRef Label) {
    printCommon(E, "try_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }

  void visitSequenceExpr(SequenceExpr *E, StringRef Label) {
    printCommon(E, "sequence_expr", Label);
    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i) {
      OS << '\n';
      printRec(E->getElement(i));
    }
    printCommonPost();
  }

  void visitCaptureListExpr(CaptureListExpr *E, StringRef Label) {
    printCommon(E, "capture_list", Label);
    for (auto capture : E->getCaptureList()) {
      OS << '\n';
      Indent += 2;
      printRec(capture.PBD);
      Indent -= 2;
    }
    printRec(E->getClosureBody());
    printCommonPost();
  }

  llvm::raw_ostream &printClosure(AbstractClosureExpr *E, char const *name, StringRef Label) {
    printCommon(E, name, Label);
    PrintWithColorRAII(OS, DiscriminatorColor)
      << " discriminator=" << E->getDiscriminator();

    switch (auto isolation = E->getActorIsolation()) {
    case ClosureActorIsolation::Independent:
      break;

    case ClosureActorIsolation::ActorInstance:
      PrintWithColorRAII(OS, CapturesColor) << " actor-isolated="
        << isolation.getActorInstance()->printRef();
      break;

    case ClosureActorIsolation::GlobalActor:
      PrintWithColorRAII(OS, CapturesColor) << " global-actor-isolated="
        << isolation.getGlobalActor().getString();
      break;
    }

    if (!E->getCaptureInfo().isTrivial()) {
      OS << " ";
      E->getCaptureInfo().print(PrintWithColorRAII(OS, CapturesColor).getOS());
    }
    // Printing a function type doesn't indicate whether it's escaping because it doesn't 
    // matter in 99% of contexts. AbstractClosureExpr nodes are one of the only exceptions.
    if (auto Ty = getTypeOfExpr(E)) {
      if (auto fType = Ty->getAs<AnyFunctionType>()) {
        if (!fType->getExtInfo().isNoEscape())
          PrintWithColorRAII(OS, ClosureModifierColor) << " escaping";
        if (fType->getExtInfo().isSendable())
          PrintWithColorRAII(OS, ClosureModifierColor) << " concurrent";
      }
    }

    return OS;
  }

  void visitClosureExpr(ClosureExpr *E, StringRef Label) {
    printClosure(E, "closure_expr", Label);
    if (E->hasSingleExpressionBody())
      PrintWithColorRAII(OS, ClosureModifierColor) << " single-expression";
    if (E->allowsImplicitSelfCapture())
      PrintWithColorRAII(OS, ClosureModifierColor) << " implicit-self";
    if (E->inheritsActorContext())
      PrintWithColorRAII(OS, ClosureModifierColor) << " inherits-actor-context";

    if (E->getParameters()) {
      OS << '\n';
      PrintDecl(Ctx, OS, Delegate, Indent+2)
          .printParameterList(E->getParameters());
    }

    OS << '\n';
    printRec(E->getBody());
    printCommonPost();
  }
  void visitAutoClosureExpr(AutoClosureExpr *E, StringRef Label) {
    printClosure(E, "autoclosure_expr", Label);

    if (E->getParameters()) {
      OS << '\n';
      PrintDecl(Ctx, OS, Delegate, Indent+2)
          .printParameterList(E->getParameters());
    }

    OS << '\n';
    printRec(E->getSingleExpressionBody());

    printCommonPost();
  }

  void visitDynamicTypeExpr(DynamicTypeExpr *E, StringRef Label) {
    printCommon(E, "metatype_expr", Label);
    OS << '\n';
    printRec(E->getBase());
    printCommonPost();
  }

  void visitOpaqueValueExpr(OpaqueValueExpr *E, StringRef Label) {
    printCommon(E, "opaque_value_expr", Label);
    OS << " @ " << (void*)E;
    printCommonPost();
  }

  void visitPropertyWrapperValuePlaceholderExpr(
      PropertyWrapperValuePlaceholderExpr *E, StringRef Label) {
    printCommon(E, "property_wrapper_value_placeholder_expr", Label);
    OS << '\n';
    printRec(E->getOpaqueValuePlaceholder());
    if (auto *value = E->getOriginalWrappedValue()) {
      OS << '\n';
      printRec(value);
    }
    printCommonPost();
  }

  void visitAppliedPropertyWrapperExpr(AppliedPropertyWrapperExpr *E,
                                       StringRef Label) {
    printCommon(E, "applied_property_wrapper_expr", Label);
    printRec(E->getValue());
    printCommonPost();
  }

  void visitDefaultArgumentExpr(DefaultArgumentExpr *E, StringRef Label) {
    printCommon(E, "default_argument_expr", Label);
    OS << " default_args_owner=";
    E->getDefaultArgsOwner().dump(OS);
    OS << " param=" << E->getParamIndex();
    printCommonPost();
  }

  void printArgumentLabels(ArrayRef<Identifier> argLabels) {
    PrintWithColorRAII(OS, ArgumentsColor) << " arg_labels=";
    for (auto label : argLabels) {
      PrintWithColorRAII(OS, ArgumentsColor)
        << (label.empty() ? "_" : label.str()) << ":";
    }
  }

  void printApplyExpr(ApplyExpr *E, const char *NodeName, StringRef Label) {
    printCommon(E, NodeName, Label);
    if (E->isThrowsSet()) {
      PrintWithColorRAII(OS, ExprModifierColor)
        << (E->throws() ? " throws" : " nothrow");
    }
    if (auto call = dyn_cast<CallExpr>(E))
      printArgumentLabels(call->getArgumentLabels());

    OS << '\n';
    printRec(E->getFn());
    OS << '\n';
    printRec(E->getArg());
    printCommonPost();
  }

  void visitCallExpr(CallExpr *E, StringRef Label) {
    printApplyExpr(E, "call_expr", Label);
  }
  void visitPrefixUnaryExpr(PrefixUnaryExpr *E, StringRef Label) {
    printApplyExpr(E, "prefix_unary_expr", Label);
  }
  void visitPostfixUnaryExpr(PostfixUnaryExpr *E, StringRef Label) {
    printApplyExpr(E, "postfix_unary_expr", Label);
  }
  void visitBinaryExpr(BinaryExpr *E, StringRef Label) {
    printApplyExpr(E, "binary_expr", Label);
  }
  void visitDotSyntaxCallExpr(DotSyntaxCallExpr *E, StringRef Label) {
    printApplyExpr(E, "dot_syntax_call_expr", Label);
  }
  void visitConstructorRefCallExpr(ConstructorRefCallExpr *E, StringRef Label) {
    printApplyExpr(E, "constructor_ref_call_expr", Label);
  }
  void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E, StringRef Label) {
    printCommon(E, "dot_syntax_base_ignored", Label);
    OS << '\n';
    printRec(E->getLHS());
    OS << '\n';
    printRec(E->getRHS());
    printCommonPost();
  }

  void printExplicitCastExpr(ExplicitCastExpr *E, const char *name, StringRef Label) {
    printCommon(E, name, Label);

    if (auto checkedCast = dyn_cast<CheckedCastExpr>(E))
      OS << ' ' << getCheckedCastKindName(checkedCast->getCastKind());

    OS << " writtenType='";
    if (canGetTypeOfTypeRepr())
      getTypeOfTypeRepr(E->getCastTypeRepr()).print(OS);
    else
      E->getCastType().print(OS);
    OS << "'";

    OS << '\n';
    printRec(E->getSubExpr());

    printCommonPost();
  }
  void visitForcedCheckedCastExpr(ForcedCheckedCastExpr *E, StringRef Label) {
    printExplicitCastExpr(E, "forced_checked_cast_expr", Label);
  }
  void visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E,
                                       StringRef Label) {
    printExplicitCastExpr(E, "conditional_checked_cast_expr", Label);
  }
  void visitIsExpr(IsExpr *E, StringRef Label) {
    printExplicitCastExpr(E, "is_subtype_expr", Label);
  }
  void visitCoerceExpr(CoerceExpr *E, StringRef Label) {
    printExplicitCastExpr(E, "coerce_expr", Label);
  }
  void visitArrowExpr(ArrowExpr *E, StringRef Label) {
    printCommon(E, "arrow_expr", Label);
    printFlag(E->getAsyncLoc().isValid(), "async");
    printFlag(E->getThrowsLoc().isValid(), "throws");
    OS << '\n';
    printRec(E->getArgsExpr());
    OS << '\n';
    printRec(E->getResultExpr());
    printCommonPost();
  }
  void visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E, StringRef Label) {
    printCommon(E, "rebind_self_in_constructor_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitIfExpr(IfExpr *E, StringRef Label) {
    printCommon(E, "if_expr", Label);
    OS << '\n';
    printRec(E->getCondExpr());
    OS << '\n';
    printRec(E->getThenExpr());
    OS << '\n';
    printRec(E->getElseExpr());
    printCommonPost();
  }
  void visitAssignExpr(AssignExpr *E, StringRef Label) {
    printCommon(E, "assign_expr", Label);
    OS << '\n';
    printRec(E->getDest());
    OS << '\n';
    printRec(E->getSrc());
    printCommonPost();
  }
  void visitEnumIsCaseExpr(EnumIsCaseExpr *E, StringRef Label) {
    printCommon(E, "enum_is_case_expr", Label);
    OS << ' ' << E->getEnumElement()->getBaseIdentifier();
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitUnresolvedPatternExpr(UnresolvedPatternExpr *E, StringRef Label) {
    printCommon(E, "unresolved_pattern_expr", Label);
    OS << '\n';
    printRec(E->getSubPattern());
    printCommonPost();
  }
  void visitBindOptionalExpr(BindOptionalExpr *E, StringRef Label) {
    printCommon(E, "bind_optional_expr", Label);
    OS << " depth=" << E->getDepth() << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitOptionalEvaluationExpr(OptionalEvaluationExpr *E, StringRef Label) {
    printCommon(E, "optional_evaluation_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitForceValueExpr(ForceValueExpr *E, StringRef Label) {
    printCommon(E, "force_value_expr", Label);
    if (E->isForceOfImplicitlyUnwrappedOptional())
      PrintWithColorRAII(OS, ExprModifierColor) << " implicit_iuo_unwrap";

    OS << '\n';
    printRec(E->getSubExpr());

    printCommonPost();
  }
  void visitOpenExistentialExpr(OpenExistentialExpr *E, StringRef Label) {
    printCommon(E, "open_existential_expr", Label);

    OS << '\n';
    printRec(E->getOpaqueValue());

    OS << '\n';
    printRec(E->getExistentialValue());

    OS << '\n';
    printRec(E->getSubExpr());

    printCommonPost();
  }
  void visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *E,
                                         StringRef Label) {
    printCommon(E, "make_temporarily_escapable_expr", Label);

    OS << '\n';
    printRec(E->getOpaqueValue());

    OS << '\n';
    printRec(E->getNonescapingClosureValue());

    OS << '\n';
    printRec(E->getSubExpr());

    printCommonPost();
  }
  void visitEditorPlaceholderExpr(EditorPlaceholderExpr *E, StringRef Label) {
    printCommon(E, "editor_placeholder_expr", Label);

    // Print the trailing angle bracket location
    if (Ctx) {
      auto TABL = E->getTrailingAngleBracketLoc();
      if (TABL.isValid()) {
        PrintWithColorRAII(OS, LocationColor) << " trailing_angle_bracket_loc=";
        TABL.print(PrintWithColorRAII(OS, LocationColor).getOS(),
                   Ctx->SourceMgr);
      }
    }

    auto *TyR = E->getPlaceholderTypeRepr();
    auto *ExpTyR = E->getTypeForExpansion();
    if (TyR) {
      OS << '\n';
      printRec(TyR, "placeholder_type_repr");
    }
    if (ExpTyR && ExpTyR != TyR) {
      OS << '\n';
      printRec(ExpTyR, "type_for_expansion");
    }

    printSemanticExpr(E->getSemanticExpr());

    printCommonPost();
  }
  void visitLazyInitializerExpr(LazyInitializerExpr *E, StringRef Label) {
    printCommon(E, "lazy_initializer_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }
  void visitObjCSelectorExpr(ObjCSelectorExpr *E, StringRef Label) {
    printCommon(E, "objc_selector_expr", Label);
    OS << " kind=" << getObjCSelectorExprKindString(E->getSelectorKind());
    PrintWithColorRAII(OS, DeclColor) << " decl=";
    printDeclRef(E->getMethod());
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }

  void visitKeyPathExpr(KeyPathExpr *E, StringRef Label) {
    printCommon(E, "keypath_expr", Label);
    printFlag(E->isObjC(), "objc");

    OS << '\n';
    Indent += 2;
    printCommon("", "components", ExprColor);
    OS << "\n";
    Indent += 2;
    for (unsigned i : indices(E->getComponents())) {
      auto &component = E->getComponents()[i];
      OS << '\n';
      switch (component.getKind()) {
      case KeyPathExpr::Component::Kind::Invalid:
        printCommon("", "invalid", ASTNodeColor);
        break;

      case KeyPathExpr::Component::Kind::OptionalChain:
        printCommon("", "optional_chain", ASTNodeColor);
        break;
        
      case KeyPathExpr::Component::Kind::OptionalForce:
        printCommon("", "optional_force", ASTNodeColor);
        break;
        
      case KeyPathExpr::Component::Kind::OptionalWrap:
        printCommon("", "optional_wrap", ASTNodeColor);
        break;
        
      case KeyPathExpr::Component::Kind::Property:
        printCommon("", "property", ASTNodeColor);
        PrintWithColorRAII(OS, DeclColor) << " decl=";
        printDeclRef(component.getDeclRef());
        break;
      
      case KeyPathExpr::Component::Kind::Subscript:
        printCommon("", "subscript", ASTNodeColor);
        PrintWithColorRAII(OS, DeclColor) << " decl='";
        printDeclRef(component.getDeclRef());
        PrintWithColorRAII(OS, DeclColor) << "'";
        break;
      
      case KeyPathExpr::Component::Kind::UnresolvedProperty:
        printCommon("", "unresolved_property", ASTNodeColor);
        PrintWithColorRAII(OS, IdentifierColor)
          << " decl_name='" << component.getUnresolvedDeclName() << "'";
        break;
        
      case KeyPathExpr::Component::Kind::UnresolvedSubscript:
        printCommon("", "unresolved_subscript", ASTNodeColor);
        printArgumentLabels(component.getSubscriptLabels());
        break;
      case KeyPathExpr::Component::Kind::Identity:
        printCommon("", "identity", ASTNodeColor);
        break;

      case KeyPathExpr::Component::Kind::TupleElement:
        printCommon("", "tuple_element", ASTNodeColor);
        PrintWithColorRAII(OS, DiscriminatorColor)
          << " #" << component.getTupleIndex();
        break;
      case KeyPathExpr::Component::Kind::DictionaryKey:
        printCommon("", "dict_key", ASTNodeColor);
        PrintWithColorRAII(OS, IdentifierColor)
          << " key='" << component.getUnresolvedDeclName() << "'";
        break;
      case KeyPathExpr::Component::Kind::CodeCompletion:
        PrintWithColorRAII(OS, ASTNodeColor) << "completion";
        break;
      }
      PrintWithColorRAII(OS, TypeColor)
        << " type='" << getTypeOfKeyPathComponent(E, i) << "'";
      if (auto indexExpr = component.getIndexExpr()) {
        OS << '\n';
        printRec(indexExpr);
      }
      printCommonPost();
    }
    Indent -= 2;

    printCommonPost();
    Indent -= 2;

    if (auto stringLiteral = E->getObjCStringLiteralExpr()) {
      OS << '\n';
      printRec(stringLiteral, "objc_string_literal");
    }
    if (!E->isObjC()) {
      if (auto root = E->getParsedRoot()) {
        OS << "\n";
        printRec(root, "parsed_root");
      }
      if (auto path = E->getParsedPath()) {
        OS << "\n";
        printRec(path, "parsed_path");
      }
    }
    printCommonPost();
  }

  void visitKeyPathDotExpr(KeyPathDotExpr *E, StringRef Label) {
    printCommon(E, "key_path_dot_expr", Label);
    printCommonPost();
  }

  void visitOneWayExpr(OneWayExpr *E, StringRef Label) {
    printCommon(E, "one_way_expr", Label);
    OS << '\n';
    printRec(E->getSubExpr());
    printCommonPost();
  }

  void visitTapExpr(TapExpr *E, StringRef Label) {
    if (!Ctx)
      Ctx = &E->getVar()->getDeclContext()->getASTContext();

    printCommon(E, "tap_expr", Label);
    PrintWithColorRAII(OS, DeclColor) << " var=";
    printDeclRef(E->getVar());
    OS << '\n';

    printRec(E->getSubExpr());
    OS << '\n';

    printRec(E->getBody());
    printCommonPost();
  }
};

} // end anonymous namespace

void Expr::dump() const {
  dump(llvm::errs());
  llvm::errs() << "\n";
}

void Expr::dump(raw_ostream &OS, llvm::function_ref<Type(Expr *)> getTypeOfExpr,
                llvm::function_ref<Type(TypeRepr *)> getTypeOfTypeRepr,
                llvm::function_ref<Type(KeyPathExpr *E, unsigned index)>
                    getTypeOfKeyPathComponent,
                unsigned Indent) const {
  ExprTypeDelegate delegate(getTypeOfExpr, getTypeOfTypeRepr,
                            getTypeOfKeyPathComponent);
  PrintExpr(getASTContextFromType(getTypeOfExpr(const_cast<Expr *>(this))),
            OS, delegate, Indent)
      .visit(const_cast<Expr *>(this), "");
}

void Expr::dump(raw_ostream &OS, unsigned Indent) const {
  ExprTypeDelegate delegate;
  PrintExpr(getASTContextFromType(getType()), OS, delegate, Indent)
      .visit(const_cast<Expr *>(this), "");
}

void Expr::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  // FIXME: Fully use the ASTPrinter.
  llvm::SmallString<128> Str;
  llvm::raw_svector_ostream OS(Str);
  dump(OS);
  Printer << OS.str();
}

//===----------------------------------------------------------------------===//
// Printing for TypeRepr and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
class PrintTypeRepr : public TypeReprVisitor<PrintTypeRepr, void, StringRef> {
public:
  ASTContext *Ctx;
  raw_ostream &OS;
  ExprTypeDelegate &Delegate;
  unsigned Indent;

  explicit PrintTypeRepr(ASTContext *ctx, raw_ostream &os,
                         ExprTypeDelegate &delegate, unsigned indent)
    : Ctx(ctx), OS(os), Delegate(delegate), Indent(indent) { }

  void printRec(Decl *D, StringRef Label = "");
  void printRec(Expr *E, StringRef Label = "");
  void printRec(TypeRepr *T, StringRef Label = "");

  // Print a single flag.
  void printFlag(StringRef label) {
    PrintWithColorRAII(OS, TypeFieldColor) << " " << label;
  }

  // Print a single flag if it is set.
  void printFlag(bool isSet, StringRef label) {
    if (isSet)
      printFlag(label);
  }

  void printCommon(const char *name, StringRef label, TerminalColor Color) {
    OS.indent(Indent);
    PrintWithColorRAII(OS, ParenthesisColor) << '(';
    PrintWithColorRAII(OS, LabelColor) << label;
    if (!label.empty()) {  // not null/empty
      OS << "=";
    }

    PrintWithColorRAII(OS, Color) << name;
  }

  raw_ostream &printCommon(const char *Name, StringRef Label) {
    printCommon(Name, Label, TypeReprColor);
    return OS;
  }

  void printCommonPost() {
    PrintWithColorRAII(OS, ParenthesisColor) << ')';
  }

  void visitErrorTypeRepr(ErrorTypeRepr *T, StringRef Label) {
    printCommon("type_error", Label);
  }

  void visitAttributedTypeRepr(AttributedTypeRepr *T, StringRef Label) {
    printCommon("type_attributed", Label) << " attrs=";
    T->printAttrs(OS);
    OS << '\n';
    printRec(T->getTypeRepr());
  }

  void visitIdentTypeRepr(IdentTypeRepr *T, StringRef Label) {
    printCommon("type_ident", Label);
    Indent += 2;
    for (auto comp : T->getComponentRange()) {
      OS << '\n';
      printCommon("component", "");
      PrintWithColorRAII(OS, IdentifierColor)
        << " id='" << comp->getNameRef() << '\'';
      OS << " bind=";
      if (comp->isBound())
        comp->getBoundDecl()->dumpRef(OS);
      else OS << "none";
      printCommonPost();
      if (auto GenIdT = dyn_cast<GenericIdentTypeRepr>(comp)) {
        for (auto genArg : GenIdT->getGenericArgs()) {
          OS << '\n';
          printRec(genArg);
        }
      }
    }
    printCommonPost();
    Indent -= 2;
  }

  void visitFunctionTypeRepr(FunctionTypeRepr *T, StringRef Label) {
    printCommon("type_function", Label);
    OS << '\n'; printRec(T->getArgsTypeRepr());
    if (T->isAsync())
      OS << " async ";
    if (T->isThrowing())
      OS << " throws ";
    OS << '\n'; printRec(T->getResultTypeRepr());
    printCommonPost();
  }

  void visitArrayTypeRepr(ArrayTypeRepr *T, StringRef Label) {
    printCommon("type_array", Label) << '\n';
    printRec(T->getBase());
    printCommonPost();
  }

  void visitDictionaryTypeRepr(DictionaryTypeRepr *T, StringRef Label) {
    printCommon("type_dictionary", Label) << '\n';
    printRec(T->getKey());
    OS << '\n';
    printRec(T->getValue());
    printCommonPost();
  }

  void visitTupleTypeRepr(TupleTypeRepr *T, StringRef Label) {
    printCommon("type_tuple", Label);

    for (auto elem : T->getElements()) {
      OS << '\n';
      if (elem.Name.empty())
        printRec(elem.Type);
      else
        printRec(elem.Type, elem.Name.str());
    }
    printCommonPost();
  }

  void visitCompositionTypeRepr(CompositionTypeRepr *T, StringRef Label) {
    printCommon("type_composite", Label);
    for (auto elem : T->getTypes()) {
      OS << '\n';
      printRec(elem);
    }
    printCommonPost();
  }

  void visitMetatypeTypeRepr(MetatypeTypeRepr *T, StringRef Label) {
    printCommon("type_metatype", Label);
    OS << '\n';
    printRec(T->getBase());
    printCommonPost();
  }

  void visitProtocolTypeRepr(ProtocolTypeRepr *T, StringRef Label) {
    printCommon("type_protocol", Label);
    OS << '\n';
    printRec(T->getBase());
    printCommonPost();
  }

  void visitInOutTypeRepr(InOutTypeRepr *T, StringRef Label) {
    printCommon("type_inout", Label);
    OS << '\n';
    printRec(T->getBase());
    printCommonPost();
  }
  
  void visitSharedTypeRepr(SharedTypeRepr *T, StringRef Label) {
    printCommon("type_shared", Label);
    OS << '\n';
    printRec(T->getBase());
    printCommonPost();
  }

  void visitOwnedTypeRepr(OwnedTypeRepr *T, StringRef Label) {
    printCommon("type_owned", Label);
    OS << '\n';
    printRec(T->getBase());
    printCommonPost();
  }

  void visitIsolatedTypeRepr(IsolatedTypeRepr *T, StringRef Label) {
    printCommon("isolated", Label);
    OS << '\n';
    printRec(T->getBase());
    printCommonPost();
  }

  void visitOptionalTypeRepr(OptionalTypeRepr *T, StringRef Label) {
    printCommon("type_optional", Label);
    OS << '\n';
    printRec(T->getBase());
    printCommonPost();
  }

  void visitImplicitlyUnwrappedOptionalTypeRepr(
      ImplicitlyUnwrappedOptionalTypeRepr *T, StringRef Label) {
    printCommon("type_implicitly_unwrapped_optional", Label);
    OS << '\n';
    printRec(T->getBase());
    printCommonPost();
  }

  void visitOpaqueReturnTypeRepr(OpaqueReturnTypeRepr *T, StringRef Label) {
    printCommon("type_opaque_return", Label);
    OS << '\n';
    printRec(T->getConstraint());
    printCommonPost();
  }

  void visitNamedOpaqueReturnTypeRepr(NamedOpaqueReturnTypeRepr *T, StringRef Label) {
    printCommon("type_named_opaque_return", Label);
    OS << '\n';
    printRec(T->getBase());
    printCommonPost();
  }

  void visitPlaceholderTypeRepr(PlaceholderTypeRepr *T, StringRef Label) {
    printCommon("type_placeholder", Label);
    printCommonPost();
  }

  void visitFixedTypeRepr(FixedTypeRepr *T, StringRef Label) {
    printCommon("type_fixed", Label);
    auto Ty = T->getType();
    if (!Ctx && Ty)
      Ctx = &Ty->getASTContext();

    if (Ctx) {
      if (T->getLoc().isValid()) {
        OS << " location=@";
        T->getLoc().print(OS, Ctx->SourceMgr);
      } else {
        OS << " location=<<invalid>>";
      }
    }
    OS << " type="; Ty.dump(OS);
    printCommonPost();
  }

  void visitSILBoxTypeRepr(SILBoxTypeRepr *T, StringRef Label) {
    printCommon("sil_box", Label);
    Indent += 2;

    ArrayRef<SILBoxTypeReprField> Fields = T->getFields();
    for (unsigned i = 0, end = Fields.size(); i != end; ++i) {
      OS << '\n';
      printCommon("sil_box_field", "");
      printFlag(Fields[i].isMutable(), "mutable");
      OS << '\n';
      printRec(Fields[i].getFieldType());
      printCommonPost();
    }

    for (auto genArg : T->getGenericArguments()) {
      OS << '\n';
      printRec(genArg);
    }

    printCommonPost();
    Indent -= 2;
  }
};

} // end anonymous namespace

void TypeRepr::dump() const {
  ExprTypeDelegate delegate;
  PrintTypeRepr(nullptr, llvm::errs(), delegate, 0)
      .visit(const_cast<TypeRepr*>(this), "");
  llvm::errs() << '\n';
}

// Recursive helpers to avoid infinite recursion for recursive protocol
// conformances.
static void dumpProtocolConformanceRec(
    const ProtocolConformance *conformance, llvm::raw_ostream &out,
    unsigned indent,
    llvm::SmallPtrSetImpl<const ProtocolConformance *> &visited);

static void dumpProtocolConformanceRefRec(
    const ProtocolConformanceRef conformance, llvm::raw_ostream &out,
    unsigned indent,
    llvm::SmallPtrSetImpl<const ProtocolConformance *> &visited) {
  if (conformance.isInvalid()) {
    out.indent(indent) << "(invalid_conformance)";
  } else if (conformance.isConcrete()) {
    dumpProtocolConformanceRec(conformance.getConcrete(), out, indent, visited);
  } else {
    out.indent(indent) << "(abstract_conformance protocol="
                       << conformance.getAbstract()->getName();
    PrintWithColorRAII(out, ParenthesisColor) << ')';
  }
}

static void dumpProtocolConformanceRec(
    const ProtocolConformance *conformance, llvm::raw_ostream &out,
    unsigned indent,
    llvm::SmallPtrSetImpl<const ProtocolConformance *> &visited) {
  // A recursive conformance shouldn't have its contents printed, or there's
  // infinite recursion. (This also avoids printing things that occur multiple
  // times in a conformance hierarchy.)
  auto shouldPrintDetails = visited.insert(conformance).second;

  auto printCommon = [&](StringRef kind) {
    out.indent(indent);
    PrintWithColorRAII(out, ParenthesisColor) << '(';
    out << kind << "_conformance type=" << conformance->getType()
        << " protocol=" << conformance->getProtocol()->getName();

    if (!shouldPrintDetails)
      out << " (details printed above)";
  };

  switch (conformance->getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto normal = cast<NormalProtocolConformance>(conformance);

    printCommon("normal");
    if (!shouldPrintDetails)
      break;

    // Maybe print information about the conforming context?
    if (normal->isLazilyLoaded()) {
      out << " lazy";
    } else {
      normal->forEachTypeWitness(
          [&](const AssociatedTypeDecl *req, Type ty,
              const TypeDecl *) -> bool {
            out << '\n';
            out.indent(indent + 2);
            PrintWithColorRAII(out, ParenthesisColor) << '(';
            out << "assoc_type req=" << req->getName() << " type=";
            PrintWithColorRAII(out, TypeColor) << Type(ty->getDesugaredType());
            PrintWithColorRAII(out, ParenthesisColor) << ')';
            return false;
          });
      normal->forEachValueWitness([&](const ValueDecl *req,
                                      Witness witness) {
        out << '\n';
        out.indent(indent + 2);
        PrintWithColorRAII(out, ParenthesisColor) << '(';
        out << "value req=" << req->getName() << " witness=";
        if (!witness) {
          out << "(none)";
        } else if (witness.getDecl() == req) {
          out << "(dynamic)";
        } else {
          witness.getDecl()->dumpRef(out);
        }
        PrintWithColorRAII(out, ParenthesisColor) << ')';
      });

      for (auto sigConf : normal->getSignatureConformances()) {
        out << '\n';
        dumpProtocolConformanceRefRec(sigConf, out, indent + 2, visited);
      }
    }

    if (auto condReqs = normal->getConditionalRequirementsIfAvailable()) {
      for (auto requirement : *condReqs) {
        out << '\n';
        out.indent(indent + 2);
        requirement.dump(out);
      }
    } else {
      out << '\n';
      out.indent(indent + 2);
      out << "(conditional requirements unable to be computed)";
    }
    break;
  }

  case ProtocolConformanceKind::Self: {
    printCommon("self");
    break;
  }

  case ProtocolConformanceKind::Inherited: {
    auto conf = cast<InheritedProtocolConformance>(conformance);
    printCommon("inherited");
    if (!shouldPrintDetails)
      break;

    out << '\n';
    dumpProtocolConformanceRec(conf->getInheritedConformance(), out, indent + 2,
                               visited);
    break;
  }

  case ProtocolConformanceKind::Specialized: {
    auto conf = cast<SpecializedProtocolConformance>(conformance);
    printCommon("specialized");
    if (!shouldPrintDetails)
      break;

    out << '\n';
    dumpSubstitutionMapRec(conf->getSubstitutionMap(), out,
                           SubstitutionMap::DumpStyle::Full, indent + 2,
                           visited);
    out << '\n';
    if (auto condReqs = conf->getConditionalRequirementsIfAvailableOrCached(
            /*computeIfPossible=*/false)) {
      for (auto subReq : *condReqs) {
        out.indent(indent + 2);
        subReq.dump(out);
        out << '\n';
      }
    } else {
      out.indent(indent + 2);
      out << "(conditional requirements unable to be computed)\n";
    }
    dumpProtocolConformanceRec(conf->getGenericConformance(), out, indent + 2,
                               visited);
    break;
  }
  }

  PrintWithColorRAII(out, ParenthesisColor) << ')';
}

static void dumpSubstitutionMapRec(
    SubstitutionMap map, llvm::raw_ostream &out,
    SubstitutionMap::DumpStyle style, unsigned indent,
    llvm::SmallPtrSetImpl<const ProtocolConformance *> &visited) {
  auto genericSig = map.getGenericSignature();
  out.indent(indent);

  auto printParen = [&](char p) {
    PrintWithColorRAII(out, ParenthesisColor) << p;
  };
  printParen('(');
  SWIFT_DEFER { printParen(')'); };
  out << "substitution_map generic_signature=";
  if (genericSig.isNull()) {
    out << "<nullptr>";
    return;
  }

  genericSig->print(out);
  auto genericParams = genericSig->getGenericParams();
  auto replacementTypes =
      static_cast<const SubstitutionMap &>(map).getReplacementTypesBuffer();
  for (unsigned i : indices(genericParams)) {
    if (style == SubstitutionMap::DumpStyle::Minimal) {
      out << " ";
    } else {
      out << "\n";
      out.indent(indent + 2);
    }
    printParen('(');
    out << "substitution ";
    genericParams[i]->print(out);
    out << " -> ";
    if (replacementTypes[i]) {
      PrintOptions opts;
      opts.PrintForSIL = true;
      replacementTypes[i]->print(out, opts);
    }
    else
      out << "<<unresolved concrete type>>";
    printParen(')');
  }
  // A minimal dump doesn't need the details about the conformances, a lot of
  // that info can be inferred from the signature.
  if (style == SubstitutionMap::DumpStyle::Minimal)
    return;

  auto conformances = map.getConformances();
  for (const auto &req : genericSig->getRequirements()) {
    if (req.getKind() != RequirementKind::Conformance)
      continue;

    out << "\n";
    out.indent(indent + 2);
    printParen('(');
    out << "conformance type=";
    req.getFirstType()->print(out);
    out << "\n";
    dumpProtocolConformanceRefRec(conformances.front(), out, indent + 4,
                                  visited);

    printParen(')');
    conformances = conformances.slice(1);
  }
}

void ProtocolConformanceRef::dump() const {
  dump(llvm::errs());
  llvm::errs() << '\n';
}

void ProtocolConformanceRef::dump(llvm::raw_ostream &out, unsigned indent,
                                  bool details) const {
  llvm::SmallPtrSet<const ProtocolConformance *, 8> visited;
  if (!details && isConcrete())
    visited.insert(getConcrete());

  dumpProtocolConformanceRefRec(*this, out, indent, visited);
}

void ProtocolConformanceRef::print(llvm::raw_ostream &out) const {
  llvm::SmallPtrSet<const ProtocolConformance *, 8> visited;
  dumpProtocolConformanceRefRec(*this, out, 0, visited);

}

void ProtocolConformance::dump() const {
  auto &out = llvm::errs();
  dump(out);
  out << '\n';
}

void ProtocolConformance::dump(llvm::raw_ostream &out, unsigned indent) const {
  llvm::SmallPtrSet<const ProtocolConformance *, 8> visited;
  dumpProtocolConformanceRec(this, out, indent, visited);
}

void SubstitutionMap::dump(llvm::raw_ostream &out, DumpStyle style,
                           unsigned indent) const {
  llvm::SmallPtrSet<const ProtocolConformance *, 8> visited;
  dumpSubstitutionMapRec(*this, out, style, indent, visited);
}

void SubstitutionMap::dump() const {
  dump(llvm::errs());
  llvm::errs() << "\n";
}

//===----------------------------------------------------------------------===//
// Dumping for Types.
//===----------------------------------------------------------------------===//

namespace {
  class PrintType : public TypeVisitor<PrintType, void, StringRef> {
    ASTContext *Ctx;
    raw_ostream &OS;
    ExprTypeDelegate &Delegate;
    unsigned Indent;

    void printCommon(const char *name, StringRef label, TerminalColor Color) {
      OS.indent(Indent);
      PrintWithColorRAII(OS, ParenthesisColor) << '(';
      PrintWithColorRAII(OS, LabelColor) << label;
      if (!label.empty()) {  // not null/empty
        OS << "=";
      }

      PrintWithColorRAII(OS, Color) << name;
    }

    raw_ostream &printCommon(const char *name, StringRef label) {
      printCommon(name, label, TypeColor);
      return OS;
    }

    void printCommonPost() {
      PrintWithColorRAII(OS, ParenthesisColor) << ')';
    }

    // Print a single flag.
    void printFlag(StringRef label) {
      PrintWithColorRAII(OS, TypeFieldColor) << " " << label;
    }

    // Print a single flag if it is set.
    void printFlag(bool isSet, StringRef label) {
      if (isSet)
        printFlag(label);
    }

    // Print a field with a value.
    template<typename T>
    void print(const T &value, StringRef label) {
      OS << " ";
      PrintWithColorRAII(OS, TypeFieldColor) << label;
      if (!label.empty())
        OS << "=";
      OS << value;
    }

    void dumpParameterFlags(ParameterTypeFlags paramFlags) {
      printFlag(paramFlags.isVariadic(), "vararg");
      printFlag(paramFlags.isAutoClosure(), "autoclosure");
      printFlag(paramFlags.isNonEphemeral(), "nonEphemeral");
      switch (paramFlags.getValueOwnership()) {
      case ValueOwnership::Default: break;
      case ValueOwnership::Owned: printFlag("owned"); break;
      case ValueOwnership::Shared: printFlag("shared"); break;
      case ValueOwnership::InOut: printFlag("inout"); break;
      }
    }

  public:
    explicit PrintType(ASTContext *ctx, raw_ostream &os,
                       ExprTypeDelegate &delegate, unsigned indent)
      : Ctx(ctx), OS(os), Delegate(delegate), Indent(indent) { }

    void visit(Type T, StringRef Label) {
      if (!T.isNull())
        TypeVisitor<PrintType, void, StringRef>::visit(T, Label);
      else {
        printCommon("<<null>>", Label, TypeColor);
        printCommonPost();
      }
    }

    void printRec(Type type, StringRef label = "");

#define TRIVIAL_TYPE_PRINTER(Class,Name)                                       \
    void visit##Class##Type(Class##Type *T, StringRef label) {                 \
      printCommon(#Name "_type", label);                                       \
      printCommonPost();                                                       \
    }

    void visitErrorType(ErrorType *T, StringRef label) {
      printCommon("error_type", label);
      if (auto originalType = T->getOriginalType())
        printRec(originalType, "original_type");
      printCommonPost();
    }

    TRIVIAL_TYPE_PRINTER(Unresolved, unresolved)

    void visitPlaceholderType(PlaceholderType *T, StringRef label) {
      printCommon("placeholder_type", label);
      auto originator = T->getOriginator();
      if (auto *typeVar = originator.dyn_cast<TypeVariableType *>()) {
        printRec(typeVar, "type_variable");
      } else if (auto *VD = originator.dyn_cast<VarDecl *>()) {
        VD->dumpRef(PrintWithColorRAII(OS, DeclColor).getOS());
      } else if (auto *EE = originator.dyn_cast<ErrorExpr *>()) {
        printFlag("error_expr");
      } else if (auto *DMT = originator.dyn_cast<DependentMemberType *>()) {
        printRec(DMT, "dependent_member_type");
      } else {
        printFlag("placeholder_type_repr");
      }
      printCommonPost();
    }

    void visitBuiltinIntegerType(BuiltinIntegerType *T, StringRef label) {
      printCommon("builtin_integer_type", label);
      if (T->isFixedWidth())
        print(T->getFixedWidth(), "bit_width");
      else
        printFlag("word_sized");
      printCommonPost();
    }

    void visitBuiltinFloatType(BuiltinFloatType *T, StringRef label) {
      printCommon("builtin_float_type", label);
      print(T->getBitWidth(), "bit_width");
      printCommonPost();
    }

    TRIVIAL_TYPE_PRINTER(BuiltinIntegerLiteral, builtin_integer_literal)
    TRIVIAL_TYPE_PRINTER(BuiltinJob, builtin_job)
    TRIVIAL_TYPE_PRINTER(BuiltinExecutor, builtin_executor_ref)
    TRIVIAL_TYPE_PRINTER(BuiltinDefaultActorStorage, builtin_default_actor_storage)
    TRIVIAL_TYPE_PRINTER(BuiltinRawPointer, builtin_raw_pointer)
    TRIVIAL_TYPE_PRINTER(BuiltinRawUnsafeContinuation, builtin_raw_unsafe_continuation)
    TRIVIAL_TYPE_PRINTER(BuiltinNativeObject, builtin_native_object)
    TRIVIAL_TYPE_PRINTER(BuiltinBridgeObject, builtin_bridge_object)
    TRIVIAL_TYPE_PRINTER(BuiltinUnsafeValueBuffer, builtin_unsafe_value_buffer)
    TRIVIAL_TYPE_PRINTER(SILToken, sil_token)

    void visitBuiltinVectorType(BuiltinVectorType *T, StringRef label) {
      printCommon("builtin_vector_type", label);
      print(T->getNumElements(), "num_elements");
      printRec(T->getElementType());
      printCommonPost();
    }

    void visitTypeAliasType(TypeAliasType *T, StringRef label) {
      printCommon("type_alias_type", label);
      print(T->getDecl()->printRef(), "decl");
      PrintWithColorRAII(OS, TypeColor) << " underlying=";
      if (auto underlying = T->getSinglyDesugaredType()) {
        PrintWithColorRAII(OS, TypeColor)
          << "'" << underlying->getString() << "'";
      } else {
        PrintWithColorRAII(OS, TypeColor) << "<<<unresolved>>>";
      }
      if (T->getParent())
        printRec(T->getParent(), "parent");

      for (const auto &arg : T->getDirectGenericArgs())
        printRec(arg);
      printCommonPost();
    }

    void visitParenType(ParenType *T, StringRef label) {
      printCommon("paren_type", label);
      dumpParameterFlags(T->getParameterFlags());
      printRec(T->getUnderlyingType());
      printCommonPost();
    }

    void visitTupleType(TupleType *T, StringRef label) {
      printCommon("tuple_type", label);
      print(T->getNumElements(), "num_elements");
      Indent += 2;
      for (const auto &elt : T->getElements()) {
        OS << "\n";
        printCommon("", "tuple_type_elt", TypeFieldColor);
        if (elt.hasName())
          print(elt.getName().str(), "name");
        dumpParameterFlags(elt.getParameterFlags());
        printRec(elt.getType());
        printCommonPost();
      }
      Indent -= 2;
      printCommonPost();
    }

#define REF_STORAGE(Name, name, ...) \
    void visit##Name##StorageType(Name##StorageType *T, StringRef label) { \
      printCommon(#name "_storage_type", label); \
      printRec(T->getReferentType()); \
      printCommonPost(); \
    }
#include "swift/AST/ReferenceStorage.def"

    void visitEnumType(EnumType *T, StringRef label) {
      printCommon("enum_type", label);
      print(T->getDecl()->printRef(), "decl");
      if (T->getParent())
        printRec(T->getParent(), "parent");
      printCommonPost();
    }

    void visitStructType(StructType *T, StringRef label) {
      printCommon("struct_type", label);
      print(T->getDecl()->printRef(), "decl");
      if (T->getParent())
        printRec(T->getParent(), "parent");
      printCommonPost();
    }

    void visitClassType(ClassType *T, StringRef label) {
      printCommon("class_type", label);
      print(T->getDecl()->printRef(), "decl");
      if (T->getParent())
        printRec(T->getParent(), "parent");
      printCommonPost();
    }

    void visitProtocolType(ProtocolType *T, StringRef label) {
      printCommon("protocol_type", label);
      print(T->getDecl()->printRef(), "decl");
      if (T->getParent())
        printRec(T->getParent(), "parent");
      printCommonPost();
    }

    void visitMetatypeType(MetatypeType *T, StringRef label) {
      printCommon("metatype_type", label);
      if (T->hasRepresentation())
        OS << " " << getMetatypeRepresentationString(T->getRepresentation());
      printRec(T->getInstanceType());
      printCommonPost();
    }

    void visitExistentialMetatypeType(ExistentialMetatypeType *T,
                                      StringRef label) {
      printCommon("existential_metatype_type", label);
      if (T->hasRepresentation())
        OS << " " << getMetatypeRepresentationString(T->getRepresentation());
      printRec(T->getInstanceType());
      printCommonPost();
    }

    void visitModuleType(ModuleType *T, StringRef label) {
      printCommon("module_type", label);
      print(T->getModule()->getName(), "module");
      printCommonPost();
    }

    void visitDynamicSelfType(DynamicSelfType *T, StringRef label) {
      printCommon("dynamic_self_type", label);
      printRec(T->getSelfType());
      printCommonPost();
    }
    
    void printArchetypeCommon(ArchetypeType *T,
                              const char *className,
                              StringRef label) {
      printCommon(className, label);
      print(static_cast<void *>(T), "address");
      printFlag(T->requiresClass(), "class");
      if (auto layout = T->getLayoutConstraint()) {
        OS << " layout=";
        layout->print(OS);
      }
      for (auto proto : T->getConformsTo())
        print(proto->printRef(), "conforms_to");
      if (auto superclass = T->getSuperclass())
        printRec(superclass, "superclass");
    }
    
    void printArchetypeNestedTypes(ArchetypeType *T) {
      Indent += 2;
      for (auto nestedType : T->getKnownNestedTypes()) {
        OS << "\n";
        printCommon("nested_type", "", TypeFieldColor);
        OS << " " << nestedType.first.str() << "=";
        if (!nestedType.second) {
          PrintWithColorRAII(OS, TypeColor) << "<<unresolved>>";
        } else {
          PrintWithColorRAII(OS, TypeColor) << nestedType.second.getString();
        }
        printCommonPost();
      }
      Indent -= 2;
    }

    void visitPrimaryArchetypeType(PrimaryArchetypeType *T, StringRef label) {
      printArchetypeCommon(T, "primary_archetype_type", label);
      print(T->getFullName(), "name");
      OS << "\n";
      printArchetypeNestedTypes(T);
      printCommonPost();
    }
    void visitNestedArchetypeType(NestedArchetypeType *T, StringRef label) {
      printArchetypeCommon(T, "nested_archetype_type", label);
      print(T->getFullName(), "name");
      print(T->getParent(), "parent");
      print(T->getAssocType()->printRef(), "assoc_type");
      printArchetypeNestedTypes(T);
      printCommonPost();
    }
    void visitOpenedArchetypeType(OpenedArchetypeType *T, StringRef label) {
      printArchetypeCommon(T, "opened_archetype_type", label);
      printRec(T->getOpenedExistentialType(), "opened_existential");
      print(T->getOpenedExistentialID(), "opened_existential_id");
      printArchetypeNestedTypes(T);
      printCommonPost();
    }
    void visitOpaqueTypeArchetypeType(OpaqueTypeArchetypeType *T,
                                      StringRef label) {
      printArchetypeCommon(T, "opaque_type", label);
      print(T->getDecl()->getNamingDecl()->printRef(), "decl");
      if (!T->getSubstitutions().empty()) {
        OS << '\n';
        SmallPtrSet<const ProtocolConformance *, 4> Dumped;
        dumpSubstitutionMapRec(T->getSubstitutions(), OS,
                               SubstitutionMap::DumpStyle::Full,
                               Indent + 2, Dumped);
      }
      printArchetypeNestedTypes(T);
      printCommonPost();
    }

    void visitGenericTypeParamType(GenericTypeParamType *T, StringRef label) {
      printCommon("generic_type_param_type", label);
      print(T->getDepth(), "depth");
      print(T->getIndex(), "index");
      if (auto decl = T->getDecl())
        print(decl->printRef(), "decl");
      printCommonPost();
    }

    void visitDependentMemberType(DependentMemberType *T, StringRef label) {
      printCommon("dependent_member_type", label);
      if (auto assocType = T->getAssocType()) {
        print(assocType->printRef(), "assoc_type");
      } else {
        print(T->getName(), "name");
      }
      printRec(T->getBase(), "base");
      printCommonPost();
    }

    void printAnyFunctionParams(ArrayRef<AnyFunctionType::Param> params,
                                StringRef label) {
      printCommon("function_params", label);
      print(params.size(), "num_params");
      Indent += 2;
      for (const auto &param : params) {
        OS << "\n";
        printCommon("", "param", TypeFieldColor);
        if (param.hasLabel())
          print(param.getLabel().str(), "api_name");
        if (param.hasInternalLabel())
          print(param.getInternalLabel().str(), "internal_name");
        dumpParameterFlags(param.getParameterFlags());
        printRec(param.getPlainType());
        printCommonPost();
      }
      Indent -= 2;
      printCommonPost();
    }

    void printAnyFunctionTypeCommon(AnyFunctionType *T, const char *name,
                                    StringRef label) {
      printCommon(name, label);
      SILFunctionType::Representation representation =
        T->getExtInfo().getSILRepresentation();

      if (representation != SILFunctionType::Representation::Thick)
        print(getSILFunctionTypeRepresentationString(representation),
                   "representation");

      printFlag(!T->isNoEscape(), "escaping");
      printFlag(T->isSendable(), "Sendable");
      printFlag(T->isAsync(), "async");
      printFlag(T->isThrowing(), "throws");

      OS << "\n";
      Indent += 2;
      // [TODO: Improve-Clang-type-printing]
      if (!T->getClangTypeInfo().empty()) {
        std::string s;
        llvm::raw_string_ostream os(s);
        auto &clangCtx = Ctx->getClangModuleLoader()->getClangASTContext();
        T->getClangTypeInfo().dump(os, clangCtx);
        print(os.str(), "clang_type");
      }

      if (Type globalActor = T->getGlobalActor()) {
        print(globalActor.getString(), "global_actor");
      }

      printAnyFunctionParams(T->getParams(), "input");
      Indent -=2;
      printRec(T->getResult(), "output");
    }

    void visitFunctionType(FunctionType *T, StringRef label) {
      printAnyFunctionTypeCommon(T, "function_type", label);
      printCommonPost();
    }

    void visitGenericFunctionType(GenericFunctionType *T, StringRef label) {
      printAnyFunctionTypeCommon(T, "generic_function_type", label);

      Indent += 2;
      OS << "\n";
      // FIXME: generic signature dumping needs improvement
      printCommon("", "generic_sig", ASTNodeColor);
      OS << T->getGenericSignature()->getAsString();
      printCommonPost();
      Indent -= 2;

      printCommonPost();
    }

    void visitSILFunctionType(SILFunctionType *T, StringRef label) {
      printCommon("sil_function_type", label);
      print(T->getString(), "type");

      for (auto param : T->getParameters()) {
        printRec(param.getInterfaceType(), "input");
      }
      for (auto yield : T->getYields()) {
        printRec(yield.getInterfaceType(), "yield");
      }
      for (auto result : T->getResults()) {
        printRec(result.getInterfaceType(), "result");
      }
      if (auto error  = T->getOptionalErrorResult()) {
        printRec(error->getInterfaceType(), "error");
      }
      OS << '\n';
      T->getPatternSubstitutions().dump(OS, SubstitutionMap::DumpStyle::Full,
                                        Indent+2);
      OS << '\n';
      T->getInvocationSubstitutions().dump(OS, SubstitutionMap::DumpStyle::Full,
                                           Indent+2);
      // [TODO: Improve-Clang-type-printing]
      if (!T->getClangTypeInfo().empty()) {
        std::string s;
        llvm::raw_string_ostream os(s);
        auto &clangCtx = Ctx->getClangModuleLoader()->getClangASTContext();
        T->getClangTypeInfo().dump(os, clangCtx);
        print(os.str(), "clang_type");
      }
      printCommonPost();
    }

    void visitSILBlockStorageType(SILBlockStorageType *T, StringRef label) {
      printCommon("sil_block_storage_type", label);
      printRec(T->getCaptureType());
      printCommonPost();
    }

    void visitSILBoxType(SILBoxType *T, StringRef label) {
      printCommon("sil_box_type", label);
      // FIXME: Print the structure of the type.
      print(T->getString(), "type");
      printCommonPost();
    }

    void visitArraySliceType(ArraySliceType *T, StringRef label) {
      printCommon("array_slice_type", label);
      printRec(T->getBaseType());
      printCommonPost();
    }

    void visitOptionalType(OptionalType *T, StringRef label) {
      printCommon("optional_type", label);
      printRec(T->getBaseType());
      printCommonPost();
    }

    void visitDictionaryType(DictionaryType *T, StringRef label) {
      printCommon("dictionary_type", label);
      printRec(T->getKeyType(), "key");
      printRec(T->getValueType(), "value");
      printCommonPost();
    }

    void visitProtocolCompositionType(ProtocolCompositionType *T,
                                      StringRef label) {
      printCommon("protocol_composition_type", label);
      printFlag(T->hasExplicitAnyObject(), "any_object");
      for (auto proto : T->getMembers()) {
        printRec(proto);
      }
      printCommonPost();
    }

    void visitLValueType(LValueType *T, StringRef label) {
      printCommon("lvalue_type", label);
      printRec(T->getObjectType());
      printCommonPost();
    }

    void visitInOutType(InOutType *T, StringRef label) {
      printCommon("inout_type", label);
      printRec(T->getObjectType());
      printCommonPost();
    }

    void visitUnboundGenericType(UnboundGenericType *T, StringRef label) {
      printCommon("unbound_generic_type", label);
      print(T->getDecl()->printRef(), "decl");
      if (T->getParent())
        printRec(T->getParent(), "parent");
      printCommonPost();
    }

    void visitBoundGenericClassType(BoundGenericClassType *T, StringRef label) {
      printCommon("bound_generic_class_type", label);
      print(T->getDecl()->printRef(), "decl");
      if (T->getParent())
        printRec(T->getParent(), "parent");
      for (auto arg : T->getGenericArgs())
        printRec(arg);
      printCommonPost();
    }

    void visitBoundGenericStructType(BoundGenericStructType *T,
                                     StringRef label) {
      printCommon("bound_generic_struct_type", label);
      print(T->getDecl()->printRef(), "decl");
      if (T->getParent())
        printRec(T->getParent(), "parent");
      for (auto arg : T->getGenericArgs())
        printRec(arg);
      printCommonPost();
    }

    void visitBoundGenericEnumType(BoundGenericEnumType *T, StringRef label) {
      printCommon("bound_generic_enum_type", label);
      print(T->getDecl()->printRef(), "decl");
      if (T->getParent())
        printRec(T->getParent(), "parent");
      for (auto arg : T->getGenericArgs())
        printRec(arg);
      printCommonPost();
    }

    void visitTypeVariableType(TypeVariableType *T, StringRef label) {
      printCommon("type_variable_type", label);
      print(T->getID(), "id");
      printCommonPost();
    }

#undef TRIVIAL_TYPE_PRINTER
  };
} // end anonymous namespace

void Type::dump() const {
  dump(llvm::errs());
}

void Type::dump(raw_ostream &os, unsigned indent) const {
  #if SWIFT_BUILD_ONLY_SYNTAXPARSERLIB
    return; // not needed for the parser library.
  #endif

  ExprTypeDelegate delegate;
  PrintType(getASTContextFromType(*this), os, delegate, indent)
      .visit(*this, "");
  os << "\n";
}

void TypeBase::dump() const {
  // Make sure to print type variables.
  Type(const_cast<TypeBase *>(this)).dump();
}

void TypeBase::dump(raw_ostream &os, unsigned indent) const {
  Type(const_cast<TypeBase *>(this)).dump(os, indent);
}

void GenericSignatureImpl::dump() const {
  GenericSignature(const_cast<GenericSignatureImpl *>(this)).dump();
}

void GenericEnvironment::dump(raw_ostream &os) const {
  os << "Generic environment:\n";
  for (auto gp : getGenericParams()) {
    gp->dump(os);
    mapTypeIntoContext(gp)->dump(os);
  }
  os << "Generic parameters:\n";
  for (auto paramTy : getGenericParams())
    paramTy->dump(os);
}

void GenericEnvironment::dump() const {
  dump(llvm::errs());
}

StringRef swift::getAccessorKindString(AccessorKind value) {
  switch (value) {
#define ACCESSOR(ID)
#define SINGLETON_ACCESSOR(ID, KEYWORD) \
  case AccessorKind::ID: return #KEYWORD;
#include "swift/AST/AccessorKinds.def"
  }

  llvm_unreachable("Unhandled AccessorKind in switch.");
}

void StableSerializationPath::dump() const {
  dump(llvm::errs());
}

static StringRef getExternalPathComponentKindString(
                  StableSerializationPath::ExternalPath::ComponentKind kind) {
  switch (kind) {
#define CASE(ID, STRING) \
  case StableSerializationPath::ExternalPath::ID: return STRING;
  CASE(Record, "record")
  CASE(Enum, "enum")
  CASE(Namespace, "namespace")
  CASE(Typedef, "typedef")
  CASE(TypedefAnonDecl, "anonymous tag")
  CASE(ObjCInterface, "@interface")
  CASE(ObjCProtocol, "@protocol")
#undef CASE
  }
  llvm_unreachable("bad kind");
}

void StableSerializationPath::dump(llvm::raw_ostream &os) const {
  if (isSwiftDecl()) {
    os << "clang decl of:\n";
    getSwiftDecl()->dump(os, 2);
  } else {
    auto &path = getExternalPath();
    using ExternalPath = StableSerializationPath::ExternalPath;
    os << "external path: ";
    size_t index = 0;
    for (auto &entry : path.Path) {
      if (index++) os << " -> ";
      os << getExternalPathComponentKindString(entry.first);
      if (ExternalPath::requiresIdentifier(entry.first))  {
        os << "(" << entry.second << ")";
      }
    }
    os << "\n";
  }
}

void PrintPattern::printRec(Decl *D, StringRef Label) {
  PrintDecl(Ctx, OS, Delegate, Indent + 2).visit(D, Label);
}
void PrintPattern::printRec(Expr *E, StringRef Label) {
  PrintExpr(Ctx, OS, Delegate, Indent + 2).visit(E, Label);
}
void PrintPattern::printRec(Stmt *S, StringRef Label) {
  PrintStmt(Ctx, OS, Delegate, Indent + 2).visit(S, Label);
}
void PrintPattern::printRec(TypeRepr *T, StringRef Label) {
  PrintTypeRepr(Ctx, OS, Delegate, Indent+2).visit(T, Label);
}
void PrintPattern::printRec(const Pattern *P, StringRef Label) {
  PrintPattern(Ctx, OS, Delegate, Indent+2).visit(const_cast<Pattern *>(P), Label);
}

void PrintDecl::printRec(Decl *D, StringRef Label) {
  PrintDecl(Ctx, OS, Delegate, Indent + 2).visit(D, Label);
}
void PrintDecl::printRec(Expr *E, StringRef Label) {
  PrintExpr(Ctx, OS, Delegate, Indent + 2).visit(E, Label);
}
void PrintDecl::printRec(Stmt *S, StringRef Label) {
  PrintStmt(Ctx, OS, Delegate, Indent + 2).visit(S, Label);
}
void PrintDecl::printRec(Pattern *P, StringRef Label) {
  PrintPattern(Ctx, OS, Delegate, Indent+2).visit(P, Label);
}
void PrintDecl::printRec(TypeRepr *T, StringRef Label) {
  PrintTypeRepr(Ctx, OS, Delegate, Indent+2).visit(T, Label);
}

void PrintStmt::printRec(Stmt *S, StringRef Label) {
  PrintStmt(Ctx, OS, Delegate, Indent + 2).visit(S, Label);
}
void PrintStmt::printRec(Decl *D, StringRef Label) {
  PrintDecl(Ctx, OS, Delegate, Indent + 2).visit(D, Label);
}
void PrintStmt::printRec(Expr *E, StringRef Label) {
  PrintExpr(Ctx, OS, Delegate, Indent + 2).visit(E, Label);
}
void PrintStmt::printRec(const Pattern *P, StringRef Label) {
  PrintPattern(Ctx, OS, Delegate, Indent+2).visit(const_cast<Pattern *>(P), Label);
}

void PrintExpr::printRec(Expr *E, StringRef Label) {
  PrintExpr(Ctx, OS, Delegate, Indent + 2).visit(E, Label);
}
void PrintExpr::printRec(Decl *D, StringRef Label) {
  PrintDecl(Ctx, OS, Delegate, Indent + 2).visit(D, Label);
}
void PrintExpr::printRec(Stmt *S, StringRef Label) {
  PrintStmt(Ctx, OS, Delegate, Indent + 2).visit(S, Label);
}
void PrintExpr::printRec(const Pattern *P, StringRef Label) {
  PrintPattern(Ctx, OS, Delegate, Indent+2).visit(const_cast<Pattern *>(P), Label);
}
void PrintExpr::printRec(TypeRepr *T, StringRef Label) {
  PrintTypeRepr(Ctx, OS, Delegate, Indent+2).visit(T, Label);
}

void PrintTypeRepr::printRec(Decl *D, StringRef Label) {
  PrintDecl(Ctx, OS, Delegate, Indent + 2).visit(D, Label);
}
void PrintTypeRepr::printRec(Expr *E, StringRef Label) {
  PrintExpr(Ctx, OS, Delegate, Indent + 2).visit(E, Label);
}
void PrintTypeRepr::printRec(TypeRepr *T, StringRef Label) {
  PrintTypeRepr(Ctx, OS, Delegate, Indent+2).visit(T, Label);
}

void PrintType::printRec(Type type, StringRef label) {
  OS << "\n";
  Indent += 2;
  visit(type, label);
  Indent -=2;
}
