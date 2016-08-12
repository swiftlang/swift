//===--- SILPrinter.cpp - Pretty-printing of SIL Code ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file defines the logic to pretty-print SIL, Instructions, etc.
///
//===----------------------------------------------------------------------===//

#include "swift/Strings.h"
#include "swift/Basic/DemangleWrappers.h"
#include "swift/Basic/QuotedString.h"
#include "swift/SIL/SILPrintContext.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILCoverageMap.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/SILVTable.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/Types.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/FileSystem.h"


using namespace swift;
using namespace demangle_wrappers;

llvm::cl::opt<bool>
SILPrintNoColor("sil-print-no-color", llvm::cl::init(""),
                llvm::cl::desc("Don't use color when printing SIL"));

llvm::cl::opt<bool>
SILFullDemangle("sil-full-demangle", llvm::cl::init(false),
                llvm::cl::desc("Fully demangle symbol names in SIL output"));

static std::string demangleSymbol(StringRef Name) {
  if (SILFullDemangle)
    return demangleSymbolAsString(Name);
  return demangleSymbolAsString(Name,
                    Demangle::DemangleOptions::SimplifiedUIDemangleOptions());
}

struct ID {
  enum ID_Kind {
    SILBasicBlock, SILUndef, SSAValue
  } Kind;
  unsigned Number;

  // A stable ordering of ID objects.
  bool operator<(ID Other) const {
    if (unsigned(Kind) < unsigned(Other.Kind))
      return true;
    if (Number < Other.Number)
      return true;
    return false;
  }
};

enum SILColorKind {
  SC_Type,
};

namespace {
/// RAII based coloring of SIL output.
class SILColor {
  raw_ostream &OS;
  enum raw_ostream::Colors Color;
public:
#define DEF_COL(NAME, RAW) case NAME: Color = raw_ostream::RAW; break;

  explicit SILColor(raw_ostream &OS, SILColorKind K) : OS(OS) {
    if (!OS.has_colors() || SILPrintNoColor)
      return;
    switch (K) {
      DEF_COL(SC_Type, YELLOW)
    }
    OS.resetColor();
    OS.changeColor(Color);
  }

  explicit SILColor(raw_ostream &OS, ID::ID_Kind K) : OS(OS) {
    if (!OS.has_colors() || SILPrintNoColor)
      return;
    switch (K) {
      DEF_COL(ID::SILUndef, RED)
      DEF_COL(ID::SILBasicBlock, GREEN)
      DEF_COL(ID::SSAValue, MAGENTA)
    }
    OS.resetColor();
    OS.changeColor(Color);
  }
  
  ~SILColor() {
    if (!OS.has_colors() || SILPrintNoColor)
      return;
    // FIXME: instead of resetColor(), we can look into
    // capturing the current active color and restoring it.
    OS.resetColor();
  }
#undef DEF_COL
};
}

static raw_ostream &operator<<(raw_ostream &OS, ID i) {
  SILColor C(OS, i.Kind);
  switch (i.Kind) {
  case ID::SILUndef: OS << "undef"; return OS;
  case ID::SILBasicBlock: OS << "bb"; break;
  case ID::SSAValue: OS << '%'; break;
  }
  OS << i.Number;

  return OS;
}

/// IDAndType - Used when a client wants to print something like "%0 : $Int".
struct IDAndType {
  ID id;
  SILType Ty;
};

/// Return the fully qualified dotted path for DeclContext.
static void printFullContext(const DeclContext *Context, raw_ostream &Buffer) {
  if (!Context)
    return;
  switch (Context->getContextKind()) {
  case DeclContextKind::Module:
    if (Context == cast<Module>(Context)->getASTContext().TheBuiltinModule)
      Buffer << cast<Module>(Context)->getName() << ".";
    return;

  case DeclContextKind::FileUnit:
    // Ignore the file; just print the module.
    printFullContext(Context->getParent(), Buffer);
    return;

  case DeclContextKind::Initializer:
    // FIXME
    Buffer << "<initializer>";
    return;

  case DeclContextKind::AbstractClosureExpr:
    // FIXME
    Buffer << "<anonymous function>";
    return;

  case DeclContextKind::SerializedLocal:
    Buffer << "<serialized local context>";
    return;

  case DeclContextKind::GenericTypeDecl: {
    auto *generic = cast<GenericTypeDecl>(Context);
    printFullContext(generic->getDeclContext(), Buffer);
    Buffer << generic->getName() << ".";
    return;
  }

  case DeclContextKind::ExtensionDecl: {
    Type Ty = cast<ExtensionDecl>(Context)->getExtendedType();
    TypeBase *Base = Ty->getCanonicalType().getPointer();
    const NominalTypeDecl *ExtNominal = 0;
    switch (Base->getKind()) {
      default:
        llvm_unreachable("unhandled context kind in SILPrint!");
      case TypeKind::Protocol:
        ExtNominal = cast<ProtocolType>(Base)->getDecl();
        break;
      case TypeKind::Enum:
        ExtNominal = cast<EnumType>(Base)->getDecl();
        break;
      case TypeKind::Struct:
        ExtNominal = cast<StructType>(Base)->getDecl();
        break;
      case TypeKind::Class:
        ExtNominal = cast<ClassType>(Base)->getDecl();
        break;
      case TypeKind::BoundGenericEnum:
        ExtNominal = cast<BoundGenericEnumType>(Base)->getDecl();
        break;
      case TypeKind::BoundGenericStruct:
        ExtNominal = cast<BoundGenericStructType>(Base)->getDecl();
        break;
      case TypeKind::BoundGenericClass:
        ExtNominal = cast<BoundGenericClassType>(Base)->getDecl();
        break;
    }
    printFullContext(ExtNominal->getDeclContext(), Buffer);
    Buffer << ExtNominal->getName() << ".";
    return;
  }
  
  case DeclContextKind::TopLevelCodeDecl:
    // FIXME
    Buffer << "<top level code>";
    return;
  case DeclContextKind::AbstractFunctionDecl:
    // FIXME
    Buffer << "<abstract function>";
    return;
  case DeclContextKind::SubscriptDecl:
    // FIXME
    Buffer << "<subscript>";
    return;
  }
  llvm_unreachable("bad decl context");
}

static void printValueDecl(ValueDecl *Decl, raw_ostream &OS) {
  printFullContext(Decl->getDeclContext(), OS);
  assert(Decl->hasName());

  if (Decl->isOperator())
    OS << '"' << Decl->getName() << '"';
  else
    OS << Decl->getName();
}

/// SILDeclRef uses sigil "#" and prints the fully qualified dotted path.
void SILDeclRef::print(raw_ostream &OS) const {
  OS << "#";
  if (isNull()) {
    OS << "<null>";
    return;
  }

  bool isDot = true;
  if (!hasDecl()) {
    OS << "<anonymous function>";
  } else if (kind == SILDeclRef::Kind::Func) {
    auto *FD = cast<FuncDecl>(getDecl());
    switch (FD->getAccessorKind()) {
    case AccessorKind::IsWillSet:
      printValueDecl(FD->getAccessorStorageDecl(), OS);
      OS << "!willSet";
      break;
    case AccessorKind::IsDidSet:
      printValueDecl(FD->getAccessorStorageDecl(), OS);
      OS << "!didSet";
      break;
    case AccessorKind::NotAccessor:
      printValueDecl(FD, OS);
      isDot = false;
      break;
    case AccessorKind::IsGetter:
      printValueDecl(FD->getAccessorStorageDecl(), OS);
      OS << "!getter";
      break;
    case AccessorKind::IsSetter:
      printValueDecl(FD->getAccessorStorageDecl(), OS);
      OS << "!setter";
      break;
    case AccessorKind::IsMaterializeForSet:
      printValueDecl(FD->getAccessorStorageDecl(), OS);
      OS << "!materializeForSet";
      break;
    case AccessorKind::IsAddressor:
      printValueDecl(FD->getAccessorStorageDecl(), OS);
      OS << "!addressor";
      break;
    case AccessorKind::IsMutableAddressor:
      printValueDecl(FD->getAccessorStorageDecl(), OS);
      OS << "!mutableAddressor";
      break;
    }
  } else {
    printValueDecl(getDecl(), OS);
  }
  switch (kind) {
  case SILDeclRef::Kind::Func:
    break;
  case SILDeclRef::Kind::Allocator:
    OS << "!allocator";
    break;
  case SILDeclRef::Kind::Initializer:
    OS << "!initializer";
    break;
  case SILDeclRef::Kind::EnumElement:
    OS << "!enumelt";
    break;
  case SILDeclRef::Kind::Destroyer:
    OS << "!destroyer";
    break;
  case SILDeclRef::Kind::Deallocator:
    OS << "!deallocator";
    break;
  case SILDeclRef::Kind::IVarInitializer:
    OS << "!ivarinitializer";
    break;
  case SILDeclRef::Kind::IVarDestroyer:
    OS << "!ivardestroyer";
    break;
  case SILDeclRef::Kind::GlobalAccessor:
    OS << "!globalaccessor";
    break;
  case SILDeclRef::Kind::GlobalGetter:
    OS << "!globalgetter";
    break;
  case SILDeclRef::Kind::DefaultArgGenerator:
    OS << "!defaultarg" << "." << defaultArgIndex;
    break;
  case SILDeclRef::Kind::StoredPropertyInitializer:
    OS << "!propertyinit";
    break;
  }
  if (uncurryLevel != 0)
    OS << (isDot ? '.' : '!')  << uncurryLevel;

  if (isForeign)
    OS << ((isDot || uncurryLevel != 0) ? '.' : '!')  << "foreign";

  if (isDirectReference)
    OS << ((isDot || uncurryLevel != 0) ? '.' : '!')  << "direct";
}

void SILDeclRef::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

static void print(raw_ostream &OS, SILValueCategory category) {
  switch (category) {
  case SILValueCategory::Object: return;
  case SILValueCategory::Address: OS << '*'; return;
  }
  llvm_unreachable("bad value category!");
}

static StringRef getCastConsumptionKindName(CastConsumptionKind kind) {
  switch (kind) {
  case CastConsumptionKind::TakeAlways: return "take_always";
  case CastConsumptionKind::TakeOnSuccess: return "take_on_success";
  case CastConsumptionKind::CopyOnSuccess: return "copy_on_success";
  }
  llvm_unreachable("bad cast consumption kind");
}

static void printSILTypeColorAndSigil(raw_ostream &OS, SILType t) {
  SILColor C(OS, SC_Type);
  OS << '$';
  
  // Potentially add a leading sigil for the value category.
  ::print(OS, t.getCategory());
}
      
void SILType::print(raw_ostream &OS) const {
  printSILTypeColorAndSigil(OS, *this);
  
  // Print other types as their Swift representation.
  PrintOptions SubPrinter = PrintOptions::printSIL();
  getSwiftRValueType().print(OS, SubPrinter);
}

void SILType::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

namespace {
  
/// SILPrinter class - This holds the internal implementation details of
/// printing SIL structures.
class SILPrinter : public SILVisitor<SILPrinter> {
  SILPrintContext &Ctx;
  struct {
    llvm::formatted_raw_ostream OS;
    PrintOptions ASTOptions;
  } PrintState;
  SILValue subjectValue;
  unsigned LastBufferID;

  llvm::DenseMap<const SILBasicBlock *, unsigned> BlocksToIDMap;
  llvm::DenseMap<const ValueBase *, unsigned> ValueToIDMap;

  // Printers for the underlying stream.
#define SIMPLE_PRINTER(TYPE) \
  SILPrinter &operator<<(TYPE value) { \
    PrintState.OS << value;            \
    return *this;                      \
  }
  SIMPLE_PRINTER(char)
  SIMPLE_PRINTER(unsigned)
  SIMPLE_PRINTER(StringRef)
  SIMPLE_PRINTER(Identifier)
  SIMPLE_PRINTER(ID)
  SIMPLE_PRINTER(QuotedString)
  SIMPLE_PRINTER(SILDeclRef)
  SIMPLE_PRINTER(APInt)
#undef SIMPLE_PRINTER
  
  SILPrinter &operator<<(IDAndType i) {
    SILColor C(PrintState.OS, SC_Type);
    return *this << i.id << " : " << i.Ty;
  }
  
  SILPrinter &operator<<(Type t) {
    // Print the type using our print options.
    t.print(PrintState.OS, PrintState.ASTOptions);
    return *this;
  }
  
  SILPrinter &operator<<(SILType t) {
    printSILTypeColorAndSigil(PrintState.OS, t);
    t.getSwiftRValueType().print(PrintState.OS, PrintState.ASTOptions);
    return *this;
  }
  
public:
  SILPrinter(
      SILPrintContext &PrintCtx,
      llvm::DenseMap<CanType, Identifier> *AlternativeTypeNames = nullptr)
      : Ctx(PrintCtx),
        PrintState{{PrintCtx.OS()}, PrintOptions::printSIL()},
        LastBufferID(0) {
    PrintState.ASTOptions.AlternativeTypeNames = AlternativeTypeNames;
    PrintState.ASTOptions.PrintForSIL = true;
  }

  ID getID(const SILBasicBlock *B);
  ID getID(SILValue V);
  IDAndType getIDAndType(SILValue V) {
    return { getID(V), V->getType() };
  }

  //===--------------------------------------------------------------------===//
  // Big entrypoints.
  void print(const SILFunction *F) {
    // If we are asked to emit sorted SIL, print out our BBs in RPOT order.
    if (Ctx.sortSIL()) {
      std::vector<SILBasicBlock *> RPOT;
      auto *UnsafeF = const_cast<SILFunction *>(F);
      std::copy(po_begin(UnsafeF), po_end(UnsafeF),
                std::back_inserter(RPOT));
      std::reverse(RPOT.begin(), RPOT.end());
      // Initialize IDs so our IDs are in RPOT as well. This is a hack.
      for (unsigned Index : indices(RPOT))
        BlocksToIDMap[RPOT[Index]] = Index;
      interleave(RPOT,
                 [&](SILBasicBlock *B) { print(B); },
                 [&] { *this << '\n'; });
      return;
    }

    interleave(*F,
               [&](const SILBasicBlock &B) { print(&B); },
               [&] { *this << '\n'; });
  }

  void print(const SILBasicBlock *BB) {
    // Output uses for BB arguments.
    if (!BB->bbarg_empty()) {
      for (auto I = BB->bbarg_begin(), E = BB->bbarg_end(); I != E; ++I) {
        SILValue V = *I;
        if (V->use_empty())
          continue;
        *this << "// " << getID(V);
        PrintState.OS.PadToColumn(50);
        *this << "// user";
        if (std::next(V->use_begin()) != V->use_end())
          *this << 's';
        *this << ": ";

        llvm::SmallVector<ID, 32> UserIDs;
        for (auto *Op : V->getUses())
          UserIDs.push_back(getID(Op->getUser()));

        // Display the user ids sorted to give a stable use order in the
        // printer's output if we are asked to do so. This makes diffing large
        // sections of SIL significantly easier at the expense of not showing
        // the _TRUE_ order of the users in the use list.
        if (Ctx.sortSIL()) {
          std::sort(UserIDs.begin(), UserIDs.end());
        }

        interleave(UserIDs.begin(), UserIDs.end(),
            [&] (ID id) { *this << id; },
            [&] { *this << ", "; });
        *this << '\n';
      }
    }

    *this << getID(BB);

    if (!BB->bbarg_empty()) {
      *this << '(';
      for (auto I = BB->bbarg_begin(), E = BB->bbarg_end(); I != E; ++I) {
        if (I != BB->bbarg_begin()) *this << ", ";
        *this << getIDAndType(*I);
      }
      *this << ')';
    }

    *this << ":";

    if (!BB->pred_empty()) {
      PrintState.OS.PadToColumn(50);
      
      *this << "// Preds:";

      llvm::SmallVector<ID, 32> PredIDs;
      for (auto *BBI : BB->getPreds())
        PredIDs.push_back(getID(BBI));

      // Display the pred ids sorted to give a stable use order in the printer's
      // output if we are asked to do so. This makes diffing large sections of
      // SIL significantly easier at the expense of not showing the _TRUE_ order
      // of the users in the use list.
      if (Ctx.sortSIL()) {
        std::sort(PredIDs.begin(), PredIDs.end());
      }

      for (auto Id : PredIDs)
        *this << ' ' << Id;
    }
    *this << '\n';

    for (const SILInstruction &I : *BB) {
      Ctx.printInstructionCallBack(&I);
      print(&I);
    }
  }

  //===--------------------------------------------------------------------===//
  // SILInstruction Printing Logic

  bool printTypeDependentOperands(SILInstruction *I) {
    ArrayRef<Operand> TypeDepOps = I->getTypeDependentOperands();
    if (TypeDepOps.empty())
      return false;

    PrintState.OS.PadToColumn(50);
    *this << "// type-defs: ";
    interleave(TypeDepOps,
               [&](const Operand &op) { *this << getID(op.get()); },
               [&] { *this << ", "; });
    return true;
  }

  /// Print out the users of the SILValue \p V. Return true if we printed out
  /// either an id or a use list. Return false otherwise.
  bool printUsersOfSILValue(SILValue V, bool printedSlashes) {

    if (V->hasValue() && V->use_empty())
      return printedSlashes;

    if (printedSlashes) {
      *this << "; ";
    } else {
      PrintState.OS.PadToColumn(50);
      *this << "// ";
    }
    if (!V->hasValue()) {
      *this << "id: " << getID(V);
      return true;
    }

    if (V->use_empty())
      return true;

    *this << "user";
    if (std::next(V->use_begin()) != V->use_end())
      *this << 's';
    *this << ": ";

    llvm::SmallVector<ID, 32> UserIDs;
    for (auto *Op : V->getUses())
      UserIDs.push_back(getID(Op->getUser()));

    // If we are asked to, display the user ids sorted to give a stable use
    // order in the printer's output. This makes diffing large sections of SIL
    // significantly easier.
    if (Ctx.sortSIL()) {
      std::sort(UserIDs.begin(), UserIDs.end());
    }

    interleave(UserIDs.begin(), UserIDs.end(), [&](ID id) { *this << id; },
               [&] { *this << ", "; });
    return true;
  }

  void printDebugLocRef(SILLocation Loc, const SourceManager &SM,
                        bool PrintComma = true) {
    auto DL = Loc.decodeDebugLoc(SM);
    if (DL.Filename) {
      if (PrintComma)
        *this << ", ";
      *this << "loc " << QuotedString(DL.Filename) << ':' << DL.Line << ':'
            << DL.Column;
    }
  }

  void printDebugScope(const SILDebugScope *DS, const SourceManager &SM) {
    if (!DS)
      return;

    if (!Ctx.hasScopeID(DS)) {
      printDebugScope(DS->Parent.dyn_cast<const SILDebugScope *>(), SM);
      printDebugScope(DS->InlinedCallSite, SM);
      unsigned ID = Ctx.assignScopeID(DS);
      *this << "sil_scope " << ID << " { ";
      printDebugLocRef(DS->Loc, SM, false);
      *this << " parent ";
      if (auto *F = DS->Parent.dyn_cast<SILFunction *>())
        *this << "@" << F->getName() << " : $" << F->getLoweredFunctionType();
      else {
        auto *PS = DS->Parent.get<const SILDebugScope *>();
        *this << Ctx.getScopeID(PS);
      }
      if (auto *CS = DS->InlinedCallSite)
        *this << " inlined_at " << Ctx.getScopeID(CS);
      *this << " }\n";
    }
  }

  void printDebugScopeRef(const SILDebugScope *DS, const SourceManager &SM,
                          bool PrintComma = true) {
    if (DS) {
      if (PrintComma)
        *this << ", ";
      *this << "scope " << Ctx.getScopeID(DS);
    }
  }

  void printSILLocation(SILLocation L, SILModule &M, const SILDebugScope *DS,
                        bool printedSlashes) {
    if (!L.isNull()) {
      if (!printedSlashes) {
        PrintState.OS.PadToColumn(50);
        *this << "//";
      }
      *this << " ";

      // To minimize output, only print the line and column number for
      // everything but the first instruction.
      L.getSourceLoc().printLineAndColumn(PrintState.OS,
                                          M.getASTContext().SourceMgr);

      // Print the type of location.
      switch (L.getKind()) {
      case SILLocation::NoneKind:
        assert(L.isAutoGenerated() && "This kind shouldn't be printed.");
        break;
      case SILLocation::RegularKind:
        break;
      case SILLocation::ReturnKind:
        *this << ":return";
        break;
      case SILLocation::ImplicitReturnKind:
        *this << ":imp_return";
        break;
      case SILLocation::InlinedKind:
        *this << ":inlined";
        break;
      case SILLocation::MandatoryInlinedKind:
        *this << ":minlined";
        break;
      case SILLocation::CleanupKind:
        *this << ":cleanup";
        break;
      case SILLocation::ArtificialUnreachableKind:
        *this << ":art_unreach";
        break;
      }
      if (L.isSILFile())
        *this << ":sil";
      if (L.isAutoGenerated())
        *this << ":auto_gen";
      if (L.isInPrologue())
        *this << ":in_prologue";
    }
    if (L.isNull()) {
      if (!printedSlashes) {
        PrintState.OS.PadToColumn(50);
        *this << "//";
      }
      if (L.isInTopLevel())
        *this << " top_level";
      else if (L.isAutoGenerated())
        *this << " auto_gen";
      else
        *this << " no_loc";
      if (L.isInPrologue())
        *this << ":in_prologue";
    }

    // Print inlined-at location, if any.
    if (DS) {
      SILFunction *InlinedF = DS->getInlinedFunction();
      auto InlineScopes = DS->flattenedInlineTree();
      for (auto *CS : reversed(InlineScopes)) {
        *this << ": ";
        if (InlinedF) {
          *this << demangleSymbol(InlinedF->getName());
        } else {
          *this << '?';
        }
        *this << " perf_inlined_at ";
        auto CallSite = CS->Loc;
        if (!CallSite.isNull() && CallSite.isASTNode())
          CallSite.getSourceLoc().print(
            PrintState.OS, M.getASTContext().SourceMgr, LastBufferID);
        else
          *this << "?";

        InlinedF = CS->getInlinedFunction();
      }
    }
  }

  void print(SILValue V, bool PrintScopes = false) {
    // Lazily print any debug locations used in this value.
    if (PrintScopes)
      if (auto *I = dyn_cast<SILInstruction>(V)) {
        auto &SM = I->getModule().getASTContext().SourceMgr;
        printDebugScope(I->getDebugScope(), SM);
      }

    if (auto *FRI = dyn_cast<FunctionRefInst>(V))
      *this << "  // function_ref "
            << demangleSymbol(FRI->getReferencedFunction()->getName())
            << "\n";

    *this << "  ";

    // Print result.
    if (V->hasValue()) {
      ID Name = getID(V);
      *this << Name << " = ";
    }

    // Print the value.
    visit(V);

    bool printedSlashes = false;
    if (auto *I = dyn_cast<SILInstruction>(V)) {
      auto &SM = I->getModule().getASTContext().SourceMgr;
      printDebugLocRef(I->getLoc(), SM);
      printDebugScopeRef(I->getDebugScope(), SM);
      printedSlashes = printTypeDependentOperands(I);
    }

    // Print users, or id for valueless instructions.
    printedSlashes = printUsersOfSILValue(V, printedSlashes);

    // Print SIL location.
    if (Ctx.printVerbose()) {
      if (auto *I = dyn_cast<SILInstruction>(V)) {
        printSILLocation(I->getLoc(), I->getModule(), I->getDebugScope(),
                         printedSlashes);
      }
    }
    
    *this << '\n';
  }
  
  void printInContext(SILValue V) {
    subjectValue = V;
    
    auto sortByID = [&](SILValue a, SILValue b) {
      return getID(a).Number < getID(b).Number;
    };

    
    if (auto *I = dyn_cast<SILInstruction>(V)) {
      auto operands = map<SmallVector<SILValue,4>>(I->getAllOperands(),
                                                   [](Operand const &o) {
                                                     return o.get();
                                                   });
      std::sort(operands.begin(), operands.end(), sortByID);
      for (auto &operand : operands) {
        *this << "   ";
        print(operand);
      }
    }
    
    *this << "-> ";
    print(V);
    
    auto users = map<SmallVector<SILValue,4>>(V->getUses(),
                                              [](Operand *o) {
                                                return o->getUser();
                                              });
    std::sort(users.begin(), users.end(), sortByID);
    for (auto &user : users) {
      *this << "   ";
      print(user);
    }
  }

  void visitSILArgument(SILArgument *A) {
    // This should really only happen during debugging.
    *this << "argument of " << getID(A->getParent()) << " : "
          << A->getType();
  }

  void visitSILUndef(SILUndef *A) {
    // This should really only happen during debugging.
    *this << "undef<" << A->getType() << ">";
  }

  void printDebugVar(SILDebugVariable Var) {
    if (Var.Name.empty())
      return;
    if (Var.Constant)
      *this << ", let";
    else
      *this << ", var";
    *this << ", name \"" << Var.Name << '"';
    if (Var.ArgNo)
      *this << ", argno " << Var.ArgNo;
  }

  void visitAllocStackInst(AllocStackInst *AVI) {
    *this << "alloc_stack " << AVI->getElementType();
    printDebugVar(AVI->getVarInfo());
  }

  void visitAllocRefInst(AllocRefInst *ARI) {
    *this << "alloc_ref ";
    if (ARI->isObjC())
      *this << "[objc] ";
    if (ARI->canAllocOnStack())
      *this << "[stack] ";
    *this << ARI->getType();
  }

  void visitAllocRefDynamicInst(AllocRefDynamicInst *ARDI) {
    *this << "alloc_ref_dynamic ";
    if (ARDI->isObjC())
      *this << "[objc] ";
    *this << getIDAndType(ARDI->getOperand());
    *this << ", " << ARDI->getType();
  }

  void visitAllocValueBufferInst(AllocValueBufferInst *AVBI) {
    *this << "alloc_value_buffer " << AVBI->getValueType()
       << " in " << getIDAndType(AVBI->getOperand());
  }

  void visitAllocBoxInst(AllocBoxInst *ABI) {
    *this << "alloc_box " << ABI->getElementType();
    printDebugVar(ABI->getVarInfo());
  }

  void printSubstitutions(ArrayRef<Substitution> Subs) {
    if (Subs.empty())
      return;
    
    *this << '<';
    interleave(Subs,
               [&](const Substitution &s) { *this << s.getReplacement(); },
               [&] { *this << ", "; });
    *this << '>';
  }

  void visitApplyInst(ApplyInst *AI) {
    *this << "apply ";
    if (AI->isNonThrowing())
      *this << "[nothrow] ";
    *this << getID(AI->getCallee());
    printSubstitutions(AI->getSubstitutions());
    *this << '(';
    interleave(AI->getArguments(),
               [&](const SILValue &arg) { *this << getID(arg); },
               [&] { *this << ", "; });
    *this << ") : " << AI->getCallee()->getType();
  }

  void visitTryApplyInst(TryApplyInst *AI) {
    *this << "try_apply ";
    *this << getID(AI->getCallee());
    printSubstitutions(AI->getSubstitutions());
    *this << '(';
    interleave(AI->getArguments(),
               [&](const SILValue &arg) { *this << getID(arg); },
               [&] { *this << ", "; });
    *this << ") : " << AI->getCallee()->getType();
    *this << ", normal " << getID(AI->getNormalBB());
    *this << ", error " << getID(AI->getErrorBB());
  }
  
  
  void visitPartialApplyInst(PartialApplyInst *CI) {
    *this << "partial_apply ";
    *this << getID(CI->getCallee());
    printSubstitutions(CI->getSubstitutions());
    *this << '(';
    interleave(CI->getArguments(),
               [&](const SILValue &arg) { *this << getID(arg); },
               [&] { *this << ", "; });
    *this << ") : " << CI->getCallee()->getType();
  }

  void visitFunctionRefInst(FunctionRefInst *FRI) {
    *this << "function_ref ";
    FRI->getReferencedFunction()->printName(PrintState.OS);
    *this << " : " << FRI->getType();
  }
  
  void visitBuiltinInst(BuiltinInst *BI) {
    *this << "builtin " << QuotedString(BI->getName().str());
    printSubstitutions(BI->getSubstitutions());
    *this << "(";
    
    interleave(BI->getArguments(), [&](SILValue v) {
      *this << getIDAndType(v);
    }, [&]{
      *this << ", ";
    });
    
    *this << ") : ";
    *this << BI->getType();
  }
  
  void visitAllocGlobalInst(AllocGlobalInst *AGI) {
    *this << "alloc_global ";
    if (AGI->getReferencedGlobal()) {
      AGI->getReferencedGlobal()->printName(PrintState.OS);
    } else {
      *this << "<<placeholder>>";
    }
  }
  
  void visitGlobalAddrInst(GlobalAddrInst *GAI) {
    *this << "global_addr ";
    if (GAI->getReferencedGlobal()) {
      GAI->getReferencedGlobal()->printName(PrintState.OS);
    } else {
      *this << "<<placeholder>>";
    }
    *this << " : " << GAI->getType();
  }

  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
    const auto &lit = ILI->getValue();
    *this << "integer_literal " << ILI->getType() << ", " << lit;
  }
  void visitFloatLiteralInst(FloatLiteralInst *FLI) {
    *this << "float_literal " << FLI->getType() << ", 0x";
    APInt bits = FLI->getBits();
    *this << bits.toString(16, /*Signed*/ false);
    llvm::SmallString<12> decimal;
    FLI->getValue().toString(decimal);
    *this << " // " << decimal;
  }
  static StringRef getStringEncodingName(StringLiteralInst::Encoding kind) {
    switch (kind) {
    case StringLiteralInst::Encoding::UTF8: return "utf8 ";
    case StringLiteralInst::Encoding::UTF16: return "utf16 ";
    case StringLiteralInst::Encoding::ObjCSelector: return "objc_selector ";
    }
    llvm_unreachable("bad string literal encoding");
  }
  void visitStringLiteralInst(StringLiteralInst *SLI) {
    *this << "string_literal " << getStringEncodingName(SLI->getEncoding())
       << QuotedString(SLI->getValue());
  }
  void visitLoadInst(LoadInst *LI) {
    *this << "load " << getIDAndType(LI->getOperand());
  }
  void visitStoreInst(StoreInst *SI) {
    *this << "store " << getID(SI->getSrc()) << " to "
       << getIDAndType(SI->getDest());
  }
  void visitAssignInst(AssignInst *AI) {
    *this << "assign " << getID(AI->getSrc()) << " to "
       << getIDAndType(AI->getDest());
  }
  void visitMarkUninitializedInst(MarkUninitializedInst *MU) {
    *this << "mark_uninitialized ";
    switch (MU->getKind()) {
    case MarkUninitializedInst::Var: *this << "[var] "; break;
    case MarkUninitializedInst::RootSelf:  *this << "[rootself] "; break;
    case MarkUninitializedInst::DerivedSelf:  *this << "[derivedself] "; break;
    case MarkUninitializedInst::DerivedSelfOnly:
      *this << "[derivedselfonly] ";
      break;
    case MarkUninitializedInst::DelegatingSelf: *this << "[delegatingself] ";break;
    }
    
    *this << getIDAndType(MU->getOperand());
  }
  void visitMarkUninitializedBehaviorInst(MarkUninitializedBehaviorInst *MU) {
    *this << "mark_uninitialized_behavior "
          << getID(MU->getInitStorageFunc());
    printSubstitutions(MU->getInitStorageSubstitutions());
    *this << '(' << getID(MU->getStorage()) << ") : "
          << MU->getInitStorageFunc()->getType() << ", "
          << getID(MU->getSetterFunc());
    printSubstitutions(MU->getSetterSubstitutions());
    *this << '(' << getID(MU->getSelf()) << ") : "
          << MU->getSetterFunc()->getType();
  }
  void visitMarkFunctionEscapeInst(MarkFunctionEscapeInst *MFE) {
    *this << "mark_function_escape ";
    interleave(MFE->getElements(),
               [&](SILValue Var) {
                 *this << getIDAndType(Var);
               },
               [&] { *this << ", "; });
  }

  void visitDebugValueInst(DebugValueInst *DVI) {
    *this << "debug_value " << getIDAndType(DVI->getOperand());
    printDebugVar(DVI->getVarInfo());
  }

  void visitDebugValueAddrInst(DebugValueAddrInst *DVAI) {
    *this << "debug_value_addr " << getIDAndType(DVAI->getOperand());
    printDebugVar(DVAI->getVarInfo());
  }

  void visitLoadUnownedInst(LoadUnownedInst *LI) {
    *this << "load_unowned ";
    if (LI->isTake())
      *this << "[take] ";
    *this << getIDAndType(LI->getOperand());
  }
  void visitStoreUnownedInst(StoreUnownedInst *SI) {
    *this << "store_unowned " << getID(SI->getSrc()) << " to ";
    if (SI->isInitializationOfDest())
      *this << "[initialization] ";
    *this << getIDAndType(SI->getDest());
  }

  void visitLoadWeakInst(LoadWeakInst *LI) {
    *this << "load_weak ";
    if (LI->isTake())
      *this << "[take] ";
    *this << getIDAndType(LI->getOperand());
  }
  void visitStoreWeakInst(StoreWeakInst *SI) {
    *this << "store_weak " << getID(SI->getSrc()) << " to ";
    if (SI->isInitializationOfDest())
      *this << "[initialization] ";
    *this << getIDAndType(SI->getDest());
  }

  void visitCopyAddrInst(CopyAddrInst *CI) {
    *this << "copy_addr ";
    if (CI->isTakeOfSrc())
      *this << "[take] ";
    *this << getID(CI->getSrc()) << " to ";
    if (CI->isInitializationOfDest())
      *this << "[initialization] ";
    *this << getIDAndType(CI->getDest());
  }

  void visitBindMemoryInst(BindMemoryInst *BI) {
    *this << "bind_memory ";
    *this << getIDAndType(BI->getBase()) << ", ";
    *this << getIDAndType(BI->getIndex()) << " to ";
    *this << BI->getBoundType();
  }
  
  void printUncheckedConversionInst(ConversionInst *CI, SILValue operand,
                                    StringRef name) {
    *this << name << " " << getIDAndType(operand) << " to " << CI->getType();
  }

  void visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *CI) {
    *this << "unconditional_checked_cast "
       << getIDAndType(CI->getOperand())
       << " to " << CI->getType();
  }
  
  void visitCheckedCastBranchInst(CheckedCastBranchInst *CI) {
    *this << "checked_cast_br ";
    if (CI->isExact()) *this << "[exact] ";
    *this << getIDAndType(CI->getOperand())
       << " to " << CI->getCastType() << ", "
       << getID(CI->getSuccessBB()) << ", " << getID(CI->getFailureBB());
  }

  void visitUnconditionalCheckedCastAddrInst(UnconditionalCheckedCastAddrInst *CI) {
    *this << "unconditional_checked_cast_addr "
       << getCastConsumptionKindName(CI->getConsumptionKind())
       << ' '    << CI->getSourceType() << " in " << getIDAndType(CI->getSrc())
       << " to " << CI->getTargetType() << " in " << getIDAndType(CI->getDest());
  }
  
  void visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *CI) {
    *this << "checked_cast_addr_br "
       << getCastConsumptionKindName(CI->getConsumptionKind())
       << ' '    << CI->getSourceType() << " in " << getIDAndType(CI->getSrc())
       << " to " << CI->getTargetType() << " in " << getIDAndType(CI->getDest())
       << ", " << getID(CI->getSuccessBB()) << ", " << getID(CI->getFailureBB());
  }
  
  void visitConvertFunctionInst(ConvertFunctionInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "convert_function");
  }
  void visitThinFunctionToPointerInst(ThinFunctionToPointerInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),
                                 "thin_function_to_pointer");
  }
  void visitPointerToThinFunctionInst(PointerToThinFunctionInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),
                                 "pointer_to_thin_function");
  }
  void visitUpcastInst(UpcastInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "upcast");
  }
  void visitAddressToPointerInst(AddressToPointerInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "address_to_pointer");
  }
  void visitPointerToAddressInst(PointerToAddressInst *CI) {
    *this << "pointer_to_address " << getIDAndType(CI->getOperand()) << " to ";
    if (CI->isStrict())
      *this << "[strict] ";
    *this << CI->getType();
  }
  void visitUncheckedRefCastInst(UncheckedRefCastInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "unchecked_ref_cast");
  }
  void visitUncheckedRefCastAddrInst(UncheckedRefCastAddrInst *CI) {
    *this << "unchecked_ref_cast_addr "
       << ' '    << CI->getSourceType() << " in " << getIDAndType(CI->getSrc())
       << " to " << CI->getTargetType() << " in " << getIDAndType(CI->getDest());
  }
  void visitUncheckedAddrCastInst(UncheckedAddrCastInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "unchecked_addr_cast");
  }
  void visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "unchecked_trivial_bit_cast");
  }
  void visitUncheckedBitwiseCastInst(UncheckedBitwiseCastInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),
                                 "unchecked_bitwise_cast");
  }
  void visitRefToRawPointerInst(RefToRawPointerInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "ref_to_raw_pointer");
  }
  void visitRawPointerToRefInst(RawPointerToRefInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "raw_pointer_to_ref");
  }
  void visitRefToUnownedInst(RefToUnownedInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "ref_to_unowned");
  }
  void visitUnownedToRefInst(UnownedToRefInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "unowned_to_ref");
  }
  void visitRefToUnmanagedInst(RefToUnmanagedInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "ref_to_unmanaged");
  }
  void visitUnmanagedToRefInst(UnmanagedToRefInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "unmanaged_to_ref");
  }
  void visitThinToThickFunctionInst(ThinToThickFunctionInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),"thin_to_thick_function");
  }
  void visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),"thick_to_objc_metatype");
  }
  void visitObjCToThickMetatypeInst(ObjCToThickMetatypeInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),"objc_to_thick_metatype");
  }
  void visitObjCMetatypeToObjectInst(ObjCMetatypeToObjectInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),
                                 "objc_metatype_to_object");
  }
  void visitObjCExistentialMetatypeToObjectInst(
                                      ObjCExistentialMetatypeToObjectInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),
                                 "objc_existential_metatype_to_object");
  }
  void visitObjCProtocolInst(ObjCProtocolInst *CI) {
    *this << "objc_protocol #" << CI->getProtocol()->getName()
       << " : " << CI->getType();
  }
  
  void visitRefToBridgeObjectInst(RefToBridgeObjectInst *I) {
    *this << "ref_to_bridge_object " << getIDAndType(I->getConverted())
       << ", " << getIDAndType(I->getBitsOperand());
  }
  
  void visitBridgeObjectToRefInst(BridgeObjectToRefInst *I) {
    printUncheckedConversionInst(I, I->getOperand(), "bridge_object_to_ref");
  }
  void visitBridgeObjectToWordInst(BridgeObjectToWordInst *I) {
    printUncheckedConversionInst(I, I->getOperand(), "bridge_object_to_word");
  }

  void visitIsNonnullInst(IsNonnullInst *I) {
    *this << "is_nonnull " << getIDAndType(I->getOperand());
  }
  
  void visitRetainValueInst(RetainValueInst *I) {
    visitRefCountingInst(I, "retain_value");
  }

  void visitReleaseValueInst(ReleaseValueInst *I) {
    visitRefCountingInst(I, "release_value");
  }

  void visitAutoreleaseValueInst(AutoreleaseValueInst *I) {
    visitRefCountingInst(I, "autorelease_value");
  }

  void visitSetDeallocatingInst(SetDeallocatingInst *I) {
    visitRefCountingInst(I, "set_deallocating");
  }

  void visitStructInst(StructInst *SI) {
    *this << "struct " << SI->getType() << " (";
    interleave(SI->getElements(),
               [&](const SILValue &V) { *this << getIDAndType(V); },
               [&] { *this << ", "; });
    *this << ')';
  }

  void visitTupleInst(TupleInst *TI) {
    *this << "tuple ";
    
    // Check to see if the type of the tuple can be inferred accurately from the
    // elements.
    bool SimpleType = true;
    for (auto &Elt : TI->getType().castTo<TupleType>()->getElements()) {
      if (Elt.hasName() || Elt.isVararg()) {
        SimpleType = false;
        break;
      }
    }
    
    // If the type is simple, just print the tuple elements.
    if (SimpleType) {
      *this << '(';
      interleave(TI->getElements(),
                 [&](const SILValue &V){ *this << getIDAndType(V); },
                 [&] { *this << ", "; });
      *this << ')';
    } else {
      // Otherwise, print the type, then each value.
      *this << TI->getType() << " (";
      interleave(TI->getElements(),
                 [&](const SILValue &V){ *this << getID(V); },
                 [&] { *this << ", "; });
      *this << ')';
    }
  }
  
  void visitEnumInst(EnumInst *UI) {
    *this << "enum " << UI->getType() << ", "
       << SILDeclRef(UI->getElement(), SILDeclRef::Kind::EnumElement);
    if (UI->hasOperand()) {
      *this << ", " << getIDAndType(UI->getOperand());
    }
  }

  void visitInitEnumDataAddrInst(InitEnumDataAddrInst *UDAI) {
    *this << "init_enum_data_addr "
       << getIDAndType(UDAI->getOperand()) << ", "
       << SILDeclRef(UDAI->getElement(), SILDeclRef::Kind::EnumElement);
  }
  
  void visitUncheckedEnumDataInst(UncheckedEnumDataInst *UDAI) {
    *this << "unchecked_enum_data "
       << getIDAndType(UDAI->getOperand()) << ", "
       << SILDeclRef(UDAI->getElement(), SILDeclRef::Kind::EnumElement);
  }
  
  void visitUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *UDAI) {
    *this << "unchecked_take_enum_data_addr "
       << getIDAndType(UDAI->getOperand()) << ", "
       << SILDeclRef(UDAI->getElement(), SILDeclRef::Kind::EnumElement);
  }
  
  void visitInjectEnumAddrInst(InjectEnumAddrInst *IUAI) {
    *this << "inject_enum_addr "
       << getIDAndType(IUAI->getOperand()) << ", "
       << SILDeclRef(IUAI->getElement(), SILDeclRef::Kind::EnumElement);
  }
  
  void visitTupleExtractInst(TupleExtractInst *EI) {
    *this << "tuple_extract " << getIDAndType(EI->getOperand()) << ", "
          << EI->getFieldNo();
  }
  void visitTupleElementAddrInst(TupleElementAddrInst *EI) {
    *this << "tuple_element_addr " << getIDAndType(EI->getOperand()) << ", "
          << EI->getFieldNo();
  }
  void visitStructExtractInst(StructExtractInst *EI) {
    *this << "struct_extract " << getIDAndType(EI->getOperand()) << ", #";
    printFullContext(EI->getField()->getDeclContext(), PrintState.OS);
    *this << EI->getField()->getName().get();
  }
  void visitStructElementAddrInst(StructElementAddrInst *EI) {
    *this << "struct_element_addr " << getIDAndType(EI->getOperand()) << ", #";
    printFullContext(EI->getField()->getDeclContext(), PrintState.OS);
    *this << EI->getField()->getName().get();
  }
  void visitRefElementAddrInst(RefElementAddrInst *EI) {
    *this << "ref_element_addr " << getIDAndType(EI->getOperand()) << ", #";
    printFullContext(EI->getField()->getDeclContext(), PrintState.OS);
    *this << EI->getField()->getName().get();
  }

  void printMethodInst(MethodInst *I, SILValue Operand, StringRef Name) {
    *this << Name << " ";
    if (I->isVolatile())
      *this << "[volatile] ";
    
    *this << getIDAndType(Operand) << ", " << I->getMember();
  }
  
  void visitClassMethodInst(ClassMethodInst *AMI) {
    printMethodInst(AMI, AMI->getOperand(), "class_method");
    *this << " : " << AMI->getMember().getDecl()->getInterfaceType();
    *this << " , ";
    *this << AMI->getType();
  }
  void visitSuperMethodInst(SuperMethodInst *AMI) {
    printMethodInst(AMI, AMI->getOperand(), "super_method");
    *this << " : " << AMI->getMember().getDecl()->getInterfaceType();
    *this << " , ";
    *this << AMI->getType();
  }
  void visitWitnessMethodInst(WitnessMethodInst *WMI) {
    *this << "witness_method ";
    if (WMI->isVolatile())
      *this << "[volatile] ";
    *this << "$" << WMI->getLookupType() << ", " << WMI->getMember();
    if (!WMI->getTypeDependentOperands().empty()) {
      *this << ", ";
      *this << getIDAndType(WMI->getTypeDependentOperands()[0].get());
    }
    *this << " : " << WMI->getType();
  }
  void visitDynamicMethodInst(DynamicMethodInst *DMI) {
    printMethodInst(DMI, DMI->getOperand(), "dynamic_method");
    *this << " : " << DMI->getMember().getDecl()->getInterfaceType();
    *this << ", ";
    *this << DMI->getType();
  }
  void visitOpenExistentialAddrInst(OpenExistentialAddrInst *OI) {
    *this << "open_existential_addr " << getIDAndType(OI->getOperand())
       << " to " << OI->getType();
  }
  void visitOpenExistentialRefInst(OpenExistentialRefInst *OI) {
    *this << "open_existential_ref " << getIDAndType(OI->getOperand())
       << " to " << OI->getType();
  }
  void visitOpenExistentialMetatypeInst(OpenExistentialMetatypeInst *OI) {
    *this << "open_existential_metatype " << getIDAndType(OI->getOperand())
       << " to " << OI->getType();
  }
  void visitOpenExistentialBoxInst(OpenExistentialBoxInst *OI) {
    *this << "open_existential_box " << getIDAndType(OI->getOperand())
       << " to " << OI->getType();
  }
  void visitInitExistentialAddrInst(InitExistentialAddrInst *AEI) {
    *this << "init_existential_addr " << getIDAndType(AEI->getOperand()) << ", $"
          << AEI->getFormalConcreteType();
  }
  void visitInitExistentialRefInst(InitExistentialRefInst *AEI) {
    *this << "init_existential_ref " << getIDAndType(AEI->getOperand())
       << " : $" << AEI->getFormalConcreteType()
       << ", " << AEI->getType();
  }
  void visitInitExistentialMetatypeInst(InitExistentialMetatypeInst *AEI) {
    *this << "init_existential_metatype " << getIDAndType(AEI->getOperand())
       << ", " << AEI->getType();
  }
  void visitAllocExistentialBoxInst(AllocExistentialBoxInst *AEBI) {
    *this << "alloc_existential_box " << AEBI->getExistentialType()
       << ", $" << AEBI->getFormalConcreteType();
  }
  void visitDeinitExistentialAddrInst(DeinitExistentialAddrInst *DEI) {
    *this << "deinit_existential_addr " << getIDAndType(DEI->getOperand());
  }
  void visitDeallocExistentialBoxInst(DeallocExistentialBoxInst *DEI) {
    *this << "dealloc_existential_box " << getIDAndType(DEI->getOperand())
       << ", $" << DEI->getConcreteType();
  }
  void visitProjectBlockStorageInst(ProjectBlockStorageInst *PBSI) {
    *this << "project_block_storage " << getIDAndType(PBSI->getOperand());
  }
  void visitInitBlockStorageHeaderInst(InitBlockStorageHeaderInst *IBSHI) {
    *this << "init_block_storage_header " << getIDAndType(IBSHI->getBlockStorage())
       << ", invoke " << getID(IBSHI->getInvokeFunction());
    printSubstitutions(IBSHI->getSubstitutions());
    *this << " : " << IBSHI->getInvokeFunction()->getType()
          << ", type " << IBSHI->getType();
  }
  void visitValueMetatypeInst(ValueMetatypeInst *MI) {
    *this << "value_metatype " << MI->getType() << ", "
       << getIDAndType(MI->getOperand());
  }
  void visitExistentialMetatypeInst(ExistentialMetatypeInst *MI) {
    *this << "existential_metatype " << MI->getType() << ", "
       << getIDAndType(MI->getOperand());
  }
  void visitMetatypeInst(MetatypeInst *MI) {
    *this << "metatype " << MI->getType();
  }
  
  void visitFixLifetimeInst(FixLifetimeInst *RI) {
    *this << "fix_lifetime " << getIDAndType(RI->getOperand());
  }
  void visitMarkDependenceInst(MarkDependenceInst *MDI) {
    *this << "mark_dependence " << getIDAndType(MDI->getValue())
       << " on " << getIDAndType(MDI->getBase());
  }
  void visitCopyBlockInst(CopyBlockInst *RI) {
    *this << "copy_block " << getIDAndType(RI->getOperand());
  }
  void visitRefCountingInst(RefCountingInst *I, StringRef InstName) {
    *this << InstName << " ";
    if (I->isNonAtomic())
      *this << "[nonatomic] ";
    *this << getIDAndType(I->getOperand(0));
  }
  void visitStrongRetainInst(StrongRetainInst *RI) {
    visitRefCountingInst(RI, "strong_retain");
  }
  void visitStrongReleaseInst(StrongReleaseInst *RI) {
    visitRefCountingInst(RI, "strong_release");
  }
  void visitStrongPinInst(StrongPinInst *PI) {
    visitRefCountingInst(PI, "strong_pin");
  }
  void visitStrongUnpinInst(StrongUnpinInst *UI) {
    visitRefCountingInst(UI, "strong_unpin");
  }
  void visitStrongRetainUnownedInst(StrongRetainUnownedInst *RI) {
    visitRefCountingInst(RI, "strong_retain_unowned");
  }
  void visitUnownedRetainInst(UnownedRetainInst *RI) {
    visitRefCountingInst(RI, "unowned_retain");
  }
  void visitUnownedReleaseInst(UnownedReleaseInst *RI) {
    visitRefCountingInst(RI, "unowned_release");
  }
  void visitIsUniqueInst(IsUniqueInst *CUI) {
    *this << "is_unique " << getIDAndType(CUI->getOperand());
  }
  void visitIsUniqueOrPinnedInst(IsUniqueOrPinnedInst *CUI) {
    *this << "is_unique_or_pinned " << getIDAndType(CUI->getOperand());
  }
  void visitDeallocStackInst(DeallocStackInst *DI) {
    *this << "dealloc_stack " << getIDAndType(DI->getOperand());
  }
  void visitDeallocRefInst(DeallocRefInst *DI) {
    *this << "dealloc_ref ";
    if (DI->canAllocOnStack())
      *this << "[stack] ";
    *this << getIDAndType(DI->getOperand());
  }
  void visitDeallocPartialRefInst(DeallocPartialRefInst *DPI) {
    *this << "dealloc_partial_ref ";
    *this << getIDAndType(DPI->getInstance());
    *this << ", ";
    *this << getIDAndType(DPI->getMetatype());
  }
  void visitDeallocValueBufferInst(DeallocValueBufferInst *DVBI) {
    *this << "dealloc_value_buffer " << DVBI->getValueType()
       << " in " << getIDAndType(DVBI->getOperand());
  }
  void visitDeallocBoxInst(DeallocBoxInst *DI) {
    *this << "dealloc_box " << getIDAndType(DI->getOperand());
  }
  void visitDestroyAddrInst(DestroyAddrInst *DI) {
    *this << "destroy_addr " << getIDAndType(DI->getOperand());
  }
  void visitProjectValueBufferInst(ProjectValueBufferInst *PVBI) {
    *this << "project_value_buffer " << PVBI->getValueType()
       << " in " << getIDAndType(PVBI->getOperand());
  }
  void visitProjectBoxInst(ProjectBoxInst *PBI) {
    *this << "project_box " << getIDAndType(PBI->getOperand());
  }
  void visitProjectExistentialBoxInst(ProjectExistentialBoxInst *PEBI) {
    *this << "project_existential_box " << PEBI->getValueType()
    << " in " << getIDAndType(PEBI->getOperand());
  }

  void visitCondFailInst(CondFailInst *FI) {
    *this << "cond_fail " << getIDAndType(FI->getOperand());
  }
  
  void visitIndexAddrInst(IndexAddrInst *IAI) {
    *this << "index_addr " << getIDAndType(IAI->getBase()) << ", "
       << getIDAndType(IAI->getIndex());
  }

  void visitIndexRawPointerInst(IndexRawPointerInst *IAI) {
    *this << "index_raw_pointer " << getIDAndType(IAI->getBase()) << ", "
    << getIDAndType(IAI->getIndex());
  }
  
  void visitUnreachableInst(UnreachableInst *UI) {
    *this << "unreachable";
  }

  void visitReturnInst(ReturnInst *RI) {
    *this << "return " << getIDAndType(RI->getOperand());
  }
  
  void visitThrowInst(ThrowInst *TI) {
    *this << "throw " << getIDAndType(TI->getOperand());
  }

  void visitSwitchValueInst(SwitchValueInst *SII) {
    *this << "switch_value " << getIDAndType(SII->getOperand());
    for (unsigned i = 0, e = SII->getNumCases(); i < e; ++i) {
      SILValue value;
      SILBasicBlock *dest;
      std::tie(value, dest) = SII->getCase(i);
      *this << ", case " << getID(value) << ": " << getID(dest);
    }
    if (SII->hasDefault())
      *this << ", default " << getID(SII->getDefaultBB());
  }
  
  void printSwitchEnumInst(SwitchEnumInstBase *SOI) {
    *this << getIDAndType(SOI->getOperand());
    for (unsigned i = 0, e = SOI->getNumCases(); i < e; ++i) {
      EnumElementDecl *elt;
      SILBasicBlock *dest;
      std::tie(elt, dest) = SOI->getCase(i);
      *this << ", case " << SILDeclRef(elt, SILDeclRef::Kind::EnumElement)
         << ": " << getID(dest);
    }
    if (SOI->hasDefault())
      *this << ", default " << getID(SOI->getDefaultBB());
  }
  
  void visitSwitchEnumInst(SwitchEnumInst *SOI) {
    *this << "switch_enum ";
    printSwitchEnumInst(SOI);
  }
  void visitSwitchEnumAddrInst(SwitchEnumAddrInst *SOI) {
    *this << "switch_enum_addr ";
    printSwitchEnumInst(SOI);
  }
  
  void printSelectEnumInst(SelectEnumInstBase *SEI) {
    *this << getIDAndType(SEI->getEnumOperand());

    for (unsigned i = 0, e = SEI->getNumCases(); i < e; ++i) {
      EnumElementDecl *elt;
      SILValue result;
      std::tie(elt, result) = SEI->getCase(i);
      *this << ", case " << SILDeclRef(elt, SILDeclRef::Kind::EnumElement)
         << ": " << getID(result);
    }
    if (SEI->hasDefault())
      *this << ", default " << getID(SEI->getDefaultResult());

    *this << " : " << SEI->getType();
  }

  void visitSelectEnumInst(SelectEnumInst *SEI) {
    *this << "select_enum ";
    printSelectEnumInst(SEI);
  }
  void visitSelectEnumAddrInst(SelectEnumAddrInst *SEI) {
    *this << "select_enum_addr ";
    printSelectEnumInst(SEI);
  }

  void visitSelectValueInst(SelectValueInst *SVI) {
    *this << "select_value ";
    *this << getIDAndType(SVI->getOperand());

    for (unsigned i = 0, e = SVI->getNumCases(); i < e; ++i) {
      SILValue casevalue;
      SILValue result;
      std::tie(casevalue, result) = SVI->getCase(i);
      *this << ", case " << getID(casevalue)
         << ": " << getID(result);
    }
    if (SVI->hasDefault())
      *this << ", default " << getID(SVI->getDefaultResult());

    *this << " : " << SVI->getType();
  }
  
  void visitDynamicMethodBranchInst(DynamicMethodBranchInst *DMBI) {
    *this << "dynamic_method_br " << getIDAndType(DMBI->getOperand()) << ", "
          << DMBI->getMember()
          << ", " << getID(DMBI->getHasMethodBB()) << ", "
          << getID(DMBI->getNoMethodBB());
  }

  void printBranchArgs(OperandValueArrayRef args) {
    if (args.empty()) return;

    *this << '(';
    interleave(args,
               [&](SILValue v) { *this << getIDAndType(v); },
               [&] { *this << ", "; });
    *this << ')';
  }
  
  void visitBranchInst(BranchInst *UBI) {
    *this << "br " << getID(UBI->getDestBB());
    printBranchArgs(UBI->getArgs());
  }

  void visitCondBranchInst(CondBranchInst *CBI) {
    *this << "cond_br " << getID(CBI->getCondition()) << ", "
       << getID(CBI->getTrueBB());
    printBranchArgs(CBI->getTrueArgs());
    *this << ", " << getID(CBI->getFalseBB());
    printBranchArgs(CBI->getFalseArgs());
  }
};
} // end anonymous namespace

ID SILPrinter::getID(const SILBasicBlock *Block) {
  // Lazily initialize the Blocks-to-IDs mapping.
  if (BlocksToIDMap.empty()) {
    unsigned idx = 0;
    for (const SILBasicBlock &B : *Block->getParent())
      BlocksToIDMap[&B] = idx++;
  }

  ID R = { ID::SILBasicBlock, BlocksToIDMap[Block] };
  return R;
}

ID SILPrinter::getID(SILValue V) {
  if (isa<SILUndef>(V))
    return { ID::SILUndef, 0 };

  // Lazily initialize the instruction -> ID mapping.
  if (ValueToIDMap.empty()) {
    V->getParentBB()->getParent()->numberValues(ValueToIDMap);
  }

  ID R = { ID::SSAValue, ValueToIDMap[V] };
  return R;
}

void SILBasicBlock::printAsOperand(raw_ostream &OS, bool PrintType) {
  SILPrintContext Ctx(OS);
  OS << SILPrinter(Ctx).getID(this);
}

//===----------------------------------------------------------------------===//
// Printing for SILInstruction, SILBasicBlock, SILFunction, and SILModule
//===----------------------------------------------------------------------===//

void ValueBase::dump() const {
  print(llvm::errs());
}

void ValueBase::print(raw_ostream &OS) const {
  SILPrintContext Ctx(OS);
  SILPrinter(Ctx).print(this);
}

/// Pretty-print the SILBasicBlock to errs.
void SILBasicBlock::dump() const {
  print(llvm::errs());
}

/// Pretty-print the SILBasicBlock to the designated stream.
void SILBasicBlock::print(raw_ostream &OS) const {
  SILPrintContext Ctx(OS);
  SILPrinter(Ctx).print(this);
}

/// Pretty-print the SILFunction to errs.
void SILFunction::dump(bool Verbose) const {
  SILPrintContext Ctx(llvm::errs(), Verbose);
  print(Ctx);
}

// This is out of line so the debugger can find it.
void SILFunction::dump() const {
  dump(false);
}

void SILFunction::dump(const char *FileName) const {
  std::error_code EC;
  llvm::raw_fd_ostream os(FileName, EC, llvm::sys::fs::OpenFlags::F_None);
  print(os);
}

static StringRef getLinkageString(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Public: return "public ";
  case SILLinkage::Hidden: return "hidden ";
  case SILLinkage::Shared: return "shared ";
  case SILLinkage::Private: return "private ";
  case SILLinkage::PublicExternal: return "public_external ";
  case SILLinkage::HiddenExternal: return "hidden_external ";
  case SILLinkage::SharedExternal: return "shared_external ";
  case SILLinkage::PrivateExternal: return "private_external ";
  }
  llvm_unreachable("bad linkage");
}

static void printLinkage(llvm::raw_ostream &OS, SILLinkage linkage,
                         bool isDefinition) {
  if ((isDefinition && linkage == SILLinkage::DefaultForDefinition) ||
      (!isDefinition && linkage == SILLinkage::DefaultForDeclaration))
    return;

  OS << getLinkageString(linkage);
}

/// Pretty-print the SILFunction to the designated stream.
void SILFunction::print(SILPrintContext &PrintCtx) const {
  auto &SM = getModule().getASTContext().SourceMgr;
  llvm::raw_ostream &OS = PrintCtx.OS();
  for (auto &BB : *this)
    for (auto &I : BB) {
      SILPrinter P(PrintCtx);
      P.printDebugScope(I.getDebugScope(), SM);
    }
  OS << "\n";

  OS << "// " << demangleSymbol(getName()) << '\n';
  OS << "sil ";
  printLinkage(OS, getLinkage(), isDefinition());

  if (isTransparent())
    OS << "[transparent] ";

  if (isFragile())
    OS << "[fragile] ";

  switch (isThunk()) {
  case IsNotThunk: break;
  case IsThunk: OS << "[thunk] "; break;
  case IsReabstractionThunk: OS << "[reabstraction_thunk] "; break;
  }

  if (isGlobalInit())
    OS << "[global_init] ";
  
  switch (getInlineStrategy()) {
    case NoInline: OS << "[noinline] "; break;
    case AlwaysInline: OS << "[always_inline] "; break;
    case InlineDefault: break;
  }

  if (getEffectsKind() == EffectsKind::ReadOnly)
    OS << "[readonly] ";
  else if (getEffectsKind() == EffectsKind::ReadNone)
      OS << "[readnone] ";
  if (getEffectsKind() == EffectsKind::ReadWrite)
    OS << "[readwrite] ";

  for (auto &Attr : getSemanticsAttrs())
    OS << "[_semantics \"" << Attr << "\"] ";

  for (auto *Attr : getSpecializeAttrs()) {
    OS << "[_specialize "; Attr->print(OS); OS << "] ";
  }

  // TODO: Handle clang node owners which don't have a name.
  if (hasClangNode() && getClangNodeOwner()->hasName()) {
    OS << "[clang ";
    printValueDecl(getClangNodeOwner(), OS);
    OS << "] ";
  }

  printName(OS);
  OS << " : $";
  
  // Print the type by substituting our context parameter names for the dependent
  // parameters. In SIL, we may end up with multiple generic parameters that
  // have the same name from different contexts, for instance, a generic
  // protocol requirement with a generic method parameter <T>, which is
  // witnessed by a generic type that has a generic type parameter also named
  // <T>, so we may need to introduce disambiguating aliases.
  llvm::DenseMap<CanType, Identifier> Aliases;
  llvm::DenseSet<Identifier> UsedNames;
  
  auto params = ContextGenericParams;
  llvm::SmallString<16> disambiguatedNameBuf;
  unsigned disambiguatedNameCounter = 1;
  while (params) {
    for (ArchetypeType *param : params->getPrimaryArchetypes()) {
      Identifier name = param->getName();
      while (!UsedNames.insert(name).second) {
        disambiguatedNameBuf.clear();
        {
          llvm::raw_svector_ostream names(disambiguatedNameBuf);
          names << param->getName() << disambiguatedNameCounter++;
        }
        name = getASTContext().getIdentifier(disambiguatedNameBuf);
      }
      if (name != param->getName())
        Aliases[CanType(param)] = name;
    }
    
    params = params->getOuterParameters();
  }

  {
    PrintOptions withContextGenericParams = PrintOptions::printSIL();
    withContextGenericParams.ContextGenericParams = ContextGenericParams;
    withContextGenericParams.AlternativeTypeNames =
      Aliases.empty() ? nullptr : &Aliases;
    LoweredType->print(OS, withContextGenericParams);
  }
  
  if (!isExternalDeclaration()) {
    OS << " {\n";

    SILPrinter(PrintCtx, (Aliases.empty() ? nullptr : &Aliases))
        .print(this);
    OS << "}";
  }
  
  OS << "\n\n";
}
      
/// Pretty-print the SILFunction's name using SIL syntax,
/// '@function_mangled_name'.
void SILFunction::printName(raw_ostream &OS) const {
  OS << "@" << Name;  
}

/// Pretty-print a global variable to the designated stream.
void SILGlobalVariable::print(llvm::raw_ostream &OS, bool Verbose) const {
  OS << "// " << demangleSymbol(getName()) << '\n';
  
  OS << "sil_global ";
  printLinkage(OS, getLinkage(), isDefinition());

  if (isFragile())
    OS << "[fragile] ";
  
  if (isLet())
    OS << "[let] ";

  printName(OS);
  OS << " : " << LoweredType;

  if (getInitializer()) {
    OS << ", ";
    getInitializer()->printName(OS);
    OS << " : " << getInitializer()->getLoweredType();
  }

  OS << "\n\n";
}

void SILGlobalVariable::dump(bool Verbose) const {
  print(llvm::errs(), Verbose);
}

void SILGlobalVariable::printName(raw_ostream &OS) const {
  OS << "@" << Name;
}
      
/// Pretty-print the SILModule to errs.
void SILModule::dump(bool Verbose) const {
  SILPrintContext Ctx(llvm::errs(), Verbose);
  print(Ctx);
}

void SILModule::dump(const char *FileName, bool Verbose,
                     bool PrintASTDecls) const {
  std::error_code EC;
  llvm::raw_fd_ostream os(FileName, EC, llvm::sys::fs::OpenFlags::F_None);
  SILPrintContext Ctx(os, Verbose);
  print(Ctx, getSwiftModule(), PrintASTDecls);
}

static void printSILGlobals(SILPrintContext &Ctx,
                            const SILModule::GlobalListType &Globals) {
  if (!Ctx.sortSIL()) {
    for (const SILGlobalVariable &g : Globals)
      g.print(Ctx.OS(), Ctx.printVerbose());
    return;
  }

  std::vector<const SILGlobalVariable *> globals;
  globals.reserve(Globals.size());
  for (const SILGlobalVariable &g : Globals)
    globals.push_back(&g);
  std::sort(globals.begin(), globals.end(),
    [] (const SILGlobalVariable *g1, const SILGlobalVariable *g2) -> bool {
      return g1->getName().compare(g2->getName()) == -1;
    }
  );
  for (const SILGlobalVariable *g : globals)
    g->print(Ctx.OS(), Ctx.printVerbose());
}

static void printSILFunctions(SILPrintContext &Ctx,
                              const SILModule::FunctionListType &Functions) {
  if (!Ctx.sortSIL()) {
    for (const SILFunction &f : Functions)
      f.print(Ctx);
    return;
  }

  std::vector<const SILFunction *> functions;
  functions.reserve(Functions.size());
  for (const SILFunction &f : Functions)
    functions.push_back(&f);
  std::sort(functions.begin(), functions.end(),
    [] (const SILFunction *f1, const SILFunction *f2) -> bool {
      return f1->getName().compare(f2->getName()) == -1;
    }
  );
  for (const SILFunction *f : functions)
    f->print(Ctx);
}

static void printSILVTables(SILPrintContext &Ctx,
                            const SILModule::VTableListType &VTables) {
  if (!Ctx.sortSIL()) {
    for (const SILVTable &vt : VTables)
      vt.print(Ctx.OS(), Ctx.printVerbose());
    return;
  }

  std::vector<const SILVTable *> vtables;
  vtables.reserve(VTables.size());
  for (const SILVTable &vt : VTables)
    vtables.push_back(&vt);
  std::sort(vtables.begin(), vtables.end(),
    [] (const SILVTable *v1, const SILVTable *v2) -> bool {
      StringRef Name1 = v1->getClass()->getName().str();
      StringRef Name2 = v2->getClass()->getName().str();
      return Name1.compare(Name2) == -1;
    }
  );
  for (const SILVTable *vt : vtables)
    vt->print(Ctx.OS(), Ctx.printVerbose());
}

static void
printSILWitnessTables(SILPrintContext &Ctx,
                      const SILModule::WitnessTableListType &WTables) {
  if (!Ctx.sortSIL()) {
    for (const SILWitnessTable &wt : WTables)
      wt.print(Ctx.OS(), Ctx.printVerbose());
    return;
  }

  std::vector<const SILWitnessTable *> witnesstables;
  witnesstables.reserve(WTables.size());
  for (const SILWitnessTable &wt : WTables)
    witnesstables.push_back(&wt);
  std::sort(witnesstables.begin(), witnesstables.end(),
    [] (const SILWitnessTable *w1, const SILWitnessTable *w2) -> bool {
      return w1->getName().compare(w2->getName()) == -1;
    }
  );
  for (const SILWitnessTable *wt : witnesstables)
    wt->print(Ctx.OS(), Ctx.printVerbose());
}

static void
printSILDefaultWitnessTables(SILPrintContext &Ctx,
                        const SILModule::DefaultWitnessTableListType &WTables) {
  if (!Ctx.sortSIL()) {
    for (const SILDefaultWitnessTable &wt : WTables)
      wt.print(Ctx.OS(), Ctx.printVerbose());
    return;
  }

  std::vector<const SILDefaultWitnessTable *> witnesstables;
  witnesstables.reserve(WTables.size());
  for (const SILDefaultWitnessTable &wt : WTables)
    witnesstables.push_back(&wt);
  std::sort(witnesstables.begin(), witnesstables.end(),
    [] (const SILDefaultWitnessTable *w1,
        const SILDefaultWitnessTable *w2) -> bool {
      return w1->getProtocol()->getName()
          .compare(w2->getProtocol()->getName()) == -1;
    }
  );
  for (const SILDefaultWitnessTable *wt : witnesstables)
    wt->print(Ctx.OS(), Ctx.printVerbose());
}

static void
printSILCoverageMaps(SILPrintContext &Ctx,
                     const SILModule::CoverageMapListType &CoverageMaps) {
  if (!Ctx.sortSIL()) {
    for (const SILCoverageMap &M : CoverageMaps)
      M.print(Ctx);
    return;
  }

  std::vector<const SILCoverageMap *> Maps;
  Maps.reserve(CoverageMaps.size());
  for (const SILCoverageMap &M : CoverageMaps)
    Maps.push_back(&M);
  std::sort(Maps.begin(), Maps.end(),
            [](const SILCoverageMap *LHS, const SILCoverageMap *RHS) -> bool {
              return LHS->getName().compare(RHS->getName()) == -1;
            });
  for (const SILCoverageMap *M : Maps)
    M->print(Ctx);
}

/// Pretty-print the SILModule to the designated stream.
void SILModule::print(SILPrintContext &PrintCtx, Module *M,
                      bool PrintASTDecls) const {
  llvm::raw_ostream &OS = PrintCtx.OS();
  OS << "sil_stage ";
  switch (Stage) {
  case SILStage::Raw:
    OS << "raw";
    break;
  case SILStage::Canonical:
    OS << "canonical";
    break;
  }
  
  OS << "\n\nimport Builtin\nimport " << STDLIB_NAME
     << "\nimport SwiftShims" << "\n\n";

  // Print the declarations and types from the origin module, unless we're not
  // in whole-module mode.
  if (M && AssociatedDeclContext == M && PrintASTDecls) {
    PrintOptions Options = PrintOptions::printSIL();
    Options.TypeDefinitions = true;
    Options.VarInitializers = true;
    // FIXME: ExplodePatternBindingDecls is incompatible with VarInitializers!
    Options.ExplodePatternBindingDecls = true;
    Options.SkipImplicit = false;
    Options.PrintGetSetOnRWProperties = true;
    Options.PrintInSILBody = false;
    Options.PrintDefaultParameterPlaceholder = false;

    SmallVector<Decl *, 32> topLevelDecls;
    M->getTopLevelDecls(topLevelDecls);
    for (const Decl *D : topLevelDecls) {
      if ((isa<ValueDecl>(D) || isa<OperatorDecl>(D) ||
           isa<ExtensionDecl>(D)) &&
          !D->isImplicit()) {
        if (auto *FD = dyn_cast<FuncDecl>(D))
          if (FD->isAccessor())
            continue;
        D->print(OS, Options);
        OS << "\n\n";
      }
    }
  }

  printSILGlobals(PrintCtx, getSILGlobalList());
  printSILFunctions(PrintCtx, getFunctionList());
  printSILVTables(PrintCtx, getVTableList());
  printSILWitnessTables(PrintCtx, getWitnessTableList());
  printSILDefaultWitnessTables(PrintCtx, getDefaultWitnessTableList());
  printSILCoverageMaps(PrintCtx, getCoverageMapList());
  
  OS << "\n\n";
}

void ValueBase::dumpInContext() const {
  printInContext(llvm::errs());
}
void ValueBase::printInContext(llvm::raw_ostream &OS) const {
  SILPrintContext Ctx(OS);
  SILPrinter(Ctx).printInContext(this);
}

void SILVTable::print(llvm::raw_ostream &OS, bool Verbose) const {
  OS << "sil_vtable " << getClass()->getName() << " {\n";
  for (auto &entry : getEntries()) {
    OS << "  ";
    entry.first.print(OS);
    OS << ": " << entry.second->getName()
       << "\t// " << demangleSymbol(entry.second->getName()) << "\n";
  }
  OS << "}\n\n";
}

void SILVTable::dump() const {
  print(llvm::errs());
}

void SILWitnessTable::print(llvm::raw_ostream &OS, bool Verbose) const {
  PrintOptions Options = PrintOptions::printSIL();
  OS << "sil_witness_table ";
  printLinkage(OS, getLinkage(), /*isDefinition*/ isDefinition());
  if (isFragile())
    OS << "[fragile] ";

  getConformance()->printName(OS, Options);

  if (isDeclaration()) {
    OS << "\n\n";
    return;
  }

  OS << " {\n";
  
  for (auto &witness : getEntries()) {
    OS << "  ";
    switch (witness.getKind()) {
    case Invalid:
      llvm_unreachable("invalid witness?!");
    case Method: {
      // method #declref: @function
      auto &methodWitness = witness.getMethodWitness();
      OS << "method ";
      methodWitness.Requirement.print(OS);
      OS << ": ";
      if (methodWitness.Witness) {
        methodWitness.Witness->printName(OS);
        OS << "\t// "
           << demangleSymbol(methodWitness.Witness->getName());
      } else {
        OS << "nil";
      }
      break;
    }
    case AssociatedType: {
      // associated_type AssociatedTypeName: ConformingType
      auto &assocWitness = witness.getAssociatedTypeWitness();
      OS << "associated_type ";
      OS << assocWitness.Requirement->getName() << ": ";
      assocWitness.Witness->print(OS, PrintOptions::printSIL());
      break;
    }
    case AssociatedTypeProtocol: {
      // associated_type_protocol (AssociatedTypeName: Protocol): <conformance>
      auto &assocProtoWitness = witness.getAssociatedTypeProtocolWitness();
      OS << "associated_type_protocol ("
         << assocProtoWitness.Requirement->getName() << ": "
         << assocProtoWitness.Protocol->getName() << "): ";
      if (assocProtoWitness.Witness.isConcrete())
        assocProtoWitness.Witness.getConcrete()->printName(OS, Options);
      else
        OS << "dependent";
      break;
    }
    case BaseProtocol: {
      // base_protocol Protocol: <conformance>
      auto &baseProtoWitness = witness.getBaseProtocolWitness();
      OS << "base_protocol "
         << baseProtoWitness.Requirement->getName() << ": ";
      baseProtoWitness.Witness->printName(OS, Options);
      break;
    }
    case MissingOptional: {
      // optional requirement 'declref': <<not present>>
      OS << "optional requirement '"
         << witness.getMissingOptionalWitness().Witness->getName()
         << "': <<not present>>";
      break;
    }
    }
    OS << '\n';
  }
  
  OS << "}\n\n";
}

void SILWitnessTable::dump() const {
  print(llvm::errs());
}

void SILDefaultWitnessTable::print(llvm::raw_ostream &OS, bool Verbose) const {
  // sil_default_witness_table [<Linkage>] <Protocol> <MinSize>
  OS << "sil_default_witness_table ";
  printLinkage(OS, getLinkage(), ForDefinition);
  OS << getProtocol()->getName() << " {\n";
  
  for (auto &witness : getEntries()) {
    if (!witness.isValid()) {
      OS << "  no_default\n";
      continue;
    }

    // method #declref: @function
    OS << "  method ";
    witness.getRequirement().print(OS);
    OS << ": ";
    witness.getWitness()->printName(OS);
    OS << "\t// "
       << demangleSymbolAsString(witness.getWitness()->getName());
    OS << '\n';
  }
  
  OS << "}\n\n";
}

void SILDefaultWitnessTable::dump() const {
  print(llvm::errs());
}

void SILCoverageMap::print(SILPrintContext &PrintCtx) const {
  llvm::raw_ostream &OS = PrintCtx.OS();
  OS << "sil_coverage_map " << QuotedString(getFile()) << " " << getName()
     << " " << getHash() << " {\t// " << demangleSymbol(getName())
     << "\n";
  if (PrintCtx.sortSIL())
    std::sort(MappedRegions, MappedRegions + NumMappedRegions,
              [](const MappedRegion &LHS, const MappedRegion &RHS) {
      return std::tie(LHS.StartLine, LHS.StartCol, LHS.EndLine, LHS.EndCol) <
             std::tie(RHS.StartLine, RHS.StartCol, RHS.EndLine, RHS.EndCol);
    });
  for (auto &MR : getMappedRegions()) {
    OS << "  " << MR.StartLine << ":" << MR.StartCol << " -> " << MR.EndLine
       << ":" << MR.EndCol << " : ";
    printCounter(OS, MR.Counter);
    OS << "\n";
  }
  OS << "}\n\n";
}

void SILCoverageMap::dump() const {
  print(llvm::errs());
}

void SILDebugScope::flatten(const SILDebugScope *DS,
                            SILDebugScope::InlineScopeList &List) {
  if (DS) {
    if (auto *CS = DS->InlinedCallSite) {
      flatten(CS->Parent.dyn_cast<const SILDebugScope *>(), List);
      List.push_back(CS);
    }
    flatten(DS->Parent.dyn_cast<const SILDebugScope *>(), List);
  }
}

void SILDebugScope::dump(SourceManager &SM, llvm::raw_ostream &OS,
                         unsigned Indent) const {
  OS << "{\n";
  OS.indent(Indent);
  if (Loc.isASTNode())
    Loc.getSourceLoc().print(OS, SM);
  OS << "\n";
  OS.indent(Indent + 2);
  OS << " parent: ";
  if (auto *P = Parent.dyn_cast<const SILDebugScope *>()) {
    P->dump(SM, OS, Indent + 2);
    OS.indent(Indent + 2);
  }
  else if (auto *F = Parent.dyn_cast<SILFunction *>())
    OS << "@" << F->getName();
  else
    OS << "nullptr";

  OS << "\n";
  OS.indent(Indent + 2);
  if (auto *CS = InlinedCallSite) {
    OS << "inlinedCallSite: ";
    CS->dump(SM, OS, Indent + 2);
    OS.indent(Indent + 2);
  }
  OS << "}\n";
}

void SILSpecializeAttr::print(llvm::raw_ostream &OS) const {
  SILPrintContext Ctx(OS);
  SILPrinter(Ctx).printSubstitutions(getSubstitutions());
}

//===----------------------------------------------------------------------===//
// SILPrintContext members
//===----------------------------------------------------------------------===//

SILPrintContext::~SILPrintContext() {
}

void SILPrintContext::printInstructionCallBack(const SILInstruction *I) {
}
