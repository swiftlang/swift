//===--- SILPrinter.cpp - Pretty-printing of SIL Code ---------------------===//
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
// This file defines the logic to pretty-print SIL, Instructions, etc.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Demangle.h"
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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/FormattedStream.h"

using namespace swift;
using namespace Demangle;

struct ID {
  enum ID_Kind {
    SILBasicBlock, SSAValue
  } Kind;
  unsigned Number;
  int ResultNumber;
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
    if (!OS.has_colors())
      return;
    switch (K) {
      DEF_COL(SC_Type, YELLOW)
    }
    OS.resetColor();
    OS.changeColor(Color);
  }

  explicit SILColor(raw_ostream &OS, ID::ID_Kind K) : OS(OS) {
    if (!OS.has_colors())
      return;
    switch (K) {
      DEF_COL(ID::SILBasicBlock, GREEN)
      DEF_COL(ID::SSAValue, MAGENTA)
    }
    OS.resetColor();
    OS.changeColor(Color);
  }
  
  ~SILColor() {
    if (!OS.has_colors())
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
  case ID::SILBasicBlock: OS << "bb"; break;
  case ID::SSAValue: OS << '%'; break;
  }
  OS << i.Number;

  if (i.ResultNumber != -1)
    OS << '#' << i.ResultNumber;
  return OS;
}

/// IDAndType - Used when a client wants to print something like "%0 : $Int".
struct IDAndType {
  ID id;
  SILType Ty;
};

static raw_ostream &operator<<(raw_ostream &OS, IDAndType i) {
  SILColor C(OS, SC_Type);
  return OS << i.id << " : " << i.Ty;
}

/// Return the fully qualified dotted path for DeclContext.
static void printFullContext(const DeclContext *Context, raw_ostream &Buffer) {
  if (!Context)
    return;
  switch (Context->getContextKind()) {
  case DeclContextKind::Module:
    if (auto BM = dyn_cast<BuiltinModule>(Context))
      Buffer << BM->Name << ".";
    return;

  case DeclContextKind::AbstractClosureExpr:
    // FIXME
    Buffer << "<anonymous function>";
    return;

  case DeclContextKind::NominalTypeDecl: {
    const NominalTypeDecl *Nominal = cast<NominalTypeDecl>(Context);
    printFullContext(Nominal->getDeclContext(), Buffer);
    Buffer << Nominal->getName() << ".";
    return;
  }

  case DeclContextKind::ExtensionDecl: {
    Type Ty = cast<ExtensionDecl>(Context)->getExtendedType();
    TypeBase *Base = Ty->getCanonicalType().getPointer();
    const NominalTypeDecl *ExtNominal = 0;
    switch (Base->getKind()) {
      default:
        llvm_unreachable("unhandled context kind in SILPrint!");
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
    llvm_unreachable("unhandled context kind in SILPrint!");
  case DeclContextKind::AbstractFunctionDecl:
    // FIXME
    Buffer << "<abstract function>";
    return;
  }
  llvm_unreachable("bad decl context");
}

/// SILDeclRef uses sigil "#" and prints the fully qualified dotted path.
void SILDeclRef::print(raw_ostream &OS) const {
  OS << "#";
  if (isNull()) {
    OS << "<null>";
    return;
  }
  
  if (hasDecl()) {
    printFullContext(getDecl()->getDeclContext(), OS);
    OS << getDecl()->getName();
  } else {
    OS << "<anonymous function>";
  }
  switch (kind) {
  case SILDeclRef::Kind::Func:
    break;
  case SILDeclRef::Kind::Getter:
    OS << "!getter";
    break;
  case SILDeclRef::Kind::Setter:
    OS << "!setter";
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
  case SILDeclRef::Kind::GlobalAccessor:
    OS << "!globalaccessor";
    break;
  case SILDeclRef::Kind::DefaultArgGenerator:
    OS << "!defaultarg" << "." << defaultArgIndex;
    break;
  }
  if (uncurryLevel != 0) {
    if (kind != SILDeclRef::Kind::Func)
      OS << "." << uncurryLevel;
    else
      OS << "!" << uncurryLevel;
  }
  if (isForeign) {
    if (uncurryLevel != 0 || kind != SILDeclRef::Kind::Func)
      OS << ".foreign";
    else
      OS << "!foreign";
  }
}

void SILDeclRef::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

std::string SILType::getAsString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS);
  return OS.str();
}

static void print(raw_ostream &OS, SILValueCategory category) {
  switch (category) {
  case SILValueCategory::Object: return;
  case SILValueCategory::Address: OS << '*'; return;
  case SILValueCategory::LocalStorage: OS << "*@local_storage "; return;
  }
  llvm_unreachable("bad value category!");
}
      
void SILType::print(raw_ostream &OS) const {
  SILColor C(OS, SC_Type);
  OS << '$';

  // Potentially add a leading sigil for the value category.
  ::print(OS, getCategory());

  // For the Self archetype of a protocol, print @sil_self protocol.
  if (auto archetypeTy = getSwiftRValueType()->getAs<ArchetypeType>()) {
    if (auto proto = archetypeTy->getSelfProtocol()) {
      OS << "@sil_self ";
      proto->getDeclaredType()->print(OS);
      return;
    }
  }

  // Print other types as their Swift representation.
  getSwiftRValueType().print(OS);
}

void SILType::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

namespace {
  
/// SILPrinter class - This holds the internal implementation details of
/// printing SIL structures.
class SILPrinter : public SILVisitor<SILPrinter> {
  llvm::formatted_raw_ostream OS;
  SILValue subjectValue;
  bool Verbose;

  llvm::DenseMap<const SILBasicBlock *, unsigned> BlocksToIDMap;

  llvm::DenseMap<const ValueBase*, unsigned> ValueToIDMap;
public:
  SILPrinter(raw_ostream &OS, bool V = false) : OS(OS), Verbose(V) {
  }

  ID getID(const SILBasicBlock *B);
  ID getID(SILValue V);
  IDAndType getIDAndType(SILValue V) {
    return { getID(V), V.getType() };
  }

  //===--------------------------------------------------------------------===//
  // Big entrypoints.
  void print(const SILFunction *F) {
    interleave(*F,
               [&](const SILBasicBlock &B) { print(&B); },
               [&] { OS << '\n'; });
  }

  void print(const SILBasicBlock *BB) {
    OS << getID(BB);

    if (!BB->bbarg_empty()) {
      OS << '(';
      for (auto I = BB->bbarg_begin(), E = BB->bbarg_end(); I != E; ++I) {
        if (I != BB->bbarg_begin()) OS << ", ";
        OS << getIDAndType(*I);
      }
      OS << ')';
    }

    OS << ":";

    if (!BB->pred_empty()) {
      OS.PadToColumn(50);
      OS << "// Preds:";
      for (auto BBI = BB->pred_begin(), E = BB->pred_end(); BBI != E; ++BBI)
        OS << ' ' << getID(*BBI);
    }
    OS << '\n';

    for (const SILInstruction &I : *BB)
      print(&I);
  }

  //===--------------------------------------------------------------------===//
  // SILInstruction Printing Logic

  void print(SILValue V) {
    if (auto *FRI = dyn_cast<FunctionRefInst>(V))
      OS << "  // function_ref "
         << demangleSymbolAsString(FRI->getReferencedFunction()->getName())
         << "\n";

    OS << "  ";

    // Print result.
    if (V->hasValue()) {
      ID Name = getID(V);
      Name.ResultNumber = -1;  // Don't print subresult number.
      OS << Name << " = ";
    }

    // Print the value.
    visit(V);

    // Print users.
    bool printedSlashes = false;
    if (!V->use_empty()) {
      OS.PadToColumn(50);
      OS << "// user";
      if (std::next(V->use_begin()) != V->use_end())
        OS << 's';
      OS << ": ";
      interleave(V->use_begin(), V->use_end(),
                 [&] (Operand *o) { OS << getID(o->getUser()); },
                 [&] { OS << ", "; });
      printedSlashes = true;
    }

    // Print SIL location.
    if (Verbose) {
      if (SILInstruction *I = dyn_cast<SILInstruction>(V.getDef())) {
        SILLocation L = I->getLoc();
        SILModule &M = I->getModule();
        if (!L.isNull()) {
          if (!printedSlashes) {
            OS.PadToColumn(50);
            OS << "//";
          }
          OS << " ";

          // To minimize output, only print the line and column number for
          // everything but the first instruction.
          L.getSourceLoc().printLineAndColumn(OS, M.getASTContext().SourceMgr);

          // Print the type of location.
          switch (L.getKind()) {
          case SILLocation::NoneKind :
            assert(L.isAutoGenerated() && "This kind shouldn't be printed.");
            break;
          case SILLocation::RegularKind :
            break;
          case SILLocation::ReturnKind :
            OS << ":return"; break;
          case SILLocation::ImplicitReturnKind :
            OS << ":imp_return"; break;
          case SILLocation::InlinedKind :
            OS << ":inlined"; break;
          case SILLocation::CleanupKind :
            OS << ":cleanup"; break;
          case SILLocation::ArtificialUnreachableKind :
            OS << ":art_unreach"; break;
          case SILLocation::SILFileKind :
            OS << ":sil"; break;
          }
          if (L.isAutoGenerated())
            OS << ":auto_gen";

        }
        if (L.isNull()) {
          if (!printedSlashes) {
            OS.PadToColumn(50);
            OS << "//";
          }
          if (L.isInTopLevel())
            OS << " top_level";
          else if (L.isAutoGenerated())
            OS << " auto_gen";
          else
            OS << " no_loc";
        }

      }
    }
    
    OS << '\n';
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
        OS << "   ";
        print(operand);
      }
    }
    
    OS << "-> ";
    print(V);
    
    auto users = map<SmallVector<SILValue,4>>(V->getUses(),
                                              [](Operand *o) {
                                                return o->getUser();
                                              });
    std::sort(users.begin(), users.end(), sortByID);
    for (auto &user : users) {
      OS << "   ";
      print(user);
    }
  }

  void visitSILArgument(SILArgument *A) {
    // This should really only happen during debugging.
    OS << "argument of " << getID(A->getParent()) << " : ";
    A->getType().print(OS);
  }
  
  void visitAllocStackInst(AllocStackInst *AVI) {
    OS << "alloc_stack " << AVI->getElementType();
    if (VarDecl *vd = AVI->getDecl())
      OS << "  // var " << vd->getName();
  }

  void visitAllocRefInst(AllocRefInst *ARI) {
    OS << "alloc_ref " << ARI->getType();
  }
  
  void visitAllocBoxInst(AllocBoxInst *ABI) {
    OS << "alloc_box " << ABI->getElementType();
  }

  void visitAllocArrayInst(AllocArrayInst *AAI) {
    OS << "alloc_array " << AAI->getElementType()
       << ", " << getIDAndType(AAI->getNumElements());
  }
  
  void printSubstitutions(ArrayRef<Substitution> Subs) {
    if (Subs.empty())
      return;
    
    OS << '<';
    interleave(Subs,
               [&](const Substitution &s) {
                 s.Archetype->print(OS);
                 OS << " = ";
                 s.Replacement->print(OS);
               },
               [&] { OS << ", "; });
    OS << '>';
  }
  
  void visitApplyInst(ApplyInst *AI) {
    OS << "apply ";
    if (AI->isTransparent())
      OS << "[transparent] ";
    OS << getID(AI->getCallee());
    printSubstitutions(AI->getSubstitutions());
    OS << '(';
    interleave(AI->getArguments(),
               [&](const SILValue &arg) { OS << getID(arg); },
               [&] { OS << ", "; });
    OS << ") : " << AI->getCallee().getType();
  }
  
  void visitPartialApplyInst(PartialApplyInst *CI) {
    OS << "partial_apply ";
    OS << getID(CI->getCallee());
    printSubstitutions(CI->getSubstitutions());
    OS << '(';
    interleave(CI->getArguments(),
               [&](const SILValue &arg) { OS << getID(arg); },
               [&] { OS << ", "; });
    OS << ") : " << CI->getCallee().getType();
  }

  void visitFunctionRefInst(FunctionRefInst *FRI) {
    OS << "function_ref ";
    FRI->getReferencedFunction()->printName(OS);
    OS << " : " << FRI->getType();
  }
  
  void visitBuiltinFunctionRefInst(BuiltinFunctionRefInst *BFI) {
    OS << "builtin_function_ref " << SILDeclRef(BFI->getReferencedFunction())
       << " : " << BFI->getType();
  }
  
  void visitGlobalAddrInst(GlobalAddrInst *GAI) {
    OS << "global_addr #" << GAI->getGlobal()->getName()
       << " : " << GAI->getType();
  }

  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
    const auto &lit = ILI->getValue();
    OS << "integer_literal " << ILI->getType() << ", " << lit;
  }
  void visitFloatLiteralInst(FloatLiteralInst *FLI) {
    OS << "float_literal " << FLI->getType() << ", 0x";
    APInt bits = FLI->getBits();
    OS << bits.toString(16, /*Signed*/ false);
    llvm::SmallString<12> decimal;
    FLI->getValue().toString(decimal);
    OS << " // " << decimal;
  }
  void visitStringLiteralInst(StringLiteralInst *SLI) {
    OS << "string_literal " << SLI->getType()
       << ", \"" << SLI->getValue() << "\"";
  }
  void visitLoadInst(LoadInst *LI) {
    OS << "load " << getIDAndType(LI->getOperand());
  }
  void visitStoreInst(StoreInst *SI) {
    OS << "store " << getID(SI->getSrc()) << " to "
       << getIDAndType(SI->getDest());
  }
  void visitAssignInst(AssignInst *AI) {
    OS << "assign " << getID(AI->getSrc()) << " to "
       << getIDAndType(AI->getDest());
  }
  void visitMarkUninitializedInst(MarkUninitializedInst *MU) {
    OS << "mark_uninitialized " << getIDAndType(MU->getOperand());
  }
  void visitMarkFunctionEscapeInst(MarkFunctionEscapeInst *MFE) {
    OS << "mark_function_escape ";
    interleave(MFE->getElements(),
               [&](SILValue Var) {
                 OS << getIDAndType(Var);
               },
               [&] { OS << ", "; });
  }
  void visitLoadWeakInst(LoadWeakInst *LI) {
    OS << "load_weak ";
    if (LI->isTake())
      OS << "[take] ";
    OS << getIDAndType(LI->getOperand());
  }
  void visitStoreWeakInst(StoreWeakInst *SI) {
    OS << "store_weak " << getID(SI->getSrc()) << " to ";
    if (SI->isInitializationOfDest())
      OS << "[initialization] ";
    OS << getIDAndType(SI->getDest());
  }
  void visitCopyAddrInst(CopyAddrInst *CI) {
    OS << "copy_addr ";
    if (CI->isTakeOfSrc())
      OS << "[take] ";
    OS << getID(CI->getSrc()) << " to ";
    if (CI->isInitializationOfDest())
      OS << "[initialization] ";
    OS << getIDAndType(CI->getDest());
  }
  void visitInitializeVarInst(InitializeVarInst *ZI) {
    OS << "initialize_var ";
    if (!ZI->canDefaultConstruct())
      OS << "[no_default_construct] ";
    OS << getIDAndType(ZI->getOperand());
  }
  
  void printUncheckedConversionInst(ConversionInst *CI, SILValue operand,
                                    StringRef name) {
    OS << name << " " << getIDAndType(operand) << " to " << CI->getType();
  }

  StringRef getCastKindName(CheckedCastKind kind) const {
    switch (kind) {
    case CheckedCastKind::Unresolved:
    case CheckedCastKind::InvalidCoercible:
      llvm_unreachable("invalid cast kind for SIL");
    case CheckedCastKind::Downcast:
      return "downcast";
    case CheckedCastKind::SuperToArchetype:
      return "super_to_archetype";
    case CheckedCastKind::ArchetypeToArchetype:
      return "archetype_to_archetype";
    case CheckedCastKind::ArchetypeToConcrete:
      return "archetype_to_concrete";
    case CheckedCastKind::ExistentialToArchetype:
      return "existential_to_archetype";
    case CheckedCastKind::ExistentialToConcrete:
      return "existential_to_concrete";
    }
  }
  
  void visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *CI) {
    OS << "unconditional_checked_cast " << getCastKindName(CI->getCastKind())
       << ' ' << getIDAndType(CI->getOperand())
       << " to " << CI->getType();
  }
  
  void visitCheckedCastBranchInst(CheckedCastBranchInst *CI) {
    OS << "checked_cast_br " << getCastKindName(CI->getCastKind()) << ' '
       << getIDAndType(CI->getOperand())
       << " to " << CI->getCastType() << ", "
       << getID(CI->getSuccessBB()) << ", " << getID(CI->getFailureBB());
  }
  
  void visitConvertFunctionInst(ConvertFunctionInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "convert_function");
  }
  void visitCoerceInst(CoerceInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "coerce");
  }
  void visitUpcastInst(UpcastInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "upcast");
  }
  void visitAddressToPointerInst(AddressToPointerInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "address_to_pointer");
  }
  void visitPointerToAddressInst(PointerToAddressInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "pointer_to_address");
  }
  void visitRefToObjectPointerInst(RefToObjectPointerInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "ref_to_object_pointer");
  }
  void visitObjectPointerToRefInst(ObjectPointerToRefInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "object_pointer_to_ref");
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
  void visitConvertCCInst(ConvertCCInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "convert_cc");
  }
  void visitThinToThickFunctionInst(ThinToThickFunctionInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),"thin_to_thick_function");
  }
  void visitBridgeToBlockInst(BridgeToBlockInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(), "bridge_to_block");
  }
  void visitArchetypeRefToSuperInst(ArchetypeRefToSuperInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),"archetype_ref_to_super");
  }
  void visitUpcastExistentialRefInst(UpcastExistentialRefInst *CI) {
    printUncheckedConversionInst(CI, CI->getOperand(),"upcast_existential_ref");
  }

  void visitIsNonnullInst(IsNonnullInst *I) {
    OS << "is_nonnull " << getIDAndType(I->getOperand());
  }
  
  void visitCopyValueInst(CopyValueInst *I) {
    OS << "copy_value " << getIDAndType(I->getOperand());
  }

  void visitDestroyValueInst(DestroyValueInst *I) {
    OS << "destroy_value " << getIDAndType(I->getOperand());
  }

  void visitStructInst(StructInst *SI) {
    OS << "struct " << SI->getType() << " (";
    interleave(SI->getElements(),
               [&](const SILValue &V) { OS << getIDAndType(V); },
               [&] { OS << ", "; });
    OS << ')';
  }

  void visitTupleInst(TupleInst *TI) {
    OS << "tuple ";
    
    // Check to see if the type of the tuple can be inferred accurately from the
    // elements.
    bool SimpleType = true;
    for (auto &Elt : TI->getType().castTo<TupleType>()->getFields()) {
      if (Elt.hasName() || Elt.isVararg() || Elt.hasInit()) {
        SimpleType = false;
        break;
      }
    }
    
    // If the type is simple, just print the tuple elements.
    if (SimpleType) {
      OS << '(';
      interleave(TI->getElements(),
                 [&](const SILValue &V){ OS << getIDAndType(V); },
                 [&] { OS << ", "; });
      OS << ')';
    } else {
      // Otherwise, print the type, then each value.
      OS << TI->getType() << " (";
      interleave(TI->getElements(),
                 [&](const SILValue &V){ OS << getID(V); },
                 [&] { OS << ", "; });
      OS << ')';
    }
  }
  
  void visitEnumInst(EnumInst *UI) {
    OS << "enum " << UI->getType() << ", "
       << SILDeclRef(UI->getElement(), SILDeclRef::Kind::EnumElement);
    if (UI->hasOperand()) {
      OS << ", " << getIDAndType(UI->getOperand());
    }
  }
  
  void visitEnumDataAddrInst(EnumDataAddrInst *UDAI) {
    OS << "enum_data_addr "
       << getIDAndType(UDAI->getOperand()) << ", "
       << SILDeclRef(UDAI->getElement(), SILDeclRef::Kind::EnumElement);
  }
  
  void visitInjectEnumAddrInst(InjectEnumAddrInst *IUAI) {
    OS << "inject_enum_addr "
       << getIDAndType(IUAI->getOperand()) << ", "
       << SILDeclRef(IUAI->getElement(), SILDeclRef::Kind::EnumElement);
  }
  
  void visitTupleExtractInst(TupleExtractInst *EI) {
    OS << "tuple_extract " << getIDAndType(EI->getOperand()) << ", "
       << EI->getFieldNo();
  }
  void visitTupleElementAddrInst(TupleElementAddrInst *EI) {
    OS << "tuple_element_addr " << getIDAndType(EI->getOperand()) << ", "
       << EI->getFieldNo();
  }
  void visitStructExtractInst(StructExtractInst *EI) {
    OS << "struct_extract " << getIDAndType(EI->getOperand()) << ", #"
       << EI->getField()->getName().get();
  }
  void visitStructElementAddrInst(StructElementAddrInst *EI) {
    OS << "struct_element_addr " << getIDAndType(EI->getOperand()) << ", #"
    << EI->getField()->getName().get();
  }
  void visitRefElementAddrInst(RefElementAddrInst *EI) {
    OS << "ref_element_addr " << getIDAndType(EI->getOperand()) << ", #"
       << EI->getField()->getName().get();
  }
  
  void visitBuiltinZeroInst(BuiltinZeroInst *ZI) {
    OS << "builtin_zero " << ZI->getType();
  }

  void printMethodInst(MethodInst *I, SILValue Operand, StringRef Name) {
    OS << Name << " ";
    if (I->isVolatile())
      OS << "[volatile] ";
    
    OS << getIDAndType(Operand) << ", ";
    I->getMember().print(OS);
  }
  
  void visitClassMethodInst(ClassMethodInst *AMI) {
    printMethodInst(AMI, AMI->getOperand(), "class_method");
    OS << " : " << AMI->getType();
  }
  void visitSuperMethodInst(SuperMethodInst *AMI) {
    printMethodInst(AMI, AMI->getOperand(), "super_method");
    OS << " : " << AMI->getType();
  }
  void visitArchetypeMethodInst(ArchetypeMethodInst *AMI) {
    OS << "archetype_method ";
    if (AMI->isVolatile())
      OS << "[volatile] ";
    AMI->getLookupArchetype().print(OS);
    OS << ", ";
    AMI->getMember().print(OS);
    OS << " : " << AMI->getType(0);
  }
  void visitProtocolMethodInst(ProtocolMethodInst *AMI) {
    printMethodInst(AMI, AMI->getOperand(), "protocol_method");
    OS << " : " << AMI->getType();
  }
  void visitDynamicMethodInst(DynamicMethodInst *DMI) {
    printMethodInst(DMI, DMI->getOperand(), "dynamic_method");
    OS << " : " << DMI->getType();
  }
  void visitProjectExistentialInst(ProjectExistentialInst *PI) {
    OS << "project_existential " << getIDAndType(PI->getOperand())
       << " to " << PI->getType();
  }
  void visitProjectExistentialRefInst(ProjectExistentialRefInst *PI) {
    OS << "project_existential_ref " << getIDAndType(PI->getOperand())
       << " to " << PI->getType();
  }
  void visitInitExistentialInst(InitExistentialInst *AEI) {
    OS << "init_existential " << getIDAndType(AEI->getOperand()) << ", ";
    AEI->getConcreteType().print(OS);
  }
  void visitInitExistentialRefInst(InitExistentialRefInst *AEI) {
    OS << "init_existential_ref " << getIDAndType(AEI->getOperand()) << ", ";
    AEI->getType().print(OS);
  }
  void visitUpcastExistentialInst(UpcastExistentialInst *UEI) {
    OS << "upcast_existential ";
    if (UEI->isTakeOfSrc())
      OS << "[take] ";
    OS << getIDAndType(UEI->getSrcExistential())
       << " to " << getIDAndType(UEI->getDestExistential());
  }
  void visitDeinitExistentialInst(DeinitExistentialInst *DEI) {
    OS << "deinit_existential " << getIDAndType(DEI->getOperand());
  }
  void visitClassMetatypeInst(ClassMetatypeInst *MI) {
    OS << "class_metatype " << MI->getType() << ", "
       << getIDAndType(MI->getOperand());
  }
  void visitArchetypeMetatypeInst(ArchetypeMetatypeInst *MI) {
    OS << "archetype_metatype " << MI->getType() << ", "
       << getIDAndType(MI->getOperand());
  }
  void visitProtocolMetatypeInst(ProtocolMetatypeInst *MI) {
    OS << "protocol_metatype " << MI->getType() << ", "
       << getIDAndType(MI->getOperand());
  }
  void visitMetatypeInst(MetatypeInst *MI) {
    OS << "metatype " << MI->getType();
  }
  void visitModuleInst(ModuleInst *MI) {
    OS << "module #" << MI->getType().castTo<ModuleType>()->getModule()->Name;
  }
  
  void visitStrongRetainInst(StrongRetainInst *RI) {
    OS << "strong_retain " << getIDAndType(RI->getOperand());
  }
  void visitStrongRetainAutoreleasedInst(StrongRetainAutoreleasedInst *RI) {
    OS << "strong_retain_autoreleased " << getIDAndType(RI->getOperand());
  }
  void visitStrongReleaseInst(StrongReleaseInst *RI) {
    OS << "strong_release " << getIDAndType(RI->getOperand());
  }
  void visitStrongRetainUnownedInst(StrongRetainUnownedInst *RI) {
    OS << "strong_retain_unowned " << getIDAndType(RI->getOperand());
  }
  void visitUnownedRetainInst(UnownedRetainInst *RI) {
    OS << "unowned_retain " << getIDAndType(RI->getOperand());
  }
  void visitUnownedReleaseInst(UnownedReleaseInst *RI) {
    OS << "unowned_release " << getIDAndType(RI->getOperand());
  }
  void visitDeallocStackInst(DeallocStackInst *DI) {
    OS << "dealloc_stack " << getIDAndType(DI->getOperand());
  }
  void visitDeallocRefInst(DeallocRefInst *DI) {
    OS << "dealloc_ref " << getIDAndType(DI->getOperand());
  }
  void visitDeallocBoxInst(DeallocBoxInst *DI) {
    OS << "dealloc_box " << DI->getElementType() << ", "
       << getIDAndType(DI->getOperand());
  }
  void visitDestroyAddrInst(DestroyAddrInst *DI) {
    OS << "destroy_addr " << getIDAndType(DI->getOperand());
  }
  
  void visitIndexAddrInst(IndexAddrInst *IAI) {
    OS << "index_addr " << getIDAndType(IAI->getBase()) << ", "
       << getIDAndType(IAI->getIndex());
  }

  void visitIndexRawPointerInst(IndexRawPointerInst *IAI) {
    OS << "index_raw_pointer " << getIDAndType(IAI->getBase()) << ", "
    << getIDAndType(IAI->getIndex());
  }
  
  void visitUnreachableInst(UnreachableInst *UI) {
    OS << "unreachable";
  }

  void visitReturnInst(ReturnInst *RI) {
    OS << "return " << getIDAndType(RI->getOperand());
  }
  
  void visitAutoreleaseReturnInst(AutoreleaseReturnInst *RI) {
    OS << "autorelease_return " << getIDAndType(RI->getOperand());
  }
  
  void visitSwitchIntInst(SwitchIntInst *SII) {
    OS << "switch_int " << getIDAndType(SII->getOperand());
    for (unsigned i = 0, e = SII->getNumCases(); i < e; ++i) {
      APInt value;
      SILBasicBlock *dest;
      std::tie(value, dest) = SII->getCase(i);
      OS << ", case " << value << ": " << getID(dest);
    }
    if (SII->hasDefault())
      OS << ", default " << getID(SII->getDefaultBB());
  }
  
  void printSwitchEnumInst(SwitchEnumInstBase *SOI) {
    OS << getIDAndType(SOI->getOperand());
    for (unsigned i = 0, e = SOI->getNumCases(); i < e; ++i) {
      EnumElementDecl *elt;
      SILBasicBlock *dest;
      std::tie(elt, dest) = SOI->getCase(i);
      OS << ", case " << SILDeclRef(elt, SILDeclRef::Kind::EnumElement)
      << ": " << getID(dest);
    }
    if (SOI->hasDefault())
      OS << ", default " << getID(SOI->getDefaultBB());
  }
  
  void visitSwitchEnumInst(SwitchEnumInst *SOI) {
    OS << "switch_enum ";
    printSwitchEnumInst(SOI);
  }
  void visitDestructiveSwitchEnumAddrInst(DestructiveSwitchEnumAddrInst *SOI){
    OS << "destructive_switch_enum_addr ";
    printSwitchEnumInst(SOI);
  }
  void visitDynamicMethodBranchInst(DynamicMethodBranchInst *DMBI) {
    OS << "dynamic_method_br " << getIDAndType(DMBI->getOperand()) << ", ";
    DMBI->getMember().print(OS);
    OS << ", " << getID(DMBI->getHasMethodBB()) << ", "
       << getID(DMBI->getNoMethodBB());
  }

  void printBranchArgs(OperandValueArrayRef args) {
    if (args.empty()) return;

    OS << '(';
    interleave(args,
               [&](SILValue v) { OS << getIDAndType(v); },
               [&] { OS << ", "; });
    OS << ')';
  }
  
  void visitBranchInst(BranchInst *UBI) {
    OS << "br " << getID(UBI->getDestBB());
    printBranchArgs(UBI->getArgs());
  }

  void visitCondBranchInst(CondBranchInst *CBI) {
    OS << "cond_br " << getID(CBI->getCondition()) << ", "
       << getID(CBI->getTrueBB());
    printBranchArgs(CBI->getTrueArgs());
    OS << ", " << getID(CBI->getFalseBB());
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

  ID R = { ID::SILBasicBlock, BlocksToIDMap[Block], -1 };
  return R;
}

ID SILPrinter::getID(SILValue V) {
  // Lazily initialize the instruction -> ID mapping.
  if (ValueToIDMap.empty()) {
    const SILBasicBlock *ParentBB;
    if (const SILInstruction *I = dyn_cast<SILInstruction>(V))
      ParentBB = I->getParent();
    else
      ParentBB = cast<SILArgument>(V)->getParent();

    // Keep the values in ValueToIDMap with a +1 bias so that lookups will get
    // 0 for invalid numbers.
    unsigned idx = 0;
    for (auto &BB : *ParentBB->getParent()) {
      for (auto I = BB.bbarg_begin(), E = BB.bbarg_end(); I != E; ++I)
        ValueToIDMap[*I] = ++idx;

      for (auto &I : BB) {
        ValueToIDMap[&I] = ++idx;
      }
    }
  }

  int ResultNumber = -1;
  if (V.getDef()->getTypes().size() > 1)
    ResultNumber = V.getResultNumber();

  ID R = { ID::SSAValue, ValueToIDMap[V.getDef()]-1, ResultNumber };
  return R;
}

void swift::WriteAsOperand(raw_ostream &out, SILBasicBlock *BB,
                           bool printType) {
  out << SILPrinter(out).getID(BB);
}

//===----------------------------------------------------------------------===//
// Printing for SILInstruction, SILBasicBlock, SILFunction, and SILModule
//===----------------------------------------------------------------------===//

void ValueBase::dump() const {
  print(llvm::errs());
}

void ValueBase::print(raw_ostream &OS) const {
  SILPrinter(OS).print(this);
}

/// Pretty-print the SILBasicBlock to errs.
void SILBasicBlock::dump() const {
  print(llvm::errs());
}

/// Pretty-print the SILBasicBlock to the designated stream.
void SILBasicBlock::print(raw_ostream &OS) const {
  SILPrinter(OS).print(this);
}

/// Pretty-print the SILFunction to errs.
void SILFunction::dump(bool Verbose) const {
  print(llvm::errs(), Verbose);
}

/// Pretty-print the SILFunction to the designated stream.
void SILFunction::print(llvm::raw_ostream &OS, bool Verbose) const {
  OS << "// " << demangleSymbolAsString(getName()) << '\n';
  OS << "sil ";
  switch (getLinkage()) {
  case SILLinkage::Internal:
    OS << "internal ";
    break;
  case SILLinkage::Thunk:
    OS << "thunk ";
    break;
  case SILLinkage::External:
    break;
  case SILLinkage::Deserialized:
    OS << "deserialized ";
    break;
  }

  if (isTransparent())
    OS << "[transparent] ";
  
  printName(OS);
  OS << " : " << LoweredType;
  
  if (!isExternalDeclaration()) {
    OS << " {\n";
    SILPrinter(OS, Verbose).print(this);
    OS << "}";
  }
  
  OS << "\n\n";
}
      
/// Pretty-print the SILFunction's name using SIL syntax,
/// '@function_mangled_name'.
void SILFunction::printName(raw_ostream &OS) const {
  OS << "@" << Name;  
}
      
/// Pretty-print the SILModule to errs.
void SILModule::dump() const {
  print(llvm::errs());
}

/// Pretty-print the SILModule to the designated stream.
void SILModule::print(llvm::raw_ostream &OS, bool Verbose,
                      TranslationUnit *TU) const {
  OS << "sil_stage ";
  switch (Stage) {
  case SILStage::Raw:
    OS << "raw";
    break;
  case SILStage::Canonical:
    OS << "canonical";
    break;
  }
  
  OS << "\n\nimport Builtin\nimport swift\n\n";

  // Print the declarations and types from the translation unit.
  if (TU) {
    // Compute the list of emitted functions, whose AST Decls we do not need to
    // print.
    llvm::DenseSet<const Decl*> emittedFunctions;
    for (const SILFunction &f : *this)
      if (f.hasLocation())
        emittedFunctions.insert(f.getLocation().getAsASTNode<Decl>());

    PrintOptions Options;
    Options.FunctionDefinitions = false;
    Options.TypeDefinitions = true;
    Options.VarInitializers = true;
    Options.SkipImplicit = true;

    for (auto ID = TU->MainSourceFile->Decls.begin(),
              ED = TU->MainSourceFile->Decls.end(); ID != ED; ++ID) {
      const Decl *D = *ID;
      if ((isa<ValueDecl>(D) || isa<OperatorDecl>(D)) &&
          !emittedFunctions.count(D) &&
          !D->isImplicit()) {
        D->print(OS, Options, nullptr);
        OS << "\n\n";
      }
    }
  }

  for (const SILFunction &f : *this)
    f.print(OS, Verbose);

  for (const SILVTable &vt : getVTables())
    vt.print(OS, Verbose);
  
  OS << "\n\n";
}

void ValueBase::dumpInContext() const {
  printInContext(llvm::errs());
}
void ValueBase::printInContext(llvm::raw_ostream &OS) const {
  SILPrinter(OS).printInContext(this);
}

void SILVTable::print(llvm::raw_ostream &OS, bool Verbose) const {
  // FIXME: When the parser learns to parse back sil_vtable, remove the /**/
  // from the output here.
  
  OS << "/*\n";
  OS << "sil_vtable " << getClass()->getName() << " {\n";
  for (auto &entry : getEntries()) {
    OS << "  ";
    entry.first.print(OS);
    OS << ": " << entry.second->getName() << "\n";
  }
  OS << "}\n";
  OS << "*/\n\n";
}

void SILVTable::dump() const {
  print(llvm::errs());
}
