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
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/FormattedStream.h"

using namespace swift;
using namespace Demangle;

struct ID {
  enum {
    SILBasicBlock, SSAValue
  } Kind;
  unsigned Number;
  int ResultNumber;
};

static raw_ostream &operator<<(raw_ostream &OS, ID i) {
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
  return OS << i.id << " : " << i.Ty;
}

/// Return the fully qualified dotted path for DeclContext.
static void printFullContext(const DeclContext *Context, raw_ostream &Buffer) {
  if (!Context)
    return;
  switch (Context->getContextKind()) {
  case DeclContextKind::BuiltinModule:
    Buffer << cast<BuiltinModule>(Context)->Name << ".";
    return;
  case DeclContextKind::ClangModule:
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::SerializedModule:
    return;

  case DeclContextKind::CapturingExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::ConstructorDecl:
  case DeclContextKind::DestructorDecl:
    llvm_unreachable("unhandled context type in SILPrint!");

  case DeclContextKind::ExtensionDecl: {
    Type Ty = cast<ExtensionDecl>(Context)->getExtendedType();
    TypeBase *Base = Ty->getCanonicalType().getPointer();
    const NominalTypeDecl *ExtNominal = 0;
    switch (Base->getKind()) {
      default:
        llvm_unreachable("unhandled context type in SILPrint!");
      case TypeKind::Union:
        ExtNominal = cast<UnionType>(Base)->getDecl();
        break;
      case TypeKind::Struct:
        ExtNominal = cast<StructType>(Base)->getDecl();
        break;
      case TypeKind::Class:
        ExtNominal = cast<ClassType>(Base)->getDecl();
        break;
      case TypeKind::BoundGenericUnion:
        ExtNominal = cast<BoundGenericUnionType>(Base)->getDecl();
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

  case DeclContextKind::NominalTypeDecl: {
    const NominalTypeDecl *Nominal = cast<NominalTypeDecl>(Context);
    printFullContext(Nominal->getDeclContext(), Buffer);
    Buffer << Nominal->getName() << ".";
    return;
  }
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
  case SILDeclRef::Kind::UnionElement:
    OS << "!unionelt";
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
  if (isObjC) {
    if (uncurryLevel != 0 || kind != SILDeclRef::Kind::Func)
      OS << ".objc";
    else
      OS << "!objc";
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
  case SILValueCategory::LocalStorage: OS << "*[local_storage] "; return;
  }
  llvm_unreachable("bad value category!");
}
      
void SILType::print(raw_ostream &OS) const {
  OS << '$';

  // Potentially add a leading sigil for the value category.
  ::print(OS, getCategory());

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

  llvm::DenseMap<const SILBasicBlock *, unsigned> BlocksToIDMap;

  llvm::DenseMap<const ValueBase*, unsigned> ValueToIDMap;
public:
  SILPrinter(raw_ostream &OS) : OS(OS) {
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
    OS << "  ";
    if (V->hasValue()) {
      ID Name = getID(V);
      Name.ResultNumber = -1;  // Don't print subresult number.
      OS << Name << " = ";
    }
    visit(V);
    if (!V->use_empty()) {
      OS.PadToColumn(50);
      OS << "// user";
      if (std::next(V->use_begin()) != V->use_end())
        OS << 's';
      OS << ": ";
      interleave(V->use_begin(), V->use_end(),
                 [&] (Operand *o) { OS << getID(o->getUser()); },
                 [&] { OS << ", "; });
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
    OS << "argument of " << getID(A->getParent());
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
  
  void printFunctionInst(FunctionInst *FI) {
    OS << getID(FI->getCallee()) << '(';
    interleave(FI->getArguments(),
               [&](const SILValue &arg) { OS << getID(arg); },
               [&] { OS << ", "; });
    OS << ") : " << FI->getCallee().getType();
  }

  void visitApplyInst(ApplyInst *AI) {
    OS << "apply ";
    printFunctionInst(AI);
  }
  
  void visitPartialApplyInst(PartialApplyInst *CI) {
    OS << "partial_apply ";
    printFunctionInst(CI);
  }

  void visitFunctionRefInst(FunctionRefInst *DRI) {
    OS << "function_ref ";
    DRI->getFunction()->printName(OS);
    OS << " : " << DRI->getType();
    OS << " // " << demangleSymbol(DRI->getFunction()->getName());
  }
  
  void visitBuiltinFunctionRefInst(BuiltinFunctionRefInst *BFI) {
    OS << "builtin_function_ref " << SILDeclRef(BFI->getFunction())
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
  void visitSpecializeInst(SpecializeInst *SI) {
    OS << "specialize " << getID(SI->getOperand()) << ", "
       << SI->getType() << ", ";
    interleave(SI->getSubstitutions(),
               [&](const Substitution &s) {
                 s.Archetype->print(OS);
                 OS << " = ";
                 s.Replacement->print(OS);
               },
               [&] { OS << ", "; });
  }
  
  void printUncheckedConversionInst(ConversionInst *CI,
                                    SILValue operand,
                                    llvm::StringRef name) {
    OS << name << " " << getIDAndType(operand) << " to " << CI->getType();
  }

  void printCheckedConversionInst(CheckedConversionInst *CI, SILValue operand,
                                  llvm::StringRef name) {
    OS << name;
    switch (CI->getMode()) {
    case CheckedCastMode::Conditional:
      OS << " conditional ";
      break;
    case CheckedCastMode::Unconditional:
      OS << " unconditional ";
      break;
    }
    OS << getIDAndType(operand) << " to " << CI->getType();
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
  void visitDowncastInst(DowncastInst *CI) {
    printCheckedConversionInst(CI, CI->getOperand(), "downcast");
  }
  void visitSuperToArchetypeRefInst(SuperToArchetypeRefInst *CI) {
    printCheckedConversionInst(CI, CI->getOperand(),
                               "super_to_archetype_ref");
  }
  void visitDowncastArchetypeAddrInst(DowncastArchetypeAddrInst *CI) {
    printCheckedConversionInst(CI, CI->getOperand(), "downcast_archetype_addr");
  }
  void visitDowncastArchetypeRefInst(DowncastArchetypeRefInst *CI) {
    printCheckedConversionInst(CI, CI->getOperand(), "downcast_archetype_ref");
  }
  void visitProjectDowncastExistentialAddrInst(
                                      ProjectDowncastExistentialAddrInst *CI) {
    printCheckedConversionInst(CI, CI->getOperand(),
                               "project_downcast_existential_addr");
  }
  void visitDowncastExistentialRefInst(DowncastExistentialRefInst *CI) {
    printCheckedConversionInst(CI, CI->getOperand(), "downcast_existential_ref");
  }

  void visitIsNonnullInst(IsNonnullInst *I) {
    OS << "is_nonnull " << getIDAndType(I->getOperand());
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

  void printDynamicMethodInst(DynamicMethodInst *I,
                              SILValue Operand,
                              StringRef Name) {
    OS << Name << " ";
    if (I->isVolatile())
      OS << "[volatile] ";
    
    OS << getIDAndType(Operand) << ", ";
    I->getMember().print(OS);
  }
  
  void visitClassMethodInst(ClassMethodInst *AMI) {
    printDynamicMethodInst(AMI, AMI->getOperand(), "class_method");
    OS << " : " << AMI->getType();
  }
  void visitSuperMethodInst(SuperMethodInst *AMI) {
    printDynamicMethodInst(AMI, AMI->getOperand(), "super_method");
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
    printDynamicMethodInst(AMI, AMI->getOperand(), "protocol_method");
    OS << " : " << AMI->getType();
  }
  void visitProjectExistentialInst(ProjectExistentialInst *PI) {
    OS << "project_existential " << getIDAndType(PI->getOperand())
      << " to " << PI->getType();
  }
  void visitProjectExistentialRefInst(ProjectExistentialRefInst *PI) {
    OS << "project_existential_ref " << getIDAndType(PI->getOperand());
  }
  void visitInitExistentialInst(InitExistentialInst *AEI) {
    OS << "init_existential " << getID(AEI->getOperand()) << ", ";
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
    OS << "deinit_existential " << getID(DEI->getOperand());
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
  
  void visitRetainInst(RetainInst *RI) {
    OS << "retain " << getIDAndType(RI->getOperand());
  }
  void visitRetainAutoreleasedInst(RetainAutoreleasedInst *RI) {
    OS << "retain_autoreleased " << getIDAndType(RI->getOperand());
  }
  void visitReleaseInst(ReleaseInst *RI) {
    OS << "release " << getIDAndType(RI->getOperand());
  }
  void visitRetainUnownedInst(RetainUnownedInst *RI) {
    OS << "retain_unowned " << getIDAndType(RI->getOperand());
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
  
  void visitSwitchUnionInst(SwitchUnionInst *SOI) {
    OS << "switch_union " << getIDAndType(SOI->getOperand());
    for (unsigned i = 0, e = SOI->getNumCases(); i < e; ++i) {
      UnionElementDecl *elt;
      SILBasicBlock *dest;
      std::tie(elt, dest) = SOI->getCase(i);
      OS << ", case " << SILDeclRef(elt, SILDeclRef::Kind::UnionElement)
         << ": " << getID(dest);
    }
    if (SOI->hasDefault())
      OS << ", default " << getID(SOI->getDefaultBB());
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
    OS << "condbranch " << getID(CBI->getCondition()) << ", "
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
void SILFunction::dump() const {
  print(llvm::errs());
}

/// Pretty-print the SILFunction to the designated stream.
void SILFunction::print(llvm::raw_ostream &OS) const {
  OS << "// " << demangleSymbol(getName()) << '\n';
  OS << "sil ";
  switch (getLinkage()) {
  case SILLinkage::Internal:
    OS << "internal ";
    break;
  case SILLinkage::ClangThunk:
    OS << "clang_thunk ";
    break;
  case SILLinkage::External:
    break;
  }
  
  printName(OS);
  OS << " : " << LoweredType;
  
  if (!isExternalDeclaration()) {
    OS << " {\n";
    SILPrinter(OS).print(this);
    OS << "}";
  }
  
  OS << "\n\n";
}
      
/// Pretty-print the SILFunction's name using SIL syntax,
/// '@function_mangled_name'.
void SILFunction::printName(raw_ostream &OS) const {
  OS << "@" << Name;  
}

/// Verify the module.
void SILModule::verify() const {
#ifndef NDEBUG
  llvm::StringSet<> functionNames;
  for (SILFunction const &f : *this) {
    if (!functionNames.insert(f.getName())) {
      llvm::errs() << "Function redefined: " << f.getName() << "!\n";
      assert(false && "triggering standard assertion failure routine");
    }
    f.verify();
  }
#endif
}
      
/// Pretty-print the SILModule to errs.
void SILModule::dump() const {
  print(llvm::errs());
}

/// Pretty-print the SILModule to the designated stream.
void SILModule::print(llvm::raw_ostream &OS) const {
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

  for (const SILFunction &f : *this)
    f.print(OS);
}

void ValueBase::dumpInContext() const {
  SILPrinter(llvm::errs()).printInContext(this);
}
