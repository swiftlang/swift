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

#include "swift/SIL/SILConstant.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Interleave.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/FormattedStream.h"
using namespace swift;

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

void SILConstant::print(raw_ostream &OS) const {
  if (isNull()) {
    OS << "<null>";
    return;
  }
  
  if (hasDecl()) {
    OS << getDecl()->getName();
  } else {
    OS << "<anonymous function>";
  }
  switch (kind) {
  case SILConstant::Kind::Func:
    break;
  case SILConstant::Kind::Getter:
    OS << ".getter";
    break;
  case SILConstant::Kind::Setter:
    OS << ".setter";
    break;
  case SILConstant::Kind::Allocator:
    OS << ".allocator";
    break;
  case SILConstant::Kind::Initializer:
    OS << ".initializer";
    break;
  case SILConstant::Kind::OneOfElement:
    OS << ".oneofelt";
    break;
  case SILConstant::Kind::Destroyer:
    OS << ".destroyer";
    break;
  case SILConstant::Kind::GlobalAccessor:
    OS << ".globalaccessor";
    break;
  }
  if (uncurryLevel != 0) {
    OS << "." << uncurryLevel;
  }
  if (isObjC)
    OS << ".objc";
}

void SILConstant::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void SILType::print(raw_ostream &OS) const {
  // Build up the attributes for a SIL type, if any.
  llvm::SmallString<64> Attributes;
  if (is<AnyFunctionType>()) {
    auto info = getFunctionTypeInfo();

    if (info->getUncurryLevel()) {
      if (!Attributes.empty()) Attributes += ", ";
      Attributes += "sil_uncurry=";
      Attributes += llvm::utostr(info->getUncurryLevel());
    }

    if (info->hasIndirectReturn()) {
      if (!Attributes.empty()) Attributes += ", ";
      Attributes += "sil_sret";
    }
  }


  // If we have any attributes, print them out.
  if (!Attributes.empty())
    OS << '[' << Attributes << "] ";

  if (isAddress())
    OS << '*';

  // Print other types as their Swift representation.
  getSwiftRValueType()->print(OS);
}

void SILType::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

raw_ostream &operator<<(raw_ostream &OS, SILType t) {
  t.print(OS);
  return OS;
}

raw_ostream &operator<<(raw_ostream &OS, SILConstant t) {
  t.print(OS);
  return OS;
}

namespace {
  
/// SILPrinter class - This holds the internal implementation details of
/// printing SIL structures.
class SILPrinter : public SILVisitor<SILPrinter> {
  llvm::formatted_raw_ostream OS;
  SILValue subjectValue;

  llvm::DenseMap<const SILBasicBlock *, unsigned> BlocksToIDMap;
  ID getID(const SILBasicBlock *B);

  llvm::DenseMap<const ValueBase*, unsigned> ValueToIDMap;
  ID getID(SILValue V);

public:
  SILPrinter(raw_ostream &OS) : OS(OS) {
  }

  void print(const SILFunction *F) {
    interleave(F->begin(), F->end(),
               [&](const SILBasicBlock &B) { print(&B); },
               [&] { OS << '\n'; });
  }

  void print(const SILBasicBlock *BB) {
    OS << getID(BB);

    if (!BB->bbarg_empty()) {
      OS << '(';
      for (auto I = BB->bbarg_begin(), E = BB->bbarg_end(); I != E; ++I) {
        if (I != BB->bbarg_begin()) OS << ", ";
        OS << getID(*I) << " : " << (*I)->getType();
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

  void printAsOperand(SILBasicBlock *BB) {
    OS << getID(BB);
  }

  void print(SILValue V) {
    ID Name = getID(V);
    Name.ResultNumber = -1;  // Don't print subresult number.
    OS << "  " << Name << " = ";
    visit(V);
    if (!V->use_empty()) {
      OS.PadToColumn(50);
      OS << "// users: ";
      interleave(V->use_begin(), V->use_end(),
                 [&] (Operand *o) {
                   OS << getID(o->getUser());
                 },
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
  
  void printAllocKind(AllocKind kind) {
    switch (kind) {
    case AllocKind::Heap:
      OS << "heap ";
      break;
    case AllocKind::Pseudo:
      OS << "pseudo ";
      break;
    case AllocKind::Stack:
      OS << "stack ";
      break;
    }
  }

  void visitAllocVarInst(AllocVarInst *AVI) {
    OS << "alloc_var ";
    printAllocKind(AVI->getAllocKind());
    OS << "$" << AVI->getElementType().getString();
    if (VarDecl *vd = AVI->getDecl())
      OS << "  // var " << vd->getName();
  }

  void visitAllocRefInst(AllocRefInst *ARI) {
    OS << "alloc_ref ";
    printAllocKind(ARI->getAllocKind());
    OS << "$" << ARI->getType().getSwiftType().getString();
  }
  
  void visitAllocBoxInst(AllocBoxInst *ABI) {
    OS << "alloc_box $" << ABI->getElementType().getString();
  }

  void visitAllocArrayInst(AllocArrayInst *AAI) {
    OS << "alloc_array $" << AAI->getElementType().getString()
       << ", " << getID(AAI->getNumElements());
  }
  
  void printFunctionInst(FunctionInst *FI) {
    OS << getID(FI->getCallee()) << '(';
    bool first = true;
    for (auto arg : FI->getArguments()) {
      if (first)
        first = false;
      else
        OS << ", ";
      OS << getID(arg);
    }
    OS << ')';
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
    OS << "function_ref $" << DRI->getType() << ", ";
    DRI->getFunction()->printName(OS);
  }
  
  void visitBuiltinFunctionRefInst(BuiltinFunctionRefInst *BFI) {
    OS << "builtin_function_ref $" << BFI->getType()
       << ", @" << BFI->getFunction()->getName();
  }
  
  void visitGlobalAddrInst(GlobalAddrInst *GAI) {
    OS << "global_addr $" << GAI->getType() << ", @";
    OS << GAI->getGlobal()->getName();
  }

  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
    const auto &lit = ILI->getValue();
    OS << "integer_literal $" << ILI->getType() << ", " << lit;
  }
  void visitFloatLiteralInst(FloatLiteralInst *FLI) {
    SmallVector<char, 12> Buffer;
    FLI->getValue().toString(Buffer);
    OS << "float_literal $" << FLI->getType() << ", "
       << StringRef(Buffer.data(), Buffer.size());
  }
  void visitStringLiteralInst(StringLiteralInst *SLI) {
    OS << "string_literal $" << SLI->getType()
       << ", \"" << SLI->getValue() << "\"";
  }
  void visitLoadInst(LoadInst *LI) {
    OS << "load " << getID(LI->getOperand());
  }
  void visitStoreInst(StoreInst *SI) {
    OS << "store " << getID(SI->getSrc()) << " to " << getID(SI->getDest());
  }
  void visitCopyAddrInst(CopyAddrInst *CI) {
    OS << "copy_addr " << getID(CI->getSrc());
    if (CI->isTakeOfSrc())
      OS << " [take]";
    OS << " to " << getID(CI->getDest());
    if (CI->isInitializationOfDest())
      OS << " [initialization]";
  }
  void visitInitializeVarInst(InitializeVarInst *ZI) {
    OS << "initialize_var ";
    if (!ZI->canDefaultConstruct())
      OS << "[no_default_construct] ";
    OS << getID(ZI->getOperand());
  }
  void visitSpecializeInst(SpecializeInst *SI) {
    OS << "specialize " << getID(SI->getOperand()) << ", $"
       << SI->getType() << ", ";
    bool first = true;
    for (Substitution const &s : SI->getSubstitutions()) {
      if (!first)
        OS << ", ";
      s.Archetype->print(OS);
      OS << " = ";
      s.Replacement->print(OS);
      first = false;
    }
  }
  
  void printConversionInst(ConversionInst *CI,
                           SILValue operand,
                           llvm::StringRef name) {
    OS << name << " " << getID(operand) << ", $"
      << CI->getType();
  }
  
  void visitConvertFunctionInst(ConvertFunctionInst *CI) {
    printConversionInst(CI, CI->getOperand(), "convert_function");
  }
  void visitCoerceInst(CoerceInst *CI) {
    printConversionInst(CI, CI->getOperand(), "coerce");
  }
  void visitUpcastInst(UpcastInst *CI) {
    printConversionInst(CI, CI->getOperand(), "upcast");
  }
  void visitDowncastInst(DowncastInst *CI) {
    printConversionInst(CI, CI->getOperand(), "downcast");
  }
  void visitAddressToPointerInst(AddressToPointerInst *CI) {
    printConversionInst(CI, CI->getOperand(), "address_to_pointer");
  }
  void visitPointerToAddressInst(PointerToAddressInst *CI) {
    printConversionInst(CI, CI->getOperand(), "pointer_to_address");
  }
  void visitRefToObjectPointerInst(RefToObjectPointerInst *CI) {
    printConversionInst(CI, CI->getOperand(), "ref_to_object_pointer");
  }
  void visitObjectPointerToRefInst(ObjectPointerToRefInst *CI) {
    printConversionInst(CI, CI->getOperand(), "object_pointer_to_ref");
  }
  void visitRefToRawPointerInst(RefToRawPointerInst *CI) {
    printConversionInst(CI, CI->getOperand(), "ref_to_raw_pointer");
  }
  void visitRawPointerToRefInst(RawPointerToRefInst *CI) {
    printConversionInst(CI, CI->getOperand(), "raw_pointer_to_ref");
  }
  void visitConvertCCInst(ConvertCCInst *CI) {
    printConversionInst(CI, CI->getOperand(), "convert_cc");
  }
  void visitThinToThickFunctionInst(ThinToThickFunctionInst *CI) {
    printConversionInst(CI, CI->getOperand(), "thin_to_thick_function");
  }
  void visitBridgeToBlockInst(BridgeToBlockInst *CI) {
    printConversionInst(CI, CI->getOperand(), "bridge_to_block");
  }
  void visitArchetypeToSuperInst(ArchetypeToSuperInst *CI) {
    printConversionInst(CI, CI->getOperand(), "archetype_to_super");
  }

  void visitSuperToArchetypeInst(SuperToArchetypeInst *I) {
    OS << "super_to_archetype " << getID(I->getSrcBase()) << " to "
       << getID(I->getDestArchetypeAddress());
  }
  
  void visitIsaInst(IsaInst *I) {
    OS << "isa " << getID(I->getOperand()) << ", $" << I->getTestType();
  }

  void visitStructInst(StructInst *TI) {
    OS << "struct $";
    TI->getType().print(OS);
    OS << ", (";
    bool isFirst = true;
    for (const auto &Elem : TI->getElements()) {
      if (isFirst)
        isFirst = false;
      else
        OS << ", ";
      OS << getID(Elem);
    }
    OS << ')';
  }

  void visitTupleInst(TupleInst *TI) {
    OS << "tuple (";
    bool isFirst = true;
    for (const auto &Elem : TI->getElements()) {
      if (isFirst)
        isFirst = false;
      else
        OS << ", ";
      OS << getID(Elem);
    }
    OS << ')';
  }
  void visitTupleExtractInst(TupleExtractInst *EI) {
    OS << "tuple_extract " << getID(EI->getOperand()) << ", "
       << EI->getFieldNo();
  }
  void visitTupleElementAddrInst(TupleElementAddrInst *EI) {
    OS << "tuple_element_addr " << getID(EI->getOperand()) << ", "
       << EI->getFieldNo();
  }
  void visitStructExtractInst(StructExtractInst *EI) {
    OS << "struct_extract " << getID(EI->getOperand()) << ", @"
    << EI->getField()->getName().get();
  }
  void visitStructElementAddrInst(StructElementAddrInst *EI) {
    OS << "struct_element_addr " << getID(EI->getOperand()) << ", @"
    << EI->getField()->getName().get();
  }
  void visitRefElementAddrInst(RefElementAddrInst *EI) {
    OS << "ref_element_addr " << getID(EI->getOperand()) << ", @"
       << EI->getField()->getName().get();
  }
  
  void visitBuiltinZeroInst(BuiltinZeroInst *ZI) {
    OS << "builtin_zero $" << ZI->getType();
  }

  void printDynamicMethodInst(DynamicMethodInst *I,
                              SILValue Operand,
                              StringRef Name) {
    OS << Name << " ";
    if (I->isVolatile())
      OS << "[volatile] ";
    
    OS << getID(Operand) << ", @";
    I->getMember().print(OS);
  }
  
  void visitClassMethodInst(ClassMethodInst *AMI) {
    printDynamicMethodInst(AMI, AMI->getOperand(), "class_method");
  }
  void visitSuperMethodInst(SuperMethodInst *AMI) {
    printDynamicMethodInst(AMI, AMI->getOperand(), "super_method");
  }
  void visitArchetypeMethodInst(ArchetypeMethodInst *AMI) {
    OS << "archetype_method ";
    if (AMI->isVolatile())
      OS << "[volatile] ";
    OS << "$";
    AMI->getLookupArchetype().print(OS);
    OS << ", @";
    AMI->getMember().print(OS);
  }
  void visitProtocolMethodInst(ProtocolMethodInst *AMI) {
    printDynamicMethodInst(AMI, AMI->getOperand(), "protocol_method");
  }
  void visitProjectExistentialInst(ProjectExistentialInst *PI) {
    OS << "project_existential " << getID(PI->getOperand());
  }
  void visitInitExistentialInst(InitExistentialInst *AEI) {
    OS << "init_existential " << getID(AEI->getOperand()) << ", $";
    AEI->getConcreteType().print(OS);
  }
  void visitUpcastExistentialInst(UpcastExistentialInst *UEI) {
    OS << "upcast_existential ";
    if (UEI->isTakeOfSrc())
      OS << "[take] ";
    OS << getID(UEI->getSrcExistential())
       << " to " << getID(UEI->getDestExistential());
  }
  void visitDeinitExistentialInst(DeinitExistentialInst *DEI) {
    OS << "deinit_existential " << getID(DEI->getOperand());
  }
  void visitClassMetatypeInst(ClassMetatypeInst *MI) {
    OS << "class_metatype $" << MI->getType() << ", " << getID(MI->getOperand());
  }
  void visitArchetypeMetatypeInst(ArchetypeMetatypeInst *MI) {
    OS << "archetype_metatype $" << MI->getType() << ", "
       << getID(MI->getOperand());
  }
  void visitProtocolMetatypeInst(ProtocolMetatypeInst *MI) {
    OS << "protocol_metatype $" << MI->getType() << ", "
       << getID(MI->getOperand());
  }
  void visitMetatypeInst(MetatypeInst *MI) {
    OS << "metatype $" << MI->getType();
  }
  void visitModuleInst(ModuleInst *MI) {
    OS << "module @" << MI->getType().castTo<ModuleType>()->getModule()->Name;
  }
  void visitAssociatedMetatypeInst(AssociatedMetatypeInst *MI) {
    OS << "associated_metatype " << getID(MI->getOperand())
       << ", $" << MI->getType();
  }
  
  void visitRetainInst(RetainInst *RI) {
    OS << "retain " << getID(RI->getOperand());
  }
  void visitRetainAutoreleasedInst(RetainAutoreleasedInst *RI) {
    OS << "retain_autoreleased " << getID(RI->getOperand());
  }
  void visitReleaseInst(ReleaseInst *RI) {
    OS << "release " << getID(RI->getOperand());
  }
  void visitDeallocVarInst(DeallocVarInst *DI) {
    OS << "dealloc_var ";
    printAllocKind(DI->getAllocKind());
    OS << getID(DI->getOperand());
  }
  void visitDeallocRefInst(DeallocRefInst *DI) {
    OS << "dealloc_ref " << getID(DI->getOperand());
  }
  void visitDestroyAddrInst(DestroyAddrInst *DI) {
    OS << "destroy_addr " << getID(DI->getOperand());
  }
  
  void visitIndexAddrInst(IndexAddrInst *IAI) {
    OS << "index_addr " << getID(IAI->getOperand()) << ", " <<IAI->getIndex();
  }

  void visitUnreachableInst(UnreachableInst *UI) {
    OS << "unreachable";
  }

  void visitReturnInst(ReturnInst *RI) {
    OS << "return " << getID(RI->getOperand());
  }
  
  void visitAutoreleaseReturnInst(AutoreleaseReturnInst *RI) {
    OS << "autorelease_return " << getID(RI->getOperand());
  }

  void printBranchArgs(OperandValueArrayRef args) {
    if (!args.empty()) {
      OS << '(';
      interleave(args.begin(), args.end(),
                 [&](SILValue v) { OS << getID(v); },
                 [&] { OS << ", "; });
      OS << ')';
    }
    
  }
  
  void visitBranchInst(BranchInst *UBI) {
    OS << "br ";
    printAsOperand(UBI->getDestBB());
    printBranchArgs(UBI->getArgs());
  }

  void visitCondBranchInst(CondBranchInst *CBI) {
    OS << "condbranch " << getID(CBI->getCondition()) << ", ";
    printAsOperand(CBI->getTrueBB());
    printBranchArgs(CBI->getTrueArgs());
    OS << ", ";
    printAsOperand(CBI->getFalseBB());
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

    unsigned idx = 0;
    for (auto &BB : *ParentBB->getParent()) {
      for (auto I = BB.bbarg_begin(), E = BB.bbarg_end(); I != E; ++I)
        ValueToIDMap[*I] = idx++;

      for (auto &I : BB)
        ValueToIDMap[&I] = idx++;
    }
  }

  int ResultNumber = -1;
  if (V.getDef()->getTypes().size() > 1)
    ResultNumber = V.getResultNumber();

  ID R = { ID::SSAValue, ValueToIDMap[V.getDef()], ResultNumber };
  return R;
}

void swift::WriteAsOperand(raw_ostream &out, SILBasicBlock *BB,
                           bool printType) {
  SILPrinter(out).printAsOperand(BB);
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
  OS << " : $" << LoweredType;
  
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
  OS << "@" << MangledName;  
}

/// Verify the module.
void SILModule::verify() const {
#ifndef NDEBUG
  llvm::StringSet<> functionNames;
  for (SILFunction const &f : *this) {
    if (!functionNames.insert(f.getMangledName())) {
      llvm::errs() << "Function redefined: " << f.getMangledName() << "!\n";
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
  for (SILFunction const &f : *this)
    f.print(OS);
}

void ValueBase::dumpInContext() const {
  SILPrinter(llvm::errs()).printInContext(this);
}

//===----------------------------------------------------------------------===//
// Printing for PrettyStackTrace
//===----------------------------------------------------------------------===//

void PrettyStackTraceSILFunction::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  SourceLoc sloc;
  F->printName(out);

  out << " at ";
  printSourceLoc(out, sloc, F->getASTContext());
  out << '\n';
}
