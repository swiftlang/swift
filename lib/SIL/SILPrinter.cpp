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
#include "llvm/Support/FormattedStream.h"
using namespace swift;

struct ID {
  enum {
    BasicBlock, SSAValue
  } Kind;
  unsigned Number;
  int ResultNumber;
};

static raw_ostream &operator<<(raw_ostream &OS, ID i) {
  switch (i.Kind) {
  case ID::BasicBlock: OS << "bb"; break;
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
  case SILConstant::Kind::Destructor:
    OS << ".destructor";
    break;
  case SILConstant::Kind::GlobalAccessor:
    OS << ".globalaccessor";
    break;
  case SILConstant::Kind::GlobalAddress:
    OS << ".globaladdress";
    break;
  }
  if (uncurryLevel != 0) {
    OS << "." << uncurryLevel;
  }
}

void SILConstant::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void SILType::print(raw_ostream &OS) const {
  if (isAddress()) {
    OS << "*";
  }
  CanType swiftTy = getSwiftRValueType();
  unsigned uncurries = uncurryLevel;
  while (uncurries-- > 0) {
    AnyFunctionType *fTy = cast<AnyFunctionType>(swiftTy);
    if (auto *pfTy = dyn_cast<PolymorphicFunctionType>(fTy))
      pfTy->printGenericParams(OS);
    bool hasParens = fTy->getInput()->is<TupleType>();
    if (!hasParens) OS << '(';
    fTy->getInput()->print(OS);
    if (!hasParens) OS << ')';
    swiftTy = CanType(fTy->getResult());
  }
  swiftTy->print(OS);
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

  llvm::DenseMap<const BasicBlock *, unsigned> BlocksToIDMap;
  ID getID(const BasicBlock *B);

  llvm::DenseMap<const ValueBase*, unsigned> ValueToIDMap;
  ID getID(Value V);

public:
  SILPrinter(raw_ostream &OS) : OS(OS) {
  }

  void print(const Function *F) {
    interleave(F->begin(), F->end(),
               [&](const BasicBlock &B) { print(&B); },
               [&] { OS << '\n'; });
  }

  void print(const BasicBlock *BB) {
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
      OS << "; Preds:";
      for (auto BBI = BB->pred_begin(), E = BB->pred_end(); BBI != E; ++BBI)
        OS << ' ' << getID(*BBI);
    }
    OS << '\n';

    for (const Instruction &I : *BB)
      print(&I);
  }

  //===--------------------------------------------------------------------===//
  // Instruction Printing Logic

  void print(Value V) {
    ID Name = getID(V);
    Name.ResultNumber = -1;  // Don't print subresult number.
    OS << "  " << Name << " = ";
    visit(V);
    if (!V->use_empty()) {
      OS.PadToColumn(50);
      OS << "; users: ";
      interleave(V->use_begin(), V->use_end(),
                 [&] (Operand *o) {
                   OS << getID(o->getUser());
                 },
                 [&] { OS << ", "; });
    }
    OS << '\n';
  }
  void visitInstruction(Instruction *I) {
    assert(0 && "SILPrinter not implemented for this instruction!");
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
      OS << "  ; var " << vd->getName();
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

  void visitConstantRefInst(ConstantRefInst *DRI) {
    OS << "constant_ref $" << DRI->getType(0) << ", @";
    DRI->getConstant().print(OS);
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
    OS << "string_literal \"" << SLI->getValue() << "\"";
  }
  void visitLoadInst(LoadInst *LI) {
    OS << "load " << getID(LI->getLValue());
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
    OS << "initialize_var " << getID(ZI->getDest());
  }
  void visitSpecializeInst(SpecializeInst *SI) {
    OS << "specialize " << getID(SI->getOperand()) << ", $"
       << SI->getType() << "  ; ";
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
  
  void printConversionInst(ConversionInst *CI, llvm::StringRef name) {
    OS << name << " " << getID(CI->getOperand()) << ", $"
      << CI->getType();
  }
  
  void visitConvertFunctionInst(ConvertFunctionInst *CI) {
    printConversionInst(CI, "convert_function");
  }
  void visitCoerceInst(CoerceInst *CI) {
    printConversionInst(CI, "coerce");
  }
  void visitUpcastInst(UpcastInst *CI) {
    printConversionInst(CI, "upcast");
  }
  void visitDowncastInst(DowncastInst *CI) {
    printConversionInst(CI, "downcast");
  }
  void visitAddressToPointerInst(AddressToPointerInst *CI) {
    printConversionInst(CI, "address_to_pointer");
  }
  void visitThinToThickFunctionInst(ThinToThickFunctionInst *CI) {
    printConversionInst(CI, "thin_to_thick_function");
  }
  void visitArchetypeToSuperInst(ArchetypeToSuperInst *CI) {
    printConversionInst(CI, "archetype_to_super");
  }

  void visitSuperToArchetypeInst(SuperToArchetypeInst *I) {
    OS << "super_to_archetype " << getID(I->getSrcBase()) << " to "
       << getID(I->getDestArchetypeAddress());
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
  void visitExtractInst(ExtractInst *EI) {
    OS << "extract " << getID(EI->getOperand()) << ", "
       << EI->getFieldNo();
  }
  void visitElementAddrInst(ElementAddrInst *EI) {
    OS << "element_addr " << getID(EI->getOperand()) << ", "
       << EI->getFieldNo();
  }
  void visitRefElementAddrInst(RefElementAddrInst *EI) {
    OS << "ref_element_addr " << getID(EI->getOperand()) << ", @"
       << EI->getField()->getName().get();
  }
  void visitClassMethodInst(ClassMethodInst *AMI) {
    OS << "class_method " << getID(AMI->getOperand()) << ", @";
    AMI->getMember().print(OS);
  }
  void visitSuperMethodInst(SuperMethodInst *AMI) {
    OS << "super_method " << getID(AMI->getOperand()) << ", @";
    AMI->getMember().print(OS);
  }
  void visitArchetypeMethodInst(ArchetypeMethodInst *AMI) {
    OS << "archetype_method " << getID(AMI->getOperand()) << ", @";
    AMI->getMember().print(OS);
  }
  void visitProtocolMethodInst(ProtocolMethodInst *AMI) {
    OS << "protocol_method " << getID(AMI->getOperand()) << ", @";
    AMI->getMember().print(OS);
  }
  void visitProjectExistentialInst(ProjectExistentialInst *PI) {
    OS << "project_existential " << getID(PI->getOperand());
  }
  void visitInitExistentialInst(InitExistentialInst *AEI) {
    OS << "init_existential " << getID(AEI->getExistential()) << ", $";
    AEI->getConcreteType()->print(OS);
  }
  void visitUpcastExistentialInst(UpcastExistentialInst *UEI) {
    OS << "upcast_existential ";
    if (UEI->isTakeOfSrc())
      OS << "[take] ";
    OS << getID(UEI->getSrcExistential())
       << " to " << getID(UEI->getDestExistential());
  }
  void visitDeinitExistentialInst(DeinitExistentialInst *DEI) {
    OS << "deinit_existential " << getID(DEI->getExistential());
  }
  void visitClassMetatypeInst(ClassMetatypeInst *MI) {
    OS << "class_metatype $" << MI->getType() << ", " << getID(MI->getBase());
  }
  void visitArchetypeMetatypeInst(ArchetypeMetatypeInst *MI) {
    OS << "archetype_metatype $" << MI->getType() << ", " << getID(MI->getBase());
  }
  void visitProtocolMetatypeInst(ProtocolMetatypeInst *MI) {
    OS << "protocol_metatype $" << MI->getType() << ", " << getID(MI->getBase());
  }
  void visitMetatypeInst(MetatypeInst *MI) {
    OS << "metatype $" << MI->getType();
  }
  void visitModuleInst(ModuleInst *MI) {
    OS << "module @" << MI->getType().castTo<ModuleType>()->getModule()->Name;
  }
  void visitAssociatedMetatypeInst(AssociatedMetatypeInst *MI) {
    OS << "associated_metatype " << getID(MI->getSourceMetatype())
       << ", $" << MI->getType();
  }
  
  void visitRetainInst(RetainInst *RI) {
    OS << "retain " << getID(RI->getOperand());
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

  void visitIntegerValueInst(IntegerValueInst *IVI) {
    OS << "integer_value " << IVI->getValue() << ", $"
       << IVI->getType();
  }

  void visitUnreachableInst(UnreachableInst *UI) {
    OS << "unreachable";
  }

  void visitReturnInst(ReturnInst *RI) {
    OS << "return " << '(' << getID(RI->getReturnValue()) << ')';
  }

  void printBranchArgs(OperandValueArrayRef args) {
    if (!args.empty()) {
      OS << '(';
      interleave(args.begin(), args.end(),
                 [&](Value v) { OS << getID(v); },
                 [&]() { OS << ", "; });
      OS << ')';
    }
    
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

ID SILPrinter::getID(const BasicBlock *Block) {
  // Lazily initialize the Blocks-to-IDs mapping.
  if (BlocksToIDMap.empty()) {
    unsigned idx = 0;
    for (const BasicBlock &B : *Block->getParent())
      BlocksToIDMap[&B] = idx++;
  }

  ID R = { ID::BasicBlock, BlocksToIDMap[Block], -1 };
  return R;
}

ID SILPrinter::getID(Value V) {
  // Lazily initialize the instruction -> ID mapping.
  if (ValueToIDMap.empty()) {
    const BasicBlock *ParentBB;
    if (const Instruction *I = dyn_cast<Instruction>(V))
      ParentBB = I->getParent();
    else
      ParentBB = cast<BBArgument>(V)->getParent();

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

//===----------------------------------------------------------------------===//
// Printing for Instruction, BasicBlock, Function, and SILModule
//===----------------------------------------------------------------------===//

void ValueBase::dump() const {
  print(llvm::errs());
}

void ValueBase::print(raw_ostream &OS) const {
  SILPrinter(OS).print(this);
}

/// Pretty-print the BasicBlock to errs.
void BasicBlock::dump() const {
  print(llvm::errs());
}

/// Pretty-print the BasicBlock to the designated stream.
void BasicBlock::print(raw_ostream &OS) const {
  SILPrinter(OS).print(this);
}

/// Pretty-print the Function to errs.
void Function::dump() const {
  print(llvm::errs());
}

/// Pretty-print the Function to the designated stream.
void Function::print(llvm::raw_ostream &OS) const {
  SILPrinter(OS).print(this);
}

/// Pretty-print the SILModule to errs.
void SILModule::dump() const {
  print(llvm::errs());
}

/// Pretty-print the SILModule to the designated stream.
void SILModule::print(llvm::raw_ostream &OS) const {
  for (std::pair<SILConstant, Function*> vf : *this) {
    OS << "sil @";
    vf.first.print(OS);
    OS << " : $" << vf.second->getLoweredType() << " {\n";
    vf.second->print(OS);
    OS << "}\n\n";
  }
  
  if (toplevel) {
    OS << "toplevel {\n";
    toplevel->print(OS);
    OS << "}\n\n";
  }
}

//===----------------------------------------------------------------------===//
// Printing for PrettyStackTrace
//===----------------------------------------------------------------------===//

void PrettyStackTraceSILConstant::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  ASTContext *Ctx;
  SourceLoc sloc;
  if (ValueDecl *decl = C.loc.dyn_cast<ValueDecl*>()) {
    if (decl->getName().get())
      out << '\'' << decl->getName() << '\'';
    else
      out << "'anonname=" << (const void*)decl << '\'';
    Ctx = &decl->getASTContext();
    sloc = decl->getStartLoc();
  }
  else if (CapturingExpr *expr = C.loc.dyn_cast<CapturingExpr*>()) {
    out << "anonymous function";
    Ctx = &expr->getASTContext();
    sloc = expr->getStartLoc();
  } else
    llvm_unreachable("impossible sil constant");

  out << " at ";
  printSourceLoc(out, sloc, *Ctx);
}
