//===--- GenFunc.cpp - Swift IR Generation for Function Types -------------===//
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
//  This file implements IR generation for function types in Swift.  This
//  includes creating the IR type as well as capturing variables and
//  performing calls.
//
//  Swift function types are always expanded as a struct containing
//  two opaque pointers.  The first pointer is to a function (should
//  this be a descriptor?) to which the second pointer is passed,
//  along with the formal arguments.  The function pointer is opaque
//  because the alternative would require infinite types to faithfully
//  represent, since aggregates containing function types can be
//  passed and returned by value, not necessary as first-class
//  aggregates.
//
//  There are several considerations for whether to pass the data
//  pointer as the first argument or the last:
//    - On CCs that pass anything in registers, dropping the last
//      argument is significantly more efficient than dropping the
//      first, and it's not that unlikely that the data might
//      be ignored.
//    - A specific instance of that:  we can use the address of a
//      global "data-free" function directly when taking an
//      address-of-function.
//    - Replacing a pointer argument with a different pointer is
//      quite efficient with pretty much any CC.
//    - Later arguments can be less efficient to access if they
//      actually get passed on the stack, but there's some leeway
//      with a decent CC.
//    - Passing the data pointer last inteferes with native variadic
//      arguments, but we probably don't ever want to use native
//      variadic arguments.
//  This works out to a pretty convincing argument for passing the
//  data pointer as the last argument.
//
//  On the other hand, it is not compatible with blocks.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "RValue.h"

using namespace swift;
using namespace irgen;

namespace {
  class FuncTypeInfo : public TypeInfo {
    FunctionType *FnTy;
    mutable llvm::FunctionType *FunctionTypeWithData;
    mutable llvm::FunctionType *FunctionTypeWithoutData;
  public:
    FuncTypeInfo(FunctionType *Ty, llvm::StructType *T, Size S, Alignment A)
      : TypeInfo(T, S, A), FnTy(Ty),
        FunctionTypeWithData(0), FunctionTypeWithoutData(0) {}

    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }

    llvm::FunctionType *getFunctionType(IRGenModule &IGM, bool NeedsData) const;

    RValueSchema getSchema() const {
      llvm::StructType *Ty = getStorageType();
      assert(Ty->getNumElements() == 2);
      return RValueSchema::forScalars(Ty->getElementType(0),
                                      Ty->getElementType(1));
    }

    RValue load(IRGenFunction &IGF, const LValue &LV) const {
      llvm::Value *Addr = LV.getAddress();

      // Load the function.
      llvm::Value *FnAddr =
        IGF.Builder.CreateStructGEP(Addr, 0, Addr->getName() + ".fn");
      llvm::LoadInst *Fn =
        IGF.Builder.CreateLoad(FnAddr, LV.getAlignment(),
                               FnAddr->getName() + ".load");

      // Load the data.  This load is offset by sizeof(void*) from the
      // base and so may have a lesser alignment.
      // FIXME: retains?
      llvm::Value *DataAddr =
        IGF.Builder.CreateStructGEP(Addr, 1, Addr->getName() + ".data");
      llvm::Value *Data =
        IGF.Builder.CreateLoad(DataAddr,
                               std::min(LV.getAlignment(), StorageAlignment),
                               DataAddr->getName() + ".load");

      return RValue::forScalars(Fn, Data);
    }

    void store(IRGenFunction &IGF, const RValue &RV, const LValue &LV) const {
      assert(RV.isScalar() && RV.getScalars().size() == 2);
      llvm::Value *Addr = LV.getAddress();

      // Store the function pointer.
      llvm::Value *FnAddr =
        IGF.Builder.CreateStructGEP(Addr, 0, Addr->getName() + ".fn");
      IGF.Builder.CreateStore(RV.getScalars()[0], FnAddr, LV.getAlignment());

      // Store the data.
      // FIXME: retains?
      llvm::Value *DataAddr =
        IGF.Builder.CreateStructGEP(Addr, 1, Addr->getName() + ".data");
      IGF.Builder.CreateStore(RV.getScalars()[1], DataAddr,
                              std::min(LV.getAlignment(), StorageAlignment));
    }
  };
}

const TypeInfo *
TypeConverter::convertFunctionType(IRGenModule &IGM, FunctionType *T) {
  Size StructSize = Size(IGM.TargetData.getPointerSize()) * 2;
  Alignment StructAlign = Alignment(IGM.TargetData.getPointerABIAlignment());
  llvm::Type *Elts[] = { IGM.Int8PtrTy, IGM.Int8PtrTy };
  llvm::StructType *StructType
    = llvm::StructType::get(IGM.getLLVMContext(), Elts, /*packed*/ false);
  return new FuncTypeInfo(T, StructType, StructSize, StructAlign);
}

/// Accumulate an argument of the given type.
static void addArgType(IRGenModule &IGM, Type Ty,
                       SmallVectorImpl<llvm::Type*> &ArgTypes) {
  RValueSchema Schema = IGM.getFragileTypeInfo(Ty).getSchema();
  if (Schema.isScalar()) {
    for (llvm::Type *Arg : Schema.getScalarTypes())
      ArgTypes.push_back(Arg);
  } else {
    ArgTypes.push_back(Schema.getAggregateType()->getPointerTo());
  }
}

llvm::FunctionType *
FuncTypeInfo::getFunctionType(IRGenModule &IGM, bool NeedsData) const {
  if (NeedsData && FunctionTypeWithData)
    return FunctionTypeWithData;
  if (!NeedsData && FunctionTypeWithoutData)
    return FunctionTypeWithoutData;

  SmallVector<llvm::Type*, 16> ArgTypes;
  llvm::Type *ResultType;

  // Compute the result-type information.
  RValueSchema ResultSchema = IGM.getFragileTypeInfo(FnTy->Result).getSchema();

  // If this is an aggregate return, return indirectly.
  if (ResultSchema.isAggregate()) {
    ResultType = llvm::Type::getVoidTy(IGM.getLLVMContext());
    ArgTypes.push_back(ResultSchema.getAggregateType()->getPointerTo());

  // If there are no results, return void.
  } else if (ResultSchema.getScalarTypes().empty()) {
    ResultType = llvm::Type::getVoidTy(IGM.getLLVMContext());

  // If there is exactly one result, return it.
  } else if (ResultSchema.getScalarTypes().size() == 1) {
    ResultType = ResultSchema.getScalarTypes()[0];

  // Otherwise, return a first-class aggregate.
  } else {
    ResultType = llvm::StructType::get(IGM.getLLVMContext(),
                                       ResultSchema.getScalarTypes());
  }

  // Drill into the first level of tuple, if present.
  if (TupleType *Tuple = FnTy->Input->getAs<TupleType>()) {
    for (const TupleTypeElt &Field : Tuple->Fields) {
      addArgType(IGM, Field.Ty, ArgTypes);
    }

  // Otherwise, just add the argument type.
  } else {
    addArgType(IGM, FnTy->Input, ArgTypes);
  }

  // If we need a data argument, add it in last.
  // See the discussion in the header comment, above.
  if (NeedsData) {
    ArgTypes.push_back(IGM.Int8PtrTy);
  }

  // Create the appropriate LLVM type.
  llvm::FunctionType *IRType =
    llvm::FunctionType::get(ResultType, ArgTypes, /*variadic*/ false);

  // Cache the type.
  if (NeedsData)
    FunctionTypeWithData = IRType;
  else
    FunctionTypeWithoutData = IRType;

  return IRType;
}

/// Form an r-value which refers to the given global function.
RValue IRGenFunction::emitRValueForFunction(FuncDecl *Fn) {
  if (!Fn->Context->isLocalContext()) {
    llvm::Function *Function = IGM.getAddrOfGlobalFunction(Fn);
    llvm::Value *Data = llvm::UndefValue::get(IGM.Int8PtrTy);
    return RValue::forScalars(Function, Data);
  }

  unimplemented(Fn->getLocStart(),
                "local function emission is not yet implemented");
  llvm::Value *Undef = llvm::UndefValue::get(IGM.Int8PtrTy);
  return RValue::forScalars(Undef, Undef);
}

llvm::FunctionType *IRGenModule::getFunctionType(FuncDecl *Fn) {
  const FuncTypeInfo &TypeInfo =
    static_cast<const FuncTypeInfo &>(getFragileTypeInfo(Fn->Ty));
  return TypeInfo.getFunctionType(*this, /*data*/ false);
}

namespace {
  struct ArgList {
    llvm::SmallVector<llvm::Value *, 16> Values;
    llvm::SmallVector<llvm::AttributeWithIndex, 4> Attrs;
  };
}

static void emitArg(IRGenFunction &IGF, Expr *Arg, ArgList &Args) {
  RValue RV = IGF.emitRValue(Arg);
  if (RV.isScalar()) {
    Args.Values.append(RV.getScalars().begin(), RV.getScalars().end());
  } else {
    Args.Values.push_back(RV.getAggregateAddress());
  }
}

/// Emit the given expression as an expanded tuple, if possible.
static void emitExpanded(IRGenFunction &IGF, Expr *Arg, ArgList &Args) {
  // If it's a tuple literal, we want to expand it directly.
  if (TupleExpr *ArgTuple = dyn_cast<TupleExpr>(Arg)) {
    // But ignore grouping parens.
    if (ArgTuple->isGroupingParen()) {
      return emitExpanded(IGF, ArgTuple->SubExprs[0], Args);
    }

    // TODO: we might need to change the order of the arguments here.
    for (unsigned I = 0, E = ArgTuple->NumSubExprs; I != E; ++I)
      emitArg(IGF, ArgTuple->SubExprs[I], Args);
    return;
  }

  // It's not a tuple literal.  If it has tuple type, evaluate and expand.
  if (Arg->getType()->is<TupleType>()) {
    // TODO: if it's a load from a tuple l-value, we should just emit
    // the l-value and extract scalars from that instead of potentially
    // copying into a temporary and then extracting from it.
    RValue RV = IGF.emitRValue(Arg);
    if (RV.isScalar()) {
      Args.Values.append(RV.getScalars().begin(), RV.getScalars().end());
    } else {
      IGF.unimplemented(Arg->getLocStart(),
                        "expansion of aggregate-valued tuple as argument");
    }
    return;
  }

  // Otherwise it's a single, non-tuple argument, which we should
  // evaluate straight.
  emitArg(IGF, Arg, Args);
}

/// emitBuiltinCall - Emit a call to a builtin function.
static RValue emitBuiltinCall(IRGenFunction &IGF, FuncDecl *Fn, Expr *Arg,
                              const TypeInfo &ResultInfo) {
  assert(ResultInfo.getSchema().isScalar() && "builtin type with agg return");

  // Emit the arguments.  Maybe we'll get builtins that are more
  // complex than this.
  ArgList Args;
  emitExpanded(IGF, Arg, Args);

  BuiltinTypeKind TypeArg;
  switch (isBuiltinValue(Fn->Name.str(), TypeArg)) {
  case BuiltinValueKind::None: llvm_unreachable("not a builtin after all!");

/// A macro which expands to the emission of a simple unary operation
/// or predicate.
#define UNARY_OPERATION(Op)                                                 \
    assert(Args.Values.size() == 1 && "wrong operands to unary operation"); \
    return RValue::forScalars(IGF.Builder.Create##Op(Args.Values[0]));      \

/// A macro which expands to the emission of a simple binary operation
/// or predicate.
#define BINARY_OPERATION(Op)                                                \
    assert(Args.Values.size() == 2 && "wrong operands to binary operation");\
    return RValue::forScalars(IGF.Builder.Create##Op(Args.Values[0],        \
                                                     Args.Values[1]));      \

/// A macro which expands to the emission of a simple binary operation
/// or predicate defined over both floating-point and integer types.
#define BINARY_ARITHMETIC_OPERATION(IntOp, FPOp)                            \
    assert(Args.Values.size() == 2 && "wrong operands to binary operation");\
    if (Args.Values[0]->getType()->isFloatingPointTy()) {                   \
      return RValue::forScalars(IGF.Builder.Create##FPOp(Args.Values[0],    \
                                                         Args.Values[1]));  \
    } else {                                                                \
      return RValue::forScalars(IGF.Builder.Create##IntOp(Args.Values[0],   \
                                                          Args.Values[1])); \
    }

  case BuiltinValueKind::Neg:       UNARY_OPERATION(Neg)
  case BuiltinValueKind::Not:       UNARY_OPERATION(Not)
  case BuiltinValueKind::Add:       BINARY_ARITHMETIC_OPERATION(Add, FAdd)
  case BuiltinValueKind::And:       BINARY_OPERATION(And)
  case BuiltinValueKind::FDiv:      BINARY_OPERATION(FDiv)
  case BuiltinValueKind::Mul:       BINARY_ARITHMETIC_OPERATION(Mul, FMul)
  case BuiltinValueKind::Or:        BINARY_OPERATION(Or)
  case BuiltinValueKind::SDiv:      BINARY_OPERATION(SDiv)
  case BuiltinValueKind::SDivExact: BINARY_OPERATION(ExactSDiv)
  case BuiltinValueKind::SRem:      BINARY_OPERATION(SRem)
  case BuiltinValueKind::Sub:       BINARY_ARITHMETIC_OPERATION(Sub, FSub)
  case BuiltinValueKind::UDiv:      BINARY_OPERATION(UDiv)
  case BuiltinValueKind::UDivExact: BINARY_OPERATION(ExactUDiv)
  case BuiltinValueKind::URem:      BINARY_OPERATION(URem)
  case BuiltinValueKind::Xor:       BINARY_OPERATION(Xor)
  case BuiltinValueKind::CmpEQ:     BINARY_OPERATION(ICmpEQ)
  case BuiltinValueKind::CmpNE:     BINARY_OPERATION(ICmpNE)
  case BuiltinValueKind::CmpSLE:    BINARY_OPERATION(ICmpSLE)
  case BuiltinValueKind::CmpSLT:    BINARY_OPERATION(ICmpSLT)
  case BuiltinValueKind::CmpSGE:    BINARY_OPERATION(ICmpSGE)
  case BuiltinValueKind::CmpSGT:    BINARY_OPERATION(ICmpSGT)
  case BuiltinValueKind::CmpULE:    BINARY_OPERATION(ICmpULE)
  case BuiltinValueKind::CmpULT:    BINARY_OPERATION(ICmpULT)
  case BuiltinValueKind::CmpUGE:    BINARY_OPERATION(ICmpUGE)
  case BuiltinValueKind::CmpUGT:    BINARY_OPERATION(ICmpUGT)
  case BuiltinValueKind::FCmpOEQ:   BINARY_OPERATION(FCmpOEQ)
  case BuiltinValueKind::FCmpOGT:   BINARY_OPERATION(FCmpOGT)
  case BuiltinValueKind::FCmpOGE:   BINARY_OPERATION(FCmpOGE)
  case BuiltinValueKind::FCmpOLT:   BINARY_OPERATION(FCmpOLT)
  case BuiltinValueKind::FCmpOLE:   BINARY_OPERATION(FCmpOLE)
  case BuiltinValueKind::FCmpONE:   BINARY_OPERATION(FCmpONE)
  case BuiltinValueKind::FCmpORD:   BINARY_OPERATION(FCmpORD)
  case BuiltinValueKind::FCmpUEQ:   BINARY_OPERATION(FCmpUEQ)
  case BuiltinValueKind::FCmpUGT:   BINARY_OPERATION(FCmpUGT)
  case BuiltinValueKind::FCmpUGE:   BINARY_OPERATION(FCmpUGE)
  case BuiltinValueKind::FCmpULT:   BINARY_OPERATION(FCmpULT)
  case BuiltinValueKind::FCmpULE:   BINARY_OPERATION(FCmpULE)
  case BuiltinValueKind::FCmpUNE:   BINARY_OPERATION(FCmpUNE)
  case BuiltinValueKind::FCmpUNO:   BINARY_OPERATION(FCmpUNO)
  }
  llvm_unreachable("bad builtin kind!");
}

/// Emit a function call.
RValue IRGenFunction::emitApplyExpr(ApplyExpr *E, const TypeInfo &ResultInfo) {
  // Check for a call to a builtin.
  if (ValueDecl *Fn = E->getCalledValue())
    if (Fn->Context == IGM.Context.BuiltinModule)
      return emitBuiltinCall(*this, cast<FuncDecl>(Fn), E->Arg, ResultInfo);

  const FuncTypeInfo &FnInfo =
    static_cast<const FuncTypeInfo &>(IGM.getFragileTypeInfo(E->Fn->getType()));

  RValue FnRValue = emitRValue(E->Fn, FnInfo);
  assert(FnRValue.isScalar() && FnRValue.getScalars().size() == 2);

  ArgList Args;

  // The first argument is the implicit aggregate return slot, if required.
  RValueSchema ResultSchema = ResultInfo.getSchema();
  if (ResultSchema.isAggregate()) {
    LValue ResultSlot =
      createFullExprAlloca(ResultSchema.getAggregateType(),
                           ResultSchema.getAggregateAlignment(),
                           "call.aggresult");
    Args.Values.push_back(ResultSlot.getAddress());
    Args.Attrs.push_back(llvm::AttributeWithIndex::get(1,
                                llvm::Attribute::StructRet |
                                llvm::Attribute::NoAlias));
  }

  // Emit the arguments, drilling into the first level of tuple, if
  // present.
  emitExpanded(*this, E->Arg, Args);

  llvm::Value *Fn = FnRValue.getScalars()[0];
  llvm::Value *Data = FnRValue.getScalars()[1];

  // Don't bother passing a data argument if the r-value says it's
  // undefined.
  bool NeedsData = !isa<llvm::UndefValue>(Data);
  if (NeedsData) {
    Args.Values.push_back(Data);
  }
  llvm::FunctionType *FnType = FnInfo.getFunctionType(IGM, NeedsData);

  llvm::Value *CastFn = Builder.CreateBitCast(Fn, FnType->getPointerTo(),
                                              "fn.cast");

  // TODO: exceptions, calling conventions
  llvm::CallInst *Call =
    Builder.CreateCall(CastFn, Args.Values);
  Call->setAttributes(llvm::AttrListPtr::get(Args.Attrs.data(),
                                             Args.Attrs.size()));

  // Build an RValue result.
  if (ResultSchema.isAggregate()) {
    return RValue::forAggregate(Args.Values[0]);
  } else if (ResultSchema.getScalarTypes().size() == 1) {
    return RValue::forScalars(Call);
  } else {
    // This does the right thing for void returns as well.
    llvm::SmallVector<llvm::Value*, RValue::MaxScalars> Result;
    for (unsigned I = 0, E = ResultSchema.getScalarTypes().size(); I != E; ++I){
      llvm::Value *Scalar = Builder.CreateExtractValue(Call, I);
      Result.push_back(Scalar);
    }
    return RValue::forScalars(Result);
  }
}

/// Emit the prologue for the function.
void IRGenFunction::emitPrologue() {
  // Set up the IRBuilder.
  llvm::BasicBlock *EntryBB = createBasicBlock("entry");
  assert(CurFn->getBasicBlockList().empty() && "prologue already emitted?");
  CurFn->getBasicBlockList().push_back(EntryBB);
  Builder.SetInsertPoint(EntryBB);

  // Set up the alloca insertion point.
  AllocaIP = Builder.CreateAlloca(IGM.Int1Ty, /*array size*/ nullptr,
                                  "alloca point");

  // Set up the return block and insert it.  This creates a second
  // insertion point that most blocks should be inserted before.
  ReturnBB = createBasicBlock("return");
  CurFn->getBasicBlockList().push_back(ReturnBB);

  FunctionType *FnTy = CurFuncExpr->getType()->getAs<FunctionType>();
  assert(FnTy && "emitting a declaration that's not a function?");

  llvm::Function::arg_iterator CurParm = CurFn->arg_begin();

  // Set up the result slot.
  const TypeInfo &ResultInfo = IGM.getFragileTypeInfo(FnTy->Result);
  RValueSchema ResultSchema = ResultInfo.getSchema();
  if (ResultSchema.isAggregate()) {
    ReturnSlot = LValue::forAddress(CurParm++, ResultInfo.StorageAlignment);
  } else if (ResultSchema.isScalar(0)) {
    assert(!ReturnSlot.isValid());
  } else {
    ReturnSlot = createScopeAlloca(ResultInfo.getStorageType(),
                                   ResultInfo.StorageAlignment,
                                   "return_value");
  }

  // Set up the parameters.
  for (ArgDecl *Parm : CurFuncExpr->NamedArgs) {
    const TypeInfo &ParmInfo = IGM.getFragileTypeInfo(Parm->Ty);
    RValueSchema ParmSchema = ParmInfo.getSchema();

    // Make an l-value for the parameter.
    LValue ParmLV;
    if (ParmSchema.isAggregate()) {
      ParmLV = LValue::forAddress(CurParm++, ParmInfo.StorageAlignment);
    } else {
      ParmLV = createScopeAlloca(ParmInfo.getStorageType(),
                                 ParmInfo.StorageAlignment,
                                 Parm->Name.str());
    }

    // If the parameter was scalar, form an r-value from the
    // parameters and store that.
    if (ParmSchema.isScalar()) {
      SmallVector<llvm::Value*, RValue::MaxScalars> Scalars;
      for (llvm::Type *ParmType : ParmSchema.getScalarTypes()) {
        llvm::Value *V = CurParm++;
        assert(V->getType() == ParmType);
        (void) ParmType;
        Scalars.push_back(V);
      }

      RValue ParmRV = RValue::forScalars(Scalars);
      ParmInfo.store(*this, ParmRV, ParmLV);
    }

    assert(!Locals.count(Parm));
    Locals.insert(std::make_pair(Parm, ParmLV));
  }

  // TODO: data pointer

  assert(CurParm == CurFn->arg_end() && "didn't exhaust all parameters?");
}

/// Emit the epilogue for the function.
void IRGenFunction::emitEpilogue() {
  // Destroy the alloca insertion point.
  AllocaIP->eraseFromParent();

  // If there are no edges to the return block, we never want to emit it.
  if (ReturnBB->use_empty()) {
    ReturnBB->eraseFromParent();

    // Normally this means that we'll just insert the epilogue in the
    // current block, but if the current IP is unreachable then so is
    // the entire epilogue.
    if (!Builder.hasValidIP()) return;

  // Otherwise, branch to it if the current IP is reachable.
  } else if (Builder.hasValidIP()) {
    Builder.CreateBr(ReturnBB);
    Builder.SetInsertPoint(ReturnBB);

  // Otherwise, if there is exactly one use of the return block, merge
  // it into its predecessor.
  } else if (ReturnBB->hasOneUse()) {
    // return statements are never emitted as conditional branches.
    llvm::BranchInst *Br = cast<llvm::BranchInst>(*ReturnBB->use_begin());
    assert(Br->isUnconditional());
    Builder.SetInsertPoint(Br->getParent());
    Br->eraseFromParent();
    ReturnBB->eraseFromParent();

  // Otherwise, just move the IP to the return block.
  } else {
    Builder.SetInsertPoint(ReturnBB);
  }

  FunctionType *FnTy = CurFuncExpr->getType()->getAs<FunctionType>();
  assert(FnTy && "emitting a declaration that's not a function?");

  const TypeInfo &ResultInfo = IGM.getFragileTypeInfo(FnTy->Result);
  RValueSchema ResultSchema = ResultInfo.getSchema();
  if (ResultSchema.isAggregate()) {
    assert(isa<llvm::Argument>(ReturnSlot.getAddress()));
    Builder.CreateRetVoid();
  } else if (ResultSchema.isScalar(0)) {
    assert(!ReturnSlot.isValid());
    Builder.CreateRetVoid();
  } else {
    RValue RV = ResultInfo.load(*this, ReturnSlot);
    if (RV.isScalar(1)) {
      Builder.CreateRet(RV.getScalars()[0]);
    } else {
      llvm::Value *Result = llvm::UndefValue::get(CurFn->getReturnType());
      for (unsigned I = 0, E = RV.getScalars().size(); I != E; ++I)
        Result = Builder.CreateInsertValue(Result, RV.getScalars()[I], I);
      Builder.CreateRet(Result);
    }
  }
}

/// Emit the definition for the given global function.
void IRGenModule::emitGlobalFunction(FuncDecl *FD) {
  // Nothing to do if the function has no body.
  if (!FD->Init) return;

  llvm::Function *Addr = getAddrOfGlobalFunction(FD);

  IRGenFunction(*this, cast<FuncExpr>(FD->Init), Addr).emitFunction();
}

void IRGenFunction::emitFunction() {
  emitPrologue();
  emitBraceStmt(CurFuncExpr->Body);
  emitEpilogue();
}
