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

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
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
RValue IRGenFunction::getRValueForGlobalFunction(FuncDecl *Fn) {
  // FIXME: descriptor?
  llvm::Function *Function = IGM.getAddrOfGlobalFunction(Fn);
  llvm::Value *Data = llvm::UndefValue::get(IGM.Int8PtrTy);
  return RValue::forScalars(Function, Data);
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
  if (Arg->Ty->is<TupleType>()) {
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

/// Emit a function call.
RValue IRGenFunction::emitApplyExpr(ApplyExpr *E, const TypeInfo &ResultInfo) {
  const FuncTypeInfo &FnInfo =
    static_cast<const FuncTypeInfo &>(IGM.getFragileTypeInfo(E->Fn->Ty));

  RValue FnRValue = emitRValue(E->Fn, FnInfo);
  assert(FnRValue.isScalar() && FnRValue.getScalars().size() == 2);

  ArgList Args;

  // The first argument is the implicit aggregate return slot, if required.
  RValueSchema ResultSchema = ResultInfo.getSchema();
  if (ResultSchema.isAggregate()) {
    llvm::AllocaInst *ResultSlot =
      createFullExprAlloca(ResultSchema.getAggregateType(),
                           ResultSchema.getAggregateAlignment(),
                           "call.aggresult");
    Args.Values.push_back(ResultSlot);
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

  llvm::Value *CastFn = Builder.CreateBitCast(Fn, FnType, "fn.cast");

  // TODO: exceptions, calling conventions
  llvm::CallInst *Call =
    Builder.CreateCall(CastFn, Args.Values, Fn->getName() + ".call");
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

  FunctionType *FnTy = CurFuncExpr->Ty->getAs<FunctionType>();
  assert(FnTy && "emitting a declaration that's not a function?");

  llvm::Function::arg_iterator CurParm = CurFn->arg_begin();

  // Set up the result slot.
  const TypeInfo &ResultInfo = IGM.getFragileTypeInfo(FnTy->Result);
  RValueSchema ResultSchema = ResultInfo.getSchema();
  llvm::Value *ResultAddr;
  if (ResultSchema.isAggregate()) {
    ResultAddr = CurParm++;
  } else if (ResultSchema.isScalar(0)) {
    ResultAddr = nullptr;
  } else {
    ResultAddr = createScopeAlloca(ResultInfo.getStorageType(),
                                   ResultInfo.StorageAlignment,
                                   "return_value");
  }
  if (ResultAddr)
    ReturnSlot = LValue::forAddress(ResultAddr, ResultInfo.StorageAlignment);

  // Set up the parameters.  This is syntactically required to be a
  // tuple, I think.
  TupleType *ParmTupleTy = FnTy->Input->getAs<TupleType>();
  assert(ParmTupleTy && "parameter type is not a tuple?");

  for (const TupleTypeElt &Field : ParmTupleTy->Fields) {
    const TypeInfo &ParmInfo = IGM.getFragileTypeInfo(Field.Ty);
    RValueSchema ParmSchema = ParmInfo.getSchema();

    // Make an address for the parameter.
    llvm::Value *ParmAddr;
    if (ParmSchema.isAggregate()) {
      ParmAddr = CurParm++;
    } else {
      ParmAddr = createScopeAlloca(ParmInfo.getStorageType(),
                                   ParmInfo.StorageAlignment,
                                   Field.Name.str());
    }

    // Turn that into an l-value.
    LValue ParmLV = LValue::forAddress(ParmAddr, ParmInfo.StorageAlignment);

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

    // Store ParmLV in the locals map somehow?
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

  // Otherwise, branch to it if the current IP is reachable.
  } else if (Builder.GetInsertPoint()) {
    Builder.CreateBr(ReturnBB);
    Builder.SetInsertPoint(ReturnBB);
  }

  FunctionType *FnTy = CurFuncExpr->Ty->getAs<FunctionType>();
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
