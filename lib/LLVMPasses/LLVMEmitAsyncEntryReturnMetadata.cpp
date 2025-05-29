//===--- LLVMEmitAsyncEntryReturnMetadata.cpp - Async function metadata ---===//
//

#include "swift/LLVMPasses/Passes.h"
#include "llvm/Pass.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

using namespace llvm;
using namespace swift;

#define DEBUG_TYPE "swift-async-return"

PreservedAnalyses AsyncEntryReturnMetadataPass::run(Module &M,
                                                    ModuleAnalysisManager &AM) {
  bool changed = false;

  SmallVector<llvm::Function *, 16> asyncEntries;
  SmallVector<llvm::Function *, 16> asyncReturns;
  for (auto &F : M) {
    if (F.isDeclaration())
      continue;

    if (F.hasFnAttribute("async_entry"))
      asyncEntries.push_back(&F);
    if (F.hasFnAttribute("async_ret"))
      asyncReturns.push_back(&F);
  }

  auto &ctxt = M.getContext();
  auto int32Ty = llvm::Type::getInt32Ty(ctxt);
  auto sizeTy = M.getDataLayout().getIntPtrType(ctxt, /*addrspace*/ 0);

  auto addSection = [&] (const char * sectionName, const char *globalName,
                         SmallVectorImpl<llvm::Function *> & entries) {
    if (entries.empty())
      return;

    auto intArrayTy = llvm::ArrayType::get(int32Ty, entries.size());
    auto global =
      new llvm::GlobalVariable(M, intArrayTy, true,
                               llvm::GlobalValue::InternalLinkage,
                               nullptr, /*init*/ globalName,
                               nullptr, /*insertBefore*/
                               llvm::GlobalValue::NotThreadLocal,
                               0/*address space*/);
    global->setAlignment(Align(4));
    global->setSection(sectionName);
    size_t index = 0;
    SmallVector<llvm::Constant*, 16> offsets;
    for (auto *fn : entries) {
      llvm::Constant *indices[] = { llvm::ConstantInt::get(int32Ty, 0),
        llvm::ConstantInt::get(int32Ty, index)};
      ++index;

      llvm::Constant *base = llvm::ConstantExpr::getInBoundsGetElementPtr(
       intArrayTy, global, indices);
      base = llvm::ConstantExpr::getPtrToInt(base, sizeTy);
      auto *target = llvm::ConstantExpr::getPtrToInt(fn, sizeTy);
      llvm::Constant *offset = llvm::ConstantExpr::getSub(target, base);

      if (sizeTy != int32Ty) {
        offset = llvm::ConstantExpr::getTrunc(offset, int32Ty);
      }
      offsets.push_back(offset);
    }
    auto constant = llvm::ConstantArray::get(intArrayTy, offsets);
    global->setInitializer(constant);
    appendToUsed(M, global);

    llvm::GlobalVariable::SanitizerMetadata Meta;
    Meta.IsDynInit = false;
    Meta.NoAddress = true;
    global->setSanitizerMetadata(Meta);

    changed = true;
  };

  addSection("__TEXT,__swift_as_entry, coalesced, no_dead_strip",
             "__swift_async_entry_functlets",
             asyncEntries);
  addSection("__TEXT,__swift_as_ret, coalesced, no_dead_strip",
             "__swift_async_ret_functlets",
             asyncReturns);

  if (!changed)
    return PreservedAnalyses::all();

  return PreservedAnalyses::none();
}
