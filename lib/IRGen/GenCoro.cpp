//===---- GenCoro.cpp - Code generation related to coroutines -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/ABI/Coro.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/Basic/Assertions.h"
#include "swift/IRGen/Linking.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

#include "ConstantBuilder.h"
#include "Explosion.h"
#include "GenCoro.h"
#include "GenFunc.h"
#include "GenPointerAuth.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "clang/CodeGen/CodeGenABITypes.h"

using namespace swift;
using namespace irgen;

// Add a flag which when set to false forces execution of the open-coded version
// in order to execution test it.
static llvm::cl::opt<bool> EnableRuntimeTaskDeallocThrough(
    "enable-runtime-task-dealloc-through", llvm::cl::init(true),
    llvm::cl::Hidden,
    llvm::cl::desc(
        "Use the swift_task_dealloc_through symbol if it's available"));

namespace {

class GetDeallocThroughFn {
  IRGenModule &IGM;
  class Builder {
    IRGenModule &IGM;
    IRGenFunction &IGF;
    bool isPointer8Bytes;

  public:
    Builder(IRGenFunction &IGF)
        : IGM(IGF.IGM), IGF(IGF),
          isPointer8Bytes(IGM.getPointerSize() == Size(8)) {
      assert(!IGM.getAvailabilityRange().isContainedIn(
                 IGM.Context.getCoroutineAccessorsAvailability()) ||
             !EnableRuntimeTaskDeallocThrough);
    }

    /// Emit the function.
    void build() {
      auto parameters = IGF.collectParameters();
      auto *ptr = parameters.claimNext();

      if (EnableRuntimeTaskDeallocThrough) {
        emitForwardToWeakRuntimeFunction(ptr);
      }

      auto allocationPtrAddr = emitOffsetIntoTask();

      auto *loop = IGF.createBasicBlock("loop");
      IGF.Builder.CreateBr(loop);

      auto *exit = IGF.createBasicBlock("exit");
      emitLoopBlock(loop, allocationPtrAddr, ptr, exit);
      emitExitBlock(exit);
    }

  private:
    /// Check whether the swift_task_dealloc_through is available and call it if
    /// so.
    void emitForwardToWeakRuntimeFunction(llvm::Value *ptr) {
      auto runtimeFnValue = cast<llvm::Function>(IGM.getTaskDeallocThroughFn());
      runtimeFnValue->setLinkage(llvm::GlobalValue::ExternalWeakLinkage);
      auto *symbolExists = IGF.Builder.CreateICmpNE(
          runtimeFnValue, llvm::ConstantPointerNull::get(IGF.IGM.PtrTy),
          "runtime_has_dealloc_through");

      auto *noSymbolBlock = IGF.createBasicBlock("no_runtime_symbol");
      auto *hasSymbolBlock = IGF.createBasicBlock("runtime_symbol");

      IGF.Builder.CreateCondBr(symbolExists, hasSymbolBlock, noSymbolBlock);

      IGF.Builder.emitBlock(hasSymbolBlock);
      IGF.Builder.CreateCall(
          FunctionPointer::forDirect(
              FunctionPointer::Kind::Function, runtimeFnValue, nullptr,
              IGM.getTaskDeallocThroughFunctionPointer().getSignature()),
          {ptr});
      IGF.Builder.CreateRetVoid();

      IGF.Builder.emitBlock(noSymbolBlock);
      // The rest of the function will be emitted starting with this block.
    }

    /// Calculate the address (an offset into the current task) of the pointer
    /// to the latest allocation.
    ///
    /// This calculation depends on the deployment target.  If it's new enough
    /// (aligned with at least Swift 5.7), the "new" layouts can be used
    /// unconditionally.  Otherwise, the old layout must be used when the OS is
    /// aligned with a release before Swift 5.7.
    Address emitOffsetIntoTask() {
      auto *task = IGF.getAsyncTask();
      if (!IGM.Context.getSwift57Availability().hasMinimumVersion()) {
        return emitNewLayoutOffsetIntoTask(task);
      }
      auto deploymentRange =
          AvailabilityRange::forDeploymentTarget(IGM.Context);
      if (deploymentRange.isContainedIn(IGM.Context.getSwift57Availability())) {
        return emitNewLayoutOffsetIntoTask(task);
      }

      auto *oldBlock = IGF.createBasicBlock("old_layout");
      auto *newBlock = IGF.createBasicBlock("new_layout");
      auto *mergeBlock = IGF.createBasicBlock("merge_layout");

      auto *isAtLeast57 = emitSwift57VersionCheck();
      IGF.Builder.CreateCondBr(isAtLeast57, newBlock, oldBlock);

      IGF.Builder.emitBlock(oldBlock);
      auto oldOffset = emitOldLayoutOffsetIntoTask(task);
      IGF.Builder.CreateBr(mergeBlock);

      IGF.Builder.emitBlock(newBlock);
      auto newOffset = emitNewLayoutOffsetIntoTask(task);
      IGF.Builder.CreateBr(mergeBlock);
      auto *finalNewBlock = IGF.Builder.GetInsertBlock();

      IGF.Builder.emitBlock(mergeBlock);
      auto offsetPhi = IGF.Builder.CreatePHI(newOffset.getType(), 2);
      offsetPhi->addIncoming(oldOffset.getAddress(), oldBlock);
      offsetPhi->addIncoming(newOffset.getAddress(), finalNewBlock);
      auto addressPhi =
          Address(offsetPhi, oldOffset.getType(), oldOffset.getAlignment());
      return addressPhi;
    }

    /// Given that the OS is aligned with a release before Swift 5.7, calculate
    /// the address (an offset from \p task) of the pointer to the latest
    /// allocation.
    Address emitOldLayoutOffsetIntoTask(llvm::Value *task) {
      if (!isPointer8Bytes) {
        return computeOffsetIntoTask(
            task, getTaskLayoutInfo(PrivateLayout::Old32Bit));
      }
      return computeOffsetIntoTask(task,
                                   getTaskLayoutInfo(PrivateLayout::Old64Bit));
    }

    /// Given that the OS is aligned with at least Swift 5.7, calculate the
    /// address (an offset from \p task) of the pointer to the latest
    /// allocation.
    Address emitNewLayoutOffsetIntoTask(llvm::Value *task) {
      if (!isPointer8Bytes) {
        return emitNew32BitOffsetIntoTask(task);
      }
      return computeOffsetIntoTask(task,
                                   getTaskLayoutInfo(PrivateLayout::New64Bit));
    }

    /// Given that the target is 32-bit and the OS is Swift 5.7 aligned or
    /// greater, calculate the address (an offset from \p task) of the pointer
    /// to the latest allocation.
    ///
    /// This is complicated by the fact that the layout changes depending on
    /// whether SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION is defined.  To
    /// determine this, the existence of
    /// _swift_concurrency_debug_supportsPriorityEscalation is checked.
    Address emitNew32BitOffsetIntoTask(llvm::Value *task) {
      auto *noEscalationBlock = IGF.createBasicBlock("escalation_no");
      auto *yesEscalationBlock = IGF.createBasicBlock("escalation_yes");
      auto *hasEscalationSymbolBlock =
          IGF.createBasicBlock("escalation_has_symbol");
      auto *mergeEscalationBlock = IGF.createBasicBlock("escalation_merge");

      auto *escalationSymbol = IGF.IGM.Module.getOrInsertGlobal(
          "_swift_concurrency_debug_supportsPriorityEscalation",
          IGF.IGM.Int8Ty);
      ApplyIRLinkage(IRLinkage::ExternalWeakImport)
          .to(cast<llvm::GlobalVariable>(escalationSymbol));
      auto *symbolExists = IGF.Builder.CreateICmpNE(
          escalationSymbol, llvm::ConstantPointerNull::get(IGF.IGM.PtrTy),
          "escalation_is_defined");

      IGF.Builder.CreateCondBr(symbolExists, hasEscalationSymbolBlock,
                               noEscalationBlock);
      IGF.Builder.emitBlock(hasEscalationSymbolBlock);
      auto escalationAddr =
          Address(escalationSymbol, IGF.IGM.Int8Ty, Alignment(1));
      auto *escalation = IGF.Builder.CreateLoad(escalationAddr, "escalation");
      auto *escalationIsTrue = IGF.Builder.CreateICmpNE(
          escalation, llvm::ConstantInt::get(IGF.IGM.Int8Ty, false),
          "escalation_is_true");
      IGF.Builder.CreateCondBr(escalationIsTrue, yesEscalationBlock,
                               noEscalationBlock);

      IGF.Builder.emitBlock(yesEscalationBlock);
      auto escalationOffset = computeOffsetIntoTask(
          task, getTaskLayoutInfo(PrivateLayout::New32BitWithEscalation));
      IGF.Builder.CreateBr(mergeEscalationBlock);

      IGF.Builder.emitBlock(noEscalationBlock);
      auto noEscalationOffset = computeOffsetIntoTask(
          task, getTaskLayoutInfo(PrivateLayout::New32BitSansEscalation));
      IGF.Builder.CreateBr(mergeEscalationBlock);

      IGF.Builder.emitBlock(mergeEscalationBlock);
      auto offsetPhi = IGF.Builder.CreatePHI(noEscalationOffset.getType(), 2);
      offsetPhi->addIncoming(noEscalationOffset.getAddress(),
                             noEscalationBlock);
      offsetPhi->addIncoming(escalationOffset.getAddress(), yesEscalationBlock);
      auto addressPhi = Address(offsetPhi, noEscalationOffset.getType(),
                                noEscalationOffset.getAlignment());
      return addressPhi;
    }

    /// Calculate a bit suitable for a CondBr indicating that the OS is aligned
    /// with Swift 5.7 or greater.
    ///
    /// This is relevant because the layout of Task changed from pre-5.7 to 5.7.
    llvm::Value *emitSwift57VersionCheck() {
      auto availability = IGM.Context.getSwift57Availability();
      auto deploymentRange =
          AvailabilityRange::forDeploymentTarget(IGM.Context);
      assert(!deploymentRange.isContainedIn(availability));
      (void)deploymentRange;
      assert(availability.hasMinimumVersion());
      // FIXME: [availability] This does not generate the correct query for
      // macCatalyst or zippered targets (rdar://155999964).
      auto version = availability.getRawMinimumVersion();
      auto *major = getInt32Constant(version.getMajor());
      auto *minor = getInt32Constant(version.getMinor());
      auto *patch = getInt32Constant(version.getSubminor());

      auto *isAtLeastValue =
          IGF.emitTargetOSVersionAtLeastCall(major, minor, patch);

      auto *isAtLeast = IGF.Builder.CreateICmpNE(
          isAtLeastValue, llvm::Constant::getNullValue(IGM.Int32Ty));

      return isAtLeast;
    }

    llvm::ConstantInt *getInt32Constant(std::optional<unsigned> value) {
      return llvm::ConstantInt::get(IGM.Int32Ty, value.value_or(0));
    };

    /// Specifies a layout of the Task runtime struct, specifically just enough
    /// of its private fields in order to find the pointer to the latest
    /// allocation.
    ///
    /// There are two "flavors" of layouts:
    /// Swift 5.6 and before:
    /// class Task {
    ///   AsyncContext * __ptrauth_swift_task_resume_context ResumeContext;
    ///
    ///   #if SWIFT_POINTER_IS_8_BYTES
    ///   void *Reserved64;
    ///   #endif
    ///
    ///   // May or may not have been visible in Task.h.
    ///   swift::atomic<ActiveTaskStatus> Status;
    ///   // Never directly visible in Task.h.
    ///   TaskAllocator Allocator;
    /// }
    /// Swift 5.7 and after:
    /// class Task {
    ///   AsyncContext * __ptrauth_swift_task_resume_context ResumeContext;
    ///
    ///   #if SWIFT_POINTER_IS_8_BYTES
    ///   void *Reserved64;
    ///   #endif
    ///
    ///   // Always hidden via OpaquePrivateStorage.
    ///   uintptr_t ExclusivityAccessSet[2] = {0, 0};
    ///   // size here was always ((32-bit && escalating) ? 4 : 2) * word_size.
    ///   alignas(ActiveTaskStatus) char StatusStorage[sizeof(ActiveTaskStatus)]
    ///
    ///   TaskAllocator Allocator;
    /// }
    struct PrivateLayout {
      enum Kind : uint8_t {
        Old32Bit,
        Old64Bit,
        New32BitSansEscalation,
        New32BitWithEscalation,
        New64Bit,
      };
      Kind kind;
      PrivateLayout(Kind kind) : kind(kind) {}
      bool isPointer8Bytes() {
        switch (kind) {
        case Old64Bit:
        case New64Bit:
          return true;
        case Old32Bit:
        case New32BitSansEscalation:
        case New32BitWithEscalation:
          return false;
        }
        llvm_unreachable("covered switch");
      }
      bool isOld() {
        switch (kind) {
        case Old32Bit:
        case Old64Bit:
          return true;
        case New32BitSansEscalation:
        case New32BitWithEscalation:
        case New64Bit:
          return false;
        }
        llvm_unreachable("covered switch");
      }
      unsigned activeTaskStatusSizeInWords() {
        switch (kind) {
        case Old32Bit:
        case Old64Bit:
          return 2;

        // #if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION &&
        // SWIFT_POINTER_IS_4_BYTES #define ACTIVE_TASK_STATUS_SIZE (4 *
        // (sizeof(uintptr_t))) #else #define ACTIVE_TASK_STATUS_SIZE (2 *
        // (sizeof(uintptr_t))) #endif
        case New32BitSansEscalation:
          return 2;
        case New32BitWithEscalation:
          return 4;
        case New64Bit:
          return 2;
        }
        llvm_unreachable("covered switch");
      }
      struct Field {
        enum Kind {
          RefCountedStruct,
          Int8Ptr,
          Int32,
          FunctionPtr,
          SwiftContextPtr,
          Ptr,
          IntPtr,
          AllocationPtr,
        };
        Kind kind;
        Field(Kind kind) : kind(kind) {}
        Size getSizeInBytes(Size wordSize) {
          switch (kind) {
          case RefCountedStruct:
            return wordSize * 2;
          case Int8Ptr:
          case FunctionPtr:
          case SwiftContextPtr:
          case Ptr:
          case IntPtr:
            return wordSize;
          case Int32:
            return Size(4);
          case AllocationPtr:
            return wordSize;
          }
          llvm_unreachable("covered switch");
        }
        llvm::Type *getType(IRGenModule &IGM) {
          switch (kind) {
          case RefCountedStruct:
            return IGM.RefCountedStructTy;
          case Int8Ptr:
            return IGM.Int8PtrTy;
          case Int32:
            return IGM.Int32Ty;
          case FunctionPtr:
            return IGM.FunctionPtrTy;
          case SwiftContextPtr:
            return IGM.SwiftContextPtrTy;
          case Ptr:
            return IGM.PtrTy;
          case IntPtr:
            return IGM.IntPtrTy;
          case AllocationPtr:
            return IGM.PtrTy;
          }
          llvm_unreachable("covered switch");
        }
      };
      template <typename Impl>
      struct FieldVisitor {
        Impl &asImpl() { return *static_cast<Impl *>(this); }
        void visit(PrivateLayout layout) {
          asImpl().visitField(Field::RefCountedStruct); // object header
          asImpl().visitField(Field::Int8Ptr);
          asImpl().visitField(Field::Int8Ptr);     // Job.SchedulerPrivate
          asImpl().visitField(Field::Int32);       // Job.Flags
          asImpl().visitField(Field::Int32);       // Job.ID
          asImpl().visitField(Field::Int8Ptr);     // Job.Voucher
          asImpl().visitField(Field::Int8Ptr);     // Job.Reserved
          asImpl().visitField(Field::FunctionPtr); // Job.RunJob/Job.ResumeTask
          asImpl().visitField(Field::SwiftContextPtr); // Task.ResumeContext
          if (layout.isPointer8Bytes()) {
            // #if SWIFT_POINTER_IS_8_BYTES
            //   void *Reserved64;
            // #endif
            asImpl().visitField(Field::Ptr);
          }
          if (layout.isOld()) {
            //// May or may not have been visible in Task.h.
            // swift::atomic<ActiveTaskStatus> Status;
            asImpl().visitArray(Field::Ptr,
                                layout.activeTaskStatusSizeInWords());
          } else {
            //// Always hidden via OpaquePrivateStorage.
            // uintptr_t ExclusivityAccessSet[2] = {0, 0};
            asImpl().visitArray(Field::IntPtr, 2);
            //// The size here is always ((32-bit && escalating) ? 4 : 2) *
            /// word_size.
            // alignas(ActiveTaskStatus) char
            // StatusStorage[sizeof(ActiveTaskStatus)];
            asImpl().visitArray(Field::Ptr,
                                layout.activeTaskStatusSizeInWords());
          }
          //// Never directly visible in Task.h.
          // TaskAllocator Allocator;
          //  The first field is always a pointer to the latest allocation.
          asImpl().visitField(Field::AllocationPtr);
        }
      };
      std::string getName() {
        switch (kind) {
        case Old32Bit:
          return "swift.back_deploy.task.pre_57";
        case Old64Bit:
          return "swift.back_deploy.task.pre_57";
        case New32BitSansEscalation:
          return "swift.back_deploy.task.post_57.nonescalating";
        case New32BitWithEscalation:
          return "swift.back_deploy.task.post_57.escalating";
        case New64Bit:
          return "swift.back_deploy.task.post_57";
        }
        llvm_unreachable("covered switch");
      };
      void getFields(IRGenModule &IGM,
                     SmallVectorImpl<llvm::Type *> &fieldTys) {
        struct Visitor : PrivateLayout::FieldVisitor<Visitor> {
          SmallVectorImpl<llvm::Type *> &fieldTys;
          IRGenModule &IGM;
          Visitor(SmallVectorImpl<llvm::Type *> &fieldTys, IRGenModule &IGM)
              : fieldTys(fieldTys), IGM(IGM) {}

          void visitField(PrivateLayout::Field field) {
            fieldTys.push_back(field.getType(IGM));
          };
          void visitArray(PrivateLayout::Field field, unsigned count) {
            fieldTys.push_back(llvm::ArrayType::get(field.getType(IGM), count));
          }
        };

        Visitor visitor(fieldTys, IGM);
        visitor.visit(*this);
      }
      unsigned getAllocatorIndex() {
        struct Visitor : FieldVisitor<Visitor> {
          unsigned index = 0;
          void visitField(PrivateLayout::Field field) {
            if (field.kind == Field::AllocationPtr)
              return;
            index++;
          };
          void visitArray(PrivateLayout::Field field, unsigned count) {
            index++;
          }
        };
        Visitor visitor;
        visitor.visit(*this);
        return visitor.index;
      }
      Size getAllocatorOffset(IRGenModule &IGM) {
        struct Visitor : FieldVisitor<Visitor> {
          Size wordSize;
          Size offset;
          Visitor(Size wordSize) : wordSize(wordSize), offset(0) {}
          void visitField(PrivateLayout::Field field) {
            if (field.kind == Field::AllocationPtr)
              return;
            offset += field.getSizeInBytes(wordSize);
          };
          void visitArray(PrivateLayout::Field field, unsigned count) {
            offset += field.getSizeInBytes(wordSize) * count;
          }
        };
        Visitor visitor(IGM.getPointerSize());
        visitor.visit(*this);
        return Size(visitor.offset);
      }
    };

    llvm::DenseMap<PrivateLayout::Kind, llvm::StructType *> extendedTaskTys;
    llvm::StructType *getExtendedSwiftTaskTy(PrivateLayout layout) {
      auto iterator = extendedTaskTys.find(layout.kind);
      if (iterator != extendedTaskTys.end())
        return iterator->second;

      assert(layout.isPointer8Bytes() == isPointer8Bytes);

      SmallVector<llvm::Type *, 16> fieldTys;
      layout.getFields(IGM, fieldTys);

      auto name = layout.getName();
      return llvm::StructType::create(IGM.getLLVMContext(), fieldTys, name,
                                      /*packed*/ false);
    }

    llvm::StructType *_allocationTy = nullptr;
    /// The type of the allocation to which there's a pointer within the Task.
    llvm::StructType *getAllocationTy() {
      if (!_allocationTy) {
        _allocationTy = IGM.createTransientStructType(
            "swift.back_deploy.task.stack_allocator.allocation",
            {// Allocation *previous;
             IGM.PtrTy,
             // Slab *slab;
             IGM.PtrTy});
      }
      return _allocationTy;
    }

    /// The facts about the layout of Task in some PrivateLayout that will
    /// allow the offset to the address of the latest allocation to be
    /// calculated.
    struct TaskLayoutInfo {
      llvm::StructType *ty;
      unsigned allocationIndex;
      Size allocationOffset;
    };
    TaskLayoutInfo getTaskLayoutInfo(PrivateLayout layout) {
      return {getExtendedSwiftTaskTy(layout), layout.getAllocatorIndex(),
              layout.getAllocatorOffset(IGM)};
    };

    /// Calculate the address of the pointer to the latest allocation.  It's an
    /// offset from \p task determined by \p info.
    Address computeOffsetIntoTask(llvm::Value *task, TaskLayoutInfo info) {
      auto taskAddr = Address(task, info.ty, IGF.IGM.getPointerAlignment());
      auto allocationPtrAddr = IGF.Builder.CreateStructGEP(
          taskAddr, info.allocationIndex, info.allocationOffset);
      return allocationPtrAddr;
    }

    /// Load the \p allocationPtrAddr and offset from it to calculate the latest
    /// allocated memory.  Pass that allocated memory to swift_task_dealloc.
    ///
    /// If this allocated memory is the memory we are deallocating "through"
    /// (i.e \p ptr), then we're done (i.e. br \p exit).  Otherwise, keep going
    /// (i.e. br \p loop).
    void emitLoopBlock(llvm::BasicBlock *loop, Address allocationPtrAddr,
                       llvm::Value *ptr, llvm::BasicBlock *exit) {
      IGF.Builder.emitBlock(loop);
      auto allocationAddr =
          IGF.Builder.CreateLoad(allocationPtrAddr, "allocation");
      auto headerSize = IGF.alignUpToMaximumAlignment(
          IGM.SizeTy, llvm::ConstantInt::get(
                          IGM.IntPtrTy, 2 * IGM.getPointerSize().getValue()));
      auto *current = IGF.Builder.CreateInBoundsGEP(IGM.Int8Ty, allocationAddr,
                                                    {headerSize});

      IGF.Builder.CreateCall(IGM.getTaskDeallocFunctionPointer(), {current});

      auto *currentIsntPtr =
          IGF.Builder.CreateICmpNE(current, ptr, "current_is_not_ptr");
      IGF.Builder.CreateCondBr(currentIsntPtr, loop, exit);
    }

    /// Return void.
    void emitExitBlock(llvm::BasicBlock *exit) {
      // Emit the exit block.
      IGF.Builder.emitBlock(exit);
      IGF.Builder.CreateRetVoid();
    }
  };

public:
  GetDeallocThroughFn(IRGenModule &IGM) : IGM(IGM) {}
  llvm::Constant *get() {
    if (EnableRuntimeTaskDeallocThrough &&
        IGM.getAvailabilityRange().isContainedIn(
            IGM.Context.getCoroutineAccessorsAvailability())) {
      // For high enough deployment targets, just use the runtime entry point.
      return IGM.getTaskDeallocThroughFn();
    }
    return IGM.getOrCreateHelperFunction(
        "_swift_task_dealloc_through", IGM.VoidTy, {IGM.Int8PtrTy},
        [](auto &IGF) { Builder(IGF).build(); },
        /*setIsNoInline=*/true,
        /*forPrologue=*/false,
        /*isPerformanceConstraint=*/false,
        /*optionalLinkageOverride=*/nullptr, IGM.SwiftCC);
  }
};

} // end anonymous namespace

void IRGenFunction::emitTaskDeallocThrough(Address address) {
  auto getDeallocThroughFn = GetDeallocThroughFn(IGM);
  auto fnPtr = FunctionPointer::forDirect(
      FunctionPointer::Kind::Function, getDeallocThroughFn.get(), nullptr,
      IGM.getTaskDeallocThroughFunctionPointer().getSignature());
  auto *call = Builder.CreateCall(fnPtr, {address.getAddress()});
  call->setDoesNotThrow();
  call->setCallingConv(IGM.SwiftCC);
}

namespace {
struct AllocationKind {
  enum Kind {
    Normal,
    Frame,
  };
  Kind kind;
  constexpr AllocationKind(Kind kind) : kind(kind) {}
  constexpr operator Kind() { return kind; }

  StringRef getAllocationFunctionName() {
    switch (kind) {
    case Normal:
      return "_swift_coro_alloc";
    case Frame:
      return "_swift_coro_alloc_frame";
    }
  }

  StringRef getDeallocationFunctionName() {
    switch (kind) {
    case Normal:
      return "_swift_coro_dealloc";
    case Frame:
      return "_swift_coro_dealloc_frame";
    }
  }
};

struct Allocator {
  struct Field {
    enum Kind : uint8_t {
      Flags = 0,
      Allocate = 1,
      Deallocate = 2,
      AllocateFrame = 3,
      DeallocateFrame = 4,
    };
    constexpr static std::array<Field, 5> allFields() {
      return {Field::Flags, Field::Allocate, Field::Deallocate,
              Field::AllocateFrame, Field::DeallocateFrame};
    }
    Kind kind;
    constexpr Field(Kind kind) : kind(kind) {}
    constexpr operator Kind() { return kind; }

    bool isAllocateFunction() const {
      switch (kind) {
      case Flags:
      case Field::Deallocate:
      case Field::DeallocateFrame:
        return false;
      case Field::Allocate:
      case Field::AllocateFrame:
        return true;
      }
    }

    llvm::Type *getType(IRGenModule &IGM) {
      switch (kind) {
      case Flags:
        return IGM.Int32Ty;
      case Field::Allocate:
      case Field::Deallocate:
      case Field::AllocateFrame:
      case Field::DeallocateFrame:
        return IGM.PtrTy;
      }
    }

    Alignment getAlignment(IRGenModule &IGM) {
      switch (kind) {
      case Flags:
        return Alignment(4);
      case Field::Allocate:
      case Field::Deallocate:
      case Field::AllocateFrame:
      case Field::DeallocateFrame:
        return IGM.getPointerAlignment();
      }
    }

    StringRef getName() {
      switch (kind) {
      case Flags:
        return "flags";
      case Field::Allocate:
        return "allocate_fn";
      case Field::Deallocate:
        return "deallocate_fn";
      case Field::AllocateFrame:
        return "allocate_frame_fn";
      case Field::DeallocateFrame:
        return "deallocate_frame_fn";
      }
    }

    bool isFunctionPointer() {
      switch (kind) {
      case Flags:
        return false;
      case Field::Allocate:
      case Field::Deallocate:
      case Field::AllocateFrame:
      case Field::DeallocateFrame:
        return true;
      }
    }

    llvm::FunctionType *getFunctionType(IRGenModule &IGM) {
      switch (kind) {
      case Flags:
        llvm_unreachable("not a function");
      case Field::Allocate:
      case Field::AllocateFrame:
        return IGM.CoroAllocateFnTy;
      case Field::Deallocate:
      case Field::DeallocateFrame:
        return IGM.CoroDeallocateFnTy;
      }
    }

    llvm::AttributeList getFunctionAttributeList(IRGenModule &IGM) {
      switch (kind) {
      case Flags:
        llvm_unreachable("not a function");
      case Field::Allocate:
      case Field::Deallocate:
      case Field::AllocateFrame:
      case Field::DeallocateFrame:
        auto attrs = llvm::AttributeList();
        IGM.addSwiftCoroAttributes(attrs, 1);
        return attrs;
      }
    }

    const PointerAuthSchema &getSchema(IRGenModule &IGM) {
      switch (kind) {
      case Flags:
        llvm_unreachable("no schema");
      case Field::Allocate:
        return IGM.getOptions().PointerAuth.CoroAllocationFunction;
      case Field::Deallocate:
        return IGM.getOptions().PointerAuth.CoroDeallocationFunction;
      case Field::AllocateFrame:
        return IGM.getOptions().PointerAuth.CoroFrameAllocationFunction;
      case Field::DeallocateFrame:
        return IGM.getOptions().PointerAuth.CoroFrameDeallocationFunction;
      }
    }

    const PointerAuthEntity getEntity(IRGenModule &IGM) {
      switch (kind) {
      case Flags:
        llvm_unreachable("no entity");
      case Field::Allocate:
        return PointerAuthEntity::Special::CoroAllocationFunction;
      case Field::Deallocate:
        return PointerAuthEntity::Special::CoroDeallocationFunction;
      case Field::AllocateFrame:
        return PointerAuthEntity::Special::CoroFrameAllocationFunction;
      case Field::DeallocateFrame:
        return PointerAuthEntity::Special::CoroFrameDeallocationFunction;
      }
    }
  };

  static void visitFields(llvm::function_ref<void(Field)> visitor) {
    for (auto field : Field::allFields()) {
      visitor(field);
    }
  }

  static ConstantInitFuture
  buildStruct(ConstantInitBuilder &builder, IRGenModule &IGM,
              llvm::function_ref<llvm::Constant *(Field)> valueForField) {
    auto allocator = builder.beginStruct(IGM.CoroAllocatorTy);
    visitFields([&](auto field) {
      auto *value = valueForField(field);
      if (!field.isFunctionPointer()) {
        allocator.add(value);
        return;
      }
      allocator.addSignedPointer(value, field.getSchema(IGM),
                                 field.getEntity(IGM));
    });
    return allocator.finishAndCreateFuture();
  }

  llvm::Value *address;
  IRGenFunction &IGF;

  Allocator(llvm::Value *address, IRGenFunction &IGF)
      : address(address), IGF(IGF) {}

  struct FieldLoad {
    llvm::Value *address;
    llvm::Value *value;
  };

  FieldLoad loadField(Field field) {
    auto *fieldAddress = IGF.Builder.CreateInBoundsGEP(
        IGF.IGM.CoroAllocatorTy, address,
        {llvm::ConstantInt::get(IGF.IGM.Int32Ty, 0),
         llvm::ConstantInt::get(IGF.IGM.Int32Ty, field.kind)});
    auto *value =
        IGF.Builder.CreateLoad(Address(fieldAddress, field.getType(IGF.IGM),
                                       field.getAlignment(IGF.IGM)),
                               field.getName());
    return {fieldAddress, value};
  }

  llvm::Value *getFlags() { return loadField(Field::Flags).value; }

  FunctionPointer getAllocate(AllocationKind kind) {
    switch (kind) {
    case AllocationKind::Normal:
      return getAllocate();
    case AllocationKind::Frame:
      return getAllocateFrame();
    }
  }

  FunctionPointer getDeallocate(AllocationKind kind) {
    switch (kind) {
    case AllocationKind::Normal:
      return getDeallocate();
    case AllocationKind::Frame:
      return getDeallocateFrame();
    }
  }

  void emitOnNullAllocator(StringRef nullBlockName, StringRef nonnullBlockName,
                           llvm::function_ref<void(IRGenFunction &)> body) {
    auto *nullAllocator =
        IGF.Builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ, address,
                              llvm::ConstantPointerNull::get(
                                  cast<llvm::PointerType>(address->getType())));
    auto *poplessReturn = IGF.createBasicBlock(nullBlockName);
    auto *normalReturn = IGF.createBasicBlock(nonnullBlockName);
    IGF.Builder.CreateCondBr(nullAllocator, poplessReturn, normalReturn);
    IGF.Builder.emitBlock(poplessReturn);
    body(IGF);
    // Start emitting the "normal" block.
    IGF.Builder.emitBlock(normalReturn);
  }

private:
  FunctionPointer getAllocate() { return getFunctionPointer(Field::Allocate); }

  FunctionPointer getDeallocate() {
    return getFunctionPointer(Field::Deallocate);
  }

  FunctionPointer getAllocateFrame() {
    return getFunctionPointer(Field::AllocateFrame);
  }

  FunctionPointer getDeallocateFrame() {
    return getFunctionPointer(Field::DeallocateFrame);
  }

  FunctionPointer getFunctionPointer(Field field) {
    auto fieldValues = loadField(field);
    auto *callee = fieldValues.value;
    if (auto &schema = field.getSchema(IGF.IGM)) {
      auto info = PointerAuthInfo::emit(IGF, schema, fieldValues.address,
                                        field.getEntity(IGF.IGM));
      callee = emitPointerAuthAuth(IGF, callee, info);
    }
    return FunctionPointer::createUnsigned(
        FunctionPointer::Kind::Function, callee,
        Signature(field.getFunctionType(IGF.IGM),
                  field.getFunctionAttributeList(IGF.IGM), IGF.IGM.SwiftCC));
  }
};

llvm::Constant *getCoroAllocFn(AllocationKind kind, IRGenModule &IGM) {
  return IGM.getOrCreateHelperFunction(
      kind.getAllocationFunctionName(), IGM.CoroAllocationTy,
      {IGM.CoroAllocatorPtrTy, IGM.CoroAllocationTy, IGM.SizeTy, IGM.Int64Ty},
      [kind](IRGenFunction &IGF) {
        auto isSwiftCoroCCAvailable =
            IGF.IGM.SwiftCoroCC == llvm::CallingConv::SwiftCoro;
        auto parameters = IGF.collectParameters();
        auto *frame = parameters.claimNext();
        auto *allocatorValue = parameters.claimNext();
        auto allocator = Allocator(allocatorValue, IGF);
        auto *size = parameters.claimNext();
        auto *typeID = parameters.claimNext();
        if (isSwiftCoroCCAvailable) {
          // swiftcorocc is available, so if there's no allocator pointer,
          // allocate storage on the stack and return a pointer to it without
          // popping the stack.
          allocator.emitOnNullAllocator("popless", "normal", [size](auto &IGF) {
            // Emit the dynamic alloca.
            auto *alloca =
                IGF.Builder.IRBuilderBase::CreateAlloca(IGF.IGM.Int8Ty, size);
            alloca->setAlignment(llvm::Align(MaximumAlignment));
            auto *retPopless = IGF.Builder.CreateIntrinsic(
                IGF.IGM.VoidTy, llvm::Intrinsic::ret_popless, {});
            retPopless->setTailCallKind(
                llvm::CallInst::TailCallKind::TCK_MustTail);
            IGF.Builder.CreateRet(alloca);
          });
        }
        auto fnPtr = allocator.getAllocate(kind);
        auto *call = IGF.Builder.CreateCall(
            fnPtr, {frame, allocatorValue, size, typeID});
        call->setDoesNotThrow();
        call->setCallingConv(IGF.IGM.SwiftCC);
        IGF.Builder.CreateRet(call);
      },
      /*setIsNoInline=*/true,
      /*forPrologue=*/false,
      /*isPerformanceConstraint=*/false,
      /*optionalLinkageOverride=*/nullptr, IGM.SwiftCoroCC,
      /*transformAttributes=*/
      [&IGM](llvm::AttributeList &attrs) {
        IGM.addSwiftCoroAttributes(attrs, 1);
      });
}

llvm::Constant *getCoroDeallocFn(AllocationKind kind, IRGenModule &IGM) {
  return IGM.getOrCreateHelperFunction(
      kind.getDeallocationFunctionName(), IGM.VoidTy,
      {IGM.CoroAllocatorPtrTy, IGM.CoroAllocationTy, IGM.CoroAllocationTy},
      [kind](IRGenFunction &IGF) {
        auto isSwiftCoroCCAvailable =
            IGF.IGM.SwiftCoroCC == llvm::CallingConv::SwiftCoro;
        auto parameters = IGF.collectParameters();
        auto *frame = parameters.claimNext();
        auto *allocatorValue = parameters.claimNext();
        auto allocator = Allocator(allocatorValue, IGF);
        auto *ptr = parameters.claimNext();
        if (isSwiftCoroCCAvailable) {
          // swiftcorocc is available, so if there's no allocator pointer,
          // storage was allocated on the stack which will be naturally cleaned
          // up when the coroutine's frame is "freed".
          allocator.emitOnNullAllocator("null_allocator", "nonnull_allocator",
                                        [&](auto &IGF) {
                                          // Nothing to do here.
                                          IGF.Builder.CreateRetVoid();
                                        });
        }
        auto shouldDeallocateImmediatelyFlag = CoroAllocatorFlags(0);
        shouldDeallocateImmediatelyFlag.setShouldDeallocateImmediately(true);
        auto *flags = allocator.getFlags();
        auto *isDeallocDeferringAllocator = IGF.Builder.CreateICmpNE(
            IGF.Builder.CreateAnd(
                flags,
                llvm::APInt(IGF.IGM.Int32Ty->getBitWidth(),
                            shouldDeallocateImmediatelyFlag.getOpaqueValue())),
            llvm::ConstantInt::get(IGF.IGM.Int32Ty, 0));
        auto *deferringAllocatorBlock =
            IGF.createBasicBlock("deferring_allocator");
        auto *normalBlock = IGF.createBasicBlock("normal");
        IGF.Builder.CreateCondBr(isDeallocDeferringAllocator,
                                 deferringAllocatorBlock, normalBlock);
        IGF.Builder.emitBlock(deferringAllocatorBlock);
        // Nothing to do here.
        IGF.Builder.CreateRetVoid();
        // Start emitting the "normal" block.
        IGF.Builder.emitBlock(normalBlock);
        auto fnPtr = allocator.getDeallocate(kind);
        auto *call = IGF.Builder.CreateCall(fnPtr, {frame, allocatorValue, ptr});
        call->setDoesNotThrow();
        call->setCallingConv(IGF.IGM.SwiftCC);
        IGF.Builder.CreateRetVoid();
      },
      /*setIsNoInline=*/true,
      /*forPrologue=*/false,
      /*isPerformanceConstraint=*/false,
      /*optionalLinkageOverride=*/nullptr, IGM.SwiftCoroCC,
      /*transformAttributes=*/
      [&IGM](llvm::AttributeList &attrs) {
        IGM.addSwiftCoroAttributes(attrs, 1);
      });
}

} // end anonymous namespace

llvm::Constant *swift::irgen::getCoroAllocFn(IRGenModule &IGM) {
  return getCoroAllocFn(AllocationKind::Normal, IGM);
}

llvm::Constant *swift::irgen::getCoroAllocFrameFn(IRGenModule &IGM) {
  return getCoroAllocFn(AllocationKind::Frame, IGM);
}

llvm::Constant *swift::irgen::getCoroDeallocFn(IRGenModule &IGM) {
  return getCoroDeallocFn(AllocationKind::Normal, IGM);
}

llvm::Constant *swift::irgen::getCoroDeallocFrameFn(IRGenModule &IGM) {
  return getCoroDeallocFn(AllocationKind::Frame, IGM);
}

static llvm::Constant *getAddrOfSwiftCoroAllocatorThunk(
    StringRef name, Allocator::Field field, IRGenModule &IGM,
    llvm::function_ref<llvm::Value *(IRGenFunction &, llvm::Value *,
                                     llvm::Value *, llvm::Value *,
                                     llvm::Value *)>
        builder) {
  auto *ty = field.getFunctionType(IGM);
  return IGM.getOrCreateHelperFunction(
      name, ty->getReturnType(), ty->params(),
      [builder, field](IRGenFunction &IGF) {
        auto parameters = IGF.collectParameters();
        auto *frame = parameters.claimNext();
        auto *allocator = parameters.claimNext();
        // allocate - size; deallocate - address
        auto *value = parameters.claimNext();
        auto *typeID =
            field.isAllocateFunction() ? parameters.claimNext() : nullptr;
        auto *result = builder(IGF, frame, allocator, value, typeID);
        if (field.isAllocateFunction()) {
          IGF.Builder.CreateRet(result);
        } else {
          IGF.Builder.CreateRetVoid();
        }
      },
      /*setIsNoInline=*/true, /*forPrologue=*/false,
      /*isPerformanceConstraint=*/false,
      /*optionalLinkage=*/nullptr,
      /*specialCallingConv=*/std::nullopt,
      /*transformAttributes=*/
      [&IGM](llvm::AttributeList &attrs) {
        IGM.addSwiftCoroAttributes(attrs, 1);
      });
}

using GetAllocatorFunctionPointer = FunctionPointer (IRGenModule::*)();

static llvm::Constant *
getAddrOfSwiftCoroAllocatorThunk(StringRef name, Allocator::Field field,
                                 GetAllocatorFunctionPointer getCator,
                                 IRGenModule &IGM) {
  return getAddrOfSwiftCoroAllocatorThunk(
      name, field, IGM,
      [getCator](IRGenFunction &IGF, auto *frame, auto *allocator, auto *value,
                 auto *typeID) {
        auto alloc = (IGF.IGM.*getCator)();
        auto *call = IGF.Builder.CreateCall(alloc, {value});
        return call;
      });
}

static llvm::Constant *getAddrOfGlobalCoroAllocator(
    IRGenModule &IGM, CoroAllocatorKind kind, bool shouldDeallocateImmediately,
    llvm::Constant *allocFn, llvm::Constant *deallocFn,
    llvm::Constant *allocFrameFn, llvm::Constant *deallocFrameFn) {
  auto entity = LinkEntity::forCoroAllocator(kind);
  auto taskAllocator = IGM.getOrCreateLazyGlobalVariable(
      entity,
      [&](ConstantInitBuilder &builder) -> ConstantInitFuture {
        return Allocator::buildStruct(
            builder, IGM, [&](auto field) -> llvm::Constant * {
              switch (field) {
              case Allocator::Field::Flags: {
                auto flags = CoroAllocatorFlags(kind);
                flags.setShouldDeallocateImmediately(
                    shouldDeallocateImmediately);
                return llvm::ConstantInt::get(IGM.Int32Ty,
                                              flags.getOpaqueValue());
              }
              case Allocator::Field::Allocate:
                return allocFn;
              case Allocator::Field::Deallocate:
                return deallocFn;
              case Allocator::Field::AllocateFrame:
                return allocFrameFn;
              case Allocator::Field::DeallocateFrame:
                return deallocFrameFn;
              }
            });
      },
      [&](llvm::GlobalVariable *var) { var->setConstant(true); });
  return taskAllocator;
}

static llvm::Constant *getAddrOfGlobalCoroAllocator(
    IRGenModule &IGM, CoroAllocatorKind kind, bool shouldDeallocateImmediately,
    llvm::Constant *allocFn, llvm::Constant *deallocFn) {
  return getAddrOfGlobalCoroAllocator(IGM, kind, shouldDeallocateImmediately,
                                      allocFn, deallocFn, allocFn, deallocFn);
}

static llvm::Constant *getAddrOfSwiftCoroMalloc(IRGenModule &IGM) {
  return getAddrOfSwiftCoroAllocatorThunk(
      "_swift_coro_malloc", Allocator::Field::Allocate,
      &IRGenModule::getMallocFunctionPointer, IGM);
}

static llvm::Constant *getAddrOfSwiftCoroFree(IRGenModule &IGM) {
  return getAddrOfSwiftCoroAllocatorThunk(
      "_swift_coro_free", Allocator::Field::Deallocate,
      &IRGenModule::getFreeFunctionPointer, IGM);
}

llvm::Constant *IRGenModule::getAddrOfGlobalCoroMallocAllocator() {
  return getAddrOfGlobalCoroAllocator(*this, CoroAllocatorKind::Malloc,
                                      /*shouldDeallocateImmediately=*/true,
                                      getAddrOfSwiftCoroMalloc(*this),
                                      getAddrOfSwiftCoroFree(*this));
}

static llvm::Constant *getAddrOfSwiftCoroTypedMalloc(IRGenModule &IGM) {
  return getAddrOfSwiftCoroAllocatorThunk(
      "_swift_coro_typed_malloc", Allocator::Field::Allocate, IGM,
      [](IRGenFunction &IGF, auto *frame, auto *allocator, auto *size,
         auto *typeID) {
        auto callee = getCoroFrameAllocStubFunctionPointer(IGF.IGM);
        auto *allocation = IGF.Builder.CreateCall(callee, {size, typeID});
        return allocation;
      });
}

llvm::Constant *IRGenModule::getAddrOfGlobalCoroTypedMallocAllocator() {
  return getAddrOfGlobalCoroAllocator(*this, CoroAllocatorKind::TypedMalloc,
                                      /*shouldDeallocateImmediately=*/true,
                                      getAddrOfSwiftCoroTypedMalloc(*this),
                                      getAddrOfSwiftCoroFree(*this));
}

static llvm::Constant *
getAddrOfSwiftCoroTaskAlloc(IRGenModule &IGM) {
  return getAddrOfSwiftCoroAllocatorThunk(
      "_swift_coro_task_alloc", Allocator::Field::Allocate,
      &IRGenModule::getTaskAllocFunctionPointer, IGM);
}

static llvm::Constant *
getAddrOfSwiftCoroTaskDealloc(IRGenModule &IGM) {
  return getAddrOfSwiftCoroAllocatorThunk(
      "_swift_coro_task_dealloc", Allocator::Field::Deallocate,
      &IRGenModule::getTaskDeallocFunctionPointer, IGM);
}

llvm::Constant *IRGenModule::getAddrOfGlobalCoroAsyncTaskAllocator() {
  return getAddrOfGlobalCoroAllocator(*this, CoroAllocatorKind::Async,
                                      /*shouldDeallocateImmediately=*/false,
                                      getAddrOfSwiftCoroTaskAlloc(*this),
                                      getAddrOfSwiftCoroTaskDealloc(*this));
}

llvm::Value *
irgen::emitYieldOnce2CoroutineAllocator(IRGenFunction &IGF,
                                        std::optional<CoroAllocatorKind> kind) {
  if (!kind) {
    return IGF.getCoroutineAllocator();
  }
  switch (*kind) {
  case CoroAllocatorKind::Stack:
    return llvm::ConstantPointerNull::get(IGF.IGM.CoroAllocatorPtrTy);
  case CoroAllocatorKind::Async:
    return IGF.IGM.getAddrOfGlobalCoroAsyncTaskAllocator();
  case CoroAllocatorKind::Malloc:
    return IGF.IGM.getAddrOfGlobalCoroMallocAllocator();
  case CoroAllocatorKind::TypedMalloc:
    return IGF.IGM.getAddrOfGlobalCoroTypedMallocAllocator();
  }
  llvm_unreachable("unhandled case");
}
