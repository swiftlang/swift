//===--- MemAccessUtils.cpp - Utilities for SIL memory access. ------------===//
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

#define DEBUG_TYPE "sil-access-utils"

#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILUndef.h"

using namespace swift;

AccessedStorage::Kind AccessedStorage::classify(SILValue base) {
  switch (base->getKind()) {
  // An AllocBox is a fully identified memory location.
  case ValueKind::AllocBoxInst:
    return Box;
  // An AllocStack is a fully identified memory location, which may occur
  // after inlining code already subjected to stack promotion.
  case ValueKind::AllocStackInst:
    return Stack;
  case ValueKind::GlobalAddrInst:
    return Global;
  case ValueKind::ApplyInst: {
    FullApplySite apply(cast<ApplyInst>(base));
    if (auto *funcRef = apply.getReferencedFunction()) {
      if (getVariableOfGlobalInit(funcRef))
        return Global;
    }
    return Unidentified;
  }
  case ValueKind::RefElementAddrInst:
    return Class;
  // A function argument is effectively a nested access, enforced
  // independently in the caller and callee.
  case ValueKind::SILFunctionArgument:
    return Argument;
  // View the outer begin_access as a separate location because nested
  // accesses do not conflict with each other.
  case ValueKind::BeginAccessInst:
    return Nested;
  default:
    return Unidentified;
  }
}

AccessedStorage::AccessedStorage(SILValue base, Kind kind) {
  assert(base && "invalid storage base");
  initKind(kind);

  switch (kind) {
  case Box:
    assert(isa<AllocBoxInst>(base));
    value = base;
    break;
  case Stack:
    assert(isa<AllocStackInst>(base));
    value = base;
    break;
  case Nested:
    assert(isa<BeginAccessInst>(base));
    value = base;
    break;
  case Unidentified:
    value = base;
    break;
  case Argument:
    paramIndex = cast<SILFunctionArgument>(base)->getIndex();
    break;
  case Global:
    if (auto *GAI = dyn_cast<GlobalAddrInst>(base))
      global = GAI->getReferencedGlobal();
    else {
      FullApplySite apply(cast<ApplyInst>(base));
      auto *funcRef = apply.getReferencedFunction();
      assert(funcRef);
      global = getVariableOfGlobalInit(funcRef);
      assert(global);
      // Require a decl for all formally accessed globals defined in this
      // module. (Access of globals defined elsewhere has Unidentified storage).
      // AccessEnforcementWMO requires this.
      assert(global->getDecl());
    }
    break;
  case Class: {
    // Do a best-effort to find the identity of the object being projected
    // from. It is OK to be unsound here (i.e. miss when two ref_element_addrs
    // actually refer the same address) because these will be dynamically
    // checked.
    auto *REA = cast<RefElementAddrInst>(base);
    SILValue Object = stripBorrow(REA->getOperand());
    objProj = ObjectProjection(Object, Projection(REA));
  }
  }
}

const ValueDecl *AccessedStorage::getDecl(SILFunction *F) const {
  switch (getKind()) {
  case Box:
    return cast<AllocBoxInst>(value)->getLoc().getAsASTNode<VarDecl>();

  case Stack:
    return cast<AllocStackInst>(value)->getDecl();

  case Global:
    return global->getDecl();

  case Class:
    return objProj.getProjection().getVarDecl(objProj.getObject()->getType());

  case Argument:
    return getArgument(F)->getDecl();

  case Nested:
    return nullptr;

  case Unidentified:
    return nullptr;
  }
}

const char *AccessedStorage::getKindName(AccessedStorage::Kind k) {
  switch (k) {
  case Box:
    return "Box";
  case Stack:
    return "Stack";
  case Nested:
    return "Nested";
  case Unidentified:
    return "Unidentified";
  case Argument:
    return "Argument";
  case Global:
    return "Global";
  case Class:
    return "Class";
  }
}

void AccessedStorage::print(raw_ostream &os) const {
  os << getKindName(getKind()) << " ";
  switch (getKind()) {
  case Box:
  case Stack:
  case Nested:
  case Unidentified:
    os << value;
    break;
  case Argument:
    os << "index: " << paramIndex << "\n";
    break;
  case Global:
    os << *global;
    break;
  case Class:
    os << objProj.getObject() << "  ";
    objProj.getProjection().print(os, objProj.getObject()->getType());
    os << "\n";
  }
}

void AccessedStorage::dump() const { print(llvm::dbgs()); }

// Return true if the given apply invokes a global addressor defined in another
// module.
static bool isExternalGlobalAddressor(ApplyInst *AI) {
  FullApplySite apply(AI);
  auto *funcRef = apply.getReferencedFunction();
  if (!funcRef)
    return false;
  
  return funcRef->isGlobalInit() && funcRef->isExternalDeclaration();
}

// Return true if the given StructExtractInst extracts the RawPointer from
// Unsafe[Mutable]Pointer.
static bool isUnsafePointerExtraction(StructExtractInst *SEI) {
  assert(isa<BuiltinRawPointerType>(SEI->getType().getASTType()));
  auto &C = SEI->getModule().getASTContext();
  auto *decl = SEI->getStructDecl();
  return decl == C.getUnsafeMutablePointerDecl()
    || decl == C.getUnsafePointerDecl();
}

// Given an address base is a block argument, verify that it is actually a box
// projected from a switch_enum. This is a valid pattern at any SIL stage
// resulting in a block-type phi. In later SIL stages, the optimizer may form
// address-type phis, causing this assert if called on those cases.
static void checkSwitchEnumBlockArg(SILPHIArgument *arg) {
  assert(!arg->getType().isAddress());
  SILBasicBlock *Pred = arg->getParent()->getSinglePredecessorBlock();
  if (!Pred || !isa<SwitchEnumInst>(Pred->getTerminator())) {
    arg->dump();
    llvm_unreachable("unexpected box source.");
  }
}

/// Return true if the given address value is produced by a special address
/// producer that is only used for local initialization, not formal access.
static bool isAddressForLocalInitOnly(SILValue sourceAddr) {
  switch (sourceAddr->getKind()) {
  default:
    return false;

  // Value to address conversions: the operand is the non-address source
  // value. These allow local mutation of the value but should never be used
  // for formal access of an lvalue.
  case ValueKind::OpenExistentialBoxInst:
  case ValueKind::ProjectExistentialBoxInst:
    return true;

  // Self-evident local initialization.
  case ValueKind::InitEnumDataAddrInst:
  case ValueKind::InitExistentialAddrInst:
  case ValueKind::AllocExistentialBoxInst:
  case ValueKind::AllocValueBufferInst:
  case ValueKind::ProjectValueBufferInst:
    return true;
  }
}

// AccessEnforcementWMO makes a strong assumption that all accesses are either
// identified or are *not* accessing a global variable or class property defined
// in this module. Consequently, we cannot simply bail out on
// PointerToAddressInst as an Unidentified access.
AccessedStorage swift::findAccessedStorage(SILValue sourceAddr) {
  SILValue address = sourceAddr;
  while (true) {
    AccessedStorage::Kind kind = AccessedStorage::classify(address);
    // First handle identified cases: these are always valid as the base of
    // a formal access.
    if (kind != AccessedStorage::Unidentified)
      return AccessedStorage(address, kind);

    // If the address producer cannot immediately be classified, follow the
    // use-def chain of address, box, or RawPointer producers.
    assert(address->getType().isAddress()
           || isa<SILBoxType>(address->getType().getASTType())
           || isa<BuiltinRawPointerType>(address->getType().getASTType()));

    // Handle other unidentified address sources.
    switch (address->getKind()) {
    default:
      if (isAddressForLocalInitOnly(address))
        return AccessedStorage(address, AccessedStorage::Unidentified);
      return AccessedStorage();

    case ValueKind::SILUndef:
      return AccessedStorage(address, AccessedStorage::Unidentified);

    case ValueKind::ApplyInst:
      if (isExternalGlobalAddressor(cast<ApplyInst>(address)))
        return AccessedStorage(address, AccessedStorage::Unidentified);

      // Don't currently allow any other calls to return an accessed address.
      return AccessedStorage();

    case ValueKind::StructExtractInst:
      // Handle nested access to a KeyPath projection. The projection itself
      // uses a Builtin. However, the returned UnsafeMutablePointer may be
      // converted to an address and accessed via an inout argument.
      if (isUnsafePointerExtraction(cast<StructExtractInst>(address)))
        return AccessedStorage(address, AccessedStorage::Unidentified);
      return AccessedStorage();

    // A block argument may be a box value projected out of
    // switch_enum. Address-type block arguments are not allowed.
    case ValueKind::SILPHIArgument:
      if (address->getType().isAddress())
        return AccessedStorage();

      checkSwitchEnumBlockArg(cast<SILPHIArgument>(address));
      return AccessedStorage(address, AccessedStorage::Unidentified);

    // Load a box from an indirect payload of an opaque enum.
    // We must have peeked past the project_box earlier in this loop.
    // (the indirectness makes it a box, the load is for address-only).
    //
    // %payload_adr = unchecked_take_enum_data_addr %enum : $*Enum, #Enum.case
    // %box = load [take] %payload_adr : $*{ var Enum }
    //
    // FIXME: this case should go away with opaque values.
    //
    // Otherwise return invalid AccessedStorage.
    case ValueKind::LoadInst: {
      if (address->getType().is<SILBoxType>()) {
        address = cast<LoadInst>(address)->getOperand();
        assert(isa<UncheckedTakeEnumDataAddrInst>(address));
        continue;
      }
      return AccessedStorage();
    }

    // ref_tail_addr project an address from a reference.
    // This is a valid address producer for nested @inout argument
    // access, but it is never used for formal access of identified objects.
    case ValueKind::RefTailAddrInst:
      return AccessedStorage(address, AccessedStorage::Unidentified);

    // Inductive cases:
    // Look through address casts to find the source address.
    case ValueKind::MarkUninitializedInst:
    case ValueKind::OpenExistentialAddrInst:
    case ValueKind::UncheckedAddrCastInst:
    // Inductive cases that apply to any type.
    case ValueKind::CopyValueInst:
    case ValueKind::MarkDependenceInst:
    // Look through a project_box to identify the underlying alloc_box as the
    // accesed object. It must be possible to reach either the alloc_box or the
    // containing enum in this loop, only looking through simple value
    // propagation such as copy_value.
    case ValueKind::ProjectBoxInst:
    // Handle project_block_storage just like project_box.
    case ValueKind::ProjectBlockStorageInst:
    // Look through begin_borrow in case a local box is borrowed.
    case ValueKind::BeginBorrowInst:
      address = cast<SingleValueInstruction>(address)->getOperand(0);
      continue;

    // Access to a Builtin.RawPointer. Treat this like the inductive cases
    // above because some RawPointers originate from identified locations. See
    // the special case for global addressors, which return RawPointer, above.
    //
    // If the inductive search does not find a valid addressor, it will
    // eventually reach the default case that returns in invalid location. This
    // is correct for RawPointer because, although accessing a RawPointer is
    // legal SIL, there is no way to guarantee that it doesn't access class or
    // global storage, so returning a valid unidentified storage object would be
    // incorrect. It is the caller's responsibility to know that formal access
    // to such a location can be safely ignored.
    //
    // For example:
    //
    // - KeyPath Builtins access RawPointer. However, the caller can check
    // that the access `isFromBuilin` and ignore the storage.
    //
    // - lldb generates RawPointer access for debugger variables, but SILGen
    // marks debug VarDecl access as 'Unsafe' and SIL passes don't need the
    // AccessedStorage for 'Unsafe' access.
    case ValueKind::PointerToAddressInst:
      address = cast<SingleValueInstruction>(address)->getOperand(0);
      continue;

    // Address-to-address subobject projections.
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
    case ValueKind::TailAddrInst:
    case ValueKind::IndexAddrInst:
      address = cast<SingleValueInstruction>(address)->getOperand(0);
      continue;
    }
  }
}

AccessedStorage swift::findAccessedStorageNonNested(SILValue sourceAddr) {
  while (true) {
    const AccessedStorage &storage = findAccessedStorage(sourceAddr);
    if (!storage || storage.getKind() != AccessedStorage::Nested)
      return storage;
    
    sourceAddr = cast<BeginAccessInst>(storage.getValue())->getSource();
  }
}

// Return true if the given access is on a 'let' lvalue.
static bool isLetAccess(const AccessedStorage &storage, SILFunction *F) {
  if (auto *decl = dyn_cast_or_null<VarDecl>(storage.getDecl(F)))
    return decl->isLet();

  // It's unclear whether a global will ever be missing it's varDecl, but
  // technically we only preserve it for debug info. So if we don't have a decl,
  // check the flag on SILGlobalVariable, which is guaranteed valid, 
  if (storage.getKind() == AccessedStorage::Global)
    return storage.getGlobal()->isLet();

  return false;
}

static bool isScratchBuffer(SILValue value) {
  // Special case unsafe value buffer access.
  return value->getType().is<BuiltinUnsafeValueBufferType>();
}

bool swift::memInstMustInitialize(Operand *memOper) {
  SILValue address = memOper->get();
  SILInstruction *memInst = memOper->getUser();

  switch (memInst->getKind()) {
  default:
    return false;

  case SILInstructionKind::CopyAddrInst: {
    auto *CAI = cast<CopyAddrInst>(memInst);
    return CAI->getDest() == address && CAI->isInitializationOfDest();
  }
  case SILInstructionKind::InitExistentialAddrInst:
  case SILInstructionKind::InitEnumDataAddrInst:
  case SILInstructionKind::InjectEnumAddrInst:
    return true;

  case SILInstructionKind::StoreInst:
    return cast<StoreInst>(memInst)->getOwnershipQualifier()
           == StoreOwnershipQualifier::Init;

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Store##Name##Inst: \
    return cast<Store##Name##Inst>(memInst)->isInitializationOfDest();
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Store##Name##Inst: \
    return cast<Store##Name##Inst>(memInst)->isInitializationOfDest();
#include "swift/AST/ReferenceStorage.def"
  }
}

bool swift::isPossibleFormalAccessBase(const AccessedStorage &storage,
                                       SILFunction *F) {
  switch (storage.getKind()) {
  case AccessedStorage::Box:
  case AccessedStorage::Stack:
    if (isScratchBuffer(storage.getValue()))
      return false;
    break;
  case AccessedStorage::Global:
    break;
  case AccessedStorage::Class:
    break;
  case AccessedStorage::Argument:
    // Function arguments are accessed by the caller.
    return false;
  case AccessedStorage::Nested: {
    // A begin_access is considered a separate base for the purpose of conflict
    // checking. However, for the purpose of inserting unenforced markers and
    // performaing verification, it needs to be ignored.
    auto *BAI = cast<BeginAccessInst>(storage.getValue());
    const AccessedStorage &nestedStorage =
      findAccessedStorage(BAI->getSource());
    if (!nestedStorage)
      return false;

    return isPossibleFormalAccessBase(nestedStorage, F);
  }
  case AccessedStorage::Unidentified:
    if (isAddressForLocalInitOnly(storage.getValue()))
      return false;

    if (isa<SILPHIArgument>(storage.getValue())) {
      checkSwitchEnumBlockArg(cast<SILPHIArgument>(storage.getValue()));
      return false;
    }
    // Pointer-to-address exclusivity cannot be enforced. `baseAddress` may be
    // pointing anywhere within an object.
    if (isa<PointerToAddressInst>(storage.getValue()))
      return false;

    if (isa<SILUndef>(storage.getValue()))
      return false;

    if (isScratchBuffer(storage.getValue()))
      return false;
  }
  // Additional checks that apply to anything that may fall through.

  // Immutable values are only accessed for initialization.
  if (isLetAccess(storage, F))
    return false;

  return true;
}

/// Helper for visitApplyAccesses that visits address-type call arguments,
/// including arguments to @noescape functions that are passed as closures to
/// the current call.
static void visitApplyAccesses(ApplySite apply,
                               llvm::function_ref<void(Operand *)> visitor) {
  for (Operand &oper : apply.getArgumentOperands()) {
    // Consider any address-type operand an access. Whether it is read or modify
    // depends on the argument convention.
    if (oper.get()->getType().isAddress()) {
      visitor(&oper);
      continue;
    }
    auto fnType = oper.get()->getType().getAs<SILFunctionType>();
    if (!fnType || !fnType->isNoEscape())
      continue;

    // When @noescape function closures are passed as arguments, their
    // arguments are considered accessed at the call site.
    FindClosureResult result = findClosureForAppliedArg(oper.get());
    if (!result.PAI)
      continue;

    // Recursively visit @noescape function closure arguments.
    visitApplyAccesses(result.PAI, visitor);
  }
}

static void visitBuiltinAddress(BuiltinInst *builtin,
                                llvm::function_ref<void(Operand *)> visitor) {
  if (auto kind = builtin->getBuiltinKind()) {
    switch (kind.getValue()) {
    default:
      builtin->dump();
      llvm_unreachable("unexpected bulitin memory access.");

      // Buitins that affect memory but can't be formal accesses.
    case BuiltinValueKind::UnexpectedError:
    case BuiltinValueKind::ErrorInMain:
    case BuiltinValueKind::IsOptionalType:
    case BuiltinValueKind::AllocRaw:
    case BuiltinValueKind::DeallocRaw:
    case BuiltinValueKind::Fence:
    case BuiltinValueKind::StaticReport:
    case BuiltinValueKind::Once:
    case BuiltinValueKind::OnceWithContext:
    case BuiltinValueKind::Unreachable:
    case BuiltinValueKind::CondUnreachable:
    case BuiltinValueKind::DestroyArray:
    case BuiltinValueKind::UnsafeGuaranteed:
    case BuiltinValueKind::UnsafeGuaranteedEnd:
    case BuiltinValueKind::Swift3ImplicitObjCEntrypoint:
    case BuiltinValueKind::TSanInoutAccess:
      return;

      // General memory access to a pointer in first operand position.
    case BuiltinValueKind::CmpXChg:
    case BuiltinValueKind::AtomicLoad:
    case BuiltinValueKind::AtomicStore:
    case BuiltinValueKind::AtomicRMW:
      // Currently ignored because the access is on a RawPointer, not a
      // SIL address.
      // visitor(&builtin->getAllOperands()[0]);
      return;

      // Arrays: (T.Type, Builtin.RawPointer, Builtin.RawPointer,
      // Builtin.Word)
    case BuiltinValueKind::CopyArray:
    case BuiltinValueKind::TakeArrayNoAlias:
    case BuiltinValueKind::TakeArrayFrontToBack:
    case BuiltinValueKind::TakeArrayBackToFront:
    case BuiltinValueKind::AssignCopyArrayNoAlias:
    case BuiltinValueKind::AssignCopyArrayFrontToBack:
    case BuiltinValueKind::AssignCopyArrayBackToFront:
    case BuiltinValueKind::AssignTakeArray:
      // Currently ignored because the access is on a RawPointer.
      // visitor(&builtin->getAllOperands()[1]);
      // visitor(&builtin->getAllOperands()[2]);
      return;
    }
  }
  if (auto ID = builtin->getIntrinsicID()) {
    switch (ID.getValue()) {
      // Exhaustively verifying all LLVM instrinsics that access memory is
      // impractical. Instead, we call out the few common cases and return in
      // the default case.
    default:
      return;
    case llvm::Intrinsic::memcpy:
    case llvm::Intrinsic::memmove:
      // Currently ignored because the access is on a RawPointer.
      // visitor(&builtin->getAllOperands()[0]);
      // visitor(&builtin->getAllOperands()[1]);
      return;
    case llvm::Intrinsic::memset:
      // Currently ignored because the access is on a RawPointer.
      // visitor(&builtin->getAllOperands()[0]);
      return;
    }
  }
  llvm_unreachable("Must be either a builtin or intrinsic.");
}

void swift::visitAccessedAddress(SILInstruction *I,
                                 llvm::function_ref<void(Operand *)> visitor) {
  assert(I->mayReadOrWriteMemory());

  // Reference counting instructions do not access user visible memory.
  if (isa<RefCountingInst>(I))
    return;

  if (isa<DeallocationInst>(I))
    return;

  if (auto apply = FullApplySite::isa(I)) {
    visitApplyAccesses(apply, visitor);
    return;
  }

  if (auto builtin = dyn_cast<BuiltinInst>(I)) {
    visitBuiltinAddress(builtin, visitor);
    return;
  }

  switch (I->getKind()) {
  default:
    I->dump();
    llvm_unreachable("unexpected memory access.");

  case SILInstructionKind::AssignInst:
    visitor(&I->getAllOperands()[AssignInst::Dest]);
    return;

  case SILInstructionKind::CheckedCastAddrBranchInst:
    visitor(&I->getAllOperands()[CheckedCastAddrBranchInst::Src]);
    visitor(&I->getAllOperands()[CheckedCastAddrBranchInst::Dest]);
    return;

  case SILInstructionKind::CopyAddrInst:
    visitor(&I->getAllOperands()[CopyAddrInst::Src]);
    visitor(&I->getAllOperands()[CopyAddrInst::Dest]);
    return;

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Store##Name##Inst:
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Store##Name##Inst:
#include "swift/AST/ReferenceStorage.def"
  case SILInstructionKind::StoreInst:
  case SILInstructionKind::StoreBorrowInst:
    visitor(&I->getAllOperands()[StoreInst::Dest]);
    return;

  case SILInstructionKind::SelectEnumAddrInst:
    visitor(&I->getAllOperands()[0]);
    return;

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Load##Name##Inst:
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Load##Name##Inst:
#include "swift/AST/ReferenceStorage.def"
  case SILInstructionKind::InitExistentialAddrInst:
  case SILInstructionKind::InjectEnumAddrInst:
  case SILInstructionKind::LoadInst:
  case SILInstructionKind::LoadBorrowInst:
  case SILInstructionKind::OpenExistentialAddrInst:
  case SILInstructionKind::SwitchEnumAddrInst:
  case SILInstructionKind::UncheckedTakeEnumDataAddrInst:
  case SILInstructionKind::UnconditionalCheckedCastInst: {
    // Assuming all the above have only a single address operand.
    assert(I->getNumOperands() - I->getNumTypeDependentOperands() == 1);
    Operand *singleOperand = &I->getAllOperands()[0];
    // Check the operand type because UnconditionalCheckedCastInst may operate
    // on a non-address.
    if (singleOperand->get()->getType().isAddress())
      visitor(singleOperand);
    return;
  }
  // Non-access cases: these are marked with memory side effects, but, by
  // themselves, do not access formal memory.
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Copy##Name##ValueInst:
#include "swift/AST/ReferenceStorage.def"
  case SILInstructionKind::AbortApplyInst:
  case SILInstructionKind::AllocBoxInst:
  case SILInstructionKind::AllocExistentialBoxInst:
  case SILInstructionKind::AllocGlobalInst:
  case SILInstructionKind::BeginAccessInst:
  case SILInstructionKind::BeginApplyInst:
  case SILInstructionKind::BeginBorrowInst:
  case SILInstructionKind::BeginUnpairedAccessInst:
  case SILInstructionKind::BindMemoryInst:
  case SILInstructionKind::CheckedCastValueBranchInst:
  case SILInstructionKind::CondFailInst:
  case SILInstructionKind::CopyBlockInst:
  case SILInstructionKind::CopyBlockWithoutEscapingInst:
  case SILInstructionKind::CopyValueInst:
  case SILInstructionKind::DeinitExistentialAddrInst:
  case SILInstructionKind::DeinitExistentialValueInst:
  case SILInstructionKind::DestroyAddrInst:
  case SILInstructionKind::DestroyValueInst:
  case SILInstructionKind::EndAccessInst:
  case SILInstructionKind::EndApplyInst:
  case SILInstructionKind::EndBorrowArgumentInst:
  case SILInstructionKind::EndBorrowInst:
  case SILInstructionKind::EndUnpairedAccessInst:
  case SILInstructionKind::EndLifetimeInst:
  case SILInstructionKind::ExistentialMetatypeInst:
  case SILInstructionKind::FixLifetimeInst:
  case SILInstructionKind::InitExistentialValueInst:
  case SILInstructionKind::IsUniqueInst:
  case SILInstructionKind::IsEscapingClosureInst:
  case SILInstructionKind::IsUniqueOrPinnedInst:
  case SILInstructionKind::KeyPathInst:
  case SILInstructionKind::OpenExistentialBoxInst:
  case SILInstructionKind::OpenExistentialBoxValueInst:
  case SILInstructionKind::OpenExistentialValueInst:
  case SILInstructionKind::PartialApplyInst:
  case SILInstructionKind::ProjectValueBufferInst:
  case SILInstructionKind::StrongPinInst:
  case SILInstructionKind::YieldInst:
  case SILInstructionKind::UnwindInst:
  case SILInstructionKind::UncheckedOwnershipConversionInst:
  case SILInstructionKind::UncheckedRefCastAddrInst:
  case SILInstructionKind::UnconditionalCheckedCastAddrInst:
  case SILInstructionKind::UnconditionalCheckedCastValueInst:
  case SILInstructionKind::ValueMetatypeInst:
    return;
  }
}

SILBasicBlock::iterator swift::removeBeginAccess(BeginAccessInst *beginAccess) {
  while (!beginAccess->use_empty()) {
    Operand *op = *beginAccess->use_begin();

    // Delete any associated end_access instructions.
    if (auto endAccess = dyn_cast<EndAccessInst>(op->getUser())) {
      endAccess->eraseFromParent();

      // Forward all other uses to the original address.
    } else {
      op->set(beginAccess->getSource());
    }
  }
  return beginAccess->getParent()->erase(beginAccess);
}
