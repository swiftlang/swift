//===--- SILInstruction.h - Instructions for SIL code -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the high-level SILInstruction class used for SIL code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_INSTRUCTION_H
#define SWIFT_SIL_INSTRUCTION_H

#include "swift/AST/Builtins.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/Basic/Compiler.h"
#include "swift/SIL/Consumption.h"
#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILFunctionConventions.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILSuccessor.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {

class DeclRefExpr;
class FloatLiteralExpr;
class FuncDecl;
class IntegerLiteralExpr;
class SILBasicBlock;
class SILBuilder;
class SILDebugLocation;
class SILDebugScope;
class SILFunction;
class SILGlobalVariable;
class SILOpenedArchetypesState;
class SILType;
class SILArgument;
class Stmt;
class StringLiteralExpr;
class Substitution;
class ValueDecl;
class VarDecl;
class FunctionRefInst;

template <typename ImplClass> class SILClonerWithScopes;

// An enum class for SILInstructions that enables exhaustive switches over
// instructions.
enum class SILInstructionKind : std::underlying_type<ValueKind>::type {
#define INST(Id, Parent, TextualName, MemoryBehavior, ReleasingBehavior)       \
  Id = static_cast<std::underlying_type<ValueKind>::type>(ValueKind::Id),
#include "SILNodes.def"
};

/// This is the root class for all instructions that can be used as the contents
/// of a Swift SILBasicBlock.
class SILInstruction : public ValueBase,public llvm::ilist_node<SILInstruction>{
  friend llvm::ilist_traits<SILInstruction>;
  friend llvm::ilist_traits<SILBasicBlock>;

  /// A backreference to the containing basic block.  This is maintained by
  /// ilist_traits<SILInstruction>.
  SILBasicBlock *ParentBB;

  /// This instruction's containing lexical scope and source location
  /// used for debug info and diagnostics.
  SILDebugLocation Location;

  SILInstruction() = delete;
  void operator=(const SILInstruction &) = delete;
  void operator delete(void *Ptr, size_t) SWIFT_DELETE_OPERATOR_DELETED

  /// Check any special state of instructions that are not represented in the
  /// instructions operands/type.
  bool hasIdenticalState(const SILInstruction *RHS) const;

  /// Update this instruction's SILDebugScope. This function should
  /// never be called directly. Use SILBuilder, SILBuilderWithScope or
  /// SILClonerWithScope instead.
  void setDebugScope(SILBuilder &B, const SILDebugScope *DS);

protected:
  SILInstruction(ValueKind Kind, SILDebugLocation DebugLoc,
                 SILType Ty = SILType())
      : ValueBase(Kind, Ty), ParentBB(0), Location(DebugLoc) {}

public:
  /// Instructions should be allocated using a dedicated instruction allocation
  /// function from the ContextTy.
  template <typename ContextTy>
  void *operator new(size_t Bytes, const ContextTy &C,
                     size_t Alignment = alignof(ValueBase)) {
    return C.allocateInst(Bytes, Alignment);
  }

  enum class MemoryBehavior {
    None,
    /// The instruction may read memory.
    MayRead,
    /// \brief The instruction may write to memory.
    MayWrite,
    /// The instruction may read or write memory.
    MayReadWrite,
    /// \brief The instruction may have side effects not captured
    ///        solely by its users. Specifically, it can return,
    ///        release memory, or store. Note, alloc is not considered
    ///        to have side effects because its result/users represent
    ///        its effect.
    MayHaveSideEffects,
  };

  /// Enumeration representing whether the execution of an instruction can
  /// result in memory being released.
  enum class ReleasingBehavior {
    DoesNotRelease,
    MayRelease,
  };

  const SILBasicBlock *getParent() const { return ParentBB; }
  SILBasicBlock *getParent() { return ParentBB; }

  SILFunction *getFunction();
  const SILFunction *getFunction() const;

  SILModule &getModule() const;

  /// This instruction's source location (AST node).
  SILLocation getLoc() const;
  const SILDebugScope *getDebugScope() const;
  SILDebugLocation getDebugLocation() const { return Location; }

  /// Sets the debug location.
  /// Note: Usually it should not be needed to use this function as the location
  /// is already set in when creating an instruction.
  void setDebugLocation(SILDebugLocation Loc) { Location = Loc; }

  /// removeFromParent - This method unlinks 'self' from the containing basic
  /// block, but does not delete it.
  ///
  void removeFromParent();

  /// eraseFromParent - This method unlinks 'self' from the containing basic
  /// block and deletes it.
  ///
  void eraseFromParent();

  /// Unlink this instruction from its current basic block and insert it into
  /// the basic block that Later lives in, right before Later.
  void moveBefore(SILInstruction *Later);

  /// Unlink this instruction from its current basic block and insert it into
  /// the basic block that Earlier lives in, right after Earlier.
  void moveAfter(SILInstruction *Earlier);

  /// \brief Drops all uses that belong to this instruction.
  void dropAllReferences();

  /// \brief Replace all uses of this instruction with Undef.
  ///
  /// TODO: This should be on ValueBase, but ValueBase currently does not have
  /// access to a SILModule. If that ever changes, this method should move to
  /// ValueBase.
  void replaceAllUsesWithUndef();

  /// Return the array of operands for this instruction.
  ArrayRef<Operand> getAllOperands() const;

  /// Return the array of type dependent operands for this instruction.
  ///
  /// Type dependent operands are hidden operands, i.e. not part of the SIL
  /// syntax (although they are printed as "type-defs" in comments).
  /// Their purpose is to establish a def-use relationship between
  ///   -) an instruction/argument which defines a type, e.g. open_existential
  /// and
  ///   -) this instruction, which uses the type, but doesn't use the defining
  ///      instruction as value-operand, e.g. a type in the substitution list.
  ///
  /// Currently there are two kinds of type dependent operands:
  ///
  /// 1. for opened archetypes:
  ///     %o = open_existential_addr %0 : $*P to $*@opened("UUID") P
  ///     %w = witness_method $@opened("UUID") P, ... // type-defs: %o
  ///
  /// 2. for the dynamic self argument:
  ///     sil @foo : $@convention(method) (@thick X.Type) {
  ///     bb0(%0 : $@thick X.Type):
  ///       %a = apply %f<@dynamic_self X>() ... // type-defs: %0
  ///
  /// The type dependent operands are just there to let optimizations know that
  /// there is a dependency between the instruction/argument which defines the
  /// type and the instruction which uses the type.
  ArrayRef<Operand> getTypeDependentOperands() const;

  /// Return the array of mutable operands for this instruction.
  MutableArrayRef<Operand> getAllOperands();

  /// Return the array of mutable type dependent operands for this instruction.
  MutableArrayRef<Operand> getTypeDependentOperands();

  unsigned getNumOperands() const { return getAllOperands().size(); }

  unsigned getNumTypeDependentOperands() const {
    return getTypeDependentOperands().size();
  }

  bool isTypeDependentOperand(unsigned i) const {
    return i >= getNumOperands() - getNumTypeDependentOperands();
  }

  bool isTypeDependentOperand(const Operand &Op) const {
    assert(Op.getUser() == this &&
           "Operand does not belong to a SILInstruction");
    return isTypeDependentOperand(Op.getOperandNumber());
  }

  SILValue getOperand(unsigned Num) const {
    return getAllOperands()[Num].get();
  }
  void setOperand(unsigned Num, SILValue V) { getAllOperands()[Num].set(V); }
  void swapOperands(unsigned Num1, unsigned Num2) {
    getAllOperands()[Num1].swap(getAllOperands()[Num2]);
  }

  MemoryBehavior getMemoryBehavior() const;
  ReleasingBehavior getReleasingBehavior() const;

  /// Returns true if the instruction may release any object.
  bool mayRelease() const;

  /// Returns true if the instruction may release or may read the reference
  /// count of any object.
  bool mayReleaseOrReadRefCount() const;

  /// Can this instruction abort the program in some manner?
  bool mayTrap() const;

  /// Returns true if the given instruction is completely identical to RHS.
  bool isIdenticalTo(const SILInstruction *RHS) const {
    return isIdenticalTo(RHS,
                         [](const SILValue &Op1, const SILValue &Op2) -> bool {
                           return Op1 == Op2; });
  }
  
  /// Returns true if the given instruction is completely identical to RHS,
  /// using \p opEqual to compare operands.
  ///
  template <typename OpCmp>
  bool isIdenticalTo(const SILInstruction *RHS, OpCmp opEqual) const {
    // Quick check if both instructions have the same kind, number of operands,
    // and types. This should filter out most cases.
    if (getKind() != RHS->getKind() ||
        getNumOperands() != RHS->getNumOperands() ||
        getType() != RHS->getType()) {
      return false;
    }
    
    // Check operands.
    for (unsigned i = 0, e = getNumOperands(); i != e; ++i)
      if (!opEqual(getOperand(i), RHS->getOperand(i)))
        return false;

    // Check any special state of instructions that are not represented in the
    // instructions operands/type.
    return hasIdenticalState(RHS);
  }

  /// \brief Returns true if the instruction may have side effects.
  ///
  /// Instructions that store into memory or change retain counts as well as
  /// calls and deallocation instructions are considered to have side effects
  /// that are not visible by merely examining their uses.
  bool mayHaveSideEffects() const;

  /// Returns true if the instruction may write to memory.
  bool mayWriteToMemory() const {
    MemoryBehavior B = getMemoryBehavior();
    return B == MemoryBehavior::MayWrite ||
      B == MemoryBehavior::MayReadWrite ||
      B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if the instruction may read from memory.
  bool mayReadFromMemory() const {
    MemoryBehavior B = getMemoryBehavior();
    return B == MemoryBehavior::MayRead ||
      B == MemoryBehavior::MayReadWrite ||
      B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if the instruction may read from or write to memory.
  bool mayReadOrWriteMemory() const {
    return getMemoryBehavior() != MemoryBehavior::None;
  }

  /// Returns true if the result of this instruction is a pointer to stack
  /// allocated memory. In this case there must be an adjacent deallocating
  /// instruction.
  bool isAllocatingStack() const;

  /// Returns true if this is the deallocation of a stack allocating instruction.
  /// The first operand must be the allocating instruction.
  bool isDeallocatingStack() const;

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_SILInstruction &&
           V->getKind() <= ValueKind::Last_SILInstruction;
  }

  /// Create a new copy of this instruction, which retains all of the operands
  /// and other information of this one.  If an insertion point is specified,
  /// then the new instruction is inserted before the specified point, otherwise
  /// the new instruction is returned without a parent.
  SILInstruction *clone(SILInstruction *InsertPt = nullptr);

  /// Invoke an Instruction's destructor. This dispatches to the appropriate
  /// leaf class destructor for the type of the instruction. This does not
  /// deallocate the instruction.
  static void destroy(SILInstruction *I);

  /// Returns true if the instruction can be duplicated without any special
  /// additional handling. It is important to know this information when
  /// you perform such optimizations like e.g. jump-threading.
  bool isTriviallyDuplicatable() const;

  /// Verify that all operands of this instruction have compatible ownership
  /// with this instruction.
  void verifyOperandOwnership() const;
};

/// Returns the combined behavior of \p B1 and \p B2.
inline SILInstruction::MemoryBehavior
combineMemoryBehavior(SILInstruction::MemoryBehavior B1,
                  SILInstruction::MemoryBehavior B2) {
  // Basically the combined behavior is the maximum of both operands.
  auto Result = std::max(B1, B2);

  // With one exception: MayRead, MayWrite -> MayReadWrite.
  if (Result == SILInstruction::MemoryBehavior::MayWrite &&
        (B1 == SILInstruction::MemoryBehavior::MayRead ||
         B2 == SILInstruction::MemoryBehavior::MayRead))
    return SILInstruction::MemoryBehavior::MayReadWrite;
  return Result;
}

/// Pretty-print the MemoryBehavior.
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                              SILInstruction::MemoryBehavior B);
/// Pretty-print the ReleasingBehavior.
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                              SILInstruction::ReleasingBehavior B);

/// A template base class for instructions that take a single SILValue operand
/// and has no result or a single value result.
template<ValueKind KIND, typename BASE = SILInstruction, bool HAS_RESULT = true>
class UnaryInstructionBase : public BASE {
  // Space for 1 operand.
  FixedOperandList<1> Operands;

  /// Check HAS_RESULT in enable_if predicates by injecting a dependency on
  /// a template argument.
  template<typename X>
  struct has_result {
    enum { value = HAS_RESULT };
  };

public:
  UnaryInstructionBase(SILDebugLocation DebugLoc, SILValue Operand)
      : BASE(KIND, DebugLoc), Operands(this, Operand) {}

  template <typename X = void>
  UnaryInstructionBase(
      SILDebugLocation DebugLoc, SILValue Operand,
      typename std::enable_if<has_result<X>::value, SILType>::type Ty)
      : BASE(KIND, DebugLoc, Ty), Operands(this, Operand) {}

  template <typename X = void, typename... A>
  UnaryInstructionBase(
      SILDebugLocation DebugLoc, SILValue Operand,
      typename std::enable_if<has_result<X>::value, SILType>::type Ty,
      A &&... args)
      : BASE(KIND, DebugLoc, Ty, std::forward<A>(args)...),
        Operands(this, Operand) {}

  SILValue getOperand() const { return Operands[0].get(); }
  void setOperand(SILValue V) { Operands[0].set(V); }

  Operand &getOperandRef() { return Operands[0]; }

  /// getType() is ok if this is known to only have one type.
  template<typename X = void>
  typename std::enable_if<has_result<X>::value, SILType>::type
  getType() const { return ValueBase::getType(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return {};
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return {};
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == KIND;
  }
};

/// A template base class for instructions that take a single regular SILValue
/// operand, a set of type dependent operands and has no result
/// or a single value result. The operands are tail allocated after the
/// instruction. Further trailing data can be allocated as well if
/// TRAILING_TYPES are provided.
template<ValueKind KIND, typename DERIVED,
         typename BASE, bool HAS_RESULT,
         typename... TRAILING_TYPES>
class UnaryInstructionWithTypeDependentOperandsBase :
  public BASE,
  protected llvm::TrailingObjects<DERIVED, Operand, TRAILING_TYPES...> {

  /// Check HAS_RESULT in enable_if predicates by injecting a dependency on
  /// a template argument.
  template<typename X>
  struct has_result {
    enum { value = HAS_RESULT };
  };

protected:
  friend llvm::TrailingObjects<DERIVED, Operand, TRAILING_TYPES...>;

  typedef llvm::TrailingObjects<DERIVED, Operand, TRAILING_TYPES...>
      TrailingObjects;

  using TrailingObjects::totalSizeToAlloc;

  // Total number of operands of this instruction.
  // It is number of type dependent operands + 1.
  unsigned NumOperands;

public:
  // Destruct tail allocated objects.
  ~UnaryInstructionWithTypeDependentOperandsBase() {
    Operand *Operands = &getAllOperands()[0];
    for (unsigned i = 0, end = NumOperands; i < end; ++i) {
      Operands[i].~Operand();
    }
  }

  size_t numTrailingObjects(
    typename TrailingObjects::template OverloadToken<Operand>) const {
    return NumOperands;
  }

  UnaryInstructionWithTypeDependentOperandsBase(
      SILDebugLocation DebugLoc, SILValue Operand,
      ArrayRef<SILValue> TypeDependentOperands)
      : BASE(KIND, DebugLoc), NumOperands(1 + TypeDependentOperands.size()) {
    TrailingOperandsList::InitOperandsList(getAllOperands().begin(), this,
                                           Operand, TypeDependentOperands);
  }

  template <typename X = void>
  UnaryInstructionWithTypeDependentOperandsBase(
      SILDebugLocation DebugLoc, SILValue Operand,
      ArrayRef<SILValue> TypeDependentOperands,
      typename std::enable_if<has_result<X>::value, SILType>::type Ty)
      : BASE(KIND, DebugLoc, Ty),
        NumOperands(1 + TypeDependentOperands.size())
  {
    TrailingOperandsList::InitOperandsList(getAllOperands().begin(), this,
                                           Operand, TypeDependentOperands);
  }

  template <typename X = void, typename... A>
  UnaryInstructionWithTypeDependentOperandsBase(
      SILDebugLocation DebugLoc, SILValue Operand,
      ArrayRef<SILValue> TypeDependentOperands,
      typename std::enable_if<has_result<X>::value, SILType>::type Ty,
      A &&... args)
      : BASE(KIND, DebugLoc, Ty, std::forward<A>(args)...),
        NumOperands(1 + TypeDependentOperands.size())
  {
    TrailingOperandsList::InitOperandsList(getAllOperands().begin(), this,
                                           Operand, TypeDependentOperands);
  }

  unsigned getNumTypeDependentOperands() const {
    return NumOperands - 1;
  }

  SILValue getOperand() const { return getAllOperands()[0].get(); }
  void setOperand(SILValue V) { getAllOperands()[0].set(V); }

  Operand &getOperandRef() { return getAllOperands()[0]; }

  /// getType() is ok if this is known to only have one type.
  template<typename X = void>
  typename std::enable_if<has_result<X>::value, SILType>::type
  getType() const { return ValueBase::getType(); }

  ArrayRef<Operand> getAllOperands() const {
    return {TrailingObjects::template getTrailingObjects<Operand>(),
            static_cast<size_t>(NumOperands)};
  }

  MutableArrayRef<Operand> getAllOperands() {
    return {TrailingObjects::template getTrailingObjects<Operand>(),
            static_cast<size_t>(NumOperands)};
  }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return getAllOperands().slice(1);
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return getAllOperands().slice(1);
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == KIND;
  }
};

/// Holds common debug information about local variables and function
/// arguments that are needed by DebugValueInst, DebugValueAddrInst,
/// AllocStackInst, and AllocBoxInst.
struct SILDebugVariable {
  SILDebugVariable() : Constant(true), ArgNo(0) {}
  SILDebugVariable(bool Constant, unsigned ArgNo)
    : Constant(Constant), ArgNo(ArgNo) {}
  SILDebugVariable(StringRef Name, bool Constant, unsigned ArgNo)
    : Name(Name), Constant(Constant), ArgNo(ArgNo) {}
  StringRef Name;
  bool Constant;
  unsigned ArgNo;
};

/// A DebugVariable where storage for the strings has been
/// tail-allocated following the parent SILInstruction.
class TailAllocatedDebugVariable {
  /// The source function argument position from left to right
  /// starting with 1 or 0 if this is a local variable.
  unsigned ArgNo : 16;
  /// When this is nonzero there is a tail-allocated string storing
  /// variable name present. This typically only happens for
  /// instructions that were created from parsing SIL assembler.
  unsigned NameLength : 15;
  bool Constant : 1;
public:
  TailAllocatedDebugVariable(SILDebugVariable DbgVar, char *buf);

  unsigned getArgNo() const { return ArgNo; }
  void setArgNo(unsigned N) { ArgNo = N; }
  /// Returns the name of the source variable, if it is stored in the
  /// instruction.
  StringRef getName(const char *buf) const;
  bool isLet() const  { return Constant; }

  SILDebugVariable get(VarDecl *VD, const char *buf) const {
    if (VD)
      return {VD->getName().empty() ? "" : VD->getName().str(), VD->isLet(),
              getArgNo()};
    else
      return {getName(buf), isLet(), getArgNo()};
  }
};

//===----------------------------------------------------------------------===//
// Allocation Instructions
//===----------------------------------------------------------------------===//

/// Abstract base class for allocation instructions, like alloc_stack, alloc_box
/// and alloc_ref, etc.
class AllocationInst : public SILInstruction {
protected:
  AllocationInst(ValueKind Kind, SILDebugLocation DebugLoc, SILType Ty)
      : SILInstruction(Kind, DebugLoc, Ty) {}

public:

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_AllocationInst &&
      V->getKind() <= ValueKind::Last_AllocationInst;
  }
};

/// Base class for allocation/deallocation instructions where the allocation
/// can be promoted to the stack.
/// Note that IRGen can still decide to _not_ promote the allocation on the
/// stack.
class StackPromotable {

  /// If true, the allocation can be done on the stack (the final decision is
  /// in IRGen).
  bool OnStack = false;

public:
  StackPromotable(bool OnStack) : OnStack(OnStack) { }

  bool canAllocOnStack() const { return OnStack; }

  void setStackAllocatable() { OnStack = true; }
};

/// AllocStackInst - This represents the allocation of an unboxed (i.e., no
/// reference count) stack memory.  The memory is provided uninitialized.
class AllocStackInst final
    : public AllocationInst,
      private llvm::TrailingObjects<AllocStackInst, Operand, char> {
  friend TrailingObjects;
  friend SILBuilder;

  unsigned NumOperands;
  TailAllocatedDebugVariable VarInfo;

  AllocStackInst(SILDebugLocation Loc, SILType elementType,
                 ArrayRef<SILValue> TypeDependentOperands,
                 SILFunction &F,
                 SILDebugVariable Var);

  static AllocStackInst *create(SILDebugLocation Loc, SILType elementType,
                                SILFunction &F,
                                SILOpenedArchetypesState &OpenedArchetypes,
                                SILDebugVariable Var);

  size_t numTrailingObjects(OverloadToken<Operand>) const {
    return NumOperands;
  }

public:
  ~AllocStackInst() {
    Operand *Operands = getTrailingObjects<Operand>();
    for (unsigned i = 0, end = NumOperands; i < end; ++i) {
      Operands[i].~Operand();
    }
  }

  /// Return the underlying variable declaration associated with this
  /// allocation, or null if this is a temporary allocation.
  VarDecl *getDecl() const;

  /// Return the debug variable information attached to this instruction.
  SILDebugVariable getVarInfo() const {
    return VarInfo.get(getDecl(), getTrailingObjects<char>());
  };
  void setArgNo(unsigned N) { VarInfo.setArgNo(N); }

  /// getElementType - Get the type of the allocated memory (as opposed to the
  /// type of the instruction itself, which will be an address type).
  SILType getElementType() const {
    return getType().getObjectType();
  }

  ArrayRef<Operand> getAllOperands() const {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  MutableArrayRef<Operand> getAllOperands() {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return getAllOperands();
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return getAllOperands();
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocStackInst;
  }
};

/// The base class for AllocRefInst and AllocRefDynamicInst.
class AllocRefInstBase
    : public AllocationInst,
      public StackPromotable {
protected:

  AllocRefInstBase(ValueKind Kind,
                   SILDebugLocation DebugLoc,
                   SILType ObjectType,
                   bool objc, bool canBeOnStack,
                   ArrayRef<SILType> ElementTypes,
                   ArrayRef<SILValue> AllOperands);

  // Number of tail-allocated arrays.
  unsigned short NumTailTypes;

  bool ObjC;

  /// The first NumTailTypes operands are counts for the tail allocated
  /// elements, the remaining operands are opened archetype operands.
  TailAllocatedOperandList<0> Operands;

  SILType *getTypeStorage() {
    return reinterpret_cast<SILType*>(Operands.asArray().end());
  }

  const SILType *getTypeStorage() const {
    return reinterpret_cast<const SILType*>(Operands.asArray().end());
  }

public:
  ArrayRef<SILType> getTailAllocatedTypes() const {
    return {getTypeStorage(), NumTailTypes};
  }

  MutableArrayRef<SILType> getTailAllocatedTypes() {
    return {getTypeStorage(), NumTailTypes};
  }
  
  ArrayRef<Operand> getTailAllocatedCounts() const {
    return getAllOperands().slice(0, NumTailTypes);
  }

  MutableArrayRef<Operand> getTailAllocatedCounts() {
    return getAllOperands().slice(0, NumTailTypes);
  }

  ArrayRef<Operand> getAllOperands() const {
    return Operands.asArray();
  }

  MutableArrayRef<Operand> getAllOperands() {
    return Operands.asArray();
  }
  
  /// Whether to use Objective-C's allocation mechanism (+allocWithZone:).
  bool isObjC() const { return ObjC; }
};

/// AllocRefInst - This represents the primitive allocation of an instance
/// of a reference type. Aside from the reference count, the instance is
/// returned uninitialized.
/// Optionally, the allocated instance contains space for one or more tail-
/// allocated arrays.
class AllocRefInst final : public AllocRefInstBase {
  friend SILBuilder;

  AllocRefInst(SILDebugLocation DebugLoc, SILFunction &F,
               SILType ObjectType,
               bool objc, bool canBeOnStack,
               ArrayRef<SILType> ElementTypes,
               ArrayRef<SILValue> AllOperands)
      : AllocRefInstBase(ValueKind::AllocRefInst, DebugLoc, ObjectType, objc,
                         canBeOnStack, ElementTypes, AllOperands) {
    static_assert(sizeof(AllocRefInst) == sizeof(AllocRefInstBase),
                  "subclass has extra storage");
  }

  static AllocRefInst *create(SILDebugLocation DebugLoc, SILFunction &F,
                              SILType ObjectType,
                              bool objc, bool canBeOnStack,
                              ArrayRef<SILType> ElementTypes,
                              ArrayRef<SILValue> ElementCountOperands,
                              SILOpenedArchetypesState &OpenedArchetypes);

public:
  ArrayRef<Operand> getTypeDependentOperands() const {
    return getAllOperands().slice(NumTailTypes);
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return getAllOperands().slice(NumTailTypes);
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocRefInst;
  }
};

/// AllocRefDynamicInst - This represents the primitive allocation of
/// an instance of a reference type whose runtime type is provided by
/// the given metatype value. Aside from the reference count, the
/// instance is returned uninitialized.
/// Optionally, the allocated instance contains space for one or more tail-
/// allocated arrays.
class AllocRefDynamicInst final : public AllocRefInstBase {
  friend SILBuilder;

  AllocRefDynamicInst(SILDebugLocation DebugLoc,
                      SILType ty,
                      bool objc,
                      ArrayRef<SILType> ElementTypes,
                      ArrayRef<SILValue> AllOperands)
      : AllocRefInstBase(ValueKind::AllocRefDynamicInst, DebugLoc,
                         ty, objc, false,
                         ElementTypes, AllOperands) {
    static_assert(sizeof(AllocRefInst) == sizeof(AllocRefInstBase),
                  "subclass has extra storage");
  }

  static AllocRefDynamicInst *
  create(SILDebugLocation DebugLoc, SILFunction &F,
         SILValue metatypeOperand, SILType ty, bool objc,
         ArrayRef<SILType> ElementTypes,
         ArrayRef<SILValue> ElementCountOperands,
         SILOpenedArchetypesState &OpenedArchetypes);

public:
  SILValue getMetatypeOperand() const {
    return getAllOperands()[NumTailTypes].get();
  }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return getAllOperands().slice(NumTailTypes + 1);
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return getAllOperands().slice(NumTailTypes + 1);
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocRefDynamicInst;
  }
};

/// AllocValueBufferInst - Allocate memory in a value buffer.
class AllocValueBufferInst final
    : public UnaryInstructionWithTypeDependentOperandsBase<
                                  ValueKind::AllocValueBufferInst,
                                  AllocValueBufferInst,
                                  AllocationInst,
                                  true> {
  friend SILBuilder;

  AllocValueBufferInst(SILDebugLocation DebugLoc, SILType valueType,
                       SILValue operand,
                       ArrayRef<SILValue> TypeDependentOperands);

  static AllocValueBufferInst *
  create(SILDebugLocation DebugLoc, SILType valueType, SILValue operand,
         SILFunction &F, SILOpenedArchetypesState &OpenedArchetypes);

public:

  SILType getValueType() const { return getType().getObjectType(); }
};

/// This represents the allocation of a heap box for a Swift value of some type.
/// The instruction returns two values.  The first return value is the object
/// pointer with Builtin.NativeObject type.  The second return value
/// is an address pointing to the contained element. The contained
/// element is uninitialized.
class AllocBoxInst final
    : public AllocationInst,
      private llvm::TrailingObjects<AllocBoxInst, Operand, char> {
  friend TrailingObjects;
  friend SILBuilder;

  unsigned NumOperands;

  TailAllocatedDebugVariable VarInfo;

  AllocBoxInst(SILDebugLocation DebugLoc, CanSILBoxType BoxType,
               ArrayRef<SILValue> TypeDependentOperands, SILFunction &F,
               SILDebugVariable Var);

  static AllocBoxInst *create(SILDebugLocation Loc, CanSILBoxType boxType,
                              SILFunction &F,
                              SILOpenedArchetypesState &OpenedArchetypes,
                              SILDebugVariable Var);

  size_t numTrailingObjects(OverloadToken<Operand>) const {
    return NumOperands;
  }

public:
  ~AllocBoxInst() {
    Operand *Operands = getTrailingObjects<Operand>();
    for (unsigned i = 0, end = NumOperands; i < end; ++i) {
      Operands[i].~Operand();
    }
  }
  
  CanSILBoxType getBoxType() const {
    return getType().castTo<SILBoxType>();
  }

  /// Return the underlying variable declaration associated with this
  /// allocation, or null if this is a temporary allocation.
  VarDecl *getDecl() const;

  /// Return the debug variable information attached to this instruction.
  SILDebugVariable getVarInfo() const {
    return VarInfo.get(getDecl(), getTrailingObjects<char>());
  };

  ArrayRef<Operand> getAllOperands() const {
    return {getTrailingObjects<Operand>(), NumOperands};
  }

  MutableArrayRef<Operand> getAllOperands() {
    return {getTrailingObjects<Operand>(), NumOperands};
  }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return getAllOperands();
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return getAllOperands();
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocBoxInst;
  }
};

/// This represents the allocation of a heap box for an existential container.
/// The instruction returns two values.  The first return value is the owner
/// pointer, which has the existential type.  The second return value
/// is an address pointing to the contained element. The contained
/// value is uninitialized.
class AllocExistentialBoxInst final
    : public AllocationInst,
      private llvm::TrailingObjects<AllocExistentialBoxInst, Operand> {
  friend TrailingObjects;
  friend SILBuilder;
  unsigned NumOperands;
  CanType ConcreteType;
  ArrayRef<ProtocolConformanceRef> Conformances;

  AllocExistentialBoxInst(SILDebugLocation DebugLoc, SILType ExistentialType,
                          CanType ConcreteType,
                          ArrayRef<ProtocolConformanceRef> Conformances,
                          ArrayRef<SILValue> TypeDependentOperands,
                          SILFunction *Parent);

  static AllocExistentialBoxInst *
  create(SILDebugLocation DebugLoc, SILType ExistentialType,
         CanType ConcreteType, ArrayRef<ProtocolConformanceRef> Conformances,
         SILFunction *Parent, SILOpenedArchetypesState &OpenedArchetypes);

public:
  ~AllocExistentialBoxInst() {
    Operand *Operands = getTrailingObjects<Operand>();
    for (unsigned i = 0, end = NumOperands; i < end; ++i) {
      Operands[i].~Operand();
    }
  }

  CanType getFormalConcreteType() const { return ConcreteType; }

  SILType getExistentialType() const { return getType(); }

  ArrayRef<ProtocolConformanceRef> getConformances() const {
    return Conformances;
  }

  ArrayRef<Operand> getAllOperands() const {
    return {getTrailingObjects<Operand>(), NumOperands};
  }

  MutableArrayRef<Operand> getAllOperands() {
    return {getTrailingObjects<Operand>(), NumOperands};
  }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return getAllOperands();
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return getAllOperands();
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocExistentialBoxInst;
  }
};

void *allocateApplyInst(SILFunction &F, size_t size, size_t align);
class PartialApplyInst;

/// ApplyInstBase - An abstract class for different kinds of function
/// application.
template <class Impl, class Base,
          bool IsFullApply = !std::is_same<Impl, PartialApplyInst>::value>
class ApplyInstBase;

// The partial specialization for non-full applies.  Note that the
// partial specialization for full applies inherits from this.
template <class Impl, class Base>
class ApplyInstBase<Impl, Base, false> : public Base {
  enum {
    Callee
  };

  /// The type of the callee with our substitutions applied.
  SILType SubstCalleeType;

  /// The number of tail-allocated substitutions, allocated after the operand
  /// list's tail allocation.
  unsigned NumSubstitutions: 31;

  /// Used for apply_inst instructions: true if the called function has an
  /// error result but is not actually throwing.
  bool NonThrowing: 1;

  /// The number of call arguments as required by the callee.
  unsigned NumCallArguments;

  /// The fixed operand is the callee;  the rest are arguments.
  TailAllocatedOperandList<1> Operands;

  Substitution *getSubstitutionsStorage() {
    return reinterpret_cast<Substitution*>(Operands.asArray().end());
  }

  const Substitution *getSubstitutionsStorage() const {
    return reinterpret_cast<const Substitution*>(Operands.asArray().end());
  }

protected:
  template <class... As>
  ApplyInstBase(ValueKind kind, SILDebugLocation DebugLoc, SILValue callee,
                SILType substCalleeType, SubstitutionList substitutions,
                ArrayRef<SILValue> args,
                ArrayRef<SILValue> TypeDependentOperands,
                As... baseArgs)
      : Base(kind, DebugLoc, baseArgs...), SubstCalleeType(substCalleeType),
        NumSubstitutions(substitutions.size()), NonThrowing(false),
        NumCallArguments(args.size()),
        Operands(this, args, TypeDependentOperands, callee) {
    static_assert(sizeof(Impl) == sizeof(*this),
        "subclass has extra storage, cannot use TailAllocatedOperandList");
    memcpy(getSubstitutionsStorage(), substitutions.begin(),
           sizeof(substitutions[0]) * substitutions.size());
  }

  static void *allocate(SILFunction &F,
                        SubstitutionList substitutions,
                        ArrayRef<SILValue> TypeDependentOperands,
                        ArrayRef<SILValue> args) {
    return allocateApplyInst(
        F, sizeof(Impl) + decltype(Operands)::getExtraSize(
                              args.size() + TypeDependentOperands.size()) +
               sizeof(substitutions[0]) * substitutions.size(),
        alignof(Impl));
  }

  void setNonThrowing(bool isNonThrowing) { NonThrowing = isNonThrowing; }
  
  bool isNonThrowingApply() const { return NonThrowing; }
  
public:
  /// The operand number of the first argument.
  static unsigned getArgumentOperandNumber() { return 1; }

  SILValue getCallee() const { return Operands[Callee].get(); }

  /// Gets the referenced function by looking through partial apply,
  /// convert_function, and thin to thick function until we find a function_ref.
  ///
  /// This is defined out of line to work around incomplete definition
  /// issues. It is at the bottom of the file.
  SILFunction *getCalleeFunction() const;

  /// Gets the referenced function if the callee is a function_ref instruction.
  SILFunction *getReferencedFunction() const {
    if (auto *FRI = dyn_cast<FunctionRefInst>(getCallee()))
      return FRI->getReferencedFunction();
    return nullptr;
  }

  /// Get the type of the callee without the applied substitutions.
  CanSILFunctionType getOrigCalleeType() const {
    return getCallee()->getType().template castTo<SILFunctionType>();
  }
  SILFunctionConventions getOrigCalleeConv() const {
    return SILFunctionConventions(getOrigCalleeType(), this->getModule());
  }

  /// Get the type of the callee with the applied substitutions.
  CanSILFunctionType getSubstCalleeType() const {
    return SubstCalleeType.castTo<SILFunctionType>();
  }
  SILType getSubstCalleeSILType() const {
    return SubstCalleeType;
  }
  SILFunctionConventions getSubstCalleeConv() const {
    return SILFunctionConventions(getSubstCalleeType(), this->getModule());
  }

  bool isCalleeNoReturn() const {
    return getSubstCalleeSILType().isNoReturnFunction();
  }

  bool isCalleeThin() const {
    auto Rep = getSubstCalleeType()->getRepresentation();
    return Rep == FunctionType::Representation::Thin;
  }

  /// True if this application has generic substitutions.
  bool hasSubstitutions() const { return NumSubstitutions != 0; }

  /// The substitutions used to bind the generic arguments of this function.
  MutableArrayRef<Substitution> getSubstitutions() {
    return {getSubstitutionsStorage(), NumSubstitutions};
  }

  SubstitutionList getSubstitutions() const {
    return {getSubstitutionsStorage(), NumSubstitutions};
  }

  /// The arguments passed to this instruction.
  MutableArrayRef<Operand> getArgumentOperands() {
    return Operands.getDynamicAsArray().slice(0, getNumCallArguments());
  }

  ArrayRef<Operand> getArgumentOperands() const {
    return Operands.getDynamicAsArray().slice(0, getNumCallArguments());
  }

  /// The arguments passed to this instruction.
  OperandValueArrayRef getArguments() const {
    return OperandValueArrayRef(
        Operands.getDynamicAsArray().slice(0, getNumCallArguments()));
  }

  /// Returns the number of arguments for this partial apply.
  unsigned getNumArguments() const { return getArguments().size(); }

  Operand &getArgumentRef(unsigned i) {
    return getArgumentOperands()[i];
  }

  /// Return the ith argument passed to this instruction.
  SILValue getArgument(unsigned i) const { return getArguments()[i]; }

  /// Set the ith argument of this instruction.
  void setArgument(unsigned i, SILValue V) {
    return getArgumentOperands()[i].set(V);
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }

  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  unsigned getNumCallArguments() const {
    return NumCallArguments;
  }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return Operands.getDynamicAsArray().slice(NumCallArguments);
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return Operands.getDynamicAsArray().slice(NumCallArguments);
  }
};

/// Given the callee operand of an apply or try_apply instruction,
/// does it have the given semantics?
bool doesApplyCalleeHaveSemantics(SILValue callee, StringRef semantics);

/// The partial specialization of ApplyInstBase for full applications.
/// Adds some methods relating to 'self' and to result types that don't
/// make sense for partial applications.
template <class Impl, class Base>
class ApplyInstBase<Impl, Base, true>
  : public ApplyInstBase<Impl, Base, false> {
  using super = ApplyInstBase<Impl, Base, false>;
protected:
  template <class... As>
  ApplyInstBase(As &&...args)
    : ApplyInstBase<Impl, Base, false>(std::forward<As>(args)...) {}

public:
  using super::getCallee;
  using super::getSubstCalleeType;
  using super::getSubstCalleeConv;
  using super::hasSubstitutions;
  using super::getSubstitutions;
  using super::getNumArguments;
  using super::getArgument;
  using super::getArguments;
  using super::getArgumentOperands;

  /// The collection of following routines wrap the representation difference in
  /// between the self substitution being first, but the self parameter of a
  /// function being last.
  ///
  /// The hope is that this will prevent any future bugs from coming up related
  /// to this.
  ///
  /// Self is always the last parameter, but self substitutions are always
  /// first. The reason to add this method is to wrap that dichotomy to reduce
  /// errors.
  ///
  /// FIXME: Could this be standardized? It has and will lead to bugs. IMHO.
  SILValue getSelfArgument() const {
    assert(hasSelfArgument() && "Must have a self argument");
    assert(getNumArguments() && "Should only be called when Callee has "
           "arguments.");
    return getArgument(getNumArguments()-1);
  }

  Operand &getSelfArgumentOperand() {
    assert(hasSelfArgument() && "Must have a self argument");
    assert(getNumArguments() && "Should only be called when Callee has "
           "arguments.");
    return getArgumentOperands()[getNumArguments()-1];
  }

  void setSelfArgument(SILValue V) {
    assert(hasSelfArgument() && "Must have a self argument");
    assert(getNumArguments() && "Should only be called when Callee has "
                                      "arguments.");
    getArgumentOperands()[getNumArguments() - 1].set(V);
  }

  OperandValueArrayRef getArgumentsWithoutSelf() const {
    assert(hasSelfArgument() && "Must have a self argument");
    assert(getNumArguments() && "Should only be called when Callee has "
           "at least a self parameter.");
    assert(hasSubstitutions() && "Should only be called when Callee has "
           "substitutions.");
    ArrayRef<Operand> ops = this->getArgumentOperands();
    ArrayRef<Operand> opsWithoutSelf = ArrayRef<Operand>(&ops[0],
                                                         ops.size()-1);
    return OperandValueArrayRef(opsWithoutSelf);
  }

  SILArgumentConvention getArgumentConvention(unsigned index) const {
    return getSubstCalleeConv().getSILArgumentConvention(index);
  }

  Optional<SILResultInfo> getSingleResult() const {
    auto SubstCallee = getSubstCalleeType();
    if (SubstCallee->getNumAllResults() != 1)
      return None;
    return SubstCallee->getSingleResult();
  }

  bool hasIndirectResults() const {
    return getSubstCalleeConv().hasIndirectSILResults();
  }
  unsigned getNumIndirectResults() const {
    return getSubstCalleeConv().getNumIndirectSILResults();
  }

  bool hasSelfArgument() const {
    return getSubstCalleeType()->hasSelfParam();
  }

  bool hasGuaranteedSelfArgument() const {
    auto C = getSubstCalleeType()->getSelfParameter().getConvention();
    return C == ParameterConvention::Direct_Guaranteed;
  }

  OperandValueArrayRef getIndirectSILResults() const {
    return getArguments().slice(0, getNumIndirectResults());
  }

  OperandValueArrayRef getArgumentsWithoutIndirectResults() const {
    return getArguments().slice(getNumIndirectResults());
  }

  bool hasSemantics(StringRef semanticsString) const {
    return doesApplyCalleeHaveSemantics(getCallee(), semanticsString);
  }
};

/// ApplyInst - Represents the full application of a function value.
class ApplyInst : public ApplyInstBase<ApplyInst, SILInstruction> {
  friend SILBuilder;

  ApplyInst(SILDebugLocation DebugLoc, SILValue Callee,
            SILType SubstCalleeType, SILType ReturnType,
            SubstitutionList Substitutions,
            ArrayRef<SILValue> Args,
            ArrayRef<SILValue> TypeDependentOperands,
            bool isNonThrowing);

  static ApplyInst *create(SILDebugLocation DebugLoc, SILValue Callee,
                           SILType SubstCalleeType, SILType ReturnType,
                           SubstitutionList Substitutions,
                           ArrayRef<SILValue> Args, bool isNonThrowing,
                           SILFunction &F,
                           SILOpenedArchetypesState &OpenedArchetypes);

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::ApplyInst;
  }
  
  /// Returns true if the called function has an error result but is not actually
  /// throwing an error.
  bool isNonThrowing() const {
    return isNonThrowingApply();
  }
};

/// PartialApplyInst - Represents the creation of a closure object by partial
/// application of a function value.
class PartialApplyInst
    : public ApplyInstBase<PartialApplyInst, SILInstruction> {
  friend SILBuilder;

  PartialApplyInst(SILDebugLocation DebugLoc, SILValue Callee,
                   SILType SubstCalleeType,
                   SubstitutionList Substitutions,
                   ArrayRef<SILValue> Args,
                   ArrayRef<SILValue> TypeDependentOperands,
                   SILType ClosureType);

  static PartialApplyInst *create(SILDebugLocation DebugLoc, SILValue Callee,
                                  SILType SubstCalleeType,
                                  SubstitutionList Substitutions,
                                  ArrayRef<SILValue> Args, SILType ClosureType,
                                  SILFunction &F,
                                  SILOpenedArchetypesState &OpenedArchetypes);

public:
  /// Return the result function type of this partial apply.
  CanSILFunctionType getFunctionType() const {
    return getType().castTo<SILFunctionType>();
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::PartialApplyInst;
  }
};

//===----------------------------------------------------------------------===//
// Literal instructions.
//===----------------------------------------------------------------------===//

/// Abstract base class for literal instructions.
class LiteralInst : public SILInstruction {
protected:
  LiteralInst(ValueKind Kind, SILDebugLocation DebugLoc, SILType Ty)
      : SILInstruction(Kind, DebugLoc, Ty) {}

public:

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_LiteralInst &&
      V->getKind() <= ValueKind::Last_LiteralInst;
  }
};

/// FunctionRefInst - Represents a reference to a SIL function.
class FunctionRefInst : public LiteralInst {
  friend SILBuilder;

  SILFunction *Function;
  /// Construct a FunctionRefInst.
  ///
  /// \param DebugLoc  The location of the reference.
  /// \param F         The function being referenced.
  FunctionRefInst(SILDebugLocation DebugLoc, SILFunction *F);

public:
  ~FunctionRefInst();

  /// Return the referenced function.
  SILFunction *getReferencedFunction() const { return Function; }

  void dropReferencedFunction();

  CanSILFunctionType getFunctionType() const {
    return getType().castTo<SILFunctionType>();
  }
  SILFunctionConventions getConventions() const {
    return SILFunctionConventions(getFunctionType(), getModule());
  }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::FunctionRefInst;
  }
};

/// Represents an invocation of builtin functionality provided by the code
/// generator.
class BuiltinInst : public SILInstruction {
  friend SILBuilder;

  /// The name of the builtin to invoke.
  Identifier Name;
  
  /// The number of tail-allocated substitutions, allocated after the operand
  /// list's tail allocation.
  unsigned NumSubstitutions;
  
  /// The value arguments to the builtin.
  TailAllocatedOperandList<0> Operands;
  
  Substitution *getSubstitutionsStorage() {
    return reinterpret_cast<Substitution*>(Operands.asArray().end());
  }
  const Substitution *getSubstitutionsStorage() const {
    return reinterpret_cast<const Substitution*>(Operands.asArray().end());
  }

  BuiltinInst(SILDebugLocation DebugLoc, Identifier Name, SILType ReturnType,
              SubstitutionList Substitutions, ArrayRef<SILValue> Args);

  static BuiltinInst *create(SILDebugLocation DebugLoc, Identifier Name,
                             SILType ReturnType,
                             SubstitutionList Substitutions,
                             ArrayRef<SILValue> Args, SILFunction &F);

public:
  /// Return the name of the builtin operation.
  Identifier getName() const { return Name; }
  void setName(Identifier I) { Name = I; }
  
  /// \brief Looks up the llvm intrinsic ID and type for the builtin function.
  ///
  /// \returns Returns llvm::Intrinsic::not_intrinsic if the function is not an
  /// intrinsic. The particular intrinsic functions which correspond to the
  /// returned value are defined in llvm/Intrinsics.h.
  const IntrinsicInfo &getIntrinsicInfo() const;
  
  /// \brief Looks up the lazily cached identification for the builtin function.
  const BuiltinInfo &getBuiltinInfo() const;

  /// \brief Looks up the llvm intrinsic ID of this builtin. Returns None if
  /// this is not an intrinsic.
  llvm::Optional<llvm::Intrinsic::ID> getIntrinsicID() const {
    auto I = getIntrinsicInfo();
    if (I.ID == llvm::Intrinsic::not_intrinsic)
      return None;
    return I.ID;
  }

  /// \brief Looks up the BuiltinKind of this builtin. Returns None if this is
  /// not a builtin.
  llvm::Optional<BuiltinValueKind> getBuiltinKind() const {
    auto I = getBuiltinInfo();
    if (I.ID == BuiltinValueKind::None)
      return None;
    return I.ID;
  }

  /// True if this builtin application has substitutions, which represent type
  /// parameters to the builtin.
  bool hasSubstitutions() const {
    return NumSubstitutions != 0;
  }

  /// Return the type parameters to the builtin.
  SubstitutionList getSubstitutions() const {
    return {getSubstitutionsStorage(), NumSubstitutions};
  }
  /// Return the type parameters to the builtin.
  MutableArrayRef<Substitution> getSubstitutions() {
    return {getSubstitutionsStorage(), NumSubstitutions};
  }
  
  /// The arguments to the builtin.
  ArrayRef<Operand> getAllOperands() const {
    return Operands.asArray();
  }
  /// The arguments to the builtin.
  MutableArrayRef<Operand> getAllOperands() {
    return Operands.asArray();
  }
  /// The arguments to the builtin.
  OperandValueArrayRef getArguments() const {
    return Operands.asValueArray();
  }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::BuiltinInst;
  }
};
  
/// Initializes a SIL global variable. Only valid once, before any
/// usages of the global via GlobalAddrInst.
class AllocGlobalInst : public SILInstruction {
  friend SILBuilder;

  SILGlobalVariable *Global;

  AllocGlobalInst(SILDebugLocation DebugLoc, SILGlobalVariable *Global);

public:
  // FIXME: This constructor should be private but is currently used
  //        in the SILParser.

  /// Create a placeholder instruction with an unset global reference.
  AllocGlobalInst(SILDebugLocation DebugLoc);

  /// Return the referenced global variable.
  SILGlobalVariable *getReferencedGlobal() const { return Global; }
  
  void setReferencedGlobal(SILGlobalVariable *v) { Global = v; }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocGlobalInst;
  }
};

/// Gives the address of a SIL global variable. Only valid after an
/// AllocGlobalInst.
class GlobalAddrInst : public LiteralInst {
  friend SILBuilder;
  
  SILGlobalVariable *Global;

  GlobalAddrInst(SILDebugLocation DebugLoc, SILGlobalVariable *Global);

public:
  // FIXME: This constructor should be private but is currently used
  //        in the SILParser.

  /// Create a placeholder instruction with an unset global reference.
  GlobalAddrInst(SILDebugLocation DebugLoc, SILType Ty);
  
  /// Return the referenced global variable.
  SILGlobalVariable *getReferencedGlobal() const { return Global; }
  
  void setReferencedGlobal(SILGlobalVariable *v) { Global = v; }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::GlobalAddrInst;
  }
};

/// IntegerLiteralInst - Encapsulates an integer constant, as defined originally
/// by an IntegerLiteralExpr.
class IntegerLiteralInst final : public LiteralInst,
    private llvm::TrailingObjects<IntegerLiteralInst, llvm::integerPart> {
  friend TrailingObjects;
  friend SILBuilder;

  unsigned numBits;

  IntegerLiteralInst(SILDebugLocation Loc, SILType Ty, const APInt &Value);

  static IntegerLiteralInst *create(IntegerLiteralExpr *E,
                                    SILDebugLocation Loc, SILFunction &B);
  static IntegerLiteralInst *create(SILDebugLocation Loc, SILType Ty,
                                    intmax_t Value, SILFunction &B);
  static IntegerLiteralInst *create(SILDebugLocation Loc, SILType Ty,
                                    const APInt &Value, SILFunction &B);

public:
  /// getValue - Return the APInt for the underlying integer literal.
  APInt getValue() const;

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::IntegerLiteralInst;
  }
};

/// FloatLiteralInst - Encapsulates a floating point constant, as defined
/// originally by a FloatLiteralExpr.
class FloatLiteralInst final : public LiteralInst,
    private llvm::TrailingObjects<FloatLiteralInst, llvm::integerPart> {
  friend TrailingObjects;
  friend SILBuilder;

  unsigned numBits;

  FloatLiteralInst(SILDebugLocation Loc, SILType Ty, const APInt &Bits);

  static FloatLiteralInst *create(FloatLiteralExpr *E, SILDebugLocation Loc,
                                  SILFunction &B);
  static FloatLiteralInst *create(SILDebugLocation Loc, SILType Ty,
                                  const APFloat &Value, SILFunction &B);

public:
  /// \brief Return the APFloat for the underlying FP literal.
  APFloat getValue() const;

  /// \brief Return the bitcast representation of the FP literal as an APInt.
  APInt getBits() const;

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::FloatLiteralInst;
  }
};

/// StringLiteralInst - Encapsulates a string constant, as defined originally by
/// a StringLiteralExpr.  This produces the address of the string data as a
/// Builtin.RawPointer.
class StringLiteralInst final : public LiteralInst,
    private llvm::TrailingObjects<StringLiteralInst, char> {
  friend TrailingObjects;
  friend SILBuilder;

public:
  enum class Encoding {
    UTF8,
    UTF16,
    /// UTF-8 encoding of an Objective-C selector.
    ObjCSelector,
  };

private:
  unsigned Length;
  Encoding TheEncoding;

  StringLiteralInst(SILDebugLocation DebugLoc, StringRef text,
                    Encoding encoding, SILType ty);

  static StringLiteralInst *create(SILDebugLocation DebugLoc, StringRef Text,
                                   Encoding encoding, SILFunction &F);

public:
  /// getValue - Return the string data for the literal, in UTF-8.
  StringRef getValue() const {
    return {getTrailingObjects<char>(), Length};
  }

  /// getEncoding - Return the desired encoding of the text.
  Encoding getEncoding() const { return TheEncoding; }

  /// getCodeUnitCount - Return encoding-based length of the string
  /// literal in code units.
  uint64_t getCodeUnitCount();

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::StringLiteralInst;
  }
};

//===----------------------------------------------------------------------===//
// Memory instructions.
//===----------------------------------------------------------------------===//

/// StringLiteralInst::Encoding hashes to its underlying integer representation.
static inline llvm::hash_code hash_value(StringLiteralInst::Encoding E) {
  return llvm::hash_value(size_t(E));
}

// *NOTE* When serializing, we can only represent up to 4 values here. If more
// qualifiers are added, SIL serialization must be updated.
enum class LoadOwnershipQualifier {
  Unqualified, Take, Copy, Trivial
};

/// LoadInst - Represents a load from a memory location.
class LoadInst
  : public UnaryInstructionBase<ValueKind::LoadInst>
{
  friend SILBuilder;

  LoadOwnershipQualifier OwnershipQualifier;

  /// Constructs a LoadInst.
  ///
  /// \param DebugLoc The location of the expression that caused the load.
  ///
  /// \param LValue The SILValue representing the lvalue (address) to
  ///        use for the load.
  LoadInst(SILDebugLocation DebugLoc, SILValue LValue,
           LoadOwnershipQualifier Q = LoadOwnershipQualifier::Unqualified)
      : UnaryInstructionBase(DebugLoc, LValue,
                             LValue->getType().getObjectType()),
        OwnershipQualifier(Q) {}

public:
  LoadOwnershipQualifier getOwnershipQualifier() const {
    return OwnershipQualifier;
  }
};

// *NOTE* When serializing, we can only represent up to 4 values here. If more
// qualifiers are added, SIL serialization must be updated.
enum class StoreOwnershipQualifier {
  Unqualified, Init, Assign, Trivial
};

/// StoreInst - Represents a store from a memory location.
class StoreInst : public SILInstruction {
  friend SILBuilder;

private:
  FixedOperandList<2> Operands;
  StoreOwnershipQualifier OwnershipQualifier;

  StoreInst(SILDebugLocation DebugLoc, SILValue Src, SILValue Dest,
            StoreOwnershipQualifier Qualifier);

public:
  enum {
    /// the value being stored
    Src,
    /// the lvalue being stored to
    Dest
  };

  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::StoreInst;
  }

  StoreOwnershipQualifier getOwnershipQualifier() const {
    return OwnershipQualifier;
  }
};

/// Represents a load of a borrowed value. Must be paired with an end_borrow
/// instruction in its use-def list.
class LoadBorrowInst : public UnaryInstructionBase<ValueKind::LoadBorrowInst> {
  friend class SILBuilder;

  LoadBorrowInst(SILDebugLocation DebugLoc, SILValue LValue)
      : UnaryInstructionBase(DebugLoc, LValue,
                             LValue->getType().getObjectType()) {}
};

/// Represents the begin scope of a borrowed value. Must be paired with an
/// end_borrow instruction in its use-def list.
class BeginBorrowInst
    : public UnaryInstructionBase<ValueKind::BeginBorrowInst> {
  friend class SILBuilder;

  BeginBorrowInst(SILDebugLocation DebugLoc, SILValue LValue)
      : UnaryInstructionBase(DebugLoc, LValue,
                             LValue->getType().getObjectType()) {}
};

/// Represents a store of a borrowed value into an address. Returns the borrowed
/// address. Must be paired with an end_borrow in its use-def list.
class StoreBorrowInst : public SILInstruction {
  friend class SILBuilder;

public:
  enum {
    /// The source of the value being borrowed.
    Src,
    /// The destination of the borrowed value.
    Dest
  };

private:
  FixedOperandList<2> Operands;
  StoreBorrowInst(SILDebugLocation DebugLoc, SILValue Src, SILValue Dest);

public:
  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::StoreBorrowInst;
  }
};

/// Represents the end of a borrow scope for a value or address from another
/// value or address.
///
/// The semantics of the instruction here is that the "dest" SILValue can not be
/// used after this instruction and the "src" SILValue must stay alive up to
/// EndBorrowInst.
class EndBorrowInst : public SILInstruction {
  friend class SILBuilder;

public:
  enum {
    /// The borrowed value.
    BorrowedValue,
    /// The original value that was borrowed from.
    OriginalValue
  };

private:
  FixedOperandList<2> Operands;
  EndBorrowInst(SILDebugLocation DebugLoc, SILValue BorrowedValue,
                SILValue OriginalValue);

public:
  SILValue getBorrowedValue() const { return Operands[BorrowedValue].get(); }

  SILValue getOriginalValue() const { return Operands[OriginalValue].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::EndBorrowInst;
  }
};

/// Represents the end of a borrow scope for an argument. The reason why this is
/// separate from end_borrow is that an argument is not borrowed from a
/// specific SSA value. Instead it is borrowed from potentially many different
/// incoming values.
class EndBorrowArgumentInst
    : public UnaryInstructionBase<ValueKind::EndBorrowArgumentInst> {
  friend class SILBuilder;

  EndBorrowArgumentInst(SILDebugLocation DebugLoc, SILArgument *Arg);
};

/// AssignInst - Represents an abstract assignment to a memory location, which
/// may either be an initialization or a store sequence.  This is only valid in
/// Raw SIL.
class AssignInst : public SILInstruction {
  friend SILBuilder;

  enum {
    /// the value being stored
    Src,
    /// the lvalue being stored to
    Dest
  };
  FixedOperandList<2> Operands;

  AssignInst(SILDebugLocation DebugLoc, SILValue Src, SILValue Dest);

public:

  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  bool isUnownedAssign() const {
    return getDest()->getType().getObjectType().is<UnownedStorageType>();
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AssignInst;
  }
};

/// Abstract base class for instructions that mark storage as uninitialized.

/// Indicates that a memory location is uninitialized at
/// this point and needs to be initialized by the end of the function and before
/// any escape point for this instruction.  This is only valid in Raw SIL.
class MarkUninitializedInst
  : public UnaryInstructionBase<ValueKind::MarkUninitializedInst> {
  friend SILBuilder;

public:
  /// This enum captures what the mark_uninitialized instruction is designating.
  enum Kind {
    /// Var designates the start of a normal variable live range.
    Var,

    /// RootSelf designates "self" in a struct, enum, or root class.
    RootSelf,

    /// DerivedSelf designates "self" in a derived (non-root) class.
    DerivedSelf,

    /// DerivedSelfOnly designates "self" in a derived (non-root)
    /// class whose stored properties have already been initialized.
    DerivedSelfOnly,

    /// DelegatingSelf designates "self" on a struct, enum, or class
    /// in a delegating constructor (one that calls self.init).
    DelegatingSelf,
  };
private:
  Kind ThisKind;

  MarkUninitializedInst(SILDebugLocation DebugLoc, SILValue Address, Kind K)
      : UnaryInstructionBase(DebugLoc, Address, Address->getType()),
        ThisKind(K) {}

public:

  Kind getKind() const { return ThisKind; }

  bool isVar() const { return ThisKind == Var; }
  bool isRootSelf() const {
    return ThisKind == RootSelf;
  }
  bool isDerivedClassSelf() const {
    return ThisKind == DerivedSelf;
  }
  bool isDerivedClassSelfOnly() const {
    return ThisKind == DerivedSelfOnly;
  }
  bool isDelegatingSelf() const {
    return ThisKind == DelegatingSelf;
  }
};

/// MarkUninitializedBehaviorInst - Indicates that a logical property
/// is uninitialized at this point and needs to be initialized by the end of the
/// function and before any escape point for this instruction. Assignments
/// to the property trigger the behavior's `init` or `set` logic based on
/// the logical initialization state of the property.
///
/// This is only valid in Raw SIL.
class MarkUninitializedBehaviorInst final : public SILInstruction,
      private llvm::TrailingObjects<MarkUninitializedBehaviorInst, Substitution>
{
  friend SILBuilder;
  friend TrailingObjects;

  FixedOperandList<4> Operands;
  unsigned NumInitStorageSubstitutions, NumSetterSubstitutions;
  
  enum {
    // The initialization function for the storage.
    InitStorageFunc,
    // Address of the behavior storage being initialized.
    Storage,
    // The setter function for the behavior property.
    SetterFunc,
    // The address or reference to the parent `self` being initialized.
    Self,
  };
  
  size_t numTrailingObjects(OverloadToken<Substitution>) {
    return NumInitStorageSubstitutions + NumSetterSubstitutions;
  }
  
  MarkUninitializedBehaviorInst(SILDebugLocation DebugLoc,
                                SILValue InitStorage,
                                SubstitutionList InitStorageSubs,
                                SILValue Storage,
                                SILValue Setter,
                                SubstitutionList SetterSubs,
                                SILValue Self,
                                SILType Ty);
  
  static MarkUninitializedBehaviorInst *create(SILModule &M,
                                         SILDebugLocation DebugLoc,
                                         SILValue InitStorage,
                                         SubstitutionList InitStorageSubs,
                                         SILValue Storage,
                                         SILValue Setter,
                                         SubstitutionList SetterSubs,
                                         SILValue Self,
                                         SILType Ty);

public:
  SILValue getInitStorageFunc() const {
    return Operands[InitStorageFunc].get();
  }
  SubstitutionList getInitStorageSubstitutions() const {
    return {getTrailingObjects<Substitution>(), NumInitStorageSubstitutions};
  }
  SILValue getStorage() const {
    return Operands[Storage].get();
  }

  SILValue getSetterFunc() const {
    return Operands[SetterFunc].get();
  }
  SubstitutionList getSetterSubstitutions() const {
    return {getTrailingObjects<Substitution>() + NumInitStorageSubstitutions,
            NumSetterSubstitutions};
  }
  SILValue getSelf() const {
    return Operands[Self].get();
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::MarkUninitializedBehaviorInst;
  }
};

/// MarkFunctionEscape - Represents the escape point of set of variables due to
/// a function definition which uses the variables.  This is only valid in Raw
/// SIL.
class MarkFunctionEscapeInst : public SILInstruction {
  friend SILBuilder;

  TailAllocatedOperandList<0> Operands;

  /// Private constructor.  Because this is variadic, object creation goes
  /// through 'create()'.
  MarkFunctionEscapeInst(SILDebugLocation DebugLoc,
                         ArrayRef<SILValue> Elements);

  /// Construct a MarkFunctionEscapeInst.
  static MarkFunctionEscapeInst *create(SILDebugLocation DebugLoc,
                                        ArrayRef<SILValue> Elements,
                                        SILFunction &F);

public:
  /// The elements referenced by this instruction.
  MutableArrayRef<Operand> getElementOperands() {
    return Operands.getDynamicAsArray();
  }

  /// The elements referenced by this instruction.
  OperandValueArrayRef getElements() const {
    return Operands.getDynamicValuesAsArray();
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::MarkFunctionEscapeInst;
  }
};

/// Define the start or update to a symbolic variable value (for loadable
/// types).
class DebugValueInst final
  : public UnaryInstructionBase<ValueKind::DebugValueInst>,
    private llvm::TrailingObjects<DebugValueInst, char> {
  friend TrailingObjects;
  friend SILBuilder;
  TailAllocatedDebugVariable VarInfo;

  DebugValueInst(SILDebugLocation DebugLoc, SILValue Operand,
                 SILDebugVariable Var);
  static DebugValueInst *create(SILDebugLocation DebugLoc, SILValue Operand,
                                SILModule &M, SILDebugVariable Var);

  size_t numTrailingObjects(OverloadToken<char>) const { return 1; }

public:
  /// Return the underlying variable declaration that this denotes,
  /// or null if we don't have one.
  VarDecl *getDecl() const;
  /// Return the debug variable information attached to this instruction.
  SILDebugVariable getVarInfo() const {
    return VarInfo.get(getDecl(), getTrailingObjects<char>());
  }
};

/// Define the start or update to a symbolic variable value (for address-only
/// types) .
class DebugValueAddrInst final
  : public UnaryInstructionBase<ValueKind::DebugValueAddrInst>,
    private llvm::TrailingObjects<DebugValueAddrInst, char> {
  friend TrailingObjects;
  friend SILBuilder;
  TailAllocatedDebugVariable VarInfo;

  DebugValueAddrInst(SILDebugLocation DebugLoc, SILValue Operand,
                     SILDebugVariable Var);
  static DebugValueAddrInst *create(SILDebugLocation DebugLoc,
                                    SILValue Operand, SILModule &M,
                                    SILDebugVariable Var);

public:
  /// Return the underlying variable declaration that this denotes,
  /// or null if we don't have one.
  VarDecl *getDecl() const;
  /// Return the debug variable information attached to this instruction.
  SILDebugVariable getVarInfo() const {
    return VarInfo.get(getDecl(), getTrailingObjects<char>());
  };
};


/// An abstract class representing a load from some kind of reference storage.
template <ValueKind K>
class LoadReferenceInstBase : public UnaryInstructionBase<K> {
  static SILType getResultType(SILType operandTy) {
    assert(operandTy.isAddress() && "loading from non-address operand?");
    auto refType = cast<ReferenceStorageType>(operandTy.getSwiftRValueType());
    return SILType::getPrimitiveObjectType(refType.getReferentType());
  }

  unsigned IsTake : 1; // FIXME: pack this somewhere

protected:
  LoadReferenceInstBase(SILDebugLocation loc, SILValue lvalue, IsTake_t isTake)
    : UnaryInstructionBase<K>(loc, lvalue, getResultType(lvalue->getType())),
      IsTake(unsigned(isTake)) {
  }

public:
  IsTake_t isTake() const { return IsTake_t(IsTake); }
};

/// An abstract class representing a store to some kind of reference storage.
template <ValueKind K>
class StoreReferenceInstBase : public SILInstruction {
  enum { Src, Dest };
  FixedOperandList<2> Operands;
  unsigned IsInitializationOfDest : 1; // FIXME: pack this somewhere
protected:
  StoreReferenceInstBase(SILDebugLocation loc, SILValue src, SILValue dest,
                         IsInitialization_t isInit)
    : SILInstruction(K, loc), Operands(this, src, dest),
      IsInitializationOfDest(unsigned(isInit)) {
  }

public:
  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  IsInitialization_t isInitializationOfDest() const {
    return IsInitialization_t(IsInitializationOfDest);
  }
  void setIsInitializationOfDest(IsInitialization_t I) {
    IsInitializationOfDest = (bool)I;
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == K;
  }
};

/// Represents a load from a @weak memory location.
class LoadWeakInst
  : public LoadReferenceInstBase<ValueKind::LoadWeakInst>
{
  friend SILBuilder;

  /// \param loc The location of the expression that caused the load.
  /// \param lvalue The SILValue representing the address to
  ///        use for the load.
  LoadWeakInst(SILDebugLocation loc, SILValue lvalue, IsTake_t isTake)
    : LoadReferenceInstBase(loc, lvalue, isTake) {}
};

/// Represents a store to a @weak memory location.
class StoreWeakInst
  : public StoreReferenceInstBase<ValueKind::StoreWeakInst>
{
  friend SILBuilder;

  StoreWeakInst(SILDebugLocation loc, SILValue src, SILValue dest,
                IsInitialization_t isInit)
    : StoreReferenceInstBase(loc, src, dest, isInit) {}
};

/// Represents a load from an @unowned memory location.
///
/// This is only required for address-only unowned references; for loadable
/// unowned references, it's better to use a load and a strong_retain_unowned.
class LoadUnownedInst
  : public LoadReferenceInstBase<ValueKind::LoadUnownedInst>
{
  friend SILBuilder;

  /// \param loc The location of the expression that caused the load.
  /// \param lvalue The SILValue representing the address to
  ///        use for the load.
  LoadUnownedInst(SILDebugLocation loc, SILValue lvalue, IsTake_t isTake)
    : LoadReferenceInstBase(loc, lvalue, isTake) {}
};

/// Represents a store to an @unowned memory location.
///
/// This is only required for address-only unowned references; for loadable
/// unowned references, it's better to use a ref_to_unowned and a store.
class StoreUnownedInst
  : public StoreReferenceInstBase<ValueKind::StoreUnownedInst>
{
  friend SILBuilder;

  StoreUnownedInst(SILDebugLocation loc, SILValue src, SILValue dest,
                   IsInitialization_t isInit)
    : StoreReferenceInstBase(loc, src, dest, isInit) {}
};

/// CopyAddrInst - Represents a copy from one memory location to another. This
/// is similar to:
///   %1 = load %src
///   store %1 to %dest
/// but a copy instruction must be used for address-only types.
class CopyAddrInst : public SILInstruction {
  friend SILBuilder;

public:
  enum {
    /// The lvalue being loaded from.
    Src,

    /// The lvalue being stored to.
    Dest
  };

private:
  // FIXME: compress storage

  /// IsTakeOfSrc - True if ownership will be taken from the value at the source
  /// memory location.
  unsigned IsTakeOfSrc : 1;

  /// IsInitializationOfDest - True if this is the initialization of the
  /// uninitialized destination memory location.
  unsigned IsInitializationOfDest : 1;

  FixedOperandList<2> Operands;

  CopyAddrInst(SILDebugLocation DebugLoc, SILValue Src, SILValue Dest,
               IsTake_t isTakeOfSrc, IsInitialization_t isInitializationOfDest);

public:
  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  void setSrc(SILValue V) { Operands[Src].set(V); }
  void setDest(SILValue V) { Operands[Dest].set(V); }

  IsTake_t isTakeOfSrc() const { return IsTake_t(IsTakeOfSrc); }
  IsInitialization_t isInitializationOfDest() const {
    return IsInitialization_t(IsInitializationOfDest);
  }

  void setIsTakeOfSrc(IsTake_t T) {
    IsTakeOfSrc = (bool)T;
  }
  void setIsInitializationOfDest(IsInitialization_t I) {
    IsInitializationOfDest = (bool)I;
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::CopyAddrInst;
  }
};

/// BindMemoryInst -
/// "bind_memory %0 : $Builtin.RawPointer, %1 : $Builtin.Word to $T"
/// Binds memory at the raw pointer %0 to type $T with enough capacity
/// to hold $1 values.
class BindMemoryInst final :
    public SILInstruction,
    protected llvm::TrailingObjects<BindMemoryInst, Operand> {

  typedef llvm::TrailingObjects<BindMemoryInst, Operand> TrailingObjects;
  friend TrailingObjects;
  using TrailingObjects::totalSizeToAlloc;

  friend SILBuilder;

  enum { BaseOperIdx, IndexOperIdx, NumFixedOpers };

  SILType BoundType;

  // Fixed operands + opened archetype operands.
  unsigned NumOperands;

  static BindMemoryInst *create(
    SILDebugLocation Loc, SILValue Base, SILValue Index, SILType BoundType,
    SILFunction &F, SILOpenedArchetypesState &OpenedArchetypes);

  BindMemoryInst(SILDebugLocation Loc, SILValue Base, SILValue Index,
                 SILType BoundType,
                 ArrayRef<SILValue> TypeDependentOperands);

public:
  // Destruct tail allocated objects.
  ~BindMemoryInst() {
    Operand *Operands = &getAllOperands()[0];
    for (unsigned i = 0, end = NumOperands; i < end; ++i) {
      Operands[i].~Operand();
    }
  }

  SILValue getBase() const { return getAllOperands()[BaseOperIdx].get(); }

  SILValue getIndex() const { return getAllOperands()[IndexOperIdx].get(); }

  SILType getBoundType() const { return BoundType ; }

  // Implement llvm::TrailingObjects.
  size_t numTrailingObjects(
    typename TrailingObjects::template OverloadToken<Operand>) const {
    return NumOperands;
  }

  ArrayRef<Operand> getAllOperands() const {
    return {TrailingObjects::template getTrailingObjects<Operand>(),
            static_cast<size_t>(NumOperands)};
  }

  MutableArrayRef<Operand> getAllOperands() {
    return {TrailingObjects::template getTrailingObjects<Operand>(),
            static_cast<size_t>(NumOperands)};
  }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return getAllOperands().slice(2);
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return getAllOperands().slice(2);
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::BindMemoryInst;
  }
};

//===----------------------------------------------------------------------===//
// Conversion instructions.
//===----------------------------------------------------------------------===//

/// ConversionInst - Abstract class representing instructions that convert
/// values.
///
class ConversionInst : public SILInstruction {
protected:
  ConversionInst(ValueKind Kind, SILDebugLocation DebugLoc, SILType Ty)
      : SILInstruction(Kind, DebugLoc, Ty) {}

public:
  /// All conversion instructions take the converted value, whose reference
  /// identity is expected to be preserved through the conversion chain, as their
  /// first operand. Some instructions may take additional operands that do not
  /// affect the reference identity.
  SILValue getConverted() const { return getOperand(0); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_ConversionInst &&
      V->getKind() <= ValueKind::Last_ConversionInst;
  }
};

/// ConvertFunctionInst - Change the type of a function value without
/// affecting how it will codegen.
class ConvertFunctionInst
  : public UnaryInstructionBase<ValueKind::ConvertFunctionInst, ConversionInst>
{
  friend SILBuilder;

  ConvertFunctionInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// ThinFunctionToPointerInst - Convert a thin function pointer to a
/// Builtin.RawPointer.
class ThinFunctionToPointerInst
  : public UnaryInstructionBase<ValueKind::ThinFunctionToPointerInst,
                                ConversionInst>
{
  friend SILBuilder;

  ThinFunctionToPointerInst(SILDebugLocation DebugLoc, SILValue operand,
                            SILType ty)
      : UnaryInstructionBase(DebugLoc, operand, ty) {}
};

/// PointerToThinFunctionInst - Convert a Builtin.RawPointer to a thin
/// function pointer.
class PointerToThinFunctionInst
  : public UnaryInstructionBase<ValueKind::PointerToThinFunctionInst,
                                ConversionInst>
{
  friend SILBuilder;

  PointerToThinFunctionInst(SILDebugLocation DebugLoc, SILValue operand,
                            SILType ty)
      : UnaryInstructionBase(DebugLoc, operand, ty) {}
};

/// UpcastInst - Perform a conversion of a class instance to a supertype.
class UpcastInst
  : public UnaryInstructionBase<ValueKind::UpcastInst, ConversionInst>
{
  friend SILBuilder;

  UpcastInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// AddressToPointerInst - Convert a SIL address to a Builtin.RawPointer value.
class AddressToPointerInst
  : public UnaryInstructionBase<ValueKind::AddressToPointerInst, ConversionInst>
{
  friend SILBuilder;

  AddressToPointerInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// PointerToAddressInst - Convert a Builtin.RawPointer value to a SIL address.
class PointerToAddressInst
  : public UnaryInstructionBase<ValueKind::PointerToAddressInst, ConversionInst>
{
  friend SILBuilder;

  bool IsStrict, IsInvariant;

  PointerToAddressInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty,
                       bool IsStrict, bool IsInvariant)
    : UnaryInstructionBase(DebugLoc, Operand, Ty),
      IsStrict(IsStrict), IsInvariant(IsInvariant) {}

public:
  /// Whether the returned address adheres to strict aliasing.
  /// If true, then the type of each memory access dependent on
  /// this address must be consistent with the memory's bound type.
  bool isStrict() const { return IsStrict; }
  /// Whether the returned address is invariant.
  /// If true, then loading from an address derived from this pointer always
  /// produces the same value.
  bool isInvariant() const { return IsInvariant; }
};

/// Convert a heap object reference to a different type without any runtime
/// checks.
class UncheckedRefCastInst final
  : public UnaryInstructionWithTypeDependentOperandsBase<
                                ValueKind::UncheckedRefCastInst,
                                UncheckedRefCastInst,
                                ConversionInst,
                                /* HAS_RESULT */ true>
{
  friend SILBuilder;

  UncheckedRefCastInst(SILDebugLocation DebugLoc, SILValue Operand,
                       ArrayRef<SILValue> TypeDependentOperands, SILType Ty)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Operand,
                                               TypeDependentOperands, Ty) {}
  static UncheckedRefCastInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty,
         SILFunction &F, SILOpenedArchetypesState &OpenedArchetypes);
};

/// Converts a heap object reference to a different type without any runtime
/// checks. This is a variant of UncheckedRefCast that works on address types,
/// thus encapsulates an implicit load and take of the reference followed by a
/// store and initialization of a new reference.
class UncheckedRefCastAddrInst : public SILInstruction {
public:
  enum {
    /// the value being stored
    Src,
    /// the lvalue being stored to
    Dest
  };

private:
  FixedOperandList<2> Operands;
  CanType SourceType;
  CanType TargetType;
public:
  UncheckedRefCastAddrInst(SILDebugLocation Loc, SILValue src, CanType srcType,
                           SILValue dest, CanType targetType);

  CastConsumptionKind getConsumptionKind() const {
    return CastConsumptionKind::TakeAlways;
  }

  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  /// Returns the formal type of the source value.
  CanType getSourceType() const { return SourceType; }

  /// Returns the formal target type.
  CanType getTargetType() const { return TargetType; }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::UncheckedRefCastAddrInst;
  }
};

class UncheckedAddrCastInst final
  : public UnaryInstructionWithTypeDependentOperandsBase<
                                ValueKind::UncheckedAddrCastInst,
                                UncheckedAddrCastInst,
                                ConversionInst,
                                true>
{
  friend SILBuilder;

  UncheckedAddrCastInst(SILDebugLocation DebugLoc, SILValue Operand,
                        ArrayRef<SILValue> TypeDependentOperands, SILType Ty)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Operand,
                                               TypeDependentOperands, Ty) {}
  static UncheckedAddrCastInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty,
         SILFunction &F, SILOpenedArchetypesState &OpenedArchetypes);
};

/// Convert a value's binary representation to a trivial type of the same size.
class UncheckedTrivialBitCastInst final
  : public UnaryInstructionWithTypeDependentOperandsBase<
                                ValueKind::UncheckedTrivialBitCastInst,
                                UncheckedTrivialBitCastInst,
                                ConversionInst,
                                true>
{
  friend SILBuilder;

  UncheckedTrivialBitCastInst(SILDebugLocation DebugLoc, SILValue Operand,
                              ArrayRef<SILValue> TypeDependentOperands,
                              SILType Ty)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Operand,
                                               TypeDependentOperands, Ty) {}

  static UncheckedTrivialBitCastInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty,
         SILFunction &F, SILOpenedArchetypesState &OpenedArchetypes);
};
  
/// Bitwise copy a value into another value of the same size or smaller.
class UncheckedBitwiseCastInst final
  : public UnaryInstructionWithTypeDependentOperandsBase<
                                ValueKind::UncheckedBitwiseCastInst,
                                UncheckedBitwiseCastInst,
                                ConversionInst,
                                true>
{
  friend SILBuilder;

  UncheckedBitwiseCastInst(SILDebugLocation DebugLoc, SILValue Operand,
                           ArrayRef<SILValue> TypeDependentOperands,
                           SILType Ty)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Operand,
                                               TypeDependentOperands, Ty) {}
  static UncheckedBitwiseCastInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty,
         SILFunction &F, SILOpenedArchetypesState &OpenedArchetypes);
};

/// Build a Builtin.BridgeObject from a heap object reference by bitwise-or-ing
/// in bits from a word.
class RefToBridgeObjectInst : public ConversionInst {
  friend SILBuilder;

  FixedOperandList<2> Operands;
  RefToBridgeObjectInst(SILDebugLocation DebugLoc, SILValue ConvertedValue,
                        SILValue MaskValue, SILType BridgeObjectTy)
      : ConversionInst(ValueKind::RefToBridgeObjectInst, DebugLoc,
                       BridgeObjectTy),
        Operands(this, ConvertedValue, MaskValue) {}

public:
  
  SILValue getBitsOperand() const { return Operands[1].get(); }
  
  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::RefToBridgeObjectInst;
  }
};

/// Extract the heap object reference from a BridgeObject.
class BridgeObjectToRefInst
  : public UnaryInstructionBase<ValueKind::BridgeObjectToRefInst,
                                ConversionInst>
{
  friend SILBuilder;

  BridgeObjectToRefInst(SILDebugLocation DebugLoc, SILValue Operand,
                        SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// Retrieve the bit pattern of a BridgeObject.
class BridgeObjectToWordInst
  : public UnaryInstructionBase<ValueKind::BridgeObjectToWordInst,
                                ConversionInst>
{
  friend SILBuilder;

  BridgeObjectToWordInst(SILDebugLocation DebugLoc, SILValue Operand,
                         SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// RefToRawPointer - Convert a reference type to a Builtin.RawPointer.
class RefToRawPointerInst
  : public UnaryInstructionBase<ValueKind::RefToRawPointerInst, ConversionInst>
{
  friend SILBuilder;

  RefToRawPointerInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// RawPointerToRefInst - Convert a Builtin.RawPointer to a reference type.
class RawPointerToRefInst
  : public UnaryInstructionBase<ValueKind::RawPointerToRefInst, ConversionInst>
{
  friend SILBuilder;

  RawPointerToRefInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};
  
/// RefToUnownedInst - Given a value of a reference type,
/// convert it to an unowned reference.
///
/// This does nothing at runtime; it just changes the formal type.
class RefToUnownedInst
  : public UnaryInstructionBase<ValueKind::RefToUnownedInst, ConversionInst>
{
  friend SILBuilder;

  RefToUnownedInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// UnownedToRefInst - Given a value of an @unowned type,
/// convert it to the underlying reference type.
///
/// This does nothing at runtime; it just changes the formal type.
class UnownedToRefInst
  : public UnaryInstructionBase<ValueKind::UnownedToRefInst, ConversionInst>
{
  friend SILBuilder;

  UnownedToRefInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// RefToUnmanagedInst - Given a value of a reference type,
/// convert it to an unmanaged reference.
///
/// This does nothing at runtime; it just changes the formal type.
class RefToUnmanagedInst
  : public UnaryInstructionBase<ValueKind::RefToUnmanagedInst, ConversionInst>
{
  friend SILBuilder;

  RefToUnmanagedInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// UnmanagedToRefInst - Given a value of an unmanaged reference type,
/// convert it to the underlying reference type.
///
/// This does nothing at runtime; it just changes the formal type.
class UnmanagedToRefInst
  : public UnaryInstructionBase<ValueKind::UnmanagedToRefInst, ConversionInst>
{
  friend SILBuilder;

  UnmanagedToRefInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// ThinToThickFunctionInst - Given a thin function reference, adds a null
/// context to convert the value to a thick function type.
class ThinToThickFunctionInst
  : public UnaryInstructionBase<ValueKind::ThinToThickFunctionInst,
                                ConversionInst>
{
  friend SILBuilder;

  ThinToThickFunctionInst(SILDebugLocation DebugLoc, SILValue Operand,
                          SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}

public:
  /// Return the callee of the thin_to_thick_function.
  ///
  /// This is not technically necessary, but from a symmetry perspective it
  /// makes sense to follow the lead of partial_apply which also creates
  /// closures.
  SILValue getCallee() const { return getOperand(); }
};

/// Given a thick metatype value, produces an Objective-C metatype
/// value.
class ThickToObjCMetatypeInst
  : public UnaryInstructionBase<ValueKind::ThickToObjCMetatypeInst,
                                ConversionInst>
{
  friend SILBuilder;

  ThickToObjCMetatypeInst(SILDebugLocation DebugLoc, SILValue Operand,
                          SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// Given an Objective-C metatype value, produces a thick metatype
/// value.
class ObjCToThickMetatypeInst
  : public UnaryInstructionBase<ValueKind::ObjCToThickMetatypeInst,
                                ConversionInst>
{
  friend SILBuilder;

  ObjCToThickMetatypeInst(SILDebugLocation DebugLoc, SILValue Operand,
                          SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// Given an Objective-C metatype value, convert it to an AnyObject value.
class ObjCMetatypeToObjectInst
  : public UnaryInstructionBase<ValueKind::ObjCMetatypeToObjectInst,
                                ConversionInst>
{
  friend SILBuilder;

  ObjCMetatypeToObjectInst(SILDebugLocation DebugLoc, SILValue Operand,
                           SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// Given an Objective-C existential metatype value, convert it to an AnyObject
/// value.
class ObjCExistentialMetatypeToObjectInst
  : public UnaryInstructionBase<ValueKind::ObjCExistentialMetatypeToObjectInst,
                                ConversionInst>
{
  friend SILBuilder;

  ObjCExistentialMetatypeToObjectInst(SILDebugLocation DebugLoc,
                                      SILValue Operand, SILType Ty)
      : UnaryInstructionBase(DebugLoc, Operand, Ty) {}
};

/// Return the Objective-C Protocol class instance for a protocol.
class ObjCProtocolInst : public SILInstruction
{
  friend SILBuilder;

  ProtocolDecl *Proto;
  ObjCProtocolInst(SILDebugLocation DebugLoc, ProtocolDecl *Proto, SILType Ty)
      : SILInstruction(ValueKind::ObjCProtocolInst, DebugLoc, Ty),
        Proto(Proto) {}

public:
  ProtocolDecl *getProtocol() const { return Proto; }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::ObjCProtocolInst;
  }
};

/// Test that an address or reference type is not null.
class IsNonnullInst : public UnaryInstructionBase<ValueKind::IsNonnullInst> {
  friend SILBuilder;

  IsNonnullInst(SILDebugLocation DebugLoc, SILValue Operand, SILType BoolTy)
      : UnaryInstructionBase(DebugLoc, Operand, BoolTy) {}
};
  

/// Perform an unconditional checked cast that aborts if the cast fails.
class UnconditionalCheckedCastInst final
  : public UnaryInstructionWithTypeDependentOperandsBase<
                                ValueKind::UnconditionalCheckedCastInst,
                                UnconditionalCheckedCastInst,
                                ConversionInst,
                                true>
{
  friend SILBuilder;

  UnconditionalCheckedCastInst(SILDebugLocation DebugLoc, SILValue Operand,
                               ArrayRef<SILValue> TypeDependentOperands,
                               SILType DestTy)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Operand,
                                               TypeDependentOperands,
                                               DestTy) {}

  static UnconditionalCheckedCastInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType DestTy,
         SILFunction &F, SILOpenedArchetypesState &OpenedArchetypes);
};

/// Perform an unconditional checked cast that aborts if the cast fails.
/// The result of the checked cast is left in the destination address.
class UnconditionalCheckedCastAddrInst : public SILInstruction
{
  friend SILBuilder;

  enum {
    /// the value being stored
    Src,
    /// the lvalue being stored to
    Dest
  };
  FixedOperandList<2> Operands;
  CastConsumptionKind ConsumptionKind;
  CanType SourceType;
  CanType TargetType;

  UnconditionalCheckedCastAddrInst(SILDebugLocation Loc,
                                   CastConsumptionKind consumption,
                                   SILValue src, CanType sourceType,
                                   SILValue dest, CanType targetType);

public:
  CastConsumptionKind getConsumptionKind() const { return ConsumptionKind; }

  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  /// Returns the formal type of the source value.
  CanType getSourceType() const { return SourceType; }

  /// Returns the formal target type.
  CanType getTargetType() const { return TargetType; }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::UnconditionalCheckedCastAddrInst;
  }
};

/// Perform an unconditional checked cast that aborts if the cast fails.
/// The result of the checked cast is left in the destination.
class UnconditionalCheckedCastValueInst final
    : public UnaryInstructionWithTypeDependentOperandsBase<
          ValueKind::UnconditionalCheckedCastValueInst,
          UnconditionalCheckedCastValueInst, ConversionInst, true> {
  friend SILBuilder;

  UnconditionalCheckedCastValueInst(SILDebugLocation DebugLoc, SILValue Operand,
                                    ArrayRef<SILValue> TypeDependentOperands,
                                    SILType DestTy)
      : UnaryInstructionWithTypeDependentOperandsBase(
            DebugLoc, Operand, TypeDependentOperands, DestTy) {}

  static UnconditionalCheckedCastValueInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType DestTy,
         SILFunction &F, SILOpenedArchetypesState &OpenedArchetypes);
};

/// StructInst - Represents a constructed loadable struct.
class StructInst : public SILInstruction {
  friend SILBuilder;

  TailAllocatedOperandList<0> Operands;

  /// Because of the storage requirements of StructInst, object
  /// creation goes through 'create()'.
  StructInst(SILDebugLocation DebugLoc, SILType Ty,
             ArrayRef<SILValue> Elements);

  /// Construct a StructInst.
  static StructInst *create(SILDebugLocation DebugLoc, SILType Ty,
                            ArrayRef<SILValue> Elements, SILFunction &F);

public:
  /// The elements referenced by this StructInst.
  MutableArrayRef<Operand> getElementOperands() {
    return Operands.getDynamicAsArray();
  }

  /// The elements referenced by this StructInst.
  OperandValueArrayRef getElements() const {
    return Operands.getDynamicValuesAsArray();
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  SILValue getFieldValue(const VarDecl *V) const {
    return getOperandForField(V)->get();
  }

  /// Return the Operand associated with the given VarDecl.
  const Operand *getOperandForField(const VarDecl *V) const {
    return const_cast<StructInst*>(this)->getOperandForField(V);
  }

  Operand *getOperandForField(const VarDecl *V) {
    // If V is null or is computed, there is no operand associated with it.
    assert(V && V->hasStorage() &&
           "getOperandForField only works with stored fields");

    StructDecl *S = getStructDecl();

    NominalTypeDecl::StoredPropertyRange Range = S->getStoredProperties();
    unsigned Index = 0;
    for (auto I = Range.begin(), E = Range.end(); I != E; ++I, ++Index)
      if (V == *I)
        return &getAllOperands()[Index];

    // Did not find a matching VarDecl, return nullptr.
    return nullptr;
  }

  /// Search the operands of this struct for a unique non-trivial field. If we
  /// find it, return it. Otherwise return SILValue().
  SILValue getUniqueNonTrivialFieldValue() {
    SILModule &Mod = getModule();
    ArrayRef<Operand> Ops = getAllOperands();

    Optional<unsigned> Index;
    // For each operand...
    for (unsigned i = 0, e = Ops.size(); i != e; ++i) {
      // If the operand is not trivial...
      if (!Ops[i].get()->getType().isTrivial(Mod)) {
        // And we have not found an Index yet, set index to i and continue.
        if (!Index.hasValue()) {
          Index = i;
          continue;
        }

        // Otherwise, we have two values that are non-trivial. Bail.
        return SILValue();
      }
    }

    // If we did not find an index, return an empty SILValue.
    if (!Index.hasValue())
      return SILValue();

    // Otherwise, return the value associated with index.
    return Ops[Index.getValue()].get();
  }

  StructDecl *getStructDecl() const {
    auto s = getType().getStructOrBoundGenericStruct();
    assert(s && "A struct should always have a StructDecl associated with it");
    return s;
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::StructInst;
  }
};

/// RefCountingInst - An abstract class of instructions which
/// manipulate the reference count of their object operand.
class RefCountingInst : public SILInstruction {
public:
  /// The atomicity of a reference counting operation to be used.
  enum class Atomicity : bool {
    /// Atomic reference counting operations should be used.
    Atomic,
    /// Non-atomic reference counting operations can be used.
    NonAtomic,
  };
protected:
  Atomicity atomicity;
protected:
  RefCountingInst(ValueKind Kind, SILDebugLocation DebugLoc)
      : SILInstruction(Kind, DebugLoc), atomicity(Atomicity::Atomic) {}

  RefCountingInst(ValueKind Kind, SILDebugLocation DebugLoc, SILType Type)
      : SILInstruction(Kind, DebugLoc, Type), atomicity(Atomicity::Atomic) {}

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_RefCountingInst &&
           V->getKind() <= ValueKind::Last_RefCountingInst;
  }

  void setAtomicity(Atomicity flag) { atomicity = flag; }
  void setNonAtomic() { atomicity = Atomicity::NonAtomic; }
  void setAtomic() { atomicity = Atomicity::Atomic; }
  Atomicity getAtomicity() const { return atomicity; }
  bool isNonAtomic() const { return atomicity == Atomicity::NonAtomic; }
  bool isAtomic() const { return atomicity == Atomicity::Atomic; }
};

/// RetainValueInst - Copies a loadable value.
class RetainValueInst : public UnaryInstructionBase<ValueKind::RetainValueInst,
                                                    RefCountingInst,
                                                    /*HasValue*/ false> {
  friend SILBuilder;

  RetainValueInst(SILDebugLocation DebugLoc, SILValue operand,
                  Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, operand) {
    setAtomicity(atomicity);
  }
};

/// ReleaseValueInst - Destroys a loadable value.
class ReleaseValueInst : public UnaryInstructionBase<ValueKind::ReleaseValueInst,
                                                     RefCountingInst,
                                                     /*HasValue*/ false> {
  friend SILBuilder;

  ReleaseValueInst(SILDebugLocation DebugLoc, SILValue operand,
                   Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, operand) {
    setAtomicity(atomicity);
  }
};

/// Copies a loadable value in an unmanaged, unbalanced way. Only meant for use
/// in ownership qualified SIL. Please do not use this EVER unless you are
/// implementing a part of the stdlib called Unmanaged.
class UnmanagedRetainValueInst
    : public UnaryInstructionBase<ValueKind::UnmanagedRetainValueInst,
                                  RefCountingInst,
                                  /*HasValue*/ false> {
  friend SILBuilder;

  UnmanagedRetainValueInst(SILDebugLocation DebugLoc, SILValue operand,
                           Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, operand) {
    setAtomicity(atomicity);
  }
};

/// Destroys a loadable value in an unmanaged, unbalanced way. Only meant for
/// use in ownership qualified SIL. Please do not use this EVER unless you are
/// implementing a part of the stdlib called Unmanaged.
class UnmanagedReleaseValueInst
    : public UnaryInstructionBase<ValueKind::UnmanagedReleaseValueInst,
                                  RefCountingInst,
                                  /*HasValue*/ false> {
  friend SILBuilder;

  UnmanagedReleaseValueInst(SILDebugLocation DebugLoc, SILValue operand,
                            Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, operand) {
    setAtomicity(atomicity);
  }
};

/// Transfers ownership of a loadable value to the current autorelease
/// pool. Unmanaged, so it is ignored from an ownership balancing perspective.
class UnmanagedAutoreleaseValueInst
                  : public UnaryInstructionBase<ValueKind::UnmanagedAutoreleaseValueInst,
                                                RefCountingInst,
                                                /*HasValue*/ false> {
  friend SILBuilder;

  UnmanagedAutoreleaseValueInst(SILDebugLocation DebugLoc, SILValue operand,
                                Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, operand) {
    setAtomicity(atomicity);
  }
};

/// Transfers ownership of a loadable value to the current autorelease pool.
class AutoreleaseValueInst
                  : public UnaryInstructionBase<ValueKind::AutoreleaseValueInst,
                                                RefCountingInst,
                                                /*HasValue*/ false> {
  friend SILBuilder;

  AutoreleaseValueInst(SILDebugLocation DebugLoc, SILValue operand,
                       Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, operand) {
    setAtomicity(atomicity);
  }
};

/// SetDeallocatingInst - Sets the operand in deallocating state.
///
/// This is the same operation what's done by a strong_release immediately
/// before it calls the deallocator of the object.
class SetDeallocatingInst
                 : public UnaryInstructionBase<ValueKind::SetDeallocatingInst,
                                                RefCountingInst,
                                                /*HasValue*/ false> {
  friend SILBuilder;

  SetDeallocatingInst(SILDebugLocation DebugLoc, SILValue operand,
                      Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, operand) {
    setAtomicity(atomicity);
  }
};

/// StrongPinInst - Ensure that the operand is retained and pinned, if
/// not by this operation then by some enclosing pin.
///
/// Transformations must not do anything which reorders pin and unpin
/// operations.  (This should generally be straightforward, as pin and
/// unpin may be conservatively assumed to have arbitrary
/// side-effects.)
class StrongPinInst
  : public UnaryInstructionBase<ValueKind::StrongPinInst, RefCountingInst,
                                /*HasResult*/ true>
{
  friend SILBuilder;

  StrongPinInst(SILDebugLocation DebugLoc, SILValue operand,
                Atomicity atomicity);
};

/// StrongUnpinInst - Given that the operand is the result of a
/// strong_pin instruction, unpin it.
class StrongUnpinInst
  : public UnaryInstructionBase<ValueKind::StrongUnpinInst, RefCountingInst,
                                /*HasResult*/ false>
{
  friend SILBuilder;

  StrongUnpinInst(SILDebugLocation DebugLoc, SILValue operand,
                  Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, operand) {
    setAtomicity(atomicity);
  }
};

/// TupleInst - Represents a constructed loadable tuple.
class TupleInst : public SILInstruction {
  friend SILBuilder;

  TailAllocatedOperandList<0> Operands;

  /// Because of the storage requirements of TupleInst, object
  /// creation goes through 'create()'.
  TupleInst(SILDebugLocation DebugLoc, SILType Ty,
            ArrayRef<SILValue> Elements);

  /// Construct a TupleInst.
  static TupleInst *create(SILDebugLocation DebugLoc, SILType Ty,
                           ArrayRef<SILValue> Elements, SILFunction &F);

public:
  /// The elements referenced by this TupleInst.
  MutableArrayRef<Operand> getElementOperands() {
    return Operands.getDynamicAsArray();
  }

  /// The elements referenced by this TupleInst.
  OperandValueArrayRef getElements() const {
    return Operands.getDynamicValuesAsArray();
  }

  /// Return the i'th value referenced by this TupleInst.
  SILValue getElement(unsigned i) const {
    return getElements()[i];
  }

  unsigned getElementIndex(Operand *operand) {
    assert(operand->getUser() == this);
    return operand->getOperandNumber();
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::TupleInst;
  }

  TupleType *getTupleType() const {
    return getType().getSwiftRValueType()->castTo<TupleType>();
  }

  /// Search the operands of this tuple for a unique non-trivial elt. If we find
  /// it, return it. Otherwise return SILValue().
  SILValue getUniqueNonTrivialElt() {
    SILModule &Mod = getModule();
    ArrayRef<Operand> Ops = getAllOperands();

    Optional<unsigned> Index;
    // For each operand...
    for (unsigned i = 0, e = Ops.size(); i != e; ++i) {
      // If the operand is not trivial...
      if (!Ops[i].get()->getType().isTrivial(Mod)) {
        // And we have not found an Index yet, set index to i and continue.
        if (!Index.hasValue()) {
          Index = i;
          continue;
        }

        // Otherwise, we have two values that are non-trivial. Bail.
        return SILValue();
      }
    }

    // If we did not find an index, return an empty SILValue.
    if (!Index.hasValue())
      return SILValue();

    // Otherwise, return the value associated with index.
    return Ops[Index.getValue()].get();
  }
};

/// Represents a loadable enum constructed from one of its
/// elements.
class EnumInst : public SILInstruction {
  friend SILBuilder;

  Optional<FixedOperandList<1>> OptionalOperand;
  EnumElementDecl *Element;

  EnumInst(SILDebugLocation DebugLoc, SILValue Operand,
           EnumElementDecl *Element, SILType ResultTy)
      : SILInstruction(ValueKind::EnumInst, DebugLoc, ResultTy),
        Element(Element) {
    if (Operand) {
      OptionalOperand.emplace(this, Operand);
    }
  }

public:
  EnumElementDecl *getElement() const { return Element; }

  bool hasOperand() const { return OptionalOperand.hasValue(); }
  SILValue getOperand() const { return OptionalOperand->asValueArray()[0]; }

  Operand &getOperandRef() { return OptionalOperand->asArray()[0]; }

  ArrayRef<Operand> getAllOperands() const {
    return OptionalOperand ? OptionalOperand->asArray() : ArrayRef<Operand>{};
  }

  MutableArrayRef<Operand> getAllOperands() {
    return OptionalOperand
      ? OptionalOperand->asArray() : MutableArrayRef<Operand>{};
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::EnumInst;
  }
};

/// Unsafely project the data for an enum case out of an enum without checking
/// the tag.
class UncheckedEnumDataInst
  : public UnaryInstructionBase<ValueKind::UncheckedEnumDataInst>
{
  friend SILBuilder;

  EnumElementDecl *Element;

  UncheckedEnumDataInst(SILDebugLocation DebugLoc, SILValue Operand,
                        EnumElementDecl *Element, SILType ResultTy)
      : UnaryInstructionBase(DebugLoc, Operand, ResultTy), Element(Element) {}

public:
  EnumElementDecl *getElement() const { return Element; }

  EnumDecl *getEnumDecl() const {
    auto *E = getOperand()->getType().getEnumOrBoundGenericEnum();
    assert(E && "Operand of unchecked_enum_data must be of enum type");
    return E;
  }

  unsigned getElementNo() const {
    unsigned i = 0;
    for (EnumElementDecl *E : getEnumDecl()->getAllElements()) {
      if (E == Element)
        return i;
      ++i;
    }
    llvm_unreachable("An unchecked_enum_data's enumdecl should have at least "
                     "on element, the element that is being extracted");
  }
};

/// Projects the address of the data for a case inside an uninitialized enum in
/// order to initialize the payload for that case.
class InitEnumDataAddrInst
  : public UnaryInstructionBase<ValueKind::InitEnumDataAddrInst>
{
  friend SILBuilder;

  EnumElementDecl *Element;

  InitEnumDataAddrInst(SILDebugLocation DebugLoc, SILValue Operand,
                       EnumElementDecl *Element, SILType ResultTy)
      : UnaryInstructionBase(DebugLoc, Operand, ResultTy), Element(Element) {}

public:
  EnumElementDecl *getElement() const { return Element; }
};

/// InjectEnumAddrInst - Tags an enum as containing a case. The data for
/// that case, if any, must have been written into the enum first.
class InjectEnumAddrInst
  : public UnaryInstructionBase<ValueKind::InjectEnumAddrInst,
                                SILInstruction,
                                /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  EnumElementDecl *Element;

  InjectEnumAddrInst(SILDebugLocation DebugLoc, SILValue Operand,
                     EnumElementDecl *Element)
      : UnaryInstructionBase(DebugLoc, Operand), Element(Element) {}

public:
  EnumElementDecl *getElement() const { return Element; }
};

/// Invalidate an enum value and take ownership of its payload data
/// without moving it in memory.
class UncheckedTakeEnumDataAddrInst
  : public UnaryInstructionBase<ValueKind::UncheckedTakeEnumDataAddrInst>
{
  friend SILBuilder;

  EnumElementDecl *Element;

  UncheckedTakeEnumDataAddrInst(SILDebugLocation DebugLoc, SILValue Operand,
                                EnumElementDecl *Element, SILType ResultTy)
      : UnaryInstructionBase(DebugLoc, Operand, ResultTy), Element(Element) {}

public:
  EnumElementDecl *getElement() const { return Element; }

  EnumDecl *getEnumDecl() const {
    auto *E = getOperand()->getType().getEnumOrBoundGenericEnum();
    assert(E && "Operand of unchecked_take_enum_data_addr must be of enum"
                " type");
    return E;
  }

  unsigned getElementNo() const {
    unsigned i = 0;
    for (EnumElementDecl *E : getEnumDecl()->getAllElements()) {
      if (E == Element)
        return i;
      ++i;
    }
    llvm_unreachable(
        "An unchecked_enum_data_addr's enumdecl should have at least "
        "on element, the element that is being extracted");
  }
};

// Base class of all select instructions like select_enum, select_value, etc.
// The template parameter represents a type of case values to be compared
// with the operand of a select instruction.
template <class Derived, class T>
class SelectInstBase : public SILInstruction {
protected:
  unsigned NumCases : 31;
  unsigned HasDefault : 1;

  /// The first operand is the operand of select_xxx instruction. The rest of
  /// the operands are the case values and results of a select instruction.
  TailAllocatedOperandList<1> Operands;

public:
  SelectInstBase(ValueKind kind, SILDebugLocation DebugLoc, SILType type,
                 unsigned numCases, bool hasDefault,
                 ArrayRef<SILValue> operands, SILValue operand)
      : SILInstruction(kind, DebugLoc, type), NumCases(numCases),
        HasDefault(hasDefault), Operands(this, operands, operand) {}

  SILValue getOperand() const { return Operands[0].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  std::pair<T, SILValue> getCase(unsigned i) {
    return static_cast<const Derived *>(this)->getCase(i);
  }

  unsigned getNumCases() const { return NumCases; }

  bool hasDefault() const { return HasDefault; }

  SILValue getDefaultResult() const {
    return static_cast<const Derived *>(this)->getDefaultResult();
  }
};

/// Common base class for the select_enum and select_enum_addr instructions,
/// which select one of a set of possible results based on the case of an enum.
class SelectEnumInstBase
    : public SelectInstBase<SelectEnumInstBase, EnumElementDecl *> {
  // Tail-allocated after the operands is an array of `NumCases`
  // EnumElementDecl* pointers, referencing the case discriminators for each
  // operand.
  
  EnumElementDecl **getCaseBuf() {
    return reinterpret_cast<EnumElementDecl**>(Operands.asArray().end());
  }
  EnumElementDecl * const* getCaseBuf() const {
    return reinterpret_cast<EnumElementDecl* const*>(Operands.asArray().end());
  }

protected:
  SelectEnumInstBase(
      ValueKind Kind, SILDebugLocation DebugLoc, SILValue Enum, SILType Type,
      SILValue DefaultValue,
      ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues);

  template <typename SELECT_ENUM_INST>
  static SELECT_ENUM_INST *
  createSelectEnum(SILDebugLocation DebugLoc, SILValue Enum, SILType Type,
                   SILValue DefaultValue,
                   ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues,
                   SILFunction &F);

public:
  SILValue getEnumOperand() const { return getOperand(); }
  
  std::pair<EnumElementDecl*, SILValue>
  getCase(unsigned i) const {
    assert(i < NumCases && "case out of bounds");
    return std::make_pair(getCaseBuf()[i], Operands[i+1].get());
  }
  
  /// Return the value that will be used as the result for the specified enum
  /// case.
  SILValue getCaseResult(EnumElementDecl *D) {
    for (unsigned i = 0, e = getNumCases(); i != e; ++i) {
      auto Entry = getCase(i);
      if (Entry.first == D) return Entry.second;
    }
    // select_enum is required to be fully covered, so return the default if we
    // didn't find anything.
    return getDefaultResult();
  }
  
  /// \brief If the default refers to exactly one case decl, return it.
  NullablePtr<EnumElementDecl> getUniqueCaseForDefault();

  SILValue getDefaultResult() const {
    assert(HasDefault && "doesn't have a default");
    return Operands[NumCases + 1].get();
  }

  /// If there is a single case that returns a literal "true" value (an
  /// "integer_literal $Builtin.Int1, 1" value), return it.
  ///
  /// FIXME: This is used to interoperate with passes that reasoned about the
  /// old enum_is_tag insn. Ideally those passes would become general enough
  /// not to need this.
  NullablePtr<EnumElementDecl> getSingleTrueElement() const;
};
  
/// Select one of a set of values based on the case of an enum.
class SelectEnumInst : public SelectEnumInstBase {
  friend SILBuilder;

private:
  friend SelectEnumInstBase;

  SelectEnumInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Type,
                 SILValue DefaultValue,
                 ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues)
      : SelectEnumInstBase(ValueKind::SelectEnumInst, DebugLoc, Operand, Type,
                           DefaultValue, CaseValues) {}

  static SelectEnumInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType Type,
         SILValue DefaultValue,
         ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues,
         SILFunction &F);

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SelectEnumInst;
  }
};

/// Select one of a set of values based on the case of an enum.
class SelectEnumAddrInst : public SelectEnumInstBase {
  friend SILBuilder;
  friend SelectEnumInstBase;

  SelectEnumAddrInst(
      SILDebugLocation DebugLoc, SILValue Operand, SILType Type,
      SILValue DefaultValue,
      ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues)
      : SelectEnumInstBase(ValueKind::SelectEnumAddrInst, DebugLoc, Operand,
                           Type, DefaultValue, CaseValues) {}

  static SelectEnumAddrInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType Type,
         SILValue DefaultValue,
         ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues,
         SILFunction &F);

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SelectEnumAddrInst;
  }
};

/// Select on a value of a builtin integer type.
class SelectValueInst : public SelectInstBase<SelectValueInst, SILValue> {
  friend SILBuilder;

  SelectValueInst(SILDebugLocation DebugLoc, SILValue Operand, SILType Type,
                  SILValue DefaultResult,
                  ArrayRef<SILValue> CaseValuesAndResults);

  static SelectValueInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType Type,
         SILValue DefaultValue,
         ArrayRef<std::pair<SILValue, SILValue>> CaseValues, SILFunction &F);

  OperandValueArrayRef getCaseBuf() const {
    return Operands.getDynamicValuesAsArray();
  }

public:
  ~SelectValueInst();

  std::pair<SILValue, SILValue>
  getCase(unsigned i) const {
    assert(i < NumCases && "case out of bounds");
    return {getCaseBuf()[i*2], getCaseBuf()[i*2+1]};
  }

  SILValue getDefaultResult() const {
    assert(HasDefault && "doesn't have a default");
    return getCaseBuf()[NumCases*2];
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SelectValueInst;
  }
};

/// MetatypeInst - Represents the production of an instance of a given metatype
/// named statically.
class MetatypeInst final
    : public SILInstruction,
      private llvm::TrailingObjects<MetatypeInst, Operand> {
  friend TrailingObjects;
  friend SILBuilder;

  unsigned NumOperands;

  /// Constructs a MetatypeInst
  MetatypeInst(SILDebugLocation DebugLoc, SILType Metatype,
               ArrayRef<SILValue> TypeDependentOperands);

  static MetatypeInst *create(SILDebugLocation DebugLoc, SILType Metatype,
                              SILFunction *F,
                              SILOpenedArchetypesState &OpenedArchetypes);

public:
  ~MetatypeInst() {
    Operand *Operands = getTrailingObjects<Operand>();
    for (unsigned i = 0, end = NumOperands; i < end; ++i) {
      Operands[i].~Operand();
    }
  }

  ArrayRef<Operand> getAllOperands() const {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  MutableArrayRef<Operand> getAllOperands() {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::MetatypeInst;
  }
};

/// Represents loading a dynamic metatype from a value.
class ValueMetatypeInst
  : public UnaryInstructionBase<ValueKind::ValueMetatypeInst>
{
  friend SILBuilder;

  ValueMetatypeInst(SILDebugLocation DebugLoc, SILType Metatype, SILValue Base)
      : UnaryInstructionBase(DebugLoc, Base, Metatype) {}
};

/// ExistentialMetatype - Represents loading a dynamic metatype from an
/// existential container.
class ExistentialMetatypeInst
  : public UnaryInstructionBase<ValueKind::ExistentialMetatypeInst>
{
  friend SILBuilder;

  ExistentialMetatypeInst(SILDebugLocation DebugLoc, SILType Metatype,
                          SILValue Base)
      : UnaryInstructionBase(DebugLoc, Base, Metatype) {}
};

/// Extract a numbered element out of a value of tuple type.
class TupleExtractInst
  : public UnaryInstructionBase<ValueKind::TupleExtractInst>
{
  friend SILBuilder;

  unsigned FieldNo;

  TupleExtractInst(SILDebugLocation DebugLoc, SILValue Operand,
                   unsigned FieldNo, SILType ResultTy)
      : UnaryInstructionBase(DebugLoc, Operand, ResultTy), FieldNo(FieldNo) {}

public:
  unsigned getFieldNo() const { return FieldNo; }

  TupleType *getTupleType() const {
    return getOperand()->getType().getSwiftRValueType()->castTo<TupleType>();
  }

  unsigned getNumTupleElts() const {
    return getTupleType()->getNumElements();
  }

  /// Returns true if this is a trivial result of a tuple that is non-trivial
  /// and represents one RCID.
  bool isTrivialEltOfOneRCIDTuple() const;
  bool isEltOnlyNonTrivialElt() const;
};

/// Derive the address of a numbered element from the address of a tuple.
class TupleElementAddrInst
  : public UnaryInstructionBase<ValueKind::TupleElementAddrInst>
{
  friend SILBuilder;

  unsigned FieldNo;

  TupleElementAddrInst(SILDebugLocation DebugLoc, SILValue Operand,
                       unsigned FieldNo, SILType ResultTy)
      : UnaryInstructionBase(DebugLoc, Operand, ResultTy), FieldNo(FieldNo) {}

public:
  unsigned getFieldNo() const { return FieldNo; }


  TupleType *getTupleType() const {
    return getOperand()->getType().getSwiftRValueType()->castTo<TupleType>();
  }
};

/// Extract a physical, fragile field out of a value of struct type.
class StructExtractInst
  : public UnaryInstructionBase<ValueKind::StructExtractInst>
{
  friend SILBuilder;

  VarDecl *Field;

  StructExtractInst(SILDebugLocation DebugLoc, SILValue Operand,
                    VarDecl *Field, SILType ResultTy)
      : UnaryInstructionBase(DebugLoc, Operand, ResultTy), Field(Field) {}

public:
  VarDecl *getField() const { return Field; }

  unsigned getFieldNo() const {
    unsigned i = 0;
    for (VarDecl *D : getStructDecl()->getStoredProperties()) {
      if (Field == D)
        return i;
      ++i;
    }
    llvm_unreachable("A struct_extract's structdecl has at least 1 field, the "
                     "field of the struct_extract.");
  }

  StructDecl *getStructDecl() const {
    auto s = getOperand()->getType().getStructOrBoundGenericStruct();
    assert(s);
    return s;
  }

  /// Returns true if this is a trivial result of a struct that is non-trivial
  /// and represents one RCID.
  bool isTrivialFieldOfOneRCIDStruct() const;

  /// Return true if we are extracting the only non-trivial field of out parent
  /// struct. This implies that a ref count operation on the aggregate is
  /// equivalent to a ref count operation on this field.
  bool isFieldOnlyNonTrivialField() const;
};

/// Derive the address of a physical field from the address of a struct.
class StructElementAddrInst
  : public UnaryInstructionBase<ValueKind::StructElementAddrInst>
{
  friend SILBuilder;

  VarDecl *Field;

  StructElementAddrInst(SILDebugLocation DebugLoc, SILValue Operand,
                        VarDecl *Field, SILType ResultTy)
      : UnaryInstructionBase(DebugLoc, Operand, ResultTy), Field(Field) {}

public:
  VarDecl *getField() const { return Field; }

  unsigned getFieldNo() const {
    unsigned i = 0;
    for (auto *D : getStructDecl()->getStoredProperties()) {
      if (Field == D)
        return i;
      ++i;
    }
    llvm_unreachable("A struct_element_addr's structdecl has at least 1 field, "
                     "the field of the struct_element_addr.");
  }

  StructDecl *getStructDecl() const {
    auto s = getOperand()->getType().getStructOrBoundGenericStruct();
    assert(s);
    return s;
  }
};

/// RefElementAddrInst - Derive the address of a named element in a reference
/// type instance.
class RefElementAddrInst
  : public UnaryInstructionBase<ValueKind::RefElementAddrInst>
{
  friend SILBuilder;

  VarDecl *Field;

  RefElementAddrInst(SILDebugLocation DebugLoc, SILValue Operand,
                     VarDecl *Field, SILType ResultTy)
      : UnaryInstructionBase(DebugLoc, Operand, ResultTy), Field(Field) {}

public:
  VarDecl *getField() const { return Field; }

  unsigned getFieldNo() const {
    unsigned i = 0;
    for (auto *D : getClassDecl()->getStoredProperties()) {
      if (Field == D)
        return i;
      ++i;
    }
    llvm_unreachable("A ref_element_addr's classdecl has at least 1 field, the "
                     "field of the ref_element_addr.");
  }

  ClassDecl *getClassDecl() const {
    auto s = getOperand()->getType().getClassOrBoundGenericClass();
    assert(s);
    return s;
  }
};

/// RefTailAddrInst - Derive the address of the first element of the first
/// tail-allocated array in a reference type instance.
class RefTailAddrInst
  : public UnaryInstructionBase<ValueKind::RefTailAddrInst>
{
  friend SILBuilder;

  RefTailAddrInst(SILDebugLocation DebugLoc, SILValue Operand, SILType ResultTy)
      : UnaryInstructionBase(DebugLoc, Operand, ResultTy) {}

public:
  ClassDecl *getClassDecl() const {
    auto s = getOperand()->getType().getClassOrBoundGenericClass();
    assert(s);
    return s;
  }

  SILType getTailType() const { return getType().getObjectType(); }
};

/// MethodInst - Abstract base for instructions that implement dynamic
/// method lookup.
class MethodInst : public SILInstruction {
  SILDeclRef Member;
  bool Volatile;
public:
  MethodInst(ValueKind Kind, SILDebugLocation DebugLoc, SILType Ty,
             SILDeclRef Member, bool Volatile = false)
      : SILInstruction(Kind, DebugLoc, Ty), Member(Member), Volatile(Volatile) {
  }

  SILDeclRef getMember() const { return Member; }

  /// True if this dynamic dispatch is semantically required.
  bool isVolatile() const { return Volatile; }

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_MethodInst &&
      V->getKind() <= ValueKind::Last_MethodInst;
  }
};

/// ClassMethodInst - Given the address of a value of class type and a method
/// constant, extracts the implementation of that method for the dynamic
/// instance type of the class.
class ClassMethodInst
  : public UnaryInstructionBase<ValueKind::ClassMethodInst, MethodInst>
{
  friend SILBuilder;

  ClassMethodInst(SILDebugLocation DebugLoc, SILValue Operand,
                  SILDeclRef Member, SILType Ty, bool Volatile = false)
      : UnaryInstructionBase(DebugLoc, Operand, Ty, Member, Volatile) {}
};

/// SuperMethodInst - Given the address of a value of class type and a method
/// constant, extracts the implementation of that method for the superclass of
/// the static type of the class.
class SuperMethodInst
  : public UnaryInstructionBase<ValueKind::SuperMethodInst, MethodInst>
{
  friend SILBuilder;

  SuperMethodInst(SILDebugLocation DebugLoc, SILValue Operand,
                  SILDeclRef Member, SILType Ty, bool Volatile = false)
      : UnaryInstructionBase(DebugLoc, Operand, Ty, Member, Volatile) {}
};

/// WitnessMethodInst - Given a type, a protocol conformance,
/// and a protocol method constant, extracts the implementation of that method
/// for the type.
class WitnessMethodInst final
    : public MethodInst,
      llvm::TrailingObjects<WitnessMethodInst, Operand> {
  friend TrailingObjects;
  friend SILBuilder;

  CanType LookupType;
  ProtocolConformanceRef Conformance;
  unsigned NumOperands;

  WitnessMethodInst(SILDebugLocation DebugLoc, CanType LookupType,
                    ProtocolConformanceRef Conformance, SILDeclRef Member,
                    SILType Ty, ArrayRef<SILValue> TypeDependentOperands,
                    bool Volatile = false)
      : MethodInst(ValueKind::WitnessMethodInst, DebugLoc, Ty, Member,
                   Volatile),
        LookupType(LookupType), Conformance(Conformance),
        NumOperands(TypeDependentOperands.size()) {
    TrailingOperandsList::InitOperandsList(getAllOperands().begin(), this,
                                           TypeDependentOperands);
  }

  static WitnessMethodInst *
  create(SILDebugLocation DebugLoc, CanType LookupType,
         ProtocolConformanceRef Conformance, SILDeclRef Member, SILType Ty,
         SILFunction *Parent, SILOpenedArchetypesState &OpenedArchetypes,
         bool Volatile = false);

public:
  ~WitnessMethodInst() {
    Operand *Operands = getTrailingObjects<Operand>();
    for (unsigned i = 0, end = NumOperands; i < end; ++i) {
      Operands[i].~Operand();
    }
  }

  CanType getLookupType() const { return LookupType; }
  ProtocolDecl *getLookupProtocol() const {
    return getMember().getDecl()->getDeclContext()
             ->getAsProtocolOrProtocolExtensionContext();
  }

  ProtocolConformanceRef getConformance() const { return Conformance; }

  ArrayRef<Operand> getAllOperands() const {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  MutableArrayRef<Operand> getAllOperands() {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  ArrayRef<Operand> getTypeDependentOperands() const {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  MutableArrayRef<Operand> getTypeDependentOperands() {
    return { getTrailingObjects<Operand>(), NumOperands };
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::WitnessMethodInst;
  }
};

/// Given the address of a value of AnyObject protocol type and a method
/// constant referring to some Objective-C method, performs dynamic method
/// lookup to extract the implementation of that method. This method lookup
/// can fail at run-time
class DynamicMethodInst final
  : public UnaryInstructionWithTypeDependentOperandsBase<
                                   ValueKind::DynamicMethodInst,
                                   DynamicMethodInst,
                                   MethodInst,
                                   true>
{
  friend SILBuilder;

  DynamicMethodInst(SILDebugLocation DebugLoc, SILValue Operand,
                    ArrayRef<SILValue> TypeDependentOperands,
                    SILDeclRef Member, SILType Ty, bool Volatile)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Operand,
                               TypeDependentOperands, Ty, Member, Volatile) {}

  static DynamicMethodInst *
  create(SILDebugLocation DebugLoc, SILValue Operand,
         SILDeclRef Member, SILType Ty, bool Volatile, SILFunction *F,
         SILOpenedArchetypesState &OpenedArchetypes);
};

/// Access allowed to the opened value by the open_existential_addr instruction.
/// Allowing mutable access to the opened existential requires a boxed
/// existential value's box to be unique.
enum class OpenedExistentialAccess { Immutable, Mutable };

OpenedExistentialAccess getOpenedExistentialAccessFor(AccessKind access);

/// Given the address of an existential, "opens" the
/// existential by returning a pointer to a fresh archetype T, which also
/// captures the (dynamic) conformances.
class OpenExistentialAddrInst
  : public UnaryInstructionBase<ValueKind::OpenExistentialAddrInst>
{
  friend SILBuilder;
  OpenedExistentialAccess ForAccess;

  OpenExistentialAddrInst(SILDebugLocation DebugLoc, SILValue Operand,
                          SILType SelfTy, OpenedExistentialAccess AccessKind);

public:
  OpenedExistentialAccess getAccessKind() const { return ForAccess; }
};

/// Given an opaque value referring to an existential, "opens" the
/// existential by returning a pointer to a fresh archetype T, which also
/// captures the (dynamic) conformances.
class OpenExistentialOpaqueInst
    : public UnaryInstructionBase<ValueKind::OpenExistentialOpaqueInst> {
  friend SILBuilder;

  OpenExistentialOpaqueInst(SILDebugLocation DebugLoc, SILValue Operand,
                            SILType SelfTy);
};

/// Given a class existential, "opens" the
/// existential by returning a pointer to a fresh archetype T, which also
/// captures the (dynamic) conformances.
class OpenExistentialRefInst
  : public UnaryInstructionBase<ValueKind::OpenExistentialRefInst>
{
  friend SILBuilder;

  OpenExistentialRefInst(SILDebugLocation DebugLoc, SILValue Operand,
                         SILType Ty);
};

/// Given an existential metatype,
/// "opens" the existential by returning a pointer to a fresh
/// archetype metatype T.Type, which also captures the (dynamic)
/// conformances.
class OpenExistentialMetatypeInst
  : public UnaryInstructionBase<ValueKind::OpenExistentialMetatypeInst>
{
  friend SILBuilder;

  OpenExistentialMetatypeInst(SILDebugLocation DebugLoc, SILValue operand,
                              SILType ty);
};

/// Given a boxed existential container,
/// "opens" the existential by returning a pointer to a fresh
/// archetype T, which also captures the (dynamic) conformances.
class OpenExistentialBoxInst
  : public UnaryInstructionBase<ValueKind::OpenExistentialBoxInst>
{
  friend SILBuilder;

  OpenExistentialBoxInst(SILDebugLocation DebugLoc, SILValue operand,
                         SILType ty);
};

/// Given an address to an uninitialized buffer of
/// a protocol type, initializes its existential container to contain a concrete
/// value of the given type, and returns the address of the uninitialized
/// concrete value inside the existential container.
class InitExistentialAddrInst final
  : public UnaryInstructionWithTypeDependentOperandsBase<
                                ValueKind::InitExistentialAddrInst,
                                InitExistentialAddrInst,
                                SILInstruction,
                                true>
{
  friend SILBuilder;

  CanType ConcreteType;
  ArrayRef<ProtocolConformanceRef> Conformances;

  InitExistentialAddrInst(SILDebugLocation DebugLoc, SILValue Existential,
                          ArrayRef<SILValue> TypeDependentOperands,
                          CanType ConcreteType, SILType ConcreteLoweredType,
                          ArrayRef<ProtocolConformanceRef> Conformances)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Existential,
                             TypeDependentOperands,
                             ConcreteLoweredType.getAddressType()),
        ConcreteType(ConcreteType), Conformances(Conformances) {}

  static InitExistentialAddrInst *
  create(SILDebugLocation DebugLoc, SILValue Existential, CanType ConcreteType,
         SILType ConcreteLoweredType,
         ArrayRef<ProtocolConformanceRef> Conformances, SILFunction *Parent,
         SILOpenedArchetypesState &OpenedArchetypes);

public:
  ArrayRef<ProtocolConformanceRef> getConformances() const {
    return Conformances;
  }
  
  CanType getFormalConcreteType() const {
    return ConcreteType;
  }

  SILType getLoweredConcreteType() const {
    return getType();
  }
};

/// Given an uninitialized buffer of a protocol type,
/// initializes its existential container to contain a concrete
/// value of the given type, and returns the uninitialized
/// concrete value inside the existential container.
class InitExistentialOpaqueInst final
    : public UnaryInstructionWithTypeDependentOperandsBase<
          ValueKind::InitExistentialOpaqueInst, InitExistentialOpaqueInst,
          SILInstruction, true> {
  friend SILBuilder;

  CanType ConcreteType;
  ArrayRef<ProtocolConformanceRef> Conformances;

  InitExistentialOpaqueInst(SILDebugLocation DebugLoc, SILType ExistentialType,
                            CanType FormalConcreteType, SILValue Instance,
                            ArrayRef<SILValue> TypeDependentOperands,
                            ArrayRef<ProtocolConformanceRef> Conformances)
      : UnaryInstructionWithTypeDependentOperandsBase(
            DebugLoc, Instance, TypeDependentOperands, ExistentialType),
        ConcreteType(FormalConcreteType), Conformances(Conformances) {}

  static InitExistentialOpaqueInst *
  create(SILDebugLocation DebugLoc, SILType ExistentialType,
         CanType ConcreteType, SILValue Instance,
         ArrayRef<ProtocolConformanceRef> Conformances, SILFunction *Parent,
         SILOpenedArchetypesState &OpenedArchetypes);

public:
  CanType getFormalConcreteType() const { return ConcreteType; }

  ArrayRef<ProtocolConformanceRef> getConformances() const {
    return Conformances;
  }
};

/// InitExistentialRefInst - Given a class instance reference and a set of
/// conformances, creates a class existential value referencing the
/// class instance.
class InitExistentialRefInst final
  : public UnaryInstructionWithTypeDependentOperandsBase<
                                ValueKind::InitExistentialRefInst,
                                InitExistentialRefInst,
                                SILInstruction,
                                true>
{
  friend SILBuilder;

  CanType ConcreteType;
  ArrayRef<ProtocolConformanceRef> Conformances;

  InitExistentialRefInst(SILDebugLocation DebugLoc, SILType ExistentialType,
                         CanType FormalConcreteType, SILValue Instance,
                         ArrayRef<SILValue> TypeDependentOperands,
                         ArrayRef<ProtocolConformanceRef> Conformances)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Instance,
                                               TypeDependentOperands,
                                               ExistentialType),
        ConcreteType(FormalConcreteType), Conformances(Conformances) {}

  static InitExistentialRefInst *
  create(SILDebugLocation DebugLoc, SILType ExistentialType,
         CanType ConcreteType, SILValue Instance,
         ArrayRef<ProtocolConformanceRef> Conformances, SILFunction *Parent,
         SILOpenedArchetypesState &OpenedArchetypes);

  //size_t numTrailingObjects(OverloadToken<Operand>) const { return NumOperands; }

public:
  CanType getFormalConcreteType() const {
    return ConcreteType;
  }

  ArrayRef<ProtocolConformanceRef> getConformances() const {
    return Conformances;
  }
};

/// InitExistentialMetatypeInst - Given a metatype reference and a set
/// of conformances, creates an existential metatype value referencing
/// the metatype.
class InitExistentialMetatypeInst final
  : public UnaryInstructionWithTypeDependentOperandsBase<
                                  ValueKind::InitExistentialMetatypeInst,
                                  InitExistentialMetatypeInst,
                                  SILInstruction,
                                  true,
                                  ProtocolConformanceRef>
{
  friend SILBuilder;

  unsigned NumConformances;

  InitExistentialMetatypeInst(SILDebugLocation DebugLoc,
                              SILType existentialMetatypeType,
                              SILValue metatype,
                              ArrayRef<SILValue> TypeDependentOperands,
                              ArrayRef<ProtocolConformanceRef> conformances);

  static InitExistentialMetatypeInst *
  create(SILDebugLocation DebugLoc, SILType existentialMetatypeType,
         SILValue metatype, ArrayRef<ProtocolConformanceRef> conformances,
         SILFunction *parent, SILOpenedArchetypesState &OpenedArchetypes);

public:
  /// Return the object type which was erased.  That is, if this
  /// instruction erases Decoder<T>.Type.Type to Printable.Type.Type,
  /// this method returns Decoder<T>.
  CanType getFormalErasedObjectType() const {
    CanType exType = getType().getSwiftRValueType();
    CanType concreteType = getOperand()->getType().getSwiftRValueType();
    while (auto exMetatype = dyn_cast<ExistentialMetatypeType>(exType)) {
      exType = exMetatype.getInstanceType();
      concreteType = cast<MetatypeType>(concreteType).getInstanceType();
    }
    assert(exType.isExistentialType());
    return concreteType;
  }

  ArrayRef<ProtocolConformanceRef> getConformances() const;
};

/// DeinitExistentialAddrInst - Given an address of an existential that has been
/// partially initialized with an InitExistentialAddrInst but whose value buffer
/// has not been initialized, deinitializes the existential and deallocates
/// the value buffer. This should only be used for partially-initialized
/// existentials; a fully-initialized existential can be destroyed with
/// DestroyAddrInst and deallocated with DeallocStackInst.
class DeinitExistentialAddrInst
  : public UnaryInstructionBase<ValueKind::DeinitExistentialAddrInst,
                                SILInstruction,
                                /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  DeinitExistentialAddrInst(SILDebugLocation DebugLoc, SILValue Existential)
      : UnaryInstructionBase(DebugLoc, Existential) {}
};

class DeinitExistentialOpaqueInst
    : public UnaryInstructionBase<ValueKind::DeinitExistentialOpaqueInst,
                                  SILInstruction, /*HAS_RESULT*/ false> {
  friend SILBuilder;

  DeinitExistentialOpaqueInst(SILDebugLocation DebugLoc, SILValue Existential)
      : UnaryInstructionBase(DebugLoc, Existential) {}
};

/// Projects the capture storage address from a @block_storage address.
class ProjectBlockStorageInst
  : public UnaryInstructionBase<ValueKind::ProjectBlockStorageInst,
                                SILInstruction>
{
  friend SILBuilder;

  ProjectBlockStorageInst(SILDebugLocation DebugLoc, SILValue Operand,
                          SILType DestTy)
      : UnaryInstructionBase(DebugLoc, Operand, DestTy) {}
};


/// Initializes a block header, creating a block that
/// invokes a given thin cdecl function.
class InitBlockStorageHeaderInst : public SILInstruction {
  friend SILBuilder;

  enum { BlockStorage, InvokeFunction };
  unsigned NumSubstitutions;
  FixedOperandList<2> Operands;
  
  Substitution *getSubstitutionsStorage() {
    return reinterpret_cast<Substitution*>(Operands.asArray().end());
  }
  const Substitution *getSubstitutionsStorage() const {
    return reinterpret_cast<const Substitution*>(Operands.asArray().end());
  }

  InitBlockStorageHeaderInst(SILDebugLocation DebugLoc, SILValue BlockStorage,
                             SILValue InvokeFunction, SILType BlockType,
                             SubstitutionList Subs)
      : SILInstruction(ValueKind::InitBlockStorageHeaderInst, DebugLoc,
                       BlockType),
        NumSubstitutions(Subs.size()),
        Operands(this, BlockStorage, InvokeFunction) {
    memcpy(getSubstitutionsStorage(), Subs.begin(),
           sizeof(Subs[0]) * Subs.size());
  }
  
  static InitBlockStorageHeaderInst *create(SILFunction &F,
                              SILDebugLocation DebugLoc, SILValue BlockStorage,
                              SILValue InvokeFunction, SILType BlockType,
                              SubstitutionList Subs);
public:
  /// Get the block storage address to be initialized.
  SILValue getBlockStorage() const { return Operands[BlockStorage].get(); }
  /// Get the invoke function to form the block around.
  SILValue getInvokeFunction() const { return Operands[InvokeFunction].get(); }

  SubstitutionList getSubstitutions() const {
    return {getSubstitutionsStorage(), NumSubstitutions};
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::InitBlockStorageHeaderInst;
  }
};

/// StrongRetainInst - Increase the strong reference count of an object.
class StrongRetainInst
  : public UnaryInstructionBase<ValueKind::StrongRetainInst,
                                RefCountingInst,
                                /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  StrongRetainInst(SILDebugLocation DebugLoc, SILValue Operand,
                   Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, Operand) {
    setAtomicity(atomicity);
  }
};

/// StrongReleaseInst - Decrease the strong reference count of an object.
///
/// An object can be destroyed when its strong reference count is
/// zero.  It can be deallocated when both its strong reference and
/// weak reference counts reach zero.
class StrongReleaseInst
  : public UnaryInstructionBase<ValueKind::StrongReleaseInst,
                                RefCountingInst, /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  StrongReleaseInst(SILDebugLocation DebugLoc, SILValue Operand,
                    Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, Operand) {
    setAtomicity(atomicity);
  }
};

/// StrongRetainUnownedInst - Increase the strong reference count of an object
/// and assert that it has not been deallocated.
///
/// The operand must be an @unowned type.
class StrongRetainUnownedInst :
    public UnaryInstructionBase<ValueKind::StrongRetainUnownedInst,
                                RefCountingInst, /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  StrongRetainUnownedInst(SILDebugLocation DebugLoc, SILValue operand,
                          Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, operand) {
    setAtomicity(atomicity);
  }
};

/// UnownedRetainInst - Increase the unowned reference count of an object.
class UnownedRetainInst :
    public UnaryInstructionBase<ValueKind::UnownedRetainInst,
                                RefCountingInst, /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  UnownedRetainInst(SILDebugLocation DebugLoc, SILValue Operand,
                    Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, Operand) {
    setAtomicity(atomicity);
  }
};

/// UnownedReleaseInst - Decrease the unowned reference count of an object.
class UnownedReleaseInst :
     public UnaryInstructionBase<ValueKind::UnownedReleaseInst,
                                 RefCountingInst, /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  UnownedReleaseInst(SILDebugLocation DebugLoc, SILValue Operand,
                     Atomicity atomicity)
      : UnaryInstructionBase(DebugLoc, Operand) {
    setAtomicity(atomicity);
  }
};

/// FixLifetimeInst - An artificial use of a value for the purposes of ARC or
/// RVO optimizations.
class FixLifetimeInst :
  public UnaryInstructionBase<ValueKind::FixLifetimeInst,
                              SILInstruction, /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  FixLifetimeInst(SILDebugLocation DebugLoc, SILValue Operand)
      : UnaryInstructionBase(DebugLoc, Operand) {}
};

/// EndLifetimeInst - An artificial end lifetime use of a value for the purpose
/// of working around verification problems.
///
/// Specifically, the signature of destroying deinit takes self at +0 and
/// returns self at +1. This is an issue since a deallocating deinit takes in
/// self at +1. Previously, we could rely on the deallocating bit being set in
/// the object header to allow SILGen to statically balance the +1 from the
/// deallocating deinit. This is because deallocating values used to be
/// immortal. The runtime now asserts if we release a deallocating value,
/// meaning such an approach does not work. This instruction acts as a "fake"
/// lifetime ending use allowing for static verification of deallocating
/// destroyers, without an actual release being emitted (avoiding the runtime
/// assert).
class EndLifetimeInst
    : public UnaryInstructionBase<ValueKind::EndLifetimeInst, SILInstruction,
                                  /*HAS_RESULT*/ false> {
  friend SILBuilder;

  EndLifetimeInst(SILDebugLocation DebugLoc, SILValue Operand)
      : UnaryInstructionBase(DebugLoc, Operand) {}
};

/// An unsafe conversion in between ownership kinds.
///
/// This is used today in destructors where due to objective-c legacy
/// constraints, we need to be able to convert a guaranteed paramter to an owned
/// parameter.
class UncheckedOwnershipConversionInst
    : public UnaryInstructionBase<ValueKind::UncheckedOwnershipConversionInst> {
  friend SILBuilder;

  ValueOwnershipKind Kind;

  UncheckedOwnershipConversionInst(SILDebugLocation DebugLoc, SILValue operand,
                                   ValueOwnershipKind Kind)
      : UnaryInstructionBase(DebugLoc, operand, operand->getType()),
        Kind(Kind) {}

public:
  ValueOwnershipKind getConversionOwnershipKind() const { return Kind; }
};

/// MarkDependenceInst - Marks that one value depends on another for
/// validity in a non-obvious way.
class MarkDependenceInst : public SILInstruction {
  friend SILBuilder;

  enum { Value, Base };
  FixedOperandList<2> Operands;

  MarkDependenceInst(SILDebugLocation DebugLoc, SILValue value, SILValue base)
      : SILInstruction(ValueKind::MarkDependenceInst, DebugLoc,
                       value->getType()),
        Operands{this, value, base} {}

public:
  SILValue getValue() const { return Operands[Value].get(); }
  SILValue getBase() const { return Operands[Base].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::MarkDependenceInst;
  }
};

/// Promote an Objective-C block that is on the stack to the heap, or simply
/// retain a block that is already on the heap.
class CopyBlockInst :
    public UnaryInstructionBase<ValueKind::CopyBlockInst,
                                SILInstruction, /*HAS_RESULT*/ true>
{
  friend SILBuilder;

  CopyBlockInst(SILDebugLocation DebugLoc, SILValue operand)
      : UnaryInstructionBase(DebugLoc, operand, operand->getType()) {}
};

class CopyValueInst : public UnaryInstructionBase<ValueKind::CopyValueInst> {
  friend class SILBuilder;

  CopyValueInst(SILDebugLocation DebugLoc, SILValue operand)
      : UnaryInstructionBase(DebugLoc, operand, operand->getType()) {}
};

class CopyUnownedValueInst
    : public UnaryInstructionBase<ValueKind::CopyUnownedValueInst> {
  friend class SILBuilder;

  CopyUnownedValueInst(SILDebugLocation DebugLoc, SILValue operand,
                       SILModule &M)
      : UnaryInstructionBase(DebugLoc, operand,
                             operand->getType().getReferentType(M)) {}
};

class DestroyValueInst
    : public UnaryInstructionBase<ValueKind::DestroyValueInst> {
  friend class SILBuilder;

  DestroyValueInst(SILDebugLocation DebugLoc, SILValue operand)
      : UnaryInstructionBase(DebugLoc, operand) {}
};

/// Given an object reference, return true iff it is non-nil and refers
/// to a native swift object with strong reference count of 1.
class IsUniqueInst : public UnaryInstructionBase<ValueKind::IsUniqueInst>
{
  friend SILBuilder;

  IsUniqueInst(SILDebugLocation DebugLoc, SILValue Operand, SILType BoolTy)
      : UnaryInstructionBase(DebugLoc, Operand, BoolTy) {}
};

/// Given an object reference, return true iff it is non-nil and either refers
/// to a native swift object with strong reference count of 1 or refers to a
/// pinned object (for simultaneous access to multiple subobjects).
class IsUniqueOrPinnedInst :
    public UnaryInstructionBase<ValueKind::IsUniqueOrPinnedInst> {
  friend SILBuilder;

  IsUniqueOrPinnedInst(SILDebugLocation DebugLoc, SILValue Operand,
                       SILType BoolTy)
      : UnaryInstructionBase(DebugLoc, Operand, BoolTy) {}
};

//===----------------------------------------------------------------------===//
// DeallocationInsts
//===----------------------------------------------------------------------===//

/// DeallocationInst - An abstract parent class for Dealloc{Stack, Box, Ref}.
class DeallocationInst : public SILInstruction {
protected:
  DeallocationInst(ValueKind Kind, SILDebugLocation DebugLoc)
      : SILInstruction(Kind, DebugLoc) {}

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_DeallocationInst &&
      V->getKind() <= ValueKind::Last_DeallocationInst;
  }
};

/// DeallocStackInst - Deallocate stack memory allocated by alloc_stack.
class DeallocStackInst :
    public UnaryInstructionBase<ValueKind::DeallocStackInst, DeallocationInst,
                                /*HAS_RESULT*/ false> {
  friend SILBuilder;

  DeallocStackInst(SILDebugLocation DebugLoc, SILValue operand)
      : UnaryInstructionBase(DebugLoc, operand) {}
};

/// Deallocate memory for a reference type instance from a destructor or
/// failure path of a constructor.
///
/// This does not destroy the referenced instance; it must be destroyed
/// first.
///
/// It is undefined behavior if the type of the operand does not match the
/// most derived type of the allocated instance.
class DeallocRefInst :
  public UnaryInstructionBase<ValueKind::DeallocRefInst, DeallocationInst,
                              /*HAS_RESULT*/ false>,
  public StackPromotable {
  friend SILBuilder;

private:
  DeallocRefInst(SILDebugLocation DebugLoc, SILValue Operand,
                 bool canBeOnStack = false)
      : UnaryInstructionBase(DebugLoc, Operand), StackPromotable(canBeOnStack) {
  }
};

/// Deallocate memory for a reference type instance from a failure path of a
/// constructor.
///
/// The instance is assumed to have been partially initialized, with the
/// initialized portion being all instance variables in classes that are more
/// derived than the given metatype.
///
/// The metatype value can either be the static self type (in a designated
/// initializer) or a dynamic self type (in a convenience initializer).
class DeallocPartialRefInst : public DeallocationInst {
  friend SILBuilder;

private:
  FixedOperandList<2> Operands;

  DeallocPartialRefInst(SILDebugLocation DebugLoc, SILValue Operand,
                        SILValue Metatype)
      : DeallocationInst(ValueKind::DeallocPartialRefInst, DebugLoc),
        Operands(this, Operand, Metatype) {}

public:
  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }
  
  SILValue getInstance() const { return getOperand(0); }
  SILValue getMetatype() const { return getOperand(1); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::DeallocPartialRefInst;
  }
};

/// Deallocate memory allocated for an unsafe value buffer.
class DeallocValueBufferInst :
  public UnaryInstructionBase<ValueKind::DeallocValueBufferInst,
                              DeallocationInst, /*HAS_RESULT*/ true> {
  friend SILBuilder;

  SILType ValueType;

  DeallocValueBufferInst(SILDebugLocation DebugLoc, SILType valueType,
                         SILValue operand)
      : UnaryInstructionBase(DebugLoc, operand), ValueType(valueType) {}

public:
  SILType getValueType() const { return ValueType; }
};

/// Deallocate memory allocated for a boxed value created by an AllocBoxInst.
/// It is undefined behavior if the type of the boxed type does not match the
/// type the box was allocated for.
///
/// This does not destroy the boxed value instance; it must either be
/// uninitialized or have been manually destroyed.
class DeallocBoxInst :
  public UnaryInstructionBase<ValueKind::DeallocBoxInst, DeallocationInst,
                              /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  DeallocBoxInst(SILDebugLocation DebugLoc, SILValue operand)
      : UnaryInstructionBase(DebugLoc, operand) {}
};

/// Deallocate memory allocated for a boxed existential container created by
/// AllocExistentialBox. It is undefined behavior if the given concrete type
/// does not match the concrete type for which the box was allocated.
///
/// This does not destroy the boxed value instance; it must either be
/// uninitialized or have been manually destroyed.
class DeallocExistentialBoxInst :
  public UnaryInstructionBase<ValueKind::DeallocExistentialBoxInst,
                              DeallocationInst,
                              /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  CanType ConcreteType;

  DeallocExistentialBoxInst(SILDebugLocation DebugLoc, CanType concreteType,
                            SILValue operand)
      : UnaryInstructionBase(DebugLoc, operand), ConcreteType(concreteType) {}

public:
  CanType getConcreteType() const { return ConcreteType; }
};

/// Destroy the value at a memory location according to
/// its SIL type. This is similar to:
///   %1 = load %operand
///   release_value %1
/// but a destroy instruction can be used for types that cannot be loaded,
/// such as resilient value types.
class DestroyAddrInst : public UnaryInstructionBase<ValueKind::DestroyAddrInst,
                                                    SILInstruction,
                                                    /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  DestroyAddrInst(SILDebugLocation DebugLoc, SILValue Operand)
      : UnaryInstructionBase(DebugLoc, Operand) {}
};

/// Project out the address of the value
/// stored in the given Builtin.UnsafeValueBuffer.
class ProjectValueBufferInst :
  public UnaryInstructionBase<ValueKind::ProjectValueBufferInst,
                              SILInstruction, /*HasResult*/ true> {
  friend SILBuilder;

  ProjectValueBufferInst(SILDebugLocation DebugLoc, SILType valueType,
                         SILValue operand)
      : UnaryInstructionBase(DebugLoc, operand, valueType.getAddressType()) {}

public:
  SILType getValueType() const { return getType().getObjectType(); }
};

/// Project out the address of the value in a box.
class ProjectBoxInst :
  public UnaryInstructionBase<ValueKind::ProjectBoxInst,
                              SILInstruction, /*HasResult*/ true> {
  friend SILBuilder;

  unsigned Index;

  ProjectBoxInst(SILDebugLocation DebugLoc,
                 SILValue operand,
                 unsigned fieldIndex,
                 SILType fieldTy)
      : UnaryInstructionBase(DebugLoc, operand, fieldTy.getAddressType()),
        Index(fieldIndex) {}


public:
  unsigned getFieldIndex() const { return Index; }
};

/// Project out the address of the value in an existential box.
class ProjectExistentialBoxInst :
  public UnaryInstructionBase<ValueKind::ProjectExistentialBoxInst,
                              SILInstruction, /*HasResult*/ true> {
  friend SILBuilder;
  
  ProjectExistentialBoxInst(SILDebugLocation DebugLoc, SILType valueType,
                            SILValue operand)
      : UnaryInstructionBase(DebugLoc, operand, valueType.getAddressType()) {}
};

//===----------------------------------------------------------------------===//
// Runtime failure
//===----------------------------------------------------------------------===//

/// Trigger a runtime failure if the given Int1 value is true.
class CondFailInst : public UnaryInstructionBase<ValueKind::CondFailInst,
                                                 SILInstruction,
                                                 /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  CondFailInst(SILDebugLocation DebugLoc, SILValue Operand)
      : UnaryInstructionBase(DebugLoc, Operand) {}
};

//===----------------------------------------------------------------------===//
// Pointer/address indexing instructions
//===----------------------------------------------------------------------===//

/// Abstract base class for indexing instructions.
class IndexingInst : public SILInstruction {
  enum { Base, Index };
  FixedOperandList<2> Operands;
public:
  IndexingInst(ValueKind Kind, SILDebugLocation DebugLoc, SILValue Operand,
               SILValue Index)
      : SILInstruction(Kind, DebugLoc, Operand->getType()),
        Operands{this, Operand, Index} {}

  IndexingInst(ValueKind Kind, SILDebugLocation DebugLoc, SILValue Operand,
               SILValue Index, SILType ResultTy)
      : SILInstruction(Kind, DebugLoc, ResultTy),
        Operands{this, Operand, Index} {}

  SILValue getBase() const { return Operands[Base].get(); }
  SILValue getIndex() const { return Operands[Index].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_IndexingInst
        && V->getKind() <= ValueKind::Last_IndexingInst;
  }
};

/// IndexAddrInst - "%2 : $*T = index_addr %0 : $*T, %1 : $Builtin.Word"
/// This takes an address and indexes it, striding by the pointed-
/// to type.  This is used to index into arrays of uniform elements.
class IndexAddrInst : public IndexingInst {
  friend SILBuilder;

  enum { Base, Index };

  IndexAddrInst(SILDebugLocation DebugLoc, SILValue Operand, SILValue Index)
      : IndexingInst(ValueKind::IndexAddrInst, DebugLoc, Operand, Index) {}

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::IndexAddrInst;
  }
};

/// TailAddrInst - like IndexingInst, but aligns-up the resulting address to a
/// tail-allocated element type.
class TailAddrInst : public IndexingInst {
  friend SILBuilder;

  TailAddrInst(SILDebugLocation DebugLoc, SILValue Operand, SILValue Count,
               SILType ResultTy)
      : IndexingInst(ValueKind::TailAddrInst, DebugLoc, Operand, Count,
                     ResultTy) {}

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::TailAddrInst;
  }

  SILType getTailType() const { return getType().getObjectType(); }
};

/// IndexRawPointerInst
/// %2 : $Builtin.RawPointer \
///   = index_raw_pointer %0 : $Builtin.RawPointer, %1 : $Builtin.Word
/// This takes an address and indexes it, striding by the pointed-
/// to type.  This is used to index into arrays of uniform elements.
class IndexRawPointerInst : public IndexingInst {
  friend SILBuilder;

  enum { Base, Index };

  IndexRawPointerInst(SILDebugLocation DebugLoc, SILValue Operand,
                      SILValue Index)
      : IndexingInst(ValueKind::IndexRawPointerInst, DebugLoc, Operand, Index) {
  }

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::IndexRawPointerInst;
  }
};

//===----------------------------------------------------------------------===//
// Instructions representing terminators
//===----------------------------------------------------------------------===//

enum class TermKind {
#define TERMINATOR(Id, Parent, TextualName, MemBehavior, MayRelease) Id,
#include "SILNodes.def"
};

struct ValueKindAsTermKind {
  TermKind K;

  ValueKindAsTermKind(ValueKind V) {
    switch (V) {
#define TERMINATOR(Id, Parent, TextualName, MemBehavior, MayRelease)           \
  case ValueKind::Id:                                                          \
    K = TermKind::Id;                                                          \
    break;
#include "SILNodes.def"
    default:
      llvm_unreachable("Not a terminator kind?!");
    }
  }

  operator TermKind() const { return K; }
};

/// This class defines a "terminating instruction" for a SILBasicBlock.
class TermInst : public SILInstruction {
protected:
  TermInst(ValueKind K, SILDebugLocation DebugLoc)
      : SILInstruction(K, DebugLoc) {}

public:

  using ConstSuccessorListTy = ArrayRef<SILSuccessor>;
  using SuccessorListTy = MutableArrayRef<SILSuccessor>;

  /// The successor basic blocks of this terminator.
  SuccessorListTy getSuccessors();
  ConstSuccessorListTy getSuccessors() const {
    return const_cast<TermInst*>(this)->getSuccessors();
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_TermInst &&
           V->getKind() <= ValueKind::Last_TermInst;
  }

  bool isBranch() const { return !getSuccessors().empty(); }

  /// Returns true if this terminator exits the function.
  bool isFunctionExiting() const;

  TermKind getTermKind() const { return ValueKindAsTermKind(getKind()); }
};

/// UnreachableInst - Position in the code which would be undefined to reach.
/// These are always implicitly generated, e.g. when falling off the end of a
/// function or after a no-return function call.
class UnreachableInst : public TermInst {
  friend SILBuilder;

  UnreachableInst(SILDebugLocation DebugLoc)
      : TermInst(ValueKind::UnreachableInst, DebugLoc) {}

public:
  SuccessorListTy getSuccessors() {
    // No Successors.
    return SuccessorListTy();
  }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::UnreachableInst;
  }
};

/// ReturnInst - Representation of a ReturnStmt.
class ReturnInst
  : public UnaryInstructionBase<ValueKind::ReturnInst, TermInst,
                                /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  /// Constructs a ReturnInst representing a return.
  ///
  /// \param DebugLoc The backing AST location.
  ///
  /// \param ReturnValue The value to be returned.
  ///
  ReturnInst(SILDebugLocation DebugLoc, SILValue ReturnValue)
      : UnaryInstructionBase(DebugLoc, ReturnValue) {}

public:
  SuccessorListTy getSuccessors() {
    // No Successors.
    return SuccessorListTy();
  }
};

/// ThrowInst - Throw a typed error (which, in our system, is
/// essentially just a funny kind of return).
class ThrowInst
  : public UnaryInstructionBase<ValueKind::ThrowInst, TermInst,
                                /*HAS_RESULT*/ false>
{
  friend SILBuilder;

  /// Constructs a ThrowInst representing a throw out of the current
  /// function.
  ///
  /// \param DebugLoc The location of the throw.
  /// \param errorValue The value to be thrown.
  ThrowInst(SILDebugLocation DebugLoc, SILValue errorValue)
      : UnaryInstructionBase(DebugLoc, errorValue) {}

public:
  SuccessorListTy getSuccessors() {
    // No successors.
    return SuccessorListTy();
  }
};

/// BranchInst - An unconditional branch.
class BranchInst : public TermInst {
  friend SILBuilder;

  SILSuccessor DestBB;
  // FIXME: probably needs dynamic adjustment
  TailAllocatedOperandList<0> Operands;

  BranchInst(SILDebugLocation DebugLoc, SILBasicBlock *DestBB,
             ArrayRef<SILValue> Args);

  /// Construct a BranchInst that will branch to the specified block.
  /// The destination block must take no parameters.
  static BranchInst *create(SILDebugLocation DebugLoc, SILBasicBlock *DestBB,
                            SILFunction &F);

  /// Construct a BranchInst that will branch to the specified block with
  /// the given parameters.
  static BranchInst *create(SILDebugLocation DebugLoc, SILBasicBlock *DestBB,
                            ArrayRef<SILValue> Args, SILFunction &F);

public:
  /// \brief returns jump target for the branch.
  SILBasicBlock *getDestBB() const { return DestBB; }

  /// The arguments for the destination BB.
  OperandValueArrayRef getArgs() const { return Operands.asValueArray(); }

  SuccessorListTy getSuccessors() {
    return SuccessorListTy(&DestBB, 1);
  }

  unsigned getNumArgs() const { return Operands.size(); }
  SILValue getArg(unsigned i) const { return Operands[i].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::BranchInst;
  }
};

/// A conditional branch.
class CondBranchInst : public TermInst {
  friend SILBuilder;

public:
  enum {
    /// The operand index of the condition value used for the branch.
    ConditionIdx
  };
  enum {
    // Map branch targets to block successor indices.
    TrueIdx,
    FalseIdx
  };
private:
  SILSuccessor DestBBs[2];
  /// The number of arguments for the True branch.
  unsigned NumTrueArgs;
  /// The number of arguments for the False branch.
  unsigned NumFalseArgs;

  /// The first argument is the condition; the rest are BB arguments.
  TailAllocatedOperandList<1> Operands;
  CondBranchInst(SILDebugLocation DebugLoc, SILValue Condition,
                 SILBasicBlock *TrueBB, SILBasicBlock *FalseBB,
                 ArrayRef<SILValue> Args, unsigned NumTrue, unsigned NumFalse);

  /// Construct a CondBranchInst that will branch to TrueBB or FalseBB based on
  /// the Condition value. Both blocks must not take any arguments.
  static CondBranchInst *create(SILDebugLocation DebugLoc, SILValue Condition,
                                SILBasicBlock *TrueBB, SILBasicBlock *FalseBB,
                                SILFunction &F);

  /// Construct a CondBranchInst that will either branch to TrueBB and pass
  /// TrueArgs or branch to FalseBB and pass FalseArgs based on the Condition
  /// value.
  static CondBranchInst *create(SILDebugLocation DebugLoc, SILValue Condition,
                                SILBasicBlock *TrueBB,
                                ArrayRef<SILValue> TrueArgs,
                                SILBasicBlock *FalseBB,
                                ArrayRef<SILValue> FalseArgs, SILFunction &F);

public:
  SILValue getCondition() const { return Operands[ConditionIdx].get(); }
  void setCondition(SILValue newCondition) {
    Operands[ConditionIdx].set(newCondition);
  }

  SuccessorListTy getSuccessors() {
    return DestBBs;
  }

  SILBasicBlock *getTrueBB() { return DestBBs[0]; }
  const SILBasicBlock *getTrueBB() const { return DestBBs[0]; }
  SILBasicBlock *getFalseBB() { return DestBBs[1]; }
  const SILBasicBlock *getFalseBB() const { return DestBBs[1]; }

  /// Get the arguments to the true BB.
  OperandValueArrayRef getTrueArgs() const;
  /// Get the arguments to the false BB.
  OperandValueArrayRef getFalseArgs() const;

  /// Get the operands to the true BB.
  ArrayRef<Operand> getTrueOperands() const;
  MutableArrayRef<Operand> getTrueOperands();

  /// Get the operands to the false BB.
  ArrayRef<Operand> getFalseOperands() const;
  MutableArrayRef<Operand> getFalseOperands();

  bool isConditionOperandIndex(unsigned OpIndex) const {
    assert(OpIndex < getNumOperands() &&
           "OpIndex must be an index for an actual operand");
    return OpIndex == ConditionIdx;
  }

  /// Is \p OpIndex an operand associated with the true case?
  bool isTrueOperandIndex(unsigned OpIndex) const {
    assert(OpIndex < getNumOperands() &&
           "OpIndex must be an index for an actual operand");
    if (NumTrueArgs == 0)
      return false;

    auto Operands = getTrueOperands();
    return Operands.front().getOperandNumber() <= OpIndex &&
           OpIndex <= Operands.back().getOperandNumber();
  }

  /// Is \p OpIndex an operand associated with the false case?
  bool isFalseOperandIndex(unsigned OpIndex) const {
    assert(OpIndex < getNumOperands() &&
           "OpIndex must be an index for an actual operand");
    if (NumFalseArgs == 0)
      return false;

    auto Operands = getFalseOperands();
    return Operands.front().getOperandNumber() <= OpIndex &&
           OpIndex <= Operands.back().getOperandNumber();
  }

  /// Returns the argument on the cond_br terminator that will be passed to
  /// DestBB in A.
  SILValue getArgForDestBB(const SILBasicBlock *DestBB,
                           const SILArgument *A) const;

  /// Returns the argument on the cond_br terminator that will be passed as the
  /// \p Index argument to DestBB.
  SILValue getArgForDestBB(const SILBasicBlock *DestBB,
                           unsigned ArgIndex) const;

  void swapSuccessors();

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::CondBranchInst;
  }
};

/// A switch on a value of a builtin type.
class SwitchValueInst : public TermInst {
  friend SILBuilder;

  unsigned NumCases : 31;
  unsigned HasDefault : 1;
  TailAllocatedOperandList<1> Operands;

  SwitchValueInst(SILDebugLocation DebugLoc, SILValue Operand,
                  SILBasicBlock *DefaultBB, ArrayRef<SILValue> Cases,
                  ArrayRef<SILBasicBlock *> BBs);

  // Tail-allocated after the SwitchValueInst record are:
  // - `NumCases` SILValue values, containing
  //   the SILValue references for each case
  // - `NumCases + HasDefault` SILSuccessor records, referencing the
  //   destinations for each case, ending with the default destination if
  //   present.


  OperandValueArrayRef getCaseBuf() const {
    return Operands.getDynamicValuesAsArray();
  }

  SILSuccessor *getSuccessorBuf() {
    return reinterpret_cast<SILSuccessor*>(Operands.asArray().end());
  }
  const SILSuccessor *getSuccessorBuf() const {
    return reinterpret_cast<const SILSuccessor *>(Operands.asArray().end());
  }

  static SwitchValueInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILBasicBlock *DefaultBB,
         ArrayRef<std::pair<SILValue, SILBasicBlock *>> CaseBBs,
         SILFunction &F);

public:
  /// Clean up tail-allocated successor records for the switch cases.
  ~SwitchValueInst();

  SILValue getOperand() const { return Operands[0].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  SuccessorListTy getSuccessors() {
    return MutableArrayRef<SILSuccessor>{getSuccessorBuf(),
                           static_cast<size_t>(NumCases + HasDefault)};
  }

  unsigned getNumCases() const { return NumCases; }
  std::pair<SILValue, SILBasicBlock*>
  getCase(unsigned i) const {
    assert(i < NumCases && "case out of bounds");
    return {getCaseBuf()[i], getSuccessorBuf()[i]};
  }

  bool hasDefault() const { return HasDefault; }
  SILBasicBlock *getDefaultBB() const {
    assert(HasDefault && "doesn't have a default");
    return getSuccessorBuf()[NumCases];
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SwitchValueInst;
  }
};

/// Common implementation for the switch_enum and
/// switch_enum_addr instructions.
class SwitchEnumInstBase : public TermInst {
  FixedOperandList<1> Operands;
  unsigned NumCases : 31;
  unsigned HasDefault : 1;

  // Tail-allocated after the SwitchEnumInst record are:
  // - an array of `NumCases` EnumElementDecl* pointers, referencing the case
  //   discriminators
  // - `NumCases + HasDefault` SILSuccessor records, referencing the
  //   destinations for each case, ending with the default destination if
  //   present.
  // FIXME: This should use llvm::TrailingObjects, but it has subclasses
  // (which are empty, of course).

  EnumElementDecl **getCaseBuf() {
    return reinterpret_cast<EnumElementDecl**>(this + 1);

  }
  EnumElementDecl * const* getCaseBuf() const {
    return reinterpret_cast<EnumElementDecl* const*>(this + 1);

  }

  SILSuccessor *getSuccessorBuf() {
    return reinterpret_cast<SILSuccessor*>(getCaseBuf() + NumCases);
  }
  const SILSuccessor *getSuccessorBuf() const {
    return reinterpret_cast<const SILSuccessor*>(getCaseBuf() + NumCases);
  }

protected:
  SwitchEnumInstBase(
      ValueKind Kind, SILDebugLocation DebugLoc, SILValue Operand,
      SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs);

  template <typename SWITCH_ENUM_INST>
  static SWITCH_ENUM_INST *createSwitchEnum(
      SILDebugLocation DebugLoc, SILValue Operand, SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs,
      SILFunction &F);

public:
  /// Clean up tail-allocated successor records for the switch cases.
  ~SwitchEnumInstBase();

  SILValue getOperand() const { return Operands[0].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  SuccessorListTy getSuccessors() {
    return MutableArrayRef<SILSuccessor>{getSuccessorBuf(),
                           static_cast<size_t>(NumCases + HasDefault)};
  }

  unsigned getNumCases() const { return NumCases; }
  std::pair<EnumElementDecl*, SILBasicBlock*>
  getCase(unsigned i) const {
    assert(i < NumCases && "case out of bounds");
    return {getCaseBuf()[i], getSuccessorBuf()[i].getBB()};
  }

  /// \brief Return the block that will be branched to on the specified enum
  /// case.
  SILBasicBlock *getCaseDestination(EnumElementDecl *D) {
    for (unsigned i = 0, e = getNumCases(); i != e; ++i) {
      auto Entry = getCase(i);
      if (Entry.first == D) return Entry.second;
    }
    // switch_enum is required to be fully covered, so return the default if we
    // didn't find anything.
    return getDefaultBB();
  }

  /// \brief If the default refers to exactly one case decl, return it.
  NullablePtr<EnumElementDecl> getUniqueCaseForDefault();

  /// \brief If the given block only has one enum element decl matched to it,
  /// return it.
  NullablePtr<EnumElementDecl> getUniqueCaseForDestination(SILBasicBlock *BB);

  bool hasDefault() const { return HasDefault; }
  SILBasicBlock *getDefaultBB() const {
    assert(HasDefault && "doesn't have a default");
    return getSuccessorBuf()[NumCases];
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::SwitchEnumInst &&
           V->getKind() <= ValueKind::SwitchEnumAddrInst;
  }
};

/// A switch on a loadable enum's discriminator. The data for each case is
/// passed into the corresponding destination block as an argument.
class SwitchEnumInst : public SwitchEnumInstBase {
  friend SILBuilder;

private:
  friend SwitchEnumInstBase;

  SwitchEnumInst(
      SILDebugLocation DebugLoc, SILValue Operand, SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs)
      : SwitchEnumInstBase(ValueKind::SwitchEnumInst, DebugLoc, Operand,
                           DefaultBB, CaseBBs) {}

  static SwitchEnumInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILBasicBlock *DefaultBB,
         ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs,
         SILFunction &F);

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SwitchEnumInst;
  }
};

/// A switch on an enum's discriminator in memory.
class SwitchEnumAddrInst : public SwitchEnumInstBase {
  friend SILBuilder;

private:
  friend SwitchEnumInstBase;

  SwitchEnumAddrInst(
      SILDebugLocation DebugLoc, SILValue Operand, SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs)
      : SwitchEnumInstBase(ValueKind::SwitchEnumAddrInst, DebugLoc, Operand,
                           DefaultBB, CaseBBs) {}

  static SwitchEnumAddrInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILBasicBlock *DefaultBB,
         ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs,
         SILFunction &F);

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SwitchEnumAddrInst;
  }
};

/// Branch on the existence of an Objective-C method in the dynamic type of
/// an object.
///
/// If the method exists, branches to the first BB, providing it with the
/// method reference; otherwise, branches to the second BB.
class DynamicMethodBranchInst : public TermInst {
  friend SILBuilder;

  SILDeclRef Member;

  SILSuccessor DestBBs[2];

  /// The operand.
  FixedOperandList<1> Operands;

  DynamicMethodBranchInst(SILDebugLocation DebugLoc, SILValue Operand,
                          SILDeclRef Member, SILBasicBlock *HasMethodBB,
                          SILBasicBlock *NoMethodBB);

  /// Construct a DynamicMethodBranchInst that will branch to \c HasMethodBB or
  /// \c NoMethodBB based on the ability of the object operand to respond to
  /// a message with the same selector as the member.
  static DynamicMethodBranchInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILDeclRef Member,
         SILBasicBlock *HasMethodBB, SILBasicBlock *NoMethodBB, SILFunction &F);

public:
  SILValue getOperand() const { return Operands[0].get(); }

  SILDeclRef getMember() const { return Member; }

  SuccessorListTy getSuccessors() {
    return DestBBs;
  }

  SILBasicBlock *getHasMethodBB() { return DestBBs[0]; }
  const SILBasicBlock *getHasMethodBB() const { return DestBBs[0]; }
  SILBasicBlock *getNoMethodBB() { return DestBBs[1]; }
  const SILBasicBlock *getNoMethodBB() const { return DestBBs[1]; }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::DynamicMethodBranchInst;
  }
};

/// Perform a checked cast operation and branch on whether the cast succeeds.
/// The success branch destination block receives the cast result as a BB
/// argument.
class CheckedCastBranchInst final:
  public UnaryInstructionWithTypeDependentOperandsBase<
                              ValueKind::CheckedCastBranchInst,
                              CheckedCastBranchInst,
                              TermInst,
                              false> {
  friend SILBuilder;

  SILType DestTy;
  bool IsExact;

  SILSuccessor DestBBs[2];

  CheckedCastBranchInst(SILDebugLocation DebugLoc, bool IsExact,
                        SILValue Operand,
                        ArrayRef<SILValue> TypeDependentOperands,
                        SILType DestTy,
                        SILBasicBlock *SuccessBB, SILBasicBlock *FailureBB)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Operand,
                                               TypeDependentOperands),
        DestTy(DestTy), IsExact(IsExact),
        DestBBs{{this, SuccessBB}, {this, FailureBB}} {}

  static CheckedCastBranchInst *
  create(SILDebugLocation DebugLoc, bool IsExact, SILValue Operand,
         SILType DestTy, SILBasicBlock *SuccessBB, SILBasicBlock *FailureBB,
         SILFunction &F, SILOpenedArchetypesState &OpenedArchetypes);

public:
  bool isExact() const { return IsExact; }

  SuccessorListTy getSuccessors() {
    return DestBBs;
  }

  SILType getCastType() const { return DestTy; }

  SILBasicBlock *getSuccessBB() { return DestBBs[0]; }
  const SILBasicBlock *getSuccessBB() const { return DestBBs[0]; }
  SILBasicBlock *getFailureBB() { return DestBBs[1]; }
  const SILBasicBlock *getFailureBB() const { return DestBBs[1]; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::CheckedCastBranchInst;
  }
};

/// Perform a checked cast operation and branch on whether the cast succeeds.
/// The success branch destination block receives the cast result as a BB
/// argument.
class CheckedCastValueBranchInst final
    : public UnaryInstructionWithTypeDependentOperandsBase<
          ValueKind::CheckedCastValueBranchInst, CheckedCastValueBranchInst,
          TermInst, false> {
  friend SILBuilder;

  SILType DestTy;

  SILSuccessor DestBBs[2];

  CheckedCastValueBranchInst(SILDebugLocation DebugLoc, SILValue Operand,
                             ArrayRef<SILValue> TypeDependentOperands,
                             SILType DestTy, SILBasicBlock *SuccessBB,
                             SILBasicBlock *FailureBB)
      : UnaryInstructionWithTypeDependentOperandsBase(DebugLoc, Operand,
                                                      TypeDependentOperands),
        DestTy(DestTy), DestBBs{{this, SuccessBB}, {this, FailureBB}} {}

  static CheckedCastValueBranchInst *
  create(SILDebugLocation DebugLoc, SILValue Operand, SILType DestTy,
         SILBasicBlock *SuccessBB, SILBasicBlock *FailureBB, SILFunction &F,
         SILOpenedArchetypesState &OpenedArchetypes);

public:
  SuccessorListTy getSuccessors() { return DestBBs; }

  SILType getCastType() const { return DestTy; }

  SILBasicBlock *getSuccessBB() { return DestBBs[0]; }
  const SILBasicBlock *getSuccessBB() const { return DestBBs[0]; }
  SILBasicBlock *getFailureBB() { return DestBBs[1]; }
  const SILBasicBlock *getFailureBB() const { return DestBBs[1]; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::CheckedCastValueBranchInst;
  }
};

/// Perform a checked cast operation and branch on whether the cast succeeds.
/// The result of the checked cast is left in the destination address.
class CheckedCastAddrBranchInst : public TermInst {
  friend SILBuilder;

  CastConsumptionKind ConsumptionKind;

  enum {
    /// the value being stored
    Src,
    /// the lvalue being stored to
    Dest
  };
  FixedOperandList<2> Operands;
  SILSuccessor DestBBs[2];

  CanType SourceType;
  CanType TargetType;

  CheckedCastAddrBranchInst(SILDebugLocation DebugLoc,
                            CastConsumptionKind consumptionKind, SILValue src,
                            CanType srcType, SILValue dest, CanType targetType,
                            SILBasicBlock *successBB, SILBasicBlock *failureBB)
      : TermInst(ValueKind::CheckedCastAddrBranchInst, DebugLoc),
        ConsumptionKind(consumptionKind), Operands{this, src, dest},
        DestBBs{{this, successBB}, {this, failureBB}}, SourceType(srcType),
        TargetType(targetType) {}

public:
  CastConsumptionKind getConsumptionKind() const { return ConsumptionKind; }

  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  /// Returns the formal type of the source value.
  CanType getSourceType() const { return SourceType; }

  /// Returns the formal target type.
  CanType getTargetType() const { return TargetType; }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  SuccessorListTy getSuccessors() {
    return DestBBs;
  }

  SILBasicBlock *getSuccessBB() { return DestBBs[0]; }
  const SILBasicBlock *getSuccessBB() const { return DestBBs[0]; }
  SILBasicBlock *getFailureBB() { return DestBBs[1]; }
  const SILBasicBlock *getFailureBB() const { return DestBBs[1]; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::CheckedCastAddrBranchInst;
  }
};

/// A private abstract class to store the destinations of a TryApplyInst.
class TryApplyInstBase : public TermInst {
public:
  enum {
    // Map branch targets to block successor indices.
    NormalIdx,
    ErrorIdx
  };
private:
  SILSuccessor DestBBs[2];

protected:
  TryApplyInstBase(ValueKind valueKind, SILDebugLocation Loc,
                   SILBasicBlock *normalBB, SILBasicBlock *errorBB);

public:
  SuccessorListTy getSuccessors() {
    return DestBBs;
  }

  bool isNormalSuccessorRef(SILSuccessor *successor) const {
    assert(successor == &DestBBs[0] || successor == &DestBBs[1]);
    return successor == &DestBBs[0];
  }
  bool isErrorSuccessorRef(SILSuccessor *successor) const {
    assert(successor == &DestBBs[0] || successor == &DestBBs[1]);
    return successor == &DestBBs[1];
  }  

  SILBasicBlock *getNormalBB() { return DestBBs[NormalIdx]; }
  const SILBasicBlock *getNormalBB() const { return DestBBs[NormalIdx]; }
  SILBasicBlock *getErrorBB() { return DestBBs[ErrorIdx]; }
  const SILBasicBlock *getErrorBB() const { return DestBBs[ErrorIdx]; }
};

/// TryApplyInst - Represents the full application of a function that
/// can produce an error.
class TryApplyInst
    : public ApplyInstBase<TryApplyInst, TryApplyInstBase> {
  friend SILBuilder;

  TryApplyInst(SILDebugLocation DebugLoc, SILValue callee,
               SILType substCalleeType, SubstitutionList substitutions,
               ArrayRef<SILValue> args,
               ArrayRef<SILValue> TypeDependentOperands,
               SILBasicBlock *normalBB, SILBasicBlock *errorBB);

  static TryApplyInst *create(SILDebugLocation DebugLoc, SILValue callee,
                              SILType substCalleeType,
                              SubstitutionList substitutions,
                              ArrayRef<SILValue> args, SILBasicBlock *normalBB,
                              SILBasicBlock *errorBB, SILFunction &F,
                              SILOpenedArchetypesState &OpenedArchetypes);

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::TryApplyInst;
  }
};

/// An apply instruction.
class ApplySite {
  SILInstruction *Inst;

protected:
  explicit ApplySite(void *p) : Inst(static_cast<SILInstruction *>(p)) {}

public:
  ApplySite() : Inst(nullptr) {}
  explicit ApplySite(ValueBase *inst)
    : Inst(static_cast<SILInstruction*>(inst)) {
    assert(classof(inst) && "not an apply instruction?");
  }
  ApplySite(ApplyInst *inst) : Inst(inst) {}
  ApplySite(PartialApplyInst *inst) : Inst(inst) {}
  ApplySite(TryApplyInst *inst) : Inst(inst) {}

  SILModule &getModule() const {
    return Inst->getModule();
  }

  static ApplySite isa(ValueBase *inst) {
    return (classof(inst) ? ApplySite(inst) : ApplySite());
  }

  explicit operator bool() const {
    return Inst != nullptr;
  }

  SILInstruction *getInstruction() const { return Inst; }
  SILLocation getLoc() const { return Inst->getLoc(); }
  const SILDebugScope *getDebugScope() const { return Inst->getDebugScope(); }
  SILFunction *getFunction() const { return Inst->getFunction(); }
  SILBasicBlock *getParent() const { return Inst->getParent(); }

#define FOREACH_IMPL_RETURN(OPERATION) do {                             \
    switch (Inst->getKind()) {                                          \
    case ValueKind::ApplyInst:                                          \
      return cast<ApplyInst>(Inst)->OPERATION;                          \
    case ValueKind::PartialApplyInst:                                   \
      return cast<PartialApplyInst>(Inst)->OPERATION;                   \
    case ValueKind::TryApplyInst:                                       \
      return cast<TryApplyInst>(Inst)->OPERATION;                       \
    default:                                                            \
      llvm_unreachable("not an apply instruction!");                    \
    }                                                                   \
  } while (0)

  /// Return the callee operand.
  SILValue getCallee() const {
    FOREACH_IMPL_RETURN(getCallee());
  }

  /// Gets the referenced function by looking through partial apply,
  /// convert_function, and thin to thick function until we find a function_ref.
  SILFunction *getCalleeFunction() const {
    FOREACH_IMPL_RETURN(getCalleeFunction());
  }

  /// Return the referenced function if the callee is a function_ref
  /// instruction.
  SILFunction *getReferencedFunction() const {
    FOREACH_IMPL_RETURN(getReferencedFunction());
  }

  /// Return the type.
  SILType getType() const { return getSubstCalleeConv().getSILResultType(); }

  /// Get the type of the callee without the applied substitutions.
  CanSILFunctionType getOrigCalleeType() const {
    return getCallee()->getType().castTo<SILFunctionType>();
  }
  /// Get the conventions of the callee without the applied substitutions.
  SILFunctionConventions getOrigCalleeConv() const {
    return SILFunctionConventions(getOrigCalleeType(), getModule());
  }

  /// Get the type of the callee with the applied substitutions.
  CanSILFunctionType getSubstCalleeType() const {
    return getSubstCalleeSILType().castTo<SILFunctionType>();
  }
  SILType getSubstCalleeSILType() const {
    FOREACH_IMPL_RETURN(getSubstCalleeSILType());
  }
  /// Get the conventions of the callee with the applied substitutions.
  SILFunctionConventions getSubstCalleeConv() const {
    return SILFunctionConventions(getSubstCalleeType(), getModule());
  }

  /// Check if this is a call of a never-returning function.
  bool isCalleeNoReturn() const {
    FOREACH_IMPL_RETURN(isCalleeNoReturn());
  }

  bool isCalleeThin() const {
    switch (getSubstCalleeType()->getRepresentation()) {
    case SILFunctionTypeRepresentation::CFunctionPointer:
    case SILFunctionTypeRepresentation::Thin:
    case SILFunctionTypeRepresentation::Method:
    case SILFunctionTypeRepresentation::ObjCMethod:
    case SILFunctionTypeRepresentation::WitnessMethod:
    case SILFunctionTypeRepresentation::Closure:
      return true;
    case SILFunctionTypeRepresentation::Block:
    case SILFunctionTypeRepresentation::Thick:
      return false;
    }
  }

  /// True if this application has generic substitutions.
  bool hasSubstitutions() const {
    FOREACH_IMPL_RETURN(hasSubstitutions());
  }

  /// The substitutions used to bind the generic arguments of this function.
  MutableArrayRef<Substitution> getSubstitutions() const {
    FOREACH_IMPL_RETURN(getSubstitutions());
  }

  /// The arguments passed to this instruction.
  MutableArrayRef<Operand> getArgumentOperands() const {
    FOREACH_IMPL_RETURN(getArgumentOperands());
  }

  /// The arguments passed to this instruction.
  OperandValueArrayRef getArguments() const {
    FOREACH_IMPL_RETURN(getArguments());
  }

  /// The number of call arguments.
  unsigned getNumCallArguments() const {
    FOREACH_IMPL_RETURN(getNumCallArguments());
  }

  /// The arguments passed to this instruction, without self.
  OperandValueArrayRef getArgumentsWithoutSelf() const {
    switch (Inst->getKind()) {
    case ValueKind::ApplyInst:
      return cast<ApplyInst>(Inst)->getArgumentsWithoutSelf();
    case ValueKind::TryApplyInst:
      return cast<TryApplyInst>(Inst)->getArgumentsWithoutSelf();
    default:
      llvm_unreachable("not implemented for this instruction!");
    }
  }

  /// Returns the number of arguments for this partial apply.
  unsigned getNumArguments() const { return getArguments().size(); }

  // Get the callee argument index corresponding to the caller's first applied
  // argument. Returns 0 for full applies. May return > 0 for partial applies.
  unsigned getCalleeArgIndexOfFirstAppliedArg() const {
    switch (Inst->getKind()) {
    case ValueKind::ApplyInst:
    case ValueKind::TryApplyInst:
      return 0;
    case ValueKind::PartialApplyInst:
      return getSubstCalleeConv().getNumSILArguments() - getNumArguments();
    default:
      llvm_unreachable("not implemented for this instruction!");
    }
  }

  Operand &getArgumentRef(unsigned i) const { return getArgumentOperands()[i]; }

  /// Return the ith argument passed to this instruction.
  SILValue getArgument(unsigned i) const { return getArguments()[i]; }

  /// Set the ith argument of this instruction.
  void setArgument(unsigned i, SILValue V) const {
    getArgumentOperands()[i].set(V);
  }

  /// Return the self argument passed to this instruction.
  bool hasSelfArgument() const {
    switch (Inst->getKind()) {
    case ValueKind::ApplyInst:
      return cast<ApplyInst>(Inst)->hasSelfArgument();
    case ValueKind::TryApplyInst:
      return cast<TryApplyInst>(Inst)->hasSelfArgument();
    default:
      llvm_unreachable("not implemented for this instruction!");
    }
  }

  /// Return the self argument passed to this instruction.
  SILValue getSelfArgument() const {
    switch (Inst->getKind()) {
    case ValueKind::ApplyInst:
      return cast<ApplyInst>(Inst)->getSelfArgument();
    case ValueKind::TryApplyInst:
      return cast<TryApplyInst>(Inst)->getSelfArgument();
    default:
      llvm_unreachable("not implemented for this instruction!");
    }
  }

  /// Return the self operand passed to this instruction.
  Operand &getSelfArgumentOperand() {
    switch (Inst->getKind()) {
    case ValueKind::ApplyInst:
      return cast<ApplyInst>(Inst)->getSelfArgumentOperand();
    case ValueKind::TryApplyInst:
      return cast<TryApplyInst>(Inst)->getSelfArgumentOperand();
    default:
      llvm_unreachable("not implemented for this instruction!");
    }
  }

#undef FOREACH_IMPL_RETURN

  static ApplySite getFromOpaqueValue(void *p) {
    return ApplySite(p);
  }

  friend bool operator==(ApplySite lhs, ApplySite rhs) {
    return lhs.getInstruction() == rhs.getInstruction();
  }
  friend bool operator!=(ApplySite lhs, ApplySite rhs) {
    return lhs.getInstruction() != rhs.getInstruction();
  }

  static bool classof(const ValueBase *inst) {
    return (inst->getKind() == ValueKind::ApplyInst ||
            inst->getKind() == ValueKind::PartialApplyInst ||
            inst->getKind() == ValueKind::TryApplyInst);
  }
};

/// A full function application.
class FullApplySite : public ApplySite {
  explicit FullApplySite(void *p) : ApplySite(p) {}

public:
  FullApplySite() : ApplySite() {}
  explicit FullApplySite(ValueBase *inst) : ApplySite(inst) {
    assert(classof(inst) && "not an apply instruction?");
  }
  FullApplySite(ApplyInst *inst) : ApplySite(inst) {}
  FullApplySite(TryApplyInst *inst) : ApplySite(inst) {}

  static FullApplySite isa(ValueBase *inst) {
    return (classof(inst) ? FullApplySite(inst) : FullApplySite());
  }

  bool hasIndirectSILResults() const {
    return getSubstCalleeConv().hasIndirectSILResults();
  }

  unsigned getNumIndirectSILResults() const {
    return getSubstCalleeConv().getNumIndirectSILResults();
  }

  OperandValueArrayRef getIndirectSILResults() const {
    return getArguments().slice(0, getNumIndirectSILResults());
  }

  OperandValueArrayRef getArgumentsWithoutIndirectResults() const {
    return getArguments().slice(getNumIndirectSILResults());
  }

  SILArgumentConvention getArgumentConvention(unsigned index) const {
    return getSubstCalleeConv().getSILArgumentConvention(index);
  }

  static FullApplySite getFromOpaqueValue(void *p) {
    return FullApplySite(p);
  }

  static bool classof(const ValueBase *inst) {
    return (inst->getKind() == ValueKind::ApplyInst ||
            inst->getKind() == ValueKind::TryApplyInst);
  }
};

// This is defined out of line to work around the fact that this depends on
// PartialApplyInst being defined, but PartialApplyInst is a subclass of
// ApplyInstBase, so we can not place ApplyInstBase after it.
template <class Impl, class Base>
SILFunction *ApplyInstBase<Impl, Base, false>::getCalleeFunction() const {
  SILValue Callee = getCallee();

  while (true) {
    if (auto *FRI = dyn_cast<FunctionRefInst>(Callee)) {
      return FRI->getReferencedFunction();
    }

    if (auto *PAI = dyn_cast<PartialApplyInst>(Callee)) {
      Callee = PAI->getCallee();
      continue;
    }

    if (auto *TTTFI = dyn_cast<ThinToThickFunctionInst>(Callee)) {
      Callee = TTTFI->getCallee();
      continue;
    }

    if (auto *CFI = dyn_cast<ConvertFunctionInst>(Callee)) {
      Callee = CFI->getConverted();
      continue;
    }

    return nullptr;
  }
}

} // end swift namespace

//===----------------------------------------------------------------------===//
// ilist_traits for SILInstruction
//===----------------------------------------------------------------------===//

namespace llvm {

template <>
struct ilist_traits<::swift::SILInstruction> :
  public ilist_default_traits<::swift::SILInstruction> {
  using SILInstruction = ::swift::SILInstruction;

private:
  swift::SILBasicBlock *getContainingBlock();

  using instr_iterator = simple_ilist<SILInstruction>::iterator;

public:
  static void deleteNode(SILInstruction *V) {
    SILInstruction::destroy(V);
  }

  void addNodeToList(SILInstruction *I);
  void removeNodeFromList(SILInstruction *I);
  void transferNodesFromList(ilist_traits<SILInstruction> &L2,
                             instr_iterator first, instr_iterator last);

private:
  void createNode(const SILInstruction &);
};

// An ApplySite casts like a SILInstruction*.
template<> struct simplify_type<const ::swift::ApplySite> {
  using SimpleType = ::swift::SILInstruction *;
  static SimpleType getSimplifiedValue(const ::swift::ApplySite &Val) {
    return Val.getInstruction();
  }
};
template<> struct simplify_type< ::swift::ApplySite>
  : public simplify_type<const ::swift::ApplySite> {};
template<> struct simplify_type< ::swift::FullApplySite>
  : public simplify_type<const ::swift::ApplySite> {};
template<> struct simplify_type<const ::swift::FullApplySite>
  : public simplify_type<const ::swift::ApplySite> {};


template<> struct DenseMapInfo< ::swift::ApplySite> {
  static ::swift::ApplySite getEmptyKey() {
    return ::swift::ApplySite::getFromOpaqueValue(
      llvm::DenseMapInfo<void *>::getEmptyKey());
  }
  static ::swift::ApplySite getTombstoneKey() {
    return ::swift::ApplySite::getFromOpaqueValue(
      llvm::DenseMapInfo<void *>::getTombstoneKey());
  }
  static unsigned getHashValue( ::swift::ApplySite AS) {
    auto *I = AS.getInstruction();
    return DenseMapInfo< ::swift::SILInstruction *>::getHashValue(I);
  }
  static bool isEqual( ::swift::ApplySite LHS, ::swift::ApplySite RHS) {
    return LHS == RHS;
  }
};

template<> struct DenseMapInfo< ::swift::FullApplySite> {
  static ::swift::FullApplySite getEmptyKey() {
    return ::swift::FullApplySite::getFromOpaqueValue(
      llvm::DenseMapInfo<void*>::getEmptyKey());
  }
  static ::swift::FullApplySite getTombstoneKey() {
    return ::swift::FullApplySite::getFromOpaqueValue(
      llvm::DenseMapInfo<void*>::getTombstoneKey());
  }
  static unsigned getHashValue( ::swift::FullApplySite AS) {
    auto *I = AS.getInstruction();
    return DenseMapInfo< ::swift::SILInstruction *>::getHashValue(I);
  }
  static bool isEqual( ::swift::FullApplySite LHS, ::swift::FullApplySite RHS) {
    return LHS == RHS;
  }
};

} // end llvm namespace

#endif
