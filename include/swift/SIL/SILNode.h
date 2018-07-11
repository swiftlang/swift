//===--- SILNode.h - Node base class for SIL --------------------*- C++ -*-===//
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
// This file defines the SILNode class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILNODE_H
#define SWIFT_SIL_SILNODE_H

#include "llvm/Support/Compiler.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "swift/Basic/InlineBitfield.h"
#include "swift/Basic/LLVM.h"
#include <type_traits>

namespace swift {

class SILBasicBlock;
class SILFunction;
class SILInstruction;
class SILModule;
class SingleValueInstruction;
class ValueBase;

/// An enumeration which contains values for all the nodes in SILNodes.def.
/// Other enumerators, like ValueKind and SILInstructionKind, ultimately
/// take their values from this enumerator.
enum class SILNodeKind {
#define NODE(ID, PARENT) \
  ID,
#define NODE_RANGE(ID, FIRST, LAST) \
  First_##ID = FIRST, \
  Last_##ID = LAST,
#include "swift/SIL/SILNodes.def"
};

enum { NumSILNodeKindBits =
  countBitsUsed(static_cast<unsigned>(SILNodeKind::Last_SILNode)) };

enum class SILInstructionKind : std::underlying_type<SILNodeKind>::type;

/// A SILNode is a node in the use-def graph of a SILFunction.  It is
/// either an instruction or a defined value which can be used by an
/// instruction.  A defined value may be an instruction result, a basic
/// block argument, or the special 'undef' value.
///
/// The 'node' intuition is slightly imprecise because a single instruction
/// may be composed of multiple SILNodes: one for the instruction itself
/// and one for each value it produces.  When an instruction kind always
/// produces exactly one value, the cast machinery (isa, cast, and dyn_cast)
/// works to make both nodes appear to be the same object: there is a value
/// kind exactly equal to the instruction kind and the value node can be
/// directly cast to the instruction's class.  When an instruction kind
/// never produces values, it has no corresponding value kind, and it is
/// a compile-time error to attempt to cast a value node to the instruction
/// class.  When an instruction kind can have multiple values (not yet
/// implemented), its value nodes have a different kind from the
/// instruction kind and it is a static error to attempt to cast a value
/// node to the instruction kind.
///
/// Another way of interpreting SILNode is that there is a SILNode for
/// everything that can be numbered in SIL assembly (plus 'undef', which
/// is not conventionally numbered).  Instructions without results are
/// still numbered in SIL in order to describe the users lists of an
/// instruction or argument.  Instructions with multiple results are
/// numbered using their first result.
///
/// SILNode is a base class of both SILInstruction and ValueBase.
/// Because there can be multiple SILNodes within a single instruction
/// object, some care must be taken when working with SILNode pointers.
/// These precautions only apply to SILNode* and not its subclasses.
///
/// - There may have multiple SILNode* values that refer to the same
///   instruction.  Data structures and algorithms that rely on uniqueness of a
///   SILNode* should generally make sure that they're working with the
///   representative SILNode*; see getRepresentativeSILNodeInObject().
///
/// - Do not use builtin C++ casts to downcast a SILNode*.  A static_cast
///   from SILNode* to SILInstruction* only works if the referenced
///   SILNode is the base subobject of the object's SILInstruction
///   subobject.  If the SILNode is actually the base subobject of a
///   ValueBase subobject, the cast will yield a corrupted value.
///   Always use the LLVM casts (cast<>, dyn_cast<>, etc.) instead.
class alignas(8) SILNode {
public:
  enum { NumVOKindBits = 3 };
  enum { NumStoreOwnershipQualifierBits = 2 };
  enum { NumLoadOwnershipQualifierBits = 2 };
  enum { NumSILAccessKindBits = 2 };
  enum { NumSILAccessEnforcementBits = 2 };

protected:
  union { uint64_t OpaqueBits;

  SWIFT_INLINE_BITFIELD_BASE(SILNode, bitmax(NumSILNodeKindBits,8)+1+1,
    Kind : bitmax(NumSILNodeKindBits,8),
    StorageLoc : 1,
    IsRepresentativeNode : 1
  );

  SWIFT_INLINE_BITFIELD_EMPTY(ValueBase, SILNode);

  SWIFT_INLINE_BITFIELD(SILArgument, ValueBase, NumVOKindBits,
    VOKind : NumVOKindBits
  );

  // No MultipleValueInstructionResult subclass needs inline bits right now,
  // therefore let's naturally align and size the Index for speed.
  SWIFT_INLINE_BITFIELD_FULL(MultipleValueInstructionResult, ValueBase,
                             NumVOKindBits+32,
      VOKind : NumVOKindBits,
      : NumPadBits,
      Index : 32
  );

  SWIFT_INLINE_BITFIELD_EMPTY(SILInstruction, SILNode);

  // Special handling for UnaryInstructionWithTypeDependentOperandsBase
  SWIFT_INLINE_BITFIELD(IBWTO, SILNode, 64-NumSILNodeBits,
    // DO NOT allocate bits at the front!
    // IBWTO is a template, and templates must allocate bits from back to front
    // so that "normal" subclassing can allocate bits from front to back.
    // If you update this, then you must update the IBWTO_BITFIELD macros.

    /*pad*/ : 32-NumSILNodeBits,

    // Total number of operands of this instruction.
    // It is number of type dependent operands + 1.
    NumOperands : 32;
    template<SILInstructionKind Kind, typename, typename, typename...>
    friend class InstructionBaseWithTrailingOperands
  );

#define IBWTO_BITFIELD(T, U, C, ...) \
  SWIFT_INLINE_BITFIELD_TEMPLATE(T, U, (C), 32, __VA_ARGS__)
#define IBWTO_BITFIELD_EMPTY(T, U) \
  SWIFT_INLINE_BITFIELD_EMPTY(T, U)

#define UIWTDOB_BITFIELD(T, U, C, ...) \
  IBWTO_BITFIELD(T, U, (C), __VA_ARGS__)
#define UIWTDOB_BITFIELD_EMPTY(T, U) \
  IBWTO_BITFIELD_EMPTY(T, U)

  SWIFT_INLINE_BITFIELD_EMPTY(SingleValueInstruction, SILInstruction);
  SWIFT_INLINE_BITFIELD_EMPTY(DeallocationInst, SILInstruction);
  SWIFT_INLINE_BITFIELD_EMPTY(LiteralInst, SingleValueInstruction);
  SWIFT_INLINE_BITFIELD_EMPTY(AllocationInst, SingleValueInstruction);

  // Ensure that StructInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(StructInst, SingleValueInstruction);

  // Ensure that TupleInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(TupleInst, SingleValueInstruction);

  IBWTO_BITFIELD(ObjectInst, SingleValueInstruction,
                             32-NumSingleValueInstructionBits,
    NumBaseElements : 32-NumSingleValueInstructionBits
  );

  IBWTO_BITFIELD(SelectEnumInstBase, SingleValueInstruction, 1,
    HasDefault : 1
  );

  SWIFT_INLINE_BITFIELD_FULL(IntegerLiteralInst, LiteralInst, 32,
    : NumPadBits,
    numBits : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(FloatLiteralInst, LiteralInst, 32,
    : NumPadBits,
    numBits : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(StringLiteralInst, LiteralInst, 2+32,
    TheEncoding : 2,
    : NumPadBits,
    Length : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(ConstStringLiteralInst, LiteralInst, 1+32,
    TheEncoding : 1,
    : NumPadBits,
    Length : 32
  );

  SWIFT_INLINE_BITFIELD(DeallocRefInst, DeallocationInst, 1,
    OnStack : 1
  );

  // Ensure that AllocBoxInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(AllocBoxInst, AllocationInst);
  // Ensure that AllocExistentialBoxInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(AllocExistentialBoxInst, AllocationInst);
  SWIFT_INLINE_BITFIELD_FULL(AllocStackInst, AllocationInst,
                             64-NumAllocationInstBits,
    NumOperands : 32-NumAllocationInstBits,
    VarInfo : 32
  );
  IBWTO_BITFIELD(AllocRefInstBase, AllocationInst, 32-NumAllocationInstBits,
    ObjC : 1,
    OnStack : 1,
    NumTailTypes : 32-1-1-NumAllocationInstBits
  );
  static_assert(32-1-1-NumAllocationInstBits >= 16, "Reconsider bitfield use?");

  UIWTDOB_BITFIELD_EMPTY(AllocValueBufferInst, AllocationInst);

  // TODO: Sort the following in SILNodes.def order

  SWIFT_INLINE_BITFIELD_EMPTY(NonValueInstruction, SILInstruction);
  SWIFT_INLINE_BITFIELD(RefCountingInst, NonValueInstruction, 1,
      atomicity : 1
  );
  SWIFT_INLINE_BITFIELD(StrongPinInst, SingleValueInstruction, 1,
      atomicity : 1
  );

  // Ensure that BindMemoryInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(BindMemoryInst, NonValueInstruction);

  // Ensure that MarkFunctionEscapeInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(MarkFunctionEscapeInst, NonValueInstruction);

  // Ensure that MetatypeInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(MetatypeInst, SingleValueInstruction);

  SWIFT_INLINE_BITFIELD(CopyAddrInst, NonValueInstruction, 1+1,
    /// IsTakeOfSrc - True if ownership will be taken from the value at the
    /// source memory location.
    IsTakeOfSrc : 1,

    /// IsInitializationOfDest - True if this is the initialization of the
    /// uninitialized destination memory location.
    IsInitializationOfDest : 1
  );

  SWIFT_INLINE_BITFIELD(LoadReferenceInstBaseT, NonValueInstruction, 1,
    IsTake : 1;
    template<SILInstructionKind K>
    friend class LoadReferenceInstBase
  );

  SWIFT_INLINE_BITFIELD(StoreReferenceInstBaseT, NonValueInstruction, 1,
    IsInitializationOfDest : 1;
    template<SILInstructionKind K>
    friend class StoreReferenceInstBase
  );

  SWIFT_INLINE_BITFIELD(BeginAccessInst, SingleValueInstruction,
                        NumSILAccessKindBits+NumSILAccessEnforcementBits
                        + 1 + 1,
    AccessKind : NumSILAccessKindBits,
    Enforcement : NumSILAccessEnforcementBits,
    NoNestedConflict : 1,
    FromBuiltin : 1
  );
  SWIFT_INLINE_BITFIELD(BeginUnpairedAccessInst, NonValueInstruction,
                        NumSILAccessKindBits + NumSILAccessEnforcementBits
                        + 1 + 1,
                        AccessKind : NumSILAccessKindBits,
                        Enforcement : NumSILAccessEnforcementBits,
                        NoNestedConflict : 1,
                        FromBuiltin : 1);

  SWIFT_INLINE_BITFIELD(EndAccessInst, NonValueInstruction, 1,
    Aborting : 1
  );
  SWIFT_INLINE_BITFIELD(EndUnpairedAccessInst, NonValueInstruction,
                        NumSILAccessEnforcementBits + 1 + 1,
                        Enforcement : NumSILAccessEnforcementBits,
                        Aborting : 1,
                        FromBuiltin : 1);

  SWIFT_INLINE_BITFIELD(StoreInst, NonValueInstruction,
                        NumStoreOwnershipQualifierBits,
    OwnershipQualifier : NumStoreOwnershipQualifierBits
  );
  SWIFT_INLINE_BITFIELD(LoadInst, SingleValueInstruction,
                        NumLoadOwnershipQualifierBits,
    OwnershipQualifier : NumLoadOwnershipQualifierBits
  );

  SWIFT_INLINE_BITFIELD(UncheckedOwnershipConversionInst,SingleValueInstruction,
                        NumVOKindBits,
    Kind : NumVOKindBits
  );

  SWIFT_INLINE_BITFIELD_FULL(TupleExtractInst, SingleValueInstruction, 32,
    : NumPadBits,
    FieldNo : 32
  );
  SWIFT_INLINE_BITFIELD_FULL(TupleElementAddrInst, SingleValueInstruction, 32,
    : NumPadBits,
    FieldNo : 32
  );

  SWIFT_INLINE_BITFIELD_EMPTY(MethodInst, SingleValueInstruction);
  // Ensure that WitnessMethodInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(WitnessMethodInst, MethodInst);
  UIWTDOB_BITFIELD_EMPTY(ObjCMethodInst, MethodInst);

  SWIFT_INLINE_BITFIELD_EMPTY(ConversionInst, SingleValueInstruction);
  SWIFT_INLINE_BITFIELD(PointerToAddressInst, ConversionInst, 1+1,
    IsStrict : 1,
    IsInvariant : 1
  );

  UIWTDOB_BITFIELD_EMPTY(ConvertFunctionInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(PointerToThinFunctionInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(UnconditionalCheckedCastInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(UpcastInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(UncheckedRefCastInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(UncheckedAddrCastInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(UncheckedTrivialBitCastInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(UncheckedBitwiseCastInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(ThinToThickFunctionInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(UnconditionalCheckedCastValueInst, ConversionInst);
  UIWTDOB_BITFIELD_EMPTY(InitExistentialAddrInst, SingleValueInstruction);
  UIWTDOB_BITFIELD_EMPTY(InitExistentialValueInst, SingleValueInstruction);
  UIWTDOB_BITFIELD_EMPTY(InitExistentialRefInst, SingleValueInstruction);
  UIWTDOB_BITFIELD_EMPTY(InitExistentialMetatypeInst, SingleValueInstruction);

  SWIFT_INLINE_BITFIELD_EMPTY(TermInst, SILInstruction);
  UIWTDOB_BITFIELD_EMPTY(CheckedCastBranchInst, SingleValueInstruction);
  UIWTDOB_BITFIELD_EMPTY(CheckedCastValueBranchInst, SingleValueInstruction);

  // Ensure that BranchInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(BranchInst, TermInst);
  // Ensure that YieldInst bitfield does not overflow.
  IBWTO_BITFIELD_EMPTY(YieldInst, TermInst);
  IBWTO_BITFIELD(CondBranchInst, TermInst, 32-NumTermInstBits,
    NumTrueArgs : 32-NumTermInstBits
  );
  IBWTO_BITFIELD(SwitchValueInst, TermInst, 1,
    HasDefault : 1
  );
  SWIFT_INLINE_BITFIELD_FULL(SwitchEnumInstBase, TermInst, 1+32,
    HasDefault : 1,
    : NumPadBits,
    NumCases : 32
  );

  } Bits;

  enum class SILNodeStorageLocation : uint8_t { Value, Instruction };

  enum class IsRepresentative : bool {
    No = false,
    Yes = true,
  };

private:

  SILNodeStorageLocation getStorageLoc() const {
    return SILNodeStorageLocation(Bits.SILNode.StorageLoc);
  }

  const SILNode *getRepresentativeSILNodeSlowPath() const;

protected:
  SILNode(SILNodeKind kind, SILNodeStorageLocation storageLoc,
          IsRepresentative isRepresentative) {
    Bits.OpaqueBits = 0;
    Bits.SILNode.Kind = unsigned(kind);
    Bits.SILNode.StorageLoc = unsigned(storageLoc);
    Bits.SILNode.IsRepresentativeNode = unsigned(isRepresentative);
  }

public:
  /// Does the given kind of node inherit from multiple multiple SILNode base
  /// classes?
  ///
  /// This enables one to know if their is a diamond in the inheritence
  /// hierarchy for this SILNode.
  static bool hasMultipleSILNodeBases(SILNodeKind kind) {
    // Currently only SingleValueInstructions.  Note that multi-result
    // instructions shouldn't return true for this.
    return kind >= SILNodeKind::First_SingleValueInstruction &&
           kind <= SILNodeKind::Last_SingleValueInstruction;
  }

  /// Is this SILNode the representative SILNode subobject in this object?
  bool isRepresentativeSILNodeInObject() const {
    return Bits.SILNode.IsRepresentativeNode;
  }

  /// Return a pointer to the representative SILNode subobject in this object.
  SILNode *getRepresentativeSILNodeInObject() {
    if (isRepresentativeSILNodeInObject())
      return this;
    return const_cast<SILNode *>(getRepresentativeSILNodeSlowPath());
  }

  const SILNode *getRepresentativeSILNodeInObject() const {
    if (isRepresentativeSILNodeInObject())
      return this;
    return getRepresentativeSILNodeSlowPath();
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  SILNodeKind getKind() const {
    return SILNodeKind(Bits.SILNode.Kind);
  }

  /// Return the SILNodeKind of this node's representative SILNode.
  SILNodeKind getKindOfRepresentativeSILNodeInObject() const {
    return getRepresentativeSILNodeInObject()->getKind();
  }

  /// If this is a SILArgument or a SILInstruction get its parent basic block,
  /// otherwise return null.
  SILBasicBlock *getParentBlock() const;

  /// If this is a SILArgument or a SILInstruction get its parent function,
  /// otherwise return null.
  SILFunction *getFunction() const;

  /// If this is a SILArgument or a SILInstruction get its parent module,
  /// otherwise return null.
  SILModule *getModule() const;

  /// Pretty-print the node.  If the node is an instruction, the output
  /// will be valid SIL assembly; otherwise, it will be an arbitrary
  /// format suitable for debugging.
  void print(raw_ostream &OS) const;
  void dump() const;

  /// Pretty-print the node in context, preceded by its operands (if the
  /// value represents the result of an instruction) and followed by its
  /// users.
  void printInContext(raw_ostream &OS) const;
  void dumpInContext() const;

  // Cast to SingleValueInstruction.  This is an implementation detail
  // of the cast machinery.  At a high level, all you need to know is to
  // never use static_cast to downcast a SILNode.
  SingleValueInstruction *castToSingleValueInstruction();
  const SingleValueInstruction *castToSingleValueInstruction() const {
    return const_cast<SILNode*>(this)->castToSingleValueInstruction();
  }

  static bool classof(const SILNode *node) {
    return true;
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                     const SILNode &node) {
  node.print(OS);
  return OS;
}

template <class To> struct cast_sil_node_is_unambiguous {
  // The only ambiguity right now is between the value and instruction
  // nodes on a SingleValueInstruction.
  static constexpr bool value =
    // If the destination type isn't a subclass of ValueBase or
    // SILInstruction, there's no ambiguity.
       (!std::is_base_of<SILInstruction, To>::value &&
        !std::is_base_of<ValueBase, To>::value)

    // If the destination type is a proper subclass of ValueBase
    // that isn't a subclass of SILInstruction, there's no ambiguity.
    || (std::is_base_of<ValueBase, To>::value &&
        !std::is_same<ValueBase, To>::value &&
        !std::is_base_of<SILInstruction, To>::value)

    // If the destination type is a proper subclass of SILInstruction
    // that isn't a subclass of ValueBase, there's no ambiguity.
    || (std::is_base_of<SILInstruction, To>::value &&
        !std::is_same<SILInstruction, To>::value &&
        !std::is_base_of<ValueBase, To>::value);
};

template <class To,
          bool IsSingleValueInstruction =
            std::is_base_of<SingleValueInstruction, To>::value,
          bool IsKnownUnambiguous =
            cast_sil_node_is_unambiguous<To>::value>
struct cast_sil_node;

// If all complete objects of the destination type are known to only
// contain a single node, we can just use a static_cast.
template <class To>
struct cast_sil_node<To, /*single value*/ false, /*unambiguous*/ true> {
  static To *doit(SILNode *node) {
    return &static_cast<To&>(*node);
  }
};

// If we're casting to a subclass of SingleValueInstruction, we don't
// need to dynamically check whether the node is an SVI.  In fact,
// we can't, because the static_cast will be ambiguous.
template <class To>
struct cast_sil_node<To, /*single value*/ true, /*unambiguous*/ false> {
  static To *doit(SILNode *node) {
    auto svi = node->castToSingleValueInstruction();
    return &static_cast<To&>(*svi);
  }
};

// Otherwise, we need to dynamically check which case we're in.
template <class To>
struct cast_sil_node<To, /*single value*/ false, /*unambiguous*/ false> {
  static To *doit(SILNode *node) {
    // If the node isn't dynamically a SingleValueInstruction, then this
    // is indeed the SILNode subobject that's statically observable in To.
    if (!SILNode::hasMultipleSILNodeBases(node->getKind())) {
      return &static_cast<To&>(*node);
    }

    auto svi = node->castToSingleValueInstruction();
    return &static_cast<To&>(*svi);
  }
};

} // end namespace swift

namespace llvm {

/// Completely take over cast<>'ing from SILNode*.  A static_cast to
/// ValueBase* or SILInstruction* can be quite wrong.
template <class To>
struct cast_convert_val<To, swift::SILNode*, swift::SILNode*> {
  using ret_type = typename cast_retty<To, swift::SILNode*>::ret_type;
  static ret_type doit(swift::SILNode *node) {
    return swift::cast_sil_node<To>::doit(node);
  }
};
template <class To>
struct cast_convert_val<To, const swift::SILNode *, const swift::SILNode *> {
  using ret_type = typename cast_retty<To, const swift::SILNode*>::ret_type;
  static ret_type doit(const swift::SILNode *node) {
    return swift::cast_sil_node<To>::doit(const_cast<swift::SILNode*>(node));
  }
};

// We don't support casting from SILNode references yet.
template <class To, class From>
struct cast_convert_val<To, swift::SILNode, From>;
template <class To, class From>
struct cast_convert_val<To, const swift::SILNode, From>;

/// ValueBase * is always at least eight-byte aligned; make the three tag bits
/// available through PointerLikeTypeTraits.
template<>
struct PointerLikeTypeTraits<swift::SILNode *> {
public:
  static inline void *getAsVoidPointer(swift::SILNode *I) {
    return (void*)I;
  }
  static inline swift::SILNode *getFromVoidPointer(void *P) {
    return (swift::SILNode *)P;
  }
  enum { NumLowBitsAvailable = 3 };
};

} // end namespace llvm

#endif
