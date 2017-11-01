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

enum {
  NumSILNodeKindBits = 8
};
static_assert(unsigned(SILNodeKind::Last_SILNode) < (1 << NumSILNodeKindBits),
              "SILNodeKind fits in NumSILNodeKindBits bits");

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
  /// The assumed number of bits that a SILNode+padding will take up. Public for
  /// static assertion purposes.
  static constexpr unsigned NumTotalSILNodeBits = 64;

private:
  static constexpr unsigned NumKindBits = NumSILNodeKindBits;
  static constexpr unsigned NumStorageLocBits = 1;
  static constexpr unsigned NumIsRepresentativeBits = 1;

protected:
  static constexpr unsigned NumSubclassDataBits =
      NumTotalSILNodeBits - NumKindBits - NumStorageLocBits -
      NumIsRepresentativeBits;

  enum class SILNodeStorageLocation : uint8_t { Value, Instruction };

  enum class IsRepresentative : uint8_t {
    No = 0,
    Yes = 1,
  };

private:
  const uint64_t Kind : NumKindBits;
  const uint64_t StorageLoc : NumStorageLocBits;
  const uint64_t IsRepresentativeNode : NumIsRepresentativeBits;
  uint64_t SubclassData : NumSubclassDataBits;

  SILNodeStorageLocation getStorageLoc() const {
    return SILNodeStorageLocation(StorageLoc);
  }

  const SILNode *getRepresentativeSILNodeSlowPath() const;

protected:
  SILNode(SILNodeKind kind, SILNodeStorageLocation storageLoc,
          IsRepresentative isRepresentative)
      : Kind(unsigned(kind)), StorageLoc(unsigned(storageLoc)),
        IsRepresentativeNode(unsigned(isRepresentative)), SubclassData(0) {}

  uint64_t getSubclassData() const { return SubclassData; }

  void setSubclassData(uint64_t NewData) {
    assert(!(NewData & ~((uint64_t(1) << NumSubclassDataBits) - 1)) &&
           "New subclass data is too large to fit in SubclassData");
    SubclassData = NewData;
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
  bool isRepresentativeSILNodeInObject() const { return IsRepresentativeNode; }

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
    return SILNodeKind(Kind);
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
class PointerLikeTypeTraits<swift::SILNode *> {
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
