//===--- Instruction.h - Instructions for high-level SIL code ---*- C++ -*-===//
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
// This file defines the high-level Instruction class used for Swift SIL code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_INSTRUCTION_H
#define SWIFT_SIL_INSTRUCTION_H

#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILSuccessor.h"
#include "swift/SIL/Value.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"

namespace swift {

class ValueDecl;
class Type;
class Function;
class BasicBlock;
class CharacterLiteralExpr;
class DeclRefExpr;
class FloatLiteralExpr;
class IntegerLiteralExpr;
class MetatypeExpr;
class StringLiteralExpr;
class Stmt;
class VarDecl;

/// This is the root class for all instructions that can be used as the contents
/// of a Swift BasicBlock.
class Instruction : public ValueBase, public llvm::ilist_node<Instruction> {
  friend struct llvm::ilist_traits<Instruction>;

  /// A backreference to the containing basic block.  This is maintained by
  /// ilist_traits<Instruction>.
  BasicBlock *ParentBB;

  SILLocation Loc;

  friend struct llvm::ilist_sentinel_traits<Instruction>;
  Instruction() = delete;
  void operator=(const Instruction &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

protected:
  Instruction(ValueKind Kind, SILLocation Loc, Type Ty)
    : ValueBase(Kind, Ty), ParentBB(0), Loc(Loc) {}
  Instruction(ValueKind Kind, SILLocation Loc, SILTypeList *TypeList = 0)
    : ValueBase(Kind, TypeList), ParentBB(0), Loc(Loc) {}

public:

  const BasicBlock *getParent() const { return ParentBB; }
  BasicBlock *getParent() { return ParentBB; }

  SILLocation getLoc() const { return Loc; }

  /// Return the AST expression that this instruction is produced from, or null
  /// if it is implicitly generated.  Note that this is aborts on locations that
  /// come from statements.
  template<typename T>
  T *getLocDecl() const { return cast_or_null<T>(Loc.template get<Decl*>()); }

  /// Return the AST expression that this instruction is produced from, or null
  /// if it is implicitly generated.  Note that this is aborts on locations that
  /// come from statements.
  template<typename T>
  T *getLocExpr() const { return cast_or_null<T>(Loc.template get<Expr*>()); }

  /// Return the AST statement that this instruction is produced from, or null
  /// if it is implicitly generated.  Note that this is aborts on locations that
  /// come from statements.
  template<typename T>
  T *getLocStmt() const { return cast_or_null<T>(Loc.template get<Stmt*>()); }


  /// removeFromParent - This method unlinks 'this' from the containing basic
  /// block, but does not delete it.
  ///
  void removeFromParent();
  
  /// eraseFromParent - This method unlinks 'this' from the containing basic
  /// block and deletes it.
  ///
  void eraseFromParent();

  static bool classof(Value V) {
    return V->getKind() >= ValueKind::First_Instruction &&
           V->getKind() <= ValueKind::Last_Instruction;
  }
};

enum class AllocKind : uint8_t {
  Heap, Stack, Pseudo
};

/// AllocInst - This is the abstract base class common among all the memory
/// allocation mechanisms.  This can allocate heap or stack memory.
class AllocInst : public Instruction {
  AllocKind allocKind;

protected:
  AllocInst(ValueKind Kind, SILLocation Loc, Type Ty, AllocKind allocKind)
    : Instruction(Kind, Loc, Ty), allocKind(allocKind) {}
public:
  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  AllocKind getAllocKind() const { return allocKind; }
  
  static bool classof(Value V) {
    return V->getKind() >= ValueKind::First_AllocInst &&
           V->getKind() <= ValueKind::Last_AllocInst;
  }
};

/// AllocVarInst - This represents the allocation of an unboxed variable or
/// temporary. The memory is provided uninitialized.
class AllocVarInst : public AllocInst {
public:
  AllocVarInst(VarDecl *VD);
  AllocVarInst(SILLocation loc, AllocKind allocKind, Type elementType);

  /// getDecl - Return the underlying variable declaration associated with this
  /// allocation, or null if this is a temporary allocation.
  VarDecl *getDecl() const;
  
  /// getElementType - Get the type of the allocated memory (as opposed to the
  /// type of the instruction itself, which will be an address type).
  Type getElementType() const;

  static bool classof(Value V) {
    return V->getKind() == ValueKind::AllocVarInst;
  }
};

/// AllocBoxInst - This represents the allocation of a heap box for a Swift
/// value of some type, and whose element memory is left uninitialized.  This
/// returns two values.  The first return element is the object pointer (pointer
/// to the object header) with Builtin.ObjectPointer type.  The second element
/// returned is an lvalue to the element.
///
class AllocBoxInst : public Instruction {
  Type ElementType;
public:
  AllocBoxInst(SILLocation Loc, Type ElementType, SILBase &B);

  Type getElementType() const { return ElementType; }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::AllocBoxInst;
  }
};


/// AllocArrayInst - This represents the allocation of an array of elements,
/// whose element memory is left uninitialized.  This returns two values.  The
/// first return element is the object pointer (pointer to the object
/// header) with Builtin.ObjectPointer type.  The second element returned is an
/// lvalue to the first array element.
///
class AllocArrayInst : public Instruction {
  Type ElementType;
  Value NumElements;
public:

  AllocArrayInst(SILLocation Loc, Type ElementType, Value NumElements,
                 SILBase &B);

  Type getElementType() const { return ElementType; }
  Value getNumElements() const { return NumElements; }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::AllocArrayInst;
  }
};


/// ApplyInst - Represents application of an argument to a function.
class ApplyInst : public Instruction {
  /// The instruction representing the called function.
  Value Callee;

  unsigned NumArgs;
  Value *getArgsStorage() { return reinterpret_cast<Value*>(this + 1); }
  
  /// Construct an ApplyInst from a given call expression and the provided
  /// arguments.
  ApplyInst(SILLocation Loc, Type Ty, Value Callee, ArrayRef<Value> Args);

public:
  static ApplyInst *create(SILLocation Loc, Value Callee,
                           ArrayRef<Value> Args, Function &F);
  
  Value getCallee() { return Callee; }
  
  /// The arguments passed to this ApplyInst.
  MutableArrayRef<Value> getArguments() {
    return MutableArrayRef<Value>(getArgsStorage(), NumArgs);
  }

  /// The arguments passed to this ApplyInst.
  ArrayRef<Value> getArguments() const {
    return const_cast<ApplyInst*>(this)->getArguments();
  }

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::ApplyInst;
  }
};

/// ConstantRefInst - Represents a reference to a *constant* declaration,
/// evaluating to its value.
class ConstantRefInst : public Instruction {
public:

  /// Construct a ConstantRefInst.
  ///
  /// \param Expr A backpointer to the original DeclRefExpr.
  ///
  ConstantRefInst(DeclRefExpr *E);

  DeclRefExpr *getExpr() const;

  /// getDecl - Return the underlying declaration.
  ValueDecl *getDecl() const;

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::ConstantRefInst;
  }
};

/// A default "zero" value used to initialize a variable that was not otherwise
/// explicitly initialized.
class ZeroValueInst : public Instruction {
public:
  ZeroValueInst(SILLocation Loc, Type Ty);

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::ZeroValueInst;
  }
};

/// IntegerLiteralInst - Encapsulates an integer constant, as defined originally
/// by an an IntegerLiteralExpr or CharacterLiteralExpr.
class IntegerLiteralInst : public Instruction {
public:
  IntegerLiteralInst(IntegerLiteralExpr *E);
  IntegerLiteralInst(CharacterLiteralExpr *E);
  
  Expr *getExpr() const;
  
  /// getValue - Return the APInt for the underlying integer literal.
  APInt getValue() const;

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::IntegerLiteralInst;
  }
};

/// FloatLiteralInst - Encapsulates a floating point constant, as defined
/// originally by a FloatLiteralExpr.
class FloatLiteralInst : public Instruction {
public:
  FloatLiteralInst(FloatLiteralExpr *E);

  FloatLiteralExpr *getExpr() const;

  /// getValue - Return the APFloat for the underlying FP literal.
  APFloat getValue() const;

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::FloatLiteralInst;
  }
};

/// StringLiteralInst - Encapsulates a string constant, as defined originally by
/// a StringLiteralExpr.
class StringLiteralInst : public Instruction {
public:
  StringLiteralInst(StringLiteralExpr *E);

  StringLiteralExpr *getExpr() const;

  /// getValue - Return the string data for the literal.
  StringRef getValue() const;

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::StringLiteralInst;
  }
};


/// LoadInst - Represents a load from a memory location.
class LoadInst : public Instruction {
  /// The LValue (memory address) to use for the load.
  Value LValue;
public:
  /// Constructs a LoadInst.
  ///
  /// \param Expr The backing LoadExpr in the AST.
  ///
  /// \param LValue The Value representing the lvalue (address) to
  ///        use for the load.
  LoadInst(SILLocation Loc, Value LValue);

  Value getLValue() const { return LValue; }

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::LoadInst;
  }
};

/// StoreInst - Represents a store from a memory location.
class StoreInst : public Instruction {
  /// The value being stored and the lvalue being stored to.
  Value Src, Dest;
public:

  StoreInst(SILLocation Loc, Value Src, Value Dest);

  Value getSrc() const { return Src; }
  Value getDest() const { return Dest; }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::StoreInst;
  }
};
  
/// CopyAddrInst - Represents a copy from one memory location to another. This
/// is similar to:
///   %1 = load %src
///   store %1 -> %dest
/// but a copy instruction can be used with types that cannot be loaded, such as
/// resilient value types.
class CopyAddrInst : public Instruction {
  /// Src - The lvalue being loaded from.
  Value Src;
  /// Dest - The lvalue being stored to.
  Value Dest;
  
  /// IsTakeOfSrc - True if ownership will be taken from the value at the source
  /// memory location.
  bool IsTakeOfSrc : 1;
  /// IsInitializationOfDest - True if this is the initialization of the
  /// uninitialized destination memory location.
  bool IsInitializationOfDest : 1;
  
public:
  CopyAddrInst(SILLocation Loc, Value Src, Value Dest,
               bool IsTakeOfSrc, bool IsInitializationOfDest);
  
  Value getSrc() const { return Src; }
  Value getDest() const { return Dest; }
  bool isTakeOfSrc() const { return IsTakeOfSrc; }
  bool isInitializationOfDest() const { return IsInitializationOfDest; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::CopyAddrInst;
  }
};

/// SpecializeInst - Specializes a reference to a generic entity by binding
/// each of its type parameters to a specific type.
///
/// This instruction takes an arbitrary value of generic type and returns a new
/// closure that takes concrete types for its archetypes.  This is commonly used
/// in call sequences to generic functions, but can occur in arbitrarily general
/// cases as well.
///
class SpecializeInst : public Instruction {
  /// The value being specialized.  It always has FunctionType.
  Value Operand;
public:

  SpecializeInst(SILLocation Loc, Value Operand, Type DestTy);

  Value getOperand() const { return Operand; }

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::SpecializeInst;
  }
};


/// ConvertInst - Change the Type of some value without affecting how it
/// will codegen.
class ConvertInst : public Instruction {
  Value Operand;
public:
  ConvertInst(SILLocation Loc, Value Operand, Type Ty);

  Value getOperand() const { return Operand; }

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::ConvertInst;
  }
};


/// TupleInst - Represents a constructed tuple.
class TupleInst : public Instruction {
  Value *getElementsStorage() {
    return reinterpret_cast<Value*>(this + 1);
  }
  unsigned NumArgs;

  /// Private constructor.  Because of the storage requirements of
  /// TupleInst, object creation goes through 'create()'.
  TupleInst(SILLocation Loc, Type Ty, ArrayRef<Value> Elements);
  static TupleInst *createImpl(SILLocation Loc, Type Ty,
                               ArrayRef<Value> Elements, Function &F);

public:
  /// The elements referenced by this TupleInst.
  MutableArrayRef<Value> getElements() {
    return MutableArrayRef<Value>(getElementsStorage(), NumArgs);
  }

  /// The elements referenced by this TupleInst.
  ArrayRef<Value> getElements() const {
    return const_cast<TupleInst*>(this)->getElements();
  }

  /// Construct a TupleInst.
  static TupleInst *create(SILLocation Loc, Type Ty, ArrayRef<Value> Elements,
                           Function &F) {
    return createImpl(SILLocation(), Ty, Elements, F);
  }

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::TupleInst;
  }
};

/// MetatypeInst - Represents the production of an instance of a given metatype.
///
/// FIXME: base!
class MetatypeInst : public Instruction {
public:

  /// Constructs a TypeOfInst.
  ///
  /// \param Expr A backpointer to the original MetatypeExpr.
  ///
  MetatypeInst(MetatypeExpr *E);

  MetatypeExpr *getExpr() const;

  /// getMetaType - Return the type of the metatype that this instruction
  /// returns.
  Type getMetaType() const;

  static bool classof(Value V) {
    return V->getKind() == ValueKind::MetatypeInst;
  }
};


/// ExtractInst - Extract a numbered element out of a value of tuple or fragile
/// struct type.
class ExtractInst : public Instruction {
  Value Operand;
  unsigned FieldNo;
public:
  ExtractInst(SILLocation Loc, Value Operand, unsigned FieldNo,
                   Type ResultTy);
  
  Value getOperand() const { return Operand; }
  unsigned getFieldNo() const { return FieldNo; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ExtractInst;
  }
};

/// RetainInst - Increase the retain count of a value.
class RetainInst : public Instruction {
  Value Operand;
public:
  RetainInst(SILLocation Loc, Value Operand);
  
  Value getOperand() const { return Operand; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::RetainInst;
  }
};

/// ReleaseInst - Decrease the retain count of a value, and dealloc the value
/// if its retain count is zero.
class ReleaseInst : public Instruction {
  Value Operand;
public:
  ReleaseInst(SILLocation Loc, Value Operand);
  
  Value getOperand() const { return Operand; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ReleaseInst;
  }
};

/// DeallocVarInst - Deallocate memory allocated by alloc_var.
class DeallocVarInst : public Instruction {
  AllocKind allocKind;
  Value Operand;
public:
  DeallocVarInst(SILLocation Loc, AllocKind allocKind, Value Operand);
  
  AllocKind getAllocKind() const { return allocKind; }
  Value getOperand() const { return Operand; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::DeallocVarInst;
  }
};

/// DestroyAddrInst - Destroy the value at a memory location and deallocate the
/// memory. This is similar to:
///   %1 = load %operand
///   release %1
///   dealloc %operand
/// but a destroy instruction can be used for types that cannot be loaded,
/// such as resilient value types.
class DestroyAddrInst : public Instruction {
  Value Operand;
public:
  DestroyAddrInst(SILLocation Loc, Value Operand);
  
  Value getOperand() const { return Operand; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ReleaseInst;
  }
};

//===----------------------------------------------------------------------===//
// SIL-only instructions that don't have an AST analog
//===----------------------------------------------------------------------===//


/// IndexAddrInst - "%1 = index_addr %0, 42"
/// This takes an lvalue and indexes over the pointer, striding by the type of
/// the lvalue.  This is used to index into arrays of uniform elements.
class IndexAddrInst : public Instruction {
  Value Operand;
  unsigned Index;
public:
  IndexAddrInst(SILLocation Loc, Value Operand, unsigned Index);

  Value getOperand() const { return Operand; }
  unsigned getIndex() const { return Index; }

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::IndexAddrInst;
  }
};

/// IntegerValueInst - Always produces an integer of the specified value.  These
/// always have Builtin.Integer type.
class IntegerValueInst : public Instruction {
  uint64_t Val;
public:
  IntegerValueInst(uint64_t Val, Type Ty);

  uint64_t getValue() const { return Val; }

  /// getType() is ok since this is known to only have one type.
  Type getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::IntegerValueInst;
  }
};


//===----------------------------------------------------------------------===//
// Instructions representing terminators
//===----------------------------------------------------------------------===//

/// This class defines a "terminating instruction" for a BasicBlock.
class TermInst : public Instruction {
protected:
  TermInst(ValueKind K, SILLocation Loc, Type Ty) : Instruction(K, Loc, Ty) {}
  TermInst(ValueKind K, SILLocation Loc) : Instruction(K, Loc) {}
public:

  typedef llvm::ArrayRef<SILSuccessor> SuccessorListTy;

  /// The successor basic blocks of this terminator.
  SuccessorListTy getSuccessors();

  /// The successor basic blocks of this terminator.
  const SuccessorListTy getSuccessors() const {
    return const_cast<TermInst*>(this)->getSuccessors();
  }

  static bool classof(Value V) {
    return V->getKind() >= ValueKind::First_TermInst &&
           V->getKind() <= ValueKind::Last_TermInst;
  }
};

/// UnreachableInst - Position in the code which would be undefined to reach.
/// These are always implicitly generated, e.g. when falling off the end of a
/// function or after a no-return function call.
class UnreachableInst : public TermInst {
public:
  UnreachableInst(Function &F);
  
  SuccessorListTy getSuccessors() {
    // No Successors.
    return SuccessorListTy();
  }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::UnreachableInst;
  }
};

/// ReturnInst - Representation of a ReturnStmt.
class ReturnInst : public TermInst {
  /// The value to be returned.  This is never null.
  Value ReturnValue;
  
public:
  /// Constructs a ReturnInst representing an \b explicit return.
  ///
  /// \param returnStmt The backing return statement in the AST.
  ///
  /// \param returnValue The value to be returned.
  ///
  ReturnInst(SILLocation Loc, Value ReturnValue);

  Value getReturnValue() const { return ReturnValue; }

  SuccessorListTy getSuccessors() {
    // No Successors.
    return SuccessorListTy();
  }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::ReturnInst;
  }
};

/// BranchInst - An unconditional branch.
class BranchInst : public TermInst {
  llvm::ArrayRef<Value> Arguments;
  SILSuccessor DestBB;
public:
  typedef ArrayRef<Value> ArgsTy;
  
  /// Construct an BranchInst that will branches to the specified block.
  BranchInst(BasicBlock *DestBB, Function &F);
  
  /// The jump target for the branch.
  BasicBlock *getDestBB() const { return DestBB; }

  SuccessorListTy getSuccessors() {
    return DestBB;
  }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::BranchInst;
  }
};

class CondBranchInst : public TermInst {
  /// The condition value used for the branch.
  Value Condition;

  SILSuccessor DestBBs[2];
public:

  CondBranchInst(SILLocation Loc, Value Condition,
                 BasicBlock *TrueBB, BasicBlock *FalseBB);

  Value getCondition() const { return Condition; }

  SuccessorListTy getSuccessors() {
    return DestBBs;
  }
  
  BasicBlock *getTrueBB() { return DestBBs[0]; }
  const BasicBlock *getTrueBB() const { return DestBBs[0]; }
  BasicBlock *getFalseBB() { return DestBBs[1]; }
  const BasicBlock *getFalseBB() const { return DestBBs[1]; }
  
  void setTrueBB(BasicBlock *BB) { DestBBs[0] = BB; }
  void setFalseBB(BasicBlock *BB) { DestBBs[1] = BB; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::CondBranchInst;
  }
};

} // end swift namespace

//===----------------------------------------------------------------------===//
// ilist_traits for Instruction
//===----------------------------------------------------------------------===//

namespace llvm {

template <>
struct ilist_traits<::swift::Instruction> :
  public ilist_default_traits<::swift::Instruction> {
  typedef ::swift::Instruction Instruction;

private:
  mutable ilist_half_node<Instruction> Sentinel;

  swift::BasicBlock *getContainingBlock();

public:
  Instruction *createSentinel() const {
    return static_cast<Instruction*>(&Sentinel);
  }
  void destroySentinel(Instruction *) const {}

  Instruction *provideInitialHead() const { return createSentinel(); }
  Instruction *ensureHead(Instruction*) const { return createSentinel(); }
  static void noteHead(Instruction*, Instruction*) {}
  static void deleteNode(Instruction *V) {}

  void addNodeToList(Instruction *I);
  void removeNodeFromList(Instruction *I);
  void transferNodesFromList(ilist_traits<Instruction> &L2,
                             ilist_iterator<Instruction> first,
                             ilist_iterator<Instruction> last);

private:
  void createNode(const Instruction &);
};

} // end llvm namespace

#endif
