//===--- SILInstruction.h - Instructions for SIL code -----------*- C++ -*-===//
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
// This file defines the high-level SILInstruction class used for SIL code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_INSTRUCTION_H
#define SWIFT_SIL_INSTRUCTION_H

#include "swift/SIL/SILLocation.h"
#include "swift/AST/Builtins.h"
#include "swift/SIL/SILSuccessor.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"

namespace swift {

class CharacterLiteralExpr;
class DeclRefExpr;
class FloatLiteralExpr;
class FuncDecl;
class IntegerLiteralExpr;
class SILBasicBlock;
class SILDebugScope;
class SILFunction;
class SILType;
class Stmt;
class StringLiteralExpr;
class Substitution;
class ValueDecl;
class VarDecl;

enum class SILInstructionMemoryBehavior {
  None,
  /// \brief The instruction may have side effects not captured solely by its
  ///        users. Specifically, it can return, release memory, or store. Note,
  ///        alloc is not considered to have side effects because its
  ///        result/users represent its effect.
  MayHaveSideEffects,
  /// \brief The istruction may write to memory.
  MayWrite,
  MayWriteAndHaveSideEffects
};

enum IsTake_t { IsNotTake, IsTake };
enum IsInitialization_t { IsNotInitialization, IsInitialization };

/// This is the root class for all instructions that can be used as the contents
/// of a Swift SILBasicBlock.
class SILInstruction : public ValueBase,public llvm::ilist_node<SILInstruction>{
  friend struct llvm::ilist_traits<SILInstruction>;

  /// A backreference to the containing basic block.  This is maintained by
  /// ilist_traits<SILInstruction>.
  SILBasicBlock *ParentBB;

  /// This instruction's location (i.e., AST).
  SILLocation Loc;
  /// This instruction's containing lexical scope used for debug info.
  SILDebugScope* DebugScope;

  friend struct llvm::ilist_sentinel_traits<SILInstruction>;
  SILInstruction() = delete;
  void operator=(const SILInstruction &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

protected:
  SILInstruction(ValueKind Kind, SILLocation Loc, SILType Ty,
                 SILDebugScope *DS=0)
    : ValueBase(Kind, Ty), ParentBB(0), Loc(Loc), DebugScope(DS) {}
  SILInstruction(ValueKind Kind, SILLocation Loc, SILTypeList *TypeList = 0,
                 SILDebugScope *DS=0)
    : ValueBase(Kind, TypeList), ParentBB(0), Loc(Loc), DebugScope(DS) {}

public:

  const SILBasicBlock *getParent() const { return ParentBB; }
  SILBasicBlock *getParent() { return ParentBB; }
  
  SILFunction *getFunction();
  const SILFunction *getFunction() const;
  
  SILModule *getModule();
  const SILModule *getModule() const;

  SILLocation getLoc() const { return Loc; }
  SILDebugScope *getDebugScope() const { return DebugScope; }
  void setDebugScope(SILDebugScope *DS)  { DebugScope = DS; }

  /// removeFromParent - This method unlinks 'self' from the containing basic
  /// block, but does not delete it.
  ///
  void removeFromParent();
  
  /// eraseFromParent - This method unlinks 'self' from the containing basic
  /// block and deletes it.
  ///
  void eraseFromParent();

  /// \brief Drops all uses that belong to this instruction.
  void dropAllReferences();

  /// Return the array of operands for this instruction.
  ArrayRef<Operand> getAllOperands() const;

  /// Return the array of mutable operands for this instruction.
  MutableArrayRef<Operand> getAllOperands();

  unsigned getNumOperands() const { return getAllOperands().size(); }
  SILValue getOperand(unsigned Num) const { return getAllOperands()[Num].get();}

  SILInstructionMemoryBehavior getMemoryBehavior() const;

  /// \brief Returns true if the instruction may have side effects.
  ///
  /// Instructions that store into memory or change retain counts as well as
  /// calls and deallocation instructions are considered to have side effects
  /// that are not visible by merely examining their uses.
  bool mayHaveSideEffects() const;

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_SILInstruction &&
           V->getKind() <= ValueKind::Last_SILInstruction;
  }
  
  /// Invoke an Instruction's destructor. This dispatches to the appropriate
  /// leaf class destructor for the type of the instruction. This does not
  /// deallocate the instruction.
  static void destroy(SILInstruction *I);
};

/// A template base class for instructions that take a single SILValue operand
/// and has no result or a single value result.
template<ValueKind KIND, typename BASE = SILInstruction, bool HAS_RESULT = true>
class UnaryInstructionBase : public BASE {
  FixedOperandList<1> Operands;
  
  /// Check HAS_RESULT in enable_if predicates by injecting a dependency on
  /// a template argument.
  template<typename X>
  struct has_result {
    enum { value = HAS_RESULT };
  };

public:
  
  UnaryInstructionBase(SILLocation Loc, SILValue Operand)
    : BASE(KIND, Loc), Operands(this, Operand) {}

  template<typename X = void>
  UnaryInstructionBase(SILLocation Loc, SILValue Operand,
                       typename std::enable_if<has_result<X>::value, 
                                               SILType>::type Ty)
    : BASE(KIND, Loc, Ty), Operands(this, Operand) {}
  
  template<typename X = void, typename...A>
  UnaryInstructionBase(SILLocation Loc, SILValue Operand,
                       typename std::enable_if<has_result<X>::value, 
                                               SILType>::type Ty,
                       A &&...args)
    : BASE(KIND, Loc, Ty, std::forward<A>(args)...), Operands(this, Operand) {}
  
  SILValue getOperand() const { return Operands[0].get(); }
  
  /// getType() is ok if this is known to only have one type.
  template<typename X = void>
  typename std::enable_if<has_result<X>::value, SILType>::type
  getType(unsigned i = 0) const { return ValueBase::getType(i); }
  
  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == KIND;
  }
};
  
/// AllocStackInst - This represents the allocation of an unboxed (i.e., no
/// reference count) stack memory.  The memory is provided uninitialized.
class AllocStackInst : public SILInstruction {
public:
  AllocStackInst(SILLocation loc, SILType elementType, SILFunction &F);

  /// getDecl - Return the underlying variable declaration associated with this
  /// allocation, or null if this is a temporary allocation.
  VarDecl *getDecl() const;
  
  /// getElementType - Get the type of the allocated memory (as opposed to the
  /// (second) type of the instruction itself, which will be an address type).
  SILType getElementType() const {
    return getType(1).getObjectType();
  }

  SILValue getContainerResult() const { return SILValue(this, 0); }
  SILValue getAddressResult() const { return SILValue(this, 1); }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocStackInst;
  }
};
  
/// AllocRefInst - This represents the primitive allocation of an instance
/// of a reference type. Aside from the reference count, the instance is
/// returned uninitialized.
class AllocRefInst : public SILInstruction {
public:
  AllocRefInst(SILLocation loc, SILType type, SILFunction &F);

  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocRefInst;
  }
};

/// This represents the allocation of a heap box for a Swift value of some type.
/// The instruction returns two values.  The first return value is the object
/// pointer with Builtin.ObjectPointer type.  The second return value
/// is an address pointing to the contained element. The contained
/// element is uninitialized.
class AllocBoxInst : public SILInstruction {
public:
  AllocBoxInst(SILLocation Loc, SILType ElementType, SILFunction &F);

  SILType getElementType() const {
    return getType(1).getObjectType();
  }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocBoxInst;
  }
};


/// AllocArrayInst - This represents the allocation of an array of elements,
/// whose element memory is left uninitialized.  This returns two values.  The
/// first return element is the object pointer (pointer to the object
/// header) with Builtin.ObjectPointer type.  The second element returned is an
/// lvalue to the first array element.
///
class AllocArrayInst : public SILInstruction {
  enum {
    NumElements
  };
  FixedOperandList<1> Operands;
public:

  AllocArrayInst(SILLocation Loc, SILType ElementType, SILValue NumElements,
                 SILFunction &F);

  SILType getElementType() const {
    return getType(1).getObjectType();
  }
  SILValue getNumElements() const { return Operands[NumElements].get(); }


  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocArrayInst;
  }
};

/// ApplyInst - Represents the full application of a function value.
class ApplyInst : public SILInstruction {
  enum {
    Callee
  };

  // Whether the callee had the attribute [transparent].
  // FIXME: pack this somewhere
  bool Transparent;

  /// The fixed operand is the callee;  the rest are arguments.
  TailAllocatedOperandList<1> Operands;

  ApplyInst(SILLocation Loc, SILValue Callee, SILType ReturnType,
            ArrayRef<SILValue> Args, bool Transparent);

public:
  static ApplyInst *create(SILLocation Loc, SILValue Callee,
                           SILType ReturnType,
                           ArrayRef<SILValue> Args,
                           bool Transparent,
                           SILFunction &F);
  SILValue getCallee() const { return Operands[Callee].get(); }
  SILFunctionTypeInfo *getFunctionTypeInfo(SILModule &M) const {
    return getCallee().getType().getFunctionTypeInfo(M);
  }

  /// The arguments passed to this instruction.
  MutableArrayRef<Operand> getArgumentOperands() {
    return Operands.getDynamicAsArray();
  }

  /// The arguments passed to this instruction.
  OperandValueArrayRef getArguments() const {
    return Operands.getDynamicValuesAsArray();
  }

  bool isTransparent() const { return Transparent; }

  bool hasIndirectReturn(SILModule &M) const {
    return getFunctionTypeInfo(M)->hasIndirectReturn();
  }

  SILValue getIndirectReturn(SILModule &M) const {
    assert(hasIndirectReturn(M) && "apply inst does not have indirect return!");
    return getArguments().front();
  }

  OperandValueArrayRef getArgumentsWithoutIndirectReturn(SILModule &M) const {
    if (hasIndirectReturn(M))
      return getArguments().slice(1);
    return getArguments();
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::ApplyInst;
  }
};

/// PartialApplyInst - Represents the creation of a closure object by partial
/// application of a function value.
class PartialApplyInst : public SILInstruction {
   enum {
    Callee
  };

  /// The fixed operand is the callee;  the rest are arguments.
  TailAllocatedOperandList<1> Operands;

  PartialApplyInst(SILLocation Loc, SILValue Callee, ArrayRef<SILValue> Args,
                   SILType ClosureType);
public:
  static PartialApplyInst *create(SILLocation Loc, SILValue Callee,
                                  ArrayRef<SILValue> Args,
                                  SILType ClosureType,
                                  SILFunction &F);

  SILValue getCallee() const { return Operands[Callee].get(); }

  /// The arguments passed to this instruction.
  MutableArrayRef<Operand> getArgumentOperands() {
    return Operands.getDynamicAsArray();
  }

  /// The arguments passed to this instruction.
  OperandValueArrayRef getArguments() const {
    return Operands.getDynamicValuesAsArray();
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::PartialApplyInst;
  }
};

/// BuiltinFunctionRefInst - Represents a reference to a primitive function from
/// the Builtin module.
class BuiltinFunctionRefInst : public SILInstruction {
  FuncDecl *Function;
public:
  BuiltinFunctionRefInst(SILLocation Loc, FuncDecl *Function, SILType Ty)
    : SILInstruction(ValueKind::BuiltinFunctionRefInst, Loc, Ty),
      Function(Function)
  {}
  
  FuncDecl *getFunction() const { return Function; }
  
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  /// \brief Looks up the llvm intrinsic ID and type for the builtin function.
  ///
  /// \returns Returns llvm::Intrinsic::not_intrinsic if the function is not an
  /// intrinsic. The particular intrinsic functions which correspond to the
  /// retruned value are defined in llvm/Intrinsics.h.
  const IntrinsicInfo &getIntrinsicInfo();

  /// \brief Looks up the lazily cached identification for the builtin function.
  const BuiltinInfo &getBuiltinInfo();

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::BuiltinFunctionRefInst;
  }
};
  
/// FunctionRefInst - Represents a reference to a SIL function.
class FunctionRefInst : public SILInstruction {
  SILFunction *Function;
public:
  /// Construct a FunctionRefInst.
  ///
  /// \param Loc  The location of the reference.
  /// \param F    The function being referenced.
  FunctionRefInst(SILLocation Loc, SILFunction *F);

  /// Return the referenced function.
  SILFunction *getFunction() const { return Function; }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::FunctionRefInst;
  }
};
  
/// GlobalAddrInst - Gives the address of a global variable.
class GlobalAddrInst : public SILInstruction {
  VarDecl *Global;
public:
  GlobalAddrInst(SILLocation Loc, VarDecl *Global, SILType AddrTy)
    : SILInstruction(ValueKind::GlobalAddrInst, Loc, AddrTy),
      Global(Global) {}
  
  /// Return the referenced global variable decl.
  VarDecl *getGlobal() const { return Global; }
  
  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }
  
  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::GlobalAddrInst;
  }
};

/// IntegerLiteralInst - Encapsulates an integer constant, as defined originally
/// by an an IntegerLiteralExpr or CharacterLiteralExpr.
class IntegerLiteralInst : public SILInstruction {
  unsigned numBits;
  
  IntegerLiteralInst(SILLocation Loc, SILType Ty, const APInt &Value);
  
public:
  static IntegerLiteralInst *create(IntegerLiteralExpr *E, SILFunction &B);
  static IntegerLiteralInst *create(CharacterLiteralExpr *E, SILFunction &B);
  static IntegerLiteralInst *create(SILLocation Loc, SILType Ty,
                                    intmax_t Value, SILFunction &B);
  static IntegerLiteralInst *create(SILLocation Loc, SILType Ty,
                                    const APInt &Value, SILFunction &B);
  
  /// getValue - Return the APInt for the underlying integer literal.
  APInt getValue() const;

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::IntegerLiteralInst;
  }
};

/// FloatLiteralInst - Encapsulates a floating point constant, as defined
/// originally by a FloatLiteralExpr.
class FloatLiteralInst : public SILInstruction {
  unsigned numBits;
  
  FloatLiteralInst(SILLocation Loc, SILType Ty, const APInt &Bits);
  
public:
  static FloatLiteralInst *create(FloatLiteralExpr *E, SILFunction &B);
  static FloatLiteralInst *create(SILLocation Loc, SILType Ty,
                                  const APFloat &Value, SILFunction &B);

  /// \brief Return the APFloat for the underlying FP literal.
  APFloat getValue() const;

  /// \brief Return the bitcast representation of the FP literal as an APInt.
  APInt getBits() const;
  
  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::FloatLiteralInst;
  }
};

/// StringLiteralInst - Encapsulates a string constant, as defined originally by
/// a StringLiteralExpr.
class StringLiteralInst : public SILInstruction {
  unsigned length;
  
  StringLiteralInst(SILLocation Loc, SILType Ty, StringRef Text);
  
public:
  static StringLiteralInst *create(StringLiteralExpr *E, SILType ty,
                                   SILFunction &B);
  static StringLiteralInst *create(SILLocation Loc, SILType ty,
                                   StringRef Text, SILFunction &B);

  /// getValue - Return the string data for the literal.
  StringRef getValue() const {
    return {reinterpret_cast<char const *>(this + 1), length};
  }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::StringLiteralInst;
  }
};


/// LoadInst - Represents a load from a memory location.
class LoadInst
  : public UnaryInstructionBase<ValueKind::LoadInst>
{
public:
  /// Constructs a LoadInst.
  ///
  /// \param Loc The location of the expression that caused the load.
  ///
  /// \param LValue The SILValue representing the lvalue (address) to
  ///        use for the load.
  LoadInst(SILLocation Loc, SILValue LValue)
    : UnaryInstructionBase(Loc, LValue, LValue.getType().getObjectType())
  {}
};

/// StoreInst - Represents a store from a memory location.
class StoreInst : public SILInstruction {
  enum {
    /// the value being stored
    Src,
    /// the lvalue being stored to
    Dest
  };
  FixedOperandList<2> Operands;
public:

  StoreInst(SILLocation Loc, SILValue Src, SILValue Dest);

  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::StoreInst;
  }
};

/// AssignInst - Represents an abstract assignment to a memory location, which
/// may either be an initialization or a store sequence.  This is only valid in
/// Raw SIL.
class AssignInst : public SILInstruction {
  enum {
    /// the value being stored
    Src,
    /// the lvalue being stored to
    Dest
  };
  FixedOperandList<2> Operands;
public:

  AssignInst(SILLocation Loc, SILValue Src, SILValue Dest);

  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AssignInst;
  }
};


/// Represents a load from a [weak] memory location.
class LoadWeakInst
  : public UnaryInstructionBase<ValueKind::LoadWeakInst>
{
  static SILType getResultType(SILType operandTy) {
    assert(operandTy.isAddress() && "loading from non-address operand?");
    auto refType = cast<ReferenceStorageType>(operandTy.getSwiftRValueType());
    return SILType::getPrimitiveObjectType(refType.getReferentType());
  }

  unsigned IsTake : 1; // FIXME: pack this somewhere

public:
  /// \param loc The location of the expression that caused the load.
  /// \param lvalue The SILValue representing the address to
  ///        use for the load.
  LoadWeakInst(SILLocation loc, SILValue lvalue, IsTake_t isTake)
    : UnaryInstructionBase(loc, lvalue, getResultType(lvalue.getType())),
      IsTake(unsigned(isTake))
  {}

  IsTake_t isTake() const { return IsTake_t(IsTake); }
};

/// Represents a store to a [weak] memory location.
class StoreWeakInst : public SILInstruction {
  enum { Src, Dest };
  FixedOperandList<2> Operands;
  unsigned IsInitializationOfDest : 1; // FIXME: pack this somewhere
public:
  StoreWeakInst(SILLocation loc, SILValue src, SILValue dest,
                IsInitialization_t isInit);

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
    return V->getKind() == ValueKind::StoreWeakInst;
  }
};

/// InitializeVarInst - Represents a default initialization of a variable.
class InitializeVarInst
  : public UnaryInstructionBase<ValueKind::InitializeVarInst,
                                SILInstruction,
                                /*HAS_RESULT*/ false>
{
  bool CanDefaultConstruct;
  
public:
  InitializeVarInst(SILLocation Loc, SILValue Dest, bool CanDefaultConstruct)
    : UnaryInstructionBase(Loc, Dest),
      CanDefaultConstruct(CanDefaultConstruct) {}
  
  /// True if this InitializeVar can be lowered to a default constructor call.
  bool canDefaultConstruct() const { return CanDefaultConstruct; }
};

/// CopyAddrInst - Represents a copy from one memory location to another. This
/// is similar to:
///   %1 = load %src
///   store %1 to %dest
/// but a copy instruction must be used for address-only types.
class CopyAddrInst : public SILInstruction {
  // FIXME: compress storage

  /// IsTakeOfSrc - True if ownership will be taken from the value at the source
  /// memory location.
  unsigned IsTakeOfSrc : 1;

  /// IsInitializationOfDest - True if this is the initialization of the
  /// uninitialized destination memory location.
  unsigned IsInitializationOfDest : 1;

  enum {
    /// The lvalue being loaded from.
    Src,
    
    /// The lvalue being stored to.
    Dest
  };
  FixedOperandList<2> Operands;
  
public:
  CopyAddrInst(SILLocation Loc, SILValue Src, SILValue Dest,
               IsTake_t isTakeOfSrc, IsInitialization_t isInitializationOfDest);
  
  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }
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

/// SpecializeInst - Specializes a reference to a generic entity by binding
/// each of its type parameters to a specific type.
///
/// This instruction takes an arbitrary value of generic type and returns a new
/// closure that takes concrete types for its archetypes.  This is commonly used
/// in call sequences to generic functions, but can occur in arbitrarily general
/// cases as well.
///
class SpecializeInst : public SILInstruction {
  enum {
    /// The value being specialized.  It always has PolymorphicFunctionType.
    Function
  };
  FixedOperandList<1> Operands;

  unsigned NumSubstitutions;
  Substitution *getSubstitutionsStorage() {
    return reinterpret_cast<Substitution *>(this + 1);
  }
  Substitution const *getSubstitutionsStorage() const {
    return reinterpret_cast<Substitution const *>(this + 1);
  }

  SpecializeInst(SILLocation Loc, SILValue Operand,
                 ArrayRef<Substitution> Substitutions,
                 SILType DestTy);

public:
  static SpecializeInst *create(SILLocation Loc, SILValue Operand,
                                ArrayRef<Substitution> Substitutions,
                                SILType DestTy,
                                SILFunction &F);

  SILValue getOperand() const { return Operands[Function].get(); }
  
  ArrayRef<Substitution> getSubstitutions() const {
    return ArrayRef<Substitution>(getSubstitutionsStorage(), NumSubstitutions);
  }
  
  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SpecializeInst;
  }
};

/// ConversionInst - Abstract class representing instructions that convert
/// values.
class ConversionInst : public SILInstruction {
public:
  ConversionInst(ValueKind Kind, SILLocation Loc, SILType Ty)
    : SILInstruction(Kind, Loc, Ty) {}
  
  /// All conversion instructions return a single result.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_ConversionInst &&
      V->getKind() <= ValueKind::Last_ConversionInst;
  }
};

/// ConvertFunctionInst - Change the type of some value without affecting how it
/// will codegen.
class ConvertFunctionInst
  : public UnaryInstructionBase<ValueKind::ConvertFunctionInst, ConversionInst>
{
public:
  ConvertFunctionInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};

/// CoerceInst - Convert a value to a type with an explicit T(x) cast.
class CoerceInst
  : public UnaryInstructionBase<ValueKind::CoerceInst, ConversionInst>
{
public:
  CoerceInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};

/// UpcastInst - Perform a conversion of a class instance to a supertype.
class UpcastInst
  : public UnaryInstructionBase<ValueKind::UpcastInst, ConversionInst>
{
public:
  UpcastInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};
  
/// AddressToPointerInst - Convert a SIL address to a Builtin.RawPointer value.
class AddressToPointerInst
  : public UnaryInstructionBase<ValueKind::AddressToPointerInst,
                                ConversionInst>
{
public:
  AddressToPointerInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};

/// PointerToAddressInst - Convert a Builtin.RawPointer value to a SIL address.
class PointerToAddressInst
  : public UnaryInstructionBase<ValueKind::PointerToAddressInst, ConversionInst>
{
public:
  PointerToAddressInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};
  
/// RefToObjectPointerInst - Convert a class instance reference to a
/// Builtin.ObjectPointer.
class RefToObjectPointerInst
  : public UnaryInstructionBase<ValueKind::RefToObjectPointerInst,
                                ConversionInst>
{
public:
  RefToObjectPointerInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};
  
/// ObjectPointerToRefInst - Convert a Builtin.ObjectPointer to a class instance
/// reference.
class ObjectPointerToRefInst
  : public UnaryInstructionBase<ValueKind::ObjectPointerToRefInst,
                                ConversionInst>
{
public:
  ObjectPointerToRefInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};
  
/// RefToRawPointer - Convert a reference type to a Builtin.RawPointer.
class RefToRawPointerInst
  : public UnaryInstructionBase<ValueKind::RefToRawPointerInst, ConversionInst>
{
public:
  RefToRawPointerInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};
  
/// RawPointerToRefInst - Convert a Builtin.RawPointer to a reference type.
class RawPointerToRefInst
  : public UnaryInstructionBase<ValueKind::RawPointerToRefInst, ConversionInst>
{
public:
  RawPointerToRefInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};

/// RefToUnownedInst - Given a value of a reference type,
/// convert it to an unowned reference.
///
/// This does nothing at runtime; it just changes the formal type.
class RefToUnownedInst
  : public UnaryInstructionBase<ValueKind::RefToUnownedInst, ConversionInst>
{
public:
  RefToUnownedInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};

/// UnownedToRefInst - Given a value of an [unowned] type,
/// convert it to the underlying reference type.
///
/// This does nothing at runtime; it just changes the formal type.
class UnownedToRefInst
  : public UnaryInstructionBase<ValueKind::UnownedToRefInst, ConversionInst>
{
public:
  UnownedToRefInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};

/// ThinToThickFunctionInst - Given a thin function reference, adds a null
/// context to convert the value to a thick function type.
class ThinToThickFunctionInst
  : public UnaryInstructionBase<ValueKind::ThinToThickFunctionInst,
                                ConversionInst>
{
public:
  ThinToThickFunctionInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};
  
/// ConvertCCInst - Thunks a function reference, giving a function reference to
/// the same function with a different calling convention.
class ConvertCCInst
  : public UnaryInstructionBase<ValueKind::ConvertCCInst, ConversionInst>
{
public:
  ConvertCCInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};
  
/// BridgeToBlockInst - Converts a Swift function value to an ObjC-compatible
/// block.
class BridgeToBlockInst
  : public UnaryInstructionBase<ValueKind::BridgeToBlockInst, ConversionInst>
{
public:
  BridgeToBlockInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};

/// ArchetypeRefToSuperInst - Given a class archetype value with a base
/// class constraint, returns a reference to the superclass instance.
class ArchetypeRefToSuperInst
  : public UnaryInstructionBase<ValueKind::ArchetypeRefToSuperInst, ConversionInst>
{
public:
  ArchetypeRefToSuperInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};

/// Test that an address or reference type is not null.
class IsNonnullInst : public UnaryInstructionBase<ValueKind::IsNonnullInst> {
public:
  IsNonnullInst(SILLocation Loc, SILValue Operand, SILType BoolTy)
    : UnaryInstructionBase(Loc, Operand, BoolTy) {}
};

/// Discriminates checked cast modes.
enum class CheckedCastMode : unsigned char {
  // Abort if the cast fails.
  Unconditional,
  // Return null if the cast fails.
  Conditional,
};
  
/// CheckedConversionInst - Abstract base class for checked conversions.
class CheckedConversionInst : public ConversionInst {
  CheckedCastMode Mode;
public:
  CheckedConversionInst(ValueKind Kind, SILLocation Loc, SILType Ty,
                        CheckedCastMode Mode)
    : ConversionInst(Kind, Loc, Ty), Mode(Mode) {}
  
  CheckedCastMode getMode() const { return Mode; }
};
  
/// DowncastInst - Perform an unchecked conversion of a class instance to a
/// subclass type.
class DowncastInst
  : public UnaryInstructionBase<ValueKind::DowncastInst, CheckedConversionInst>
{
public:
  DowncastInst(SILLocation Loc, SILValue Operand, SILType Ty,
               CheckedCastMode Mode)
    : UnaryInstructionBase(Loc, Operand, Ty, Mode) {}
};

/// SuperToArchetypeRefInst - Given a value of a class type, initializes a
/// class archetype with a superclass constraint to contain a reference to
/// the value.
class SuperToArchetypeRefInst
  : public UnaryInstructionBase<ValueKind::SuperToArchetypeRefInst,
                                CheckedConversionInst>
{
public:
  SuperToArchetypeRefInst(SILLocation Loc, SILValue Operand, SILType Ty,
                          CheckedCastMode Mode)
    : UnaryInstructionBase(Loc, Operand, Ty, Mode) {}
};

/// Given the address of an opaque archetype value, dynamically checks the concrete
/// type represented by the archetype and casts the address to the destination type if
/// successful or crashes if not.
class DowncastArchetypeAddrInst
  : public UnaryInstructionBase<ValueKind::DowncastArchetypeAddrInst,
                                CheckedConversionInst>
{
public:
  DowncastArchetypeAddrInst(SILLocation Loc, SILValue Operand, SILType Ty,
                            CheckedCastMode Mode)
    : UnaryInstructionBase(Loc, Operand, Ty, Mode) {}
};
  
/// Given a value of class archetype type, dynamically checks the concrete
/// type represented by the archetype and casts to the destination type if
/// successful or crashes if not.
class DowncastArchetypeRefInst
  : public UnaryInstructionBase<ValueKind::DowncastArchetypeRefInst,
                                CheckedConversionInst>
{
public:
  DowncastArchetypeRefInst(SILLocation Loc, SILValue Operand, SILType Ty,
                           CheckedCastMode Mode)
    : UnaryInstructionBase(Loc, Operand, Ty, Mode) {}
};
  
/// Given the address of an opaque existential container, dynamically checks the
/// concrete type contained in the existential and projects and casts the address of
/// the contained value if successful or crashes if not.
class ProjectDowncastExistentialAddrInst
  : public UnaryInstructionBase<ValueKind::ProjectDowncastExistentialAddrInst,
                                CheckedConversionInst>
{
public:
  ProjectDowncastExistentialAddrInst(SILLocation Loc, SILValue Operand,
                                     SILType Ty,
                                     CheckedCastMode Mode)
    : UnaryInstructionBase(Loc, Operand, Ty, Mode) {}
};
  
/// Given a value of class archetype type, dynamically checks the concrete
/// type contained in the existential and casts the value to the destination type if
/// successful or crashes if not.
class DowncastExistentialRefInst
  : public UnaryInstructionBase<ValueKind::DowncastExistentialRefInst,
                                CheckedConversionInst>
{
public:
  DowncastExistentialRefInst(SILLocation Loc, SILValue Operand, SILType Ty,
                             CheckedCastMode Mode)
    : UnaryInstructionBase(Loc, Operand, Ty, Mode) {}
};
  
/// StructInst - Represents a constructed loadable struct.
class StructInst : public SILInstruction {
  TailAllocatedOperandList<0> Operands;

  /// Private constructor.  Because of the storage requirements of
  /// StructInst, object creation goes through 'create()'.
  StructInst(SILLocation Loc, SILType Ty, ArrayRef<SILValue> Elements);
  static StructInst *createImpl(SILLocation Loc, SILType Ty,
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

  /// Construct a StructInst.
  static StructInst *create(SILLocation Loc, SILType Ty,
                            ArrayRef<SILValue> Elements, SILFunction &F) {
    return createImpl(Loc, Ty, Elements, F);
  }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::StructInst;
  }
};

/// TupleInst - Represents a constructed loadable tuple.
class TupleInst : public SILInstruction {
  TailAllocatedOperandList<0> Operands;

  /// Private constructor.  Because of the storage requirements of
  /// TupleInst, object creation goes through 'create()'.
  TupleInst(SILLocation Loc, SILType Ty, ArrayRef<SILValue> Elements);
  static TupleInst *createImpl(SILLocation Loc, SILType Ty,
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

  /// Construct a TupleInst.
  static TupleInst *create(SILLocation Loc, SILType Ty,
                           ArrayRef<SILValue> Elements, SILFunction &F) {
    return createImpl(Loc, Ty, Elements, F);
  }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::TupleInst;
  }
};
  
/// UnionInst - Represents a loadable union constructed from one of its
/// elements.
class UnionInst : public SILInstruction {
  Optional<FixedOperandList<1>> OptionalOperand;
  UnionElementDecl *Element;

public:
  UnionInst(SILLocation Loc, SILValue Operand, UnionElementDecl *Element,
            SILType ResultTy)
    : SILInstruction(ValueKind::UnionInst, Loc, ResultTy),
      Element(Element)
  {
    if (Operand) {
      OptionalOperand.emplace(this, Operand);
    }
  }
  
  UnionElementDecl *getElement() const { return Element; }
  
  bool hasOperand() const { return OptionalOperand.hasValue(); }
  SILValue getOperand() const { return OptionalOperand->asValueArray()[0]; }
  
  ArrayRef<Operand> getAllOperands() const {
    return OptionalOperand ? OptionalOperand->asArray() : ArrayRef<Operand>{};
  }
  
  MutableArrayRef<Operand> getAllOperands() {
    return OptionalOperand
      ? OptionalOperand->asArray() : MutableArrayRef<Operand>{};
  }
  
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::UnionInst;
  }
};
  
/// UnionDataAddrInst - Projects the address of the data for a case inside the
/// union.
class UnionDataAddrInst
  : public UnaryInstructionBase<ValueKind::UnionDataAddrInst>
{
  UnionElementDecl *Element;
public:
  UnionDataAddrInst(SILLocation Loc, SILValue Operand,
                    UnionElementDecl *Element, SILType ResultTy)
    : UnaryInstructionBase(Loc, Operand, ResultTy),
      Element(Element) {}
  
  UnionElementDecl *getElement() const { return Element; }
};
  
/// InjectUnionAddrInst - Tags a union as containing a case. The data for
/// that case, if any, must have been written into the union first.
class InjectUnionAddrInst
  : public UnaryInstructionBase<ValueKind::InjectUnionAddrInst,
                                SILInstruction,
                                /*HAS_RESULT*/ false>
{
  UnionElementDecl *Element;
public:
  InjectUnionAddrInst(SILLocation Loc, SILValue Operand,
                      UnionElementDecl *Element)
    : UnaryInstructionBase(Loc, Operand), Element(Element) {}
  
  UnionElementDecl *getElement() const { return Element; }
};

/// BuiltinZeroInst - Represents the zero value of a builtin integer,
/// floating-point, or pointer type.
class BuiltinZeroInst : public SILInstruction {
public:
  BuiltinZeroInst(SILLocation Loc, SILType Type)
    : SILInstruction(ValueKind::BuiltinZeroInst, Loc, Type) {}
  
  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::BuiltinZeroInst;
  }
};

/// MetatypeInst - Represents the production of an instance of a given metatype
/// named statically.
class MetatypeInst : public SILInstruction {
public:

  /// Constructs a MetatypeInst
  MetatypeInst(SILLocation Loc, SILType Metatype);

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::MetatypeInst;
  }
};
  
/// ClassMetatypeInst - Represents loading a dynamic class metatype
/// for a class instance.
class ClassMetatypeInst
  : public UnaryInstructionBase<ValueKind::ClassMetatypeInst>
{
public:
  ClassMetatypeInst(SILLocation Loc, SILType Metatype, SILValue Base)
    : UnaryInstructionBase(Loc, Base, Metatype) {}
};
  
/// ArchetypeMetatypeInst - Represents loading a dynamic metatype from an
/// archetype instance.
class ArchetypeMetatypeInst
  : public UnaryInstructionBase<ValueKind::ArchetypeMetatypeInst>
{
public:
  ArchetypeMetatypeInst(SILLocation Loc, SILType Metatype, SILValue Base)
    : UnaryInstructionBase(Loc, Base, Metatype) {}
};
  
/// ProtocolMetatype - Represents loading a dynamic metatype from an
/// existential container.
class ProtocolMetatypeInst
  : public UnaryInstructionBase<ValueKind::ProtocolMetatypeInst>
{
public:
  ProtocolMetatypeInst(SILLocation Loc, SILType Metatype, SILValue Base)
    : UnaryInstructionBase(Loc, Base, Metatype) {}
};

/// ModuleInst - Represents a reference to a module as a value.
class ModuleInst : public SILInstruction {
public:
  
  ModuleInst(SILLocation Loc, SILType ModuleType);
  
  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::ModuleInst;
  }
};
  
/// Extract a numbered element out of a value of tuple type.
class TupleExtractInst
  : public UnaryInstructionBase<ValueKind::TupleExtractInst>
{
  unsigned FieldNo;
public:
  TupleExtractInst(SILLocation Loc, SILValue Operand, unsigned FieldNo,
                   SILType ResultTy)
    : UnaryInstructionBase(Loc, Operand, ResultTy), FieldNo(FieldNo) {}
  
  unsigned getFieldNo() const { return FieldNo; }
};

/// Derive the address of a numbered element from the address of a tuple.
class TupleElementAddrInst
  : public UnaryInstructionBase<ValueKind::TupleElementAddrInst>
{
  unsigned FieldNo;
public:
  TupleElementAddrInst(SILLocation Loc, SILValue Operand, unsigned FieldNo,
                       SILType ResultTy)
    : UnaryInstructionBase(Loc, Operand, ResultTy), FieldNo(FieldNo) {}
  
  unsigned getFieldNo() const { return FieldNo; }
};
  
/// Extract a physical, fragile field out of a value of struct type.
class StructExtractInst
  : public UnaryInstructionBase<ValueKind::StructExtractInst>
{
  VarDecl *Field;
public:
  StructExtractInst(SILLocation Loc, SILValue Operand, VarDecl *Field,
                    SILType ResultTy)
    : UnaryInstructionBase(Loc, Operand, ResultTy), Field(Field) {}
  
  VarDecl *getField() const { return Field; }
};

/// Derive the address of a physical field from the address of a struct.
class StructElementAddrInst
  : public UnaryInstructionBase<ValueKind::StructElementAddrInst>
{
  VarDecl *Field;
public:
  StructElementAddrInst(SILLocation Loc, SILValue Operand, VarDecl *Field,
                        SILType ResultTy)
    : UnaryInstructionBase(Loc, Operand, ResultTy), Field(Field) {}
  
  VarDecl *getField() const { return Field; }
};

/// RefElementAddrInst - Derive the address of a named element in a reference
/// type instance.
class RefElementAddrInst
  : public UnaryInstructionBase<ValueKind::RefElementAddrInst>
{
  VarDecl *Field;
public:
  RefElementAddrInst(SILLocation Loc, SILValue Operand, VarDecl *Field,
                     SILType ResultTy)
    : UnaryInstructionBase(Loc, Operand, ResultTy), Field(Field) {}
  
  VarDecl *getField() const { return Field; }
};
  
/// DynamicMethodInst - Abstract base for instructions that implement dynamic
/// method lookup.
class DynamicMethodInst : public SILInstruction {
  SILDeclRef Member;
  bool Volatile;
public:
  DynamicMethodInst(ValueKind Kind,
                    SILLocation Loc, SILType Ty,
                    SILDeclRef Member,
                    bool Volatile = false)
    : SILInstruction(Kind, Loc, Ty), Member(Member), Volatile(Volatile) {}

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  SILDeclRef getMember() const { return Member; }
  
  /// True if this dynamic dispatch is semantically required.
  bool isVolatile() const { return Volatile; }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_DynamicMethodInst &&
      V->getKind() <= ValueKind::Last_DynamicMethodInst;
  }
};

/// ClassMethodInst - Given the address of a value of class type and a method
/// constant, extracts the implementation of that method for the dynamic
/// instance type of the class.
class ClassMethodInst
  : public UnaryInstructionBase<ValueKind::ClassMethodInst, DynamicMethodInst>
{
public:
  ClassMethodInst(SILLocation Loc, SILValue Operand, SILDeclRef Member,
                  SILType Ty, bool Volatile = false)
    : UnaryInstructionBase(Loc, Operand, Ty, Member, Volatile) {}
};

/// SuperMethodInst - Given the address of a value of class type and a method
/// constant, extracts the implementation of that method for the superclass of
/// the static type of the class.
class SuperMethodInst
  : public UnaryInstructionBase<ValueKind::SuperMethodInst, DynamicMethodInst>
{
public:
  SuperMethodInst(SILLocation Loc, SILValue Operand, SILDeclRef Member,
                  SILType Ty, bool Volatile = false)
    : UnaryInstructionBase(Loc, Operand, Ty, Member, Volatile) {}
};

/// ArchetypeMethodInst - Given the address of an archetype value and a method
/// constant, extracts the implementation of that method for the archetype.
class ArchetypeMethodInst : public DynamicMethodInst {
  SILType LookupType;
public:
  ArchetypeMethodInst(SILLocation Loc, SILType LookupType, SILDeclRef Member,
                      SILType Ty, bool Volatile = false)
    : DynamicMethodInst(ValueKind::ArchetypeMethodInst, Loc, Ty, Member,
                        Volatile),
      LookupType(LookupType)
  {}
  
  SILType getLookupArchetype() const { return LookupType; }
  
  ArrayRef<Operand> getAllOperands() const { return {}; }
  MutableArrayRef<Operand> getAllOperands() { return {}; }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::ArchetypeMethodInst;
  }
};
  
/// ProtocolMethodInst - Given the address of an existential and a method
/// constant, extracts the implementation of that method for the existential.
/// The result will be of the type RawPointer -> F for a method of function type
/// F. The RawPointer "self" argument can be derived from the same existential
/// using a ProjectExistentialInst.
class ProtocolMethodInst
  : public UnaryInstructionBase<ValueKind::ProtocolMethodInst,
                                DynamicMethodInst>
{
public:
  ProtocolMethodInst(SILLocation Loc, SILValue Operand, SILDeclRef Member,
                     SILType Ty, bool Volatile = false)
    : UnaryInstructionBase(Loc, Operand, Ty, Member, Volatile) {}
};
  
/// ProjectExistentialInst - Given the address of an existential, returns a
/// RawPointer pointing to the value inside the existential.
class ProjectExistentialInst
  : public UnaryInstructionBase<ValueKind::ProjectExistentialInst>
{
public:
  ProjectExistentialInst(SILLocation Loc, SILValue Operand, SILType ThisTy);
};
  
/// ProjectExistentialRefInst - Given a class existential, returns an
/// ObjCPointer referencing the contained class instance.
class ProjectExistentialRefInst
  : public UnaryInstructionBase<ValueKind::ProjectExistentialRefInst>
{
public:
  ProjectExistentialRefInst(SILLocation Loc, SILValue Operand, SILFunction &F);
};
  
/// InitExistentialInst - Given an address to an uninitialized buffer of
/// a protocol type, initializes its existential container to contain a concrete
/// value of the given type, and returns the address of the uninitialized
/// concrete value inside the existential container.
class InitExistentialInst
  : public UnaryInstructionBase<ValueKind::InitExistentialInst>
{
  ArrayRef<ProtocolConformance*> Conformances;
public:
  InitExistentialInst(SILLocation Loc,
                      SILValue Existential,
                      SILType ConcreteType,
                      ArrayRef<ProtocolConformance*> Conformances)
    : UnaryInstructionBase(Loc, Existential, ConcreteType.getAddressType()),
      Conformances(Conformances)
  {}
  
  ArrayRef<ProtocolConformance*> getConformances() const {
    return Conformances;
  }

  SILType getConcreteType() const {
    return getType();
  }
};
  
/// InitExistentialRefInst - Given a class instance reference and a set of
/// conformances, creates a class existential value referencing the
/// class instance.
class InitExistentialRefInst
  : public UnaryInstructionBase<ValueKind::InitExistentialRefInst>
{
  ArrayRef<ProtocolConformance*> Conformances;
public:
  InitExistentialRefInst(SILLocation Loc,
                         SILType ExistentialType,
                         SILValue Instance,
                         ArrayRef<ProtocolConformance*> Conformances)
    : UnaryInstructionBase(Loc, Instance, ExistentialType),
      Conformances(Conformances)
  {}
  
  ArrayRef<ProtocolConformance*> getConformances() const {
    return Conformances;
  }
};

/// DeinitExistentialInst - Given an address of an existential that has been
/// partially initialized with an InitExistentialInst but whose value buffer
/// has not been initialized, deinitializes the existential and deallocates
/// the value buffer. This should only be used for partially-initialized
/// existentials; a fully-initialized existential can be destroyed with
/// DestroyAddrInst and deallocated with DeallocStackInst.
class DeinitExistentialInst
  : public UnaryInstructionBase<ValueKind::DeinitExistentialInst,
                                SILInstruction,
                                /*HAS_RESULT*/ false>
{
public:
  DeinitExistentialInst(SILLocation Loc, SILValue Existential)
    : UnaryInstructionBase(Loc, Existential) {}
};

/// UpcastExistentialInst - Copies the concrete value from an existential
/// container of a protocol type to another uninitialized existential container
/// for a supertype of the original protocol type. The destination can be
/// of a base protocol type or of a protocol composition that is a superset
/// of the original type (or a protocol composition of base protocols).
class UpcastExistentialInst : public SILInstruction {
  unsigned IsTakeOfSrc : 1;
  enum { SrcExistential, DestExistential };
  FixedOperandList<2> Operands;
public:
  UpcastExistentialInst(SILLocation Loc,
                        SILValue SrcExistential,
                        SILValue DestExistential,
                        IsTake_t isTakeOfSrc);
  
  SILValue getSrcExistential() const { return Operands[SrcExistential].get(); }
  SILValue getDestExistential() const { return Operands[DestExistential].get();}

  /// True if the destination can take ownership of the concrete value from the
  /// source.
  IsTake_t isTakeOfSrc() const { return IsTake_t(IsTakeOfSrc); }
  
  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::UpcastExistentialInst;
  }
};
  
/// UpcastExistentialRefInst - Converts a value of class existential
/// container to another, more general class existential container type.
class UpcastExistentialRefInst
  : public UnaryInstructionBase<ValueKind::UpcastExistentialRefInst,
                                ConversionInst>
{
public:
  UpcastExistentialRefInst(SILLocation Loc,
                           SILValue Operand,
                           SILType DestTy)
    : UnaryInstructionBase(Loc, Operand, DestTy)
  {}
};

/// RefCountingInst - An abstract class of instructions which
/// manipulate the reference count of their object operand.
class RefCountingInst : public SILInstruction {
protected:
  RefCountingInst(ValueKind Kind, SILLocation Loc, SILTypeList *TypeList = 0,
                  SILDebugScope *DS=0)
    : SILInstruction(Kind, Loc, TypeList, DS) {}

public:
  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_RefCountingInst &&
           V->getKind() <= ValueKind::Last_RefCountingInst;
  }
};

/// StrongRetainInst - Increase the strong reference count of an object.
class StrongRetainInst
  : public UnaryInstructionBase<ValueKind::StrongRetainInst,
                                RefCountingInst,
                                /*HAS_RESULT*/ false>
{
public:
  StrongRetainInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};
  
/// StrongRetainAutoreleasedInst - Take ownership of the autoreleased return
/// value of an ObjC method.
class StrongRetainAutoreleasedInst
  : public UnaryInstructionBase<ValueKind::StrongRetainAutoreleasedInst,
                                RefCountingInst, /*HAS_RESULT*/ false>
{
public:
  StrongRetainAutoreleasedInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
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
public:
  StrongReleaseInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};

/// StrongRetainUnownedInst - Increase the strong reference count of an object
/// and assert that it has not been deallocated.
///
/// The operand must be an [unowned] type.
class StrongRetainUnownedInst :
    public UnaryInstructionBase<ValueKind::StrongRetainUnownedInst,
                                RefCountingInst, /*HAS_RESULT*/ false>
{
public:
  StrongRetainUnownedInst(SILLocation loc, SILValue operand)
    : UnaryInstructionBase(loc, operand) {}
};

/// UnownedRetainInst - Increase the unowned reference count of an object.
class UnownedRetainInst :
    public UnaryInstructionBase<ValueKind::UnownedRetainInst,
                                RefCountingInst, /*HAS_RESULT*/ false>
{
public:
  UnownedRetainInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};

/// UnownedReleaseInst - Decrease the unowned reference count of an object.
class UnownedReleaseInst :
     public UnaryInstructionBase<ValueKind::UnownedReleaseInst,
                                 RefCountingInst, /*HAS_RESULT*/ false>
{
public:
  UnownedReleaseInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};

/// DeallocStackInst - Deallocate stack memory allocated by alloc_stack.
class DeallocStackInst
  : public UnaryInstructionBase<ValueKind::DeallocStackInst, SILInstruction,
                                /*HAS_RESULT*/ false>
{
public:
  DeallocStackInst(SILLocation loc, SILValue operand)
    : UnaryInstructionBase(loc, operand) {}
};
  
/// DeallocRefInst - Deallocate memory allocated for a reference type
/// instance, as might have been created by an AllocRefInst. It is
/// undefined behavior if the type of the operand does not match the
/// most derived type of the allocated instance.
///
/// This does not destroy the referenced instance; it must either be
/// uninitialized or have been manually destroyed.
class DeallocRefInst : public UnaryInstructionBase<ValueKind::DeallocRefInst,
                                                   SILInstruction,
                                                   /*HAS_RESULT*/ false>
{
public:
  DeallocRefInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};

/// DeallocBoxInst - Deallocate memory allocated for a boxed value,
/// as might have been created by an AllocBoxInst. It is undefined
/// behavior if the type of the boxed type does not match the
/// type of the boxed type of the allocated box.
///
/// This does not destroy the boxed value instance; it must either be
/// uninitialized or have been manually destroyed.
class DeallocBoxInst : public UnaryInstructionBase<ValueKind::DeallocBoxInst,
                                                   SILInstruction,
                                                   /*HAS_RESULT*/ false>
{
  SILType ElementType;
public:
  DeallocBoxInst(SILLocation loc, SILType elementType, SILValue operand)
    : UnaryInstructionBase(loc, operand), ElementType(elementType) {}

  SILType getElementType() const { return ElementType; }
};

/// DestroyAddrInst - Destroy the value at a memory location according to
/// its SIL type. This is similar to:
///   %1 = load %operand
///   release %1
/// but a destroy instruction can be used for types that cannot be loaded,
/// such as resilient value types.
class DestroyAddrInst : public UnaryInstructionBase<ValueKind::DestroyAddrInst,
                                                    SILInstruction,
                                                    /*HAS_RESULT*/ false>
{
public:
  DestroyAddrInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};

//===----------------------------------------------------------------------===//
// Pointer/address indexing instructions
//===----------------------------------------------------------------------===//

/// Abstract base class for indexing instructions.
class IndexingInst : public SILInstruction {
  enum { Base, Index };
  FixedOperandList<2> Operands;
public:
  IndexingInst(ValueKind Kind,
               SILLocation Loc, SILValue Operand, SILValue Index)
    : SILInstruction(Kind, Loc, Operand.getType()),
      Operands{this, Operand, Index}
  {}
  
  SILValue getBase() const { return Operands[Base].get(); }
  SILValue getIndex() const { return Operands[Index].get(); }
  
  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_IndexingInst
        && V->getKind() <= ValueKind::Last_IndexingInst;
  }
};
  
/// IndexAddrInst - "%2 : $*T = index_addr %0 : $*T, %1 : $Builtin.Int64"
/// This takes an address and indexes it, striding by the pointed-
/// to type.  This is used to index into arrays of uniform elements.
class IndexAddrInst : public IndexingInst {
  enum { Base, Index };
public:
  IndexAddrInst(SILLocation Loc, SILValue Operand, SILValue Index)
    : IndexingInst(ValueKind::IndexAddrInst, Loc, Operand, Index)
  {}
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::IndexAddrInst;
  }
};

/// IndexRawPointerInst
/// %2 : $Builtin.RawPointer \
///   = index_raw_pointer %0 : $Builtin.RawPointer, %1 : $Builtin.Int64
/// This takes an address and indexes it, striding by the pointed-
/// to type.  This is used to index into arrays of uniform elements.
class IndexRawPointerInst : public IndexingInst {
  enum { Base, Index };
public:
  IndexRawPointerInst(SILLocation Loc, SILValue Operand, SILValue Index)
    : IndexingInst(ValueKind::IndexRawPointerInst, Loc, Operand, Index)
  {}
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::IndexRawPointerInst;
  }
};


//===----------------------------------------------------------------------===//
// Instructions representing terminators
//===----------------------------------------------------------------------===//

/// This class defines a "terminating instruction" for a SILBasicBlock.
class TermInst : public SILInstruction {
protected:
  TermInst(ValueKind K, SILLocation Loc) : SILInstruction(K, Loc) {}
public:

  typedef ArrayRef<SILSuccessor> SuccessorListTy;

  /// The successor basic blocks of this terminator.
  SuccessorListTy getSuccessors();

  /// The successor basic blocks of this terminator.
  const SuccessorListTy getSuccessors() const {
    return const_cast<TermInst*>(this)->getSuccessors();
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_TermInst &&
           V->getKind() <= ValueKind::Last_TermInst;
  }
};

/// UnreachableInst - Position in the code which would be undefined to reach.
/// These are always implicitly generated, e.g. when falling off the end of a
/// function or after a no-return function call.
class UnreachableInst : public TermInst {
public:
  UnreachableInst(SILLocation Loc)
    : TermInst(ValueKind::UnreachableInst, Loc)
  {}
  
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
public:
  /// Constructs a ReturnInst representing a return.
  ///
  /// \param Loc The backing AST location.
  ///
  /// \param ReturnValue The value to be returned.
  ///
  ReturnInst(SILLocation Loc, SILValue ReturnValue)
    : UnaryInstructionBase(Loc, ReturnValue) {}

  SuccessorListTy getSuccessors() {
    // No Successors.
    return SuccessorListTy();
  }
};

/// AutoreleaseReturnInst - Transfer ownership of a value to an ObjC
/// autorelease pool, and then return the value.
class AutoreleaseReturnInst
  : public UnaryInstructionBase<ValueKind::AutoreleaseReturnInst, TermInst,
                                /*HAS_RESULT*/ false>
{
public:
  /// Constructs an AutoreleaseReturnInst representing an autorelease-return
  /// sequence.
  ///
  /// \param Loc The backing AST location.
  ///
  /// \param ReturnValue The value to be returned.
  ///
  AutoreleaseReturnInst(SILLocation Loc, SILValue ReturnValue)
    : UnaryInstructionBase(Loc, ReturnValue) {}

  SuccessorListTy getSuccessors() {
    // No Successors.
    return SuccessorListTy();
  }
};

/// BranchInst - An unconditional branch.
class BranchInst : public TermInst {
  SILSuccessor DestBB;
  // FIXME: probably needs dynamic adjustment
  TailAllocatedOperandList<0> Operands;

  BranchInst(SILLocation Loc,
             SILBasicBlock *DestBB, ArrayRef<SILValue> Args);
public:
  typedef ArrayRef<SILValue> ArgsTy;
  
  /// Construct a BranchInst that will branch to the specified block.
  /// The destination block must take no parameters.
  static BranchInst *create(SILLocation Loc,
                            SILBasicBlock *DestBB,
                            SILFunction &F);

  /// Construct a BranchInst that will branch to the specified block with
  /// the given parameters.
  static BranchInst *create(SILLocation Loc,
                            SILBasicBlock *DestBB,
                            ArrayRef<SILValue> Args,
                            SILFunction &F);
  
  /// The jump target for the branch.
  SILBasicBlock *getDestBB() const { return DestBB; }
  
  /// The arguments for the destination BB.
  OperandValueArrayRef getArgs() const { return Operands.asValueArray(); }

  SuccessorListTy getSuccessors() {
    return DestBB;
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::BranchInst;
  }
};

/// A conditional branch.
class CondBranchInst : public TermInst {
  enum {
    /// The condition value used for the branch.
    Condition
  };

  SILSuccessor DestBBs[2];
  // The first argument is the condition; the rest are BB arguments.
  TailAllocatedOperandList<1> Operands;
  
  CondBranchInst(SILLocation Loc, SILValue Condition,
                 SILBasicBlock *TrueBB, SILBasicBlock *FalseBB,
                 ArrayRef<SILValue> Args);
  
public:
  /// Construct a CondBranchInst that will branch to TrueBB or FalseBB based on
  /// the Condition value. Both blocks must not take any arguments.
  static CondBranchInst *create(SILLocation Loc, SILValue Condition,
                                SILBasicBlock *TrueBB,
                                SILBasicBlock *FalseBB,
                                SILFunction &F);

  /// Construct a CondBranchInst that will either branch to TrueBB and pass
  /// TrueArgs or branch to FalseBB and pass FalseArgs based on the Condition
  /// value.
  static CondBranchInst *create(SILLocation Loc, SILValue Condition,
                                SILBasicBlock *TrueBB,
                                ArrayRef<SILValue> TrueArgs,
                                SILBasicBlock *FalseBB,
                                ArrayRef<SILValue> FalseArgs,
                                SILFunction &F);
  
  SILValue getCondition() const { return Operands[Condition].get(); }

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
  
  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::CondBranchInst;
  }
};

/// A switch on a builtin integer value.
class SwitchIntInst : public TermInst {
  FixedOperandList<1> Operands;
  unsigned NumCases : 31;
  unsigned HasDefault : 1;
  
  SwitchIntInst(SILLocation Loc, SILValue Operand,
                SILBasicBlock *DefaultBB,
                ArrayRef<std::pair<APInt, SILBasicBlock*>> CaseBBs);
  
  // Tail-allocated after the SwitchIntInst record are:
  // - `NumCases * getNumWordsForCase()` llvm::integerPart values, containing
  //   the bitwise representations of the APInt value for each case
  // - `NumCases + HasDefault` SILSuccessor records, referencing the
  //   destinations for each case, ending with the default destination if
  //   present.
  
  /// Returns the number of APInt bits required to represent a case value, all
  /// of which are of the operand's type.
  unsigned getBitWidthForCase() const {
    return getOperand().getType().castTo<BuiltinIntegerType>()->getBitWidth();
  }
  
  /// Returns the number of APInt words required to represent a case value, all
  /// of which are of the operand's type.
  unsigned getNumWordsForCase() const {
    return (getBitWidthForCase() + llvm::integerPartWidth - 1)
             / llvm::integerPartWidth;
  }
  
  llvm::integerPart *getCaseBuf() {
    return reinterpret_cast<llvm::integerPart*>(this + 1);
  }
  const llvm::integerPart *getCaseBuf() const {
    return reinterpret_cast<const llvm::integerPart *>(this + 1);
  }
  
  SILSuccessor *getSuccessorBuf() {
    return reinterpret_cast<SILSuccessor*>(
                             getCaseBuf() + (NumCases * getNumWordsForCase()));
  }
  const SILSuccessor *getSuccessorBuf() const {
    return reinterpret_cast<const SILSuccessor *>(
                             getCaseBuf() + (NumCases * getNumWordsForCase()));
  }
  
public:
  /// Clean up tail-allocated successor records for the switch cases.
  ~SwitchIntInst();
  
  static SwitchIntInst *create(SILLocation Loc, SILValue Operand,
                           SILBasicBlock *DefaultBB,
                           ArrayRef<std::pair<APInt, SILBasicBlock*>> CaseBBs,
                           SILFunction &F);
  
  SILValue getOperand() const { return Operands[0].get(); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  SuccessorListTy getSuccessors() {
    return ArrayRef<SILSuccessor>{getSuccessorBuf(), NumCases + HasDefault};
  }
  
  unsigned getNumCases() const { return NumCases; }
  std::pair<APInt, SILBasicBlock*>
  getCase(unsigned i) const {
    assert(i < NumCases && "case out of bounds");
    unsigned words = getNumWordsForCase();
    ArrayRef<llvm::integerPart> parts{getCaseBuf() + i * words, words};
    return {APInt(getBitWidthForCase(), parts), getSuccessorBuf()[i]};
  }
  
  bool hasDefault() const { return HasDefault; }
  SILBasicBlock *getDefaultBB() const {
    assert(HasDefault && "doesn't have a default");
    return getSuccessorBuf()[NumCases];
  }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SwitchIntInst;
  }
};

/// Common implementation for the switch_union and
/// destructive_switch_union_addr instructions.
class SwitchUnionInstBase : public TermInst {
  FixedOperandList<1> Operands;
  unsigned NumCases : 31;
  unsigned HasDefault : 1;
  
  // Tail-allocated after the SwitchUnionInst record are:
  // - an array of `NumCases` UnionElementDecl* pointers, referencing the case
  //   discriminators
  // - `NumCases + HasDefault` SILSuccessor records, referencing the
  //   destinations for each case, ending with the default destination if
  //   present.
  
  UnionElementDecl **getCaseBuf() {
    return reinterpret_cast<UnionElementDecl**>(this + 1);
    
  }
  UnionElementDecl * const* getCaseBuf() const {
    return reinterpret_cast<UnionElementDecl* const*>(this + 1);
    
  }
  
  SILSuccessor *getSuccessorBuf() {
    return reinterpret_cast<SILSuccessor*>(getCaseBuf() + NumCases);
  }
  const SILSuccessor *getSuccessorBuf() const {
    return reinterpret_cast<const SILSuccessor*>(getCaseBuf() + NumCases);
  }

protected:
  SwitchUnionInstBase(ValueKind Kind, SILLocation Loc, SILValue Operand,
                SILBasicBlock *DefaultBB,
                ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs);

  template<typename SWITCH_UNION_INST>
  static SWITCH_UNION_INST *
  createSwitchUnion(SILLocation Loc, SILValue Operand,
                SILBasicBlock *DefaultBB,
                ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs,
                SILFunction &F);
  
public:
  /// Clean up tail-allocated successor records for the switch cases.
  ~SwitchUnionInstBase();
  
  SILValue getOperand() const { return Operands[0].get(); }
  
  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  MutableArrayRef<Operand> getAllOperands() { return Operands.asArray(); }

  SuccessorListTy getSuccessors() {
    return ArrayRef<SILSuccessor>{getSuccessorBuf(), NumCases + HasDefault};
  }
  
  unsigned getNumCases() const { return NumCases; }
  std::pair<UnionElementDecl*, SILBasicBlock*>
  getCase(unsigned i) const {
    assert(i < NumCases && "case out of bounds");
    return {getCaseBuf()[i], getSuccessorBuf()[i].getBB()};
  }
  
  bool hasDefault() const { return HasDefault; }
  SILBasicBlock *getDefaultBB() const {
    assert(HasDefault && "doesn't have a default");
    return getSuccessorBuf()[NumCases];
  }
};
  
/// A switch on a loadable union's discriminator. The data for each case is
/// passed into the corresponding destination block as an argument.
class SwitchUnionInst : public SwitchUnionInstBase {
private:
  friend class SwitchUnionInstBase;
  
  SwitchUnionInst(SILLocation Loc, SILValue Operand,
              SILBasicBlock *DefaultBB,
              ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs)
    : SwitchUnionInstBase(ValueKind::SwitchUnionInst, Loc, Operand, DefaultBB,
                          CaseBBs)
    {}
  
public:
  static SwitchUnionInst *create(SILLocation Loc, SILValue Operand,
               SILBasicBlock *DefaultBB,
               ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs,
               SILFunction &F);

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SwitchUnionInst;
  }
};

/// A switch on an address-only union's discriminator. If a case is matched, the
/// tag is invalidated and the address of the data for the case is passed as
/// an argument to the destination block.
class DestructiveSwitchUnionAddrInst : public SwitchUnionInstBase {
private:
  friend class SwitchUnionInstBase;

  DestructiveSwitchUnionAddrInst(SILLocation Loc, SILValue Operand,
              SILBasicBlock *DefaultBB,
              ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs)
    : SwitchUnionInstBase(ValueKind::DestructiveSwitchUnionAddrInst,
                          Loc, Operand, DefaultBB, CaseBBs)
    {}
  
public:
  static DestructiveSwitchUnionAddrInst *create(
               SILLocation Loc, SILValue Operand,
               SILBasicBlock *DefaultBB,
               ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs,
               SILFunction &F);
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::DestructiveSwitchUnionAddrInst;
  }
};

} // end swift namespace

//===----------------------------------------------------------------------===//
// ilist_traits for SILInstruction
//===----------------------------------------------------------------------===//

namespace llvm {
  
template <>
struct ilist_traits<::swift::SILInstruction> :
  public ilist_default_traits<::swift::SILInstruction> {
  typedef ::swift::SILInstruction SILInstruction;

private:
  mutable ilist_half_node<SILInstruction> Sentinel;

  swift::SILBasicBlock *getContainingBlock();

public:
  SILInstruction *createSentinel() const {
    return static_cast<SILInstruction*>(&Sentinel);
  }
  void destroySentinel(SILInstruction *) const {}

  SILInstruction *provideInitialHead() const { return createSentinel(); }
  SILInstruction *ensureHead(SILInstruction*) const { return createSentinel(); }
  static void noteHead(SILInstruction*, SILInstruction*) {}
  static void deleteNode(SILInstruction *V) {
    SILInstruction::destroy(V);
  }

  void addNodeToList(SILInstruction *I);
  void removeNodeFromList(SILInstruction *I);
  void transferNodesFromList(ilist_traits<SILInstruction> &L2,
                             ilist_iterator<SILInstruction> first,
                             ilist_iterator<SILInstruction> last);

private:
  void createNode(const SILInstruction &);
};

} // end llvm namespace

#endif
