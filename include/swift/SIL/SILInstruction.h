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
#include "swift/SIL/SILSuccessor.h"
#include "swift/SIL/SILConstant.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"

namespace swift {

class ValueDecl;
class SILType;
class SILFunction;
class SILBasicBlock;
class CharacterLiteralExpr;
class DeclRefExpr;
class FloatLiteralExpr;
class IntegerLiteralExpr;
class MetatypeExpr;
class StringLiteralExpr;
class Stmt;
class VarDecl;
class Substitution;

/// This is the root class for all instructions that can be used as the contents
/// of a Swift SILBasicBlock.
class SILInstruction : public ValueBase, public llvm::ilist_node<SILInstruction> {
  friend struct llvm::ilist_traits<SILInstruction>;

  /// A backreference to the containing basic block.  This is maintained by
  /// ilist_traits<SILInstruction>.
  SILBasicBlock *ParentBB;

  SILLocation Loc;

  friend struct llvm::ilist_sentinel_traits<SILInstruction>;
  SILInstruction() = delete;
  void operator=(const SILInstruction &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

protected:
  SILInstruction(ValueKind Kind, SILLocation Loc, SILType Ty)
    : ValueBase(Kind, Ty), ParentBB(0), Loc(Loc) {}
  SILInstruction(ValueKind Kind, SILLocation Loc, SILTypeList *TypeList = 0)
    : ValueBase(Kind, TypeList), ParentBB(0), Loc(Loc) {}

public:

  const SILBasicBlock *getParent() const { return ParentBB; }
  SILBasicBlock *getParent() { return ParentBB; }

  SILLocation getLoc() const { return Loc; }

  /// Return the AST expression that this instruction is produced from, or null
  /// if it is implicitly generated.  Note that this is aborts on locations that
  /// come from statements.
  template<typename T>
  T *getLocDecl() const { return cast_or_null<T>(Loc.get<Decl*>()); }

  /// Return the AST expression that this instruction is produced from, or null
  /// if it is implicitly generated.  Note that this is aborts on locations that
  /// come from statements.
  template<typename T>
  T *getLocExpr() const { return cast_or_null<T>(Loc.get<Expr*>()); }

  /// Return the AST statement that this instruction is produced from, or null
  /// if it is implicitly generated.  Note that this is aborts on locations that
  /// come from statements.
  template<typename T>
  T *getLocStmt() const { return cast_or_null<T>(Loc.get<Stmt*>()); }


  /// removeFromParent - This method unlinks 'this' from the containing basic
  /// block, but does not delete it.
  ///
  void removeFromParent();
  
  /// eraseFromParent - This method unlinks 'this' from the containing basic
  /// block and deletes it.
  ///
  void eraseFromParent();

  /// Return the array of operands for this instruction.
  ArrayRef<Operand> getAllOperands() const;

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
  static constexpr bool has_result() { return HAS_RESULT; }
public:
  
  UnaryInstructionBase(SILLocation Loc, SILValue Operand)
    : BASE(KIND, Loc), Operands(this, Operand) {}

  template<typename X = void>
  UnaryInstructionBase(SILLocation Loc, SILValue Operand,
                     typename std::enable_if<has_result<X>(), SILType>::type Ty)
    : BASE(KIND, Loc, Ty), Operands(this, Operand) {}
  
  template<typename X = void, typename...A>
  UnaryInstructionBase(SILLocation Loc, SILValue Operand,
                     typename std::enable_if<has_result<X>(), SILType>::type Ty,
                     A &&...args)
    : BASE(KIND, Loc, Ty, std::forward<A>(args)...), Operands(this, Operand) {}
  
  SILValue getOperand() const { return Operands[0].get(); }
  
  /// getType() is ok if this is known to only have one type.
  template<typename X = void>
  typename std::enable_if<has_result<X>(), SILType>::type
  getType(unsigned i = 0) const { return ValueBase::getType(i); }
  
  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == KIND;
  }
};
  
enum class AllocKind : uint8_t {
  Heap, Stack, Pseudo
};

/// AllocInst - This is the abstract base class common among all the memory
/// allocation mechanisms.  This can allocate heap or stack memory.
class AllocInst : public SILInstruction {
  AllocKind allocKind;

protected:
  AllocInst(ValueKind Kind, SILLocation Loc, SILType Ty, AllocKind allocKind)
    : SILInstruction(Kind, Loc, Ty), allocKind(allocKind) {}
public:
  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  AllocKind getAllocKind() const { return allocKind; }

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_AllocInst &&
           V->getKind() <= ValueKind::Last_AllocInst;
  }
};

/// AllocVarInst - This represents the allocation of an unboxed variable or
/// temporary. The memory is provided uninitialized.
class AllocVarInst : public AllocInst {
public:
  AllocVarInst(SILLocation loc, AllocKind allocKind, SILType elementType,
               SILFunction &F);

  /// getDecl - Return the underlying variable declaration associated with this
  /// allocation, or null if this is a temporary allocation.
  VarDecl *getDecl() const;
  
  /// getElementType - Get the type of the allocated memory (as opposed to the
  /// type of the instruction itself, which will be an address type).
  Type getElementType() const;

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocVarInst;
  }
};
  
/// AllocRefInst - This represents the primitive allocation of an instance
/// of a reference type. Aside from the reference count, the instance is
/// returned uninitialized.
class AllocRefInst : public AllocInst {
public:
  AllocRefInst(SILLocation loc, AllocKind allocKind, SILType type,
               SILFunction &F);
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocRefInst;
  }
};

/// AllocBoxInst - This represents the allocation of a heap box for a Swift
/// value of some type, and whose element memory is left uninitialized.  This
/// returns two values.  The first return element is the object pointer (pointer
/// to the object header) with Builtin.ObjectPointer type.  The second element
/// returned is an lvalue to the element.
///
class AllocBoxInst : public SILInstruction {
public:
  AllocBoxInst(SILLocation Loc, SILType ElementType, SILFunction &F);

  Type getElementType() const;

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }

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

  Type getElementType() const;
  SILValue getNumElements() const { return Operands[NumElements].get(); }


  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::AllocArrayInst;
  }
};


/// FunctionInst - Abstract instruction type that represents full or partial
/// application of a function.
class FunctionInst : public SILInstruction {
  enum {
    Callee
  };
  /// The fixed operand is the callee;  the rest are arguments.
  TailAllocatedOperandList<1> Operands;

protected:
  /// Construct an ApplyInst from a given call expression and the provided
  /// arguments.
  FunctionInst(ValueKind kind,
               SILLocation Loc, SILType Ty, SILValue Callee,
               ArrayRef<SILValue> Args);

  template<typename DERIVED, typename...T>
  static DERIVED *create(SILFunction &F, ArrayRef<SILValue> Args,
                         T &&...ConstructorArgs);

public:
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

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_FunctionInst &&
           V->getKind() <= ValueKind::Last_FunctionInst;
  }
};
  
/// ApplyInst - Represents the full application of a function value.
class ApplyInst : public FunctionInst {
  friend class FunctionInst;
  ApplyInst(SILLocation Loc, SILValue Callee, SILType ReturnType,
            ArrayRef<SILValue> Args);
public:
  static ApplyInst *create(SILLocation Loc, SILValue Callee,
                           SILType ReturnType,
                           ArrayRef<SILValue> Args,
                           SILFunction &F);

  bool hasIndirectReturn() const {
    return getCallee().getType().getFunctionTypeInfo()->hasIndirectReturn();
  }
  
  SILValue getIndirectReturn() const {
    assert(hasIndirectReturn() && "apply inst does not have indirect return!");
    return getArguments().back();
  }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::ApplyInst;
  }
};

/// PartialApplyInst - Represents the creation of a closure object by partial
/// application of a function value.
class PartialApplyInst : public FunctionInst {
  friend class FunctionInst;
  PartialApplyInst(SILLocation Loc, SILValue Callee, ArrayRef<SILValue> Args,
                   SILType ClosureType);
public:
  static PartialApplyInst *create(SILLocation Loc, SILValue Callee,
                                  ArrayRef<SILValue> Args,
                                  SILType ClosureType,
                                  SILFunction &F);

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::PartialApplyInst;
  }
};

/// ConstantRefInst - Represents a reference to a *constant* declaration,
/// evaluating to its value.
class ConstantRefInst : public SILInstruction {
  SILConstant Constant;
public:
  /// Construct a ConstantRefInst.
  ///
  /// \param Loc  The location of the reference.
  /// \param C    The constant being referenced.
  /// \param F    The type of the constant. Must be a function type.
  ConstantRefInst(SILLocation Loc, SILConstant C, SILType Ty);

  /// getConstant - Return the referenced constant.
  SILConstant getConstant() const;

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::ConstantRefInst;
  }
};

/// IntegerLiteralInst - Encapsulates an integer constant, as defined originally
/// by an an IntegerLiteralExpr or CharacterLiteralExpr.
class IntegerLiteralInst : public SILInstruction {
public:
  IntegerLiteralInst(IntegerLiteralExpr *E);
  IntegerLiteralInst(CharacterLiteralExpr *E);
  
  Expr *getExpr() const;
  
  /// getValue - Return the APInt for the underlying integer literal.
  APInt getValue() const;

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::IntegerLiteralInst;
  }
};

/// FloatLiteralInst - Encapsulates a floating point constant, as defined
/// originally by a FloatLiteralExpr.
class FloatLiteralInst : public SILInstruction {
public:
  FloatLiteralInst(FloatLiteralExpr *E);

  FloatLiteralExpr *getExpr() const;

  /// getValue - Return the APFloat for the underlying FP literal.
  APFloat getValue() const;

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::FloatLiteralInst;
  }
};

/// StringLiteralInst - Encapsulates a string constant, as defined originally by
/// a StringLiteralExpr.
class StringLiteralInst : public SILInstruction {
public:
  StringLiteralInst(StringLiteralExpr *E, SILType ty);

  StringLiteralExpr *getExpr() const;

  /// getValue - Return the string data for the literal.
  StringRef getValue() const;

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }

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
  /// \param Expr The backing LoadExpr in the AST.
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

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::StoreInst;
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
               bool IsTakeOfSrc, bool IsInitializationOfDest);
  
  SILValue getSrc() const { return Operands[Src].get(); }
  SILValue getDest() const { return Operands[Dest].get(); }
  bool isTakeOfSrc() const { return IsTakeOfSrc; }
  bool isInitializationOfDest() const { return IsInitializationOfDest; }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }

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

/// DowncastInst - Perform a checked conversion of a class instance to a
/// subclass type.
class DowncastInst
  : public UnaryInstructionBase<ValueKind::DowncastInst, ConversionInst>
{
public:
  DowncastInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};
  
/// AddressToPointerInst - Convert a SIL address to a Builtin.RawPointer value.
class AddressToPointerInst
  : public UnaryInstructionBase<ValueKind::AddressToPointerInst, ConversionInst>
{
public:
  AddressToPointerInst(SILLocation Loc, SILValue Operand, SILType Ty)
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
  
/// BridgeToBlockInst - Converts a Swift function value to an ObjC-compatible
/// block.
class BridgeToBlockInst
  : public UnaryInstructionBase<ValueKind::BridgeToBlockInst, ConversionInst>
{
public:
  BridgeToBlockInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};

/// ArchetypeToSuperInst - Given the address of an archetype value with a base
/// class constraint, returns a reference to the base class instance.
class ArchetypeToSuperInst
  : public UnaryInstructionBase<ValueKind::ArchetypeToSuperInst, ConversionInst>
{
public:
  ArchetypeToSuperInst(SILLocation Loc, SILValue Operand, SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty) {}
};
  
/// SuperToArchetypeInst - Given a value of a class type, initializes an
/// archetype with a base class constraint to contain a reference to the value.
class SuperToArchetypeInst : public SILInstruction {
  enum {
    /// The value of class type.
    SrcBase,
    /// The address to store to.
    DestArchetypeAddress
  };
  FixedOperandList<2> Operands;
public:
  SuperToArchetypeInst(SILLocation Loc,
                       SILValue SrcBase,
                       SILValue DestArchetypeAddress);
  
  SILValue getSrcBase() const { return Operands[SrcBase].get(); }
  SILValue getDestArchetypeAddress() const {
    return Operands[DestArchetypeAddress].get();
  }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SuperToArchetypeInst;
  }
};

/// StructInst - Represents a constructed tuple.
class StructInst : public SILInstruction {
  TailAllocatedOperandList<0> Operands;

  /// Private constructor.  Because of the storage requirements of
  /// StructInst, object creation goes through 'create()'.
  StructInst(SILLocation Loc, SILType Ty, ArrayRef<SILValue> Elements);
  static StructInst *createImpl(SILLocation Loc, SILType Ty,
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

  /// Construct a StructInst.
  static StructInst *create(SILLocation Loc, SILType Ty,
                            ArrayRef<SILValue> Elements, SILFunction &F) {
    return createImpl(Loc, Ty, Elements, F);
  }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::StructInst;
  }
};

/// TupleInst - Represents a constructed tuple.
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

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::TupleInst;
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

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }
  
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

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::ModuleInst;
  }
};
  
/// AssociatedMetatypeInst - Extract the metatype of an associated type from a
/// metatype.
class AssociatedMetatypeInst
  : public UnaryInstructionBase<ValueKind::AssociatedMetatypeInst>
{
public:
  AssociatedMetatypeInst(SILLocation Loc, SILValue Base, SILType Metatype)
    : UnaryInstructionBase(Loc, Base, Metatype) {}
};

/// ExtractInst - Extract a numbered element out of a value of tuple or fragile
/// struct type.
class ExtractInst : public UnaryInstructionBase<ValueKind::ExtractInst> {
  unsigned FieldNo;
public:
  ExtractInst(SILLocation Loc, SILValue Operand, unsigned FieldNo,
              SILType ResultTy)
    : UnaryInstructionBase(Loc, Operand, ResultTy), FieldNo(FieldNo) {}
  
  unsigned getFieldNo() const { return FieldNo; }
};

/// ElementAddrInst - Derive the address of a numbered element from the address
/// of a tuple or fragile struct type.
class ElementAddrInst : public UnaryInstructionBase<ValueKind::ElementAddrInst>
{
  unsigned FieldNo;
public:
  ElementAddrInst(SILLocation Loc, SILValue Operand, unsigned FieldNo,
                  SILType ResultTy)
    : UnaryInstructionBase(Loc, Operand, ResultTy), FieldNo(FieldNo) {}
  
  unsigned getFieldNo() const { return FieldNo; }
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
  SILConstant Member;
public:
  DynamicMethodInst(ValueKind Kind,
                    SILLocation Loc, SILType Ty,
                    SILConstant Member)
    : SILInstruction(Kind, Loc, Ty), Member(Member) {}
  
  SILConstant getMember() const { return Member; }
  
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
  ClassMethodInst(SILLocation Loc, SILValue Operand, SILConstant Member,
                  SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty, Member) {}
};

/// SuperMethodInst - Given the address of a value of class type and a method
/// constant, extracts the implementation of that method for the superclass of
/// the static type of the class.
class SuperMethodInst
  : public UnaryInstructionBase<ValueKind::SuperMethodInst, DynamicMethodInst>
{
public:
  SuperMethodInst(SILLocation Loc, SILValue Operand, SILConstant Member,
                  SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty, Member) {}
};

/// ArchetypeMethodInst - Given the address of an archetype value and a method
/// constant, extracts the implementation of that method for the archetype.
class ArchetypeMethodInst
  : public UnaryInstructionBase<ValueKind::ArchetypeMethodInst,
                                DynamicMethodInst>
{
public:
  ArchetypeMethodInst(SILLocation Loc, SILValue Operand, SILConstant Member,
                      SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty, Member) {}
};
  
/// ProtocolMethodInst - Given the address of an existential and a method
/// constant, extracts the implementation of that method for the existential.
/// The result will be of the type RawPointer -> F for a method of function type
/// F. The RawPointer "this" argument can be derived from the same existential
/// using a ProjectExistentialInst.
class ProtocolMethodInst
  : public UnaryInstructionBase<ValueKind::ProtocolMethodInst,
                                DynamicMethodInst>
{
public:
  ProtocolMethodInst(SILLocation Loc, SILValue Operand, SILConstant Member,
                     SILType Ty)
    : UnaryInstructionBase(Loc, Operand, Ty, Member) {}
};
  
/// ProjectExistentialInst - Given the address of an existential, returns a
/// RawPointer pointing to the value inside the existential.
class ProjectExistentialInst
  : public UnaryInstructionBase<ValueKind::ProjectExistentialInst>
{
public:
  ProjectExistentialInst(SILLocation Loc, SILValue Operand, SILFunction &F);
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

  Type getConcreteType() const {
    return getType().getSwiftRValueType();
  }
};

/// DeinitExistentialInst - Given an address of an existential that has been
/// partially initialized with an InitExistentialInst but whose value buffer
/// has not been initialized, deinitializes the existential and deallocates
/// the value buffer. This should only be used for partially-initialized
/// existentials; a fully-initialized existential can be destroyed with
/// DestroyAddrInst and deallocated with DeallocVarInst.
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
  ArrayRef<ProtocolConformance*> Conformances;
public:
  UpcastExistentialInst(SILLocation Loc,
                        SILValue SrcExistential,
                        SILValue DestExistential,
                        bool isTakeOfSrc,
                        ArrayRef<ProtocolConformance*> Conformances);
  
  SILValue getSrcExistential() const { return Operands[SrcExistential].get(); }
  SILValue getDestExistential() const { return Operands[DestExistential].get();}

  /// True if the destination can take ownership of the concrete value from the
  /// source.
  bool isTakeOfSrc() const { return IsTakeOfSrc; }
  
  ArrayRef<ProtocolConformance*> getConformances() const {
    return Conformances;
  }
  
  ArrayRef<Operand> getAllOperands() const { return Operands.asArray(); }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::UpcastExistentialInst;
  }
};
  
/// RetainInst - Increase the retain count of a value.
class RetainInst : public UnaryInstructionBase<ValueKind::RetainInst,
                                               SILInstruction,
                                               /*HAS_RESULT*/ false>
{
public:
  RetainInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};
  
/// RetainAutoreleasedInst - Take ownership of the autoreleased return value of
/// an ObjC method.
class RetainAutoreleasedInst
  : public UnaryInstructionBase<ValueKind::RetainAutoreleasedInst,
                                SILInstruction, /*HAS_RESULT*/ false>
{
public:
  RetainAutoreleasedInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};

/// ReleaseInst - Decrease the retain count of a value, and dealloc the value
/// if its retain count is zero.
class ReleaseInst
  : public UnaryInstructionBase<ValueKind::ReleaseInst,
                                SILInstruction, /*HAS_RESULT*/ false>
{
public:
  ReleaseInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};

/// DeallocVarInst - Deallocate memory allocated by alloc_var.
class DeallocVarInst : public UnaryInstructionBase<ValueKind::DeallocVarInst,
                                                   SILInstruction,
                                                   /*HAS_RESULT*/ false>
{
  AllocKind allocKind;
public:
  DeallocVarInst(SILLocation Loc, AllocKind allocKind, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand), allocKind(allocKind) {}
  
  AllocKind getAllocKind() const { return allocKind; }
};
  
/// DeallocRefInst - Deallocate memory allocated for a box or reference type
/// instance. This does not destroy the referenced instance; it must either be
/// uninitialized or have been manually destroyed.
class DeallocRefInst : public UnaryInstructionBase<ValueKind::DeallocRefInst,
                                                   SILInstruction,
                                                   /*HAS_RESULT*/ false>
{
public:
  DeallocRefInst(SILLocation Loc, SILValue Operand)
    : UnaryInstructionBase(Loc, Operand) {}
};

/// DestroyAddrInst - Destroy the value at a memory location and deallocate the
/// memory. This is similar to:
///   %1 = load %operand
///   release %1
///   dealloc %operand
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
// SIL-only instructions that don't have an AST analog
//===----------------------------------------------------------------------===//


/// IndexAddrInst - "%1 = index_addr %0, 42"
/// This takes an lvalue and indexes over the pointer, striding by the type of
/// the lvalue.  This is used to index into arrays of uniform elements.
class IndexAddrInst : public UnaryInstructionBase<ValueKind::IndexAddrInst> {
  unsigned Index;
public:
  IndexAddrInst(SILLocation Loc, SILValue Operand, unsigned Index)
    : UnaryInstructionBase(Loc, Operand, Operand.getType()), Index(Index) {}
  
  unsigned getIndex() const { return Index; }
};

/// IntegerValueInst - Always produces an integer of the specified value.  These
/// always have Builtin.Integer type.
/// FIXME: This should be combined with IntegerLiteralInst.
class IntegerValueInst : public SILInstruction {
  uint64_t Val;
public:
  IntegerValueInst(uint64_t Val, SILType Ty);

  uint64_t getValue() const { return Val; }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }
  
  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::IntegerValueInst;
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

  typedef llvm::ArrayRef<SILSuccessor> SuccessorListTy;

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
  UnreachableInst(SILFunction &F);
  
  SuccessorListTy getSuccessors() {
    // No Successors.
    return SuccessorListTy();
  }

  ArrayRef<Operand> getAllOperands() const { return ArrayRef<Operand>(); }
  
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

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::CondBranchInst;
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
