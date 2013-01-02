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
#include "swift/SIL/SILConstant.h"
#include "swift/SIL/Value.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"

namespace swift {

class ValueDecl;
class SILType;
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
class Substitution;

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
  Instruction(ValueKind Kind, SILLocation Loc, SILType Ty)
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

  static bool classof(Value V) {
    return V->getKind() >= ValueKind::First_Instruction &&
           V->getKind() <= ValueKind::Last_Instruction;
  }
  
  /// Invoke an Instruction's destructor. This dispatches to the appropriate
  /// leaf class destructor for the type of the instruction. This does not
  /// deallocate the instruction.
  static void destroy(Instruction *I);
};

enum class AllocKind : uint8_t {
  Heap, Stack, Pseudo
};

/// AllocInst - This is the abstract base class common among all the memory
/// allocation mechanisms.  This can allocate heap or stack memory.
class AllocInst : public Instruction {
  AllocKind allocKind;

protected:
  AllocInst(ValueKind Kind, SILLocation Loc, SILType Ty, AllocKind allocKind)
    : Instruction(Kind, Loc, Ty), allocKind(allocKind) {}
public:
  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

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
  AllocVarInst(SILLocation loc, AllocKind allocKind, SILType elementType,
               Function &F);

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
public:
  AllocBoxInst(SILLocation Loc, SILType ElementType, Function &F);

  Type getElementType() const;

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
  Value NumElements;
public:

  AllocArrayInst(SILLocation Loc, SILType ElementType, Value NumElements,
                 Function &F);

  Type getElementType() const;
  Value getNumElements() const { return NumElements; }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::AllocArrayInst;
  }
};


/// FunctionInst - Abstract instruction type that represents full or partial
/// application of a function.
class FunctionInst : public Instruction {
  /// The instruction representing the called function.
  Value Callee;

  unsigned NumArgs;
  Value *getArgsStorage() { return reinterpret_cast<Value*>(this + 1); }

protected:
  /// Construct an ApplyInst from a given call expression and the provided
  /// arguments.
  FunctionInst(ValueKind kind,
               SILLocation Loc, SILType Ty, Value Callee, ArrayRef<Value> Args);

  template<typename DERIVED, typename...T>
  static DERIVED *create(Function &F, ArrayRef<Value> Args,
                         T &&...ConstructorArgs);

public:
  Value getCallee() { return Callee; }
  
  /// The arguments passed to this instruction.
  MutableArrayRef<Value> getArguments() {
    return MutableArrayRef<Value>(getArgsStorage(), NumArgs);
  }

  /// The arguments passed to this instruction.
  ArrayRef<Value> getArguments() const {
    return const_cast<FunctionInst*>(this)->getArguments();
  }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() >= ValueKind::First_FunctionInst &&
           V->getKind() <= ValueKind::Last_FunctionInst;
  }
};
  
/// ApplyInst - Represents the full application of a function value.
class ApplyInst : public FunctionInst {
  friend class FunctionInst;
  ApplyInst(SILLocation Loc, Value Callee, SILType ReturnType,
            ArrayRef<Value> Args);
public:
  static ApplyInst *create(SILLocation Loc, Value Callee,
                           SILType ReturnType,
                           ArrayRef<Value> Args,
                           Function &F);
};

/// ClosureInst - Represents the creation of a closure object by partial
/// application of a function value.
class ClosureInst : public FunctionInst {
  friend class FunctionInst;
  ClosureInst(SILLocation Loc, Value Callee, ArrayRef<Value> Args);
public:
  static ClosureInst *create(SILLocation Loc, Value Callee,
                             ArrayRef<Value> Args, Function &F);
};

/// ConstantRefInst - Represents a reference to a *constant* declaration,
/// evaluating to its value.
class ConstantRefInst : public Instruction {
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

  static bool classof(Value V) {
    return V->getKind() == ValueKind::ConstantRefInst;
  }
};

/// A default "zero" value used to initialize a variable that was not otherwise
/// explicitly initialized.
class ZeroValueInst : public Instruction {
public:
  ZeroValueInst(SILLocation Loc, SILType Ty);

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

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
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

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
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

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
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

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
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

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

/// ZeroAddrInst - Represents a zeroing of a buffer in memory. This is similar
/// to:
///   %1 = zero_value $T
///   store %1 to %dest
/// but zero_addr must be used for address-only types.
class ZeroAddrInst : public Instruction {
  /// Dest - the address of the buffer to be zeroed.
  Value Dest;
  
public:
  ZeroAddrInst(SILLocation Loc, Value Dest);
  
  Value getDest() const { return Dest; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ZeroAddrInst;
  }
};

/// CopyAddrInst - Represents a copy from one memory location to another. This
/// is similar to:
///   %1 = load %src
///   store %1 -> %dest
/// but a copy instruction must be used for address-only types.
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
  /// The value being specialized.  It always has PolymorphicFunctionType.
  Value Operand;
  unsigned NumSubstitutions;
  Substitution *getSubstitutionsStorage() {
    return reinterpret_cast<Substitution *>(this + 1);
  }
  Substitution const *getSubstitutionsStorage() const {
    return reinterpret_cast<Substitution const *>(this + 1);
  }

  SpecializeInst(SILLocation Loc, Value Operand,
                 ArrayRef<Substitution> Substitutions,
                 SILType DestTy);

public:
  static SpecializeInst *create(SILLocation Loc, Value Operand,
                                ArrayRef<Substitution> Substitutions,
                                SILType DestTy,
                                Function &F);

  Value getOperand() const { return Operand; }
  
  ArrayRef<Substitution> getSubstitutions() const {
    return ArrayRef<Substitution>(getSubstitutionsStorage(), NumSubstitutions);
  }
  
  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::SpecializeInst;
  }
};

/// ConversionInst - Abstract class representing instructions that convert
/// values
class ConversionInst : public Instruction {
  Value Operand;
public:
  ConversionInst(ValueKind Kind, SILLocation Loc, Value Operand, SILType Ty);
  
  Value getOperand() const { return Operand; }
  
  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }
  
  static bool classof(Value V) {
    return V->getKind() >= ValueKind::First_ConversionInst &&
      V->getKind() <= ValueKind::Last_ConversionInst;
  }
};

/// ImplicitConvertInst - Change the type of some value without affecting how it
/// will codegen.
class ImplicitConvertInst : public ConversionInst {
public:
  ImplicitConvertInst(SILLocation Loc, Value Operand, SILType Ty);
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ImplicitConvertInst;
  }
};

/// CoerceInst - Convert a value to a type with an explicit T(x) cast.
class CoerceInst : public ConversionInst {
public:
  CoerceInst(SILLocation Loc, Value Operand, SILType ty);
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::CoerceInst;
  }
};

/// DowncastInst - Perform a checked conversion of a value to a subclass type.
class DowncastInst : public ConversionInst {
public:
  DowncastInst(SILLocation Loc, Value Operand, SILType ty);
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::DowncastInst;
  }
};
  
/// ArchetypeToSuperInst - Given the address of an archetype value with a base
/// class constraint, returns a reference to the base class instance.
class ArchetypeToSuperInst : public ConversionInst {
public:
  ArchetypeToSuperInst(SILLocation Loc, Value Archetype, SILType BaseTy);
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ArchetypeToSuperInst;
  }
};

/// SuperToArchetypeInst - Given a value of a class type, initializes an
/// archetype with a base class constraint to contain a reference to the value.
class SuperToArchetypeInst : public Instruction {
  Value SrcBase, DestArchetypeAddress;
public:
  SuperToArchetypeInst(SILLocation Loc,
                       Value SrcBase,
                       Value DestArchetypeAddress);
  
  Value getSrcBase() const { return SrcBase; }
  Value getDestArchetypeAddress() const { return DestArchetypeAddress; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::SuperToArchetypeInst;
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
  TupleInst(SILLocation Loc, SILType Ty, ArrayRef<Value> Elements);
  static TupleInst *createImpl(SILLocation Loc, SILType Ty,
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
  static TupleInst *create(SILLocation Loc, SILType Ty, ArrayRef<Value> Elements,
                           Function &F) {
    return createImpl(Loc, Ty, Elements, F);
  }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::TupleInst;
  }
};

/// MetatypeInst - Represents the production of an instance of a given metatype.
class MetatypeInst : public Instruction {
public:

  /// Constructs a MetatypeInst
  MetatypeInst(SILLocation Loc, SILType Metatype);

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::MetatypeInst;
  }
};

/// AssociatedMetatypeInst - Extract the metatype of an associated type from a
/// metatype.
class AssociatedMetatypeInst : public Instruction {
private:
  Value SourceMetatype;
public:
  AssociatedMetatypeInst(SILLocation Loc,
                         Value MetatypeSrc, SILType MetatypeDest);
  
  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }
  
  Value getSourceMetatype() const { return SourceMetatype; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::AssociatedMetatypeInst;
  }
};

/// ExtractInst - Extract a numbered element out of a value of tuple or fragile
/// struct type.
class ExtractInst : public Instruction {
  Value Operand;
  unsigned FieldNo;
public:
  ExtractInst(SILLocation Loc, Value Operand, unsigned FieldNo,
              SILType ResultTy);
  
  Value getOperand() const { return Operand; }
  unsigned getFieldNo() const { return FieldNo; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ExtractInst;
  }
};

/// ElementAddrInst - Derive the address of a numbered element from the address
/// of a tuple or fragile struct type.
class ElementAddrInst : public Instruction {
  Value Operand;
  unsigned FieldNo;
public:
  ElementAddrInst(SILLocation Loc, Value Operand, unsigned FieldNo,
                  SILType ResultTy);
  
  Value getOperand() const { return Operand; }
  unsigned getFieldNo() const { return FieldNo; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ElementAddrInst;
  }
};

/// RefElementAddrInst - Derive the address of a numbered element in a reference
/// type instance.
class RefElementAddrInst : public Instruction {
  Value Operand;
  unsigned FieldNo;
public:
  RefElementAddrInst(SILLocation Loc, Value Operand, unsigned FieldNo,
                     SILType ResultTy);
  
  Value getOperand() const { return Operand; }
  unsigned getFieldNo() const { return FieldNo; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::RefElementAddrInst;
  }
};
  
/// WitnessTableMethodInst - Abstract base for instructions that query
/// protocol witness tables for method implementations.
class WitnessTableMethodInst : public Instruction {
  Value Operand;
  SILConstant Member;
public:
  WitnessTableMethodInst(ValueKind Kind,
                         SILLocation Loc, Value Operand, SILConstant Member,
                         SILType thisTy, SILType methodTy, Function &F);
  
  Value getOperand() const { return Operand; }
  SILConstant getMember() const { return Member; }
  
  static bool classof(Value V) {
    return V->getKind() >= ValueKind::First_WitnessTableMethodInst &&
      V->getKind() <= ValueKind::Last_WitnessTableMethodInst;
  }
};
  
/// ArchetypeMethodInst - Given the address of an archetype value and a method
/// constant, extracts the implementation of that method for the archetype.
class ArchetypeMethodInst : public WitnessTableMethodInst {
public:
  ArchetypeMethodInst(SILLocation Loc, Value Operand, SILConstant Member,
                      SILType methodTy, Function &F);
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ArchetypeMethodInst;
  }
};
  
/// ExistentialMethodInst - Given the address of an existential and a method
/// constant, extracts the implementation of that method for the existential.
/// The result will be of the type RawPointer -> F for a method of function type
/// F. The RawPointer "this" argument can be derived from the same existential
/// using a ProjectExistentialInst.
class ExistentialMethodInst : public WitnessTableMethodInst {
public:
  ExistentialMethodInst(SILLocation Loc, Value Operand, SILConstant Member,
                        SILType methodTy, Function &F);

  static bool classof(Value V) {
    return V->getKind() == ValueKind::ExistentialMethodInst;
  }
};
  
/// ProjectExistentialInst - Given the address of an existential, returns a
/// RawPointer pointing to the value inside the existential.
class ProjectExistentialInst : public Instruction {
  Value Operand;
public:
  ProjectExistentialInst(SILLocation Loc, Value Operand, Function &F);
  
  Value getOperand() const { return Operand; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::ProjectExistentialInst;
  }
};
  
/// AllocExistentialInst - Given an address to an uninitialized buffer of
/// an existential type, initializes the existential to contain a value of
/// a given concrete type, and returns the address of an uninitialized buffer
/// of the concrete type that then must be initialized.
class AllocExistentialInst : public Instruction {
  Value Existential;
public:
  AllocExistentialInst(SILLocation Loc,
                       Value Existential, SILType ConcreteType, Function &F);
  
  Value getExistential() const { return Existential; }
  Type getConcreteType() const;
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
  
/// DeallocExistentialInst - Given an address of an existential that has been
/// partially initialized with an AllocExistentialInst but whose value buffer
/// has not been initialized, deinitializes the existential and deallocates
/// the value buffer. This should only be used for partially-initialized
/// existentials; a fully-initialized existential can be destroyed with
/// DestroyAddrInst and deallocated with DeallocVarInst.
class DeallocExistentialInst : public Instruction {
  Value Existential;
public:
  DeallocExistentialInst(SILLocation Loc, Value Existential);
  
  Value getExistential() const { return Existential; }
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
  
/// DeallocRefInst - Deallocate memory allocated for a box or reference type
/// instance. This does not destroy the referenced instance; it must either be
/// uninitialized or have been manually destroyed.
class DeallocRefInst : public Instruction {
  Value Operand;
public:
  DeallocRefInst(SILLocation Loc, Value Operand);
  
  Value getOperand() const { return Operand; }
  
  static bool classof(Value V) {
    return V->getKind() == ValueKind::DeallocRefInst;
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
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  static bool classof(Value V) {
    return V->getKind() == ValueKind::IndexAddrInst;
  }
};

/// IntegerValueInst - Always produces an integer of the specified value.  These
/// always have Builtin.Integer type.
class IntegerValueInst : public Instruction {
  uint64_t Val;
public:
  IntegerValueInst(uint64_t Val, SILType Ty);

  uint64_t getValue() const { return Val; }

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

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
  TermInst(ValueKind K, SILLocation Loc, SILType Ty)
    : Instruction(K, Loc, Ty) {}
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
  static void deleteNode(Instruction *V) {
    Instruction::destroy(V);
  }

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
