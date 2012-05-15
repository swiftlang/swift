//===--- Types.h - Swift Language Type ASTs ---------------------*- C++ -*-===//
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
// This file defines the TypeBase class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPES_H
#define SWIFT_TYPES_H

#include "swift/AST/DeclContext.h"
#include "swift/AST/Type.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"

namespace llvm {
  struct fltSemantics;
}
namespace swift {
  class ASTContext;
  class ClassDecl;
  class Expr;
  class Identifier;
  class TypeAliasDecl;
  class TypeDecl;
  class OneOfDecl;
  class OneOfElementDecl;
  class StructDecl;
  class ProtocolDecl;
  class ValueDecl;
  class Module;
  
  enum class TypeKind {
#define TYPE(id, parent) id,
#include "swift/AST/TypeNodes.def"
  };
  
/// TypeBase - Base class for all types in Swift.
class TypeBase {
  friend class ASTContext;
  TypeBase(const TypeBase&) = delete;
  void operator=(const TypeBase&) = delete;
  
  /// CanonicalType - This field is always set to the ASTContext for canonical
  /// types, and is otherwise lazily populated by ASTContext when the canonical
  /// form of a non-canonical type is requested.
  llvm::PointerUnion<TypeBase *, ASTContext*> CanonicalType;

  /// Kind - The discriminator that indicates what subclass of type this is.
  const TypeKind Kind;

  struct TypeBaseBits {
    /// Dependent - Whether this type is dependent.
    unsigned Dependent : 1;
  };
  static const unsigned NumTypeBaseBits = 1;
  
  union {
    TypeBaseBits TypeBase;
  } TypeBits;
  
protected:
  TypeBase(TypeKind kind, ASTContext *CanTypeCtx, bool Dependent)
    : CanonicalType((TypeBase*)nullptr), Kind(kind) {
    // If this type is canonical, switch the CanonicalType union to ASTContext.
    if (CanTypeCtx)
      CanonicalType = CanTypeCtx;
    
    setDependent(Dependent);
  }

  /// \brief Mark this bit as dependent
  void setDependent(bool D = true) { TypeBits.TypeBase.Dependent = D; }
  
public:
  /// getKind - Return what kind of type this is.
  TypeKind getKind() const { return Kind; }

  /// isCanonical - Return true if this is a canonical type.
  bool isCanonical() const { return CanonicalType.is<ASTContext*>(); }
  
  /// hasCanonicalTypeComputed - Return true if we've already computed a
  /// canonical version of this type.
  bool hasCanonicalTypeComputed() const { return !CanonicalType.isNull(); }
  
  /// getCanonicalType - Return the canonical version of this type, which has
  /// sugar from all levels stripped off.
  CanType getCanonicalType();
  
  /// getASTContext - Return the ASTContext that this type belongs to.
  ASTContext &getASTContext() {
    // If this type is canonical, it has the ASTContext in it.
    if (CanonicalType.is<ASTContext*>())
      return *CanonicalType.get<ASTContext*>();
    // If not, canonicalize it to get the Context.
    return *getCanonicalType()->CanonicalType.get<ASTContext*>();
  }
  
  /// isEqual - Return true if these two types are equal, ignoring sugar.
  bool isEqual(Type Other);
  
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

  /// If this type is a (potentially sugared) type of the specified kind, remove
  /// the minimal amount of sugar required to get a pointer to the type.
  template <typename T>
  T *getAs() {
    return dyn_cast<T>(getDesugaredType());
  }

  template <typename T>
  bool is() {
    return isa<T>(getDesugaredType());
  }
  
  template <typename T>
  T *castTo() {
    return cast<T>(getDesugaredType());
  }

  
  /// getString - Return the name of the type as a string, for use in
  /// diagnostics only.
  std::string getString() const;

  /// isMaterializable - Is this type 'materializable' according to
  /// the rules of the language?  Basically, does it not contain any
  /// l-value types?
  bool isMaterializable();

  /// hasReferenceSemantics() - Do objects of this type have reference
  /// semantics?
  bool hasReferenceSemantics();
  
  /// isDependentType() - Determines whether this type is a dependent
  /// type, meaning that part of the type depends on the context in which
  /// the type occurs.
  bool isDependentType() const;
  
  /// getUnlabeledType - Retrieve a version of this type with all labels
  /// removed at every level. For example, given a tuple type 
  /// \code
  /// (p : (x : int, y : int))
  /// \endcode
  /// the result would be the (parenthesized) type ((int, int)).
  Type getUnlabeledType(ASTContext &Context);
  
  void dump() const;
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *) { return true; }

private:
  // Make placement new and vanilla new/delete illegal for Types.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
public:
  // Only allow allocation of Types using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8);  
};

/// ErrorType - This represents a type that was erroneously constructed.  This
/// is produced when parsing types and when name binding type aliases, and is
/// installed in declaration that use these erroneous types.  All uses of a
/// declaration of invalid type should be ignored and not re-diagnosed.
class ErrorType : public TypeBase {
  friend class ASTContext;
  // The Error type is always canonical.
  ErrorType(ASTContext &C) 
    : TypeBase(TypeKind::Error, &C, /*Dependent=*/false) { }
public:
  static Type get(ASTContext &C);
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ErrorType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Error;
  }
};

/// BuiltinRawPointerType - The builtin raw (and dangling) pointer type.  This
/// pointer is completely unmanaged and is equivalent to i8* in LLVM IR.
class BuiltinRawPointerType : public TypeBase {
  friend class ASTContext;
  BuiltinRawPointerType(ASTContext &C)
    : TypeBase(TypeKind::BuiltinRawPointer, &C, /*Dependent=*/false) {}
public:
  void print(raw_ostream &OS) const;
  
  static bool classof(const BuiltinRawPointerType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinRawPointer;
  }
};


/// BuiltinObjectPointerType - The builtin opaque object-pointer type.
/// Useful for keeping an object alive when it is otherwise being
/// manipulated via an unsafe pointer type.
class BuiltinObjectPointerType : public TypeBase {
  friend class ASTContext;
  BuiltinObjectPointerType(ASTContext &C)
    : TypeBase(TypeKind::BuiltinObjectPointer, &C, /*Dependent=*/false) {}
public:
  void print(raw_ostream &OS) const;

  static bool classof(const BuiltinObjectPointerType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinObjectPointer;
  }
};
  
/// BuiltinIntegerType - The builtin integer types.  These directly correspond
/// to LLVM IR integer types.  They lack signedness and have an arbitrary
/// bitwidth.
class BuiltinIntegerType : public TypeBase {
  friend class ASTContext;
  unsigned BitWidth;
  BuiltinIntegerType(unsigned BitWidth, ASTContext &C)
    : TypeBase(TypeKind::BuiltinInteger, &C, /*Dependent=*/false), 
      BitWidth(BitWidth) {}
public:
  
  static BuiltinIntegerType *get(unsigned BitWidth, ASTContext &C);
  
  /// getBitWidth - Return the bitwidth of the integer.
  unsigned getBitWidth() const {
    return BitWidth;
  }

  void print(raw_ostream &OS) const;

  static bool classof(const BuiltinIntegerType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinInteger;
  }
};
  
class BuiltinFloatType : public TypeBase {
  friend class ASTContext;
public:
  enum FPKind {
    IEEE16, IEEE32, IEEE64, IEEE80, IEEE128, /// IEEE floating point types.
    PPC128   /// PowerPC "double double" type.
  };
private:
  FPKind Kind;
  
  BuiltinFloatType(FPKind Kind, ASTContext &C)
    : TypeBase(TypeKind::BuiltinFloat, &C, /*Dependent=*/false), 
      Kind(Kind) {}
public:
  
  /// getFPKind - Get the 
  FPKind getFPKind() const {
    return Kind;
  }
  
  const llvm::fltSemantics &getAPFloatSemantics() const;
  
  void print(raw_ostream &OS) const;

  static bool classof(const BuiltinFloatType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinFloat;
  }
};
  
/// UnstructuredDependentType - This is a expression type whose actual kind
/// is specified by context which hasn't been provided yet, and which
/// has no known structure. For example, a tuple element ".foo" will have
/// an unstructured dependent type. however, a tuple "(x, .foo)" would have
/// a dependent type that is not an UnstructuredDependentType, because it is
/// known to be a tuple type and have a first element of the type of x.
class UnstructuredDependentType : public TypeBase {
  friend class ASTContext;
  // The Dependent type is always canonical.
  UnstructuredDependentType(ASTContext &C) 
    : TypeBase(TypeKind::UnstructuredDependent, &C, /*Dependent=*/true) {}
public:
  static Type get(ASTContext &C);

  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnstructuredDependentType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::UnstructuredDependent;
  }
};

/// NameAliasType - An alias type is a name for another type, just like a
/// typedef in C.
class NameAliasType : public TypeBase {
  friend class TypeAliasDecl;
  // NameAliasType are never canonical.
  NameAliasType(TypeAliasDecl *d) 
    : TypeBase(TypeKind::NameAlias, nullptr, /*Dependent=*/false), 
      TheDecl(d) {}
  TypeAliasDecl *const TheDecl;

public:
  TypeAliasDecl *getDecl() const { return TheDecl; }
   
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const NameAliasType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::NameAlias;
  }
};

/// IdentifierType - A use of a type through a (possibly dotted) name, like
/// "foo" or "a.b.c".  These are never canonical and never uniqued, as they
/// carry location info for each identifier.
class IdentifierType : public TypeBase {
public:
  class Component {
  public:
    SourceLoc Loc;
    Identifier Id;
    
    /// Value is the decl or module that this refers to.  After name binding,
    /// the last entry in the component list is known to be a TypeBase*.
    llvm::PointerUnion3<ValueDecl*, Type, Module*> Value;
    Component(SourceLoc Loc, Identifier Id) : Loc(Loc), Id(Id) {}
  };
  
private:
  // IdentifierType are never canonical.
  IdentifierType(MutableArrayRef<Component> Components)
    : TypeBase(TypeKind::Identifier, nullptr, /*Dependent=*/false), 
      Components(Components) {}
public:
  
  /// The components that make this up.
  const MutableArrayRef<Component> Components;

  
  /// getNew - Return a new IdentifierType with the specified Component
  /// information.
  static IdentifierType *getNew(ASTContext &C,
                                MutableArrayRef<Component> Components);

  /// isMapped - Determine whether name binding has resolved the identifiers
  /// to an actual type.
  bool isMapped() const;
  
  /// getMappedType - After name binding is complete, this indicates what type
  /// this refers to (without removing any other sugar).
  Type getMappedType();
  
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const IdentifierType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Identifier;
  }
};

/// ParenType - A paren type is a type that's been written in parentheses.
class ParenType : public TypeBase {
  Type UnderlyingType;

  friend class ASTContext;
  ParenType(Type UnderlyingType)
    : TypeBase(TypeKind::Paren, nullptr, UnderlyingType->isDependentType()), 
      UnderlyingType(UnderlyingType) {}
public:
  Type getUnderlyingType() const { return UnderlyingType; }

  static ParenType *get(ASTContext &C, Type underlying);
   
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ParenType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Paren;
  }
};

/// TupleTypeElt - This represents a single element of a tuple.
class TupleTypeElt {
  /// Name - An optional name for the field.
  Identifier Name;

  /// Ty - This is the type of the field, which is mandatory.
  Type Ty;

  /// Init - This is a default value for the tuple element, used if an explicit
  /// value is not specified.
  Expr *Init;

public:
  TupleTypeElt() = default;
  TupleTypeElt(Type ty, Identifier name, Expr *init = nullptr)
    : Name(name), Ty(ty), Init(init) { }

  bool hasName() const { return !Name.empty(); }
  Identifier getName() const { return Name; }

  Type getType() const { return Ty; }

  /// \brief Retrieve a copy of this tuple type element with the type replaced.
  TupleTypeElt getWithType(Type T) const {
    TupleTypeElt Result;
    Result.Ty = T;
    return Result;
  }
  
  bool hasInit() const { return Init != nullptr; }
  Expr *getInit() const { return Init; }
  void setInit(Expr *E) { Init = E; }
};
  
/// TupleType - A tuple is a parenthesized list of types where each name has an
/// optional name.
///
class TupleType : public TypeBase, public llvm::FoldingSetNode {
  const ArrayRef<TupleTypeElt> Fields;
  
public:
  /// get - Return the uniqued tuple type with the specified elements.
  static TupleType *get(ArrayRef<TupleTypeElt> Fields, ASTContext &C);

  /// getEmpty - Return the empty tuple type '()'.
  static Type getEmpty(ASTContext &C);

  /// getFields - Return the fields of this tuple.
  ArrayRef<TupleTypeElt> getFields() const { return Fields; }
  
  /// getElementType - Return the type of the specified field.
  Type getElementType(unsigned FieldNo) const {
    return Fields[FieldNo].getType();
  }
  
  /// getNamedElementId - If this tuple has a field with the specified name,
  /// return the field index, otherwise return -1.
  int getNamedElementId(Identifier I) const;
  
  /// hasAnyDefaultValues - Return true if any of our elements has a default
  /// value.
  bool hasAnyDefaultValues() const;
  
  /// getFieldForScalarInit - If a tuple of this type can be initialized with a
  /// scalar, return the field number that the scalar is assigned to.  If not,
  /// return -1.
  int getFieldForScalarInit() const;
  
  
  /// updateInitializedElementType - This methods updates the element type and
  /// initializer for a non-canonical TupleType that has an initializer for the
  /// specified element.  This should only be used by TypeChecker.
  void updateInitializedElementType(unsigned EltNo, Type NewTy, Expr *NewInit);
  
  void print(raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Tuple;
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, Fields);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, 
                      ArrayRef<TupleTypeElt> Fields);
  
private:
  TupleType(ArrayRef<TupleTypeElt> fields, ASTContext *CanCtx);
};

/// OneOfType - This represents the type declared by a OneOfDecl.
class OneOfType : public TypeBase {  
  /// TheDecl - This is the TypeDecl which declares the given type. It
  /// specifies the name and other useful information about this type.
  OneOfDecl * const TheDecl;
  
public:
  /// getDecl() - Returns the decl which declares this type.
  OneOfDecl *getDecl() const { return TheDecl; }

  void print(raw_ostream &O) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const OneOfType *D) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::OneOf;
  }
  
private:
  OneOfType(OneOfDecl *TheDecl, ASTContext &Ctx);
  friend class OneOfDecl;
};

/// StructType - This represents the type declared by a StructDecl.
class StructType : public TypeBase {  
  /// TheDecl - This is the TypeDecl which declares the given type. It
  /// specifies the name and other useful information about this type.
  StructDecl * const TheDecl;
  
public:
  /// getDecl() - Returns the decl which declares this type.
  StructDecl *getDecl() const { return TheDecl; }

  void print(raw_ostream &O) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const StructType *D) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Struct;
  }
  
private:
  StructType(StructDecl *TheDecl, ASTContext &Ctx);
  friend class StructDecl;
};

/// ClassType - This represents the type declared by a ClassDecl.
class ClassType : public TypeBase {  
  /// TheDecl - This is the TypeDecl which declares the given type. It
  /// specifies the name and other useful information about this type.
  ClassDecl * const TheDecl;
  
public:
  /// getDecl() - Returns the decl which declares this type.
  ClassDecl *getDecl() const { return TheDecl; }

  void print(raw_ostream &O) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ClassType *D) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Class;
  }
  
private:
  ClassType(ClassDecl *TheDecl, ASTContext &Ctx);
  friend class ClassDecl;
};

/// MetaTypeType - This is the type given to a metatype value.  When a type is
/// declared, a 'metatype' value is injected into the value namespace to
/// resolve references to the type.  An example:
///
///  struct x { ... }  // declares type 'x' and metatype 'x'.
///  x.a()             // use of the metatype value since its a value context.
class MetaTypeType : public TypeBase {
  TypeDecl *const TheType;
  
public:
  /// get - Return the MetaTypeType for the specified type declaration.
  static MetaTypeType *get(TypeDecl *Type);

  TypeDecl *getTypeDecl() const { return TheType; }

  void print(raw_ostream &O) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const MetaTypeType *D) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::MetaType;
  }
  
private:
  MetaTypeType(TypeDecl *Type, ASTContext *Ctx);
  friend class TypeDecl;
};
  
/// ModuleType - This is the type given to a module value, e.g. the "Builtin" in
/// "Builtin.int".  This is typically given to a ModuleExpr, but can also exist
/// on ParenExpr, for example.
class ModuleType : public TypeBase {
  Module *const TheModule;
  
public:
  /// get - Return the ModuleType for the specified module.
  static ModuleType *get(Module *M);

  Module *getModule() const { return TheModule; }
  
  void print(raw_ostream &O) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ModuleType *D) { return true; }
  static bool classof(const TypeBase *T) {
    return  T->getKind() == TypeKind::Module;
  }
  
private:
  ModuleType(Module *M, ASTContext &Ctx)
    : TypeBase(TypeKind::Module, &Ctx, // Always canonical
               /*Dependent=*/false),
      TheModule(M) {
  }
};
  
/// FunctionType - A function type has a single input and result, but these
/// types may be a tuple, for example:
///   "(int) -> int" or "(a : int, b : int) -> (int, int)".
/// Note that the parser requires that the input to a function type be a Tuple
/// or ParenType, but ParenType desugars to its element, so the input to a
/// function may be an arbitrary type.
///
/// If the AutoClosure bit is set to true, then the input type is known to be ()
/// and a value of this function type is only assignable (in source code) from
/// the destination type of the function.  Sema inserts an ImplicitClosure to
/// close over the value.  For example:
///   var x : [auto_closure] () -> int = 4
///
class FunctionType : public TypeBase {
  const Type Input;
  const Type Result;

  bool AutoClosure;
public:
  /// 'Constructor' Factory Function
  static FunctionType *get(Type Input, Type Result, ASTContext &C) {
    return get(Input, Result, false, C);
  }
  static FunctionType *get(Type Input, Type Result, bool isAutoClosure,
                           ASTContext &C);

  Type getInput() const { return Input; }
  Type getResult() const { return Result; }
  bool isAutoClosure() const { return AutoClosure; }
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const FunctionType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Function;
  }
  
private:
  FunctionType(Type Input, Type Result, bool isAutoClosure);
};
  
  
/// ArrayType - An array type has a base type and either an unspecified or a
/// constant size.  For example "int[]" and "int[4]".  Array types cannot have
/// size = 0.
class ArrayType : public TypeBase {
  const Type Base;
  
  /// Size - When this is zero it indicates an unsized array like "int[]".
  uint64_t Size;
  
public:
  /// 'Constructor' Factory Function.
  static ArrayType *get(Type BaseType, uint64_t Size, ASTContext &C);

  Type getBaseType() const { return Base; }
  uint64_t getSize() const { return Size; }
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ArrayType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Array;
  }
  
private:
  ArrayType(Type Base, uint64_t Size);
};
  
/// ArraySliceType - An array slice type is the type T[], which is
/// always sugar for a library type.
class ArraySliceType : public TypeBase {
  // ArraySliceTypes are never canonical.
  ArraySliceType(SourceLoc loc, Type base)
    : TypeBase(TypeKind::ArraySlice, nullptr, /*Dependent=*/false),
      FirstRef(loc), Base(base) {}

  SourceLoc FirstRef;
  Type Base;
  Type Impl;

public:
  static ArraySliceType *get(Type baseTy, SourceLoc loc, ASTContext &C);

  // HACK
  SourceLoc getFirstRefLoc() const {
    return FirstRef;
  }

  bool hasImplementationType() const { return !Impl.isNull(); }
  void setImplementationType(Type ty) {
    assert(!hasImplementationType());
    Impl = ty;
  }
  Type getImplementationType() const {
    assert(hasImplementationType());
    return Impl;
  }

  Type getBaseType() const {
    return Base;
  }
   
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ArraySliceType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::ArraySlice;
  }
};

/// ProtocolType - A protocol type describes an abstract interface implemented
/// by another type.
class ProtocolType : public TypeBase {
  /// TheDecl - The protocol declaration, which stores most of the information
  /// about the protocol type.
  ProtocolDecl * const TheDecl;
  
public:
  /// getNew - Return a new instance of a protocol type.  These are never
  /// uniqued since each syntactic instance of them is semantically considered
  /// to be a different type.
  static ProtocolType *getNew(ProtocolDecl *TheDecl);
  
  ProtocolDecl *getDecl() const { return TheDecl; }
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ProtocolType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Protocol;
  }

private:
  ProtocolType(ProtocolDecl *TheDecl);
};

/// LValueType - An l-value is a handle to a physical object.  The
/// type of that object uniquely determines the type of an l-value
/// for it.
///
/// L-values are not fully first-class in Swift:
///
///  A type is said to "carry" an l-value if
///   - it is an l-value type or
///   - it is a tuple and at least one of its element types
///     carries an l-value.
///
/// The type of a function argument may carry an l-value.  This
/// is done by annotating the bound variable with the [byref]
/// attribute.
///
/// The type of a return value, local variable, or field may not
/// carry an l-value.
///
/// When inferring a value type from an expression whose type
/// carries an l-value, the carried l-value types are converted
/// to their object type.
class LValueType : public TypeBase {
public:
  class Qual {
  public:
    typedef unsigned opaque_type;

    enum QualBits : opaque_type {
      // The bits are chosen to make the subtype queries as efficient
      // as possible.  Basically, we want the subtypes to involve
      // fewer bits.

      /// A heap l-value is one which is located on, or at least
      /// capable of being located on, the heap.  Heap l-values are
      /// more than just references to logical objects; they also
      /// embed a reference-countable pointer (possibly null) which
      /// keeps all the necessary components of the l-value alive.  A
      /// heap l-value can be used as a non-heap one, but not
      /// vice-versa.
      NonHeap = 0x1,

      /// The default for a [byref] type.
      DefaultForType = NonHeap,

      /// The default for a variable reference.
      DefaultForVar = 0,
  
      /// The default for the base of a member access.
      DefaultForMemberAccess = NonHeap
    };

  private:
    opaque_type Bits;

  public:
    Qual() : Bits(0) {}
    explicit Qual(unsigned bits) : Bits(bits) {}
    Qual(QualBits qual) : Bits(qual) {}

    /// Return an opaque representation of this qualifier set.
    /// The result is hashable by DenseMap.
    opaque_type getOpaqueData() const { return Bits; }

    bool isHeap() const { return !(*this & NonHeap); }

    friend Qual operator|(QualBits l, QualBits r) {
      return Qual(opaque_type(l) | opaque_type(r));
    }

    /// Union two qualifier sets, given that they are compatible.
    friend Qual operator|(Qual l, Qual r) { return Qual(l.Bits | r.Bits); }

    /// Union a qualifier set into this qualifier set, given that
    /// they are compatible.
    Qual &operator|=(Qual r) { Bits |= r.Bits; return *this; }

    /// Intersect two qualifier sets, given that they are compatible.
    friend QualBits operator&(Qual l, Qual r) {
      // Use QualBits to allow a wider range of conversions to bool.
      return QualBits(l.Bits & r.Bits);
    }

    /// Intersect a qualifier set into this qualifier set.
    Qual &operator&=(Qual r) { Bits &= r.Bits; return *this; }

    /// Invert a qualifier set.  The state of the resulting
    /// non-boolean qualifiers is non-determined, except that they are
    /// is compatible with anything.
    friend Qual operator~(Qual qs) { return Qual(~qs.Bits); }
    friend Qual operator~(QualBits qs) { return Qual(~opaque_type(qs)); }

    /// Are these qualifier sets equivalent?
    friend bool operator==(Qual l, Qual r) { return l.Bits == r.Bits; }
    friend bool operator!=(Qual l, Qual r) { return l.Bits != r.Bits; }

    /// Is one qualifier set 'QL' "smaller than" another set 'QR'?
    /// This corresponds to the subtype relation on lvalue types
    /// for a fixed type T;  that is,
    ///   'QL <= QR' iff 'T [byref(QL)]' <= 'T [byref(QR)]'.
    /// Recall that this means that the first is implicitly convertible
    /// to the latter without "coercion", for some sense of that.
    ///
    /// This is not a total order.
    ///
    /// Right now, the subtyping rules are as follows:
    ///   An l-value type is a subtype of another l-value of the
    ///   same object type except:
    ///   - an implicit l-value is not a subtype of an explicit one.
    friend bool operator<=(Qual l, Qual r) {
      // Right now, all our qualifiers are boolean and independent,
      // and we've set it up so that 1 bits correspond to supertypes.
      // Therefore this is just the set-algebraic 'is subset of'
      // operation and can be performed by intersecting the sets and
      // testing for identity with the left.
      return (l & r) == l;
    }
    friend bool operator<(Qual l, Qual r) { return l != r && l <= r; }
    friend bool operator>(Qual l, Qual r) { return r < l; }
    friend bool operator>=(Qual l, Qual r) { return r <= l; }
  };

private:
  Type ObjectTy;
  Qual Quals; // TODO: put these bits in TypeBase

  LValueType(Type objectTy, Qual quals, ASTContext *canonicalContext)
    : TypeBase(TypeKind::LValue, canonicalContext, 
               objectTy->isDependentType()),
      ObjectTy(objectTy), Quals(quals) {}

public:
  static LValueType *get(Type type, Qual quals, ASTContext &C);

  Type getObjectType() const { return ObjectTy; }
  Qual getQualifiers() const { return Quals; }

  /// Is this a 'heap' l-value type?  Certain l-values are usable as
  /// heap l-values, i.e. they can be arbitrarily persisted.
  bool isHeap() const {
    return getQualifiers().isHeap();
  }

  /// For now, no l-values are ever materializable.  Maybe in the
  /// future we'll make heap l-values materializable.
  bool isMaterializable() const {
    return false;
  }

  void print(raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const LValueType *type) { return true; }
  static bool classof(const TypeBase *type) {
    return type->getKind() == TypeKind::LValue;
  }
};

inline bool TypeBase::isDependentType() const {
  return TypeBits.TypeBase.Dependent;
}

} // end namespace swift

#endif
