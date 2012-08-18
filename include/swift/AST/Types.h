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
#include "swift/AST/TypeLoc.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/Optional.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"

namespace llvm {
  struct fltSemantics;
}
namespace swift {
  class ArchetypeType;
  class ASTContext;
  class ClassDecl;
  class ExprHandle;
  class GenericParamList;
  class Identifier;
  class TypeAliasDecl;
  class TypeDecl;
  class NominalTypeDecl;
  class OneOfDecl;
  class OneOfElementDecl;
  class StructDecl;
  class ProtocolDecl;
  class ValueDecl;
  class Module;
  class ProtocolConformance;
  class Substitution;

  enum class TypeKind {
#define TYPE(id, parent) id,
#define TYPE_RANGE(Id, FirstId, LastId) \
  First_##Id##Type = FirstId, Last_##Id##Type = LastId,
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
    /// Unresolved - Whether this type is unresolved.
    unsigned Unresolved : 1;

    /// \brief What pass # has been applied to this type.
    unsigned ValidatedToPass : 2;
  };
  static const unsigned NumTypeBaseBits = 3;
  
  union {
    TypeBaseBits TypeBase;
  } TypeBits;
  
protected:
  TypeBase(TypeKind kind, ASTContext *CanTypeCtx, bool Unresolved)
    : CanonicalType((TypeBase*)nullptr), Kind(kind) {
    // If this type is canonical, switch the CanonicalType union to ASTContext.
    if (CanTypeCtx)
      CanonicalType = CanTypeCtx;
    
    setUnresolved(Unresolved);
    TypeBits.TypeBase.ValidatedToPass = 0;
  }

  /// \brief Mark this type as unresolved.
  void setUnresolved(bool D = true) { TypeBits.TypeBase.Unresolved = D; }
  
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
  
  /// isUnresolvedType() - Determines whether this type is an unresolved
  /// type, meaning that part of the type depends on the context in which
  /// the type occurs.
  bool isUnresolvedType() const;
  
  /// isExistentialType - Determines whether this type is an existential type,
  /// whose real (runtime) type is unknown but which is known to conform to
  /// some set of protocols. Protocol and protocol-conformance types are
  /// existential types.
  bool isExistentialType();

  /// isExistentialType - Determines whether this type is an existential type,
  /// whose real (runtime) type is unknown but which is known to conform to
  /// some set of protocols. Protocol and protocol-conformance types are
  /// existential types.
  ///
  /// \param Protocols If the type is an existential type, this vector is
  /// populated with the set of protocols
  bool isExistentialType(SmallVectorImpl<ProtocolDecl *> &Protocols);

  /// \brief Determine whether the given type is "specialized", meaning that
  /// it involves generic types for which generic arguments have been provided.
  /// For example, the types Vector<Int> and Vector<Int>.Element are both
  /// specialized, but the type Vector is not.
  bool isSpecialized();

  /// \brief The last type-checking pass that has been run to validate this
  /// type.
  unsigned getValidated() const { return TypeBits.TypeBase.ValidatedToPass; }

  /// \brief Mark this type as having been validated already to the given pass.
  void setValidated(unsigned Pass) { TypeBits.TypeBase.ValidatedToPass = Pass; }

  /// getUnlabeledType - Retrieve a version of this type with all labels
  /// removed at every level. For example, given a tuple type 
  /// \code
  /// (p : (x : int, y : int))
  /// \endcode
  /// the result would be the (parenthesized) type ((int, int)).
  Type getUnlabeledType(ASTContext &Context);

  /// getRValueType - For an lvalue type, retrieves the underlying object type.
  /// Otherwise, returns the type itself.
  Type getRValueType();

  void dump() const;
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *) { return true; }

private:
  // Make vanilla new/delete illegal for Types.
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
    : TypeBase(TypeKind::Error, &C, /*Unresolved=*/false) { }
public:
  static Type get(ASTContext &C);
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ErrorType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Error;
  }
};

/// BuiltinType - An abstract class for all the builtin types.
class BuiltinType : public TypeBase {
protected:
  BuiltinType(TypeKind kind, ASTContext &canTypeCtx)
    : TypeBase(kind, &canTypeCtx, /*unresolved*/ false) {}
public:
  static bool classof(const BuiltinType *T) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_BuiltinType &&
           T->getKind() <= TypeKind::Last_BuiltinType;
  }
};

/// BuiltinRawPointerType - The builtin raw (and dangling) pointer type.  This
/// pointer is completely unmanaged and is equivalent to i8* in LLVM IR.
class BuiltinRawPointerType : public BuiltinType {
  friend class ASTContext;
  BuiltinRawPointerType(ASTContext &C)
    : BuiltinType(TypeKind::BuiltinRawPointer, C) {}
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
class BuiltinObjectPointerType : public BuiltinType {
  friend class ASTContext;
  BuiltinObjectPointerType(ASTContext &C)
    : BuiltinType(TypeKind::BuiltinObjectPointer, C) {}
public:
  void print(raw_ostream &OS) const;

  static bool classof(const BuiltinObjectPointerType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinObjectPointer;
  }
};

/// BuiltinObjCPointerType - The builtin opaque Objective-C pointer type.
/// Useful for pushing an Objective-C type through swift.
class BuiltinObjCPointerType : public BuiltinType {
  friend class ASTContext;
  BuiltinObjCPointerType(ASTContext &C)
    : BuiltinType(TypeKind::BuiltinObjCPointer, C) {}
public:
  void print(raw_ostream &OS) const;

  static bool classof(const BuiltinObjCPointerType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinObjCPointer;
  }
};
  
/// BuiltinIntegerType - The builtin integer types.  These directly correspond
/// to LLVM IR integer types.  They lack signedness and have an arbitrary
/// bitwidth.
class BuiltinIntegerType : public BuiltinType {
  friend class ASTContext;
  unsigned BitWidth;
  BuiltinIntegerType(unsigned BitWidth, ASTContext &C)
    : BuiltinType(TypeKind::BuiltinInteger, C), BitWidth(BitWidth) {}
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
  
class BuiltinFloatType : public BuiltinType {
  friend class ASTContext;
public:
  enum FPKind {
    IEEE16, IEEE32, IEEE64, IEEE80, IEEE128, /// IEEE floating point types.
    PPC128   /// PowerPC "double double" type.
  };
private:
  FPKind Kind;
  
  BuiltinFloatType(FPKind Kind, ASTContext &C)
    : BuiltinType(TypeKind::BuiltinFloat, C), Kind(Kind) {}
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
  
/// UnstructuredUnresolvedType - This is a expression type whose actual kind
/// is specified by context which hasn't been provided yet, and which
/// has no known structure. For example, a tuple element ".foo" will have
/// an unstructured unresolved type. however, a tuple "(x, .foo)" would have
/// an unresolved type that is not an UnstructuredUnresolvedType, because it is
/// known to be a tuple type and have a first element of the type of x.
class UnstructuredUnresolvedType : public TypeBase {
  friend class ASTContext;
  // The Unresolved type is always canonical.
  UnstructuredUnresolvedType(ASTContext &C) 
    : TypeBase(TypeKind::UnstructuredUnresolved, &C, /*Unresolved=*/true) {}
public:
  static Type get(ASTContext &C);

  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnstructuredUnresolvedType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::UnstructuredUnresolved;
  }
};

/// NameAliasType - An alias type is a name for another type, just like a
/// typedef in C.
class NameAliasType : public TypeBase {
  friend class TypeAliasDecl;
  // NameAliasType are never canonical.
  NameAliasType(TypeAliasDecl *d) 
    : TypeBase(TypeKind::NameAlias, nullptr, /*Unresolved=*/false), 
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
    ArrayRef<TypeLoc> GenericArgs;
    
    /// Value is the decl or module that this refers to.  After name binding,
    /// the last entry in the component list is known to be a TypeBase*.
    llvm::PointerUnion3<ValueDecl*, Type, Module*> Value;
    Component(SourceLoc Loc, Identifier Id,
              ArrayRef<TypeLoc> GenericArgs) :
        Loc(Loc), Id(Id), GenericArgs(GenericArgs) {}
  };
  
private:
  // IdentifierType are never canonical.
  IdentifierType(MutableArrayRef<Component> Components)
    : TypeBase(TypeKind::Identifier, nullptr, /*Unresolved=*/false), 
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
    : TypeBase(TypeKind::Paren, nullptr, UnderlyingType->isUnresolvedType()), 
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
  ExprHandle *Init;

  /// VarargBaseTy - This is the base type of the field, ignoring the "..."
  /// specifier, if one is present.
  Type VarargBaseTy;

public:
  TupleTypeElt() = default;
  TupleTypeElt(Type ty, Identifier name, ExprHandle *init = nullptr,
               Type VarargBaseTy = Type())
    : Name(name), Ty(ty), Init(init), VarargBaseTy(VarargBaseTy) { }

  bool hasName() const { return !Name.empty(); }
  Identifier getName() const { return Name; }

  Type getType() const { return Ty; }
  bool isVararg() const { return !VarargBaseTy.isNull(); }
  Type getVarargBaseTy() const { return VarargBaseTy; }

  /// \brief Retrieve a copy of this tuple type element with the type replaced.
  TupleTypeElt getWithType(Type T) const {
    TupleTypeElt Result(*this);
    Result.Ty = T;
    return Result;
  }
  
  bool hasInit() const { return Init != nullptr; }
  ExprHandle *getInit() const { return Init; }
  void setInit(ExprHandle *E) { Init = E; }
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
  void updateInitializedElementType(unsigned EltNo, Type NewTy);
  
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

/// UnboundGenericType - Represents a generic nominal type where the
/// type arguments have not yet been resolved.
class UnboundGenericType : public TypeBase, public llvm::FoldingSetNode {
  NominalTypeDecl *TheDecl;

  /// \brief The type of the parent, in which this type is nested.
  Type Parent;

private:
  UnboundGenericType(NominalTypeDecl *TheDecl, Type Parent, ASTContext &C)
    : TypeBase(TypeKind::UnboundGeneric,
               (!Parent || Parent->isCanonical())? &C : nullptr,
               /*Unresolved=*/false),
      TheDecl(TheDecl), Parent(Parent) { }

public:
  static UnboundGenericType* get(NominalTypeDecl *TheDecl, Type Parent,
                                 ASTContext &C);

  /// \brief Returns the declaration that declares this type.
  NominalTypeDecl *getDecl() const { return TheDecl; }

  /// \brief Returns the type of the parent of this type. This will
  /// be null for top-level types or local types, and for non-generic types
  /// will simply be the same as the declared type of the declaration context
  /// of TheDecl. For types nested within generic types, however, this will
  /// involve \c BoundGenericType nodes that provide context for the nested
  /// type, e.g., the bound type Dictionary<String, Int>.Inner would be
  /// represented as an UnboundGenericType with Dictionary<String, Int> as its
  /// parent type.
  Type getParent() const { return Parent; }

  void print(raw_ostream &O) const;

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, NominalTypeDecl *D,
                      Type Parent);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnboundGenericType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::UnboundGeneric;
  }
};

/// BoundGenericType - Represents a generic nominal type bound to the
/// given type arguments.
class BoundGenericType : public TypeBase, public llvm::FoldingSetNode {
  NominalTypeDecl *TheDecl;

  /// \brief The type of the parent, in which this type is nested.
  Type Parent;
  
  ArrayRef<Type> GenericArgs;
  

private:
  BoundGenericType(NominalTypeDecl *TheDecl, Type Parent,
                   ArrayRef<Type> GenericArgs, ASTContext *C);

public:
  static BoundGenericType* get(NominalTypeDecl *TheDecl, Type Parent,
                               ArrayRef<Type> GenericArgs);

  /// \brief Returns the declaration that declares this type.
  NominalTypeDecl *getDecl() const { return TheDecl; }

  /// \brief Returns the type of the parent of this type. This will
  /// be null for top-level types or local types, and for non-generic types
  /// will simply be the same as the declared type of the declaration context
  /// of TheDecl. For types nested within generic types, however, this will
  /// involve \c BoundGenericType nodes that provide context for the nested
  /// type, e.g., the bound type Dictionary<String, Int>.Inner<Int> would be
  /// represented as a BoundGenericType with Dictionary<String, Int> as its
  /// parent type.
  Type getParent() const { return Parent; }

  ArrayRef<Type> getGenericArgs() const { return GenericArgs; }

  /// \brief Determine whether this bound generic type has substitution
  /// information that provides protocol conformances.
  bool hasSubstitutions();

  /// \brief Retrieve the set of substitutions used to produce this bound
  /// generic type from the underlying generic type.
  ArrayRef<Substitution> getSubstitutions();

  /// \brief Set the substitution information for this bound generic type.
  ///
  /// \param Subs The set of substitutions, which must point into
  /// ASTContext-allocated memory.
  void setSubstitutions(ArrayRef<Substitution> Subs);

  void print(raw_ostream &O) const;

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, TheDecl, Parent, GenericArgs);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, NominalTypeDecl *TheDecl,
                      Type Parent, ArrayRef<Type> GenericArgs);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BoundGenericType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BoundGeneric;
  }
};

/// NominalType - Represents a type with a name that is significant, such that
/// the name distinguishes it from other structurally-similar types that have
/// different names. Nominal types are always canonical.
class NominalType : public TypeBase {
  /// TheDecl - This is the TypeDecl which declares the given type. It
  /// specifies the name and other useful information about this type.
  NominalTypeDecl * const TheDecl;

  /// \brief The type of the parent, in which this type is nested.
  Type Parent;

protected:
  NominalType(TypeKind K, ASTContext *C, NominalTypeDecl *TheDecl,
              Type Parent)
    : TypeBase(K, (!Parent || Parent->isCanonical())? C : nullptr,
               /*Unresolved=*/false), TheDecl(TheDecl), Parent(Parent) { }

public:
  static NominalType *get(NominalTypeDecl *D, Type Parent, ASTContext &C);

  /// \brief Returns the declaration that declares this type.
  NominalTypeDecl *getDecl() const { return TheDecl; }

  /// \brief Returns the type of the parent of this type. This will
  /// be null for top-level types or local types, and for non-generic types
  /// will simply be the same as the declared type of the declaration context
  /// of TheDecl. For types nested within generic types, however, this will
  /// involve \c BoundGenericType nodes that provide context for the nested
  /// type, e.g., the type Dictionary<String, Int>.ItemRange would be
  /// represented as a NominalType with Dictionary<String, Int> as its parent
  /// type.
  Type getParent() const { return Parent; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const NominalType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_NominalType &&
           T->getKind() <= TypeKind::Last_NominalType;
  }
};

/// OneOfType - This represents the type declared by a OneOfDecl.
class OneOfType : public NominalType, public llvm::FoldingSetNode {
public:
  /// getDecl() - Returns the decl which declares this type.
  OneOfDecl *getDecl() const {
    return reinterpret_cast<OneOfDecl *>(NominalType::getDecl());
  }

  /// \brief Retrieve the type when we're referencing the given oneof
  /// declaration in the parent type \c Parent.
  static OneOfType *get(OneOfDecl *D, Type Parent, ASTContext &C);

  void print(raw_ostream &O) const;

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, OneOfDecl *D, Type Parent);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const OneOfType *D) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::OneOf;
  }

private:
  OneOfType(OneOfDecl *TheDecl, Type Parent, ASTContext &Ctx);
};

/// StructType - This represents the type declared by a StructDecl.
class StructType : public NominalType, public llvm::FoldingSetNode {  
public:
  /// getDecl() - Returns the decl which declares this type.
  StructDecl *getDecl() const {
    return reinterpret_cast<StructDecl *>(NominalType::getDecl());
  }

  /// \brief Retrieve the type when we're referencing the given struct
  /// declaration in the parent type \c Parent.
  static StructType *get(StructDecl *D, Type Parent, ASTContext &C);

  void print(raw_ostream &O) const;

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, StructDecl *D, Type Parent);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const StructType *D) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Struct;
  }
  
private:
  StructType(StructDecl *TheDecl, Type Parent, ASTContext &Ctx);
};

/// ClassType - This represents the type declared by a ClassDecl.
class ClassType : public NominalType, public llvm::FoldingSetNode {  
public:
  /// getDecl() - Returns the decl which declares this type.
  ClassDecl *getDecl() const {
    return reinterpret_cast<ClassDecl *>(NominalType::getDecl());
  }

  /// \brief Retrieve the type when we're referencing the given class
  /// declaration in the parent type \c Parent.
  static ClassType *get(ClassDecl *D, Type Parent, ASTContext &C);

  void print(raw_ostream &O) const;

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, ClassDecl *D, Type Parent);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const ClassType *D) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Class;
  }
  
private:
  ClassType(ClassDecl *TheDecl, Type Parent, ASTContext &Ctx);
};

/// MetaTypeType - This is the type given to a metatype value.  When a type is
/// declared, a 'metatype' value is injected into the value namespace to
/// resolve references to the type.  An example:
///
///  struct x { ... }  // declares type 'x' and metatype 'x'.
///  x.a()             // use of the metatype value since its a value context.
class MetaTypeType : public TypeBase {
  Type InstanceType;
  
public:
  /// get - Return the MetaTypeType for the specified type declaration.
  static MetaTypeType *get(Type T, ASTContext &C);

  Type getInstanceType() const { return InstanceType; }

  void print(raw_ostream &O) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const MetaTypeType *D) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::MetaType;
  }
  
private:
  MetaTypeType(Type T, ASTContext *Ctx);
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
               /*Unresolved=*/false),
      TheModule(M) {
  }
};
  
/// AnyFunctionType - A function type has a single input and result, but
/// these types may be tuples, for example:
///   "(int) -> int" or "(a : int, b : int) -> (int, int)".
/// Note that the parser requires that the input to a function type be a Tuple
/// or ParenType, but ParenType desugars to its element, so the input to a
/// function may be an arbitrary type.
///
/// There are two kinds of function types:  monomorphic (FunctionType) and
/// polymorphic (PolymorphicFunctionType).
class AnyFunctionType : public TypeBase {
  const Type Input;
  const Type Output;
protected:
  AnyFunctionType(TypeKind Kind, ASTContext *CanTypeContext,
                  Type Input, Type Output, bool isUnresolved)
    : TypeBase(Kind, CanTypeContext,
               Input->isUnresolvedType() || Output->isUnresolvedType()),
      Input(Input), Output(Output) {
  }

public:
  Type getInput() const { return Input; }
  Type getResult() const { return Output; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const AnyFunctionType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_AnyFunctionType &&
           T->getKind() <= TypeKind::Last_AnyFunctionType;
  }
};

/// FunctionType - A monomorphic function type.
///
/// If the AutoClosure bit is set to true, then the input type is known to be ()
/// and a value of this function type is only assignable (in source code) from
/// the destination type of the function.  Sema inserts an ImplicitClosure to
/// close over the value.  For example:
///   var x : [auto_closure] () -> int = 4
class FunctionType : public AnyFunctionType {
  bool AutoClosure;
public:
  /// 'Constructor' Factory Function
  static FunctionType *get(Type Input, Type Result, ASTContext &C) {
    return get(Input, Result, false, C);
  }
  static FunctionType *get(Type Input, Type Result, bool isAutoClosure,
                           ASTContext &C);

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

/// PolymorphicFunctionType - A polymorphic function type.
///
/// If the AutoClosure bit is set to true, then the input type is known to be ()
/// and a value of this function type is only assignable (in source code) from
/// the destination type of the function.  Sema inserts an ImplicitClosure to
/// close over the value.  For example:
///   var x : [auto_closure] () -> int = 4
class PolymorphicFunctionType : public AnyFunctionType {
  // TODO: storing a GenericParamList* here is really the wrong solution;
  // we should be able to store something readily canonicalizable.
  GenericParamList *Params;
public:
  /// 'Constructor' Factory Function
  static PolymorphicFunctionType *get(Type input, Type output,
                                      GenericParamList *params,
                                      ASTContext &C);

  GenericParamList &getGenericParams() const { return *Params; }

  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const PolymorphicFunctionType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::PolymorphicFunction;
  }
  
private:
  PolymorphicFunctionType(Type input, Type output, GenericParamList *params,
                          ASTContext &C);
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
  ArraySliceType(Type base)
    : TypeBase(TypeKind::ArraySlice, nullptr, /*Unresolved=*/false),
      Base(base) {}

  Type Base;
  Type Impl;

public:
  static ArraySliceType *get(Type baseTy, ASTContext &C);

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
class ProtocolType : public NominalType {
public:
  ProtocolDecl *getDecl() const {
    return reinterpret_cast<ProtocolDecl *>(NominalType::getDecl());
  }
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ProtocolType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Protocol;
  }

private:
  friend class ProtocolDecl;
  ProtocolType(ProtocolDecl *TheDecl, ASTContext &Ctx);
};

/// ProtocolCompositionType - A type that composes some number of protocols
/// together to represent types that conform to all of the named protocols.
///
/// \code
/// protocol P { /* ... */ }
/// protocol Q { /* ... */ }
/// var x : protocol<P, Q>
/// \endcode
///
/// Here, the type of x is a a composition of the protocols 'P' and 'Q'.
///
/// The canonical form of a protocol composition type is based on a sorted (by
/// module and name), minimized (based on redundancy due to protocol
/// inheritance) protocol list. If the sorted, minimized list is a single
/// protocol, then the canonical type is that protocol type. Otherwise, it is
/// a composition of the protocols in that list.
class ProtocolCompositionType : public TypeBase, public llvm::FoldingSetNode {
  ArrayRef<Type> Protocols;
  
public:
  /// \brief Retrieve an instance of a protocol composition type with the
  /// given set of protocols.
  static Type get(ASTContext &C, ArrayRef<Type> Protocols);
  
  /// \brief Retrieve the set of protocols composed to create this type.
  ArrayRef<Type> getProtocols() const { return Protocols; }
  
  void print(raw_ostream &OS) const;
  
  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, Protocols);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, ArrayRef<Type> Protocols);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const ProtocolCompositionType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::ProtocolComposition;
  }
  
private:
  static ProtocolCompositionType *build(ASTContext &C,
                                        ArrayRef<Type> Protocols);

  ProtocolCompositionType(ASTContext *Ctx, ArrayRef<Type> Protocols)
    : TypeBase(TypeKind::ProtocolComposition, /*Context=*/Ctx,
               /*Unresolved=*/false),
      Protocols(Protocols) { }
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
               objectTy->isUnresolvedType()),
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

/// SubstitutableType - A reference to a type that can be substituted, i.e.,
/// an archetype or a generic parameter.
class SubstitutableType : public TypeBase {
  ArrayRef<ProtocolDecl *> ConformsTo;
  
protected:
  SubstitutableType(TypeKind K, ASTContext *C, bool Unresolved,
                    ArrayRef<ProtocolDecl *> ConformsTo)
    : TypeBase(K, C, Unresolved), ConformsTo(ConformsTo) { }

public:
  /// \brief Retrieve the name of this type.
  Identifier getName() const;

  /// \brief Retrieve the parent of this type, or null if this is a
  /// primary type.
  SubstitutableType *getParent() const;

  /// \brief Retrieve the archetype corresponding to this substitutable type.
  ArchetypeType *getArchetype();

  // FIXME: Temporary hack.
  bool isPrimary() const;
  unsigned getPrimaryIndex() const;

  /// getConformsTo - Retrieve the set of protocols to which this substitutable
  /// type shall conform.
  ArrayRef<ProtocolDecl *> getConformsTo() const { return ConformsTo; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const SubstitutableType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_SubstitutableType &&
           T->getKind() <= TypeKind::Last_SubstitutableType;
  }
};

/// ArchetypeType - An archetype is a type that is a stand-in used to describe
/// type parameters and associated types in generic definition and protocols.
/// Archetypes will be replaced with actual, concrete types at some later
/// point in time, whether it be at compile time due to a direct binding or
/// at run time due to the use of generic types.
class ArchetypeType : public SubstitutableType {
  ArchetypeType *Parent;
  Identifier Name;
  unsigned IndexIfPrimary;
  ArrayRef<std::pair<Identifier, ArchetypeType *>> NestedTypes;
  
public:
  /// getNew - Create a new archetype with the given name.
  ///
  /// The ConformsTo array will be copied into the ASTContext by this routine.
  static ArchetypeType *getNew(ASTContext &Ctx, ArchetypeType *Parent,
                               Identifier Name, ArrayRef<Type> ConformsTo,
                               Optional<unsigned> Index = Optional<unsigned>());

  /// getNew - Create a new archetype with the given name.
  ///
  /// The ConformsTo array will be minimized then copied into the ASTContext
  /// by this routine.
  static ArchetypeType *getNew(ASTContext &Ctx, ArchetypeType *Parent,
                          Identifier Name,
                          llvm::SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                          Optional<unsigned> Index = Optional<unsigned>());

  void print(raw_ostream &OS) const;

  /// \brief Retrieve the name of this archetype.
  Identifier getName() const { return Name; }

  /// \brief Retrieve the fully-dotted name that should be used to display this
  /// archetype.
  std::string getFullName() const;

  /// \brief Retrieve the parent of this archetype, or null if this is a
  /// primary archetype.
  ArchetypeType *getParent() const { return Parent; }

  /// \brief Retrieve the nested type with the given name.
  ArchetypeType *getNestedType(Identifier Name) const;

  /// \brief Retrieve the nested types of this archetype.
  ArrayRef<std::pair<Identifier, ArchetypeType *>> getNestedTypes() const {
    return NestedTypes;
  }

  /// \brief Set the nested types to a copy of the given array of
  /// archetypes, which will first be sorted in place.
  void setNestedTypes(ASTContext &Ctx,
         MutableArrayRef<std::pair<Identifier, ArchetypeType *>> Nested);

  /// isPrimary - Determine whether this is the archetype for a 'primary'
  /// archetype, e.g., 
  bool isPrimary() const { return IndexIfPrimary > 0; }

  // getPrimaryIndex - For a primary archetype, return the zero-based index.
  unsigned getPrimaryIndex() const {
    assert(isPrimary() && "Non-primary archetype does not have index");
    return IndexIfPrimary - 1;
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const ArchetypeType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Archetype;
  }
  
private:
  ArchetypeType(ASTContext &Ctx, ArchetypeType *Parent,
                Identifier Name, ArrayRef<ProtocolDecl *> ConformsTo,
                Optional<unsigned> Index)
    : SubstitutableType(TypeKind::Archetype, &Ctx, /*Unresolved=*/false,
                        ConformsTo),
      Parent(Parent), Name(Name), IndexIfPrimary(Index? *Index + 1 : 0) { }
};

/// DeducibleGenericParamType - A type that refers to a generic type parameter
/// that can be deduced, specified, and substituted.
///
/// In the given generic function:
///
/// \code
/// func identity<T>(x : T) { return x }
/// \endcode
///
/// The type of 'identity', for the purpose of substitution, contains a
/// \c DeducibleGenericParamType in the input type of the function.
class DeducibleGenericParamType : public SubstitutableType {
  DeducibleGenericParamType *Parent;
  ArchetypeType *Archetype;

  DeducibleGenericParamType(ASTContext &Ctx,
                            DeducibleGenericParamType *Parent,
                            ArchetypeType *Archetype)
    : SubstitutableType(TypeKind::DeducibleGenericParam, &Ctx,
                        /*Unresolved=*/true, Archetype->getConformsTo()),
      Parent(Parent), Archetype(Archetype)
  {
  }

public:
  static DeducibleGenericParamType *getNew(ASTContext &Ctx,
                                           DeducibleGenericParamType *Parent,
                                           ArchetypeType *Archetype);

  /// \brief Retrieve the archetype that generic parameter will substitute.
  ArchetypeType *getArchetype() const { return Archetype; }

  /// \brief Retrieve the parent of this generic parameter, if this generic
  /// parameter describes a nested type.
  DeducibleGenericParamType *getParent() const { return Parent; }

  /// getName - Retrieve the name of this deducible generic parameter.
  Identifier getName() const { return Archetype->getName(); }

  /// getIndex - Retrieve the index into the list of generic parameters in
  /// which this generic parameter occurs.
  unsigned getIndex() const { return Archetype->getPrimaryIndex(); }

  void print(raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const DeducibleGenericParamType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::DeducibleGenericParam;
  }
};

/// SubstitutedType - A type that has been substituted for some other type,
/// which implies that the replacement type meets all of the requirements of
/// the original type.
class SubstitutedType : public TypeBase {
  // SubstitutedTypes are never canonical.
  explicit SubstitutedType(Type Original, Type Replacement)
    : TypeBase(TypeKind::Substituted, nullptr, Replacement->isUnresolvedType()),
      Original(Original), Replacement(Replacement) {}
  
  Type Original;
  Type Replacement;
  
public:
  static SubstitutedType *get(Type Original, Type Replacement, ASTContext &C);
  
  /// \brief Retrieve the original type that is being replaced.
  Type getOriginal() const { return Replacement; }
  
  /// \brief Retrieve the replacement type.
  Type getReplacementType() const { return Replacement; }
  
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const SubstitutedType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Substituted;
  }
};

/// \brief A type variable used during type checking.
class TypeVariableType : public TypeBase {
  TypeVariableType(ASTContext &C)
    : TypeBase(TypeKind::TypeVariable, &C, true) { }

  class Implementation;
  
public:
  /// \brief Create a new type variable whose implementation is constructed
  /// with the given arguments.
  template<typename ...Args>
  static TypeVariableType *getNew(ASTContext &C, Args &&...args);

  /// \brief Retrieve the implementation data corresponding to this type
  /// variable.
  ///
  /// The contents of the implementation data for this type are hidden in the
  /// details of the constraint solver used for type checking.
  Implementation &getImpl() {
    return *reinterpret_cast<Implementation *>(this + 1);
  }

  /// \brief Retrieve the implementation data corresponding to this type
  /// variable.
  ///
  /// The contents of the implementation data for this type are hidden in the
  /// details of the constraint solver used for type checking.
  const Implementation &getImpl() const {
    return *reinterpret_cast<const Implementation *>(this + 1);
  }

  /// \brief Access the implementation object for this type variable.
  Implementation *operator->() {
    return reinterpret_cast<Implementation *>(this + 1);
  }

  void print(raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeVariableType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::TypeVariable;
  }
};

inline bool TypeBase::isUnresolvedType() const {
  return TypeBits.TypeBase.Unresolved;
}

inline bool TypeBase::isExistentialType() {
  CanType T = getCanonicalType();
  return T->getKind() == TypeKind::Protocol
         || T->getKind() == TypeKind::ProtocolComposition;
}

inline Type TypeBase::getRValueType() {
  if (!is<LValueType>())
    return this;

  return castTo<LValueType>()->getObjectType();
}

inline Identifier SubstitutableType::getName() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->getName();

  return cast<DeducibleGenericParamType>(this)->getName();
}

inline SubstitutableType *SubstitutableType::getParent() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->getParent();

  return cast<DeducibleGenericParamType>(this)->getParent();
}

inline ArchetypeType *SubstitutableType::getArchetype() {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype;

  return cast<DeducibleGenericParamType>(this)->getArchetype();
}

inline bool SubstitutableType::isPrimary() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->isPrimary();

  return cast<DeducibleGenericParamType>(this)->getParent() == nullptr;
}

inline unsigned SubstitutableType::getPrimaryIndex() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->getPrimaryIndex();
  return cast<DeducibleGenericParamType>(this)->getIndex();
}

} // end namespace swift

#endif
