//===--- Decl.h - Swift Language Declaration ASTs ---------------*- C++ -*-===//
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
// This file defines the Decl class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECL_H
#define SWIFT_DECL_H

#include "swift/AST/AccessScope.h"
#include "swift/AST/CaptureInfo.h"
#include "swift/AST/ClangNode.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/DefaultArgumentKind.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericParamKey.h"
#include "swift/AST/LayoutConstraint.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/AST/Witness.h"
#include "swift/Basic/Compiler.h"
#include "swift/Basic/OptionalEnum.h"
#include "swift/Basic/Range.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {
  enum class AccessSemantics : unsigned char;
  class ApplyExpr;
  class GenericEnvironment;
  class ArchetypeType;
  class ASTContext;
  class ASTPrinter;
  class ASTWalker;
  class ConstructorDecl;
  class DestructorDecl;
  class DiagnosticEngine;
  class DynamicSelfType;
  class Type;
  class Expr;
  class DeclRefExpr;
  class ForeignErrorConvention;
  class LiteralExpr;
  class FuncDecl;
  class BraceStmt;
  class DeclAttributes;
  class GenericSignature;
  class GenericTypeParamDecl;
  class GenericTypeParamType;
  class ModuleDecl;
  class NameAliasType;
  class EnumCaseDecl;
  class EnumElementDecl;
  class ParameterList;
  class Pattern;
  struct PrintOptions;
  class ProtocolDecl;
  class ProtocolType;
  struct RawComment;
  enum class ResilienceExpansion : unsigned;
  class TypeAliasDecl;
  class Stmt;
  class SubscriptDecl;
  class UnboundGenericType;
  class ValueDecl;
  class VarDecl;

enum class DeclKind : uint8_t {
#define DECL(Id, Parent) Id,
#define DECL_RANGE(Id, FirstId, LastId) \
  First_##Id##Decl = FirstId, Last_##Id##Decl = LastId,
#include "swift/AST/DeclNodes.def"
};

/// Fine-grained declaration kind that provides a description of the
/// kind of entity a declaration represents, as it would be used in
/// diagnostics.
///
/// For example, \c FuncDecl is a single declaration class, but it has
/// several descriptive entries depending on whether it is an
/// operator, global function, local function, method, (observing)
/// accessor, etc.
enum class DescriptiveDeclKind : uint8_t {
  Import,
  Extension,
  EnumCase,
  TopLevelCode,
  IfConfig,
  PatternBinding,
  Var,
  Param,
  Let,
  StaticVar,
  StaticLet,
  ClassVar,
  ClassLet,
  InfixOperator,
  PrefixOperator,
  PostfixOperator,
  PrecedenceGroup,
  TypeAlias,
  GenericTypeParam,
  AssociatedType,  
  Enum,
  Struct,
  Class,
  Protocol,
  GenericEnum,
  GenericStruct,
  GenericClass,
  Subscript,
  Constructor,
  Destructor,
  LocalFunction,
  GlobalFunction,
  OperatorFunction,
  Method,
  StaticMethod,
  ClassMethod,
  Getter,
  Setter,
  MaterializeForSet,
  Addressor,
  MutableAddressor,
  WillSet,
  DidSet,
  EnumElement,
  Module,
};

/// Keeps track of stage of circularity checking for the given protocol.
enum class CircularityCheck {
  /// Circularity has not yet been checked.
  Unchecked,
  /// We're currently checking circularity.
  Checking,
  /// Circularity has already been checked.
  Checked
};

/// Keeps track of whether a given class inherits initializers from its
/// superclass.
enum class StoredInheritsSuperclassInits {
  /// We have not yet checked.
  Unchecked,
  /// Superclass initializers are not inherited.
  NotInherited,
  /// Convenience initializers in the superclass are inherited.
  Inherited
};

/// Describes which spelling was used in the source for the 'static' or 'class'
/// keyword.
enum class StaticSpellingKind : uint8_t {
  None,
  KeywordStatic,
  KeywordClass,
};
  
/// Describes if an enum element constructor directly or indirectly references
/// its enclosing type.
enum class ElementRecursiveness {
  /// The element does not reference its enclosing type.
  NotRecursive,
  /// The element is currently being validated, and may references its enclosing
  /// type.
  PotentiallyRecursive,
  /// The element does not reference its enclosing type.
  Recursive
};

/// Diagnostic printing of \c StaticSpellingKind.
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, StaticSpellingKind SSK);

/// Encapsulation of the overload signature of a given declaration,
/// which is used to determine uniqueness of a declaration within a
/// given context.
///
/// Two definitions in the same context may not have the same overload
/// signature.
struct OverloadSignature {
  /// The full name of the declaration.
  DeclName Name;

  /// The interface type of the declaration, when relevant to the
  /// overload signature.
  CanType InterfaceType;

  /// The kind of unary operator.
  UnaryOperatorKind UnaryOperator = UnaryOperatorKind::None;

  /// Whether this is an instance member.
  bool IsInstanceMember = false;

  /// Whether this is a property.
  bool IsProperty = false;

  /// Whether this signature is part of a protocol extension.
  bool InProtocolExtension = false;
};

/// Determine whether two overload signatures conflict.
bool conflicting(const OverloadSignature& sig1, const OverloadSignature& sig2);

/// Decl - Base class for all declarations in Swift.
class alignas(1 << DeclAlignInBits) Decl {
  class DeclBitfields {
    friend class Decl;
    unsigned Kind : 6;

    /// \brief Whether this declaration is invalid.
    unsigned Invalid : 1;

    /// \brief Whether this declaration was implicitly created, e.g.,
    /// an implicit constructor in a struct.
    unsigned Implicit : 1;

    /// \brief Whether this declaration was mapped directly from a Clang AST.
    ///
    /// Use getClangNode() to retrieve the corresponding Clang AST.
    unsigned FromClang : 1;

    /// \brief Whether we've already performed early attribute validation.
    /// FIXME: This is ugly.
    unsigned EarlyAttrValidation : 1;

    /// \brief Whether this declaration is currently being validated.
    unsigned BeingValidated : 1;

    /// \brief Whether this declaration was added to the surrounding
    /// DeclContext of an active #if config clause.
    unsigned EscapedFromIfConfig : 1;
  };
  enum { NumDeclBits = 12 };
  static_assert(NumDeclBits <= 32, "fits in an unsigned");

  class PatternBindingDeclBitfields {
    friend class PatternBindingDecl;
    unsigned : NumDeclBits;
    
    /// \brief Whether this pattern binding declares static variables.
    unsigned IsStatic : 1;

    /// \brief Whether 'static' or 'class' was used.
    unsigned StaticSpelling : 2;

    /// \brief The number of pattern binding declarations.
    unsigned NumPatternEntries : 16;
  };
  enum { NumPatternBindingDeclBits = NumDeclBits + 19 };
  static_assert(NumPatternBindingDeclBits <= 32, "fits in an unsigned");
  
  class ValueDeclBitfields {
    friend class ValueDecl;
    friend class MemberLookupTable;
    unsigned : NumDeclBits;
    unsigned AlreadyInLookupTable : 1;

    /// Whether we have already checked whether this declaration is a 
    /// redeclaration.
    unsigned CheckedRedeclaration : 1;
  };
  enum { NumValueDeclBits = NumDeclBits + 2 };
  static_assert(NumValueDeclBits <= 32, "fits in an unsigned");

  class AbstractStorageDeclBitfields {
    friend class AbstractStorageDecl;
    unsigned : NumValueDeclBits;

    /// Whether we are overridden later
    unsigned Overridden : 1;

    /// The storage kind.
    unsigned StorageKind : 4;
  };
  enum { NumAbstractStorageDeclBits = NumValueDeclBits + 5 };
  static_assert(NumAbstractStorageDeclBits <= 32, "fits in an unsigned");

  class VarDeclBitfields {
    friend class VarDecl;
    unsigned : NumAbstractStorageDeclBits;

    /// \brief Whether this property is a type property (currently unfortunately
    /// called 'static').
    unsigned IsStatic : 1;

    /// \brief Whether this is a 'let' property, which can only be initialized
    /// once (either in its declaration, or once later), making it immutable.
    unsigned IsLet : 1;

    /// \brief Whether this declaration was an element of a capture list.
    unsigned IsCaptureList : 1;

    /// \brief Whether this vardecl has an initial value bound to it in a way
    /// that isn't represented in the AST with an initializer in the pattern
    /// binding.  This happens in cases like "for i in ...", switch cases, etc.
    unsigned HasNonPatternBindingInit : 1;

    /// \brief Whether this is a property used in expressions in the debugger.
    /// It is up to the debugger to instruct SIL how to access this variable.
    unsigned IsDebuggerVar : 1;

    /// Whether the decl can be accessed by swift users; for instance,
    /// a.storage for lazy var a is a decl that cannot be accessed.
    unsigned IsUserAccessible : 1;
  };
  enum { NumVarDeclBits = NumAbstractStorageDeclBits + 6 };
  static_assert(NumVarDeclBits <= 32, "fits in an unsigned");

  class EnumElementDeclBitfields {
    friend class EnumElementDecl;
    unsigned : NumValueDeclBits;
    
    /// \brief Whether or not this element directly or indirectly references
    /// the enum type.
    unsigned Recursiveness : 2;

    /// \brief Whether or not this element has an associated value.
    unsigned HasArgumentType : 1;
  };
  enum { NumEnumElementDeclBits = NumValueDeclBits + 3 };
  static_assert(NumEnumElementDeclBits <= 32, "fits in an unsigned");
  
  class AbstractFunctionDeclBitfields {
    friend class AbstractFunctionDecl;
    unsigned : NumValueDeclBits;

    /// \see AbstractFunctionDecl::BodyKind
    unsigned BodyKind : 3;

    /// Number of curried parameter lists.
    unsigned NumParameterLists : 6;

    /// Whether we are overridden later.
    unsigned Overridden : 1;

    /// Whether the function body throws.
    unsigned Throws : 1;
  };
  enum { NumAbstractFunctionDeclBits = NumValueDeclBits + 11 };
  static_assert(NumAbstractFunctionDeclBits <= 32, "fits in an unsigned");

  class FuncDeclBitfields {
    friend class FuncDecl;
    unsigned : NumAbstractFunctionDeclBits;

    /// Whether this function is a 'static' method.
    unsigned IsStatic : 1;

    /// \brief Whether 'static' or 'class' was used.
    unsigned StaticSpelling : 2;

    /// Whether this function is a 'mutating' method.
    unsigned Mutating : 1;

    /// Whether this function has a dynamic Self return type.
    unsigned HasDynamicSelf : 1;
    
    /// Whether we are statically dispatched even if overridable
    unsigned ForcedStaticDispatch : 1;
  };
  enum { NumFuncDeclBits = NumAbstractFunctionDeclBits + 6 };
  static_assert(NumFuncDeclBits <= 32, "fits in an unsigned");

  class ConstructorDeclBitfields {
    friend class ConstructorDecl;
    unsigned : NumAbstractFunctionDeclBits;

    /// The body initialization kind (+1), or zero if not yet computed.
    ///
    /// This value is cached but is not serialized, because it is a property
    /// of the definition of the constructor that is useful only to semantic
    /// analysis and SIL generation.
    unsigned ComputedBodyInitKind : 3;

    /// The kind of initializer we have.
    unsigned InitKind : 2;

    /// Whether this initializer is a stub placed into a subclass to
    /// catch invalid delegations to a designated initializer not
    /// overridden by the subclass. A stub will always trap at runtime.
    ///
    /// Initializer stubs can be invoked from Objective-C or through
    /// the Objective-C runtime; there is no way to directly express
    /// an object construction that will invoke a stub.
    unsigned HasStubImplementation : 1;
  };
  enum { NumConstructorDeclBits = NumAbstractFunctionDeclBits + 6 };
  static_assert(NumConstructorDeclBits <= 32, "fits in an unsigned");

  class TypeDeclBitfields {
    friend class TypeDecl;
    unsigned : NumValueDeclBits;

    /// Whether we have already checked the inheritance clause.
    ///
    /// FIXME: Is this too fine-grained?
    unsigned CheckedInheritanceClause : 1;
  };

  enum { NumTypeDeclBits = NumValueDeclBits + 1 };
  static_assert(NumTypeDeclBits <= 32, "fits in an unsigned");

  class GenericTypeDeclBitfields {
    friend class GenericTypeDecl;
    unsigned : NumTypeDeclBits;
  };

  enum { NumGenericTypeDeclBits = NumTypeDeclBits };
  static_assert(NumGenericTypeDeclBits <= 32, "fits in an unsigned");

  class TypeAliasDeclBitfields {
    friend class TypeAliasDecl;
    unsigned : NumGenericTypeDeclBits;

    /// Whether we have completed validation of the typealias.
    /// This is necessary because unlike other declarations, a
    /// typealias will not get an interface type right away.
    unsigned HasCompletedValidation : 1;
  };
  enum { NumTypeAliasDeclBits = NumGenericTypeDeclBits + 1 };
  static_assert(NumTypeAliasDeclBits <= 32, "fits in an unsigned");

  class NominalTypeDeclBitFields {
    friend class NominalTypeDecl;
    unsigned : NumGenericTypeDeclBits;
    
    /// Whether or not the nominal type decl has delayed protocol or member
    /// declarations.
    unsigned HasDelayedMembers : 1;

    /// Whether we have already added implicitly-defined initializers
    /// to this declaration.
    unsigned AddedImplicitInitializers : 1;

    /// \brief Whether or not this declaration has a failable initializer member,
    /// and whether or not we've actually searched for one.
    unsigned HasFailableInits : 1;

    /// Whether we have already searched for failable initializers.
    unsigned SearchedForFailableInits : 1;

    /// Whether there is are lazily-loaded conformances for this nominal type.
    unsigned HasLazyConformances : 1;
  };
  enum { NumNominalTypeDeclBits = NumGenericTypeDeclBits + 5 };
  static_assert(NumNominalTypeDeclBits <= 32, "fits in an unsigned");

  class ProtocolDeclBitfields {
    friend class ProtocolDecl;
    unsigned : NumNominalTypeDeclBits;

    /// Whether the \c RequiresClass bit is valid.
    unsigned RequiresClassValid : 1;

    /// Whether this is a class-bounded protocol.
    unsigned RequiresClass : 1;

    /// Whether the \c ExistentialConformsToSelf bit is valid.
    unsigned ExistentialConformsToSelfValid : 1;

    /// Whether the existential of this protocol conforms to itself.
    unsigned ExistentialConformsToSelf : 1;

    /// Whether the \c ExistentialTypeSupported bit is valid.
    unsigned ExistentialTypeSupportedValid : 1;

    /// Whether the existential of this protocol can be represented.
    unsigned ExistentialTypeSupported : 1;

    /// The stage of the circularity check for this protocol.
    unsigned Circularity : 2;
  };
  enum { NumProtocolDeclBits = NumNominalTypeDeclBits + 8 };
  static_assert(NumProtocolDeclBits <= 32, "fits in an unsigned");

  class ClassDeclBitfields {
    friend class ClassDecl;
    unsigned : NumNominalTypeDeclBits;

    /// The stage of the inheritance circularity check for this class.
    unsigned Circularity : 2;

    /// Whether this class requires all of its instance variables to
    /// have in-class initializers.
    unsigned RequiresStoredPropertyInits : 1;

    /// Whether this class inherits its superclass's convenience
    /// initializers.
    ///
    /// This is a value of \c StoredInheritsSuperclassInits.
    unsigned InheritsSuperclassInits : 2;

    /// \see ClassDecl::ForeignKind
    unsigned RawForeignKind : 2;
    
    /// Whether this class contains a destructor decl.
    ///
    /// A fully type-checked class always contains a destructor member, even if
    /// it is implicit. This bit is used during parsing and type-checking to
    /// control inserting the implicit destructor.
    unsigned HasDestructorDecl : 1;

    /// Whether the class has @objc ancestry.
    unsigned ObjCClassKind : 3;
  };
  enum { NumClassDeclBits = NumNominalTypeDeclBits + 11 };
  static_assert(NumClassDeclBits <= 32, "fits in an unsigned");

  class StructDeclBitfields {
    friend class StructDecl;
    unsigned : NumNominalTypeDeclBits;
    
    /// True if this struct has storage for fields that aren't accessible in
    /// Swift.
    unsigned HasUnreferenceableStorage : 1;
  };
  enum { NumStructDeclBits = NumNominalTypeDeclBits + 1 };
  static_assert(NumStructDeclBits <= 32, "fits in an unsigned");
  
  class EnumDeclBitfields {
    friend class EnumDecl;
    unsigned : NumNominalTypeDeclBits;
    
    /// The stage of the raw type circularity check for this class.
    unsigned Circularity : 2;
  };
  enum { NumEnumDeclBits = NumNominalTypeDeclBits + 2 };
  static_assert(NumEnumDeclBits <= 32, "fits in an unsigned");
  
  class PrecedenceGroupDeclBitfields {
    friend class PrecedenceGroupDecl;
    unsigned : NumDeclBits;

    /// The group's associativity.  A value of the Associativity enum.
    unsigned Associativity : 2;

    /// Is this an assignment operator?
    unsigned IsAssignment : 1;
  };
  enum { NumPrecedenceGroupDeclBits = NumDeclBits + 11 };
  static_assert(NumPrecedenceGroupDeclBits <= 32, "fits in an unsigned");

  class AssociatedTypeDeclBitfields {
    friend class AssociatedTypeDecl;
    unsigned : NumTypeDeclBits;
  };
  enum { NumAssociatedTypeDeclBits = NumTypeDeclBits };
  static_assert(NumAssociatedTypeDeclBits <= 32, "fits in an unsigned");

  class ImportDeclBitfields {
    friend class ImportDecl;
    unsigned : NumDeclBits;

    unsigned ImportKind : 3;
  };
  enum { NumImportDeclBits = NumDeclBits + 3 };
  static_assert(NumImportDeclBits <= 32, "fits in an unsigned");

  class ExtensionDeclBitfields {
    friend class ExtensionDecl;
    unsigned : NumDeclBits;

    /// Whether we have already checked the inheritance clause.
    ///
    /// FIXME: Is this too fine-grained?
    unsigned CheckedInheritanceClause : 1;

    /// Whether this extension has already been validated.
    unsigned Validated : 1;

    /// An encoding of the default and maximum access level for this extension.
    ///
    /// This is encoded as (1 << (maxAccess-1)) | (1 << (defaultAccess-1)),
    /// which works because the maximum is always greater than or equal to the
    /// default, and 'private' is never used. 0 represents an uncomputed value.
    unsigned DefaultAndMaxAccessLevel : 3;

    /// Whether there is are lazily-loaded conformances for this extension.
    unsigned HasLazyConformances : 1;
  };
  enum { NumExtensionDeclBits = NumDeclBits + 6 };
  static_assert(NumExtensionDeclBits <= 32, "fits in an unsigned");

  class IfConfigDeclBitfields {
    friend class IfConfigDecl;
    unsigned : NumDeclBits;

    /// Whether this decl is missing its closing '#endif'.
    unsigned HadMissingEnd : 1;
  };
  enum { NumIfConfigDeclBits = NumDeclBits + 1 };
  static_assert(NumIfConfigDeclBits <= 32, "fits in an unsigned");

protected:
  union {
    DeclBitfields DeclBits;
    PatternBindingDeclBitfields PatternBindingDeclBits;
    ValueDeclBitfields ValueDeclBits;
    AbstractStorageDeclBitfields AbstractStorageDeclBits;
    AbstractFunctionDeclBitfields AbstractFunctionDeclBits;
    VarDeclBitfields VarDeclBits;
    EnumElementDeclBitfields EnumElementDeclBits;
    FuncDeclBitfields FuncDeclBits;
    ConstructorDeclBitfields ConstructorDeclBits;
    TypeDeclBitfields TypeDeclBits;
    GenericTypeDeclBitfields GenericTypeDeclBits;
    TypeAliasDeclBitfields TypeAliasDeclBits;
    NominalTypeDeclBitFields NominalTypeDeclBits;
    ProtocolDeclBitfields ProtocolDeclBits;
    ClassDeclBitfields ClassDeclBits;
    StructDeclBitfields StructDeclBits;
    EnumDeclBitfields EnumDeclBits;
    AssociatedTypeDeclBitfields AssociatedTypeDeclBits;
    PrecedenceGroupDeclBitfields PrecedenceGroupDeclBits;
    ImportDeclBitfields ImportDeclBits;
    ExtensionDeclBitfields ExtensionDeclBits;
    IfConfigDeclBitfields IfConfigDeclBits;
    uint32_t OpaqueBits;
  };

  // FIXME: Unused padding here.

  // Storage for the declaration attributes.
  DeclAttributes Attrs;
  
  /// The next declaration in the list of declarations within this
  /// member context.
  Decl *NextDecl = nullptr;
  
  friend class DeclIterator;
  friend class IterableDeclContext;
  friend class MemberLookupTable;

private:
  llvm::PointerUnion<DeclContext *, ASTContext *> Context;

  Decl(const Decl&) = delete;
  void operator=(const Decl&) = delete;

protected:

  Decl(DeclKind kind, llvm::PointerUnion<DeclContext *, ASTContext *> context)
    : OpaqueBits(0), Context(context) {
    DeclBits.Kind = unsigned(kind);
    DeclBits.Invalid = false;
    DeclBits.Implicit = false;
    DeclBits.FromClang = false;
    DeclBits.EarlyAttrValidation = false;
    DeclBits.BeingValidated = false;
    DeclBits.EscapedFromIfConfig = false;
  }

  ClangNode getClangNodeImpl() const {
    assert(DeclBits.FromClang);
    return ClangNode::getFromOpaqueValue(
        *(reinterpret_cast<void * const*>(this) - 1));
  }

  /// \brief Set the Clang node associated with this declaration.
  void setClangNode(ClangNode Node) {
    DeclBits.FromClang = true;
    // Extra memory is allocated for this.
    *(reinterpret_cast<void **>(this) - 1) = Node.getOpaqueValue();
  }

  void updateClangNode(ClangNode node) {
    assert(hasClangNode());
    setClangNode(node);
  }
  friend class ClangImporter;

  DeclContext *getDeclContextForModule() const;

public:
  DeclKind getKind() const { return DeclKind(DeclBits.Kind); }

  /// \brief Retrieve the name of the given declaration kind.
  ///
  /// This name should only be used for debugging dumps and other
  /// developer aids, and should never be part of a diagnostic or exposed
  /// to the user of the compiler in any way.
  static StringRef getKindName(DeclKind K);

  /// Retrieve the descriptive kind for this declaration.
  DescriptiveDeclKind getDescriptiveKind() const;

  /// Produce a name for the given descriptive declaration kind, which
  /// is suitable for use in diagnostics.
  static StringRef getDescriptiveKindName(DescriptiveDeclKind K);

  /// Whether swift users should be able to access this decl. For instance,
  /// var a.storage for lazy var a is an inaccessible decl. An inaccessible decl
  /// has to be implicit; but an implicit decl does not have to be inaccessible,
  /// for instance, self.
  bool isUserAccessible() const;

  /// Determine if the decl can have a comment.  If false, a comment will
  /// not be serialized.
  bool canHaveComment() const;

  DeclContext *getDeclContext() const {
    if (auto dc = Context.dyn_cast<DeclContext *>())
      return dc;

    return getDeclContextForModule();
  }
  void setDeclContext(DeclContext *DC);

  /// Retrieve the innermost declaration context corresponding to this
  /// declaration, which will either be the declaration itself (if it's
  /// also a declaration context) or its declaration context.
  DeclContext *getInnermostDeclContext() const;

  /// \brief Retrieve the module in which this declaration resides.
  ModuleDecl *getModuleContext() const;

  /// getASTContext - Return the ASTContext that this decl lives in.
  ASTContext &getASTContext() const {
    if (auto dc = Context.dyn_cast<DeclContext *>())
      return dc->getASTContext();

    return *Context.get<ASTContext *>();
  }

  const DeclAttributes &getAttrs() const {
    return Attrs;
  }

  DeclAttributes &getAttrs() {
    return Attrs;
  }

  /// Returns the starting location of the entire declaration.
  SourceLoc getStartLoc() const { return getSourceRange().Start; }

  /// Returns the end location of the entire declaration.
  SourceLoc getEndLoc() const { return getSourceRange().End; }

  /// Returns the preferred location when referring to declarations
  /// in diagnostics.
  SourceLoc getLoc() const;

  /// Returns the source range of the entire declaration.
  SourceRange getSourceRange() const;

  SourceLoc TrailingSemiLoc;

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump() const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");
  void dump(raw_ostream &OS, unsigned Indent = 0) const;

  /// \brief Pretty-print the given declaration.
  ///
  /// \param OS Output stream to which the declaration will be printed.
  void print(raw_ostream &OS) const;
  void print(raw_ostream &OS, const PrintOptions &Opts) const;

  /// \brief Pretty-print the given declaration.
  ///
  /// \param Printer ASTPrinter object.
  ///
  /// \param Opts Options to control how pretty-printing is performed.
  ///
  /// \returns true if the declaration was printed or false if the print options
  /// required the declaration to be skipped from printing.
  bool print(ASTPrinter &Printer, const PrintOptions &Opts) const;

  /// \brief Determine whether this declaration should be printed when
  /// encountered in its declaration context's list of members.
  bool shouldPrintInContext(const PrintOptions &PO) const;

  bool walk(ASTWalker &walker);

  /// \brief Return whether this declaration has been determined invalid.
  bool isInvalid() const { return DeclBits.Invalid; }
  
  /// \brief Mark this declaration invalid.
  void setInvalid(bool isInvalid = true) { DeclBits.Invalid = isInvalid; }

  /// \brief Determine whether this declaration was implicitly generated by the
  /// compiler (rather than explicitly written in source code).
  bool isImplicit() const { return DeclBits.Implicit; }

  /// \brief Mark this declaration as implicit.
  void setImplicit(bool implicit = true) { DeclBits.Implicit = implicit; }

  /// Whether we have already done early attribute validation.
  bool didEarlyAttrValidation() const { return DeclBits.EarlyAttrValidation; }

  /// Set whether we've performed early attribute validation.
  void setEarlyAttrValidation(bool validated = true) {
    DeclBits.EarlyAttrValidation = validated;
  }
  
  /// Whether the declaration has a valid interface type and
  /// generic signature.
  bool isBeingValidated() const {
    return DeclBits.BeingValidated;
  }

  /// Toggle whether or not the declaration is being validated.
  void setIsBeingValidated(bool ibv = true) {
    assert(DeclBits.BeingValidated != ibv);
    DeclBits.BeingValidated = ibv;
  }

  bool escapedFromIfConfig() const {
    return DeclBits.EscapedFromIfConfig;
  }

  void setEscapedFromIfConfig(bool Escaped) {
    DeclBits.EscapedFromIfConfig = Escaped;
  }

  /// \returns the unparsed comment attached to this declaration.
  RawComment getRawComment() const;

  Optional<StringRef> getGroupName() const;

  Optional<StringRef> getSourceFileName() const;

  Optional<unsigned> getSourceOrder() const;

  /// \returns the brief comment attached to this declaration.
  StringRef getBriefComment() const;

  /// \brief Returns true if there is a Clang AST node associated
  /// with self.
  bool hasClangNode() const {
    return DeclBits.FromClang;
  }

  /// \brief Retrieve the Clang AST node from which this declaration was
  /// synthesized, if any.
  ClangNode getClangNode() const {
    if (!DeclBits.FromClang)
      return ClangNode();

    return getClangNodeImpl();
  }

  /// \brief Retrieve the Clang declaration from which this declaration was
  /// synthesized, if any.
  const clang::Decl *getClangDecl() const {
    if (!DeclBits.FromClang)
      return nullptr;

    return getClangNodeImpl().getAsDecl();
  }

  /// \brief Retrieve the Clang macro from which this declaration was
  /// synthesized, if any.
  const clang::MacroInfo *getClangMacro() {
    if (!DeclBits.FromClang)
      return nullptr;

    return getClangNodeImpl().getAsMacro();
  }

  bool isPrivateStdlibDecl(bool whitelistProtocols=true) const;

  /// Whether this declaration is weak-imported.
  bool isWeakImported(ModuleDecl *fromModule) const;

  /// Returns true if the nature of this declaration allows overrides.
  /// Note that this does not consider whether it is final or whether
  /// the class it's on is final.
  ///
  /// If this returns true, the decl can be safely casted to ValueDecl.
  bool isPotentiallyOverridable() const;

  // Make vanilla new/delete illegal for Decls.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) SWIFT_DELETE_OPERATOR_DELETED;

  // Only allow allocation of Decls using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, const ASTContext &C,
                     unsigned Alignment = alignof(Decl));
  void *operator new(size_t Bytes, void *Mem) { 
    assert(Mem); 
    return Mem; 
  }
};

/// \brief Allocates memory for a Decl with the given \p baseSize. If necessary,
/// it includes additional space immediately preceding the Decl for a ClangNode.
/// \note \p baseSize does not need to include space for a ClangNode if
/// requested -- the necessary space will be added automatically.
template <typename DeclTy, typename AllocatorTy>
void *allocateMemoryForDecl(AllocatorTy &allocator, size_t baseSize,
                            bool includeSpaceForClangNode) {
  static_assert(alignof(DeclTy) >= sizeof(void *),
                "A pointer must fit in the alignment of the DeclTy!");

  size_t size = baseSize;
  if (includeSpaceForClangNode)
    size += alignof(DeclTy);

  void *mem = allocator.Allocate(size, alignof(DeclTy));
  if (includeSpaceForClangNode)
    mem = reinterpret_cast<char *>(mem) + alignof(DeclTy);
  return mem;
}

enum class RequirementReprKind : unsigned {
  /// A type bound T : P, where T is a type that depends on a generic
  /// parameter and P is some type that should bound T, either as a concrete
  /// supertype or a protocol to which T must conform.
  TypeConstraint,

  /// A same-type requirement T == U, where T and U are types that shall be
  /// equivalent.
  SameType,

  /// A layout bound T : L, where T is a type that depends on a generic
  /// parameter and L is some layout specification that should bound T.
  LayoutConstraint,

  // Note: there is code that packs this enum in a 2-bit bitfield.  Audit users
  // when adding enumerators.
};

/// \brief A single requirement in a 'where' clause, which places additional
/// restrictions on the generic parameters or associated types of a generic
/// function, type, or protocol.
///
/// This always represents a requirement spelled in the source code.  It is
/// never generated implicitly.
///
/// \c GenericParamList assumes these are POD-like.
class RequirementRepr {
  SourceLoc SeparatorLoc;
  RequirementReprKind Kind : 2;
  bool Invalid : 1;
  TypeLoc FirstType;

  /// The second element represents the right-hand side of the constraint.
  /// It can be e.g. a type or a layout constraint.
  union {
    TypeLoc SecondType;
    LayoutConstraintLoc SecondLayout;
  };

  /// Set during deserialization; used to print out the requirements accurately
  /// for the generated interface.
  StringRef AsWrittenString;

  RequirementRepr(SourceLoc SeparatorLoc, RequirementReprKind Kind,
                  TypeLoc FirstType, TypeLoc SecondType)
    : SeparatorLoc(SeparatorLoc), Kind(Kind), Invalid(false),
      FirstType(FirstType), SecondType(SecondType) { }

  RequirementRepr(SourceLoc SeparatorLoc, RequirementReprKind Kind,
                  TypeLoc FirstType, LayoutConstraintLoc SecondLayout)
    : SeparatorLoc(SeparatorLoc), Kind(Kind), Invalid(false),
      FirstType(FirstType), SecondLayout(SecondLayout) { }

  void printImpl(raw_ostream &OS, bool AsWritten) const;

public:
  /// \brief Construct a new type-constraint requirement.
  ///
  /// \param Subject The type that must conform to the given protocol or
  /// composition, or be a subclass of the given class type.
  /// \param ColonLoc The location of the ':', or an invalid location if
  /// this requirement was implied.
  /// \param Constraint The protocol or protocol composition to which the
  /// subject must conform, or superclass from which the subject must inherit.
  static RequirementRepr getTypeConstraint(TypeLoc Subject,
                                           SourceLoc ColonLoc,
                                           TypeLoc Constraint) {
    return { ColonLoc, RequirementReprKind::TypeConstraint, Subject, Constraint };
  }

  /// \brief Construct a new same-type requirement.
  ///
  /// \param FirstType The first type.
  /// \param EqualLoc The location of the '==' in the same-type constraint, or
  /// an invalid location if this requirement was implied.
  /// \param SecondType The second type.
  static RequirementRepr getSameType(TypeLoc FirstType,
                                     SourceLoc EqualLoc,
                                     TypeLoc SecondType) {
    return { EqualLoc, RequirementReprKind::SameType, FirstType, SecondType };
  }

  /// \brief Construct a new layout-constraint requirement.
  ///
  /// \param Subject The type that must conform to the given layout 
  /// requirement.
  /// \param ColonLoc The location of the ':', or an invalid location if
  /// this requirement was implied.
  /// \param Layout The layout requirement to which the
  /// subject must conform.
  static RequirementRepr getLayoutConstraint(TypeLoc Subject,
                                             SourceLoc ColonLoc,
                                             LayoutConstraintLoc Layout) {
    return {ColonLoc, RequirementReprKind::LayoutConstraint, Subject,
            Layout};
  }

  /// \brief Determine the kind of requirement
  RequirementReprKind getKind() const { return Kind; }

  /// \brief Determine whether this requirement is invalid.
  bool isInvalid() const { return Invalid; }

  /// \brief Mark this requirement invalid.
  void setInvalid() { Invalid = true; }

  /// \brief For a type-bound requirement, return the subject of the
  /// conformance relationship.
  Type getSubject() const {
    assert(getKind() == RequirementReprKind::TypeConstraint ||
           getKind() == RequirementReprKind::LayoutConstraint);
    return FirstType.getType();
  }

  TypeRepr *getSubjectRepr() const {
    assert(getKind() == RequirementReprKind::TypeConstraint ||
           getKind() == RequirementReprKind::LayoutConstraint);
    return FirstType.getTypeRepr();
  }

  TypeLoc &getSubjectLoc() {
    assert(getKind() == RequirementReprKind::TypeConstraint ||
           getKind() == RequirementReprKind::LayoutConstraint);
    return FirstType;
  }

  const TypeLoc &getSubjectLoc() const {
    assert(getKind() == RequirementReprKind::TypeConstraint ||
           getKind() == RequirementReprKind::LayoutConstraint);
    return FirstType;
  }

  /// \brief For a type-bound requirement, return the protocol or to which
  /// the subject conforms or superclass it inherits.
  Type getConstraint() const {
    assert(getKind() == RequirementReprKind::TypeConstraint);
    return SecondType.getType();
  }

  TypeRepr *getConstraintRepr() const {
    assert(getKind() == RequirementReprKind::TypeConstraint);
    return SecondType.getTypeRepr();
  }

  TypeLoc &getConstraintLoc() {
    assert(getKind() == RequirementReprKind::TypeConstraint);
    return SecondType;
  }

  const TypeLoc &getConstraintLoc() const {
    assert(getKind() == RequirementReprKind::TypeConstraint);
    return SecondType;
  }

  LayoutConstraint getLayoutConstraint() const {
    assert(getKind() == RequirementReprKind::LayoutConstraint);
    return SecondLayout.getLayoutConstraint();
  }

  LayoutConstraintLoc &getLayoutConstraintLoc() {
    assert(getKind() == RequirementReprKind::LayoutConstraint);
    return SecondLayout;
  }

  const LayoutConstraintLoc &getLayoutConstraintLoc() const {
    assert(getKind() == RequirementReprKind::LayoutConstraint);
    return SecondLayout;
  }

  /// \brief Retrieve the location of the ':' in an explicitly-written
  /// conformance requirement.
  SourceLoc getColonLoc() const {
    assert(getKind() == RequirementReprKind::TypeConstraint ||
           getKind() == RequirementReprKind::LayoutConstraint);
    return SeparatorLoc;
  }

  /// \brief Retrieve the first type of a same-type requirement.
  Type getFirstType() const {
    assert(getKind() == RequirementReprKind::SameType);
    return FirstType.getType();
  }

  TypeRepr *getFirstTypeRepr() const {
    assert(getKind() == RequirementReprKind::SameType);
    return FirstType.getTypeRepr();
  }

  TypeLoc &getFirstTypeLoc() {
    assert(getKind() == RequirementReprKind::SameType);
    return FirstType;
  }

  const TypeLoc &getFirstTypeLoc() const {
    assert(getKind() == RequirementReprKind::SameType);
    return FirstType;
  }

  /// \brief Retrieve the second type of a same-type requirement.
  Type getSecondType() const {
    assert(getKind() == RequirementReprKind::SameType);
    return SecondType.getType();
  }

  TypeRepr *getSecondTypeRepr() const {
    assert(getKind() == RequirementReprKind::SameType);
    return SecondType.getTypeRepr();
  }

  TypeLoc &getSecondTypeLoc() {
    assert(getKind() == RequirementReprKind::SameType);
    return SecondType;
  }

  const TypeLoc &getSecondTypeLoc() const {
    assert(getKind() == RequirementReprKind::SameType);
    return SecondType;
  }

  /// \brief Retrieve the location of the '==' in an explicitly-written
  /// same-type requirement.
  SourceLoc getEqualLoc() const {
    assert(getKind() == RequirementReprKind::SameType);
    return SeparatorLoc;
  }

  SourceRange getSourceRange() const {
    if (getKind() == RequirementReprKind::LayoutConstraint)
      return SourceRange(FirstType.getSourceRange().Start,
                         SecondLayout.getSourceRange().End);
    return SourceRange(FirstType.getSourceRange().Start,
                       SecondType.getSourceRange().End);
  }

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump() const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");
  void print(raw_ostream &OS) const;
};
  
/// GenericParamList - A list of generic parameters that is part of a generic
/// function or type, along with extra requirements placed on those generic
/// parameters and types derived from them.
class GenericParamList final :
    private llvm::TrailingObjects<GenericParamList, GenericTypeParamDecl *> {
  friend TrailingObjects;

  SourceRange Brackets;
  unsigned NumParams;
  SourceLoc WhereLoc;
  MutableArrayRef<RequirementRepr> Requirements;

  GenericParamList *OuterParameters;

  SourceLoc TrailingWhereLoc;
  unsigned FirstTrailingWhereArg;

  GenericParamList(SourceLoc LAngleLoc,
                   ArrayRef<GenericTypeParamDecl *> Params,
                   SourceLoc WhereLoc,
                   MutableArrayRef<RequirementRepr> Requirements,
                   SourceLoc RAngleLoc);
  
  // Don't copy.
  GenericParamList(const GenericParamList &) = delete;
  GenericParamList &operator=(const GenericParamList &) = delete;
  
public:
  /// create - Create a new generic parameter list within the given AST context.
  ///
  /// \param Context The ASTContext in which the generic parameter list will
  /// be allocated.
  /// \param LAngleLoc The location of the opening angle bracket ('<')
  /// \param Params The list of generic parameters, which will be copied into
  /// ASTContext-allocated memory.
  /// \param RAngleLoc The location of the closing angle bracket ('>')
  static GenericParamList *create(ASTContext &Context,
                                  SourceLoc LAngleLoc,
                                  ArrayRef<GenericTypeParamDecl *> Params,
                                  SourceLoc RAngleLoc);

  /// create - Create a new generic parameter list and "where" clause within
  /// the given AST context.
  ///
  /// \param Context The ASTContext in which the generic parameter list will
  /// be allocated.
  /// \param LAngleLoc The location of the opening angle bracket ('<')
  /// \param Params The list of generic parameters, which will be copied into
  /// ASTContext-allocated memory.
  /// \param WhereLoc The location of the 'where' keyword, if any.
  /// \param Requirements The list of requirements, which will be copied into
  /// ASTContext-allocated memory.
  /// \param RAngleLoc The location of the closing angle bracket ('>')
  static GenericParamList *create(const ASTContext &Context,
                                  SourceLoc LAngleLoc,
                                  ArrayRef<GenericTypeParamDecl *> Params,
                                  SourceLoc WhereLoc,
                                  MutableArrayRef<RequirementRepr> Requirements,
                                  SourceLoc RAngleLoc);

  MutableArrayRef<GenericTypeParamDecl *> getParams() {
    return {getTrailingObjects<GenericTypeParamDecl *>(), NumParams};
  }

  ArrayRef<GenericTypeParamDecl *> getParams() const {
    return {getTrailingObjects<GenericTypeParamDecl *>(), NumParams};
  }

  using iterator = GenericTypeParamDecl **;
  using const_iterator = const GenericTypeParamDecl * const *;

  unsigned size() const { return NumParams; }
  iterator begin() { return getParams().begin(); }
  iterator end() { return getParams().end(); }
  const_iterator begin() const { return getParams().begin(); }
  const_iterator end() const { return getParams().end(); }

  /// \brief Retrieve the location of the 'where' keyword, or an invalid
  /// location if 'where' was not present.
  SourceLoc getWhereLoc() const { return WhereLoc; }

  /// \brief Retrieve the set of additional requirements placed on these
  /// generic parameters and types derived from them.
  ///
  /// This list may contain both explicitly-written requirements as well as
  /// implicitly-generated requirements, and may be non-empty even if no
  /// 'where' keyword is present.
  MutableArrayRef<RequirementRepr> getRequirements() { return Requirements; }

  /// \brief Retrieve the set of additional requirements placed on these
  /// generic parameters and types derived from them.
  ///
  /// This list may contain both explicitly-written requirements as well as
  /// implicitly-generated requirements, and may be non-empty even if no
  /// 'where' keyword is present.
  ArrayRef<RequirementRepr> getRequirements() const { return Requirements; }

  /// Retrieve only those requirements that are written within the brackets,
  /// which does not include any requirements written in a trailing where
  /// clause.
  ArrayRef<RequirementRepr> getNonTrailingRequirements() const {
    return Requirements.slice(0, FirstTrailingWhereArg);
  }

  /// Retrieve only those requirements that are written within the brackets,
  /// which does not include any requirements written in a trailing where
  /// clause.
  ArrayRef<RequirementRepr> getTrailingRequirements() const {
    return Requirements.slice(FirstTrailingWhereArg);
  }

  /// Determine whether the generic parameters have a trailing where clause.
  bool hasTrailingWhereClause() const {
    return FirstTrailingWhereArg < Requirements.size();
  }

  /// Add a trailing 'where' clause to the list of requirements.
  ///
  /// Trailing where clauses are written outside the angle brackets, after the
  /// main part of a declaration's signature.
  void addTrailingWhereClause(ASTContext &ctx, SourceLoc trailingWhereLoc,
                              ArrayRef<RequirementRepr> trailingRequirements);
  
  /// \brief Retrieve the outer generic parameter list, which provides the
  /// generic parameters of the context in which this generic parameter list
  /// exists.
  ///
  /// Consider the following generic class:
  ///
  /// \code
  /// class Vector<T> {
  ///   init<R : Range where R.Element == T>(range : R) { }
  /// }
  /// \endcode
  ///
  /// The generic parameter list <T> has no outer parameters, because it is
  /// the outermost generic parameter list. The generic parameter list
  /// <R : Range...> for the constructor has the generic parameter list <T> as
  /// its outer generic parameter list.
  GenericParamList *getOuterParameters() const { return OuterParameters; }

  /// \brief Set the outer generic parameter list. See \c getOuterParameters
  /// for more information.
  void setOuterParameters(GenericParamList *Outer) { OuterParameters = Outer; }

  SourceLoc getLAngleLoc() const { return Brackets.Start; }
  SourceLoc getRAngleLoc() const { return Brackets.End; }

  SourceRange getSourceRange() const { return Brackets; }

  /// Retrieve the source range covering the where clause.
  SourceRange getWhereClauseSourceRange() const {
    if (WhereLoc.isInvalid())
      return SourceRange();

    auto endLoc = Requirements[FirstTrailingWhereArg-1].getSourceRange().End;
    return SourceRange(WhereLoc, endLoc);
  }

  /// Retrieve the source range covering the trailing where clause.
  SourceRange getTrailingWhereClauseSourceRange() const {
    if (!hasTrailingWhereClause())
      return SourceRange();

    return SourceRange(TrailingWhereLoc,
                       Requirements.back().getSourceRange().End);
  }

  /// Retrieve the depth of this generic parameter list.
  unsigned getDepth() const {
    unsigned depth = 0;
    for (auto gp = getOuterParameters(); gp; gp = gp->getOuterParameters())
      ++depth;
    return depth;
  }

  void print(raw_ostream &OS);
  void dump();
};
  
/// A trailing where clause.
class alignas(RequirementRepr) TrailingWhereClause final :
    private llvm::TrailingObjects<TrailingWhereClause, RequirementRepr> {
  friend TrailingObjects;

  SourceLoc WhereLoc;

  /// The number of requirements. The actual requirements are tail-allocated.
  unsigned NumRequirements;

  TrailingWhereClause(SourceLoc whereLoc,
                      ArrayRef<RequirementRepr> requirements);

public:
  /// Create a new trailing where clause with the given set of requirements.
  static TrailingWhereClause *create(ASTContext &ctx, SourceLoc whereLoc,
                                     ArrayRef<RequirementRepr> requirements);

  /// Retrieve the location of the 'where' keyword.
  SourceLoc getWhereLoc() const { return WhereLoc; }

  /// Retrieve the set of requirements.
  MutableArrayRef<RequirementRepr> getRequirements() {
    return {getTrailingObjects<RequirementRepr>(), NumRequirements};
  }

  /// Retrieve the set of requirements.
  ArrayRef<RequirementRepr> getRequirements() const {
    return {getTrailingObjects<RequirementRepr>(), NumRequirements};
  }

  /// Compute the source range containing this trailing where clause.
  SourceRange getSourceRange() const {
    return SourceRange(WhereLoc,
                       getRequirements().back().getSourceRange().End);
  }
};

/// Describes what kind of name is being imported.
///
/// If the enumerators here are changed, make sure to update all diagnostics
/// using ImportKind as a select index.
enum class ImportKind : uint8_t {
  Module = 0,
  Type,
  Struct,
  Class,
  Enum,
  Protocol,
  Var,
  Func
};

/// ImportDecl - This represents a single import declaration, e.g.:
///   import Swift
///   import typealias Swift.Int
class ImportDecl final : public Decl,
    private llvm::TrailingObjects<ImportDecl, std::pair<Identifier,SourceLoc>> {
  friend TrailingObjects;

public:
  typedef std::pair<Identifier, SourceLoc> AccessPathElement;

private:
  SourceLoc ImportLoc;
  SourceLoc KindLoc;

  /// The number of elements in this path.
  unsigned NumPathElements;

  /// The resolved module.
  ModuleDecl *Mod = nullptr;
  /// The resolved decls if this is a decl import.
  ArrayRef<ValueDecl *> Decls;

  ImportDecl(DeclContext *DC, SourceLoc ImportLoc, ImportKind K,
             SourceLoc KindLoc, ArrayRef<AccessPathElement> Path);

public:
  static ImportDecl *create(ASTContext &C, DeclContext *DC,
                            SourceLoc ImportLoc, ImportKind Kind,
                            SourceLoc KindLoc,
                            ArrayRef<AccessPathElement> Path,
                            ClangNode ClangN = ClangNode());

  /// Returns the import kind that is most appropriate for \p VD.
  ///
  /// Note that this will never return \c Type; an imported typealias will use
  /// the more specific kind from its underlying type.
  static ImportKind getBestImportKind(const ValueDecl *VD);

  /// Returns the most appropriate import kind for the given list of decls.
  ///
  /// If the list is non-homogeneous, or if there is more than one decl that
  /// cannot be overloaded, returns None.
  static Optional<ImportKind> findBestImportKind(ArrayRef<ValueDecl *> Decls);

  ArrayRef<AccessPathElement> getFullAccessPath() const {
    return {getTrailingObjects<AccessPathElement>(), NumPathElements};
  }

  ArrayRef<AccessPathElement> getModulePath() const {
    auto result = getFullAccessPath();
    if (getImportKind() != ImportKind::Module)
      result = result.slice(0, result.size()-1);
    return result;
  }

  ArrayRef<AccessPathElement> getDeclPath() const {
    if (getImportKind() == ImportKind::Module)
      return {};
    return getFullAccessPath().back();
  }

  ImportKind getImportKind() const {
    return static_cast<ImportKind>(ImportDeclBits.ImportKind);
  }

  bool isExported() const {
    return getAttrs().hasAttribute<ExportedAttr>();
  }

  ModuleDecl *getModule() const { return Mod; }
  void setModule(ModuleDecl *M) { Mod = M; }

  ArrayRef<ValueDecl *> getDecls() const { return Decls; }
  void setDecls(ArrayRef<ValueDecl *> Ds) { Decls = Ds; }

  const clang::Module *getClangModule() const {
    return getClangNode().getClangModule();
  }

  SourceLoc getStartLoc() const { return ImportLoc; }
  SourceLoc getLoc() const { return getFullAccessPath().front().second; }
  SourceRange getSourceRange() const {
    return SourceRange(ImportLoc, getFullAccessPath().back().second);
  }
  SourceLoc getKindLoc() const { return KindLoc; }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Import;
  }
};

/// ExtensionDecl - This represents a type extension containing methods
/// associated with the type.  This is not a ValueDecl and has no Type because
/// there are no runtime values of the Extension's type.  
class ExtensionDecl final : public Decl, public DeclContext,
                            public IterableDeclContext {
  SourceLoc ExtensionLoc;  // Location of 'extension' keyword.
  SourceRange Braces;

  /// The type being extended.
  TypeLoc ExtendedType;
  
  /// The generic parameters of the extension.
  GenericParamList *GenericParams = nullptr;

  /// The generic signature or environment of this extension.
  ///
  /// When this extension stores only a signature, the generic environment
  /// will be lazily loaded.
  mutable llvm::PointerUnion<GenericSignature *, GenericEnvironment *>
    GenericSigOrEnv;

  MutableArrayRef<TypeLoc> Inherited;

  /// The trailing where clause.
  ///
  /// Note that this is not currently serialized, because semantic analysis
  /// moves the trailing where clause into the generic parameter list.
  TrailingWhereClause *TrailingWhere;

  /// \brief The next extension in the linked list of extensions.
  ///
  /// The bit indicates whether this extension has been resolved to refer to
  /// a known nominal type.
  llvm::PointerIntPair<ExtensionDecl *, 1, bool> NextExtension
    = {nullptr, false};

  /// Note that we have added a member into the iterable declaration context.
  void addedMember(Decl *member);

  friend class ExtensionIterator;
  friend class NominalTypeDecl;
  friend class MemberLookupTable;
  friend class ConformanceLookupTable;
  friend class IterableDeclContext;

  ExtensionDecl(SourceLoc extensionLoc, TypeLoc extendedType,
                MutableArrayRef<TypeLoc> inherited,
                DeclContext *parent,
                TrailingWhereClause *trailingWhereClause);

  /// Retrieve the conformance loader (if any), and removing it in the
  /// same operation. The caller is responsible for loading the
  /// conformances.
  std::pair<LazyMemberLoader *, uint64_t> takeConformanceLoader() {
    if (!ExtensionDeclBits.HasLazyConformances)
      return { nullptr, 0 };

    return takeConformanceLoaderSlow();
  }

  /// Slow path for \c takeConformanceLoader().
  std::pair<LazyMemberLoader *, uint64_t> takeConformanceLoaderSlow();

  /// Lazily populate the generic environment.
  GenericEnvironment *getLazyGenericEnvironmentSlow() const;

public:
  using Decl::getASTContext;

  /// Create a new extension declaration.
  static ExtensionDecl *create(ASTContext &ctx, SourceLoc extensionLoc,
                               TypeLoc extendedType,
                               MutableArrayRef<TypeLoc> inherited,
                               DeclContext *parent,
                               TrailingWhereClause *trailingWhereClause,
                               ClangNode clangNode = ClangNode());

  SourceLoc getStartLoc() const { return ExtensionLoc; }
  SourceLoc getLoc() const { return ExtensionLoc; }
  SourceRange getSourceRange() const {
    return { ExtensionLoc, Braces.End };
  }

  SourceRange getBraces() const { return Braces; }
  void setBraces(SourceRange braces) { Braces = braces; }

  /// Retrieve the innermost generic parameter list.
  GenericParamList *getGenericParams() const {
    return GenericParams;
  }

  void setGenericParams(GenericParamList *params);

  /// Retrieve the trailing where clause for this extension, if any.
  TrailingWhereClause *getTrailingWhereClause() const {
    return TrailingWhere;
  }

  /// Set the trailing where clause for this extension.
  void setTrailingWhereClause(TrailingWhereClause *trailingWhereClause) {
    TrailingWhere = trailingWhereClause;
  }

  /// Retrieve the generic requirements.
  ArrayRef<Requirement> getGenericRequirements() const {
    if (auto sig = getGenericSignature())
      return sig->getRequirements();
    else
      return { };
  }

  /// Retrieve the generic signature for this type.
  GenericSignature *getGenericSignature() const;

  /// Retrieve the generic context for this type.
  GenericEnvironment *getGenericEnvironment() const;

  /// Set a lazy generic environment.
  void setLazyGenericEnvironment(LazyMemberLoader *lazyLoader,
                                 GenericSignature *genericSig,
                                 uint64_t genericEnvData);

  /// Set the generic context of this extension.
  void setGenericEnvironment(GenericEnvironment *genericEnv);

  /// Retrieve the type being extended.
  Type getExtendedType() const { return ExtendedType.getType(); }

  /// Retrieve the extended type location.
  TypeLoc &getExtendedTypeLoc() { return ExtendedType; }

  /// Retrieve the extended type location.
  const TypeLoc &getExtendedTypeLoc() const { return ExtendedType; }

  /// \brief Retrieve the set of protocols that this type inherits (i.e,
  /// explicitly conforms to).
  MutableArrayRef<TypeLoc> getInherited() { return Inherited; }
  ArrayRef<TypeLoc> getInherited() const { return Inherited; }

  void setInherited(MutableArrayRef<TypeLoc> i) { Inherited = i; }

  /// Whether we started validating this extension.
  bool validated() const {
    return ExtensionDeclBits.Validated;
  }

  /// Set whether we have validated this extension.
  void setValidated(bool validated = true) {
    ExtensionDeclBits.Validated = validated;
  }

  /// Whether we have fully checked the extension.
  bool hasValidSignature() const {
    return validated() && !isBeingValidated();
  }

  /// Whether we already type-checked the inheritance clause.
  bool checkedInheritanceClause() const {
    return ExtensionDeclBits.CheckedInheritanceClause;
  }

  /// Note that we have already type-checked the inheritance clause.
  void setCheckedInheritanceClause(bool checked = true) {
    ExtensionDeclBits.CheckedInheritanceClause = checked;
  }

  bool hasDefaultAccessibility() const {
    return ExtensionDeclBits.DefaultAndMaxAccessLevel != 0;
  }

  Accessibility getDefaultAccessibility() const {
    assert(hasDefaultAccessibility() && "not computed yet");
    if (ExtensionDeclBits.DefaultAndMaxAccessLevel &
        (1 << (static_cast<unsigned>(Accessibility::FilePrivate) - 1)))
      return Accessibility::FilePrivate;
    if (ExtensionDeclBits.DefaultAndMaxAccessLevel &
        (1 << (static_cast<unsigned>(Accessibility::Internal) - 1)))
      return Accessibility::Internal;
    return Accessibility::Public;
  }

  Accessibility getMaxAccessibility() const {
    assert(hasDefaultAccessibility() && "not computed yet");
    if (ExtensionDeclBits.DefaultAndMaxAccessLevel &
        (1 << (static_cast<unsigned>(Accessibility::Public) - 1)))
      return Accessibility::Public;
    if (ExtensionDeclBits.DefaultAndMaxAccessLevel &
        (1 << (static_cast<unsigned>(Accessibility::Internal) - 1)))
      return Accessibility::Internal;
    return Accessibility::FilePrivate;
  }

  void setDefaultAndMaxAccessibility(Accessibility defaultAccess,
                                     Accessibility maxAccess) {
    assert(!hasDefaultAccessibility() && "default accessibility already set");
    assert(maxAccess >= defaultAccess);
    assert(maxAccess != Accessibility::Private && "private not valid");
    assert(defaultAccess != Accessibility::Private && "private not valid");
    ExtensionDeclBits.DefaultAndMaxAccessLevel =
        (1 << (static_cast<unsigned>(defaultAccess) - 1)) |
        (1 << (static_cast<unsigned>(maxAccess) - 1));
    assert(getDefaultAccessibility() == defaultAccess && "not enough bits");
    assert(getMaxAccessibility() == maxAccess && "not enough bits");
  }

  void setConformanceLoader(LazyMemberLoader *resolver, uint64_t contextData);

  DeclRange getMembers() const;

  /// Determine whether this is a constrained extension, which adds additional
  /// requirements beyond those of the nominal type.
  bool isConstrainedExtension() const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Extension;
  }
  static bool classof(const DeclContext *C) {
    return C->getContextKind() == DeclContextKind::ExtensionDecl;
  }
  static bool classof(const IterableDeclContext *C) {
    return C->getIterableContextKind() 
             == IterableDeclContextKind::ExtensionDecl;
  }

  using DeclContext::operator new;
};

/// \brief Iterator that walks the extensions of a particular type.
class ExtensionIterator {
  ExtensionDecl *current;

public:
  ExtensionIterator() : current() { }
  explicit ExtensionIterator(ExtensionDecl *current) : current(current) { }

  ExtensionDecl *operator*() const { return current; }
  ExtensionDecl *operator->() const { return current; }

  ExtensionIterator &operator++() {
    current = current->NextExtension.getPointer();
    return *this;
  }

  ExtensionIterator operator++(int) {
    ExtensionIterator tmp = *this;
    ++(*this);
    return tmp;
  }

  friend bool operator==(ExtensionIterator x, ExtensionIterator y) {
    return x.current == y.current;
  }

  friend bool operator!=(ExtensionIterator x, ExtensionIterator y) {
    return x.current != y.current;
  }
};

/// \brief Range that covers a set of extensions.
class ExtensionRange {
  ExtensionIterator first;
  ExtensionIterator last;

public:

  ExtensionRange(ExtensionIterator first, ExtensionIterator last)
    : first(first), last(last) { }

  typedef ExtensionIterator iterator;
  iterator begin() const { return first; }
  iterator end() const { return last; }
};
  
  
/// This represents one entry in a PatternBindingDecl, which are pairs of
/// Pattern and Initialization expression.  The pattern is always present, but
/// the initializer can be null if there is none.
class PatternBindingEntry {
  Pattern *ThePattern;

  enum class Flags {
    Checked = 1 << 0,
    Removed = 1 << 1
  };

  // When the initializer is removed we don't actually clear the pointer
  // because we might need to get initializer's source range. Since the
  // initializer is ASTContext-allocated it is safe.
  llvm::PointerIntPair<Expr *, 2, OptionSet<Flags>> InitCheckedAndRemoved;

  /// The initializer context used for this pattern binding entry.
  DeclContext *InitContext = nullptr;

  friend class PatternBindingInitializer;

public:
  PatternBindingEntry(Pattern *P, Expr *E, DeclContext *InitContext)
    : ThePattern(P), InitCheckedAndRemoved(E, {}), InitContext(InitContext) {}

  Pattern *getPattern() const { return ThePattern; }
  void setPattern(Pattern *P) { ThePattern = P; }
  Expr *getInit() const {
    return (InitCheckedAndRemoved.getInt().contains(Flags::Removed))
      ? nullptr : InitCheckedAndRemoved.getPointer();
  }
  SourceRange getOrigInitRange() const;
  void setInit(Expr *E);

  /// Retrieve the initializer as it was written in the source.
  Expr *getInitAsWritten() const { return InitCheckedAndRemoved.getPointer(); }

  bool isInitializerChecked() const {
    return InitCheckedAndRemoved.getInt().contains(Flags::Checked);
  }
  void setInitializerChecked() {
    InitCheckedAndRemoved.setInt(
      InitCheckedAndRemoved.getInt() | Flags::Checked);
  }

  // Return the first variable initialized by this pattern.
  VarDecl *getAnchoringVarDecl() const;

  // Retrieve the declaration context for the initializer.
  DeclContext *getInitContext() const { return InitContext; }

  /// Override the initializer context.
  void setInitContext(DeclContext *dc) { InitContext = dc; }

  /// Retrieve the source range covered by this pattern binding.
  ///
  /// \param omitAccessors Whether the computation should omit the accessors
  /// from the source range.
  SourceRange getSourceRange(bool omitAccessors = false) const;
};

/// \brief This decl contains a pattern and optional initializer for a set
/// of one or more VarDecls declared together.
///
/// For example, in
/// \code
///   var (a, b) = foo(), (c,d) = bar()
/// \endcode
///
/// this includes two entries in the pattern list.  The first contains the
/// pattern "(a, b)" and the initializer "foo()".  The second contains the
/// pattern "(c, d)" and the initializer "bar()".
///
class PatternBindingDecl final : public Decl,
    private llvm::TrailingObjects<PatternBindingDecl, PatternBindingEntry> {
  friend TrailingObjects;

  SourceLoc StaticLoc; ///< Location of the 'static/class' keyword, if present.
  SourceLoc VarLoc;    ///< Location of the 'var' keyword.

  friend class Decl;
  
  PatternBindingDecl(SourceLoc StaticLoc, StaticSpellingKind StaticSpelling,
                     SourceLoc VarLoc, unsigned NumPatternEntries,
                     DeclContext *Parent);

public:
  static PatternBindingDecl *create(ASTContext &Ctx, SourceLoc StaticLoc,
                                    StaticSpellingKind StaticSpelling,
                                    SourceLoc VarLoc,
                                    ArrayRef<PatternBindingEntry> PatternList,
                                    DeclContext *Parent);

  static PatternBindingDecl *create(ASTContext &Ctx, SourceLoc StaticLoc,
                                    StaticSpellingKind StaticSpelling,
                                    SourceLoc VarLoc,
                                    Pattern *Pat, Expr *E,
                                    DeclContext *Parent);

  static PatternBindingDecl *createDeserialized(
                               ASTContext &Ctx, SourceLoc StaticLoc,
                               StaticSpellingKind StaticSpelling,
                               SourceLoc VarLoc,
                               unsigned NumPatternEntries,
                               DeclContext *Parent);

  SourceLoc getStartLoc() const {
    return StaticLoc.isValid() ? StaticLoc : VarLoc;
  }
  SourceLoc getLoc() const { return VarLoc; }
  SourceRange getSourceRange() const;

  unsigned getNumPatternEntries() const {
    return PatternBindingDeclBits.NumPatternEntries;
  }
  
  ArrayRef<PatternBindingEntry> getPatternList() const {
    return const_cast<PatternBindingDecl*>(this)->getMutablePatternList();
  }

  Expr *getInit(unsigned i) const {
    return getPatternList()[i].getInit();
  }
  
  SourceRange getOrigInitRange(unsigned i) const {
    return getPatternList()[i].getOrigInitRange();
  }

  void setInit(unsigned i, Expr *E) {
    getMutablePatternList()[i].setInit(E);
  }

  Pattern *getPattern(unsigned i) const {
    return getPatternList()[i].getPattern();
  }
  
  void setPattern(unsigned i, Pattern *Pat, DeclContext *InitContext);

  /// Given that this PBD is the parent pattern for the specified VarDecl,
  /// return the entry of the VarDecl in our PatternList.  For example, in:
  ///
  ///   let (a,b) = foo(), (c,d) = bar()
  ///
  /// "a" and "b" will have index 0, since they correspond to the first pattern,
  /// and "c" and "d" will have index 1 since they correspond to the second one.
  unsigned getPatternEntryIndexForVarDecl(const VarDecl *VD) const;
  
  /// Return the PatternEntry (a pattern + initializer pair) for the specified
  /// VarDecl.
  PatternBindingEntry getPatternEntryForVarDecl(const VarDecl *VD) const {
    return getPatternList()[getPatternEntryIndexForVarDecl(VD)];
  }
  
  bool isInitializerChecked(unsigned i) const {
    return getPatternList()[i].isInitializerChecked();
  }

  void setInitializerChecked(unsigned i) {
    getMutablePatternList()[i].setInitializerChecked();
  }
  
  /// Does this binding declare something that requires storage?
  bool hasStorage() const;
  
  /// When the pattern binding contains only a single variable with no
  /// destructuring, retrieve that variable.
  VarDecl *getSingleVar() const;

  bool isStatic() const { return PatternBindingDeclBits.IsStatic; }
  void setStatic(bool s) { PatternBindingDeclBits.IsStatic = s; }
  SourceLoc getStaticLoc() const { return StaticLoc; }
  /// \returns the way 'static'/'class' was spelled in the source.
  StaticSpellingKind getStaticSpelling() const {
    return static_cast<StaticSpellingKind>(
        PatternBindingDeclBits.StaticSpelling);
  }
  /// \returns the way 'static'/'class' should be spelled for this declaration.
  StaticSpellingKind getCorrectStaticSpelling() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::PatternBinding;
  }
  
private:
  MutableArrayRef<PatternBindingEntry> getMutablePatternList() {
    // Pattern entries are tail allocated.
    return {getTrailingObjects<PatternBindingEntry>(), getNumPatternEntries()};
  }
};
  
/// TopLevelCodeDecl - This decl is used as a container for top-level
/// expressions and statements in the main module.  It is always a direct
/// child of a SourceFile.  The primary reason for building these is to give
/// top-level statements a DeclContext which is distinct from the file itself.
/// This, among other things, makes it easier to distinguish between local
/// top-level variables (which are not live past the end of the statement) and
/// global variables.
class TopLevelCodeDecl : public Decl, public DeclContext {
  BraceStmt *Body;

public:
  TopLevelCodeDecl(DeclContext *Parent, BraceStmt *Body = nullptr)
    : Decl(DeclKind::TopLevelCode, Parent),
      DeclContext(DeclContextKind::TopLevelCodeDecl, Parent),
      Body(Body) {}

  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *b) { Body = b; }

  SourceLoc getStartLoc() const;
  SourceLoc getLoc() const { return getStartLoc(); }
  SourceRange getSourceRange() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::TopLevelCode;
  }
  static bool classof(const DeclContext *C) {
    return C->getContextKind() == DeclContextKind::TopLevelCodeDecl;
  }
  
  using DeclContext::operator new;
};

/// SerializedTopLevelCodeDeclContext - This represents what was originally a
/// TopLevelCodeDecl during serialization. It is preserved only to maintain the
/// correct AST structure and remangling after deserialization.
class SerializedTopLevelCodeDeclContext : public SerializedLocalDeclContext {
public:
  SerializedTopLevelCodeDeclContext(DeclContext *Parent)
    : SerializedLocalDeclContext(LocalDeclContextKind::TopLevelCodeDecl,
                                 Parent) {}
  static bool classof(const DeclContext *DC) {
    if (auto LDC = dyn_cast<SerializedLocalDeclContext>(DC))
      return LDC->getLocalDeclContextKind() ==
        LocalDeclContextKind::TopLevelCodeDecl;
    return false;
  }
};

/// This represents one part of a #if block.  If the condition field is
/// non-null, then this represents a #if or a #elseif, otherwise it represents
/// an #else block.
struct IfConfigDeclClause {
  /// The location of the #if, #elseif, or #else keyword.
  SourceLoc Loc;
  
  /// The condition guarding this #if or #elseif block.  If this is null, this
  /// is a #else clause.
  Expr *Cond;

  ArrayRef<Decl*> Members;

  /// True if this is the active clause of the #if block.
  bool isActive;

  IfConfigDeclClause(SourceLoc Loc, Expr *Cond, ArrayRef<Decl*> Members,
                     bool isActive)
    : Loc(Loc), Cond(Cond), Members(Members), isActive(isActive) {}
};
  
  
/// IfConfigDecl - This class represents the declaration-side representation of
/// #if/#else/#endif blocks. Active and inactive block members are stored
/// separately, with the intention being that active members will be handed
/// back to the enclosing declaration.
class IfConfigDecl : public Decl {
  /// An array of clauses controlling each of the #if/#elseif/#else conditions.
  /// The array is ASTContext allocated.
  ArrayRef<IfConfigDeclClause> Clauses;
  SourceLoc EndLoc;
public:
  
  IfConfigDecl(DeclContext *Parent, ArrayRef<IfConfigDeclClause> Clauses,
               SourceLoc EndLoc, bool HadMissingEnd)
    : Decl(DeclKind::IfConfig, Parent), Clauses(Clauses), EndLoc(EndLoc)
  {
    IfConfigDeclBits.HadMissingEnd = HadMissingEnd;
  }

  ArrayRef<IfConfigDeclClause> getClauses() const { return Clauses; }

  /// Return the active clause, or null if there is no active one.
  const IfConfigDeclClause *getActiveClause() const {
    for (auto &Clause : Clauses)
      if (Clause.isActive) return &Clause;
    return nullptr;
  }

  const ArrayRef<Decl*> getActiveMembers() const {
    if (auto *Clause = getActiveClause())
      return Clause->Members;
    return {};
  }
  
  SourceLoc getEndLoc() const { return EndLoc; }
  SourceLoc getLoc() const { return Clauses[0].Loc; }

  bool hadMissingEnd() const { return IfConfigDeclBits.HadMissingEnd; }
  
  SourceRange getSourceRange() const;
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::IfConfig;
  }
};

/// ValueDecl - All named decls that are values in the language.  These can
/// have a type, etc.
class ValueDecl : public Decl {
  DeclName Name;
  SourceLoc NameLoc;
  llvm::PointerIntPair<Type, 3, OptionalEnum<Accessibility>> TypeAndAccess;

protected:
  ValueDecl(DeclKind K,
            llvm::PointerUnion<DeclContext *, ASTContext *> context,
            DeclName name, SourceLoc NameLoc)
    : Decl(K, context), Name(name), NameLoc(NameLoc) {
    ValueDeclBits.AlreadyInLookupTable = false;
    ValueDeclBits.CheckedRedeclaration = false;
  }

public:
  /// \brief Return true if this is a definition of a decl, not a forward
  /// declaration (e.g. of a function) that is implemented outside of the
  /// swift code.
  bool isDefinition() const;

  /// Determine whether we have already checked whether this
  /// declaration is a redeclaration.
  bool alreadyCheckedRedeclaration() const { 
    return ValueDeclBits.CheckedRedeclaration; 
  }

  /// Set whether we have already checked this declaration as a
  /// redeclaration.
  void setCheckedRedeclaration(bool checked) {
    ValueDeclBits.CheckedRedeclaration = checked;
  }

  bool hasName() const { return bool(Name); }
  /// TODO: Rename to getSimpleName?
  Identifier getName() const { return Name.getBaseName(); }
  bool isOperator() const { return Name.isOperator(); }

  /// Returns the string for the base name, or "_" if this is unnamed.
  StringRef getNameStr() const {
    return hasName() ? getName().str() : "_";
  }

  /// Retrieve the full name of the declaration.
  /// TODO: Rename to getName?
  DeclName getFullName() const { return Name; }
  void setName(DeclName name) { Name = name; }

  /// Retrieve the base name of the declaration, ignoring any argument
  /// names.
  DeclName getBaseName() const { return Name.getBaseName(); }

  /// Retrieve the name to use for this declaration when interoperating
  /// with the Objective-C runtime.
  ///
  /// \returns A "selector" containing the runtime name. For non-method
  /// entities (classes, protocols, properties), this operation will
  /// return a zero-parameter selector with the appropriate name in its
  /// first slot.
  Optional<ObjCSelector> getObjCRuntimeName() const;

  /// Determine whether the given declaration can infer @objc, or the
  /// Objective-C name, if used to satisfy the given requirement.
  bool canInferObjCFromRequirement(ValueDecl *requirement);

  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getLoc() const { return NameLoc; }

  bool hasAccessibility() const {
    return TypeAndAccess.getInt().hasValue();
  }

  /// \see getFormalAccess
  Accessibility getFormalAccessImpl(const DeclContext *useDC) const;

  /// Returns the access level specified explicitly by the user, or provided by
  /// default according to language rules.
  ///
  /// This is the access used when calculating if access control is being used
  /// consistently. If \p useDC is provided (the location where the value is
  /// being used), features that affect formal access such as \c \@testable are
  /// taken into account.
  ///
  /// \sa getFormalAccessScope
  Accessibility getFormalAccess(const DeclContext *useDC = nullptr) const {
    assert(hasAccessibility() && "accessibility not computed yet");
    Accessibility result = TypeAndAccess.getInt().getValue();
    if (useDC && (result == Accessibility::Internal ||
                  result == Accessibility::Public))
      return getFormalAccessImpl(useDC);
    return result;
  }

  /// Returns the outermost DeclContext from which this declaration can be
  /// accessed, or null if the declaration is public.
  ///
  /// This is used when calculating if access control is being used
  /// consistently. If \p useDC is provided (the location where the value is
  /// being used), features that affect formal access such as \c \@testable are
  /// taken into account.
  ///
  /// \invariant
  /// <code>value.isAccessibleFrom(value.getFormalAccessScope())</code>
  ///
  /// \sa getFormalAccess
  /// \sa isAccessibleFrom
  AccessScope
  getFormalAccessScope(const DeclContext *useDC = nullptr) const;

  /// Returns the access level that actually controls how a declaration should
  /// be emitted and may be used.
  ///
  /// This is the access used when making optimization and code generation
  /// decisions. It should not be used at the AST or semantic level.
  Accessibility getEffectiveAccess() const;

  void setAccessibility(Accessibility access) {
    assert(!hasAccessibility() && "accessibility already set");
    overwriteAccessibility(access);
  }

  /// Overwrite the accessibility of this declaration.
  // This is needed in the LLDB REPL.
  void overwriteAccessibility(Accessibility access) {
    TypeAndAccess.setInt(access);
  }

  /// Returns true if this declaration is accessible from the given context.
  ///
  /// A private declaration is accessible from any DeclContext within the same
  /// source file.
  ///
  /// An internal declaration is accessible from any DeclContext within the same
  /// module.
  ///
  /// A public declaration is accessible everywhere.
  ///
  /// If \p DC is null, returns true only if this declaration is public.
  bool isAccessibleFrom(const DeclContext *DC) const;

  /// Retrieve the "interface" type of this value, which uses
  /// GenericTypeParamType if the declaration is generic. For a generic
  /// function, this will have a GenericFunctionType with a
  /// GenericSignature inside the type.
  Type getInterfaceType() const;
  bool hasInterfaceType() const;

  /// Set the interface type for the given value.
  void setInterfaceType(Type type);

  bool hasValidSignature() const {
    return hasInterfaceType() && !isBeingValidated();
  }

  /// isSettable - Determine whether references to this decl may appear
  /// on the left-hand side of an assignment or as the operand of a
  /// `&` or 'inout' operator.
  bool isSettable(const DeclContext *UseDC,
                  const DeclRefExpr *base = nullptr) const;
  
  /// isInstanceMember - Determine whether this value is an instance member
  /// of an enum or protocol.
  bool isInstanceMember() const;

  /// needsCapture - Check whether referring to this decl from a nested
  /// function requires capturing it.
  bool needsCapture() const;

  /// Retrieve the context discriminator for this local value, which
  /// is the index of this declaration in the sequence of
  /// discriminated declarations with the same name in the current
  /// context.  Only local functions and variables with getters and
  /// setters have discriminators.
  unsigned getLocalDiscriminator() const;
  void setLocalDiscriminator(unsigned index);

  /// Retrieve the declaration that this declaration overrides, if any.
  ValueDecl *getOverriddenDecl() const;

  /// Compute the overload signature for this declaration.
  OverloadSignature getOverloadSignature() const;

  /// Returns true if the decl requires Objective-C interop.
  ///
  /// This can be true even if there is no 'objc' attribute on the declaration.
  /// In that case it was inferred by the type checker and set with a call to
  /// markAsObjC().
  bool isObjC() const {
    return getAttrs().hasAttribute<ObjCAttr>();
  }
  
  void setIsObjC(bool Value);

  /// Is this declaration marked with 'final'?
  bool isFinal() const {
    return getAttrs().hasAttribute<FinalAttr>();
  }

  /// Is this declaration marked with 'dynamic'?
  bool isDynamic() const {
    return getAttrs().hasAttribute<DynamicAttr>();
  }

  /// Returns true if this decl can be found by id-style dynamic lookup.
  bool canBeAccessedByDynamicLookup() const;

  /// Returns the protocol requirements that this decl conforms to.
  ArrayRef<ValueDecl *>
  getSatisfiedProtocolRequirements(bool Sorted = false) const;

  /// Determines the kind of access that should be performed by a
  /// DeclRefExpr or MemberRefExpr use of this value in the specified
  /// context.
  AccessSemantics getAccessSemanticsFromContext(const DeclContext *DC) const;

  /// Print a reference to the given declaration.
  std::string printRef() const;

  /// Dump a reference to the given declaration.
  void dumpRef(raw_ostream &os) const;

  /// Dump a reference to the given declaration.
  void dumpRef() const;

  /// Returns true if the declaration is a static member of a type.
  ///
  /// This is not necessarily the opposite of "isInstanceMember()". Both
  /// predicates will be false for declarations that either categorically
  /// can't be "static" or are in a context where "static" doesn't make sense.
  bool isStatic() const;

  /// Retrieve the location at which we should insert a new attribute or
  /// modifier.
  SourceLoc getAttributeInsertionLoc(bool forModifier) const;

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_ValueDecl &&
           D->getKind() <= DeclKind::Last_ValueDecl;
  }
  
  /// True if this is a C function that was imported as a member of a type in
  /// Swift.
  bool isImportAsMember() const;
};

/// This is a common base class for declarations which declare a type.
class TypeDecl : public ValueDecl {
  MutableArrayRef<TypeLoc> Inherited;

protected:
  TypeDecl(DeclKind K, llvm::PointerUnion<DeclContext *, ASTContext *> context,
           Identifier name, SourceLoc NameLoc,
           MutableArrayRef<TypeLoc> inherited) :
    ValueDecl(K, context, name, NameLoc), Inherited(inherited)
  {
    TypeDeclBits.CheckedInheritanceClause = false;
  }

public:
  /// The type of this declaration's values. For the type of the
  /// declaration itself, use getInterfaceType(), which returns a
  /// metatype.
  Type getDeclaredInterfaceType() const;

  /// \brief Retrieve the set of protocols that this type inherits (i.e,
  /// explicitly conforms to).
  MutableArrayRef<TypeLoc> getInherited() { return Inherited; }
  ArrayRef<TypeLoc> getInherited() const { return Inherited; }

  /// Whether we already type-checked the inheritance clause.
  bool checkedInheritanceClause() const {
    return TypeDeclBits.CheckedInheritanceClause;
  }

  /// Note that we have already type-checked the inheritance clause.
  void setCheckedInheritanceClause(bool checked = true) {
    TypeDeclBits.CheckedInheritanceClause = checked;
  }

  void setInherited(MutableArrayRef<TypeLoc> i) { Inherited = i; }

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_TypeDecl &&
           D->getKind() <= DeclKind::Last_TypeDecl;
  }
};

/// A type declaration that can have generic parameters attached to it.  Because
/// it has these generic parameters, it is always a DeclContext.
class GenericTypeDecl : public TypeDecl, public DeclContext {
  GenericParamList *GenericParams = nullptr;

  /// The generic signature or environment of this type.
  ///
  /// When this function stores only a signature, the generic environment
  /// will be lazily loaded.
  mutable llvm::PointerUnion<GenericSignature *, GenericEnvironment *>
    GenericSigOrEnv;

  /// Lazily populate the generic environment.
  GenericEnvironment *getLazyGenericEnvironmentSlow() const;

public:
  GenericTypeDecl(DeclKind K, DeclContext *DC,
                  Identifier name, SourceLoc nameLoc,
                  MutableArrayRef<TypeLoc> inherited,
                  GenericParamList *GenericParams);

  GenericParamList *getGenericParams() const { return GenericParams; }

  /// Provide the set of parameters to a generic type, or null if
  /// this function is not generic.
  void setGenericParams(GenericParamList *params);

  /// Retrieve the innermost generic parameter types.
  ArrayRef<GenericTypeParamType *> getInnermostGenericParamTypes() const {
    if (auto sig = getGenericSignature())
      return sig->getInnermostGenericParams();
    else
      return { };
  }

  /// Retrieve the generic requirements.
  ArrayRef<Requirement> getGenericRequirements() const {
    if (auto sig = getGenericSignature())
      return sig->getRequirements();
    else
      return { };
  }

  /// Retrieve the generic signature for this type.
  GenericSignature *getGenericSignature() const;

  /// Retrieve the generic context for this type.
  GenericEnvironment *getGenericEnvironment() const;

  /// Set a lazy generic environment.
  void setLazyGenericEnvironment(LazyMemberLoader *lazyLoader,
                                 GenericSignature *genericSig,
                                 uint64_t genericEnvData);

  /// Set the generic context of this function.
  void setGenericEnvironment(GenericEnvironment *genericEnv);

  // Resolve ambiguity due to multiple base classes.
  using TypeDecl::getASTContext;
  using DeclContext::operator new;
  using TypeDecl::getDeclaredInterfaceType;

  static bool classof(const DeclContext *C) {
    return C->getContextKind() == DeclContextKind::GenericTypeDecl;
  }
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_GenericTypeDecl &&
           D->getKind() <= DeclKind::Last_GenericTypeDecl;
  }
};



/// TypeAliasDecl - This is a declaration of a typealias, for example:
///
///    typealias Foo = Int
///
/// TypeAliasDecl's always have 'MetatypeType' type.
///
class TypeAliasDecl : public GenericTypeDecl {
  SourceLoc TypeAliasLoc;           // The location of the 'typealias' keyword
  TypeLoc UnderlyingTy;

public:
  TypeAliasDecl(SourceLoc TypeAliasLoc, Identifier Name,
                SourceLoc NameLoc, GenericParamList *GenericParams,
                DeclContext *DC);

  SourceLoc getStartLoc() const { return TypeAliasLoc; }
  SourceRange getSourceRange() const;

  TypeLoc &getUnderlyingTypeLoc() {
    return UnderlyingTy;
  }
  const TypeLoc &getUnderlyingTypeLoc() const {
    return UnderlyingTy;
  }

  /// Set the underlying type, for deserialization and synthesized
  /// aliases.
  void setUnderlyingType(Type type);

  bool hasCompletedValidation() const {
    return TypeAliasDeclBits.HasCompletedValidation;
  }

  void setHasCompletedValidation() {
    TypeAliasDeclBits.HasCompletedValidation = 1;
  }

  /// For generic typealiases, return the unbound generic type.
  UnboundGenericType *getUnboundGenericType() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::TypeAlias;
  }
  static bool classof(const GenericTypeDecl *D) {
    return D->getKind() == DeclKind::TypeAlias;
  }
  static bool classof(const DeclContext *C) {
    auto GTD = dyn_cast<GenericTypeDecl>(C);
    return GTD && classof(GTD);
  }
};

/// Abstract class describing generic type parameters and associated types,
/// whose common purpose is to anchor the abstract type parameter and specify
/// requirements for any corresponding type argument.
class AbstractTypeParamDecl : public TypeDecl {
protected:
  AbstractTypeParamDecl(DeclKind kind, DeclContext *dc, Identifier name,
                        SourceLoc NameLoc)
    : TypeDecl(kind, dc, name, NameLoc, { }) { }

public:
  /// Return the superclass of the generic parameter.
  Type getSuperclass() const;

  /// Retrieve the set of protocols to which this abstract type
  /// parameter conforms.
  ArrayRef<ProtocolDecl *> getConformingProtocols() const;

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_AbstractTypeParamDecl &&
           D->getKind() <= DeclKind::Last_AbstractTypeParamDecl;
  }
};

/// A declaration of a generic type parameter.
///
/// A generic type parameter introduces a new, named type parameter along
/// with some set of requirements on any type argument used to realize this
/// type parameter. The requirements involve conformances to specific
/// protocols or inheritance from a specific class type.
///
/// In the following example, 'T' is a generic type parameter with the
/// requirement that the type argument conform to the 'Comparable' protocol.
///
/// \code
/// func min<T : Comparable>(x : T, y : T) -> T { ... }
/// \endcode
class GenericTypeParamDecl : public AbstractTypeParamDecl {
  unsigned Depth : 16;
  unsigned Index : 16;

public:
  /// Construct a new generic type parameter.
  ///
  /// \param dc The DeclContext in which the generic type parameter's owner
  /// occurs. This should later be overwritten with the actual declaration
  /// context that owns the type parameter.
  ///
  /// \param name The name of the generic parameter.
  /// \param nameLoc The location of the name.
  GenericTypeParamDecl(DeclContext *dc, Identifier name, SourceLoc nameLoc,
                       unsigned depth, unsigned index);

  /// The depth of this generic type parameter, i.e., the number of outer
  /// levels of generic parameter lists that enclose this type parameter.
  ///
  /// \code
  /// struct X<T> {
  ///   func f<U>() { }
  /// }
  /// \endcode
  ///
  /// Here 'T' has depth 0 and 'U' has depth 1. Both have index 0.
  unsigned getDepth() const { return Depth; }

  /// Set the depth of this generic type parameter.
  ///
  /// \sa getDepth
  void setDepth(unsigned depth) { Depth = depth; }

  /// The index of this generic type parameter within its generic parameter
  /// list.
  ///
  /// \code
  /// struct X<T, U> {
  ///   func f<V>() { }
  /// }
  /// \endcode
  ///
  /// Here 'T' and 'U' have indexes 0 and 1, respectively. 'V' has index 0.
  unsigned getIndex() const { return Index; }

  SourceLoc getStartLoc() const { return getNameLoc(); }
  SourceRange getSourceRange() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::GenericTypeParam;
  }
};

/// A declaration of an associated type.
///
/// An associated type introduces a new, named type in a protocol that
/// can vary from one conforming type to the next. Associated types have a
/// set of requirements to which the type that replaces it much realize,
/// described via conformance to specific protocols, or inheritance from a
/// specific class type.
///
/// In the following example, 'Element' is an associated type with no
/// requirements.
///
/// \code
/// protocol Enumerator {
///   typealias Element
///   func getNext() -> Element?
/// }
/// \endcode
///
/// Every protocol has an implicitly-created associated type 'Self' that
/// describes a type that conforms to the protocol.
class AssociatedTypeDecl : public AbstractTypeParamDecl {
  /// The location of the initial keyword.
  SourceLoc KeywordLoc;

  /// The default definition.
  TypeLoc DefaultDefinition;

  LazyMemberLoader *Resolver = nullptr;
  uint64_t ResolverContextData;

public:
  AssociatedTypeDecl(DeclContext *dc, SourceLoc keywordLoc, Identifier name,
                     SourceLoc nameLoc, TypeLoc defaultDefinition);
  AssociatedTypeDecl(DeclContext *dc, SourceLoc keywordLoc, Identifier name,
                     SourceLoc nameLoc, LazyMemberLoader *definitionResolver,
                     uint64_t resolverData);

  /// Get the protocol in which this associated type is declared.
  ProtocolDecl *getProtocol() const {
    return cast<ProtocolDecl>(getDeclContext());
  }

  /// Retrieve the default definition type.
  Type getDefaultDefinitionType() const {
    return getDefaultDefinitionLoc().getType();
  }

  TypeLoc &getDefaultDefinitionLoc();
  const TypeLoc &getDefaultDefinitionLoc() const {
    return const_cast<AssociatedTypeDecl *>(this)->getDefaultDefinitionLoc();
  }

  /// computeType - Compute the type (and declared type) of this associated
  /// type; can only be called after the alias type has been resolved.
  void computeType();

  SourceLoc getStartLoc() const { return KeywordLoc; }
  SourceRange getSourceRange() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::AssociatedType;
  }
};

class MemberLookupTable;
class ConformanceLookupTable;

/// Kinds of optional types.
enum OptionalTypeKind : unsigned {
  /// The type is not an optional type.
  OTK_None = 0,

  /// The type is Optional<T>.
  OTK_Optional,

  /// The type is ImplicitlyUnwrappedOptional<T>.
  OTK_ImplicitlyUnwrappedOptional
};
enum { NumOptionalTypeKinds = 2 };
  
// Kinds of pointer types.
enum PointerTypeKind : unsigned {
  PTK_UnsafeMutableRawPointer,
  PTK_UnsafeRawPointer,
  PTK_UnsafeMutablePointer,
  PTK_UnsafePointer,
  PTK_AutoreleasingUnsafeMutablePointer,
};

static inline bool isRawPointerKind(PointerTypeKind PTK) {
  switch (PTK) {
  case PTK_UnsafeMutableRawPointer:
  case PTK_UnsafeRawPointer:
    return true;
  case PTK_UnsafeMutablePointer:
  case PTK_UnsafePointer:
  case PTK_AutoreleasingUnsafeMutablePointer:
    return false;
  }

  llvm_unreachable("Unhandled PointerTypeKind in switch.");
}

/// NominalTypeDecl - a declaration of a nominal type, like a struct.
class NominalTypeDecl : public GenericTypeDecl, public IterableDeclContext {
  SourceRange Braces;

  /// \brief The first extension of this type.
  ExtensionDecl *FirstExtension = nullptr;

  /// \brief The last extension of this type, used solely for efficient
  /// insertion of new extensions.
  ExtensionDecl *LastExtension = nullptr;

  /// \brief The generation at which we last loaded extensions.
  unsigned ExtensionGeneration;
  
  /// Prepare to traverse the list of extensions.
  void prepareExtensions();

  /// Retrieve the conformance loader (if any), and removing it in the
  /// same operation. The caller is responsible for loading the
  /// conformances.
  std::pair<LazyMemberLoader *, uint64_t> takeConformanceLoader() {
    if (!NominalTypeDeclBits.HasLazyConformances)
      return { nullptr, 0 };

    return takeConformanceLoaderSlow();
  }

  /// Slow path for \c takeConformanceLoader().
  std::pair<LazyMemberLoader *, uint64_t> takeConformanceLoaderSlow();

  /// \brief A lookup table containing all of the members of this type and
  /// its extensions.
  ///
  /// The table itself is lazily constructed and updated when
  /// lookupDirect() is called. The bit indicates whether the lookup
  /// table has already added members by walking the declarations in
  /// scope.
  llvm::PointerIntPair<MemberLookupTable *, 1, bool> LookupTable;

  /// Prepare the lookup table to make it ready for lookups.
  void prepareLookupTable(bool ignoreNewExtensions);

  /// Note that we have added a member into the iterable declaration context,
  /// so that it can also be added to the lookup table (if needed).
  void addedMember(Decl *member);

  /// \brief A lookup table used to find the protocol conformances of
  /// a given nominal type.
  mutable ConformanceLookupTable *ConformanceTable = nullptr;

  /// Prepare the conformance table.
  void prepareConformanceTable() const;

  /// Returns the protocol requirements that \c Member conforms to.
  ArrayRef<ValueDecl *>
    getSatisfiedProtocolRequirementsForMember(const ValueDecl *Member,
                                              bool Sorted) const;

  friend class ASTContext;
  friend class MemberLookupTable;
  friend class ConformanceLookupTable;
  friend class ExtensionDecl;
  friend class DeclContext;
  friend class IterableDeclContext;
  friend ArrayRef<ValueDecl *>
           ValueDecl::getSatisfiedProtocolRequirements(bool Sorted) const;

protected:
  Type DeclaredTy;
  Type DeclaredTyInContext;
  Type DeclaredInterfaceTy;

  NominalTypeDecl(DeclKind K, DeclContext *DC, Identifier name,
                  SourceLoc NameLoc,
                  MutableArrayRef<TypeLoc> inherited,
                  GenericParamList *GenericParams) :
    GenericTypeDecl(K, DC, name, NameLoc, inherited, GenericParams),
    IterableDeclContext(IterableDeclContextKind::NominalTypeDecl)
  {
    setGenericParams(GenericParams);
    NominalTypeDeclBits.HasDelayedMembers = false;
    NominalTypeDeclBits.AddedImplicitInitializers = false;
    ExtensionGeneration = 0;
    NominalTypeDeclBits.SearchedForFailableInits = false;
    NominalTypeDeclBits.HasFailableInits = false;
    NominalTypeDeclBits.HasLazyConformances = false;
  }

  friend class ProtocolType;

public:
  using GenericTypeDecl::getASTContext;

  DeclRange getMembers() const;
  SourceRange getBraces() const { return Braces; }
  
  void setBraces(SourceRange braces) { Braces = braces; }

  /// \brief Does this declaration expose a fixed layout to all resilience
  /// domains?
  ///
  /// For structs, this means clients can assume the number and order of
  /// stored properties will not change.
  ///
  /// For enums, this means clients can assume the number and order of
  /// cases will not change.
  bool hasFixedLayout() const;

  /// \brief Does this declaration expose a fixed layout to the given
  /// module?
  bool hasFixedLayout(ModuleDecl *M, ResilienceExpansion expansion) const;

  /// \brief Returns true if this decl contains delayed value or protocol
  /// declarations.
  bool hasDelayedMembers() const {
    return NominalTypeDeclBits.HasDelayedMembers;
  }
  
  /// \brief Mark this declaration as having delayed members or not.
  void setHasDelayedMembers(bool hasDelayedMembers = true) {
    NominalTypeDeclBits.HasDelayedMembers = hasDelayedMembers;
  }

  /// Determine whether we have already attempted to add any
  /// implicitly-defined initializers to this declaration.
  bool addedImplicitInitializers() const {
    return NominalTypeDeclBits.AddedImplicitInitializers;
  }

  /// Note that we have attempted to
  void setAddedImplicitInitializers() {
    NominalTypeDeclBits.AddedImplicitInitializers = true;
  }
              
  bool getHasFailableInits() const {
    return NominalTypeDeclBits.HasFailableInits;
  }
  void setHasFailableInits(bool failable = true) {
    NominalTypeDeclBits.HasFailableInits = failable;
  }
  void setSearchedForFailableInits(bool searched = true) {
    NominalTypeDeclBits.SearchedForFailableInits = searched;
  }
  bool getSearchedForFailableInits() const {
    return NominalTypeDeclBits.SearchedForFailableInits;
  }

  /// Compute the type of this nominal type.
  void computeType();

  /// getDeclaredType - Retrieve the type declared by this entity, without
  /// any generic parameters bound if this is a generic type.
  Type getDeclaredType() const;

  /// getDeclaredTypeInContext - Retrieve the type declared by this entity, with
  /// context archetypes bound if this is a generic type.
  Type getDeclaredTypeInContext() const;

  /// getDeclaredInterfaceType - Retrieve the type declared by this entity, with
  /// generic parameters bound if this is a generic type.
  Type getDeclaredInterfaceType() const;

  /// \brief Add a new extension to this nominal type.
  void addExtension(ExtensionDecl *extension);

  /// \brief Retrieve the set of extensions of this type.
  ExtensionRange getExtensions();

  /// Make a member of this nominal type, or one of its extensions,
  /// immediately visible in the lookup table.
  ///
  /// A member of a nominal type or extension thereof will become
  /// visible to name lookup as soon as it is added. However, if the
  /// addition of a member is delayed---for example, because it's
  /// being introduced in response to name lookup---this method can be
  /// called to make it immediately visible.
  void makeMemberVisible(ValueDecl *member);

  /// Find all of the declarations with the given name within this nominal type
  /// and its extensions.
  ///
  /// This routine does not look into superclasses, nor does it consider
  /// protocols to which the nominal type conforms. Furthermore, the resulting
  /// set of declarations has not been filtered for visibility, nor have
  /// overridden declarations been removed.
  ///
  /// \param ignoreNewExtensions Whether to avoid loading any new extension.
  /// Used by the module loader to break recursion.
  ArrayRef<ValueDecl *> lookupDirect(DeclName name,
                                     bool ignoreNewExtensions = false);

  /// Collect the set of protocols to which this type should implicitly
  /// conform, such as AnyObject (for classes).
  void getImplicitProtocols(SmallVectorImpl<ProtocolDecl *> &protocols);

  /// Look for conformances of this nominal type to the given
  /// protocol.
  ///
  /// \param module The module from which we initiate the search.
  /// FIXME: This is currently unused.
  ///
  /// \param protocol The protocol whose conformance is requested.
  /// \param conformances Will be populated with the set of protocol
  /// conformances found for this protocol.
  ///
  /// \returns true if any conformances were found. 
  bool lookupConformance(
         ModuleDecl *module, ProtocolDecl *protocol,
         SmallVectorImpl<ProtocolConformance *> &conformances) const;

  /// Retrieve all of the protocols that this nominal type conforms to.
  SmallVector<ProtocolDecl *, 2> getAllProtocols() const;

  /// Retrieve all of the protocol conformances for this nominal type.
  SmallVector<ProtocolConformance *, 2> getAllConformances(
                                          bool sorted = false) const;

  /// Register an externally-created protocol conformance in the
  /// conformance lookup table.
  ///
  /// This is used by deserialization of module files to report
  /// conformances.
  void registerProtocolConformance(ProtocolConformance *conformance);

  /// \brief True if the type can implicitly derive a conformance for the given
  /// protocol.
  ///
  /// If true, explicit conformance checking will synthesize implicit
  /// declarations for requirements of the protocol that are not satisfied by
  /// the type's explicit members.
  bool derivesProtocolConformance(ProtocolDecl *protocol) const;

  void setConformanceLoader(LazyMemberLoader *resolver, uint64_t contextData);

  /// classifyAsOptionalType - Decide whether this declaration is one
  /// of the library-intrinsic Optional<T> or ImplicitlyUnwrappedOptional<T> types.
  OptionalTypeKind classifyAsOptionalType() const;
  
private:
  /// Predicate used to filter StoredPropertyRange.
  struct ToStoredProperty {
    Optional<VarDecl *> operator()(Decl *decl) const;
  };
  
public:
  /// A range for iterating the stored member variables of a structure.
  using StoredPropertyRange = OptionalTransformRange<DeclRange,
                                                     ToStoredProperty>;

  /// Return a collection of the stored member variables of this type.
  StoredPropertyRange getStoredProperties() const {
    return StoredPropertyRange(getMembers(), ToStoredProperty());
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_NominalTypeDecl &&
           D->getKind() <= DeclKind::Last_NominalTypeDecl;
  }
  static bool classof(const GenericTypeDecl *D) {
    return D->getKind() >= DeclKind::First_NominalTypeDecl &&
           D->getKind() <= DeclKind::Last_NominalTypeDecl;
  }

  static bool classof(const DeclContext *C) {
    auto GTD = dyn_cast<GenericTypeDecl>(C);
    return GTD && classof(GTD);
  }
  static bool classof(const IterableDeclContext *C) {
    return C->getIterableContextKind()
             == IterableDeclContextKind::NominalTypeDecl;
  }
  static bool classof(const NominalTypeDecl *D) { return true; }
  static bool classof(const ExtensionDecl *D) { return false; }
};

/// \brief This is the declaration of an enum.
///
/// For example:
///
/// \code
///    enum Bool {
///      case false
///      case true
///    }
///
///    enum Optional<T> {
///      case none
///      case some(T)
///    }
/// \endcode
///
/// The type of the decl itself is a MetatypeType; use getDeclaredType()
/// to get the declared type ("Bool" or "Optional" in the above example).
class EnumDecl : public NominalTypeDecl {
  SourceLoc EnumLoc;

  struct {
    /// The raw type and a bit to indicate whether the
    /// raw was computed yet or not.
    llvm::PointerIntPair<Type, 1, bool> RawType;
  } LazySemanticInfo;

  friend class IterativeTypeChecker;

public:
  EnumDecl(SourceLoc EnumLoc, Identifier Name, SourceLoc NameLoc,
            MutableArrayRef<TypeLoc> Inherited,
            GenericParamList *GenericParams, DeclContext *DC);

  SourceLoc getStartLoc() const { return EnumLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(EnumLoc, getBraces().End);
  }

  EnumElementDecl *getElement(Identifier Name) const;
  
public:
  /// A range for iterating the elements of an enum.
  using ElementRange = DowncastFilterRange<EnumElementDecl, DeclRange>;

  /// A range for iterating the cases of an enum.
  using CaseRange = DowncastFilterRange<EnumCaseDecl, DeclRange>;

  /// Return a range that iterates over all the elements of an enum.
  ElementRange getAllElements() const {
    return ElementRange(getMembers());
  }
  
  /// Return a range that iterates over all the cases of an enum.
  CaseRange getAllCases() const {
    return CaseRange(getMembers());
  }

  /// Insert all of the 'case' element declarations into a DenseSet.
  void getAllElements(llvm::DenseSet<EnumElementDecl*> &elements) const {
    for (auto elt : getAllElements())
      elements.insert(elt);
  }
  
  /// Retrieve the status of circularity checking for class inheritance.
  CircularityCheck getCircularityCheck() const {
    return static_cast<CircularityCheck>(EnumDeclBits.Circularity);
  }
  
  /// Record the current stage of circularity checking.
  void setCircularityCheck(CircularityCheck circularity) {
    EnumDeclBits.Circularity = static_cast<unsigned>(circularity);
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Enum;
  }
  static bool classof(const GenericTypeDecl *D) {
    return D->getKind() == DeclKind::Enum;
  }
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Enum;
  }
  static bool classof(const DeclContext *C) {
    auto GTD = dyn_cast<GenericTypeDecl>(C);
    return GTD && classof(static_cast<const Decl*>(GTD));
  }
  static bool classof(const IterableDeclContext *C) {
    auto NTD = dyn_cast<NominalTypeDecl>(C);
    return NTD && classof(NTD);
  }
  
  /// Determine whether this enum declares a raw type in its inheritance clause.
  bool hasRawType() const {
    return (bool)LazySemanticInfo.RawType.getPointer();
  }
  /// Retrieve the declared raw type of the enum from its inheritance clause,
  /// or null if it has none.
  Type getRawType() const {
    return LazySemanticInfo.RawType.getPointer();
  }

  /// Set the raw type of the enum from its inheritance clause.
  void setRawType(Type rawType) {
    LazySemanticInfo.RawType.setPointerAndInt(rawType, true);
  }
  
  /// True if the enum has cases, and none of those cases have associated values.
  ///
  /// Note that this is false for enums with absolutely no cases.
  bool hasOnlyCasesWithoutAssociatedValues() const;

  /// True if the enum is marked 'indirect'.
  bool isIndirect() const {
    return getAttrs().hasAttribute<IndirectAttr>();
  }
};

/// StructDecl - This is the declaration of a struct, for example:
///
///    struct Complex { var R : Double, I : Double }
///
/// The type of the decl itself is a MetatypeType; use getDeclaredType()
/// to get the declared type ("Complex" in the above example).
class StructDecl : public NominalTypeDecl {
  SourceLoc StructLoc;

public:
  StructDecl(SourceLoc StructLoc, Identifier Name, SourceLoc NameLoc,
             MutableArrayRef<TypeLoc> Inherited,
             GenericParamList *GenericParams, DeclContext *DC);

  SourceLoc getStartLoc() const { return StructLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(StructLoc, getBraces().End);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Struct;
  }
  static bool classof(const GenericTypeDecl *D) {
    return D->getKind() == DeclKind::Struct;
  }
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Struct;
  }
  static bool classof(const DeclContext *C) {
    auto GTD = dyn_cast<GenericTypeDecl>(C);
    return GTD && classof(static_cast<const Decl*>(GTD));
  }
  static bool classof(const IterableDeclContext *C) {
    auto NTD = dyn_cast<NominalTypeDecl>(C);
    return NTD && classof(NTD);
  }

  /// Does this struct contain unreferenceable storage, such as C fields that
  /// cannot be represented in Swift?
  bool hasUnreferenceableStorage() const {
    return StructDeclBits.HasUnreferenceableStorage;
  }

  void setHasUnreferenceableStorage(bool v) {
    StructDeclBits.HasUnreferenceableStorage = v;
  }
};

/// The kind of artificial main to generate for a class.
enum class ArtificialMainKind : uint8_t {
  NSApplicationMain,
  UIApplicationMain,
};

enum class ObjCClassKind : uint8_t {
  /// Neither the class nor any of its superclasses are @objc.
  NonObjC,
  /// One of the superclasses is @objc but another superclass or the
  /// class itself has generic parameters, so while it cannot be
  /// directly represented in Objective-C, it has implicitly @objc
  /// members.
  ObjCMembers,
  /// The top-level ancestor of this class is not @objc, but the
  /// class itself is.
  ObjCWithSwiftRoot,
  /// The class is bona-fide @objc.
  ObjC,
};

/// ClassDecl - This is the declaration of a class, for example:
///
///    class Complex { var R : Double, I : Double }
///
/// The type of the decl itself is a MetatypeType; use getDeclaredType()
/// to get the declared type ("Complex" in the above example).
class ClassDecl : public NominalTypeDecl {
  class ObjCMethodLookupTable;

  SourceLoc ClassLoc;
  ObjCMethodLookupTable *ObjCMethodLookup = nullptr;

  /// Create the Objective-C member lookup table.
  void createObjCMethodLookup();

  struct {
    /// The superclass type and a bit to indicate whether the
    /// superclass was computed yet or not.
    llvm::PointerIntPair<Type, 1, bool> Superclass;
  } LazySemanticInfo;

  friend class IterativeTypeChecker;

public:
  ClassDecl(SourceLoc ClassLoc, Identifier Name, SourceLoc NameLoc,
            MutableArrayRef<TypeLoc> Inherited,
            GenericParamList *GenericParams, DeclContext *DC);

  SourceLoc getStartLoc() const { return ClassLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(ClassLoc, getBraces().End);
  }

  /// Determine whether this class has a superclass.
  bool hasSuperclass() const { return (bool)getSuperclass(); }

  /// Retrieve the superclass of this class, or null if there is no superclass.
  Type getSuperclass() const { return LazySemanticInfo.Superclass.getPointer(); }

  /// Retrieve the ClassDecl for the superclass of this class, or null if there
  /// is no superclass.
  ClassDecl *getSuperclassDecl() const;

  /// Set the superclass of this class.
  void setSuperclass(Type superclass);

  /// Retrieve the status of circularity checking for class inheritance.
  CircularityCheck getCircularityCheck() const {
    return static_cast<CircularityCheck>(ClassDeclBits.Circularity);
  }

  /// Record the current stage of circularity checking.
  void setCircularityCheck(CircularityCheck circularity) {
    ClassDeclBits.Circularity = static_cast<unsigned>(circularity);
  }

  //// Whether this class requires all of its stored properties to
  //// have initializers in the class definition.
  bool requiresStoredPropertyInits() const { 
    return ClassDeclBits.RequiresStoredPropertyInits;
  }

  /// Set whether this class requires all of its stored properties to
  /// have initializers in the class definition.
  void setRequiresStoredPropertyInits(bool requiresInits) {
    ClassDeclBits.RequiresStoredPropertyInits = requiresInits;
  }

  /// \see getForeignClassKind
  enum class ForeignKind : uint8_t {
    /// A normal Swift or Objective-C class.
    Normal = 0,
    /// An imported Core Foundation type. These are AnyObject-compatible but
    /// do not have runtime metadata.
    CFType,
    /// An imported Objective-C type whose class and metaclass symbols are not
    /// both available at link-time but can be accessed through the Objective-C
    /// runtime.
    RuntimeOnly
  };

  /// Whether this class is "foreign", meaning that it is implemented
  /// by a runtime that Swift does not have first-class integration
  /// with.  This generally means that:
  ///   - class data is either abstracted or cannot be made to
  ///     fit with Swift's metatype schema, and/or
  ///   - there is no facility for subclassing or adding polymorphic
  ///     methods to the class.
  ///
  /// We may find ourselves wanting to break this bit into more
  /// precise chunks later.
  ForeignKind getForeignClassKind() const {
    return static_cast<ForeignKind>(ClassDeclBits.RawForeignKind);
  }
  void setForeignClassKind(ForeignKind kind) {
    ClassDeclBits.RawForeignKind = static_cast<unsigned>(kind);
  }

  /// Returns true if this class is any kind of "foreign class".
  ///
  /// \see getForeignClassKind
  bool isForeign() const {
    return getForeignClassKind() != ForeignKind::Normal;
  }

  /// Find a method of a class that overrides a given method.
  /// Return nullptr, if no such method exists.
  AbstractFunctionDecl *findOverridingDecl(
      const AbstractFunctionDecl *method) const;

  /// Find a method implementation which will be used when a given method
  /// is invoked on an instance of this class. This implementation may stem
  /// either from a class itself or its direct or indirect superclasses.
  AbstractFunctionDecl *findImplementingMethod(
      const AbstractFunctionDecl *method) const;

  /// True if the class has a destructor.
  ///
  /// Fully type-checked classes always contain destructors, but during parsing
  /// or type-checking, the implicit destructor may not have been synthesized
  /// yet if one was not explicitly declared.
  bool hasDestructor() const { return ClassDeclBits.HasDestructorDecl; }

  /// Set the 'has destructor' flag.
  void setHasDestructor() { ClassDeclBits.HasDestructorDecl = 1; }
  
  /// Retrieve the destructor for this class.
  DestructorDecl *getDestructor();

  /// Determine whether this class inherits the convenience initializers
  /// from its superclass.
  ///
  /// \param resolver Used to resolve the signatures of initializers, which is
  /// required for name lookup.
  bool inheritsSuperclassInitializers(LazyResolver *resolver);

  /// Figure out if this class has any @objc ancestors, in which case it should
  /// have implicitly @objc members. Note that a class with generic ancestry
  /// might have implicitly @objc members, but will never itself be @objc.
  ObjCClassKind checkObjCAncestry() const;

  /// Retrieve the name to use for this class when interoperating with
  /// the Objective-C runtime.
  StringRef getObjCRuntimeName(llvm::SmallVectorImpl<char> &buffer) const;

  /// Returns the appropriate kind of entry point to generate for this class,
  /// based on its attributes.
  ///
  /// It is an error to call this on a class that does not have a
  /// *ApplicationMain attribute.
  ArtificialMainKind getArtificialMainKind() const;

  using NominalTypeDecl::lookupDirect;

  /// Look in this class and its extensions (but not any of its protocols or
  /// superclasses) for declarations with a given Objective-C selector.
  ///
  /// Note that this can find methods, initializers, deinitializers,
  /// getters, and setters.
  ///
  /// \param selector The Objective-C selector of the method we're
  /// looking for.
  ///
  /// \param isInstance Whether we are looking for an instance method
  /// (vs. a class method).
  MutableArrayRef<AbstractFunctionDecl *> lookupDirect(ObjCSelector selector,
                                                       bool isInstance);

  /// Record the presence of an @objc method whose Objective-C name has been
  /// finalized.
  void recordObjCMethod(AbstractFunctionDecl *method);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Class;
  }
  static bool classof(const GenericTypeDecl *D) {
    return D->getKind() == DeclKind::Class;
  }
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Class;
  }
  static bool classof(const DeclContext *C) {
    auto GTD = dyn_cast<GenericTypeDecl>(C);
    return GTD && classof(static_cast<const Decl*>(GTD));
  }
  static bool classof(const IterableDeclContext *C) {
    auto NTD = dyn_cast<NominalTypeDecl>(C);
    return NTD && classof(NTD);
  }
  
  /// Returns true if the decl uses the Objective-C generics model.
  ///
  /// This is true of imported Objective-C classes.
  bool usesObjCGenericsModel() const {
    return isObjC() && hasClangNode() && isGenericContext();
  }
  
  /// True if the class is known to be implemented in Swift.
  bool hasKnownSwiftImplementation() const {
    return !hasClangNode();
  }
};


/// Describes whether a requirement refers to 'Self', for use in the
/// is-inheritable and is-available-existential checks.
struct SelfReferenceKind {
  bool result;
  bool parameter;
  bool other;

  /// The type does not refer to 'Self' at all.
  static SelfReferenceKind None() {
    return SelfReferenceKind(false, false, false);
  }

  /// The type refers to 'Self', but only as the result type of a method.
  static SelfReferenceKind Result() {
    return SelfReferenceKind(true, false, false);
  }

  /// The type refers to 'Self', but only as the parameter type of a method.
  static SelfReferenceKind Parameter() {
    return SelfReferenceKind(false, true, false);
  }

  /// The type refers to 'Self' in a position that is invariant.
  static SelfReferenceKind Other() {
    return SelfReferenceKind(false, false, true);
  }

  SelfReferenceKind flip() const {
    return SelfReferenceKind(parameter, result, other);
  }

  SelfReferenceKind operator|=(SelfReferenceKind kind) {
    result |= kind.result;
    parameter |= kind.parameter;
    other |= kind.other;
    return *this;
  }

  operator bool() const {
    return result || parameter || other;
  }

private:
  SelfReferenceKind(bool result, bool parameter, bool other)
    : result(result), parameter(parameter), other(other) { }
};

/// ProtocolDecl - A declaration of a protocol, for example:
///
///   protocol Drawable {
///     func draw()
///   }
class ProtocolDecl : public NominalTypeDecl {
  SourceLoc ProtocolLoc;

  ArrayRef<ProtocolDecl *> InheritedProtocols;

  llvm::DenseMap<ValueDecl *, Witness> DefaultWitnesses;

  /// The generic signature representing exactly the new requirements introduced
  /// by this protocol.
  GenericSignature *RequirementSignature = nullptr;

  /// True if the protocol has requirements that cannot be satisfied (e.g.
  /// because they could not be imported from Objective-C).
  unsigned HasMissingRequirements : 1;

  /// Whether we have already set the list of inherited protocols.
  unsigned InheritedProtocolsSet : 1;

  /// If this is a compiler-known protocol, this will be a KnownProtocolKind
  /// value, plus one. Otherwise, it will be 0.
  unsigned KnownProtocol : 6;

  bool requiresClassSlow();

  bool existentialConformsToSelfSlow();

  bool existentialTypeSupportedSlow(LazyResolver *resolver);

public:
  ProtocolDecl(DeclContext *DC, SourceLoc ProtocolLoc, SourceLoc NameLoc,
               Identifier Name, MutableArrayRef<TypeLoc> Inherited);
  
  using Decl::getASTContext;

  /// Retrieve the set of protocols inherited from this protocol.
  ArrayRef<ProtocolDecl *> getInheritedProtocols(LazyResolver *resolver) const;

  /// \brief Determine whether this protocol inherits from the given ("super")
  /// protocol.
  bool inheritsFrom(const ProtocolDecl *Super) const;
  
  ProtocolType *getDeclaredType() const {
    return reinterpret_cast<ProtocolType *>(
      NominalTypeDecl::getDeclaredType().getPointer());
  }
  
  SourceLoc getStartLoc() const { return ProtocolLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(ProtocolLoc, getBraces().End);
  }

  /// True if this protocol can only be conformed to by class types.
  bool requiresClass() const {
    if (ProtocolDeclBits.RequiresClassValid)
      return ProtocolDeclBits.RequiresClass;

    return const_cast<ProtocolDecl *>(this)->requiresClassSlow();
  }

  /// Specify that this protocol is class-bounded, e.g., because it was
  /// annotated with the 'class' keyword.
  void setRequiresClass(bool requiresClass = true) {
    ProtocolDeclBits.RequiresClassValid = true;
    ProtocolDeclBits.RequiresClass = requiresClass;
  }

  /// Determine whether an existential conforming to this protocol can be
  /// matched with a generic type parameter constrained to this protocol.
  /// This is only permitted if there is nothing "non-trivial" that we
  /// can do with the metatype, which means the protocol must not have
  /// any static methods and must be declared @objc.
  bool existentialConformsToSelf() const {
    if (ProtocolDeclBits.ExistentialConformsToSelfValid)
      return ProtocolDeclBits.ExistentialConformsToSelf;

    return const_cast<ProtocolDecl *>(this)
             ->existentialConformsToSelfSlow();
  }

  /// Find direct Self references within the given requirement.
  ///
  /// \param allowCovariantParameters If true, 'Self' is assumed to be
  /// covariant anywhere; otherwise, only in the return type of the top-level
  /// function type.
  ///
  /// \param skipAssocTypes If true, associated types of 'Self' are ignored;
  /// otherwise, they count as an 'other' usage of 'Self'.
  SelfReferenceKind findProtocolSelfReferences(const ValueDecl *decl,
                                               bool allowCovariantParameters,
                                               bool skipAssocTypes) const;

  /// Determine whether we are allowed to refer to an existential type
  /// conforming to this protocol. This is only permitted if the type of
  /// the member does not contain any associated types, and does not
  /// contain 'Self' in 'parameter' or 'other' position.
  bool isAvailableInExistential(const ValueDecl *decl) const;

  /// Determine whether we are allowed to refer to an existential type
  /// conforming to this protocol. This is only permitted if the types of
  /// all the members do not contain any associated types, and do not
  /// contain 'Self' in 'parameter' or 'other' position.
  bool existentialTypeSupported(LazyResolver *resolver) const {
    if (ProtocolDeclBits.ExistentialTypeSupportedValid)
      return ProtocolDeclBits.ExistentialTypeSupported;

    return const_cast<ProtocolDecl *>(this)
             ->existentialTypeSupportedSlow(resolver);
  }

  /// If this is known to be a compiler-known protocol, returns the kind.
  /// Otherwise returns None.
  ///
  /// Note that this is only valid after type-checking.
  Optional<KnownProtocolKind> getKnownProtocolKind() const {
    if (KnownProtocol == 0)
      return None;
    return static_cast<KnownProtocolKind>(KnownProtocol - 1);
  }

  /// Check whether this protocol is of a specific, known protocol kind.
  bool isSpecificProtocol(KnownProtocolKind kind) const {
    if (auto knownKind = getKnownProtocolKind())
      return *knownKind == kind;

    return false;
  }

  /// Records that this is a compiler-known protocol.
  void setKnownProtocolKind(KnownProtocolKind kind) {
    assert((!getKnownProtocolKind() || *getKnownProtocolKind() == kind) &&
           "can't reset known protocol kind");
    KnownProtocol = static_cast<unsigned>(kind) + 1;
    assert(getKnownProtocolKind() && *getKnownProtocolKind() == kind &&
           "not enough bits");
  }

  /// Retrieve the status of circularity checking for protocol inheritance.
  CircularityCheck getCircularityCheck() const {
    return static_cast<CircularityCheck>(ProtocolDeclBits.Circularity);
  }

  /// Record the current stage of circularity checking.
  void setCircularityCheck(CircularityCheck circularity) {
    ProtocolDeclBits.Circularity = static_cast<unsigned>(circularity);
  }

  /// Returns true if the protocol has requirements that are not listed in its
  /// members.
  ///
  /// This can occur, for example, if the protocol is an Objective-C protocol
  /// with requirements that cannot be represented in Swift.
  bool hasMissingRequirements() const {
    (void)getMembers();
    return HasMissingRequirements;
  }

  void setHasMissingRequirements(bool newValue) {
    HasMissingRequirements = newValue;
  }

  /// Returns the default witness for a requirement, or nullptr if there is
  /// no default.
  Witness getDefaultWitness(ValueDecl *requirement) const;

  /// Record the default witness for a requirement.
  void setDefaultWitness(ValueDecl *requirement, Witness witness);

  /// Set the list of inherited protocols.
  void setInheritedProtocols(ArrayRef<ProtocolDecl *> protocols) {
    assert(!InheritedProtocolsSet && "protocols already set");
    InheritedProtocolsSet = true;
    InheritedProtocols = protocols;
  }

  void clearInheritedProtocols() {
    InheritedProtocolsSet = true;
    InheritedProtocols = { };
  }

  bool isInheritedProtocolsValid() const {
    return InheritedProtocolsSet;
  }

  /// Retrieve the name to use for this protocol when interoperating
  /// with the Objective-C runtime.
  StringRef getObjCRuntimeName(llvm::SmallVectorImpl<char> &buffer) const;

  /// Create the implicit generic parameter list for a protocol or
  /// extension thereof.
  ///
  /// FIXME: protocol extensions will introduce a where clause here as well.
  GenericParamList *createGenericParams(DeclContext *dc);

  /// Create the generic parameters of this protocol if the haven't been
  /// created yet.
  void createGenericParamsIfMissing();

  /// Retrieve the generic signature representing the requirements introduced by
  /// this protocol.
  ///
  /// These are the requirements like any inherited protocols and conformances
  /// for associated types that are mentioned literally in this
  /// decl. Requirements implied via inheritance are not mentioned, nor is the
  /// conformance of Self to this protocol.
  GenericSignature *getRequirementSignature() const {
    assert(RequirementSignature &&
           "getting requirement signature before computing it");
    return RequirementSignature;
  }

  /// Has the requirement signature been computed yet?
  bool isRequirementSignatureComputed() const {
    return RequirementSignature != nullptr;
  }

  void computeRequirementSignature();

  void setRequirementSignature(GenericSignature *sig) {
    RequirementSignature = sig;
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Protocol;
  }
  static bool classof(const GenericTypeDecl *D) {
    return D->getKind() == DeclKind::Protocol;
  }
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Protocol;
  }
  static bool classof(const DeclContext *C) {
    auto GTD = dyn_cast<GenericTypeDecl>(C);
    return GTD && classof(static_cast<const Decl*>(GTD));
  }
  static bool classof(const IterableDeclContext *C) {
    auto NTD = dyn_cast<NominalTypeDecl>(C);
    return NTD && classof(NTD);
  }
};

// Note that the values of these enums line up with %select values in
// diagnostics.
enum class AccessorKind {
  /// \brief This is not a property accessor.
  NotAccessor = -1,
  /// \brief This is a getter for a property or subscript.
  IsGetter = 0,
  /// \brief This is a setter for a property or subscript.
  IsSetter = 1,
  /// \brief This is a willSet specifier for a property.
  IsWillSet = 2,
  /// \brief This is a didSet specifier for a property.
  IsDidSet = 3,
  /// \brief This is a materializeForSet accessor for a property.
  IsMaterializeForSet = 4,
  /// \brief This is an address-family accessor for a property or
  /// subscript.  It also has an addressor kind.
  IsAddressor = 5,
  /// \brief This is a mutableAddress-family accessor for a property
  /// or subscript.  It also has an addressor kind.
  IsMutableAddressor = 6,
};

/// The safety semantics of this addressor.
enum class AddressorKind : unsigned char {
  /// \brief This is not an addressor.
  NotAddressor,
  /// \brief This is an unsafe addressor; it simply returns an address.
  Unsafe,
  /// \brief This is an owning addressor; it returns a Builtin.UnknownObject
  /// which should be released when the caller is done with the object.
  Owning,
  /// \brief This is an owning addressor; it returns a Builtin.NativeObject
  /// which should be released when the caller is done with the object.
  NativeOwning,
  /// \brief This is a pinning addressor; it returns a Builtin.NativeObject?
  /// which should be unpinned when the caller is done with the object.
  NativePinning,
};

/// Whether an access to storage is for reading, writing, or both.
enum class AccessKind : unsigned char {
  /// The access is just to read the current value.
  Read,

  /// The access is just to overwrite the current value.
  Write,

  /// The access may require either reading or writing the current value.
  ReadWrite
};

/// The way to actually evaluate an access to storage.
enum class AccessStrategy : unsigned char {
  /// The decl is a VarDecl with its own backing storage; evaluate its
  /// address directly.
  Storage,
  
  /// The decl is a VarDecl with storage defined by a property behavior;
  /// this access may initialize or reassign the storage based on dataflow.
  BehaviorStorage,

  /// The decl has addressors; call the appropriate addressor for the
  /// access kind.  These calls are currently always direct.
  Addressor,

  /// Directly call the getter, setter, or materializeForSet accessor.
  DirectToAccessor,

  /// Indirectly call the getter, setter, or materializeForSet accessor.
  DispatchToAccessor,
};

/// Information about a behavior instantiated by a storage declaration.
///
/// TODO: Accessors, composed behaviors
struct alignas(1 << 3) BehaviorRecord {
  // The behavior name.
  TypeRepr *ProtocolName;
  // The parameter expression, if any.
  Expr *Param;
  
  Optional<NormalProtocolConformance *> Conformance = None;
  // The 'value' property from the behavior protocol that provides the property
  // implementation.
  VarDecl *ValueDecl = nullptr;
  
  // Storage declaration and initializer for use by definite initialization.
  VarDecl *StorageDecl = nullptr;
  ConcreteDeclRef InitStorageDecl = nullptr;
  
  bool needsInitialization() const {
    assert((bool)StorageDecl == (bool)InitStorageDecl
           && "DI state not consistent");
    return StorageDecl != nullptr;
  }
  
  BehaviorRecord(TypeRepr *ProtocolName,
                 Expr *Param)
    : ProtocolName(ProtocolName), Param(Param)
  {}
  
  SourceLoc getLoc() const { return ProtocolName->getLoc(); }
};

/// AbstractStorageDecl - This is the common superclass for VarDecl and
/// SubscriptDecl, representing potentially settable memory locations.
class AbstractStorageDecl : public ValueDecl {
public:
  enum StorageKindTy {
    /// There are bits stored in memory for this object, and they are accessed
    /// directly.  This is not valid for a SubscriptDecl.
    Stored,
    
    /// This is a stored property with trivial accessors which simply get and
    /// set the underlying storage.  This is not valid for a SubscriptDecl.
    ///
    /// These accessors are used for several different purposes:
    ///   1) In an @objc variable, these accessors are dynamically dispatched
    ///      to and may be overridden.
    ///   2) When a stored property satisfies a protocol requirement, these
    ///      accessors end up as entries in the witness table.
    ///   3) When a stored property is accessed outside of the storage
    ///      declaration's resilience domain, when the owning type or
    ///      global variable is resilient.
    StoredWithTrivialAccessors,

    /// This is a stored property with either a didSet specifier or a
    /// willSet specifier (or both).  Sema synthesizes a setter which
    /// calls them at the appropriate points.
    StoredWithObservers,

    /// There are bits stored in memory for this object, but they are
    /// not allocated directly within the container; instead, there
    /// are accessors which return the address of the memory.  The
    /// value is accessed directly through the returned address.
    ///
    /// This is legal on both VarDecls and SubscriptDecls.
    ///
    /// There is always at least an 'address' accessor; if the object
    /// is mutable, there will also be a 'mutableAddress' accessor.
    Addressed,
    
    /// Like Addressed, this object has address accessors.  Like
    /// StoredWithTrivialAccessors, accessors have been synthesized
    /// which simply read and write through the addresses returned by
    /// the addressors.
    AddressedWithTrivialAccessors,

    /// Like Addressed, this object has address accessors.  Like
    /// StoredWithObservers, it also has either a willSet specifier or
    /// a didSet specifier.  Accessors have been synthesized, like
    /// with StoredWithObservers but using the address returned from
    /// the appropriate accessor instead.
    AddressedWithObservers,

    /// This is an override of an object which adds either a didSet
    /// specifier or a willSet specifier (or both).  Sema synthesizes
    /// a setter which calls them at the appropriate points around
    /// delegating to the superclass's setter.
    InheritedWithObservers,

    /// There is no memory associated with this decl anywhere.  It is
    /// accessed by calling a getter and setter.  If the setter is
    /// absent, then the value is only loadable, but not storable.
    Computed,

    /// This object was specified with non-trivial getter and
    /// mutableAddress accessors.  If it is accessed in a read-only
    /// manner, the getter is called; otherwise, mutableAddress is
    /// called.
    ///
    /// This turns out to the be the right thing for certain core data
    /// structures which, when they store a bridged object, cannot
    /// return the address at which the object is stored.
    ComputedWithMutableAddress,
  };
private:
  AbstractStorageDecl *OverriddenDecl;

  struct GetSetRecord;
  
  /// This is stored immediately before the GetSetRecord.
  struct alignas(1 << 3) AddressorRecord {
    FuncDecl *Address = nullptr;        // User-defined address accessor
    FuncDecl *MutableAddress = nullptr; // User-defined mutableAddress accessor

    GetSetRecord *getGetSet() {
      // Relies on not-strictly-portable ABI layout assumptions.
      return reinterpret_cast<GetSetRecord*>(this+1);
    }
  };
  void configureAddressorRecord(AddressorRecord *record,
                               FuncDecl *addressor, FuncDecl *mutableAddressor);

  struct alignas(1 << 3) GetSetRecord {
    SourceRange Braces;
    FuncDecl *Get = nullptr;       // User-defined getter
    FuncDecl *Set = nullptr;       // User-defined setter
    FuncDecl *MaterializeForSet = nullptr; // optional materializeForSet accessor

    AddressorRecord *getAddressors() {
      // Relies on not-strictly-portable ABI layout assumptions.
      return reinterpret_cast<AddressorRecord*>(this) - 1;
    }
  };
  void configureGetSetRecord(GetSetRecord *getSetRecord,
                             FuncDecl *getter, FuncDecl *setter,
                             FuncDecl *materializeForSet);
  void configureSetRecord(GetSetRecord *getSetInfo,
                          FuncDecl *setter,
                          FuncDecl *materializeForSet);

  struct ObservingRecord : GetSetRecord {
    FuncDecl *WillSet = nullptr;   // willSet(value):
    FuncDecl *DidSet = nullptr;    // didSet:
  };
  void configureObservingRecord(ObservingRecord *record,
                                FuncDecl *willSet, FuncDecl *didSet);

  llvm::PointerIntPair<GetSetRecord*, 3, OptionalEnum<Accessibility>> GetSetInfo;
  llvm::PointerIntPair<BehaviorRecord*, 3, OptionalEnum<Accessibility>>
    BehaviorInfo;

  ObservingRecord &getDidSetInfo() const {
    assert(hasObservers());
    return *static_cast<ObservingRecord*>(GetSetInfo.getPointer());
  }
  AddressorRecord &getAddressorInfo() const {
    assert(hasAddressors());
    return *GetSetInfo.getPointer()->getAddressors();
  }

  void setStorageKind(StorageKindTy K) {
    AbstractStorageDeclBits.StorageKind = unsigned(K);
  }

protected:
  AbstractStorageDecl(DeclKind Kind, DeclContext *DC, DeclName Name,
                      SourceLoc NameLoc)
    : ValueDecl(Kind, DC, Name, NameLoc), OverriddenDecl(nullptr) {
    AbstractStorageDeclBits.StorageKind = Stored;
    AbstractStorageDeclBits.Overridden = false;
  }
public:

  /// \brief Should this declaration be treated as if annotated with transparent
  /// attribute.
  bool isTransparent() const;

  /// \brief Determine whether this storage is a static member, if it
  /// is a member.  Currently only variables can be static.
  inline bool isStatic() const; // defined in this header

  /// \brief Determine whether this variable is computed, which means it
  /// has no storage but does have a user-defined getter or setter.
  ///
  StorageKindTy getStorageKind() const {
    return (StorageKindTy) AbstractStorageDeclBits.StorageKind;
  }

  /// \brief Return true if this is a VarDecl that has storage associated with
  /// it.
  bool hasStorage() const {
    switch (getStorageKind()) {
    case Stored:
    case StoredWithTrivialAccessors:
    case StoredWithObservers:
      return true;
    case InheritedWithObservers:
    case Computed:
    case ComputedWithMutableAddress:
    case Addressed:
    case AddressedWithTrivialAccessors:
    case AddressedWithObservers:
      return false;
    }
    llvm_unreachable("bad storage kind");
  }

  /// \brief Return true if this object has a getter (and, if mutable,
  /// a setter and a materializeForSet).
  bool hasAccessorFunctions() const {
    switch (getStorageKind()) {
    case Addressed:
    case Stored:
      return false;
    case StoredWithTrivialAccessors:
    case StoredWithObservers:
    case InheritedWithObservers:
    case Computed:
    case ComputedWithMutableAddress:
    case AddressedWithTrivialAccessors:
    case AddressedWithObservers:
      return true;
    }
    llvm_unreachable("bad storage kind");
  }

  /// \brief Return true if this object has observing accessors.
  ///
  /// It's generally not appropriate to use this predicate directly in
  /// a condition; instead, you should be switching on the storage kind.
  bool hasObservers() const {
    switch (getStorageKind()) {
    case Stored:
    case StoredWithTrivialAccessors:
    case Computed:
    case ComputedWithMutableAddress:
    case Addressed:
    case AddressedWithTrivialAccessors:
      return false;
    case StoredWithObservers:
    case InheritedWithObservers:
    case AddressedWithObservers:
      return true;
    }
    llvm_unreachable("bad storage kind");
  }

  /// \brief Return true if this object has either an addressor or a
  /// mutable addressor.
  ///
  /// It's generally not appropriate to use this predicate directly in
  /// a condition; instead, you should be switching on the storage
  /// kind.  Only use this for diagnostic, AST exploration, or
  /// assertion purposes.
  bool hasAddressors() const {
    switch (getStorageKind()) {
    case Stored:
    case StoredWithTrivialAccessors:
    case StoredWithObservers:
    case InheritedWithObservers:
    case Computed:
      return false;
    case ComputedWithMutableAddress:
    case Addressed:
    case AddressedWithTrivialAccessors:
    case AddressedWithObservers:
      return true;
    }
    llvm_unreachable("bad storage kind");
  }
  
  /// \brief Return true if the 'getter' is 'mutating', i.e. that it requires an
  /// lvalue base to be accessed.
  bool isGetterMutating() const;
  
  /// \brief Return true if the 'setter' is 'nonmutating', i.e. that it can be
  /// called even on an immutable base value.
  bool isSetterNonMutating() const;

  FuncDecl *getAccessorFunction(AccessorKind accessor) const;

  /// \brief Turn this into a computed variable, providing a getter and setter.
  void makeComputed(SourceLoc LBraceLoc, FuncDecl *Get, FuncDecl *Set,
                    FuncDecl *MaterializeForSet, SourceLoc RBraceLoc);

  /// \brief Turn this into a computed object, providing a getter and a mutable
  /// addressor.
  void makeComputedWithMutableAddress(SourceLoc lbraceLoc,
                                      FuncDecl *getter, FuncDecl *setter,
                                      FuncDecl *materializeForSet,
                                      FuncDecl *mutableAddressor,
                                      SourceLoc rbraceLoc);

  /// \brief Add trivial accessors to this Stored or Addressed object.
  void addTrivialAccessors(FuncDecl *Get, FuncDecl *Set,
                           FuncDecl *MaterializeForSet);

  /// \brief Turn this into a stored-with-observers var, providing the
  /// didSet/willSet specifiers.
  void makeStoredWithObservers(SourceLoc LBraceLoc, FuncDecl *WillSet,
                               FuncDecl *DidSet, SourceLoc RBraceLoc);

  /// \brief Turn this into an inherited-with-observers var, providing
  /// the didSet/willSet specifiers.
  void makeInheritedWithObservers(SourceLoc LBraceLoc, FuncDecl *WillSet,
                                  FuncDecl *DidSet, SourceLoc RBraceLoc);

  /// \brief Turn this into an addressed var.
  void makeAddressed(SourceLoc LBraceLoc, FuncDecl *Addressor,
                     FuncDecl *MutableAddressor,
                     SourceLoc RBraceLoc);

  /// \brief Turn this into an addressed var with observing accessors.
  void makeAddressedWithObservers(SourceLoc LBraceLoc, FuncDecl *Addressor,
                                  FuncDecl *MutableAddressor,
                                  FuncDecl *WillSet, FuncDecl *DidSet,
                                  SourceLoc RBraceLoc);

  /// \brief Specify the synthesized get/set functions for a
  /// StoredWithObservers or AddressedWithObservers var.  This is used by Sema.
  void setObservingAccessors(FuncDecl *Get, FuncDecl *Set,
                             FuncDecl *MaterializeForSet);

  /// \brief Add a setter to an existing Computed var.
  ///
  /// This should only be used by the ClangImporter.
  void setComputedSetter(FuncDecl *Set);

  /// \brief Add a behavior to a property.
  void addBehavior(TypeRepr *Type, Expr *Param);

  /// \brief Set a materializeForSet accessor for this declaration.
  ///
  /// This should only be used by Sema.
  void setMaterializeForSetFunc(FuncDecl *materializeForSet);

  /// \brief Specify the braces range without adding accessors.
  ///
  /// This is used to record the braces range if the accessors were rejected.
  void setInvalidBracesRange(SourceRange BracesRange);

  SourceRange getBracesRange() const {
    if (auto info = GetSetInfo.getPointer())
      return info->Braces;
    return SourceRange();
  }

  /// \brief Retrieve the getter used to access the value of this variable.
  FuncDecl *getGetter() const {
    if (auto info = GetSetInfo.getPointer())
      return info->Get;
    return nullptr;
  }
  
  /// \brief Retrieve the setter used to mutate the value of this variable.
  FuncDecl *getSetter() const {
    if (auto info = GetSetInfo.getPointer())
      return info->Set;
    return nullptr;
  }

  Accessibility getSetterAccessibility() const {
    assert(hasAccessibility());
    assert(GetSetInfo.getInt().hasValue());
    return GetSetInfo.getInt().getValue();
  }

  void setSetterAccessibility(Accessibility accessLevel) {
    assert(!GetSetInfo.getInt().hasValue());
    overwriteSetterAccessibility(accessLevel);
  }

  void overwriteSetterAccessibility(Accessibility accessLevel);

  /// \brief Retrieve the materializeForSet function, if this
  /// declaration has one.
  FuncDecl *getMaterializeForSetFunc() const {
    if (auto info = GetSetInfo.getPointer())
      return info->MaterializeForSet;
    return nullptr;
  }

  /// \brief Return the funcdecl for the 'address' accessor if it
  /// exists; this is only valid on a declaration with addressors.
  FuncDecl *getAddressor() const { return getAddressorInfo().Address; }

  /// \brief Return the funcdecl for the 'mutableAddress' accessors if
  /// it exists; this is only valid on a declaration with addressors.
  FuncDecl *getMutableAddressor() const {
    return getAddressorInfo().MutableAddress;
  }

  /// \brief Return the appropriate addressor for the given access kind.
  FuncDecl *getAddressorForAccess(AccessKind accessKind) const {
    if (accessKind == AccessKind::Read)
      return getAddressor();
    return getMutableAddressor();
  }
  
  /// \brief Return the funcdecl for the willSet specifier if it exists, this is
  /// only valid on a declaration with Observing storage.
  FuncDecl *getWillSetFunc() const { return getDidSetInfo().WillSet; }

  /// \brief Return the funcdecl for the didSet specifier if it exists, this is
  /// only valid on a declaration with Observing storage.
  FuncDecl *getDidSetFunc() const { return getDidSetInfo().DidSet; }

  /// Return true if this storage can (but doesn't have to) be accessed with
  /// Objective-C-compatible getters and setters.
  bool hasForeignGetterAndSetter() const;

  /// Return true if this storage *must* be accessed with Objective-C-compatible
  /// getters and setters.
  bool requiresForeignGetterAndSetter() const;

  /// Given that this is an Objective-C property or subscript declaration,
  /// produce its getter selector.
  ObjCSelector getObjCGetterSelector(LazyResolver *resolver = nullptr,
                                Identifier preferredName = Identifier()) const;

  /// Given that this is an Objective-C property or subscript declaration,
  /// produce its setter selector.
  ObjCSelector getObjCSetterSelector(LazyResolver *resolver = nullptr,
                                Identifier preferredName = Identifier()) const;

  AbstractStorageDecl *getOverriddenDecl() const {
    return OverriddenDecl;
  }
  void setOverriddenDecl(AbstractStorageDecl *over) {
    // FIXME: Hack due to broken class circularity checking.
    if (over == this) return;
    OverriddenDecl = over;
    over->setIsOverridden();
  }

  /// The declaration has been overridden in the module
  ///
  /// Resolved during type checking
  void setIsOverridden() {
    AbstractStorageDeclBits.Overridden = true;
  }

  /// Whether the declaration is later overridden in the module
  ///
  /// Overrides are resolved during type checking; only query this field after
  /// the whole module has been checked
  bool isOverridden() const { return AbstractStorageDeclBits.Overridden; }

  /// Returns the location of 'override' keyword, if any.
  SourceLoc getOverrideLoc() const;

  /// Returns true if this declaration has a setter accessible from the given
  /// context.
  ///
  /// If \p DC is null, returns true only if the setter is public.
  bool isSetterAccessibleFrom(const DeclContext *DC) const;

  /// Determine how this storage declaration should actually be accessed.
  AccessStrategy getAccessStrategy(AccessSemantics semantics,
                                   AccessKind accessKind) const;

  /// \brief Does this declaration expose a fixed layout to all resilience
  /// domains?
  ///
  /// Roughly speaking, this means we can make assumptions about whether
  /// the storage is stored or computed, and if stored, the precise access
  /// pattern to be used.
  bool hasFixedLayout() const;

  /// \brief Does this declaration expose a fixed layout to the given
  /// module?
  bool hasFixedLayout(ModuleDecl *M, ResilienceExpansion expansion) const;

  /// Does the storage use a behavior?
  bool hasBehavior() const {
    return BehaviorInfo.getPointer() != nullptr;
  }
  
  /// Does the storage use a behavior, and require definite initialization
  /// analysis.
  bool hasBehaviorNeedingInitialization() const {
    if (auto behavior = getBehavior()) {
      return behavior->needsInitialization();
    }
    return false;
  }
  
  /// Get the behavior info.
  const BehaviorRecord *getBehavior() const {
    return BehaviorInfo.getPointer();
  }
  BehaviorRecord *getMutableBehavior() {
    return BehaviorInfo.getPointer();
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_AbstractStorageDecl &&
           D->getKind() <= DeclKind::Last_AbstractStorageDecl;
  }
};

/// VarDecl - 'var' and 'let' declarations.
class VarDecl : public AbstractStorageDecl {
protected:
  llvm::PointerUnion<PatternBindingDecl*, Stmt*> ParentPattern;

  VarDecl(DeclKind Kind, bool IsStatic, bool IsLet, bool IsCaptureList,
          SourceLoc NameLoc, Identifier Name, Type Ty, DeclContext *DC)
    : AbstractStorageDecl(Kind, DC, Name, NameLoc)
  {
    VarDeclBits.IsUserAccessible = true;
    VarDeclBits.IsStatic = IsStatic;
    VarDeclBits.IsLet = IsLet;
    VarDeclBits.IsCaptureList = IsCaptureList;
    VarDeclBits.IsDebuggerVar = false;
    VarDeclBits.HasNonPatternBindingInit = false;
    setType(Ty);
  }

  /// This is the type specified, including location information.
  TypeLoc typeLoc;

  Type typeInContext;

public:
  VarDecl(bool IsStatic, bool IsLet, bool IsCaptureList, SourceLoc NameLoc,
          Identifier Name, Type Ty, DeclContext *DC)
    : VarDecl(DeclKind::Var, IsStatic, IsLet, IsCaptureList, NameLoc, Name, Ty,
              DC) {}

  SourceRange getSourceRange() const;

  void setUserAccessible(bool Accessible) {
    VarDeclBits.IsUserAccessible = Accessible;
  }

  bool isUserAccessible() const {
    return VarDeclBits.IsUserAccessible;
  }

  TypeLoc &getTypeLoc() { return typeLoc; }
  TypeLoc getTypeLoc() const { return typeLoc; }

  bool hasType() const {
    // We have a type if either the type has been computed already or if
    // this is a deserialized declaration with an interface type.
    return !typeInContext.isNull();
  }

  /// Get the type of the variable within its context. If the context is generic,
  /// this will use archetypes.
  Type getType() const {
    assert(!typeInContext.isNull() && "no contextual type set yet");
    return typeInContext;
  }

  /// Set the type of the variable within its context.
  void setType(Type t);

  void markInvalid();

  /// Retrieve the source range of the variable type, or an invalid range if the
  /// variable's type is not explicitly written in the source.
  ///
  /// Only for use in diagnostics.  It is not always possible to always
  /// precisely point to the variable type because of type aliases.
  SourceRange getTypeSourceRangeForDiagnostics() const;

  /// \brief Returns whether the var is settable in the specified context: this
  /// is either because it is a stored var, because it has a custom setter, or
  /// is a let member in an initializer.
  ///
  /// Pass a null context and null base to check if it's always settable.
  bool isSettable(const DeclContext *UseDC,
                  const DeclRefExpr *base = nullptr) const;

  /// Return the parent pattern binding that may provide an initializer for this
  /// VarDecl.  This returns null if there is none associated with the VarDecl.
  PatternBindingDecl *getParentPatternBinding() const {
    return ParentPattern.dyn_cast<PatternBindingDecl *>();
  }
  void setParentPatternBinding(PatternBindingDecl *PBD) {
    ParentPattern = PBD;
  }
  
  /// Return the Pattern involved in initializing this VarDecl.  However, recall
  /// that the Pattern may be involved in initializing more than just this one
  /// vardecl.  For example, if this is a VarDecl for "x", the pattern may be
  /// "(x, y)" and the initializer on the PatternBindingDecl may be "(1,2)" or
  /// "foo()".
  ///
  /// If this has no parent pattern binding decl or statement associated, it
  /// returns null.
  ///
  Pattern *getParentPattern() const;
  
  /// Return the statement that owns the pattern associated with this VarDecl,
  /// if one exists.
  Stmt *getParentPatternStmt() const {
    return ParentPattern.dyn_cast<Stmt*>();
  }
  void setParentPatternStmt(Stmt *S) {
    ParentPattern = S;
  }

  /// Return the initializer involved in this VarDecl.  Recall that the
  /// initializer may be involved in initializing more than just this one
  /// vardecl though.  For example, if this is a VarDecl for "x", the pattern
  /// may be "(x, y)" and the initializer on the PatternBindingDecl may be
  /// "(1,2)" or "foo()".
  ///
  /// If this has no parent pattern binding decl associated, or if that pattern
  /// binding has no initial value, this returns null.
  ///
  Expr *getParentInitializer() const {
    if (auto *PBD = getParentPatternBinding())
      return PBD->getPatternEntryForVarDecl(this).getInit();
    return nullptr;
  }

  VarDecl *getOverriddenDecl() const {
    return cast_or_null<VarDecl>(AbstractStorageDecl::getOverriddenDecl());
  }

  /// Determine whether this declaration is an anonymous closure parameter.
  bool isAnonClosureParam() const;

  /// Is this a type ('static') variable?
  bool isStatic() const { return VarDeclBits.IsStatic; }
  void setStatic(bool IsStatic) { VarDeclBits.IsStatic = IsStatic; }

  /// \returns the way 'static'/'class' should be spelled for this declaration.
  StaticSpellingKind getCorrectStaticSpelling() const;

  /// Is this an immutable 'let' property?
  bool isLet() const { return VarDeclBits.IsLet; }
  void setLet(bool IsLet) { VarDeclBits.IsLet = IsLet; }

  /// Is this an element in a capture list?
  bool isCaptureList() const { return VarDeclBits.IsCaptureList; }

  /// Return true if this vardecl has an initial value bound to it in a way
  /// that isn't represented in the AST with an initializer in the pattern
  /// binding.  This happens in cases like "for i in ...", switch cases, etc.
  bool hasNonPatternBindingInit() const {
    return VarDeclBits.HasNonPatternBindingInit;
  }
  void setHasNonPatternBindingInit(bool V = true) {
    VarDeclBits.HasNonPatternBindingInit = V;
  }
  
  /// Is this a special debugger variable?
  bool isDebuggerVar() const { return VarDeclBits.IsDebuggerVar; }
  void setDebuggerVar(bool IsDebuggerVar) {
    VarDeclBits.IsDebuggerVar = IsDebuggerVar;
  }

  /// Return the Objective-C runtime name for this property.
  Identifier getObjCPropertyName() const;

  /// Retrieve the default Objective-C selector for the getter of a
  /// property of the given name.
  static ObjCSelector getDefaultObjCGetterSelector(ASTContext &ctx,
                                                   Identifier propertyName);

  /// Retrieve the default Objective-C selector for the setter of a
  /// property of the given name.
  static ObjCSelector getDefaultObjCSetterSelector(ASTContext &ctx,
                                                   Identifier propertyName);

  /// If this is a simple 'let' constant, emit a note with a fixit indicating
  /// that it can be rewritten to a 'var'.  This is used in situations where the
  /// compiler detects obvious attempts to mutate a constant.
  void emitLetToVarNoteIfSimple(DeclContext *UseDC) const;

  /// Returns true if the name is the self identifier and is implicit.
  bool isSelfParameter() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { 
    return D->getKind() == DeclKind::Var || D->getKind() == DeclKind::Param; 
  }
};

/// A function parameter declaration.
class ParamDecl : public VarDecl {
  Identifier ArgumentName;
  SourceLoc ArgumentNameLoc;
  SourceLoc LetVarInOutLoc;

  struct StoredDefaultArgument {
    Expr *DefaultArg = nullptr;
    Initializer *InitContext = nullptr;
  };

  /// The default value, if any, along with whether this is varargs.
  llvm::PointerIntPair<StoredDefaultArgument *, 1> DefaultValueAndIsVariadic;
  
  /// True if the type is implicitly specified in the source, but this has an
  /// apparently valid typeRepr.  This is used in accessors, which look like:
  ///    set (value) {
  /// but need to get the typeRepr from the property as a whole so Sema can
  /// resolve the type.
  bool IsTypeLocImplicit = false;
  
  /// Information about a symbolic default argument, like #file.
  DefaultArgumentKind defaultArgumentKind = DefaultArgumentKind::None;
  
public:
  ParamDecl(bool isLet, SourceLoc letVarInOutLoc, SourceLoc argumentNameLoc,
            Identifier argumentName, SourceLoc parameterNameLoc,
            Identifier parameterName, Type ty, DeclContext *dc);

  /// Clone constructor, allocates a new ParamDecl identical to the first.
  /// Intentionally not defined as a typical copy constructor to avoid
  /// accidental copies.
  ParamDecl(ParamDecl *PD);
  
  /// Retrieve the argument (API) name for this function parameter.
  Identifier getArgumentName() const { return ArgumentName; }

  /// Retrieve the source location of the argument (API) name.
  ///
  /// The resulting source location will be valid if the argument name
  /// was specified separately from the parameter name.
  SourceLoc getArgumentNameLoc() const { return ArgumentNameLoc; }

  SourceLoc getLetVarInOutLoc() const { return LetVarInOutLoc; }

  bool isTypeLocImplicit() const { return IsTypeLocImplicit; }
  void setIsTypeLocImplicit(bool val) { IsTypeLocImplicit = val; }
  
  bool isDefaultArgument() const {
    return defaultArgumentKind != DefaultArgumentKind::None;
  }
  DefaultArgumentKind getDefaultArgumentKind() const {
    return defaultArgumentKind;
  }
  void setDefaultArgumentKind(DefaultArgumentKind K) {
    defaultArgumentKind = K;
  }
  
  Expr *getDefaultValue() const {
    if (auto stored = DefaultValueAndIsVariadic.getPointer())
      return stored->DefaultArg;
    return nullptr;
  }

  void setDefaultValue(Expr *E);

  Initializer *getDefaultArgumentInitContext() const {
    if (auto stored = DefaultValueAndIsVariadic.getPointer())
      return stored->InitContext;
    return nullptr;
  }

  void setDefaultArgumentInitContext(Initializer *initContext);

  /// Whether or not this parameter is varargs.
  bool isVariadic() const { return DefaultValueAndIsVariadic.getInt(); }
  void setVariadic(bool value = true) {DefaultValueAndIsVariadic.setInt(value);}
  
  /// Remove the type of this varargs element designator, without the array
  /// type wrapping it.  A parameter like "Int..." will have formal parameter
  /// type of "[Int]" and this returns "Int".
  static Type getVarargBaseTy(Type VarArgT);
  
  /// Remove the type of this varargs element designator, without the array
  /// type wrapping it.
  Type getVarargBaseTy() const {
    assert(isVariadic());
    return getVarargBaseTy(getInterfaceType());
  }
  
  SourceRange getSourceRange() const;

  /// Create an implicit 'self' decl for a method in the specified decl context.
  /// If 'static' is true, then this is self for a static method in the type.
  ///
  /// Note that this decl is created, but it is returned with an incorrect
  /// DeclContext that needs to be set correctly.  This is automatically handled
  /// when a function is created with this as part of its argument list.
  /// For a generic context, this also gives the parameter an unbound generic
  /// type with the expectation that type-checking will fill in the context
  /// generic parameters.
  static ParamDecl *createUnboundSelf(SourceLoc loc, DeclContext *DC);

  /// Create an implicit 'self' decl for a method in the specified decl context.
  /// If 'static' is true, then this is self for a static method in the type.
  ///
  /// Note that this decl is created, but it is returned with an incorrect
  /// DeclContext that needs to be set correctly.  This is automatically handled
  /// when a function is created with this as part of its argument list.
  static ParamDecl *createSelf(SourceLoc loc, DeclContext *DC,
                               bool isStatic = false,
                               bool isInOut = false);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { 
    return D->getKind() == DeclKind::Param;
  }
};
  
/// Describes the kind of subscripting used in Objective-C.
enum class ObjCSubscriptKind {
  /// Not an Objective-C subscripting kind.
  None,
  /// Objective-C indexed subscripting, which is based on an integral
  /// index.
  Indexed,
  /// Objective-C keyed subscripting, which is based on an object
  /// argument or metatype thereof.
  Keyed
};

/// \brief Declares a subscripting operator for a type.
///
/// A subscript declaration is defined as a get/set pair that produces a
/// specific type. For example:
///
/// \code
/// subscript (i : Int) -> String {
///   get { /* return ith String */ }
///   set { /* set ith string to value */ }
/// }
/// \endcode
///
/// A type with a subscript declaration can be used as the base of a subscript
/// expression a[i], where a is of the subscriptable type and i is the type
/// of the index. A subscript can have multiple indices:
///
/// \code
/// struct Matrix {
///   subscript (i : Int, j : Int) -> Double {
///     get { /* return element at position (i, j) */ }
///     set { /* set element at position (i, j) */ }
///   }
/// }
/// \endcode
///
/// A given type can have multiple subscript declarations, so long as the
/// signatures (indices and element type) are distinct.
///
class SubscriptDecl : public AbstractStorageDecl, public DeclContext {
  SourceLoc ArrowLoc;
  ParameterList *Indices;
  TypeLoc ElementTy;

public:
  SubscriptDecl(DeclName Name, SourceLoc SubscriptLoc, ParameterList *Indices,
                SourceLoc ArrowLoc, TypeLoc ElementTy, DeclContext *Parent)
    : AbstractStorageDecl(DeclKind::Subscript, Parent, Name, SubscriptLoc),
      DeclContext(DeclContextKind::SubscriptDecl, Parent),
      ArrowLoc(ArrowLoc), Indices(nullptr), ElementTy(ElementTy) {
    setIndices(Indices);
  }
  
  SourceLoc getSubscriptLoc() const { return getNameLoc(); }
  SourceLoc getStartLoc() const { return getSubscriptLoc(); }
  SourceRange getSourceRange() const;

  /// \brief Retrieve the indices for this subscript operation.
  ParameterList *getIndices() { return Indices; }
  const ParameterList *getIndices() const { return Indices; }
  void setIndices(ParameterList *p);

  /// Retrieve the interface type of the indices.
  Type getIndicesInterfaceType() const;

  /// \brief Retrieve the type of the element referenced by a subscript
  /// operation.
  Type getElementInterfaceType() const;
  TypeLoc &getElementTypeLoc() { return ElementTy; }
  const TypeLoc &getElementTypeLoc() const { return ElementTy; }

  /// \brief Returns whether the result of the subscript operation can be set.
  bool isSettable() const;

  /// Determine the kind of Objective-C subscripting this declaration
  /// implies.
  ObjCSubscriptKind getObjCSubscriptKind(LazyResolver *resolver) const;

  SubscriptDecl *getOverriddenDecl() const {
    return cast_or_null<SubscriptDecl>(
                                  AbstractStorageDecl::getOverriddenDecl());
  }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Subscript;
  }
  
  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::SubscriptDecl;
  }

  using DeclContext::operator new;
  using Decl::getASTContext;
};

/// Encodes imported-as-member status for C functions that get imported
/// as methods.
struct ImportAsMemberStatus {
  // non-0 denotes import-as-member. 1 denotes no self index. n+2 denotes self
  // index of n
  uint8_t rawValue = 0;

  bool isImportAsMember() const { return rawValue != 0; }
  bool isInstance() const { return rawValue >= 2; }
  bool isStatic() const { return rawValue == 1; }
  uint8_t getSelfIndex() const {
    assert(isInstance() && "not set");
    return rawValue - 2;
  }
  void setStatic() {
    assert(!isStatic() && "already set");
    rawValue = 1;
  }
  void setSelfIndex(uint8_t idx) {
    assert(!isImportAsMember() && "already set");
    assert(idx <= UINT8_MAX-2 && "out of bounds");
    rawValue = idx + 2;
  }
};

/// \brief Base class for function-like declarations.
class AbstractFunctionDecl : public ValueDecl, public DeclContext {
public:
  enum class BodyKind {
    /// The function did not have a body in the source code file.
    None,

    /// Function body is delayed, to be parsed later.
    Unparsed,

    /// Function body is parsed and available as an AST subtree.
    Parsed,

    /// Function body is not available, although it was written in the source.
    Skipped,

    /// Function body will be synthesized on demand.
    Synthesize,

    /// Function body is present and type-checked.
    TypeChecked,

    /// This is a memberwise initializer that will be synthesized by SILGen.
    MemberwiseInitializer

    // This enum currently needs to fit in a 3-bit bitfield.
  };

  BodyKind getBodyKind() const {
    return BodyKind(AbstractFunctionDeclBits.BodyKind);
  }

  using BodySynthesizer = void (*)(AbstractFunctionDecl *);

protected:
  // If a function has a body at all, we have either a parsed body AST node or
  // we have saved the end location of the unparsed body.
  union {
    /// This enum member is active if getBodyKind() is BodyKind::Parsed or
    /// BodyKind::TypeChecked.
    BraceStmt *Body;

    /// This enum member is active if getBodyKind() == BodyKind::Synthesize.
    BodySynthesizer Synthesizer;

    /// The location of the function body when the body is delayed or skipped.
    ///
    /// This enum member is active if getBodyKind() is BodyKind::Unparsed or
    /// BodyKind::Skipped.
    SourceRange BodyRange;
  };

  GenericParamList *GenericParams;

  /// The generic signature or environment of this function.
  ///
  /// When this function stores only a signature, the generic environment
  /// will be lazily loaded.
  mutable llvm::PointerUnion<GenericSignature *, GenericEnvironment *>
    GenericSigOrEnv;

  CaptureInfo Captures;

  /// Location of the 'throws' token.
  SourceLoc ThrowsLoc;

  ImportAsMemberStatus IAMStatus;

  AbstractFunctionDecl(DeclKind Kind, DeclContext *Parent, DeclName Name,
                       SourceLoc NameLoc, bool Throws, SourceLoc ThrowsLoc,
                       unsigned NumParameterLists,
                       GenericParamList *GenericParams)
      : ValueDecl(Kind, Parent, Name, NameLoc),
        DeclContext(DeclContextKind::AbstractFunctionDecl, Parent),
        Body(nullptr), GenericParams(nullptr), ThrowsLoc(ThrowsLoc) {
    setBodyKind(BodyKind::None);
    setGenericParams(GenericParams);
    AbstractFunctionDeclBits.NumParameterLists = NumParameterLists;
    AbstractFunctionDeclBits.Overridden = false;
    AbstractFunctionDeclBits.Throws = Throws;

    // Verify no bitfield truncation.
    assert(AbstractFunctionDeclBits.NumParameterLists == NumParameterLists);
  }

  void setBodyKind(BodyKind K) {
    AbstractFunctionDeclBits.BodyKind = unsigned(K);
  }

  void setGenericParams(GenericParamList *GenericParams);

  /// Lazily populate the generic environment.
  GenericEnvironment *getLazyGenericEnvironmentSlow() const;

public:
  /// \brief Should this declaration be treated as if annotated with transparent
  /// attribute.
  bool isTransparent() const;

  /// Retrieve the generic signature for this function.
  GenericSignature *getGenericSignature() const;

  /// Retrieve the generic context for this function.
  GenericEnvironment *getGenericEnvironment() const;

  /// Set a lazy generic environment.
  void setLazyGenericEnvironment(LazyMemberLoader *lazyLoader,
                                 GenericSignature *genericSig,
                                 uint64_t genericEnvData);

  /// Set the generic context of this function.
  void setGenericEnvironment(GenericEnvironment *genericEnv);

  // Expose our import as member status
  bool isImportAsMember() const { return IAMStatus.isImportAsMember(); }
  bool isImportAsInstanceMember() const { return IAMStatus.isInstance(); }
  bool isImportAsStaticMember() const { return IAMStatus.isStatic(); }
  uint8_t getSelfIndex() const { return IAMStatus.getSelfIndex(); }
  ImportAsMemberStatus getImportAsMemberStatus() const { return IAMStatus; }

  void setImportAsStaticMember() { IAMStatus.setStatic(); }
  void setSelfIndex(uint8_t idx) { return IAMStatus.setSelfIndex(idx); }

public:
  /// Retrieve the location of the 'throws' keyword, if present.
  SourceLoc getThrowsLoc() const { return ThrowsLoc; }

  /// Returns true if the function body throws.
  bool hasThrows() const { return AbstractFunctionDeclBits.Throws; }

  // FIXME: Hack that provides names with keyword arguments for accessors.
  DeclName getEffectiveFullName() const;

  /// Returns true if the function has a body written in the source file.
  ///
  /// Note that a true return value does not imply that the body was actually
  /// parsed.
  bool hasBody() const {
    return getBodyKind() != BodyKind::None;
  }

  /// Returns the function body, if it was parsed, or nullptr otherwise.
  ///
  /// Note that a null return value does not imply that the source code did not
  /// have a body for this function.
  ///
  /// \sa hasBody()
  BraceStmt *getBody(bool canSynthesize = true) const {
    if (canSynthesize && getBodyKind() == BodyKind::Synthesize) {
      const_cast<AbstractFunctionDecl *>(this)->setBodyKind(BodyKind::None);
      (*Synthesizer)(const_cast<AbstractFunctionDecl *>(this));
    }
    if (getBodyKind() == BodyKind::Parsed ||
        getBodyKind() == BodyKind::TypeChecked) {
      return Body;
    }
    return nullptr;
  }
  void setBody(BraceStmt *S, BodyKind NewBodyKind = BodyKind::Parsed) {
    assert(getBodyKind() != BodyKind::Skipped &&
           "cannot set a body if it was skipped");

    Body = S;
    setBodyKind(NewBodyKind);
  }

  /// \brief Note that the body was skipped for this function.  Function body
  /// cannot be attached after this call.
  void setBodySkipped(SourceRange bodyRange) {
    assert(getBodyKind() == BodyKind::None);
    BodyRange = bodyRange;
    setBodyKind(BodyKind::Skipped);
  }

  /// \brief Note that parsing for the body was delayed.
  void setBodyDelayed(SourceRange bodyRange) {
    assert(getBodyKind() == BodyKind::None);
    BodyRange = bodyRange;
    setBodyKind(BodyKind::Unparsed);
  }

  /// Note that parsing for the body was delayed.
  void setBodySynthesizer(BodySynthesizer synthesizer) {
    assert(getBodyKind() == BodyKind::None);
    Synthesizer = synthesizer;
    setBodyKind(BodyKind::Synthesize);
  }

  /// Note that this is a memberwise initializer and thus the body will be
  /// generated by SILGen.
  void setIsMemberwiseInitializer() {
    assert(getBodyKind() == BodyKind::None);
    assert(isa<ConstructorDecl>(this));
    setBodyKind(BodyKind::MemberwiseInitializer);
  }

  /// If a body has been loaded, flag that it's been type-checked.
  /// This is kindof a hacky operation, but it avoids some unnecessary
  /// duplication of work.
  void setBodyTypeCheckedIfPresent() {
    if (getBodyKind() == BodyKind::Parsed)
      setBodyKind(BodyKind::TypeChecked);
  }

  bool isBodyTypeChecked() const {
    return getBodyKind() == BodyKind::TypeChecked;
  }

  bool isMemberwiseInitializer() const {
    return getBodyKind() == BodyKind::MemberwiseInitializer;
  }

  /// Retrieve the source range of the function body.
  SourceRange getBodySourceRange() const;

  /// Retrieve the source range of the function declaration name + patterns.
  SourceRange getSignatureSourceRange() const;

  CaptureInfo &getCaptureInfo() { return Captures; }
  const CaptureInfo &getCaptureInfo() const { return Captures; }

  /// Retrieve the Objective-C selector that names this method.
  ObjCSelector getObjCSelector(LazyResolver *resolver = nullptr,
                               DeclName preferredName = DeclName()) const;

  /// Determine whether the given method would produce an Objective-C
  /// instance method.
  bool isObjCInstanceMethod() const;

  /// Determine the default argument kind and type for the given argument index
  /// in this declaration, which must be a function or constructor.
  ///
  /// \param Index The index of the argument for which we are querying the
  /// default argument.
  ///
  /// \returns the default argument kind and, if there is a default argument,
  /// the type of the corresponding parameter.
  std::pair<DefaultArgumentKind, Type> getDefaultArg(unsigned Index) const;

  /// Determine whether the name of an argument is an API name by default
  /// depending on the function context.
  bool argumentNameIsAPIByDefault() const;

  /// \brief Returns the "natural" number of argument clauses taken by this
  /// function.  This value is one for free-standing functions, and two for
  /// methods.
  unsigned getNumParameterLists() const {
    return AbstractFunctionDeclBits.NumParameterLists;
  }

  /// \brief Returns the parameter pattern(s) for the function definition that
  /// determine the parameter names bound in the function body.
  ///
  /// The number of "top-level" elements in this pattern will match the number
  /// of argument names in the compound name of the function or constructor.
  MutableArrayRef<ParameterList *> getParameterLists();
  ArrayRef<const ParameterList *> getParameterLists() const {
    auto paramLists =
        const_cast<AbstractFunctionDecl *>(this)->getParameterLists();
    return ArrayRef<const ParameterList *>(paramLists.data(),paramLists.size());
  }
  ParameterList *getParameterList(unsigned i) {
    return getParameterLists()[i];
  }
  const ParameterList *getParameterList(unsigned i) const {
    return getParameterLists()[i];
  }

  /// \brief If this is a method in a type or extension thereof, compute
  /// and return the type to be used for the 'self' argument of the interface
  /// type, or an empty Type() if no 'self' argument should exist.  This can
  /// only be used after name binding has resolved types.
  ///
  /// \param isInitializingCtor Specifies whether we're computing the 'self'
  /// type of an initializing constructor, which accepts an instance 'self'
  /// rather than a metatype 'self'.
  ///
  /// \param wantDynamicSelf Specifies whether the 'self' type should be
  /// wrapped in a DynamicSelfType, which is the case for the 'self' parameter
  /// type inside a class method returning 'Self'.
  Type computeInterfaceSelfType(bool isInitializingCtor=false,
                                bool wantDynamicSelf=false);

  /// \brief This method returns the implicit 'self' decl.
  ///
  /// Note that some functions don't have an implicit 'self' decl, for example,
  /// free functions.  In this case nullptr is returned.
  const ParamDecl *getImplicitSelfDecl() const {
    return const_cast<AbstractFunctionDecl*>(this)->getImplicitSelfDecl();
  }
  ParamDecl *getImplicitSelfDecl();

  /// \brief Retrieve the set of parameters to a generic function, or null if
  /// this function is not generic.
  GenericParamList *getGenericParams() const { return GenericParams; }

  /// \brief Determine whether this is a generic function, which can only be
  /// used when each of the archetypes is bound to a particular concrete type.
  bool isGeneric() const { return GenericParams != nullptr; }

  /// Retrieve the declaration that this method overrides, if any.
  AbstractFunctionDecl *getOverriddenDecl() const;

  /// Returns true if a function declaration overrides a given
  /// method from its direct or indirect superclass.
  bool isOverridingDecl(const AbstractFunctionDecl *method) const;

  /// Whether the declaration is later overridden in the module
  ///
  /// Overrides are resolved during type checking; only query this field after
  /// the whole module has been checked
  bool isOverridden() const { return AbstractFunctionDeclBits.Overridden; }

  /// The declaration has been overridden in the module
  ///
  /// Resolved during type checking
  void setIsOverridden() { AbstractFunctionDeclBits.Overridden = true; }

  /// Set information about the foreign error convention used by this
  /// declaration.
  void setForeignErrorConvention(const ForeignErrorConvention &convention);

  /// Get information about the foreign error convention used by this
  /// declaration, given that it is @objc and 'throws'.
  Optional<ForeignErrorConvention> getForeignErrorConvention() const;

  /// If this is a foreign C function imported as a method, get the index of
  /// the foreign parameter imported as `self`. If the function is imported
  /// as a static method, `-1` is returned to represent the `self` parameter
  /// being dropped altogether. `None` is returned for a normal function
  /// or method.
  Optional<int> getForeignFunctionAsMethodSelfParameterIndex() const;
  
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_AbstractFunctionDecl &&
           D->getKind() <= DeclKind::Last_AbstractFunctionDecl;
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::AbstractFunctionDecl;
  }
  
  /// True if the declaration is forced to be statically dispatched.
  bool hasForcedStaticDispatch() const;
  
  using DeclContext::operator new;
  using Decl::getASTContext;
};

class OperatorDecl;


/// FuncDecl - 'func' declaration.
class FuncDecl final : public AbstractFunctionDecl,
    private llvm::TrailingObjects<FuncDecl, ParameterList *> {
  friend class AbstractFunctionDecl;
  friend TrailingObjects;

  SourceLoc StaticLoc;  // Location of the 'static' token or invalid.
  SourceLoc FuncLoc;    // Location of the 'func' token.
  SourceLoc AccessorKeywordLoc; // Location of the accessor keyword, e.g. 'set'.

  TypeLoc FnRetType;

  /// If this declaration is part of an overload set, determine if we've
  /// searched for a common overload amongst all overloads, or if we've found
  /// one.
  unsigned HaveSearchedForCommonOverloadReturnType : 1;
  unsigned HaveFoundCommonOverloadReturnType : 1;

  /// \brief If this FuncDecl is an accessor for a property, this indicates
  /// which property and what kind of accessor.
  llvm::PointerIntPair<AbstractStorageDecl*, 3, AccessorKind> AccessorDecl;
  llvm::PointerUnion<FuncDecl *, BehaviorRecord *>
    OverriddenOrBehaviorParamDecl;
  llvm::PointerIntPair<OperatorDecl *, 3,
                       AddressorKind> OperatorAndAddressorKind;

  FuncDecl(SourceLoc StaticLoc, StaticSpellingKind StaticSpelling,
           SourceLoc FuncLoc,
           DeclName Name, SourceLoc NameLoc,
           bool Throws, SourceLoc ThrowsLoc,
           SourceLoc AccessorKeywordLoc,
           unsigned NumParameterLists,
           GenericParamList *GenericParams, DeclContext *Parent)
    : AbstractFunctionDecl(DeclKind::Func, Parent,
                           Name, NameLoc,
                           Throws, ThrowsLoc,
                           NumParameterLists, GenericParams),
      StaticLoc(StaticLoc), FuncLoc(FuncLoc),
      AccessorKeywordLoc(AccessorKeywordLoc),
      OverriddenOrBehaviorParamDecl(),
      OperatorAndAddressorKind(nullptr, AddressorKind::NotAddressor) {
    FuncDeclBits.IsStatic =
      StaticLoc.isValid() || StaticSpelling != StaticSpellingKind::None;
    FuncDeclBits.StaticSpelling = static_cast<unsigned>(StaticSpelling);
    assert(NumParameterLists > 0 && "Must have at least an empty tuple arg");
    FuncDeclBits.Mutating = false;
    FuncDeclBits.HasDynamicSelf = false;
    FuncDeclBits.ForcedStaticDispatch = false;
        
    HaveSearchedForCommonOverloadReturnType = false;
    HaveFoundCommonOverloadReturnType = false;
  }

  static FuncDecl *createImpl(ASTContext &Context, SourceLoc StaticLoc,
                              StaticSpellingKind StaticSpelling,
                              SourceLoc FuncLoc,
                              DeclName Name, SourceLoc NameLoc,
                              bool Throws, SourceLoc ThrowsLoc,
                              SourceLoc AccessorKeywordLoc,
                              GenericParamList *GenericParams,
                              unsigned NumParameterLists,
                              DeclContext *Parent,
                              ClangNode ClangN);

public:
  /// Factory function only for use by deserialization.
  static FuncDecl *createDeserialized(ASTContext &Context, SourceLoc StaticLoc,
                                      StaticSpellingKind StaticSpelling,
                                      SourceLoc FuncLoc,
                                      DeclName Name, SourceLoc NameLoc,
                                      bool Throws, SourceLoc ThrowsLoc,
                                      SourceLoc AccessorKeywordLoc,
                                      GenericParamList *GenericParams,
                                      unsigned NumParameterLists,
                                      DeclContext *Parent);

  static FuncDecl *create(ASTContext &Context, SourceLoc StaticLoc,
                          StaticSpellingKind StaticSpelling,
                          SourceLoc FuncLoc,
                          DeclName Name, SourceLoc NameLoc,
                          bool Throws, SourceLoc ThrowsLoc,
                          SourceLoc AccessorKeywordLoc,
                          GenericParamList *GenericParams,
                          ArrayRef<ParameterList *> ParameterLists,
                          TypeLoc FnRetType, DeclContext *Parent,
                          ClangNode ClangN = ClangNode());

  bool isStatic() const {
    return FuncDeclBits.IsStatic;
  }
  /// \returns the way 'static'/'class' was spelled in the source.
  StaticSpellingKind getStaticSpelling() const {
    return static_cast<StaticSpellingKind>(FuncDeclBits.StaticSpelling);
  }
  /// \returns the way 'static'/'class' should be spelled for this declaration.
  StaticSpellingKind getCorrectStaticSpelling() const;
  void setStatic(bool IsStatic = true) {
    FuncDeclBits.IsStatic = IsStatic;
  }
  bool isMutating() const {
    return FuncDeclBits.Mutating;
  }
  void setMutating(bool Mutating = true) {
    FuncDeclBits.Mutating = Mutating;
  }
  
  /// \brief Returns the parameter lists(s) for the function definition.
  ///
  /// The number of "top-level" elements will match the number of argument names
  /// in the compound name of the function or constructor.
  MutableArrayRef<ParameterList *> getParameterLists() {
    return {getTrailingObjects<ParameterList *>(), getNumParameterLists()};
  }
  ArrayRef<const ParameterList *> getParameterLists() const {
    return {getTrailingObjects<ParameterList *>(), getNumParameterLists()};
  }
  ParameterList *getParameterList(unsigned i) {
    return getParameterLists()[i];
  }
  const ParameterList *getParameterList(unsigned i) const {
    return getParameterLists()[i];
  }
  

  bool getHaveSearchedForCommonOverloadReturnType() {
    return HaveSearchedForCommonOverloadReturnType;
  }
  void setHaveSearchedForCommonOverloadReturnType(bool b = true) {
    HaveSearchedForCommonOverloadReturnType = b;
  }
  bool getHaveFoundCommonOverloadReturnType() {
    return HaveFoundCommonOverloadReturnType;
  }
  void setHaveFoundCommonOverloadReturnType(bool b = true) {
    HaveFoundCommonOverloadReturnType = b;
  }

  /// \returns true if this is non-mutating due to applying a 'mutating'
  /// attribute. For example a "mutating set" accessor.
  bool isExplicitNonMutating() const;

  void setDeserializedSignature(ArrayRef<ParameterList *> ParameterLists,
                                TypeLoc FnRetType);

  SourceLoc getStaticLoc() const { return StaticLoc; }
  SourceLoc getFuncLoc() const { return FuncLoc; }
  SourceLoc getAccessorKeywordLoc() const {return AccessorKeywordLoc; }

  SourceLoc getStartLoc() const {
    return StaticLoc.isValid() && !isAccessor() ? StaticLoc : FuncLoc;
  }
  SourceRange getSourceRange() const;

  TypeLoc &getBodyResultTypeLoc() { return FnRetType; }
  const TypeLoc &getBodyResultTypeLoc() const { return FnRetType; }

  /// Retrieve the result interface type of this function.
  Type getResultInterfaceType() const;

  /// isUnaryOperator - Determine whether this is a unary operator
  /// implementation.  This check is a syntactic rather than type-based check,
  /// which looks at the number of parameters specified, in order to allow
  /// for the definition of unary operators on tuples, as in:
  ///
  ///   prefix func + (param : (a:Int, b:Int))
  ///
  /// This also allows the unary-operator-ness of a func decl to be determined
  /// prior to type checking.
  bool isUnaryOperator() const;
  
  /// isBinaryOperator - Determine whether this is a binary operator
  /// implementation.  This check is a syntactic rather than type-based check,
  /// which looks at the number of parameters specified, in order to allow
  /// distinguishing a binary operator from a unary operator on tuples, as in:
  ///
  ///   prefix func + (_:(a:Int, b:Int)) // unary operator +(1,2)
  ///   infix func  + (a:Int, b:Int)     // binary operator 1 + 2
  ///
  /// This also allows the binary-operator-ness of a func decl to be determined
  /// prior to type checking.
  bool isBinaryOperator() const;
  
  /// makeAccessor - Note that this function is an accessor for the given
  /// VarDecl or SubscriptDecl.
  void makeAccessor(AbstractStorageDecl *D, AccessorKind Kind) {
    assert(Kind != AccessorKind::NotAccessor && "Must specify an accessor kind");
    AccessorDecl.setPointerAndInt(D, Kind);
  }

  /// Set the addressor kind of this address or mutableAddress declaration.
  void setAddressorKind(AddressorKind kind) {
    assert(kind != AddressorKind::NotAddressor);
    OperatorAndAddressorKind.setInt(kind);
  }

  AbstractStorageDecl *getAccessorStorageDecl() const {
    return AccessorDecl.getPointer();
  }

  AccessorKind getAccessorKind() const {
    if (AccessorDecl.getPointer() == nullptr)
      return AccessorKind::NotAccessor;
    return AccessorDecl.getInt();
  }

  AddressorKind getAddressorKind() const {
    return OperatorAndAddressorKind.getInt();
  }

  bool isGetter() const { return getAccessorKind() == AccessorKind::IsGetter; }
  bool isSetter() const { return getAccessorKind() == AccessorKind::IsSetter; }

  /// isGetterOrSetter - Determine whether this is a getter or a setter vs.
  /// a normal function.
  bool isGetterOrSetter() const { return isGetter() || isSetter(); }
  bool isObservingAccessor() const {
    return getAccessorKind() == AccessorKind::IsDidSet ||
           getAccessorKind() == AccessorKind::IsWillSet;
  }
  bool isAccessor() const {
    return getAccessorKind() != AccessorKind::NotAccessor;
  }

  /// Determine whether this function has a dynamic \c Self return
  /// type.
  bool hasDynamicSelf() const { return FuncDeclBits.HasDynamicSelf; }

  /// Set whether this function has a dynamic \c Self return or not.
  void setDynamicSelf(bool hasDynamicSelf) { 
    FuncDeclBits.HasDynamicSelf = hasDynamicSelf;
  }

  void getLocalCaptures(SmallVectorImpl<CapturedValue> &Result) const {
    return getCaptureInfo().getLocalCaptures(Result);
  }
  
  /// Get the supertype method this method overrides, if any.
  FuncDecl *getOverriddenDecl() const {
    return OverriddenOrBehaviorParamDecl.dyn_cast<FuncDecl *>();
  }
  void setOverriddenDecl(FuncDecl *over) {
    // FIXME: Hack due to broken class circularity checking.
    if (over == this) return;

    // A function cannot be an override if it is also a derived global decl
    // (since derived decls are at global scope).
    assert((!OverriddenOrBehaviorParamDecl
            || !OverriddenOrBehaviorParamDecl.is<FuncDecl*>())
         && "function can only be one of override, derived, or behavior param");
    OverriddenOrBehaviorParamDecl = over;
    over->setIsOverridden();
  }
  
  /// Get the property behavior this function serves as a parameter for, if
  /// any.
  BehaviorRecord *getParamBehavior() const {
    return OverriddenOrBehaviorParamDecl
      .dyn_cast<BehaviorRecord *>();
  }
  
  void setParamBehavior(BehaviorRecord *behavior) {
    // Behavior param blocks cannot be overrides or derived.
    assert((!OverriddenOrBehaviorParamDecl
            || !OverriddenOrBehaviorParamDecl
                  .is<BehaviorRecord *>())
         && "function can only be one of override, derived, or behavior param");
    OverriddenOrBehaviorParamDecl = behavior;
  }
  
  OperatorDecl *getOperatorDecl() const {
    return OperatorAndAddressorKind.getPointer();
  }
  void setOperatorDecl(OperatorDecl *o) {
    assert(isOperator() && "can't set an OperatorDecl for a non-operator");
    OperatorAndAddressorKind.setPointer(o);
  }
  
  /// Returns true if the function is forced to be statically dispatched.
  bool hasForcedStaticDispatch() const {
    return FuncDeclBits.ForcedStaticDispatch;
  }
  void setForcedStaticDispatch(bool flag) {
    FuncDeclBits.ForcedStaticDispatch = flag;
  }

  static bool classof(const Decl *D) { return D->getKind() == DeclKind::Func; }
  static bool classof(const AbstractFunctionDecl *D) {
    return classof(static_cast<const Decl*>(D));
  }
  static bool classof(const DeclContext *DC) {
    if (auto fn = dyn_cast<AbstractFunctionDecl>(DC))
      return classof(fn);
    return false;
  }

  /// True if the function is a defer body.
  bool isDeferBody() const;
};
  
/// \brief This represents a 'case' declaration in an 'enum', which may declare
/// one or more individual comma-separated EnumElementDecls.
class EnumCaseDecl final : public Decl,
    private llvm::TrailingObjects<EnumCaseDecl, EnumElementDecl *> {
  friend TrailingObjects;
  SourceLoc CaseLoc;
  
  /// The number of tail-allocated element pointers.
  unsigned NumElements;
  
  EnumCaseDecl(SourceLoc CaseLoc,
               ArrayRef<EnumElementDecl *> Elements,
               DeclContext *DC)
    : Decl(DeclKind::EnumCase, DC),
      CaseLoc(CaseLoc), NumElements(Elements.size())
  {
    std::uninitialized_copy(Elements.begin(), Elements.end(),
                            getTrailingObjects<EnumElementDecl *>());
  }

public:
  static EnumCaseDecl *create(SourceLoc CaseLoc,
                              ArrayRef<EnumElementDecl*> Elements,
                              DeclContext *DC);
  
  /// Get the list of elements declared in this case.
  ArrayRef<EnumElementDecl *> getElements() const {
    return {getTrailingObjects<EnumElementDecl *>(), NumElements};
  }
  
  SourceLoc getLoc() const {
    return CaseLoc;
  }
  
  SourceRange getSourceRange() const;
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::EnumCase;
  }
};

/// \brief This represents a single case of an 'enum' declaration.
///
/// For example, the X, Y, and Z in this enum:
///
/// \code
///   enum V {
///     case X(Int), Y(Int)
///     case Z
///   }
/// \endcode
///
/// The type of an EnumElementDecl is always the EnumType for the containing
/// enum. EnumElementDecls are represented in the AST as members of their
/// parent EnumDecl, although syntactically they are subordinate to the
/// EnumCaseDecl.
class EnumElementDecl : public ValueDecl {
  /// This is the type specified with the enum element, for
  /// example 'Int' in 'case Y(Int)'.  This is null if there is no type
  /// associated with this element, as in 'case Z' or in all elements of enum
  /// definitions.
  TypeLoc ArgumentType;
  
  SourceLoc EqualsLoc;
  
  /// The raw value literal for the enum element, or null.
  LiteralExpr *RawValueExpr;
  /// The type-checked raw value expression.
  Expr *TypeCheckedRawValueExpr = nullptr;
  
public:
  EnumElementDecl(SourceLoc IdentifierLoc, Identifier Name,
                  TypeLoc ArgumentType,
                  bool HasArgumentType,
                  SourceLoc EqualsLoc,
                  LiteralExpr *RawValueExpr,
                  DeclContext *DC)
  : ValueDecl(DeclKind::EnumElement, DC, Name, IdentifierLoc),
    ArgumentType(ArgumentType),
    EqualsLoc(EqualsLoc),
    RawValueExpr(RawValueExpr)
  {
    EnumElementDeclBits.Recursiveness =
        static_cast<unsigned>(ElementRecursiveness::NotRecursive);
    EnumElementDeclBits.HasArgumentType = HasArgumentType;
  }

  /// \returns false if there was an error during the computation rendering the
  /// EnumElementDecl invalid, true otherwise.
  bool computeType();

  Type getArgumentInterfaceType() const;

  TypeLoc &getArgumentTypeLoc() { return ArgumentType; }
  const TypeLoc &getArgumentTypeLoc() const { return ArgumentType; }

  bool hasRawValueExpr() const { return RawValueExpr; }
  LiteralExpr *getRawValueExpr() const { return RawValueExpr; }
  void setRawValueExpr(LiteralExpr *e) { RawValueExpr = e; }
  
  Expr *getTypeCheckedRawValueExpr() const {
    return TypeCheckedRawValueExpr;
  }
  void setTypeCheckedRawValueExpr(Expr *e) {
    TypeCheckedRawValueExpr = e;
  }
  
  /// Return the containing EnumDecl.
  EnumDecl *getParentEnum() const {
    return cast<EnumDecl>(getDeclContext());
  }
  
  /// Return the containing EnumCaseDecl.
  EnumCaseDecl *getParentCase() const;

  SourceLoc getStartLoc() const {
    return getNameLoc();
  }
  SourceRange getSourceRange() const;
  
  ElementRecursiveness getRecursiveness() const {
    return
      static_cast<ElementRecursiveness>(EnumElementDeclBits.Recursiveness);
  }
  
  void setRecursiveness(ElementRecursiveness recursiveness) {
    EnumElementDeclBits.Recursiveness = static_cast<unsigned>(recursiveness);
  }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::EnumElement;
  }

  /// True if the case is marked 'indirect'.
  bool isIndirect() const {
    return getAttrs().hasAttribute<IndirectAttr>();
  }
};
  
inline SourceRange EnumCaseDecl::getSourceRange() const {
  auto subRange = getElements().back()->getSourceRange();
  if (subRange.isValid())
    return {CaseLoc, subRange.End};
  return {};
}

/// Describes the kind of initializer.
enum class CtorInitializerKind {
  /// A designated initializer is an initializer responsible for initializing
  /// the stored properties of the current class and chaining to a superclass's
  /// designated initializer (for non-root classes).
  ///
  /// Designated initializers are never inherited.
  Designated,

  /// A convenience initializer is an initializer that initializes a complete
  /// object by delegating to another initializer (eventually reaching a
  /// designated initializer).
  ///
  /// Convenience initializers are inherited into subclasses that override
  /// all of their superclass's designated initializers.
  Convenience,

  /// A convenience factory initializer is a convenience initializer introduced
  /// by an imported Objective-C factory method.
  ///
  /// Convenience factory initializers cannot be expressed directly in
  /// Swift; rather, they are produced by the Clang importer when importing
  /// an instancetype factory method from Objective-C.
  ConvenienceFactory,

  /// A factory initializer is an initializer that is neither designated nor
  /// convenience: it can be used to create an object of the given type, but
  /// cannot be chained to via "super.init" nor is it inherited.
  ///
  /// A factory initializer is written with a return type of the class name
  /// itself. FIXME: However, this is only a presentation form, and at present
  /// the only factory initializers are produced by importing an Objective-C
  /// factory method that does not return instancetype.
  ///
  /// FIXME: Arguably, structs and enums only have factory initializers, and
  /// using designated initializers for them is a misnomer.
  Factory
};

/// ConstructorDecl - Declares a constructor for a type.  For example:
///
/// \code
/// struct X {
///   var x : Int
///   init(i : Int) {
///      x = i
///   }
/// }
/// \endcode
class ConstructorDecl : public AbstractFunctionDecl {
  /// The failability of this initializer, which is an OptionalTypeKind.
  unsigned Failability : 2;

  /// The location of the '!' or '?' for a failable initializer.
  SourceLoc FailabilityLoc;

  ParameterList *ParameterLists[2];

  /// The interface type of the initializing constructor.
  Type InitializerInterfaceType;

  /// The typechecked call to super.init expression, which needs to be
  /// inserted at the end of the initializer by SILGen.
  Expr *CallToSuperInit = nullptr;

  /// The constructor this overrides, which only makes sense when
  /// both the overriding and the overridden constructors are abstract.
  ConstructorDecl *OverriddenDecl = nullptr;

public:
  ConstructorDecl(DeclName Name, SourceLoc ConstructorLoc, 
                  OptionalTypeKind Failability, SourceLoc FailabilityLoc,
                  bool Throws, SourceLoc ThrowsLoc,
                  ParamDecl *SelfParam, ParameterList *BodyParams,
                  GenericParamList *GenericParams, 
                  DeclContext *Parent);

  void setParameterLists(ParamDecl *selfParam, ParameterList *bodyParams);

  SourceLoc getConstructorLoc() const { return getNameLoc(); }
  SourceLoc getStartLoc() const { return getConstructorLoc(); }
  SourceRange getSourceRange() const;

  /// getArgumentInterfaceType - get the interface type of the argument tuple
  Type getArgumentInterfaceType() const;

  /// \brief Get the interface type of the constructed object.
  Type getResultInterfaceType() const;

  /// Get the interface type of the initializing constructor.
  Type getInitializerInterfaceType();
  void setInitializerInterfaceType(Type t);

  /// Get the typechecked call to super.init expression, which needs to be
  /// inserted at the end of the initializer by SILGen.
  Expr *getSuperInitCall() { return CallToSuperInit; }
  void setSuperInitCall(Expr *CallExpr) { CallToSuperInit = CallExpr; }

  MutableArrayRef<ParameterList *> getParameterLists() {
    return { ParameterLists, 2 };
  }
  ArrayRef<const ParameterList *> getParameterLists() const {
    return AbstractFunctionDecl::getParameterLists();
  }
  ParameterList *getParameterList(unsigned i) {
    return getParameterLists()[i];
  }
  const ParameterList *getParameterList(unsigned i) const {
    return getParameterLists()[i];
  }
  
  /// Returns the normal parameters to the initializer, not including self.
  ParameterList *getParameters() { return ParameterLists[1]; }

  /// Returns the normal parameters to the initializer, not including self.
  const ParameterList *getParameters() const { return ParameterLists[1]; }


  /// Specifies the kind of initialization call performed within the body
  /// of the constructor, e.g., self.init or super.init.
  enum class BodyInitKind {
    /// There are no calls to self.init or super.init.
    None,
    /// There is a call to self.init, which delegates to another (peer)
    /// initializer.
    Delegating,
    /// There is a call to super.init, which chains to a superclass initializer.
    Chained,
    /// There are no calls to self.init or super.init explicitly in the body of
    /// the constructor, but a 'super.init' call will be implicitly added
    /// by semantic analysis.
    ImplicitChained
  };

  /// Determine whether the body of this constructor contains any delegating
  /// or superclass initializations (\c self.init or \c super.init,
  /// respectively) within its body.
  ///
  /// \param diags If non-null, this check will ensure that the constructor
  /// body is consistent in its use of delegation vs. chaining and emit any
  /// diagnostics through the given diagnostic engine.
  ///
  /// \param init If non-null and there is an explicit \c self.init or
  /// \c super.init within the body, will be set to point at that
  /// initializer.
  BodyInitKind getDelegatingOrChainedInitKind(DiagnosticEngine *diags,
                                              ApplyExpr **init = nullptr) const;

  /// Whether this constructor is required.
  bool isRequired() const {
    return getAttrs().hasAttribute<RequiredAttr>();
  }

  /// Determine the kind of initializer this is.
  CtorInitializerKind getInitKind() const {
    return static_cast<CtorInitializerKind>(ConstructorDeclBits.InitKind);
  }

  /// Set whether this is a convenience initializer.
  void setInitKind(CtorInitializerKind kind) {
    ConstructorDeclBits.InitKind = static_cast<unsigned>(kind);
  }

  /// Whether this is a designated initializer.
  bool isDesignatedInit() const {
    return getInitKind() == CtorInitializerKind::Designated;
  }

  /// Whether this is a convenience initializer.
  bool isConvenienceInit() const {
    return getInitKind() == CtorInitializerKind::Convenience ||
           getInitKind() == CtorInitializerKind::ConvenienceFactory;
  }

  /// Whether this is a factory initializer.
  bool isFactoryInit() const {
    switch (getInitKind()) {
    case CtorInitializerKind::Designated:
    case CtorInitializerKind::Convenience:
      return false;
        
    case CtorInitializerKind::Factory:
    case CtorInitializerKind::ConvenienceFactory:
      return true;
    }
    llvm_unreachable("bad CtorInitializerKind");
  }

  /// Determine whether this initializer is inheritable.
  bool isInheritable() const {
    switch (getInitKind()) {
    case CtorInitializerKind::Designated:
    case CtorInitializerKind::Factory:
      return false;

    case CtorInitializerKind::Convenience:
    case CtorInitializerKind::ConvenienceFactory:
      return true;
    }
    llvm_unreachable("bad CtorInitializerKind");
  }

  /// Determine the failability of the initializer.
  OptionalTypeKind getFailability() const {
    return static_cast<OptionalTypeKind>(Failability);
  }

  /// Retrieve the location of the '!' or '?' in a failable initializer.
  SourceLoc getFailabilityLoc() const { return FailabilityLoc; }

  /// Whether the implementation of this method is a stub that traps at runtime.
  bool hasStubImplementation() const {
    return ConstructorDeclBits.HasStubImplementation;
  }

  /// Set whether the implementation of this method is a stub that
  /// traps at runtime.
  void setStubImplementation(bool stub) {
    ConstructorDeclBits.HasStubImplementation = stub;
  }

  ConstructorDecl *getOverriddenDecl() const { return OverriddenDecl; }
  void setOverriddenDecl(ConstructorDecl *over) {
    // FIXME: Hack due to broken class circularity checking.
    if (over == this) return;

    OverriddenDecl = over;
    over->setIsOverridden();
  }

  /// Determine whether this initializer falls into the special case for
  /// Objective-C initializers with selectors longer than "init", e.g.,
  /// \c initForMemory.
  ///
  /// In such cases, one can write the Swift initializer
  /// with a single parameter of type '()', e.g,
  ///
  /// \code
  /// @objc init(forMemory: ())
  /// \endcode
  bool isObjCZeroParameterWithLongSelector() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Constructor;
  }
  static bool classof(const AbstractFunctionDecl *D) {
    return classof(static_cast<const Decl*>(D));
  }
  static bool classof(const DeclContext *DC) {
    if (auto fn = dyn_cast<AbstractFunctionDecl>(DC))
      return classof(fn);
    return false;
  }
};

/// DestructorDecl - Declares a destructor for a type.  For example:
///
/// \code
/// struct X {
///   var fd : Int
///   deinit {
///      close(fd)
///   }
/// }
/// \endcode
class DestructorDecl : public AbstractFunctionDecl {
  ParameterList *SelfParameter;
public:
  DestructorDecl(Identifier NameHack, SourceLoc DestructorLoc,
                 ParamDecl *selfDecl, DeclContext *Parent);
  
  void setSelfDecl(ParamDecl *selfDecl);

  MutableArrayRef<ParameterList *> getParameterLists() {
    return { &SelfParameter, 1 };
  }
  ArrayRef<const ParameterList *> getParameterLists() const {
    return { &SelfParameter, 1 };
  }


  
  SourceLoc getDestructorLoc() const { return getNameLoc(); }
  SourceLoc getStartLoc() const { return getDestructorLoc(); }
  SourceRange getSourceRange() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Destructor;
  }
  static bool classof(const AbstractFunctionDecl *D) {
    return classof(static_cast<const Decl*>(D));
  }
  static bool classof(const DeclContext *DC) {
    if (auto fn = dyn_cast<AbstractFunctionDecl>(DC))
      return classof(fn);
    return false;
  }
};

/// Declares a precedence group.  For example:
///
/// \code
/// precedencegroup MultiplicativePrecedence {
///   associativity: right
///   higherThan: AdditivePrecedence
/// }
/// \endcode
class PrecedenceGroupDecl : public Decl {
public:
  struct Relation {
    SourceLoc NameLoc;
    Identifier Name;
    PrecedenceGroupDecl *Group;
  };

private:
  SourceLoc PrecedenceGroupLoc, NameLoc, LBraceLoc, RBraceLoc;
  SourceLoc AssociativityKeywordLoc, AssociativityValueLoc;
  SourceLoc AssignmentKeywordLoc, AssignmentValueLoc;
  SourceLoc HigherThanLoc, LowerThanLoc;
  Identifier Name;
  unsigned NumHigherThan, NumLowerThan;
  // Tail-allocated array of Relations

  Relation *getHigherThanBuffer() {
    return reinterpret_cast<Relation*>(this + 1);
  }
  const Relation *getHigherThanBuffer() const {
    return reinterpret_cast<const Relation*>(this + 1);
  }
  Relation *getLowerThanBuffer() {
    return getHigherThanBuffer() + NumHigherThan;
  }
  const Relation *getLowerThanBuffer() const {
    return getHigherThanBuffer() + NumHigherThan;
  }

  PrecedenceGroupDecl(DeclContext *DC,
                      SourceLoc precedenceGroupLoc,
                      SourceLoc nameLoc, Identifier name,
                      SourceLoc lbraceLoc,
                      SourceLoc associativityKeywordLoc,
                      SourceLoc associativityValueLoc,
                      Associativity associativity,
                      SourceLoc assignmentKeywordLoc,
                      SourceLoc assignmentValueLoc,
                      bool isAssignment,
                      SourceLoc higherThanLoc, ArrayRef<Relation> higherThan,
                      SourceLoc lowerThanLoc, ArrayRef<Relation> lowerThan,
                      SourceLoc rbraceLoc);

public:
  static PrecedenceGroupDecl *create(DeclContext *dc,
                                     SourceLoc precedenceGroupLoc,
                                     SourceLoc nameLoc,
                                     Identifier name,
                                     SourceLoc lbraceLoc,
                                     SourceLoc associativityKeywordLoc,
                                     SourceLoc associativityValueLoc,
                                     Associativity associativity,
                                     SourceLoc assignmentKeywordLoc,
                                     SourceLoc assignmentValueLoc,
                                     bool isAssignment,
                                     SourceLoc higherThanLoc,
                                     ArrayRef<Relation> higherThan,
                                     SourceLoc lowerThanLoc,
                                     ArrayRef<Relation> lowerThan,
                                     SourceLoc rbraceLoc);


  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const {
    return { PrecedenceGroupLoc, RBraceLoc };
  }

  /// Return the location of 'precedencegroup' in:
  ///   precedencegroup MultiplicativePrecedence { ... }
  SourceLoc getPrecedenceGroupLoc() const { return PrecedenceGroupLoc; }

  /// Return the location of 'MultiplicativePrecedence' in:
  ///   precedencegroup MultiplicativePrecedence { ... }
  SourceLoc getNameLoc() const {
    return NameLoc;
  }

  Identifier getName() const {
    return Name;
  }

  SourceLoc getLBraceLoc() const { return LBraceLoc; }
  SourceLoc getRBraceLoc() const { return RBraceLoc; }

  bool isAssociativityImplicit() const {
    return AssociativityKeywordLoc.isInvalid();
  }

  /// Return the location of 'associativity' in:
  ///   associativity: left
  SourceLoc getAssociativityKeywordLoc() const {
    return AssociativityKeywordLoc;
  }

  /// Return the location of 'right' in:
  ///   associativity: right
  SourceLoc getAssociativityValueLoc() const {
    return AssociativityValueLoc;
  }

  Associativity getAssociativity() const {
    return Associativity(PrecedenceGroupDeclBits.Associativity);
  }
  bool isLeftAssociative() const {
    return getAssociativity() == Associativity::Left;
  }
  bool isRightAssociative() const {
    return getAssociativity() == Associativity::Right;
  }
  bool isNonAssociative() const {
    return getAssociativity() == Associativity::None;
  }

  bool isAssignmentImplicit() const {
    return AssignmentKeywordLoc.isInvalid();
  }

  /// Return the location of 'assignment' in:
  ///   assignment: true
  SourceLoc getAssignmentKeywordLoc() const {
    return AssignmentKeywordLoc;
  }

  /// Return the location of 'assignment' in:
  ///   assignment: true
  SourceLoc getAssignmentValueLoc() const {
    return AssignmentValueLoc;
  }

  bool isAssignment() const {
    return PrecedenceGroupDeclBits.IsAssignment;
  }

  bool isHigherThanImplicit() const {
    return HigherThanLoc.isInvalid();
  }

  /// Return the location of 'higherThan' in:
  ///   higherThan: AdditivePrecedence
  SourceLoc getHigherThanLoc() const {
    return HigherThanLoc;
  }

  ArrayRef<Relation> getHigherThan() const {
    return { getHigherThanBuffer(), NumHigherThan };
  }
  MutableArrayRef<Relation> getMutableHigherThan() {
    return { getHigherThanBuffer(), NumHigherThan };
  }

  bool isLowerThanImplicit() const {
    return LowerThanLoc.isInvalid();
  }

  /// Return the location of 'lowerThan' in:
  ///   lowerThan: MultiplicativePrecedence
  SourceLoc getLowerThanLoc() const {
    return LowerThanLoc;
  }

  ArrayRef<Relation> getLowerThan() const {
    return { getLowerThanBuffer(), NumLowerThan };
  }
  MutableArrayRef<Relation> getMutableLowerThan() {
    return { getLowerThanBuffer(), NumLowerThan };
  }

  void collectOperatorKeywordRanges(SmallVectorImpl<CharSourceRange> &Ranges);

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::PrecedenceGroup;
  }
};

/// Abstract base class of operator declarations.
class OperatorDecl : public Decl {
  SourceLoc OperatorLoc, NameLoc;
  
  Identifier name;

public:
  OperatorDecl(DeclKind kind,
               DeclContext *DC,
               SourceLoc OperatorLoc,
               Identifier Name,
               SourceLoc NameLoc)
    : Decl(kind, DC),
      OperatorLoc(OperatorLoc), NameLoc(NameLoc),
      name(Name) {}
  
  SourceLoc getLoc() const { return NameLoc; }

  SourceLoc getOperatorLoc() const { return OperatorLoc; }
  SourceLoc getNameLoc() const { return NameLoc; }
  Identifier getName() const { return name; }
  
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_OperatorDecl
        && D->getKind() <= DeclKind::Last_OperatorDecl;
  }
};

/// Declares the behavior of an infix operator. For example:
///
/// \code
/// infix operator /+/ : AdditivePrecedence
/// \endcode
class InfixOperatorDecl : public OperatorDecl {
  SourceLoc ColonLoc, PrecedenceGroupNameLoc;
  Identifier PrecedenceGroupName;
  PrecedenceGroupDecl *PrecedenceGroup = nullptr;

public:
  InfixOperatorDecl(DeclContext *DC,
                    SourceLoc operatorLoc,
                    Identifier name,
                    SourceLoc nameLoc,
                    SourceLoc colonLoc,
                    Identifier precedenceGroupName,
                    SourceLoc precedenceGroupNameLoc)
    : OperatorDecl(DeclKind::InfixOperator, DC, operatorLoc, name, nameLoc),
      ColonLoc(colonLoc), PrecedenceGroupNameLoc(precedenceGroupNameLoc),
      PrecedenceGroupName(precedenceGroupName) {
  }

  SourceLoc getEndLoc() const {
    if (PrecedenceGroupName.empty())
      return getNameLoc();
    return PrecedenceGroupNameLoc;
  }
  SourceRange getSourceRange() const {
    return { getOperatorLoc(), getEndLoc() };
  }
  
  SourceLoc getColonLoc() const { return ColonLoc; }
  SourceLoc getPrecedenceGroupNameLoc() const { return PrecedenceGroupNameLoc; }

  Identifier getPrecedenceGroupName() const { return PrecedenceGroupName; }
  PrecedenceGroupDecl *getPrecedenceGroup() const { return PrecedenceGroup; }
  void setPrecedenceGroup(PrecedenceGroupDecl *PGD) {
    PrecedenceGroup = PGD;
  }

  /// True if this decl's attributes conflict with those declared by another
  /// operator.
  bool conflictsWith(InfixOperatorDecl *other) {
    return getPrecedenceGroup() != other->getPrecedenceGroup();
  }
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::InfixOperator;
  }
};
  
/// Declares the behavior of a prefix operator. For example:
///
/// \code
/// prefix operator /+/ {}
/// \endcode
class PrefixOperatorDecl : public OperatorDecl {
public:
  PrefixOperatorDecl(DeclContext *DC, SourceLoc OperatorLoc, Identifier Name,
                     SourceLoc NameLoc)
    : OperatorDecl(DeclKind::PrefixOperator, DC, OperatorLoc, Name, NameLoc) {}

  SourceRange getSourceRange() const {
    return { getOperatorLoc(), getNameLoc() };
  }

  /// True if this decl's attributes conflict with those declared by another
  /// PrefixOperatorDecl.
  bool conflictsWith(PrefixOperatorDecl *other) {
    return false;
  }
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::PrefixOperator;
  }
};
  
/// Declares the behavior of a postfix operator. For example:
///
/// \code
/// postfix operator /+/ {}
/// \endcode
class PostfixOperatorDecl : public OperatorDecl {
public:
  PostfixOperatorDecl(DeclContext *DC, SourceLoc OperatorLoc, Identifier Name,
                     SourceLoc NameLoc)
    : OperatorDecl(DeclKind::PostfixOperator, DC, OperatorLoc, Name, NameLoc) {}
  
  SourceRange getSourceRange() const {
    return { getOperatorLoc(), getNameLoc() };
  }

  /// True if this decl's attributes conflict with those declared by another
  /// PostfixOperatorDecl.
  bool conflictsWith(PostfixOperatorDecl *other) {
    return false;
  }
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::PostfixOperator;
  }
};

inline bool ValueDecl::isSettable(const DeclContext *UseDC,
                                  const DeclRefExpr *base) const {
  if (auto vd = dyn_cast<VarDecl>(this)) {
    return vd->isSettable(UseDC, base);
  } else if (auto sd = dyn_cast<SubscriptDecl>(this)) {
    return sd->isSettable();
  } else
    return false;
}

inline Optional<VarDecl *>
NominalTypeDecl::ToStoredProperty::operator()(Decl *decl) const {
  if (auto var = dyn_cast<VarDecl>(decl)) {
    if (!var->isStatic() && var->hasStorage())
      return var;
  }

  return None;
}

inline void
AbstractStorageDecl::overwriteSetterAccessibility(Accessibility accessLevel) {
  GetSetInfo.setInt(accessLevel);
  if (auto setter = getSetter())
    setter->overwriteAccessibility(accessLevel);
  if (auto materializeForSet = getMaterializeForSetFunc())
    materializeForSet->overwriteAccessibility(accessLevel);
}

inline bool AbstractStorageDecl::isStatic() const {
  if (auto var = dyn_cast<VarDecl>(this)) {
    return var->isStatic();
  }

  // Currently, subscripts are never static.
  return false;
}

inline MutableArrayRef<ParameterList *>
AbstractFunctionDecl::getParameterLists() {
  switch (getKind()) {
  default: llvm_unreachable("Unknown AbstractFunctionDecl!");
  case DeclKind::Constructor:
    return cast<ConstructorDecl>(this)->getParameterLists();
  case DeclKind::Destructor:
    return cast<DestructorDecl>(this)->getParameterLists();
  case DeclKind::Func:
    return cast<FuncDecl>(this)->getParameterLists();
  }
}

inline DeclIterator &DeclIterator::operator++() {
  Current = Current->NextDecl;
  return *this;
}

inline bool AbstractFunctionDecl::hasForcedStaticDispatch() const {
  if (auto func = dyn_cast<FuncDecl>(this))
    return func->hasForcedStaticDispatch();
  return false;
}

inline bool ValueDecl::isStatic() const {
  // Currently, only storage and function decls can be static/class.
  if (auto storage = dyn_cast<AbstractStorageDecl>(this))
    return storage->isStatic();
  if (auto func = dyn_cast<FuncDecl>(this))
    return func->isStatic();
  return false;
}

inline bool ValueDecl::isImportAsMember() const {
  if (auto func = dyn_cast<AbstractFunctionDecl>(this))
    return func->isImportAsMember();
  return false;
}

inline bool Decl::isPotentiallyOverridable() const {
  if (isa<VarDecl>(this) ||
      isa<SubscriptDecl>(this) ||
      isa<FuncDecl>(this)) {
    return getDeclContext()->getAsClassOrClassExtensionContext();
  } else {
    return false;
  }
}

inline GenericParamKey::GenericParamKey(const GenericTypeParamDecl *d)
  : Depth(d->getDepth()), Index(d->getIndex()) { }

} // end namespace swift

#endif
