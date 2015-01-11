//===--- Decl.h - Swift Language Declaration ASTs ---------------*- C++ -*-===//
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
// This file defines the Decl class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECL_H
#define SWIFT_DECL_H

#include "swift/AST/Attr.h"
#include "swift/AST/CaptureInfo.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/DefaultArgumentKind.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Substitution.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/OptionalEnum.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <cstddef>

namespace clang {
  class Decl;
  class MacroInfo;
  class Module;
  class SourceLocation;
  class SourceRange;
}

namespace swift {
  enum class AccessSemantics : unsigned char;
  class ApplyExpr;
  class ArchetypeBuilder;
  class ArchetypeType;
  class ASTContext;
  class ASTPrinter;
  class ASTWalker;
  class DestructorDecl;
  class DiagnosticEngine;
  class DynamicSelfType;
  class Type;
  class Expr;
  class LiteralExpr;
  class FuncDecl;
  class BraceStmt;
  class DeclAttributes;
  class GenericSignature;
  class GenericTypeParamDecl;
  class GenericTypeParamType;
  class Module;
  class NameAliasType;
  class EnumElementDecl;
  class Pattern;
  struct PrintOptions;
  class ProtocolDecl;
  class ProtocolType;
  struct RawComment;
  enum class Resilience : unsigned char;
  class TypeAliasDecl;
  class Stmt;
  class SubscriptDecl;
  class ValueDecl;
  class VarDecl;

/// Represents a clang declaration, macro, or module.
class ClangNode {
  llvm::PointerUnion3<const clang::Decl *, const clang::MacroInfo *,
                      const clang::Module *> Ptr;

public:
  ClangNode() = default;
  ClangNode(const clang::Decl *D) : Ptr(D) {}
  ClangNode(const clang::MacroInfo *MI) : Ptr(MI) {}
  ClangNode(const clang::Module *Mod) : Ptr(Mod) {}

  bool isNull() const { return Ptr.isNull(); }
  explicit operator bool() const { return !isNull(); }

  const clang::Decl *getAsDecl() const {
    return Ptr.dyn_cast<const clang::Decl *>();
  }
  const clang::MacroInfo *getAsMacro() const {
    return Ptr.dyn_cast<const clang::MacroInfo *>();
  }
  const clang::Module *getAsModule() const {
    return Ptr.dyn_cast<const clang::Module *>();
  }

  const clang::Decl *castAsDecl() const {
    return Ptr.get<const clang::Decl *>();
  }
  const clang::MacroInfo *castAsMacro() const {
    return Ptr.get<const clang::MacroInfo *>();
  }
  const clang::Module *castAsModule() const {
    return Ptr.get<const clang::Module *>();
  }

  clang::SourceLocation getLocation() const;
  clang::SourceRange getSourceRange() const;

  void *getOpaqueValue() const { return Ptr.getOpaqueValue(); }
  static inline ClangNode getFromOpaqueValue(void *VP) {
    ClangNode N;
    N.Ptr = decltype(Ptr)::getFromOpaqueValue(VP);
    return N;
  }
};
  
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

/// Keeps track of whrther a given class inherits initializers from its
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
};

/// Determine whether two overload signatures conflict.
bool conflicting(const OverloadSignature& sig1, const OverloadSignature& sig2);

/// Decl - Base class for all declarations in Swift.
class alignas(1 << DeclAlignInBits) Decl {
  class DeclBitfields {
    friend class Decl;
    unsigned Kind : 8;

    /// \brief Whether this declaration is invalid.
    unsigned Invalid : 1;

    /// \brief Whether this declaration was implicitly created, e.g.,
    /// an implicit constructor in a struct.
    unsigned Implicit : 1;

    /// \brief Whether this declaration was mapped directly from a Clang AST.
    ///
    /// Use getClangAST() to retrieve the corresponding Clang AST.
    unsigned FromClang : 1;

    /// \brief Whether we've already performed early attribute validation.
    /// FIXME: This is ugly.
    unsigned EarlyAttrValidation : 1;
    
    /// \brief Whether or not this declaration is currently being type-checked.
    unsigned BeingTypeChecked : 1;
  };
  enum { NumDeclBits = 13 };
  static_assert(NumDeclBits <= 32, "fits in an unsigned");
  
  class PatternBindingDeclBitfields {
    friend class PatternBindingDecl;
    unsigned : NumDeclBits;
    
    /// \brief Whether this pattern binding declares static variables.
    unsigned IsStatic : 1;

    /// \brief Whether 'static' or 'class' was used.
    unsigned StaticSpelling : 2;

    /// \brief Whether this pattern binding appears in a conditional statement.
    unsigned Conditional : 1;
  };
  enum { NumPatternBindingDeclBits = NumDeclBits + 4 };
  static_assert(NumPatternBindingDeclBits <= 32, "fits in an unsigned");
  
  class ValueDeclBitfields {
    friend class ValueDecl;
    friend class MemberLookupTable;
    unsigned : NumDeclBits;
    unsigned ConformsToProtocolRequrement : 1;
    unsigned AlreadyInLookupTable : 1;

    /// Whether we have already checked whether this declaration is a 
    /// redeclaration.
    unsigned CheckedRedeclaration : 1;
  };
  enum { NumValueDeclBits = NumDeclBits + 3 };
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

    /// \brief Whether this vardecl has an initial value bound to it in a way
    /// that isn't represented in the AST with an initializer in the pattern
    /// binding.  This happens in cases like "for i in ...", switch cases, etc.
    unsigned HasNonPatternBindingInit : 1;
    
    /// \brief Whether this is a property used in expressions in the debugger.
    /// It is up to the debugger to instruct SIL how to access this variable.
    unsigned IsDebuggerVar : 1;
  };
  enum { NumVarDeclBits = NumAbstractStorageDeclBits + 4 };
  static_assert(NumVarDeclBits <= 32, "fits in an unsigned");
  
  class EnumElementDeclBitfields {
    friend class EnumElementDecl;
    unsigned : NumValueDeclBits;
    
    /// \brief Whether or not this element directly or indirectly references
    /// the enum type.
    unsigned Recursiveness : 2;
  };
  enum { NumEnumElementDeclBits = NumValueDeclBits + 2 };
  static_assert(NumEnumElementDeclBits <= 32, "fits in an unsigned");
  
  class AbstractFunctionDeclBitfields {
    friend class AbstractFunctionDecl;
    unsigned : NumValueDeclBits;

    /// \see AbstractFunctionDecl::BodyKind
    unsigned BodyKind : 3;

    /// Number of curried parameter patterns (tuples).
    unsigned NumParamPatterns : 6;

    /// Whether we are overridden later
    unsigned Overridden : 1;
  };
  enum { NumAbstractFunctionDeclBits = NumValueDeclBits + 10 };
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
  };
  enum { NumFuncDeclBits = NumAbstractFunctionDeclBits + 5 };
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

    /// Whether we have already set the protocols to which this type conforms.
    unsigned ProtocolsSet : 1;
  };

  enum { NumTypeDeclBits = NumValueDeclBits + 2 };
  static_assert(NumTypeDeclBits <= 32, "fits in an unsigned");
  
  class NominalTypeDeclBitFields {
    friend class NominalTypeDecl;
    unsigned : NumTypeDeclBits;
    
    /// Whether or not the nominal type decl has delayed protocol or member
    /// declarations.
    unsigned HasDelayedMembers : 1;

    /// Whether we have already added implicitly-defined initializers
    /// to this declaration.
    unsigned AddedImplicitInitializers : 1;
  };
  enum { NumNominalTypeDeclBits = NumTypeDeclBits + 2 };
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

    /// If this is a compiler-known protocol, this will be a KnownProtocolKind
    /// value, plus one. Otherwise, it will be 0.
    unsigned KnownProtocol : 5;

    /// The stage of the circularity check for this protocol.
    unsigned Circularity : 2;

    /// True if the protocol has requirements that cannot be satisfied (e.g.
    /// because they could not be imported from Objective-C).
    unsigned HasMissingRequirements : 1;
  };
  enum { NumProtocolDeclBits = NumNominalTypeDeclBits + 12 };
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

    /// Whether this class is "foreign".
    unsigned Foreign : 1;
    
    /// Whether this class contains a destructor decl.
    ///
    /// A fully type-checked class always contains a destructor member, even if
    /// it is implicit. This bit is used during parsing and type-checking to
    /// control inserting the implicit destructor.
    unsigned HasDestructorDecl : 1;
  };
  enum { NumClassDeclBits = NumNominalTypeDeclBits + 7 };
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
  
  class InfixOperatorDeclBitfields {
    friend class InfixOperatorDecl;
    unsigned : NumDeclBits;

    unsigned Associativity : 2;
    unsigned Precedence : 8;
    unsigned Assignment : 1;
    unsigned IsAssocImplicit : 1;
    unsigned IsPrecedenceImplicit : 1;
    unsigned IsAssignmentImplicit : 1;
  };
  enum { NumInfixOperatorDeclBits = NumDeclBits + 14 };
  static_assert(NumInfixOperatorDeclBits <= 32, "fits in an unsigned");

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

    unsigned DefaultAccessLevel : 2;

    /// The number of ref-components following the ExtensionDecl.
    unsigned NumRefComponents : 8;
  };
  enum { NumExtensionDeclBits = NumDeclBits + 12 };
  static_assert(NumExtensionDeclBits <= 32, "fits in an unsigned");

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
    NominalTypeDeclBitFields NominalTypeDeclBits;
    ProtocolDeclBitfields ProtocolDeclBits;
    ClassDeclBitfields ClassDeclBits;
    StructDeclBitfields StructDeclBits;
    EnumDeclBitfields EnumDeclBits;
    InfixOperatorDeclBitfields InfixOperatorDeclBits;
    ImportDeclBitfields ImportDeclBits;
    ExtensionDeclBitfields ExtensionDeclBits;
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
  DeclContext *Context;

  Decl(const Decl&) = delete;
  void operator=(const Decl&) = delete;

protected:

  Decl(DeclKind kind, DeclContext *DC) : OpaqueBits(0), Context(DC) {
    DeclBits.Kind = unsigned(kind);
    DeclBits.Invalid = false;
    DeclBits.Implicit = false;
    DeclBits.FromClang = false;
    DeclBits.EarlyAttrValidation = false;
    DeclBits.BeingTypeChecked = false;
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

  DeclContext *getDeclContext() const { return Context; }
  void setDeclContext(DeclContext *DC);

  /// Retrieve the innermost declaration context corresponding to this
  /// declaration, which will either be the declaration itself (if it's
  /// also a declaration context) or its declaration context.
  DeclContext *getInnermostDeclContext();

  /// \brief Retrieve the module in which this declaration resides.
  Module *getModuleContext() const;

  /// getASTContext - Return the ASTContext that this decl lives in.
  ASTContext &getASTContext() const {
    assert(Context && "Decl doesn't have an assigned context");
    return Context->getASTContext();
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

  /// \brief Should this declaration be treated as if annotated with transparent
  /// attribute.
  bool isTransparent() const;

  /// \brief Return whether this declaration has been determined invalid.
  bool isInvalid() const { return DeclBits.Invalid; }
  
  /// \brief Mark this declaration invalid.
  void setInvalid() { DeclBits.Invalid = true; }

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
  
  /// Whether the declaration is currently being validated.
  bool isBeingTypeChecked() { return DeclBits.BeingTypeChecked; }
  
  /// Toggle whether or not the declaration is being validated.
  void setIsBeingTypeChecked(bool ibt = true) {
    DeclBits.BeingTypeChecked = ibt;
  }

  /// \returns the unparsed comment attached to this declaration.
  RawComment getRawComment() const;

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

  bool isPrivateStdlibDecl() const;

  /// Whether this declaration is weak-imported.
  bool isWeakImported(Module *fromModule) const;

  // Make vanilla new/delete illegal for Decls.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  // Only allow allocation of Decls using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
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

/// \brief A single requirement in a 'where' clause, which places additional
/// restrictions on the generic parameters or associated types of a generic
/// function, type, or protocol.
///
/// This always represents a requirement spelled in the source code.  It is
/// never generated implicitly.
class RequirementRepr {
  SourceLoc SeparatorLoc;
  RequirementKind Kind : 2;
  bool Invalid : 1;
  TypeLoc Types[2];

  RequirementRepr(SourceLoc SeparatorLoc, RequirementKind Kind,
                  TypeLoc FirstType, TypeLoc SecondType)
    : SeparatorLoc(SeparatorLoc), Kind(Kind), Invalid(false),
      Types{FirstType, SecondType} { }
  
public:
  /// \brief Construct a new conformance requirement.
  ///
  /// \param Subject The type that must conform to the given protocol or
  /// composition, or be a subclass of the given class type.
  /// \param ColonLoc The location of the ':', or an invalid location if
  /// this requirement was implied.
  /// \param Constraint The protocol or protocol composition to which the
  /// subject must conform, or superclass from which the subject must inherit.
  static RequirementRepr getConformance(TypeLoc Subject,
                                    SourceLoc ColonLoc,
                                    TypeLoc Constraint) {
    return { ColonLoc, RequirementKind::Conformance, Subject, Constraint };
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
    return { EqualLoc, RequirementKind::SameType, FirstType, SecondType };
  }

  /// \brief Determine the kind of requirement
  RequirementKind getKind() const { return Kind; }

  /// \brief Determine whether this requirement is invalid.
  bool isInvalid() const { return Invalid; }

  /// \brief Mark this requirement invalid.
  void setInvalid() { Invalid = true; }

  /// \brief For a conformance requirement, return the subject of the
  /// conformance relationship.
  Type getSubject() const {
    assert(getKind() == RequirementKind::Conformance);
    return Types[0].getType();
  }

  TypeRepr *getSubjectRepr() const {
    assert(getKind() == RequirementKind::Conformance);
    return Types[0].getTypeRepr();
  }

  TypeLoc &getSubjectLoc() {
    assert(getKind() == RequirementKind::Conformance);
    return Types[0];
  }

  const TypeLoc &getSubjectLoc() const {
    assert(getKind() == RequirementKind::Conformance);
    return Types[0];
  }

  /// \brief For a conformance requirement, return the protocol or to which
  /// the subject conforms or superclass it inherits.
  Type getConstraint() const {
    assert(getKind() == RequirementKind::Conformance);
    return Types[1].getType();
  }

  TypeLoc &getConstraintLoc() {
    assert(getKind() == RequirementKind::Conformance);
    return Types[1];
  }

  const TypeLoc &getConstraintLoc() const {
    assert(getKind() == RequirementKind::Conformance);
    return Types[1];
  }

  /// \brief Retrieve the location of the ':' in an explicitly-written
  /// conformance requirement.
  SourceLoc getColonLoc() const {
    assert(getKind() == RequirementKind::Conformance);
    return SeparatorLoc;
  }

  /// \brief Retrieve the first type of a same-type requirement.
  Type getFirstType() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[0].getType();
  }

  TypeRepr *getFirstTypeRepr() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[0].getTypeRepr();
  }

  TypeLoc &getFirstTypeLoc() {
    assert(getKind() == RequirementKind::SameType);
    return Types[0];
  }

  const TypeLoc &getFirstTypeLoc() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[0];
  }

  /// \brief Retrieve the second type of a same-type requirement.
  Type getSecondType() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[1].getType();
  }

  TypeRepr *getSecondTypeRepr() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[1].getTypeRepr();
  }

  TypeLoc &getSecondTypeLoc() {
    assert(getKind() == RequirementKind::SameType);
    return Types[1];
  }

  const TypeLoc &getSecondTypeLoc() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[1];
  }

  /// \brief Retrieve the location of the '==' in an explicitly-written
  /// same-type requirement.
  SourceLoc getEqualLoc() const {
    assert(getKind() == RequirementKind::SameType);
    return SeparatorLoc;
  }

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump() const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");
  void print(raw_ostream &OS) const;
};
  
template<typename T, ArrayRef<T> (GenericParamList::*accessor)() const>
class NestedGenericParamListIterator;
  
/// GenericParamList - A list of generic parameters that is part of a generic
/// function or type, along with extra requirements placed on those generic
/// parameters and types derived from them.
class GenericParamList {
  SourceRange Brackets;
  unsigned NumParams;
  SourceLoc WhereLoc;
  MutableArrayRef<RequirementRepr> Requirements;
  ArrayRef<ArchetypeType *> AllArchetypes;

  GenericParamList *OuterParameters;

  /// The builder used to build archetypes for this list.
  ArchetypeBuilder *Builder;

  GenericParamList(SourceLoc LAngleLoc,
                   ArrayRef<GenericTypeParamDecl *> Params,
                   SourceLoc WhereLoc,
                   MutableArrayRef<RequirementRepr> Requirements,
                   SourceLoc RAngleLoc);
  
  void getAsGenericSignatureElements(ASTContext &C,
                         llvm::DenseMap<ArchetypeType*, Type> &archetypeMap,
                         SmallVectorImpl<GenericTypeParamType*> &genericParams,
                         SmallVectorImpl<Requirement> &requirements) const;
  
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

  /// Create a new generic parameter list with the same parameters and
  /// requirements as this one, but parented to a different outer parameter
  /// list.
  GenericParamList *cloneWithOuterParameters(const ASTContext &Context,
                                             GenericParamList *Outer) {
    auto clone = create(Context,
                        SourceLoc(),
                        getParams(),
                        SourceLoc(),
                        getRequirements(),
                        SourceLoc());
    clone->setAllArchetypes(getAllArchetypes());
    clone->setOuterParameters(Outer);
    return clone;
  }
  
  /// Create an empty generic parameter list.
  static GenericParamList *getEmpty(ASTContext &Context) {
    // TODO: Could probably unique this in the AST context.
    return create(Context, SourceLoc(), {}, SourceLoc(), {}, SourceLoc());
  }
  
  MutableArrayRef<GenericTypeParamDecl *> getParams() {
    return { reinterpret_cast<GenericTypeParamDecl **>(this + 1), NumParams };
  }

  ArrayRef<GenericTypeParamDecl *> getParams() const {
    return const_cast<GenericParamList *>(this)->getParams();
  }

  using iterator = GenericTypeParamDecl **;
  using const_iterator = const GenericTypeParamDecl * const *;

  unsigned size() const { return NumParams; }
  iterator begin() { return getParams().begin(); }
  iterator end()   { return getParams().end(); }
  const_iterator begin() const { return getParams().begin(); }
  const_iterator end()   const { return getParams().end(); }

  /// Get the total number of parameters, including those from parent generic
  /// parameter lists.
  unsigned totalSize() const {
    return NumParams + (OuterParameters ? OuterParameters->totalSize() : 0);
  }
  
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

  /// \brief Override the set of requirements associated with this generic
  /// parameter list.
  ///
  /// \param NewRequirements The new set of requirements, which is expected
  /// to be a superset of the existing set of requirements (although this
  /// property is not checked here). It is assumed that the array reference
  /// refers to ASTContext-allocated memory.
  void overrideRequirements(MutableArrayRef<RequirementRepr> NewRequirements) {
    Requirements = NewRequirements;
  }
  
  /// \brief Retrieves the list containing all archetypes described by this
  /// generic parameter clause.
  ///
  /// In this list of archetypes, the primary archetypes come first followed by
  /// any non-primary archetypes (i.e., those archetypes that encode associated
  /// types of another archetype).
  ///
  /// This does not include archetypes from the outer generic parameter list(s).
  ArrayRef<ArchetypeType *> getAllArchetypes() const { return AllArchetypes; }

  /// \brief Return the number of primary archetypes.
  unsigned getNumPrimaryArchetypes() const {
    return size();
  }
  
  /// \brief Retrieves the list containing only the primary archetypes described
  /// by this generic parameter clause. This excludes archetypes for associated
  /// types of the primary archetypes.
  ArrayRef<ArchetypeType *> getPrimaryArchetypes() const {
    return getAllArchetypes().slice(0, getNumPrimaryArchetypes());
  }
  
  /// \brief Retrieves the list containing only the associated archetypes.
  ArrayRef<ArchetypeType *> getAssociatedArchetypes() const {
    return getAllArchetypes().slice(getNumPrimaryArchetypes());
  }

  /// \brief Sets all archetypes *without* copying the source array.
  void setAllArchetypes(ArrayRef<ArchetypeType *> AA) {
    assert(AA.size() >= size()
           && "allArchetypes is smaller than number of generic params?!");
    AllArchetypes = AA;
  }

  using NestedArchetypeIterator
    = NestedGenericParamListIterator<ArchetypeType*,
                                     &GenericParamList::getAllArchetypes>;
  using NestedGenericParamIterator
    = NestedGenericParamListIterator<GenericTypeParamDecl*,
                                     &GenericParamList::getParams>;
  
  /// \brief Retrieves a list containing all archetypes from this generic
  /// parameter clause and all outer generic parameter clauses in outer-to-
  /// inner order.
  Range<NestedArchetypeIterator> getAllNestedArchetypes() const;
  
  /// \brief Retrieves a list containing all generic parameter records from
  /// this generic parameter clause and all outer generic parameter clauses in
  /// outer-to-inner order.
  Range<NestedGenericParamIterator> getNestedGenericParams() const;
  
  /// \brief Retrieve the outer generic parameter list, which provides the
  /// generic parameters of the context in which this generic parameter list
  /// exists.
  ///
  /// Consider the following generic class:
  ///
  /// \code
  /// class Vector<T> {
  ///   constructor<R : Range where R.Element == T>(range : R) { }
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

  /// Retrieve the depth of this generic parameter list.
  unsigned getDepth() const {
    unsigned depth = 0;
    for (auto gp = getOuterParameters(); gp; gp = gp->getOuterParameters())
      ++depth;
    return depth;
  }
  
  /// Get the generic parameter list as a GenericSignature in which the generic
  /// parameters have been canonicalized.
  ///
  /// \param archetypeMap   This DenseMap is populated with a mapping of
  ///                       context primary archetypes to dependent generic
  ///                       types.
  GenericSignature *getAsCanonicalGenericSignature(
                           llvm::DenseMap<ArchetypeType*, Type> &archetypeMap,
                           ASTContext &C) const;
  
  /// Derive a type substitution map for this generic parameter list from a
  /// matching substitution vector.
  TypeSubstitutionMap getSubstitutionMap(ArrayRef<Substitution> Subs) const;

  /// Derive the all-archetypes list for the given list of generic
  /// parameters.
  static ArrayRef<ArchetypeType*>
  deriveAllArchetypes(ArrayRef<GenericTypeParamDecl*> params,
                      SmallVectorImpl<ArchetypeType*> &archetypes);

  void setBuilder(ArchetypeBuilder *builder) {
    Builder = builder;
  }

  ArchetypeBuilder *getBuilder() const {
    return Builder;
  }

  /// Collect the nested archetypes of an archetype into the given
  /// collection.
  ///
  /// \param known - the set of archetypes already present in `all`
  /// \param all - the output list of archetypes
  static void addNestedArchetypes(ArchetypeType *archetype,
                                  SmallPtrSetImpl<ArchetypeType*> &known,
                                  SmallVectorImpl<ArchetypeType*> &all);
  
  void print(raw_ostream &OS);
  void dump();
};
  
/// An iterator template for lazily walking a nested generic parameter list.
template<typename T, ArrayRef<T> (GenericParamList::*accessor)() const>
class NestedGenericParamListIterator {
  SmallVector<const GenericParamList*, 2> stack;
  ArrayRef<T> elements;

  void refreshElements() {
    while (elements.empty()) {
      stack.pop_back();
      if (stack.empty()) break;
      elements = (stack.back()->*accessor)();
    }
  }
public:
  // Create a 'begin' iterator for a generic param list.
  NestedGenericParamListIterator(const GenericParamList *params) {
    // Walk up to the outermost list to create a stack of lists to walk.
    while (params) {
      stack.push_back(params);
      params = params->getOuterParameters();
    }
    // If the stack is empty, be like the 'end' iterator.
    if (stack.empty())
      return;

    elements = (stack.back()->*accessor)();
    refreshElements();
  }
  
  // Create an 'end' iterator.
  NestedGenericParamListIterator() {}
  
  // Iterator dereference.
  const T &operator*() const {
    return elements[0];
  }
  const T *operator->() const {
    return &elements[0];
  }
  
  // Iterator advancement.
  NestedGenericParamListIterator &operator++() {
    elements = elements.slice(1);
    refreshElements();
    return *this;
  }
  NestedGenericParamListIterator operator++(int) {
    auto copy = *this;
    ++(*this);
    return copy;
  }
  
  // Ghetto comparison. Only true if end() == end().
  bool operator==(const NestedGenericParamListIterator &o) const {
    return stack.empty() && o.stack.empty();
  }
  bool operator!=(const NestedGenericParamListIterator &o) const {
    return !stack.empty() || !o.stack.empty();
  }
  
  // An empty range of nested archetypes.
  static Range<NestedGenericParamListIterator> emptyRange() {
    return {{}, {}};
  }
};
  
using NestedArchetypeIterator = GenericParamList::NestedArchetypeIterator;
using NestedGenericParamIterator = GenericParamList::NestedGenericParamIterator;

inline Range<NestedArchetypeIterator>
GenericParamList::getAllNestedArchetypes() const {
  return {NestedArchetypeIterator(this), NestedArchetypeIterator()};
}
  
inline Range<NestedGenericParamIterator>
GenericParamList::getNestedGenericParams() const {
  return {NestedGenericParamIterator(this), NestedGenericParamIterator()};
}

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
class ImportDecl : public Decl {
public:
  typedef std::pair<Identifier, SourceLoc> AccessPathElement;

private:
  SourceLoc ImportLoc;
  SourceLoc KindLoc;

  /// The number of elements in this path.
  unsigned NumPathElements;

  /// The resolved module.
  Module *Mod = nullptr;
  /// The resolved decls if this is a decl import.
  ArrayRef<ValueDecl *> Decls;

  AccessPathElement *getPathBuffer() {
    return reinterpret_cast<AccessPathElement*>(this+1);
  }
  const AccessPathElement *getPathBuffer() const {
    return reinterpret_cast<const AccessPathElement*>(this+1);
  }
  
  ImportDecl(DeclContext *DC, SourceLoc ImportLoc, ImportKind K,
             SourceLoc KindLoc, ArrayRef<AccessPathElement> Path);

public:
  static ImportDecl *create(ASTContext &C, DeclContext *DC,
                            SourceLoc ImportLoc, ImportKind Kind,
                            SourceLoc KindLoc,
                            ArrayRef<AccessPathElement> Path,
                            const clang::Module *Mod = nullptr);

  /// Returns the import kind that is most appropriate for \p VD.
  ///
  /// Note that this will never return \c Type; an imported typealias will use
  /// the more specific kind from its underlying type.
  static ImportKind getBestImportKind(const ValueDecl *VD);

  /// Returns the most appropriate import kind for the given list of decls.
  ///
  /// If the list is non-homogenous, or if there is more than one decl that
  /// cannot be overloaded, returns None.
  static Optional<ImportKind> findBestImportKind(ArrayRef<ValueDecl *> Decls);

  ArrayRef<AccessPathElement> getFullAccessPath() const {
    return ArrayRef<AccessPathElement>(getPathBuffer(), NumPathElements);
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

  Module *getModule() const { return Mod; }
  void setModule(Module *M) { Mod = M; }

  ArrayRef<ValueDecl *> getDecls() const { return Decls; }
  void setDecls(ArrayRef<ValueDecl *> Ds) { Decls = Ds; }

  const clang::Module *getClangModule() const {
    if (ClangNode ClangN = getClangNode())
      return ClangN.castAsModule();
    return nullptr;
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
public:
  /// A single component within the reference to the extended type.
  struct RefComponent {
    /// The name of the type being extended.
    Identifier Name;

    /// The location of the name.
    SourceLoc NameLoc;
    
    /// The generic parameters associated with this name.
    GenericParamList *GenericParams;
  };

private:
  SourceLoc ExtensionLoc;  // Location of 'extension' keyword.
  SourceRange Braces;

  /// \brief The generic signature of this extension.
  ///
  /// This is the semantic representation of a generic parameters and the
  /// requirements placed on them.
  ///
  /// FIXME: The generic parameters here are also derivable from
  /// \c GenericParams. However, we likely want to make \c GenericParams
  /// the parsed representation, and not part of the module file.
  GenericSignature *GenericSig = nullptr;

  /// The type being extended.
  Type ExtendedType;

  MutableArrayRef<TypeLoc> Inherited;

  /// \brief The set of protocols to which this extension conforms.
  ArrayRef<ProtocolDecl *> Protocols;
  
  /// \brief The set of protocol conformance mappings. The element order
  /// corresponds to the order of Protocols.
  LazyLoaderArray<ProtocolConformance *> Conformances;

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
  friend class IterableDeclContext;

  ExtensionDecl(SourceLoc extensionLoc, ArrayRef<RefComponent> refComponents,
                MutableArrayRef<TypeLoc> inherited,
                DeclContext *parent);

public:
  using Decl::getASTContext;

  /// Create a new extension declaration.
  static ExtensionDecl *create(ASTContext &ctx, SourceLoc extensionLoc,
                               ArrayRef<RefComponent> refComponents,
                               MutableArrayRef<TypeLoc> inherited,
                               DeclContext *parent,
                               ClangNode clangNode = ClangNode());

  SourceLoc getStartLoc() const { return ExtensionLoc; }
  SourceLoc getLoc() const { return ExtensionLoc; }
  SourceRange getSourceRange() const {
    return { ExtensionLoc, Braces.End };
  }

  SourceRange getBraces() const { return Braces; }
  void setBraces(SourceRange braces) { Braces = braces; }

  /// Retrieve the reference components in the
  ArrayRef<RefComponent> getRefComponents() const {
    return { reinterpret_cast<const RefComponent *>(this + 1),
             ExtensionDeclBits.NumRefComponents };
  }

  MutableArrayRef<RefComponent> getRefComponents() {
    return { reinterpret_cast<RefComponent *>(this + 1),
             ExtensionDeclBits.NumRefComponents };
  }

  /// Retrieve the innermost generic parameter list.
  GenericParamList *getGenericParams() const {
    return getRefComponents().back().GenericParams;
  }

  /// Retrieve the generic signature for this extension.
  GenericSignature *getGenericSignature() const { return GenericSig; }

  /// Set the generic signature of this extension.
  void setGenericSignature(GenericSignature *sig);

  /// Retrieve the type being extended.
  Type getExtendedType() const { return ExtendedType; }

  /// Set the type being extended.
  void setExtendedType(Type extended) { ExtendedType = extended; }

  /// Compute the source range that covers the extended type.
  SourceRange getExtendedTypeRange() const;

  /// \brief Retrieve the set of protocols that this type inherits (i.e,
  /// explicitly conforms to).
  MutableArrayRef<TypeLoc> getInherited() { return Inherited; }
  ArrayRef<TypeLoc> getInherited() const { return Inherited; }

  /// Whether we already validated this extension.
  bool validated() const {
    return ExtensionDeclBits.Validated;
  }

  /// Set whether we have validated this extension.
  void setValidated(bool validated = true) {
    ExtensionDeclBits.Validated = validated;
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
    return ExtensionDeclBits.DefaultAccessLevel != 0;
  }

  Accessibility getDefaultAccessibility() const {
    assert(hasDefaultAccessibility() && "not computed yet");
    return static_cast<Accessibility>(ExtensionDeclBits.DefaultAccessLevel - 1);
  }

  void setDefaultAccessibility(Accessibility access) {
    assert(!hasDefaultAccessibility() && "default accessibility already set");
    ExtensionDeclBits.DefaultAccessLevel = static_cast<unsigned>(access) + 1;
  }

  /// \brief Retrieve the set of protocols to which this extension conforms.
  ArrayRef<ProtocolDecl *> getProtocols(bool forceDelayedMembers = true) const {
    return Protocols;
  }

  void setProtocols(ArrayRef<ProtocolDecl *> protocols) {
    Protocols = protocols;
  }

  /// \brief Retrieve the set of protocol conformance mappings for this type.
  ///
  /// Calculated during type-checking.
  ArrayRef<ProtocolConformance *> getConformances() const;
  void setConformances(ArrayRef<ProtocolConformance *> c) {
    Conformances = c;
  }
  void setConformanceLoader(LazyMemberLoader *resolver, uint64_t contextData);

  DeclRange getMembers(bool forceDelayedMembers = true) const;
  void setMemberLoader(LazyMemberLoader *resolver, uint64_t contextData);
  bool hasLazyMembers() const {
    return IterableDeclContext::isLazy();
  }

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

/// \brief This decl contains a pattern and optional initializer for a set
/// of one or more VarDecls declared together.
///
/// For example, in
/// \code
///   var (a, b) = foo()
/// \endcode
/// this contains the pattern "(a, b)" and the intializer "foo()".  The same
/// applies to simpler declarations like "var a = foo()".
class PatternBindingDecl : public Decl {
  SourceLoc StaticLoc; ///< Location of the 'static/class' keyword, if present.
  SourceLoc VarLoc;    ///< Location of the 'var' keyword.
  Pattern *Pat;        ///< The pattern this decl binds.

  /// The initializer, and whether it's been type-checked already.
  llvm::PointerIntPair<Expr *, 1, bool> InitAndChecked;

  friend class Decl;
  
public:
  PatternBindingDecl(SourceLoc StaticLoc, StaticSpellingKind StaticSpelling,
                     SourceLoc VarLoc,
                     Pattern *Pat, Expr *E,
                     bool isConditional,
                     DeclContext *Parent)
    : Decl(DeclKind::PatternBinding, Parent),
      StaticLoc(StaticLoc), VarLoc(VarLoc), Pat(nullptr),
      InitAndChecked(E, false) {
    PatternBindingDeclBits.IsStatic = StaticLoc.isValid();
    PatternBindingDeclBits.StaticSpelling =
        static_cast<unsigned>(StaticSpelling);
    PatternBindingDeclBits.Conditional = isConditional;
    setPattern(Pat);
  }

  SourceLoc getStartLoc() const {
    return StaticLoc.isValid() ? StaticLoc : VarLoc;
  }
  SourceLoc getLoc() const { return VarLoc; }
  SourceRange getSourceRange() const;

  Pattern *getPattern() { return Pat; }
  const Pattern *getPattern() const { return Pat; }
  void setPattern(Pattern *P);

  bool hasInit() const { return InitAndChecked.getPointer(); }
  Expr *getInit() const { return InitAndChecked.getPointer(); }
  bool wasInitChecked() const { return InitAndChecked.getInt(); }
  void setInit(Expr *expr, bool checked) {
    InitAndChecked.setPointerAndInt(expr, checked);
  }

  /// Does this binding declare something that requires storage?
  bool hasStorage() const;

  /// Does this binding appear in an 'if' or 'while' condition?
  bool isConditional() const { return PatternBindingDeclBits.Conditional; }
  
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

  /// True if this is the active clause of the #if block.  Since this is
  /// evaluated at parse time, this is always known.
  bool isActive;

  IfConfigDeclClause(SourceLoc Loc, Expr *Cond, ArrayRef<Decl*> Members,
                     bool isActive)
    : Loc(Loc), Cond(Cond), Members(Members), isActive(isActive) {
  }
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
  bool HadMissingEnd;
public:
  
  IfConfigDecl(DeclContext *Parent, ArrayRef<IfConfigDeclClause> Clauses,
               SourceLoc EndLoc, bool HadMissingEnd)
    : Decl(DeclKind::IfConfig, Parent), Clauses(Clauses), EndLoc(EndLoc),
      HadMissingEnd(HadMissingEnd) {
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

  bool hadMissingEnd() const { return HadMissingEnd; }
  
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
  llvm::PointerIntPair<Type, 2, OptionalEnum<Accessibility>> TypeAndAccess;

protected:
  ValueDecl(DeclKind K, DeclContext *DC, DeclName name, SourceLoc NameLoc)
    : Decl(K, DC), Name(name), NameLoc(NameLoc) {
    ValueDeclBits.ConformsToProtocolRequrement = false;
    ValueDeclBits.AlreadyInLookupTable = false;
    ValueDeclBits.CheckedRedeclaration = false;
  }

  /// The interface type, mutable because some subclasses compute this lazily.
  mutable Type InterfaceTy;

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

  /// Retrieve the base name of the declaration, ignoring any argument
  /// names.
  DeclName getBaseName() const { return Name.getBaseName(); }

  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getLoc() const { return NameLoc; }

  bool hasType() const { return !TypeAndAccess.getPointer().isNull(); }
  Type getType() const {
    assert(hasType() && "declaration has no type set yet");
    return TypeAndAccess.getPointer();
  }

  /// Set the type of this declaration for the first time.
  void setType(Type T);

  /// Overwrite the type of this declaration.
  void overwriteType(Type T);

  bool hasAccessibility() const {
    return TypeAndAccess.getInt().hasValue();
  }

  Accessibility getAccessibility() const {
    assert(hasAccessibility() && "accessibility not computed yet");
    return TypeAndAccess.getInt().getValue();
  }

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

  /// Get the innermost declaration context that can provide generic
  /// parameters used within this declaration.
  DeclContext *getPotentialGenericDeclContext();

  /// Get the innermost declaration context that can provide generic
  /// parameters used within this declaration.
  const DeclContext *getPotentialGenericDeclContext() const {
    return const_cast<ValueDecl *>(this)->getPotentialGenericDeclContext();
  }

  /// Retrieve the "interface" type of this value, which is the type used when
  /// the declaration is viewed from the outside. For a generic function,
  /// this will have generic function type using generic parameters rather than
  /// archetypes, while a generic nominal type's interface type will be the
  /// generic type specialized with its generic parameters.
  ///
  /// FIXME: Eventually, this will simply become the type of the value, and
  /// we will substitute in the appropriate archetypes within a particular
  /// context.
  Type getInterfaceType() const;
  bool hasInterfaceType() const { return !!InterfaceTy; }

  /// Set the interface type for the given value.
  void setInterfaceType(Type type);
  
  /// isSettable - Determine whether references to this decl may appear
  /// on the left-hand side of an assignment or as the operand of a
  /// `&` or 'inout' operator.
  bool isSettable(DeclContext *UseDC) const;
  
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
  /// In that case it was inferred by the type checker.
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

  /// Returns true if this decl conforms to a protocol requirement.
  bool conformsToProtocolRequirement() const {
    return ValueDeclBits.ConformsToProtocolRequrement;
  }
  void setConformsToProtocolRequirement(bool Value = true) {
    ValueDeclBits.ConformsToProtocolRequrement = Value;
  }

  /// Returns the protocol requirements that this decl conforms to.
  ArrayRef<ValueDecl *> getConformances();

  /// Determines the kind of access that should be performed by a
  /// DeclRefExpr or MemberRefExpr use of this value in the specified
  /// context.
  AccessSemantics getAccessSemanticsFromContext(const DeclContext *DC) const;
  
  /// Dump a reference to the given declaration.
  void dumpRef(raw_ostream &os) const;

  /// Dump a reference to the given declaration.
  void dumpRef() const;

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_ValueDecl &&
           D->getKind() <= DeclKind::Last_ValueDecl;
  }
};

/// This is a common base class for declarations which declare a type.
class TypeDecl : public ValueDecl {
  MutableArrayRef<TypeLoc> Inherited;

protected:
  TypeDecl(DeclKind K, DeclContext *DC, Identifier name, SourceLoc NameLoc,
           MutableArrayRef<TypeLoc> inherited) :
    ValueDecl(K, DC, name, NameLoc), Inherited(inherited)
  {
    TypeDeclBits.CheckedInheritanceClause = false;
    TypeDeclBits.ProtocolsSet = false;
  }
  
  /// \brief The set of protocols to which this type conforms.
  ArrayRef<ProtocolDecl *> Protocols;

public:
  Type getDeclaredType() const;

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

  /// \brief Retrieve the set of protocols to which this type conforms.
  ///
  /// FIXME: Include protocol conformance from extensions? This will require
  /// semantic analysis to compute.
  ArrayRef<ProtocolDecl *> getProtocols(bool forceDelayedMembers = true) const;

  /// \brief For declarations that are initially composed of a mix of delayed
  /// and non-delayed protocols, allow the setting of a temporary list of
  /// non-delayed protocols that will be copied over to the "official" protocol
  /// list when the delayed protocol declarations are forced.
  void setInitialUndelayedProtocols(ArrayRef<ProtocolDecl *> protocols) {
    Protocols = protocols;
  }

  void setProtocols(ArrayRef<ProtocolDecl *> protocols) {
    assert((!TypeDeclBits.ProtocolsSet || protocols.empty()) &&
           "protocols already set");
    TypeDeclBits.ProtocolsSet = true;
    Protocols = protocols;
  }

  bool isProtocolsValid() const {
    return TypeDeclBits.ProtocolsSet;
  }

  void setInherited(MutableArrayRef<TypeLoc> i) { Inherited = i; }

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_TypeDecl &&
           D->getKind() <= DeclKind::Last_TypeDecl;
  }
};

/// TypeAliasDecl - This is a declaration of a typealias, for example:
///
///    typealias foo = int
///
/// TypeAliasDecl's always have 'MetatypeType' type.
///
class TypeAliasDecl : public TypeDecl {
  /// The type that represents this (sugared) name alias.
  mutable NameAliasType *AliasTy;

  SourceLoc TypeAliasLoc; // The location of the 'typalias' keyword
  TypeLoc UnderlyingTy;

public:
  TypeAliasDecl(SourceLoc TypeAliasLoc, Identifier Name,
                SourceLoc NameLoc, TypeLoc UnderlyingTy,
                DeclContext *DC);

  SourceLoc getStartLoc() const { return TypeAliasLoc; }
  SourceRange getSourceRange() const;

  /// getUnderlyingType - Returns the underlying type, which is
  /// assumed to have been set.
  Type getUnderlyingType() const {
    assert(!UnderlyingTy.getType().isNull() &&
           "getting invalid underlying type");
    return UnderlyingTy.getType();
  }

  /// \brief Determine whether this type alias has an underlying type.
  bool hasUnderlyingType() const { return !UnderlyingTy.getType().isNull(); }

  TypeLoc &getUnderlyingTypeLoc() { return UnderlyingTy; }
  const TypeLoc &getUnderlyingTypeLoc() const { return UnderlyingTy; }

  /// getAliasType - Return the sugared version of this decl as a Type.
  NameAliasType *getAliasType() const { return AliasTy; }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::TypeAlias;
  }
};

/// Abstract class describing generic type parameters and associated types,
/// whose common purpose is to anchor the abstract type parameter and specify
/// requirements for any corresponding type argument.
class AbstractTypeParamDecl : public TypeDecl {
  /// The superclass of the generic parameter.
  Type SuperclassTy;

  /// The archetype describing this abstract type parameter within its scope.
  ArchetypeType *Archetype;

protected:
  AbstractTypeParamDecl(DeclKind kind, DeclContext *dc, Identifier name,
                        SourceLoc NameLoc)
    : TypeDecl(kind, dc, name, NameLoc, { }), Archetype(nullptr) { }

public:
  /// Return the superclass of the generic parameter.
  Type getSuperclass() const {
    return SuperclassTy;
  }

  /// Set the superclass of the generic parameter.
  void setSuperclass(Type superclassTy) {
    SuperclassTy = superclassTy;
  }

  /// Retrieve the archetype that describes this abstract type parameter
  /// within its scope.
  ArchetypeType *getArchetype() const { return Archetype; }

  /// Set the archetype used to describe this abstract type parameter within
  /// its scope.
  void setArchetype(ArchetypeType *archetype) { Archetype = archetype; }

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
/// describes via conformance to specific protocols, or inheritance from a
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

  SourceLoc getStartLoc() const { return KeywordLoc; }
  SourceRange getSourceRange() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::AssociatedType;
  }
};

class MemberLookupTable;

/// Iterator that walks the generic parameter types declared in a generic
/// signature and their dependent members.
class GenericSignatureWitnessIterator {
  ArrayRef<Requirement> p;
  
public:
  GenericSignatureWitnessIterator() = default;
  GenericSignatureWitnessIterator(ArrayRef<Requirement> p)
    : p(p)
  {
    assert(p.empty() || p.front().getKind() == RequirementKind::WitnessMarker);
  }
  
  GenericSignatureWitnessIterator &operator++() {
    do {
      p = p.slice(1);
    } while (!p.empty()
             && p.front().getKind() != RequirementKind::WitnessMarker);
    return *this;
  }
  
  GenericSignatureWitnessIterator operator++(int) {
    auto copy = *this;
    ++(*this);
    return copy;
  }
  
  Type operator*() const {
    assert(p.front().getKind() == RequirementKind::WitnessMarker);
    return p.front().getFirstType();
  }
  
  Type operator->() const {
    assert(p.front().getKind() == RequirementKind::WitnessMarker);
    return p.front().getFirstType();
  }
  
  bool operator==(const GenericSignatureWitnessIterator &o) {
    return p.data() == o.p.data() && p.size() == o.p.size();
  }
  
  bool operator!=(const GenericSignatureWitnessIterator &o) {
    return p.data() != o.p.data() || p.size() != o.p.size();
  }
  
  static GenericSignatureWitnessIterator emptyRange() {
    return GenericSignatureWitnessIterator();
  }
  
  // Allow the witness iterator to be used with a ranged for.
  GenericSignatureWitnessIterator begin() const {
    return *this;
  }
  GenericSignatureWitnessIterator end() const {
    return GenericSignatureWitnessIterator({p.end(), p.end()});
  }
};

class GenericSignature;
  
/// Describes the generic signature of a particular declaration, including
/// both the generic type parameters and the requirements placed on those
/// generic parameters.
class GenericSignature : public llvm::FoldingSetNode {
  unsigned NumGenericParams;
  unsigned NumRequirements;

  // Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  /// Retrieve a mutable version of the generic parameters.
  MutableArrayRef<GenericTypeParamType *> getGenericParamsBuffer() {
    return { reinterpret_cast<GenericTypeParamType **>(this + 1),
             NumGenericParams };
  }

  /// Retrieve a mutable verison of the requirements.
  MutableArrayRef<Requirement> getRequirementsBuffer() {
    void *genericParams = getGenericParamsBuffer().end();
    return { reinterpret_cast<Requirement *>(genericParams),
      NumRequirements };
  }

  GenericSignature(ArrayRef<GenericTypeParamType *> params,
                   ArrayRef<Requirement> requirements);

  llvm::PointerUnion<GenericSignature *, ASTContext *>
    CanonicalSignatureOrASTContext;
  
  static ASTContext &getASTContext(ArrayRef<GenericTypeParamType *> params,
                                   ArrayRef<Requirement> requirements);
  
public:
  /// Create a new generic signature with the given type parameters and
  /// requirements.
  static GenericSignature *get(ArrayRef<GenericTypeParamType *> params,
                               ArrayRef<Requirement> requirements);

  /// Create a new generic signature with the given type parameters and
  /// requirements, first canonicalizing the types.
  static CanGenericSignature getCanonical(ArrayRef<GenericTypeParamType *> params,
                                          ArrayRef<Requirement> requirements);

  /// Retrieve the generic parameters.
  ArrayRef<GenericTypeParamType *> getGenericParams() const {
    return { reinterpret_cast<GenericTypeParamType * const *>(this + 1),
             NumGenericParams };
  }

  /// Retrieve the innermost generic parameters.
  ///
  /// Given a generic signature for a nested generic type, produce an
  /// array of the generic parameters for the innermost generic type.
  ArrayRef<GenericTypeParamType *> getInnermostGenericParams() const;

  /// Retrieve the requirements.
  ArrayRef<Requirement> getRequirements() const {
    const void *genericParams = getGenericParams().end();
    return { reinterpret_cast<const Requirement *>(genericParams),
             NumRequirements };
  }

  // Only allow allocation by doing a placement new.
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
  
  /// Build a substitution map from a vector of Substitutions that correspond to
  /// the generic parameters in this generic signature. The order of primary
  /// archetypes in the substitution vector must match the order of generic
  /// parameters in getGenericParams().
  TypeSubstitutionMap getSubstitutionMap(ArrayRef<Substitution> args) const;
  
  /// Return a range that iterates through first all of the generic parameters
  /// of the signature, followed by all of their recursive member types exposed
  /// through protocol requirements.
  ///
  /// The member types are presented in the
  /// same order as GenericParamList::getAllArchetypes would present for an
  /// equivalent GenericParamList.
  GenericSignatureWitnessIterator getAllDependentTypes() const {
    return GenericSignatureWitnessIterator(getRequirements());
  }
  
  bool isCanonical() const {
    return CanonicalSignatureOrASTContext.is<ASTContext*>();
  }
  
  ASTContext &getASTContext() const;
  
  CanGenericSignature getCanonicalSignature();

  /// Uniquing for the ASTContext.
  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericParams(), getRequirements());
  }
  
  static void Profile(llvm::FoldingSetNodeID &ID,
                      ArrayRef<GenericTypeParamType *> genericParams,
                      ArrayRef<Requirement> requirements);
  
  void print(raw_ostream &OS) const;
  void dump() const;
};
  
inline
CanGenericSignature::CanGenericSignature(GenericSignature *Signature)
  : Signature(Signature)
{
  assert(!Signature || Signature->isCanonical());
}
  
inline ArrayRef<CanTypeWrapper<GenericTypeParamType>>
CanGenericSignature::getGenericParams() const{
  ArrayRef<GenericTypeParamType*> params = Signature->getGenericParams();
  auto base = reinterpret_cast<const CanTypeWrapper<GenericTypeParamType>*>(
                                                                params.data());
  return {base, params.size()};
}


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
  PTK_UnsafeMutablePointer,
  PTK_UnsafePointer,
  PTK_AutoreleasingUnsafeMutablePointer,
};

/// An implicitly created member decl, used when importing a Clang enum type.
/// These are not added to their enclosing type unless forced.
typedef std::function<void(SmallVectorImpl<Decl *> &)> DelayedDecl;

/// An implicitly created protocol decl, used when importing a Clang enum type.
/// These are not added to their enclosing type unless forced.
typedef std::function<ProtocolDecl *()> DelayedProtocolDecl;

/// NominalTypeDecl - a declaration of a nominal type, like a struct.  This
/// decl is always a DeclContext.
class NominalTypeDecl : public TypeDecl, public DeclContext,
                        public IterableDeclContext {
  SourceRange Braces;
  
  /// \brief The sets of implicit members and protocols added to imported enum
  /// types.  These members and protocols are added to the NominalDecl only if
  /// the nominal type is directly or indirectly referenced.
  ///
  /// FIXME: These should be side-table-allocated.
  ArrayRef<DelayedDecl> DelayedMembers;
  ArrayRef<DelayedProtocolDecl> DelayedProtocols;
  
  GenericParamList *GenericParams;
  
  /// Global declarations that were synthesized on this type's behalf, such as
  /// default operator definitions derived for protocol conformances.
  ArrayRef<Decl*> DerivedGlobalDecls;
  
  /// \brief The set of protocol conformance mappings. The element order
  /// corresponds to the order of Protocols returned by getProtocols().
  // FIXME: We don't really need this correspondence any more.
  LazyLoaderArray<ProtocolConformance *> Conformances;

  /// \brief The generic signature of this type.
  ///
  /// This is the semantic representation of a generic parameters and the
  /// requirements placed on them.
  ///
  /// FIXME: The generic parameters here are also derivable from
  /// \c GenericParams. However, we likely want to make \c GenericParams
  /// the parsed representation, and not part of the module file.
  GenericSignature *GenericSig = nullptr;

  /// \brief The first extension of this type.
  ExtensionDecl *FirstExtension = nullptr;

  /// \brief The last extension of this type, used solely for efficient
  /// insertion of new extensions.
  ExtensionDecl *LastExtension = nullptr;

  /// \brief The generation at which we last loaded extensions.
  unsigned ExtensionGeneration: 31;
                          
  /// \brief Whether or not the generic signature of the type declaration is
  /// currently being validated.
  unsigned ValidatingGenericSignature: 1;
                    

  /// \brief A lookup table containing all of the members of this type and
  /// its extensions.
  ///
  /// The table itself is lazily constructed and updated when
  /// lookupDirect() is called. The bit indicates whether the lookup
  /// table has already added members by walking the declarations in
  /// scope.
  llvm::PointerIntPair<MemberLookupTable *, 1, bool> LookupTable;

  /// Prepare the lookup table to make it ready for lookups.
  void prepareLookupTable();

  /// Note that we have added a member into the iterable declaration context,
  /// so that it can also be added to the lookup table (if needed).
  void addedMember(Decl *member);

  friend class MemberLookupTable;
  friend class ExtensionDecl;
  friend class IterableDeclContext;

protected:
  Type DeclaredTy;
  Type DeclaredTyInContext;

  void setDeclaredType(Type declaredTy) {
    assert(DeclaredTy.isNull() && "Already set declared type");
    DeclaredTy = declaredTy;
  }

  NominalTypeDecl(DeclKind K, DeclContext *DC, Identifier name,
                  SourceLoc NameLoc,
                  MutableArrayRef<TypeLoc> inherited,
                  GenericParamList *GenericParams) :
    TypeDecl(K, DC, name, NameLoc, inherited),
    DeclContext(DeclContextKind::NominalTypeDecl, DC),
    IterableDeclContext(IterableDeclContextKind::NominalTypeDecl),
    GenericParams(nullptr), DeclaredTy(nullptr)
  {
    setGenericParams(GenericParams);
    NominalTypeDeclBits.HasDelayedMembers = false;
    NominalTypeDeclBits.AddedImplicitInitializers = false;
    ExtensionGeneration = 0;
    ValidatingGenericSignature = false;
  }

  friend class ProtocolType;

public:
  using TypeDecl::getASTContext;

  DeclRange getMembers(bool forceDelayedMembers = true) const;
  SourceRange getBraces() const { return Braces; }
  
  void setBraces(SourceRange braces) { Braces = braces; }

  void setMemberLoader(LazyMemberLoader *resolver, uint64_t contextData);
  bool hasLazyMembers() const {
    return IterableDeclContext::isLazy();
  }
  
  void setIsValidatingGenericSignature(bool ivgs = true) {
    ValidatingGenericSignature = ivgs;
  }
  
  bool IsValidatingGenericSignature() {
    return ValidatingGenericSignature;
  }
  
  /// \brief Returns true if this this decl contains delayed value or protocol
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

  GenericParamList *getGenericParams() const { return GenericParams; }

  /// Provide the set of parameters to a generic type, or null if
  /// this function is not generic.
  void setGenericParams(GenericParamList *params);
  
  /// Set the generic signature of this type.
  void setGenericSignature(GenericSignature *sig);

  /// Retrieve the generic parameter types.
  ArrayRef<GenericTypeParamType *> getGenericParamTypes() const {
    if (!GenericSig)
      return { };

    return GenericSig->getGenericParams();
  }

  /// Retrieve the generic requirements.
  ArrayRef<Requirement> getGenericRequirements() const {
    if (!GenericSig)
      return { };

    return GenericSig->getRequirements();
  }
  
  /// Retrieve the generic signature.
  GenericSignature *getGenericSignature() const {
    return GenericSig;
  }

  /// getDeclaredType - Retrieve the type declared by this entity.
  Type getDeclaredType() const { return DeclaredTy; }

  /// Compute the type (and declared type) of this nominal type.
  void computeType();

  Type getDeclaredTypeInContext() const;

  /// Get the "interface" type of the given nominal type, which is the
  /// type used to refer to the nominal type externally.
  ///
  /// For a generic type, or a member thereof, this is the a specialization
  /// of the type using its own generic parameters.
  Type computeInterfaceType() const;

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
  ArrayRef<ValueDecl *> lookupDirect(DeclName name);

  /// Collect the set of protocols to which this type should implicitly
  /// conform, such as AnyObject (for classes).
  void getImplicitProtocols(SmallVectorImpl<ProtocolDecl *> &protocols);

  /// \brief True if the type can implicitly derive a conformance for the given
  /// protocol.
  ///
  /// If true, explicit conformance checking will synthesize implicit
  /// declarations for requirements of the protocol that are not satisfied by
  /// the type's explicit members.
  bool derivesProtocolConformance(ProtocolDecl *protocol) const;

  /// \brief Retrieve the set of protocol conformance mappings for this type.
  ///
  /// Calculated during type-checking.
  ArrayRef<ProtocolConformance *> getConformances() const;
  void setConformances(ArrayRef<ProtocolConformance *> c) {
    Conformances = c;
  }
  void setConformanceLoader(LazyMemberLoader *resolver, uint64_t contextData);

  using TypeDecl::getDeclaredInterfaceType;

  /// classifyAsOptionalType - Decide whether this declaration is one
  /// of the library-intrinsic Optional<T> or ImplicitlyUnwrappedOptional<T> types.
  OptionalTypeKind classifyAsOptionalType() const;
  
private:
  /// Predicate used to filter StoredPropertyRange.
  struct ToStoredProperty {
    Optional<VarDecl *> operator()(Decl *decl) const;
  };

  /// Force delayed implicit protocol declarations to be added to the type
  /// declaration.
  void forceDelayedProtocolDecls();
  
  /// Force delayed implicit member declarations to be added to the type
  /// declaration.
  void forceDelayedMemberDecls();
  
public:
  /// A range for iterating the stored member variables of a structure.
  using StoredPropertyRange = OptionalTransformRange<DeclRange,
                                                     ToStoredProperty>;

  /// Return a collection of the stored member variables of this type.
  StoredPropertyRange getStoredProperties() const {
    return StoredPropertyRange(getMembers(), ToStoredProperty());
  }
  
  ArrayRef<Decl *> getDerivedGlobalDecls() const { 
    return DerivedGlobalDecls;
  }

  void setDerivedGlobalDecls(MutableArrayRef<Decl*> decls) {
    DerivedGlobalDecls = decls;
  }
  
  bool hasDelayedMemberDecls() {
    return DelayedMembers.size() != 0;
  }
  
  void setDelayedMemberDecls(ArrayRef<DelayedDecl> delayedMembers) {
    DelayedMembers = delayedMembers;
    setHasDelayedMembers();
  }
  
  bool hasDelayedProtocolDecls() {
    return DelayedProtocols.size() != 0;
  }
  
  void setDelayedProtocolDecls(ArrayRef<DelayedProtocolDecl> delayedProtocols) {
    DelayedProtocols = delayedProtocols;
    setHasDelayedMembers();
  }
  
  /// Force delayed implicit member and protocol declarations to be added to the
  /// type declaration.
  void forceDelayed() {
    forceDelayedProtocolDecls();
    forceDelayedMemberDecls();
  }
  
  /// Override of getProtocols that forces any delayed protocol members to be
  /// resolved before returning the protocol array.
  ArrayRef<ProtocolDecl *> getProtocols(bool forceDelayedMembers = true) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_NominalTypeDecl &&
           D->getKind() <= DeclKind::Last_NominalTypeDecl;
  }
  static bool classof(const DeclContext *C) {
    return C->getContextKind() == DeclContextKind::NominalTypeDecl;
  }
  static bool classof(const IterableDeclContext *C) {
    return C->getIterableContextKind()
             == IterableDeclContextKind::NominalTypeDecl;
  }
  static bool classof(const NominalTypeDecl *D) { return true; }
  static bool classof(const ExtensionDecl *D) { return false; }

  using DeclContext::operator new;
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
///      case None
///      case Just(T)
///    }
/// \endcode
///
/// The type of the decl itself is a MetatypeType; use getDeclaredType()
/// to get the declared type ("Bool" or "Optional" in the above example).
class EnumDecl : public NominalTypeDecl {
  SourceLoc EnumLoc;
  Type RawType;

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

  /// Return a range that iterates over all the elements of an enum.
  ElementRange getAllElements() const {
    return ElementRange(getMembers());
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
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Enum;
  }
  static bool classof(const DeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
  static bool classof(const IterableDeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
  
  /// Determine whether this enum declares a raw type in its inheritance clause.
  bool hasRawType() const { return (bool)RawType; }
  /// Retrieve the declared raw type of the enum from its inheritance clause,
  /// or null if it has none.
  Type getRawType() const { return RawType; }

  /// Set the raw type of the enum from its inheritance clause.
  void setRawType(Type rawType) { RawType = rawType; }
  
  /// True if the enum is a "simple" enum, and none of its cases have associated
  /// payloads.
  bool isSimpleEnum() const;
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
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Struct;
  }
  static bool classof(const DeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
  static bool classof(const IterableDeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
  
  /// Does this struct contain unreferenceable storage, such as C fields that
  /// cannot be represented in Swift?
  bool hasUnreferenceableStorage() const {
    return StructDeclBits.HasUnreferenceableStorage;
  }
  
  void setHasUnreferenceableStorage(bool v) {
    StructDeclBits.HasUnreferenceableStorage = true;
  }
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
  Type Superclass;
  ObjCMethodLookupTable *ObjCMethodLookup = nullptr;

  /// Create the Objective-C member lookup table.
  void createObjCMethodLookup();

public:
  ClassDecl(SourceLoc ClassLoc, Identifier Name, SourceLoc NameLoc,
            MutableArrayRef<TypeLoc> Inherited,
            GenericParamList *GenericParams, DeclContext *DC);

  SourceLoc getStartLoc() const { return ClassLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(ClassLoc, getBraces().End);
  }

  /// Determine whether this class has a superclass.
  bool hasSuperclass() const { return (bool)Superclass; }

  /// Retrieve the superclass of this class, or null if there is no superclass.
  Type getSuperclass() const { return Superclass; }

  /// Set the superclass of this class.
  void setSuperclass(Type superclass) { Superclass = superclass; }

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
  bool isForeign() const {
    return ClassDeclBits.Foreign;
  }
  void setForeign(bool isForeign = true) {
    ClassDeclBits.Foreign = true;
  }

  /// Find a method of a class that overrides a given method.
  /// Return nullptr, if no such method exists.
  FuncDecl *findOverridingDecl(const FuncDecl *method) const;

  /// Find a method implementation which will be used when a given method
  /// is invoked on an instance of this class. This implementation may stem
  /// either from a class itself or its direct or indirect superclasses.
  FuncDecl *findImplementingMethod(const FuncDecl *method) const;

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

  /// Retrieve the name to use for this class when interoperating with
  /// the Objective-C runtime.
  StringRef getObjCRuntimeName(llvm::SmallVectorImpl<char> &buffer) const;

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
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Class;
  }
  static bool classof(const DeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
  static bool classof(const IterableDeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
};


/// ProtocolDecl - A declaration of a protocol, for example:
///
///   protocol Drawable {
///     func draw()
///   }
class ProtocolDecl : public NominalTypeDecl {
  SourceLoc ProtocolLoc;

  bool requiresClassSlow();

public:
  ProtocolDecl(DeclContext *DC, SourceLoc ProtocolLoc, SourceLoc NameLoc,
               Identifier Name, MutableArrayRef<TypeLoc> Inherited);
  
  using Decl::getASTContext;

  /// \brief Determine whether this protocol inherits from the given ("super")
  /// protocol.
  bool inheritsFrom(const ProtocolDecl *Super) const;
  
  /// \brief Collect all of the inherited protocols into the given set.
  void collectInherited(llvm::SmallPtrSet<ProtocolDecl *, 4> &Inherited);
  
  ProtocolType *getDeclaredType() const {
    return reinterpret_cast<ProtocolType *>(DeclaredTy.getPointer());
  }
  
  SourceLoc getStartLoc() const { return ProtocolLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(ProtocolLoc, getBraces().End);
  }

  /// \brief Retrieve the generic parameter 'Self'.
  GenericTypeParamDecl *getSelf() const;

  /// True if this protocol can only be conformed to by class types.
  bool requiresClass() {
    if (ProtocolDeclBits.RequiresClassValid)
      return ProtocolDeclBits.RequiresClass;

    return requiresClassSlow();
  }

  /// Specify that this protocol is class-bounded, e.g., because it was
  /// annotated with the 'class' keyword.
  void setRequiresClass() {
    ProtocolDeclBits.RequiresClassValid = true;
    ProtocolDeclBits.RequiresClass = true;
  }

  /// Determine whether an existential value conforming to just this protocol
  /// conforms to the protocol itself.
  ///
  /// \returns an empty optional if not yet known, true if the existential
  /// does conform to this protocol, and false otherwise.
  Optional<bool> existentialConformsToSelf() const {
    if (ProtocolDeclBits.ExistentialConformsToSelfValid)
      return ProtocolDeclBits.ExistentialConformsToSelf;

    return None;
  }

  /// Set whether the existential of this protocol type conforms to this
  /// protocol.
  void setExistentialConformsToSelf(bool conforms) {
    ProtocolDeclBits.ExistentialConformsToSelfValid = true;
    ProtocolDeclBits.ExistentialConformsToSelf = conforms;
  }

  /// If this is known to be a compiler-known protocol, returns the kind.
  /// Otherwise returns None.
  ///
  /// Note that this is only valid after type-checking.
  Optional<KnownProtocolKind> getKnownProtocolKind() const {
    if (ProtocolDeclBits.KnownProtocol == 0)
      return None;
    return static_cast<KnownProtocolKind>(ProtocolDeclBits.KnownProtocol - 1);
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
    ProtocolDeclBits.KnownProtocol = static_cast<unsigned>(kind) + 1;
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
    return ProtocolDeclBits.HasMissingRequirements;
  }

  void setHasMissingRequirements(bool newValue) {
    ProtocolDeclBits.HasMissingRequirements = newValue;
  }

  /// Retrieve the name to use for this protocol when interoperating
  /// with the Objective-C runtime.
  StringRef getObjCRuntimeName(llvm::SmallVectorImpl<char> &buffer) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Protocol;
  }
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Protocol;
  }
  static bool classof(const DeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
  static bool classof(const IterableDeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
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
  /// \brief This is an address accessor for a property or subscript.
  IsAddressor = 5,
  /// \brief This is a mutableAddress accessor for a property or subscript.
  IsMutableAddressor = 6,
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

  /// The decl has addressors; call the appropriate addressor for the
  /// access kind.  These calls are currently always direct.
  Addressor,

  /// Directly call the getter, setter, or materializeForSet accessor.
  DirectToAccessor,

  /// Indirectly call the getter, setter, or materializeForSet accessor.
  DispatchToAccessor,
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
    ///   3) Perhaps someday these will be used by accesses outside of this
    ///      resilience domain, when the owning type is resilient.
    ///
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
  struct AddressorRecord {
    FuncDecl *Address = nullptr;        // User-defined address accessor
    FuncDecl *MutableAddress = nullptr; // User-defined mutableAddress accessor

    GetSetRecord *getGetSet() {
      // Relies on not-strictly-portable ABI layout assumptions.
      return reinterpret_cast<GetSetRecord*>(this+1);
    }
  };
  void configureAddressorRecord(AddressorRecord *record,
                                FuncDecl *addressor, FuncDecl *mutableAddressor);

  struct GetSetRecord {
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

  struct GetSetRecordWithAddressors : AddressorRecord, GetSetRecord {};
  struct ObservingRecordWithAddressors : AddressorRecord, ObservingRecord {};

  llvm::PointerIntPair<GetSetRecord*, 2, OptionalEnum<Accessibility>> GetSetInfo;

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

  /// \brief Return the approproiate addressor for the given access kind.
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
  bool hasObjCGetterAndSetter() const;

  /// Return true if this storage *must* be accessed with Objective-C-compatible
  /// getters and setters.
  bool requiresObjCGetterAndSetter() const;

  /// Given that this is an Objective-C property or subscript declaration,
  /// produce its getter selector.
  ObjCSelector getObjCGetterSelector() const;

  /// Given that this is an Objective-C property or subscript declaration,
  /// produce its setter selector.
  ObjCSelector getObjCSetterSelector() const;

  AbstractStorageDecl *getOverriddenDecl() const {
    return OverriddenDecl;
  }
  void setOverriddenDecl(AbstractStorageDecl *over) {
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
  /// Overriddes are resolved during type checking; only query this field after
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

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_AbstractStorageDecl &&
           D->getKind() <= DeclKind::Last_AbstractStorageDecl;
  }
};

/// VarDecl - 'var' and 'let' declarations.
class VarDecl : public AbstractStorageDecl {
protected:
  llvm::PointerUnion<PatternBindingDecl *, Pattern *> ParentPattern;

  VarDecl(DeclKind Kind, bool IsStatic, bool IsLet, SourceLoc NameLoc,
          Identifier Name, Type Ty, DeclContext *DC)
    : AbstractStorageDecl(Kind, DC, Name, NameLoc) 
  {
    VarDeclBits.IsStatic = IsStatic;
    VarDeclBits.IsLet = IsLet;
    VarDeclBits.IsDebuggerVar = false;
    VarDeclBits.HasNonPatternBindingInit = false;
    setType(Ty);
  }

public:
  VarDecl(bool IsStatic, bool IsLet, SourceLoc NameLoc, Identifier Name,
          Type Ty, DeclContext *DC)
    : VarDecl(DeclKind::Var, IsStatic, IsLet, NameLoc, Name, Ty, DC) { }

  SourceLoc getStartLoc() const { return getNameLoc(); }
  SourceRange getSourceRange() const;

  /// \brief Retrieve the source range of the variable type.
  ///
  /// Only for use in diagnostics.  It is not always possible to always
  /// precisely point to the variable type because of type aliases.
  SourceRange getTypeSourceRangeForDiagnostics() const;

  /// \brief Returns whether the var is settable in the specified context: this
  /// is either because it is a stored var, because it has a custom setter, or
  /// is a let member in an initializer.
  ///
  /// Pass a null context to check if it's always settable.
  bool isSettable(DeclContext *UseDC) const;

  PatternBindingDecl *getParentPattern() const {
    return ParentPattern.dyn_cast<PatternBindingDecl *>();
  }
  void setParentPattern(PatternBindingDecl *PBD) {
    ParentPattern = PBD;
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

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { 
    return D->getKind() == DeclKind::Var || D->getKind() == DeclKind::Param; 
  }
};

/// A function parameter declaration.
class ParamDecl : public VarDecl {
  Identifier ArgumentName;
  SourceLoc ArgumentNameLoc;

public:
  ParamDecl(bool isLet, SourceLoc argumentNameLoc, 
            Identifier argumentName, SourceLoc parameterNameLoc,
            Identifier parameterName, Type ty, DeclContext *dc)
    : VarDecl(DeclKind::Param, /*IsState=*/false, isLet, parameterNameLoc, 
              parameterName, ty, dc),
      ArgumentName(argumentName), ArgumentNameLoc(argumentNameLoc) { }

  /// Retrieve the argument (API) name for this function parameter.
  Identifier getArgumentName() const { return ArgumentName; }

  /// Retrieve the source location of the argument (API) name.
  ///
  /// The resulting source location will be valid if the argument name
  /// was specified separately from the parameter name.
  SourceLoc getArgumentNameLoc() const { return ArgumentNameLoc; }

  SourceRange getSourceRange() const {
    if (ArgumentNameLoc.isValid() && getNameLoc().isInvalid())
      return ArgumentNameLoc;
    if (ArgumentNameLoc.isInvalid() && getNameLoc().isValid())
      return getNameLoc();
    return SourceRange(ArgumentNameLoc, getNameLoc());
  }

  Pattern *getParamParentPattern() const {
    return ParentPattern.dyn_cast<Pattern *>();
  }
  void setParamParentPattern(Pattern *Pat) {
    ParentPattern = Pat;
  }

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
class SubscriptDecl : public AbstractStorageDecl {
  SourceLoc ArrowLoc;
  Pattern *Indices;
  TypeLoc ElementTy;

public:
  SubscriptDecl(DeclName Name, SourceLoc SubscriptLoc, Pattern *Indices,
                SourceLoc ArrowLoc, TypeLoc ElementTy, DeclContext *Parent)
    : AbstractStorageDecl(DeclKind::Subscript, Parent, Name, SubscriptLoc),
      ArrowLoc(ArrowLoc), Indices(nullptr), ElementTy(ElementTy) {
    setIndices(Indices);
  }
  
  SourceLoc getSubscriptLoc() const { return getNameLoc(); }
  SourceLoc getStartLoc() const { return getSubscriptLoc(); }
  SourceRange getSourceRange() const;

  /// \brief Retrieve the indices for this subscript operation.
  Pattern *getIndices() { return Indices; }
  const Pattern *getIndices() const { return Indices; }
  void setIndices(Pattern *p);

  /// Retrieve the type of the indices.
  Type getIndicesType() const;

  /// Retrieve the interface type of the indices.
  Type getIndicesInterfaceType() const;

  /// \brief Retrieve the type of the element referenced by a subscript
  /// operation.
  Type getElementType() const { return ElementTy.getType(); }
  TypeLoc &getElementTypeLoc() { return ElementTy; }
  const TypeLoc &getElementTypeLoc() const { return ElementTy; }

  /// \brief Returns whether the result of the subscript operation can be set.
  bool isSettable() const;

  /// Determine the kind of Objective-C subscripting this declaration
  /// implies.
  ObjCSubscriptKind getObjCSubscriptKind() const;

  SubscriptDecl *getOverriddenDecl() const {
    return cast_or_null<SubscriptDecl>(
                                  AbstractStorageDecl::getOverriddenDecl());
  }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Subscript;
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

  CaptureInfo Captures;

  AbstractFunctionDecl(DeclKind Kind, DeclContext *Parent, DeclName Name,
                       SourceLoc NameLoc, unsigned NumParamPatterns,
                       GenericParamList *GenericParams)
      : ValueDecl(Kind, Parent, Name, NameLoc),
        DeclContext(DeclContextKind::AbstractFunctionDecl, Parent),
        Body(nullptr), GenericParams(nullptr) {
    setBodyKind(BodyKind::None);
    setGenericParams(GenericParams);
    AbstractFunctionDeclBits.NumParamPatterns = NumParamPatterns;
    AbstractFunctionDeclBits.Overridden = false;

    // Verify no bitfield truncation.
    assert(AbstractFunctionDeclBits.NumParamPatterns == NumParamPatterns);
  }

  MutableArrayRef<Pattern *> getBodyParamBuffer();

  void setBodyKind(BodyKind K) {
    AbstractFunctionDeclBits.BodyKind = unsigned(K);
  }

  void setGenericParams(GenericParamList *GenericParams);
public:
  // FIXME: Hack that provides names with keyword arguments for accessors.
  DeclName getEffectiveFullName() const;

  /// \brief If this is a method in a type extension for some type,
  /// return that type, otherwise return Type().
  Type getExtensionType() const;

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
           "can not set a body if it was skipped");

    Body = S;
    setBodyKind(NewBodyKind);
  }

  /// \brief Note that the body was skipped for this function.  Function body
  /// can not be attached after this call.
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

  /// Retrieve the source range of the function body.
  SourceRange getBodySourceRange() const;

  /// Retrieve the source range of the function declaration name + patterns.
  SourceRange getSignatureSourceRange() const;

  CaptureInfo &getCaptureInfo() { return Captures; }
  const CaptureInfo &getCaptureInfo() const { return Captures; }

  /// Retrieve the Objective-C selector that names this method.
  ObjCSelector getObjCSelector() const;

  /// Determine the default argument kind and type for the given argument index
  /// in this declaration, which must be a function or constructor.
  ///
  /// \param Index The index of the argument for which we are querying the
  /// default argument.
  ///
  /// \returns the default argument kind and, if there is a default argument,
  /// the type of the corresponding parameter.
  std::pair<DefaultArgumentKind, Type> getDefaultArg(unsigned Index) const;

  /// Determine whether the name of the ith argument is an API name by default.
  bool argumentNameIsAPIByDefault(unsigned i) const;

  unsigned getNumParamPatterns() const {
    return AbstractFunctionDeclBits.NumParamPatterns;
  }

  /// \brief Returns the "natural" number of argument clauses taken by this
  /// function.  This value is always at least one, and it may be more if the
  /// function is implicitly or explicitly curried.
  ///
  /// For example, this function:
  /// \code
  ///   func negate(x : Int) -> Int { return -x }
  /// \endcode
  /// has a natural argument count of 1 if it is freestanding.  If it is
  /// a method, it has a natural argument count of 2, as does this
  /// curried function:
  /// \code
  ///   func add(x : Int)(y : Int) -> Int { return x + y }
  /// \endcode
  ///
  /// This value never exceeds the number of chained function types
  /// in the function's type, but it can be less for functions which
  /// return a value of function type:
  /// \code
  ///   func const(x : Int) -> () -> Int { return { x } } // NAC==1
  /// \endcode
  unsigned getNaturalArgumentCount() const {
    return getNumParamPatterns();
  }

  /// \brief Returns the parameter pattern(s) for the function definition that
  /// determine the parameter names bound in the function body.
  ///
  /// The number of "top-level" elements in this pattern will match the number
  /// of argument names in the compound name of the function or constructor.
  MutableArrayRef<Pattern *> getBodyParamPatterns() {
    return getBodyParamBuffer();
  }
  ArrayRef<const Pattern *> getBodyParamPatterns() const {
    auto Patterns =
        const_cast<AbstractFunctionDecl *>(this)->getBodyParamBuffer();
    return ArrayRef<const Pattern *>(Patterns.data(), Patterns.size());
  }

  /// \brief If this is a method in a type or extension thereof, compute
  /// and return the type to be used for the 'self' argument of the type, or an
  /// empty Type() if no 'self' argument should exist.  This can
  /// only be used after name binding has resolved types.
  ///
  /// \param outerGenericParams If non-NULL, and this function is an instance
  /// of a generic type, will be set to the generic parameter list of that
  /// generic type.
  Type computeSelfType(GenericParamList **outerGenericParams = nullptr);

  /// \brief If this is a method in a type or extension thereof, compute
  /// and return the type to be used for the 'self' argument of the interface
  /// type, or an empty Type() if no 'self' argument should exist.  This can
  /// only be used after name binding has resolved types.
  ///
  /// \param isInitializingCtor Specifies whether we're computing the 'self'
  /// type of an initializing constructor, which accepts an instance 'self'
  /// rather than a metatype 'self'.
  Type computeInterfaceSelfType(bool isInitializingCtor);

  /// \brief This method returns the implicit 'self' decl.
  ///
  /// Note that some functions don't have an implicit 'self' decl, for example,
  /// free functions.  In this case nullptr is returned.
  VarDecl *getImplicitSelfDecl() const;

  /// \brief Retrieve the set of parameters to a generic function, or null if
  /// this function is not generic.
  GenericParamList *getGenericParams() const { return GenericParams; }

  /// \brief Determine whether this is a generic function, which can only be
  /// used when each of the archetypes is bound to a particular concrete type.
  bool isGeneric() const { return GenericParams != nullptr; }

  /// Retrieve the declaration that this method overrides, if any.
  AbstractFunctionDecl *getOverriddenDecl() const;

  /// Whether the declaration is later overridden in the module
  ///
  /// Overriddes are resolved during type checking; only query this field after
  /// the whole module has been checked
  bool isOverridden() const { return AbstractFunctionDeclBits.Overridden; }

  /// The declaration has been overridden in the module
  ///
  /// Resolved during type checking
  void setIsOverridden() { AbstractFunctionDeclBits.Overridden = true; }

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_AbstractFunctionDecl &&
           D->getKind() <= DeclKind::Last_AbstractFunctionDecl;
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::AbstractFunctionDecl;
  }
  
  using DeclContext::operator new;
  using Decl::getASTContext;
};

class OperatorDecl;


/// FuncDecl - 'func' declaration.
class FuncDecl : public AbstractFunctionDecl {
  friend class AbstractFunctionDecl;

  SourceLoc StaticLoc;  // Location of the 'static' token or invalid.
  SourceLoc FuncLoc;    // Location of the 'func' token.

  TypeLoc FnRetType;

  /// The result type as seen from the body of the function.
  ///
  /// \sa getBodyResultType()
  Type BodyResultType;

  /// \brief If this FuncDecl is an accessor for a property, this indicates
  /// which property and what kind of accessor.
  llvm::PointerIntPair<AbstractStorageDecl*, 3, AccessorKind> AccessorDecl;
  llvm::PointerUnion<FuncDecl *, NominalTypeDecl*> OverriddenOrDerivedForDecl;
  OperatorDecl *Operator;

  FuncDecl(SourceLoc StaticLoc, StaticSpellingKind StaticSpelling,
           SourceLoc FuncLoc, DeclName Name,
           SourceLoc NameLoc, unsigned NumParamPatterns,
           GenericParamList *GenericParams, Type Ty, DeclContext *Parent)
    : AbstractFunctionDecl(DeclKind::Func, Parent, Name, NameLoc,
                           NumParamPatterns, GenericParams),
      StaticLoc(StaticLoc), FuncLoc(FuncLoc),
      OverriddenOrDerivedForDecl(), Operator(nullptr) {
    FuncDeclBits.IsStatic = StaticLoc.isValid() || getName().isOperator();
    FuncDeclBits.StaticSpelling = static_cast<unsigned>(StaticSpelling);
    assert(NumParamPatterns > 0 && "Must have at least an empty tuple arg");
    setType(Ty);
    FuncDeclBits.Mutating = false;
    FuncDeclBits.HasDynamicSelf = false;
  }

  static FuncDecl *createImpl(ASTContext &Context, SourceLoc StaticLoc,
                              StaticSpellingKind StaticSpelling,
                              SourceLoc FuncLoc, DeclName Name,
                              SourceLoc NameLoc,
                              GenericParamList *GenericParams, Type Ty,
                              unsigned NumParamPatterns,
                              DeclContext *Parent,
                              ClangNode ClangN);

public:
  /// Factory function only for use by deserialization.
  static FuncDecl *createDeserialized(ASTContext &Context, SourceLoc StaticLoc,
                                      StaticSpellingKind StaticSpelling,
                                      SourceLoc FuncLoc, DeclName Name,
                                      SourceLoc NameLoc,
                                      GenericParamList *GenericParams, Type Ty,
                                      unsigned NumParamPatterns,
                                      DeclContext *Parent);

  static FuncDecl *create(ASTContext &Context, SourceLoc StaticLoc,
                          StaticSpellingKind StaticSpelling,
                          SourceLoc FuncLoc, DeclName Name, SourceLoc NameLoc,
                          GenericParamList *GenericParams, Type Ty,
                          ArrayRef<Pattern *> BodyParams,
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
  bool isMutating() const {
    return FuncDeclBits.Mutating;
  }
  void setStatic(bool IsStatic = true) {
    FuncDeclBits.IsStatic = IsStatic;
  }
  void setMutating(bool Mutating = true) {
    FuncDeclBits.Mutating = Mutating;
  }

  /// \returns true if this is non-mutating due to applying a 'mutating'
  /// attribute. For example a "mutating set" accessor.
  bool isExplicitNonMutating() const;

  void setDeserializedSignature(ArrayRef<Pattern *> BodyParams,
                                TypeLoc FnRetType);

  SourceLoc getStaticLoc() const { return StaticLoc; }
  SourceLoc getFuncLoc() const { return FuncLoc; }

  SourceLoc getStartLoc() const {
    return StaticLoc.isValid() ? StaticLoc : FuncLoc;
  }
  SourceRange getSourceRange() const;

  TypeLoc &getBodyResultTypeLoc() { return FnRetType; }
  const TypeLoc &getBodyResultTypeLoc() const { return FnRetType; }

  /// Retrieve the result type of this function.
  ///
  /// \sa getBodyResultType
  Type getResultType() const;

  /// Retrieve the result type of this function for use within the function
  /// definition.
  ///
  /// FIXME: The statement below is a wish, not reality.
  /// The "body" result type will only differ from the result type within the
  /// interface to the function for a polymorphic function, where the interface
  /// may contain generic parameters while the definition will contain
  /// the corresponding archetypes.
  Type getBodyResultType() const { return BodyResultType; }

  /// Set the result type as viewed from the function body.
  ///
  /// \sa getBodyResultType
  void setBodyResultType(Type bodyResultType) {
    assert(BodyResultType.isNull() && "Already set body result type");
    BodyResultType = bodyResultType;
  }

  /// Revert to an empty type.
  void revertType() {
    BodyResultType = Type();
    overwriteType(Type());
  }

  /// isUnaryOperator - Determine whether this is a unary operator
  /// implementation, in other words, the name of the function is an operator,
  /// and the argument list consists syntactically of a single-element tuple
  /// pattern. This check is syntactic rather than type-based in order to allow
  /// for the definition of unary operators on tuples, as in:
  ///   func [prefix] + (_:(a:Int, b:Int))
  /// This also allows the unary-operator-ness of a func decl to be determined
  /// prior to type checking.
  bool isUnaryOperator() const;
  
  /// isBinaryOperator - Determine whether this is a binary operator
  /// implementation, in other words, the name of the function is an operator,
  /// and the argument list consists syntactically of a two-element tuple
  /// pattern. This check is syntactic rather than type-based in order to
  /// distinguish a binary operator from a unary operator on tuples, as in:
  ///   func [prefix] + (_:(a:Int, b:Int)) // unary operator +(1,2)
  ///   func [infix]  + (a:Int, b:Int)     // binary operator 1 + 2
  /// This also allows the binary-operator-ness of a func decl to be determined
  /// prior to type checking.
  bool isBinaryOperator() const;
  
  /// makeAccessor - Note that this function is an accessor for the given
  /// VarDecl or SubscriptDecl.
  void makeAccessor(AbstractStorageDecl *D, AccessorKind Kind) {
    assert(Kind != AccessorKind::NotAccessor && "Must specify an accessor kind");
    AccessorDecl.setPointerAndInt(D, Kind);
  }

  AbstractStorageDecl *getAccessorStorageDecl() const {
    return AccessorDecl.getPointer();
  }

  AccessorKind getAccessorKind() const {
    if (AccessorDecl.getPointer() == nullptr)
      return AccessorKind::NotAccessor;
    return AccessorDecl.getInt();
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
  
  /// Retrieve the dynamic \c Self type for this method, or a null type if
  /// this method does not have a dynamic \c Self return type.
  DynamicSelfType *getDynamicSelf() const;

  /// Retrieve the dynamic \c Self interface type for this method, or
  /// a null type if this method does not have a dynamic \c Self
  /// return type.
  DynamicSelfType *getDynamicSelfInterface() const;

  /// Given that this is an Objective-C method declaration, get its selector.
  ObjCSelector getObjCSelector() const;

  void getLocalCaptures(SmallVectorImpl<CaptureInfo::
                           LocalCaptureTy> &Result) const {
    return getCaptureInfo().getLocalCaptures(this, Result);
  }
  
  /// Get the supertype method this method overrides, if any.
  FuncDecl *getOverriddenDecl() const {
    return OverriddenOrDerivedForDecl.dyn_cast<FuncDecl *>();
  }
  void setOverriddenDecl(FuncDecl *over) {
    // A function cannot be an override if it is also a derived global decl
    // (since derived decls are at global scope).
    assert((!OverriddenOrDerivedForDecl
            || !OverriddenOrDerivedForDecl.is<FuncDecl*>())
           && "function cannot be both override and derived global");
    OverriddenOrDerivedForDecl = over;
    over->setIsOverridden();
  }
  
  /// Get the type this function was implicitly generated on the behalf of for
  /// a derived protocol conformance, if any.
  NominalTypeDecl *getDerivedForTypeDecl() const {
    return OverriddenOrDerivedForDecl.dyn_cast<NominalTypeDecl *>();
  }
  void setDerivedForTypeDecl(NominalTypeDecl *ntd) {
    // A function cannot be an override if it is also a derived global decl
    // (since derived decls are at global scope).
    assert((!OverriddenOrDerivedForDecl
            || !OverriddenOrDerivedForDecl.is<NominalTypeDecl *>())
           && "function cannot be both override and derived global");
    OverriddenOrDerivedForDecl = ntd;
  }
  
  OperatorDecl *getOperatorDecl() const { return Operator; }
  void setOperatorDecl(OperatorDecl *o) {
    assert(isOperator() && "can't set an OperatorDecl for a non-operator");
    Operator = o;
  }

  /// Returns true if a function declaration overrides a given
  /// method  from its direct or indirect superclass.
  bool isOverridingDecl(const FuncDecl *method) const;

  static bool classof(const Decl *D) { return D->getKind() == DeclKind::Func; }
  static bool classof(const AbstractFunctionDecl *D) {
    return classof(static_cast<const Decl*>(D));
  }
  static bool classof(const DeclContext *DC) {
    if (auto fn = dyn_cast<AbstractFunctionDecl>(DC))
      return classof(fn);
    return false;
  }
};
  
/// \brief This represents a 'case' declaration in an 'enum', which may declare
/// one or more individual comma-separated EnumElementDecls.
class EnumCaseDecl : public Decl {
  SourceLoc CaseLoc;
  
  /// The number of tail-allocated element pointers.
  unsigned NumElements;
  
  EnumCaseDecl(SourceLoc CaseLoc,
               ArrayRef<EnumElementDecl *> Elements,
               DeclContext *DC)
    : Decl(DeclKind::EnumCase, DC),
      CaseLoc(CaseLoc), NumElements(Elements.size())
  {
    memcpy(this + 1, Elements.begin(), NumElements * sizeof(EnumElementDecl*));
  }
  
  EnumElementDecl * const *getElementsBuf() const {
    return reinterpret_cast<EnumElementDecl * const*>(this + 1);
  }
  
public:
  static EnumCaseDecl *create(SourceLoc CaseLoc,
                              ArrayRef<EnumElementDecl*> Elements,
                              DeclContext *DC);
  
  /// Get the list of elements declared in this case.
  ArrayRef<EnumElementDecl *> getElements() const {
    return {getElementsBuf(), NumElements};
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
  }

  bool hasArgumentType() const { return !ArgumentType.getType().isNull(); }
  Type getArgumentType() const { return ArgumentType.getType(); }
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
  /// A convenience initializer is written with a return type of "Self" in
  /// source code.
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
  friend class AbstractFunctionDecl;

  /// The failability of this initializer, which is an OptionalTypeKind.
  unsigned Failability : 2;

  /// The location of the '!' or '?' for a failable initializer.
  SourceLoc FailabilityLoc;

  Pattern *BodyParams[2];

  /// The type of the initializing constructor.
  Type InitializerType;

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
                  Pattern *SelfBodyParam, Pattern *BodyParams,
                  GenericParamList *GenericParams, DeclContext *Parent);

  void setBodyParams(Pattern *selfPattern, Pattern *bodyParams);

  SourceLoc getConstructorLoc() const { return getNameLoc(); }
  SourceLoc getStartLoc() const { return getConstructorLoc(); }
  SourceRange getSourceRange() const;

  /// getArgumentType - get the type of the argument tuple
  Type getArgumentType() const;

  /// \brief Get the type of the constructed object.
  Type getResultType() const;

  /// Given that this is an Objective-C method declaration, get its selector.
  ObjCSelector getObjCSelector() const;

  /// Get the type of the initializing constructor.
  Type getInitializerType() const { return InitializerType; }
  void setInitializerType(Type t) { InitializerType = t; }

  /// Get the interface type of the initializing constructor.
  Type getInitializerInterfaceType();
  void setInitializerInterfaceType(Type t);

  /// Get the typechecked call to super.init expression, which needs to be
  /// inserted at the end of the initializer by SILGen.
  Expr *getSuperInitCall() { return CallToSuperInit; }
  void setSuperInitCall(Expr *CallExpr) { CallToSuperInit = CallExpr; }

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
                                              ApplyExpr **init = nullptr);

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
    OverriddenDecl = over;
    over->setIsOverridden();
  }

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
  friend class AbstractFunctionDecl;
  Pattern *SelfPattern;
public:
  DestructorDecl(Identifier NameHack, SourceLoc DestructorLoc,
                 Pattern *SelfPattern, DeclContext *Parent);
  
  void setSelfPattern(Pattern *selfPattern);

  SourceLoc getDestructorLoc() const { return getNameLoc(); }
  SourceLoc getStartLoc() const { return getDestructorLoc(); }
  SourceRange getSourceRange() const;

  /// Retrieve the Objective-C selector associated with the destructor.
  ///
  /// This is always "dealloc".
  ObjCSelector getObjCSelector() const;

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

/// Abstract base class of operator declarations.
class OperatorDecl : public Decl {
  SourceLoc OperatorLoc, NameLoc, LBraceLoc, RBraceLoc;
  
  Identifier name;

public:
  OperatorDecl(DeclKind kind,
               DeclContext *DC,
               SourceLoc OperatorLoc,
               Identifier Name,
               SourceLoc NameLoc,
               SourceLoc LBraceLoc,
               SourceLoc RBraceLoc)
    : Decl(kind, DC),
      OperatorLoc(OperatorLoc), NameLoc(NameLoc),
      LBraceLoc(LBraceLoc), RBraceLoc(RBraceLoc),
      name(Name) {}
  
  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const { return {OperatorLoc, RBraceLoc}; }

  SourceLoc getOperatorLoc() const { return OperatorLoc; }
  SourceLoc getLBraceLoc() const { return LBraceLoc; }
  SourceLoc getRBraceLoc() const { return RBraceLoc; }
  Identifier getName() const { return name; }
  
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_OperatorDecl
      && D->getKind() <= DeclKind::Last_OperatorDecl;
  }
};

/// Declares the behavior of an infix operator. For example:
///
/// \code
/// infix operator /+/ {
///   associativity left
///   precedence 123
/// }
/// \endcode
class InfixOperatorDecl : public OperatorDecl {
  SourceLoc AssociativityLoc, AssociativityValueLoc,
    PrecedenceLoc, PrecedenceValueLoc,
    AssignmentLoc;

public:
  InfixOperatorDecl(DeclContext *DC,
                    SourceLoc OperatorLoc,
                    Identifier Name,
                    SourceLoc NameLoc,
                    SourceLoc LBraceLoc,
                    bool IsAssocImplicit,
                    SourceLoc AssociativityLoc,
                    SourceLoc AssociativityValueLoc,
                    bool IsPrecedenceImplicit,
                    SourceLoc PrecedenceLoc,
                    SourceLoc PrecedenceValueLoc,
                    bool IsAssignmentImplicit,
                    SourceLoc AssignmentLoc,
                    SourceLoc RBraceLoc,
                    InfixData InfixData)
    : OperatorDecl(DeclKind::InfixOperator, DC,
                   OperatorLoc,
                   Name,
                   NameLoc,
                   LBraceLoc,
                   RBraceLoc),
      AssociativityLoc(AssociativityLoc),
      AssociativityValueLoc(AssociativityValueLoc),
      PrecedenceLoc(PrecedenceLoc),
      PrecedenceValueLoc(PrecedenceValueLoc),
      AssignmentLoc(AssignmentLoc) {
    if (!InfixData.isValid()) {
      setInvalid();
    } else {
      assert((AssociativityLoc.isInvalid() || !IsAssocImplicit) &&
             "Associativity cannot be implicit if it came from user source");
      assert((PrecedenceLoc.isInvalid() || !IsPrecedenceImplicit) &&
             "Precedence cannot be implicit if it came from user source");
      InfixOperatorDeclBits.Precedence = InfixData.getPrecedence();
      InfixOperatorDeclBits.Associativity =
        static_cast<unsigned>(InfixData.getAssociativity());
      InfixOperatorDeclBits.Assignment =
         unsigned(InfixData.isAssignment());
      InfixOperatorDeclBits.IsPrecedenceImplicit = IsPrecedenceImplicit;
      InfixOperatorDeclBits.IsAssocImplicit = IsAssocImplicit;
      InfixOperatorDeclBits.IsAssignmentImplicit = IsAssignmentImplicit;
    }
  }
  
  SourceLoc getAssociativityLoc() const { return AssociativityLoc; }
  SourceLoc getAssociativityValueLoc() const { return AssociativityValueLoc; }
  SourceLoc getPrecedenceLoc() const { return PrecedenceLoc; }
  SourceLoc getPrecedenceValueLoc() const { return PrecedenceValueLoc; }
  SourceLoc getAssignmentLoc() const { return AssignmentLoc; }

  unsigned getPrecedence() const {
    return InfixOperatorDeclBits.Precedence;
  }

  Associativity getAssociativity() const {
    return Associativity(InfixOperatorDeclBits.Associativity);
  }
  
  bool isAssignment() const {
    return InfixOperatorDeclBits.Assignment;
  }

  InfixData getInfixData() const {
    if (isInvalid())
      return InfixData();
    return InfixData(getPrecedence(), getAssociativity(), isAssignment());
  }

  bool isAssociativityImplicit() const {
    return InfixOperatorDeclBits.IsAssocImplicit;
  }
  bool isPrecedenceImplicit() const {
    return InfixOperatorDeclBits.IsPrecedenceImplicit;
  }
  bool isAssignmentImplicit() const {
    return InfixOperatorDeclBits.IsAssignmentImplicit;
  }
  
  /// True if this decl's attributes conflict with those declared by another
  /// operator.
  bool conflictsWith(InfixOperatorDecl *other) {
    return getInfixData() != other->getInfixData();
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
                     SourceLoc NameLoc, SourceLoc LBraceLoc,
                     SourceLoc RBraceLoc)
    : OperatorDecl(DeclKind::PrefixOperator, DC,
                   OperatorLoc, Name, NameLoc, LBraceLoc, RBraceLoc) {}

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
                     SourceLoc NameLoc, SourceLoc LBraceLoc,
                     SourceLoc RBraceLoc)
    : OperatorDecl(DeclKind::PostfixOperator, DC, OperatorLoc, Name,
                   NameLoc, LBraceLoc, RBraceLoc) {}
  
  /// True if this decl's attributes conflict with those declared by another
  /// PostfixOperatorDecl.
  bool conflictsWith(PostfixOperatorDecl *other) {
    return false;
  }
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::PostfixOperator;
  }
};

inline bool ValueDecl::isSettable(DeclContext *UseDC) const {
  if (auto vd = dyn_cast<VarDecl>(this)) {
    return vd->isSettable(UseDC);
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

inline MutableArrayRef<Pattern *> AbstractFunctionDecl::getBodyParamBuffer() {
  unsigned NumPatterns = AbstractFunctionDeclBits.NumParamPatterns;
  Pattern **Ptr;
  switch (getKind()) {
  default: llvm_unreachable("Unknown AbstractFunctionDecl!");
  case DeclKind::Constructor:
    Ptr = cast<ConstructorDecl>(this)->BodyParams;
    break;

  case DeclKind::Destructor:
    Ptr = &cast<DestructorDecl>(this)->SelfPattern;
    break;

  case DeclKind::Func:
    // Body patterns are tail allocated.
    Ptr = reinterpret_cast<Pattern **>(cast<FuncDecl>(this) + 1);
    break;
  }
  return MutableArrayRef<Pattern *>(Ptr, NumPatterns);
}

inline DeclIterator &DeclIterator::operator++() {
  Current = Current->NextDecl;
  return *this;
}

} // end namespace swift

#endif
