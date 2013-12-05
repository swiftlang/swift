//===--- DeclContext.h - Swift Language Context ASTs ------------*- C++ -*-===//
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
// This file defines the DeclContext class.  A DeclContext is the semantic
// construct that a declaration belongs to, such as the enclosing
// AbstractClosureExpr or declaration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECLCONTEXT_H
#define SWIFT_DECLCONTEXT_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
  class ASTContext;
  class ASTWalker;
  class DeclContext;
  class GenericParamList;
  class LazyResolver;
  class SourceFile;
  class Type;
  class Module;
  class ValueDecl;
}

namespace llvm {
  class raw_ostream;
  template<>
  class PointerLikeTypeTraits<swift::DeclContext*> {
  public:
    static void *getAsVoidPointer(swift::DeclContext* P) {
      return (void*)P;
    }
    static swift::DeclContext *getFromVoidPointer(void *P) {
      return (swift::DeclContext*)P;
    }
    enum { NumLowBitsAvailable = 3 };
  };
}

namespace swift {

// The indentation of the members of this enum describe the inheritance
// hierarchy.  Commented out members are abstract classes.  This formation
// allows for range checks in classof.
enum class DeclContextKind : uint8_t {
  Module,
  FileUnit,
  AbstractClosureExpr,
  NominalTypeDecl,
  ExtensionDecl,
  TopLevelCodeDecl,
  AbstractFunctionDecl,

  Last_DeclContextKind = AbstractFunctionDecl
};

/// A DeclContext is an AST object which acts as a semantic container
/// for declarations.  As a policy matter, we currently define
/// contexts broadly: a lambda expression in a function is a new
/// DeclContext, but a new brace statement is not.  There's no
/// particular mandate for this, though.
///
/// Note that DeclContexts have stricter alignment requirements than AST nodes
/// in general, so if an AST node class multiply inherits from DeclContext
/// and another base class, it must 'using DeclContext::operator new;' in order
/// to use an allocator with the correct alignment.
class alignas(8) DeclContext {
  // alignas(8) because we use three tag bits on DeclContext.
  
  enum {
    KindBits =
        llvm::PointerLikeTypeTraits<swift::DeclContext*>::NumLowBitsAvailable
  };
  static_assert(unsigned(DeclContextKind::Last_DeclContextKind) < 1U<<KindBits,
                "Not enough KindBits for DeclContextKind");
  
  llvm::PointerIntPair<DeclContext*, KindBits, DeclContextKind> ParentAndKind;
  
public:
  DeclContext(DeclContextKind Kind, DeclContext *Parent)
    : ParentAndKind(Parent, Kind) {
    assert((Parent != 0 || isModuleContext()) &&
           "DeclContext must have a parent unless it is a module!");
  }

  /// Returns the kind of context this is.
  DeclContextKind getContextKind() const {
    return ParentAndKind.getInt();
  }

  /// Determines whether this context is itself a local scope in a
  /// code block.  A context that appears in such a scope, like a
  /// local type declaration, does not itself become a local context.
  bool isLocalContext() const {
    return getContextKind() == DeclContextKind::AbstractClosureExpr ||
           getContextKind() == DeclContextKind::TopLevelCodeDecl ||
           getContextKind() == DeclContextKind::AbstractFunctionDecl;
  }
  
  /// isModuleContext - Return true if this is a subclass of Module.
  bool isModuleContext() const {
    return getContextKind() == DeclContextKind::Module;
  }

  /// \returns true if this is a context with module-wide scope, e.g. a module
  /// or a source file.
  bool isModuleScopeContext() const {
    return getContextKind() == DeclContextKind::Module ||
           getContextKind() == DeclContextKind::FileUnit;
  }

  /// \returns true if this is a type context, e.g., a struct, a class, an
  /// enum, a protocol, or an extension.
  bool isTypeContext() const {
    return getContextKind() == DeclContextKind::NominalTypeDecl ||
           getContextKind() == DeclContextKind::ExtensionDecl;
  }

  /// \brief Determine whether this is an extension context.
  bool isExtensionContext() const {
    return getContextKind() == DeclContextKind::ExtensionDecl;
  }

  /// getDeclaredTypeOfContext - For a type context, retrieves the declared
  /// type of the context. Returns a null type for non-type contexts.
  Type getDeclaredTypeOfContext() const;

  /// getDeclaredTypeInContext - For a type context, retrieves the declared
  /// type of the context as visible from within the context. Returns a null
  /// type for non-type contexts.
  Type getDeclaredTypeInContext();

  /// Determine the type of 'self' within the given context.
  ///
  /// \param isStatic Whether we're referring to a static method or some other
  /// static entity, for which the 'self' type will be a metatype.
  ///
  /// \param isConstructor Whether we're producing the 'self' type for a
  /// constructor.

  /// \param outerGenericParams If non-null, receives the generic parameters
  /// corresponding to this declaration (if any).
  ///
  /// \returns the type of the 'self' parameter.
  Type getSelfTypeInContext(bool isStatic, bool isConstructor,
                            GenericParamList **outerGenericParams);

  /// \brief Retrieve the innermost generic parameters introduced by this
  /// context or one of its parent contexts, or null if this context is not
  /// directly dependent on any generic parameters.
  GenericParamList *getGenericParamsOfContext() const;

  /// Returns this or the first local parent context, or nullptr if it is not
  /// contained in one.
  DeclContext *getLocalContext();
  const DeclContext *getLocalContext() const {
    return const_cast<DeclContext*>(this)->getLocalContext();
  }

  /// Returns the semantic parent of this context.  A context has a
  /// parent if and only if it is not a module context.
  DeclContext *getParent() const {
    return ParentAndKind.getPointer();
  }

  /// Return true if this is a child of the specified other decl context.
  bool isChildContextOf(DeclContext *Other) const {
    if (this == Other) return false;

    for (const DeclContext *CurContext = this; CurContext;
         CurContext = CurContext->getParent())
      if (CurContext == Other)
        return true;
    return false;
  }

  /// Returns the module context that contains this context.
  Module *getParentModule() const;

  /// Returns the module scope context that contains this context.
  ///
  /// This is either a Module or a SourceFile.
  DeclContext *getModuleScopeContext() const;

  /// Returns the source file that contains this context, or null if this
  /// is not within a source file.
  SourceFile *getParentSourceFile() const;

  /// Determine whether this declaration context is generic, meaning that it or
  /// any of its parents have generic parameters.
  bool isGenericContext() const;

  /// Determine whether the innermost context is generic.
  bool isInnermostContextGeneric() const;

  /// Look for the set of declarations with the given name within a type,
  /// its extensions and, optionally, its supertypes.
  ///
  /// This routine performs name lookup within a given type, its extensions
  /// and, optionally, its supertypes and their extensions, from the perspective
  /// of the current DeclContext. It can eliminate non-visible, hidden, and
  /// overridden declarations from the result set. It does not, however, perform
  /// any filtering based on the semantic usefulness of the results.
  ///
  /// \param type The type to look into.
  ///
  /// \param name The name to search for.
  ///
  /// \param options Options that control name lookup, based on the
  /// \c NL_* constants in \c NameLookupOptions.
  ///
  /// \param typeResolver Used to resolve types, usually for overload purposes.
  /// May be null.
  ///
  /// \param[out] decls Will be populated with the declarations found by name
  /// lookup.
  ///
  /// \returns true if anything was found.
  bool lookupQualified(Type type, Identifier name, unsigned options,
                       LazyResolver *typeResolver,
                       SmallVectorImpl<ValueDecl *> &decls) const;

  /// Return the ASTContext for a specified DeclContext by
  /// walking up to the enclosing module and returning its ASTContext.
  ASTContext &getASTContext() const;

  /// \returns true if traversal was aborted, false otherwise.
  bool walkContext(ASTWalker &Walker);

  void dumpContext() const;
  unsigned printContext(llvm::raw_ostream &OS) const;
  
  // Only allow allocation of DeclContext using the allocator in ASTContext.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(DeclContext));
};
  
} // end namespace swift

#endif
