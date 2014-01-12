//===--- Mangle.h - Interface to Swift symbol mangling ----------*- C++ -*-===//
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

#ifndef __SWIFT_AST_MANGLE_H__
#define __SWIFT_AST_MANGLE_H__

#include "llvm/ADT/DenseMap.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"

namespace swift {
class AbstractClosureExpr;

namespace Mangle {
  
/// ExplosionKind - A policy for choosing what types should be
/// exploded, as informed by the resilience model.
enum class ExplosionKind : unsigned {
  /// A minimal explosion does not explode types that do not have a
  /// universally fragile representation.  This provides a baseline
  /// for what all components can possibly support.
  ///   - All exported functions must be compiled to at least provide
  ///     a minimally-exploded entrypoint, or else it will be
  ///     impossible for components that do not have that type
  ///     to call the function.
  ///   - Similarly, any sort of opaque function call must be through
  ///     a minimally-exploded entrypoint.
  Minimal,
  
  /// A maximal explosion explodes all types with fragile
  /// representation, even when they're not universally fragile.  This
  /// is useful when internally manipulating objects or when working
  /// with specialized entry points for a function.
  Maximal,
  
  Last_ExplosionKind = Maximal
};
 
enum class IncludeType : bool { No, Yes };

enum class OperatorFixity {
  NotOperator,
  Infix,
  Prefix,
  Postfix
};
  
/// A class for mangling declarations.
class Mangler {
  struct ArchetypeInfo {
    unsigned Depth;
    unsigned Index;
  };

  raw_ostream &Buffer;
  llvm::DenseMap<void*, unsigned> Substitutions;
  llvm::DenseMap<ArchetypeType*, ArchetypeInfo> Archetypes;
  unsigned ArchetypesDepth = 0;
  DeclContext *DeclCtx = nullptr;
  /// If enabled, Arche- and Alias types are mangled with context.
  bool DWARFMangling;

public:
  enum BindGenerics : unsigned {
    /// We don't intend to mangle any sort of type within this context
    /// and so do not require its generic parameters to be bound.
    None,

    /// We are going to mangle a method declared in this context
    /// (which must be a type context) and do not need its generic
    /// parameters, or the parameters from its enclosing types, to be
    /// bound because we will bind them as part of processing the
    /// method's formal signature.
    Enclosing,

    /// We intend to mangle a type which may be dependent on the
    /// context and so require all generic parameters to be bound.
    All
  };
  
  class ContextStack {
    Mangler &M;
    unsigned OldDepth;
    ContextStack(const ContextStack &) = delete;
    ContextStack &operator=(const ContextStack &) = delete;
  public:
    ContextStack(Mangler &M) : M(M), OldDepth(M.ArchetypesDepth) {
      M.ArchetypesDepth = 0;
    }
    ~ContextStack() { M.ArchetypesDepth = OldDepth; }
  };

  /// \param DWARFMangling - use the 'Qq' mangling format for
  /// archetypes and the 'a' mangling for alias types.
  Mangler(raw_ostream &buffer, bool DWARFMangling = false)
    : Buffer(buffer), DWARFMangling(DWARFMangling) {}
  void mangleContextOf(ValueDecl *decl, BindGenerics shouldBind);
  void mangleContext(DeclContext *ctx, BindGenerics shouldBind);
  void mangleModule(Module *module);
  void mangleDeclName(ValueDecl *decl);
  void mangleDeclType(ValueDecl *decl, ExplosionKind kind,
                      unsigned uncurryingLevel);
  void mangleEntity(ValueDecl *decl, ExplosionKind kind,
                    unsigned uncurryingLevel);
  void mangleConstructorEntity(ConstructorDecl *ctor, bool isAllocating,
                               ExplosionKind kind, unsigned uncurryingLevel);
  void mangleDestructorEntity(DestructorDecl *decl, bool isDeallocating);
  void mangleIVarInitDestroyEntity(ClassDecl *decl, bool isDestroyer);
  void mangleGetterEntity(ValueDecl *decl, ExplosionKind explosionKind);
  void mangleSetterEntity(ValueDecl *decl, ExplosionKind explosionKind);
  void mangleAddressorEntity(ValueDecl *decl);
  void mangleDefaultArgumentEntity(DeclContext *ctx, unsigned index);
  void mangleInitializerEntity(VarDecl *var);
  void mangleClosureEntity(AbstractClosureExpr *closure,
                           ExplosionKind explosion, unsigned uncurryingLevel);
  void mangleNominalType(NominalTypeDecl *decl, ExplosionKind explosionKind,
                         BindGenerics shouldBind);
  void mangleType(CanType type, ExplosionKind kind, unsigned uncurryingLevel);
  void mangleDirectness(bool isIndirect);
  void mangleProtocolName(ProtocolDecl *protocol);
  void mangleProtocolConformance(ProtocolConformance *conformance);
  void bindGenericParameters(const GenericParamList *genericParams,
                             bool mangleParameters);
  void addSubstitution(void *ptr);

  void mangleDeclTypeForDebugger(ValueDecl *decl);
  void mangleTypeForDebugger(Type decl, DeclContext *DC);
  
private:
  void mangleFunctionType(CanAnyFunctionType fn, ExplosionKind explosionKind,
                          unsigned uncurryingLevel);
  void mangleProtocolList(ArrayRef<ProtocolDecl*> protocols);
  void mangleProtocolList(ArrayRef<Type> protocols);
  void mangleIdentifier(Identifier ident,
                        OperatorFixity fixity = OperatorFixity::NotOperator);
  void manglePolymorphicType(const GenericParamList *genericParams, CanType T,
                             ExplosionKind explosion, unsigned uncurryLevel,
                             bool mangleAsFunction);
  bool tryMangleStandardSubstitution(NominalTypeDecl *type);
  bool tryMangleSubstitution(void *ptr);
};
  
} // end namespace Mangle
} // end namespace swift

#endif
