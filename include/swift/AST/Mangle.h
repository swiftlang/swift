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
#include "swift/AST/ResilienceExpansion.h"

namespace swift {

class AbstractClosureExpr;

namespace Mangle {
  
enum class IncludeType : bool { No, Yes };

enum class OperatorFixity {
  NotOperator,
  Infix,
  Prefix,
  Postfix
};

/// Defined in include/swift/SIL/Mangle.h
class SpecializationManglerBase;
  
/// A class for mangling declarations.
class Mangler {
  struct ArchetypeInfo {
    unsigned Depth;
    unsigned Index;
  };

  raw_ostream &Buffer;
  llvm::DenseMap<const void *, unsigned> Substitutions;
  llvm::DenseMap<const ArchetypeType *, ArchetypeInfo> Archetypes;
  unsigned ArchetypesDepth = 0;
  ModuleDecl *Mod = nullptr;
  const DeclContext *DeclCtx = nullptr;
  /// If enabled, Arche- and Alias types are mangled with context.
  bool DWARFMangling;
  /// If enabled, non-ASCII names are encoded in modified Punycode.
  bool UsePunycode;

  friend class SpecializationManglerBase;

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

  void setModuleContext(ModuleDecl *M) { Mod = M; }

  /// \param DWARFMangling - use the 'Qq' mangling format for
  /// archetypes and the 'a' mangling for alias types.
  /// \param usePunycode - emit modified Punycode instead of UTF-8.
  Mangler(raw_ostream &buffer, bool DWARFMangling = false, 
          bool usePunycode = true)
    : Buffer(buffer), DWARFMangling(DWARFMangling), UsePunycode(usePunycode) {}
  void mangleContextOf(const ValueDecl *decl, BindGenerics shouldBind);
  void mangleContext(const DeclContext *ctx, BindGenerics shouldBind);
  void mangleModule(const ModuleDecl *module);
  void mangleDeclName(const ValueDecl *decl);
  void mangleDeclType(const ValueDecl *decl, ResilienceExpansion expansion,
                      unsigned uncurryingLevel);
  void mangleEntity(const ValueDecl *decl, ResilienceExpansion expansion,
                    unsigned uncurryingLevel);
  void mangleConstructorEntity(const ConstructorDecl *ctor, bool isAllocating,
                               ResilienceExpansion kind,
                               unsigned uncurryingLevel);
  void mangleDestructorEntity(const DestructorDecl *decl, bool isDeallocating);
  void mangleIVarInitDestroyEntity(const ClassDecl *decl, bool isDestroyer);
  void mangleAccessorEntity(AccessorKind kind, AddressorKind addressorKind,
                            const AbstractStorageDecl *decl,
                            ResilienceExpansion expansion);
  void mangleAddressorEntity(const ValueDecl *decl);
  void mangleGlobalGetterEntity(ValueDecl *decl);
  void mangleDefaultArgumentEntity(const DeclContext *ctx, unsigned index);
  void mangleInitializerEntity(const VarDecl *var);
  void mangleClosureEntity(const SerializedAbstractClosureExpr *closure,
                           ResilienceExpansion explosion,
                           unsigned uncurryingLevel);
  void mangleClosureEntity(const AbstractClosureExpr *closure,
                           ResilienceExpansion explosion,
                           unsigned uncurryingLevel);
  void mangleNominalType(const NominalTypeDecl *decl,
                         ResilienceExpansion expansion,
                         BindGenerics shouldBind,
                         const GenericParamList *extGenericParams = nullptr);
  void mangleProtocolDecl(const ProtocolDecl *protocol);
  void mangleType(Type type, ResilienceExpansion expansion,
                  unsigned uncurryingLevel);
  void mangleDirectness(bool isIndirect);
  void mangleProtocolName(const ProtocolDecl *protocol);
  void mangleProtocolConformance(const ProtocolConformance *conformance);
  void bindGenericParameters(const GenericParamList *genericParams,
                             bool mangleParameters);
  void addSubstitution(const void *ptr);

  void mangleDeclTypeForDebugger(const ValueDecl *decl);
  void mangleTypeForDebugger(Type decl, const DeclContext *DC);
  void mangleGenericSignature(const GenericSignature *sig,
                              ResilienceExpansion expansion);

  void mangleFieldOffsetFull(const ValueDecl *decl, bool isIndirect);
  void mangleTypeMetadataFull(CanType ty, bool isPattern, bool isIndirect);

  void mangleGlobalVariableFull(const VarDecl *decl);
  
  /// Mangles globalinit_token and globalinit_func, which are used to
  /// initialize global variables.
  /// \param decl The global variable or one of the global variables of a
  /// pattern, e.g. var (a, b) = (1, 2)
  /// \param counter A consecutive number inside the compiled file.
  /// \param isInitFunc If true it's a globalinit_func, otherwise a
  /// globalinit_token.
  void mangleGlobalInit(const VarDecl *decl, int counter, bool isInitFunc);

  void mangleIdentifier(StringRef ref,
                        OperatorFixity fixity = OperatorFixity::NotOperator,
                        bool isOperator=false);
private:
  void mangleFunctionType(AnyFunctionType *fn, ResilienceExpansion expansion,
                          unsigned uncurryingLevel);
  void mangleProtocolList(ArrayRef<ProtocolDecl*> protocols);
  void mangleProtocolList(ArrayRef<Type> protocols);
  void mangleIdentifier(Identifier ident,
                        OperatorFixity fixity = OperatorFixity::NotOperator);

  void mangleClosureComponents(Type Ty, unsigned discriminator, bool isImplicit,
                               const DeclContext *parentContext,
                               const DeclContext *localContext);

  void manglePolymorphicType(const GenericParamList *genericParams, Type T,
                             ResilienceExpansion expansion,
                             unsigned uncurryLevel,
                             bool mangleAsFunction);
  bool tryMangleStandardSubstitution(const NominalTypeDecl *type);
  bool tryMangleSubstitution(const void *ptr);

  /// \brief Mangles a sugared type iff we are mangling for the debugger.
  template <class T> void mangleSugaredType(Type type) {
    assert(DWARFMangling &&
           "sugared types are only legal when mangling for the debugger");
    auto *BlandTy = cast<T>(type.getPointer())->getSinglyDesugaredType();
    mangleType(BlandTy, ResilienceExpansion::Minimal, 0);
  }

};
  
} // end namespace Mangle
} // end namespace swift

#endif
