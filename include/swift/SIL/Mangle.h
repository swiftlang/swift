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

#ifndef __SWIFT_SIL_MANGLE_H__
#define __SWIFT_SIL_MANGLE_H__

#include "llvm/ADT/DenseMap.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"

namespace swift {
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
  
public:
  Mangler(raw_ostream &buffer) : Buffer(buffer) {}
  void mangleContextOf(ValueDecl *decl);
  void mangleDeclContext(DeclContext *ctx);
  void mangleDeclName(ValueDecl *decl, IncludeType includeType);
  void mangleDeclType(ValueDecl *decl, ExplosionKind kind,
                      unsigned uncurryingLevel);
  void mangleEntity(ValueDecl *decl, ExplosionKind kind,
                    unsigned uncurryingLevel);
  void mangleNominalType(NominalTypeDecl *decl, ExplosionKind explosionKind);
  void mangleType(Type type, ExplosionKind kind, unsigned uncurryingLevel);
  void mangleDirectness(bool isIndirect);
  
private:
  void mangleFunctionType(AnyFunctionType *fn, ExplosionKind explosionKind,
                          unsigned uncurryingLevel);
  void mangleProtocolList(ArrayRef<ProtocolDecl*> protocols);
  void mangleProtocolList(ArrayRef<Type> protocols);
  void mangleProtocolName(ProtocolDecl *protocol);
  void mangleIdentifier(Identifier ident,
                        OperatorFixity fixity = OperatorFixity::NotOperator);
  void mangleGetterOrSetterContext(FuncDecl *fn);
  void bindGenericParameters(const GenericParamList *genericParams,
                             bool mangleParameters);
  void manglePolymorphicType(const GenericParamList *genericParams, Type T,
                             ExplosionKind explosion, unsigned uncurryLevel,
                             bool mangleAsFunction);
  bool tryMangleStandardSubstitution(NominalTypeDecl *type);
  bool tryMangleSubstitution(void *ptr);
  void addSubstitution(void *ptr);
};
  
} // end namespace Mangle
} // end namespace swift

#endif