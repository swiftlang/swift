//===--- Mangle.h - Interface to Swift symbol mangling ----------*- C++ -*-===//
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

#ifndef __SWIFT_AST_MANGLE_H__
#define __SWIFT_AST_MANGLE_H__

#include "llvm/ADT/DenseMap.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"

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

  
/// A class for mangling declarations. The Mangler accumulates name fragments
/// with the mangleXXX methods, and the final string is constructed with the
/// `finalize` method, after which the Mangler should not be used.
class Mangler {
  llvm::SmallVector<char, 128> Storage;
  llvm::raw_svector_ostream Buffer;

  llvm::DenseMap<const void *, unsigned> Substitutions;
  CanGenericSignature CurGenericSignature;
  ModuleDecl *Mod = nullptr;
  const DeclContext *DeclCtx = nullptr;
  /// If enabled, Arche- and Alias types are mangled with context.
  bool DWARFMangling;
  /// If enabled, non-ASCII names are encoded in modified Punycode.
  bool UsePunycode;
  /// Optimize out protocol names if a type only conforms to one protocol.
  bool OptimizeProtocolNames;

public:
  /// Finish the mangling of the symbol and return the mangled name.
  std::string finalize();

  /// Finish the mangling of the symbol and write the mangled name into
  /// \p stream.
  void finalize(llvm::raw_ostream &stream);

  void setModuleContext(ModuleDecl *M) { Mod = M; }

  /// \param DWARFMangling - use the 'Qq' mangling format for
  /// archetypes and the 'a' mangling for alias types.
  /// \param usePunycode - emit modified Punycode instead of UTF-8.
  Mangler(bool DWARFMangling = false,
          bool usePunycode = true,
          bool OptimizeProtocolNames = true)
    : Buffer(Storage), DWARFMangling(DWARFMangling), UsePunycode(usePunycode),
      OptimizeProtocolNames(OptimizeProtocolNames) {}
  void mangleContextOf(const ValueDecl *decl);
  void mangleContext(const DeclContext *ctx);
  void mangleModule(const ModuleDecl *module);
  void mangleDeclName(const ValueDecl *decl);
  void mangleDeclType(const ValueDecl *decl, unsigned uncurryingLevel);

  void mangleEntity(const ValueDecl *decl, unsigned uncurryingLevel);
  void mangleConstructorEntity(const ConstructorDecl *ctor, bool isAllocating,
                               unsigned uncurryingLevel);
  void mangleDestructorEntity(const DestructorDecl *decl, bool isDeallocating);
  void mangleIVarInitDestroyEntity(const ClassDecl *decl, bool isDestroyer);
  void mangleAccessorEntity(AccessorKind kind, AddressorKind addressorKind,
                            const AbstractStorageDecl *decl);
  void mangleAddressorEntity(const ValueDecl *decl);
  void mangleGlobalGetterEntity(ValueDecl *decl);
  void mangleDefaultArgumentEntity(const DeclContext *ctx, unsigned index);
  void mangleInitializerEntity(const VarDecl *var);
  void mangleClosureEntity(const SerializedAbstractClosureExpr *closure,
                           unsigned uncurryingLevel);
  void mangleClosureEntity(const AbstractClosureExpr *closure,
                           unsigned uncurryingLevel);
  void mangleNominalType(const NominalTypeDecl *decl);
  void mangleBoundGenericType(Type type);
  void mangleProtocolDecl(const ProtocolDecl *protocol);
  void mangleType(Type type, unsigned uncurryingLevel);
  void mangleLegacyBoxType(CanType fieldType);
  void mangleDirectness(bool isIndirect);
  void mangleProtocolName(const ProtocolDecl *protocol);
  void mangleProtocolConformance(const ProtocolConformance *conformance);
  void bindGenericParameters(CanGenericSignature sig);
  void bindGenericParameters(const DeclContext *DC);
  void addSubstitution(const void *ptr);

  void mangleDeclTypeForDebugger(const ValueDecl *decl);
  void mangleTypeForDebugger(Type decl, const DeclContext *DC);
  void mangleGenericSignature(const GenericSignature *sig);

  void mangleFieldOffsetFull(const ValueDecl *decl, bool isIndirect);
  void mangleTypeMetadataFull(CanType ty, bool isPattern);
  void mangleTypeFullMetadataFull(CanType ty);
  void mangleGlobalVariableFull(const VarDecl *decl);

  /// Adds the string \p S into the mangled name.
  void append(StringRef S);

  /// Adds the char \p C into the mangled name.
  void append(char C);

  /// Add the already mangled symbol \p Name as an identifier. (using the
  /// length prefix).
  void mangleIdentifierSymbol(StringRef Name);

  /// Add the already mangled symbol \p Name. This gives the mangler the
  /// opportunity to decode \p Name before adding it to the mangled name.
  void appendSymbol(StringRef Name);

  /// Mangle the integer \p Nat into the name.
  void mangleNatural(const APInt &Nat);

  /// Mangles globalinit_token and globalinit_func, which are used to
  /// initialize global variables.
  /// \param decl The global variable or one of the global variables of a
  /// pattern, e.g. var (a, b) = (1, 2)
  /// \param counter A consecutive number inside the compiled file.
  /// \param isInitFunc If true it's a globalinit_func, otherwise a
  /// globalinit_token.
  void mangleGlobalInit(const VarDecl *decl, int counter, bool isInitFunc);
  
  void mangleBehaviorInitThunk(const VarDecl *decl);

  void mangleIdentifier(StringRef ref,
                        OperatorFixity fixity = OperatorFixity::NotOperator,
                        bool isOperator=false);

  /// This checks whether a given array of generic type parameters are in a
  /// good order. Returns true on good order; false on malformed order.
  static bool checkGenericParamsOrder(ArrayRef<GenericTypeParamType *> params);

private:
  void mangleFunctionType(AnyFunctionType *fn, unsigned uncurryingLevel);
  void mangleProtocolList(ArrayRef<ProtocolDecl*> protocols);
  void mangleProtocolList(ArrayRef<Type> protocols);
  void mangleIdentifier(Identifier ident,
                        OperatorFixity fixity = OperatorFixity::NotOperator);
  void mangleIdentifier(DeclBaseName ident,
                        OperatorFixity fixity = OperatorFixity::NotOperator);

  void mangleClosureComponents(Type Ty, unsigned discriminator, bool isImplicit,
                               const DeclContext *parentContext,
                               const DeclContext *localContext);

  bool tryMangleStandardSubstitution(const NominalTypeDecl *type);
  bool tryMangleSubstitution(const void *ptr);

  /// \brief Mangles a sugared type iff we are mangling for the debugger.
  template <class T> void mangleSugaredType(Type type) {
    assert(DWARFMangling &&
           "sugared types are only legal when mangling for the debugger");
    auto *BlandTy = cast<T>(type.getPointer())->getSinglyDesugaredType();
    mangleType(BlandTy, 0);
  }

  void mangleGenericSignatureParts(ArrayRef<GenericTypeParamType *> params,
                                   unsigned initialParamDepth,
                                   ArrayRef<Requirement> requirements);
  CanType getDeclTypeForMangling(const ValueDecl *decl,
                                 ArrayRef<GenericTypeParamType *> &genericParams,
                                 unsigned &initialParamIndex,
                                 ArrayRef<Requirement> &requirements,
                                 SmallVectorImpl<Requirement> &requirementsBuf);

  void mangleGenericParamIndex(GenericTypeParamType *paramTy);
  void mangleAssociatedTypeName(DependentMemberType *dmt,
                                bool canAbbreviate);
  void mangleConstrainedType(CanType type);
  void mangleLayoutConstraint(LayoutConstraint layout);
};

} // end namespace Mangle
} // end namespace swift

#endif
