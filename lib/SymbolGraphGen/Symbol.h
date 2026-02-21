//===--- Symbol.h- Symbol Graph Node --------------------------------------===//
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

#ifndef SWIFT_SYMBOLGRAPHGEN_SYMBOL_H
#define SWIFT_SYMBOLGRAPHGEN_SYMBOL_H

#include "llvm/Support/JSON.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "swift/Markup/Markup.h"
#include "swift/SymbolGraphGen/PathComponent.h"
#include "swift/SymbolGraphGen/FragmentInfo.h"

namespace swift {
namespace symbolgraphgen {

struct Availability;
struct SymbolGraphASTWalker;
struct SymbolGraph;

/// A symbol from a module: a node in a graph.
class Symbol {
  /// The symbol graph in which this symbol resides.
  SymbolGraph *Graph;
  /// Either a ValueDecl* or ExtensionDecl*.
  const Decl *D;
  Type BaseType;
  const ValueDecl *SynthesizedBaseTypeDecl;

  Symbol(SymbolGraph *Graph, const ValueDecl *VD, const ExtensionDecl *ED,
         const ValueDecl *SynthesizedBaseTypeDecl,
         Type BaseTypeForSubstitution = Type());

  swift::DeclName getName(const Decl *D) const;

  void serializeKind(StringRef Identifier, StringRef DisplayName,
                     llvm::json::OStream &OS) const;

  void serializeKind(llvm::json::OStream &OS) const;

  void serializeIdentifier(llvm::json::OStream &OS) const;

  void serializePathComponents(llvm::json::OStream &OS) const;

  void serializeNames(llvm::json::OStream &OS) const;

  void serializePosition(StringRef Key, SourceLoc Loc,
                         SourceManager &SourceMgr,
                         llvm::json::OStream &OS) const;

  void serializeRange(size_t InitialIndentation,
                      SourceRange Range, SourceManager &SourceMgr,
                      llvm::json::OStream &OS) const;

  void serializeDocComment(llvm::json::OStream &OS) const;

  void serializeFunctionSignature(llvm::json::OStream &OS) const;

  void serializeGenericParam(const swift::GenericTypeParamType &Param,
                             llvm::json::OStream &OS) const;

  void serializeSwiftGenericMixin(llvm::json::OStream &OS) const;

  void serializeSwiftExtensionMixin(llvm::json::OStream &OS) const;

  void serializeDeclarationFragmentMixin(llvm::json::OStream &OS) const;

  void serializeAccessLevelMixin(llvm::json::OStream &OS) const;

  void serializeMetadataMixin(llvm::json::OStream &OS) const;

  void serializeLocationMixin(llvm::json::OStream &OS) const;

  void serializeAvailabilityMixin(llvm::json::OStream &OS) const;

  void serializeSPIMixin(llvm::json::OStream &OS) const;

public:
  Symbol(SymbolGraph *Graph, const ExtensionDecl *ED,
         const ValueDecl *SynthesizedBaseTypeDecl,
         Type BaseTypeForSubstitution = Type());

  Symbol(SymbolGraph *Graph, const ValueDecl *VD,
         const ValueDecl *SynthesizedBaseTypeDecl,
         Type BaseTypeForSubstitution = Type());

  void serialize(llvm::json::OStream &OS) const;

  const SymbolGraph *getGraph() const {
    return Graph;
  }

  const ValueDecl *getSymbolDecl() const;

  const Decl *getLocalSymbolDecl() const { return D; }

  Type getBaseType() const {
    return BaseType;
  }

  const ValueDecl *getSynthesizedBaseTypeDecl() const {
    return SynthesizedBaseTypeDecl;
  }

  /// Retrieve the path components associated with this symbol, from outermost
  /// to innermost (this symbol).
  void getPathComponents(SmallVectorImpl<PathComponent> &Components) const;

  /// Retrieve information about all symbols referenced in the declaration
  /// fragment printed for this symbol.
  void getFragmentInfo(SmallVectorImpl<FragmentInfo> &FragmentInfo) const;

  /// Print the symbol path to an output stream.
  void printPath(llvm::raw_ostream &OS) const;

  void getUSR(SmallVectorImpl<char> &USR) const;
  
  /// If this symbol is inheriting docs from a parent class, protocol, or default
  /// implementation, returns that decl. Returns null if there are no docs or if
  /// the symbol has its own doc comments to render.
  const ValueDecl *getDeclInheritingDocs() const;

  /// If this symbol is an implementation of a protocol requirement for a
  /// protocol declared outside its module, returns the upstream decl for that
  /// requirement.
  const ValueDecl *getForeignProtocolRequirement() const;

  /// If this symbol is an implementation of a protocol requirement, returns the
  /// upstream decl for that requirement.
  const ValueDecl *getProtocolRequirement() const;

  /// If this symbol is a synthesized symbol or an implementation of a protocol
  /// requirement, returns the upstream decl.
  const ValueDecl *getInheritedDecl() const;

  static bool supportsKind(DeclKind Kind);

  /// Determines the effective access level of the given extension.
  ///
  /// The effective access level is defined as the minimum of:
  ///  - the maximum access level of a property or conformance
  ///  - the access level of the extended nominal
  ///
  /// The effective access level is defined this way so that the extension
  /// symbol's access level equals the highest access level of any of the
  /// symbols the extension symbol has a relationship to.
  ///
  /// This function is not logically equivalent to
  /// `ExtensionDecl.getMaxAccessLevel()`, which computes the maximum access
  /// level any of the `ExtensionDecl`'s members
  /// **can** have based on the extended type and types used in constraints.
  static AccessLevel getEffectiveAccessLevel(const ExtensionDecl *ED);

  /// Determines the kind of Symbol the given declaration produces and
  /// returns the respective symbol kind identifier and kind name.
  static std::pair<StringRef, StringRef> getKind(const Decl *D);
};

} // end namespace symbolgraphgen
} // end namespace swift

namespace llvm {
using Symbol = swift::symbolgraphgen::Symbol;
using SymbolGraph = swift::symbolgraphgen::SymbolGraph;

template <> struct DenseMapInfo<Symbol> {
  static inline Symbol getEmptyKey() {
    return Symbol{
        DenseMapInfo<SymbolGraph *>::getEmptyKey(),
        DenseMapInfo<const swift::ValueDecl *>::getEmptyKey(),
        DenseMapInfo<const swift::ValueDecl *>::getTombstoneKey(),
        DenseMapInfo<swift::Type>::getEmptyKey(),
    };
  }
  static inline Symbol getTombstoneKey() {
    return Symbol{
        DenseMapInfo<SymbolGraph *>::getTombstoneKey(),
        DenseMapInfo<const swift::ValueDecl *>::getTombstoneKey(),
        DenseMapInfo<const swift::ValueDecl *>::getTombstoneKey(),
        DenseMapInfo<swift::Type>::getTombstoneKey(),
    };
  }
  static unsigned getHashValue(const Symbol S) {
    unsigned H = 0;
    H ^= DenseMapInfo<SymbolGraph *>::getHashValue(S.getGraph());
    H ^=
        DenseMapInfo<const swift::Decl *>::getHashValue(S.getLocalSymbolDecl());
    H ^= DenseMapInfo<const swift::ValueDecl *>::getHashValue(
        S.getSynthesizedBaseTypeDecl());
    H ^= DenseMapInfo<swift::Type>::getHashValue(S.getBaseType());
    return H;
  }
  static bool isEqual(const Symbol LHS, const Symbol RHS) {
    return LHS.getGraph() == RHS.getGraph() &&
           LHS.getLocalSymbolDecl() == RHS.getLocalSymbolDecl() &&
           LHS.getSynthesizedBaseTypeDecl() ==
               RHS.getSynthesizedBaseTypeDecl() &&
           DenseMapInfo<swift::Type>::isEqual(LHS.getBaseType(),
                                              RHS.getBaseType());
  }
};
} // end namespace llvm


#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOL_H
