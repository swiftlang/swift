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
#include "swift/Basic/LLVM.h"
#include "swift/Markup/Markup.h"

namespace swift {
namespace symbolgraphgen {

struct Availability;
struct SymbolGraphASTWalker;
struct SymbolGraph;

/// A symbol from a module: a node in a graph.
class Symbol {
  /// The symbol graph in which this symbol resides.
  SymbolGraph *Graph;
  const ValueDecl *VD;
  const NominalTypeDecl *SynthesizedBaseTypeDecl;

  void serializeKind(StringRef Identifier, StringRef DisplayName,
                     llvm::json::OStream &OS) const;

  void serializeKind(llvm::json::OStream &OS) const;

  void serializeIdentifier(llvm::json::OStream &OS) const;

  void serializePathComponents(llvm::json::OStream &OS) const;

  void serializeNames(llvm::json::OStream &OS) const;

  void serializePosition(StringRef Key, SourceLoc Loc,
                         SourceManager &SourceMgr,
                         llvm::json::OStream &OS) const;

  void serializeRange(size_t InitialIdentation,
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

  void serializeLocationMixin(llvm::json::OStream &OS) const;

  void serializeAvailabilityMixin(llvm::json::OStream &OS) const;

public:
  Symbol(SymbolGraph *Graph, const ValueDecl *VD,
         const NominalTypeDecl *SynthesizedBaseTypeDecl);

  void serialize(llvm::json::OStream &OS) const;

  const SymbolGraph *getGraph() const {
    return Graph;
  }

  const ValueDecl *getSymbolDecl() const {
    return VD;
  }

  Type getSynthesizedBaseType() const {
    if (SynthesizedBaseTypeDecl) {
      return SynthesizedBaseTypeDecl->getDeclaredInterfaceType();
    } else {
      return Type();
    }
  }

  const NominalTypeDecl *getSynthesizedBaseTypeDecl() const {
    return SynthesizedBaseTypeDecl;
  }

  void getPathComponents(SmallVectorImpl<SmallString<32>> &Components) const;

  /// Print the symbol path to an output stream.
  void printPath(llvm::raw_ostream &OS) const;

  void getUSR(SmallVectorImpl<char> &USR) const;
};

} // end namespace symbolgraphgen
} // end namespace swift

namespace llvm {
using Symbol = swift::symbolgraphgen::Symbol;
using SymbolGraph = swift::symbolgraphgen::SymbolGraph;

template <> struct DenseMapInfo<Symbol> {
  static inline Symbol getEmptyKey() {
    return Symbol {
      DenseMapInfo<SymbolGraph *>::getEmptyKey(),
      DenseMapInfo<const swift::ValueDecl *>::getEmptyKey(),
      DenseMapInfo<const swift::NominalTypeDecl *>::getTombstoneKey(),
    };
  }
  static inline Symbol getTombstoneKey() {
    return Symbol {
      DenseMapInfo<SymbolGraph *>::getTombstoneKey(),
      DenseMapInfo<const swift::ValueDecl *>::getTombstoneKey(),
      DenseMapInfo<const swift::NominalTypeDecl *>::getTombstoneKey(),
    };
  }
  static unsigned getHashValue(const Symbol S) {
    unsigned H = 0;
    H ^= DenseMapInfo<SymbolGraph *>::getHashValue(S.getGraph());
    H ^= DenseMapInfo<const swift::ValueDecl *>::getHashValue(S.getSymbolDecl());
    H ^= DenseMapInfo<const swift::NominalTypeDecl *>::getHashValue(S.getSynthesizedBaseTypeDecl());
    return H;
  }
  static bool isEqual(const Symbol LHS, const Symbol RHS) {
    return LHS.getGraph() == RHS.getGraph() &&
        LHS.getSymbolDecl() == RHS.getSymbolDecl() &&
        LHS.getSynthesizedBaseTypeDecl() ==
            RHS.getSynthesizedBaseTypeDecl();
  }
};
} // end namespace llvm


#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOL_H
