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
#include "SymbolGraph.h"

namespace swift {
namespace symbolgraphgen {

struct AvailabilityDomain;
struct SymbolGraphASTWalker;

/// A symbol from a module: a node in a graph.
struct Symbol {
  /// The symbol graph in which this symbol resides.
  SymbolGraph &Graph;

  const ValueDecl *VD;

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

  void serializeGenericRequirement(const swift::Requirement &Req,
                                   llvm::json::OStream &OS) const;

  void serializeSwiftGenericMixin(llvm::json::OStream &OS) const;

  void serializeSwiftExtensionMixin(llvm::json::OStream &OS) const;

  void serializeDeclarationFragmentMixin(llvm::json::OStream &OS) const;

  void serializeAccessLevelMixin(llvm::json::OStream &OS) const;

  void serializeLocationMixin(llvm::json::OStream &OS) const;

  llvm::Optional<StringRef>
  getDomain(PlatformAgnosticAvailabilityKind AgnosticKind,
            PlatformKind Kind) const;

  void serializeAvailabilityMixin(llvm::json::OStream &OS) const;

  void serialize(llvm::json::OStream &OS) const;
  
  bool operator==(const Symbol &Other) const {
    return VD == Other.VD;
  }
};

} // end namespace symbolgraphgen
} // end namespace swift 

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOL_H
