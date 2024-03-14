//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "Utils.h"
#include "swift/AST/NameLookup.h"

using namespace swift;
using namespace swift::refactoring;

llvm::StringRef
swift::refactoring::correctNameInternal(ASTContext &Ctx, StringRef Name,
                                        ArrayRef<ValueDecl *> AllVisibles) {
  // If we find the collision.
  bool FoundCollision = false;

  // The suffixes we cannot use by appending to the original given name.
  llvm::StringSet<> UsedSuffixes;
  for (auto VD : AllVisibles) {
    StringRef S = VD->getBaseName().userFacingName();
    if (!S.starts_with(Name))
      continue;
    StringRef Suffix = S.substr(Name.size());
    if (Suffix.empty())
      FoundCollision = true;
    else
      UsedSuffixes.insert(Suffix);
  }
  if (!FoundCollision)
    return Name;

  // Find the first suffix we can use.
  std::string SuffixToUse;
  for (unsigned I = 1;; I++) {
    SuffixToUse = std::to_string(I);
    if (UsedSuffixes.count(SuffixToUse) == 0)
      break;
  }
  return Ctx.getIdentifier((llvm::Twine(Name) + SuffixToUse).str()).str();
}

llvm::StringRef swift::refactoring::correctNewDeclName(SourceLoc Loc,
                                                       DeclContext *DC,
                                                       StringRef Name) {

  // Collect all visible decls in the decl context.
  llvm::SmallVector<ValueDecl *, 16> AllVisibles;
  VectorDeclConsumer Consumer(AllVisibles);
  ASTContext &Ctx = DC->getASTContext();
  lookupVisibleDecls(Consumer, Loc, DC, /*IncludeTopLevel*/ true);
  return correctNameInternal(Ctx, Name, AllVisibles);
}

/// If \p NTD is a protocol, return all the protocols it inherits from. If it's
/// a type, return all the protocols it conforms to.
SmallVector<ProtocolDecl *, 2>
swift::refactoring::getAllProtocols(NominalTypeDecl *NTD) {
  if (auto Proto = dyn_cast<ProtocolDecl>(NTD)) {
    return SmallVector<ProtocolDecl *, 2>(
        Proto->getInheritedProtocols().begin(),
        Proto->getInheritedProtocols().end());
  } else {
    return NTD->getAllProtocols();
  }
}
