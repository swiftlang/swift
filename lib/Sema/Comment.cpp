//===--- CommentConversion.cpp - Conversion of comments to other formats --===//
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

#include "swift/AST/Comment.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/TypeCheckRequests.h"
#include <queue>

using namespace swift;

namespace {
/// Helper class for finding the comment providing decl for either a brief or
/// raw comment.
template <typename Result>
class CommentProviderFinder final {
  using ResultWithDecl = std::pair<Result, const Decl *>;
  using VisitFnTy = std::optional<Result> (*)(const Decl *);

  VisitFnTy VisitFn;

public:
  CommentProviderFinder(VisitFnTy visitFn) : VisitFn(visitFn) {}

private:
  std::optional<ResultWithDecl> visit(const Decl *D) {
    // Adapt the provided visitor function to also return the decl.
    if (auto result = VisitFn(D))
      return {{*result, D}};
    return std::nullopt;
  }

  std::optional<ResultWithDecl> findOverriddenDecl(const ValueDecl *VD) {
    // Only applies to class member.
    if (!VD->getDeclContext()->getSelfClassDecl())
      return std::nullopt;

    while (auto *baseDecl = VD->getOverriddenDecl()) {
      if (auto result = visit(baseDecl))
        return result;

      VD = baseDecl;
    }
    return std::nullopt;
  }

  /// Check if there is an inherited protocol that has a default implementation
  /// of `VD` with a doc comment.
  std::optional<ResultWithDecl> findDefaultProvidedDecl(const ValueDecl *VD) {
    NominalTypeDecl *nominalType =
        dyn_cast_or_null<NominalTypeDecl>(VD->getDeclContext()->getAsDecl());
    if (!nominalType) {
      nominalType = VD->getDeclContext()->getExtendedProtocolDecl();
    }
    if (!nominalType)
      return std::nullopt;

    SmallVector<ValueDecl *, 2> members;
    nominalType->lookupQualified(nominalType, DeclNameRef(VD->getName()),
                                 VD->getLoc(), NLOptions::NL_ProtocolMembers,
                                 members);

    std::optional<ResultWithDecl> result;
    Type vdComparisonTy = VD->getInterfaceType();
    if (!vdComparisonTy) {
      return std::nullopt;
    }
    if (auto fnTy = vdComparisonTy->getAs<AnyFunctionType>()) {
      // Strip off the 'Self' parameter.
      vdComparisonTy = fnTy->getResult();
    }

    for (auto *member : members) {
      if (VD == member || member->isInvalid()) {
        continue;
      }
      if (!isa<TypeDecl>(member)) {
        if (!swift::TypeChecker::witnessStructureMatches(member, VD)) {
          continue;
        }
        Type memberComparisonTy = member->getInterfaceType();
        if (!memberComparisonTy) {
          continue;
        }
        if (auto fnTy = memberComparisonTy->getAs<AnyFunctionType>()) {
          // Strip off the 'Self' parameter.
          memberComparisonTy = fnTy->getResult();
        }
        if (!vdComparisonTy->matches(memberComparisonTy,
                                     TypeMatchFlags::AllowOverride)) {
          continue;
        }
      }
      auto newResult = visit(member);
      if (!newResult)
        continue;

      if (result) {
        // Found two or more decls with doc-comment.
        return std::nullopt;
      }
      result = newResult;
    }
    return result;
  }

  std::optional<ResultWithDecl> findRequirementDecl(const ValueDecl *VD) {
    std::queue<const ValueDecl *> requirements;
    while (true) {
      for (auto *req : VD->getSatisfiedProtocolRequirements()) {
        if (auto result = visit(req))
          return result;

        requirements.push(req);
      }
      if (requirements.empty())
        return std::nullopt;

      VD = requirements.front();
      requirements.pop();
    }
  }

public:
  std::optional<ResultWithDecl> findCommentProvider(const Decl *D) {
    if (auto result = visit(D))
      return result;

    auto *VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      return std::nullopt;

    if (auto result = findOverriddenDecl(VD))
      return result;

    if (auto result = findRequirementDecl(VD))
      return result;

    if (auto result = findDefaultProvidedDecl(VD))
      return result;

    return std::nullopt;
  }
};
} // end anonymous namespace

const Decl *DocCommentProvidingDeclRequest::evaluate(Evaluator &evaluator,
                                                     const Decl *D) const {
  // Search for the first decl we see with a non-empty raw comment.
  auto finder = CommentProviderFinder<RawComment>(
      [](const Decl *D) -> std::optional<RawComment> {
        auto comment = D->getRawComment();
        if (comment.isEmpty())
          return std::nullopt;
        return comment;
      });

  auto result = finder.findCommentProvider(D);
  return result ? result->second : nullptr;
}

/// Retrieve the brief comment for a given decl \p D, without attempting to
/// walk any requirements or overrides.
static std::optional<StringRef> getDirectBriefComment(const Decl *D) {
  if (!D->canHaveComment())
    return std::nullopt;

  auto *ModuleDC = D->getDeclContext()->getModuleScopeContext();
  auto &Ctx = ModuleDC->getASTContext();

  // If we expect the comment to be in the swiftdoc, check for it if we loaded a
  // swiftdoc. If missing from the swiftdoc, we know it will not be in the
  // swiftsourceinfo either, so we can bail early.
  if (auto *Unit = dyn_cast<FileUnit>(ModuleDC)) {
    if (Unit->hasLoadedSwiftDoc()) {
      auto target = getDocCommentSerializationTargetFor(D);
      if (target == DocCommentSerializationTarget::SwiftDocAndSourceInfo) {
        auto C = Unit->getCommentForDecl(D);
        if (!C)
          return std::nullopt;

        return C->Brief;
      }
    }
  }

  // Otherwise, parse the brief from the raw comment itself. This will look into
  // the swiftsourceinfo if needed.
  auto RC = D->getRawComment();
  if (RC.isEmpty())
    return std::nullopt;

  SmallString<256> BriefStr;
  llvm::raw_svector_ostream OS(BriefStr);
  printBriefComment(RC, OS);
  return Ctx.AllocateCopy(BriefStr.str());
}

StringRef SemanticBriefCommentRequest::evaluate(Evaluator &evaluator,
                                                const Decl *D) const {
  // Perform a walk over the potential providers of the brief comment,
  // retrieving the first one we come across.
  CommentProviderFinder<StringRef> finder(getDirectBriefComment);
  auto result = finder.findCommentProvider(D);
  return result ? result->first : StringRef();
}
