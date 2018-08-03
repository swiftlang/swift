//===--- ASTScope.cpp - Swift AST Scope -----------------------------------===//
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
//
// This file implements the ASTScope class and related functionality, which
// describes the scopes that exist within a Swift AST.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTScope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include <algorithm>

using namespace swift;

const ASTScope *ASTScope::getActiveContinuation() const {
  switch (continuation.getInt()) {
  case ContinuationKind::Historical:
    return nullptr;

  case ContinuationKind::Active:
  case ContinuationKind::ActiveThenSourceFile:
    return continuation.getPointer();
  }

  llvm_unreachable("Unhandled ContinuationKind in switch.");
}

const ASTScope *ASTScope::getHistoricalContinuation() const {
  switch (continuation.getInt()) {
  case ContinuationKind::Historical:
  case ContinuationKind::Active:
    return continuation.getPointer();

  case ContinuationKind::ActiveThenSourceFile:
    return getSourceFileScope();
  }

  llvm_unreachable("Unhandled ContinuationKind in switch.");
}

void ASTScope::addActiveContinuation(const ASTScope *newContinuation) const {
  assert(newContinuation && "Use 'remove active continuation'");

  // Add a new, active continuation, making sure we're not losing historical
  // information.

  // Simple case: this is the first time this node has had a continuation.
  if (!continuation.getPointer()) {
    continuation.setPointerAndInt(newContinuation, ContinuationKind::Active);
    return;
  }

  // Setting a continuation to itself is a no-op.
  if (continuation.getPointer() == newContinuation) return;

  // Setting a new continuation is only valid when we're replacing a
  // \c SourceFile continuation.
  switch (continuation.getInt()) {
  case ContinuationKind::Active:
    // Only a \c SourceFile continuation can be replaced.
    assert(continuation.getPointer()->getKind() == ASTScopeKind::SourceFile ||
           continuation.getPointer()->getParent()->getKind()
             == ASTScopeKind::TopLevelCode);
    continuation.setPointerAndInt(newContinuation,
                                  ContinuationKind::ActiveThenSourceFile);
    break;

  case ContinuationKind::Historical:
    // Only a \c SourceFile continuation can be replaced.
    assert(continuation.getPointer()->getKind() == ASTScopeKind::SourceFile ||
           continuation.getPointer()->getParent()->getKind()
             == ASTScopeKind::TopLevelCode);
    continuation.setPointerAndInt(newContinuation, ContinuationKind::Active);
    break;

  case ContinuationKind::ActiveThenSourceFile:
    llvm_unreachable("cannot replace a continuation twice");
  }
}

void ASTScope::removeActiveContinuation() const {
  switch (continuation.getInt()) {
  case ContinuationKind::Active:
    continuation.setInt(ContinuationKind::Historical);
    break;

  case ContinuationKind::Historical:
    llvm_unreachable("nothing to remove");
    break;

  case ContinuationKind::ActiveThenSourceFile:
    // Make the \c SourceFile the active continuation.
    continuation.setPointerAndInt(getSourceFileScope(),
                                  ContinuationKind::Active);
    break;
  }
}

void ASTScope::clearActiveContinuation() const {
  switch (continuation.getInt()) {
  case ContinuationKind::Active:
    continuation.setInt(ContinuationKind::Historical);
    break;

  case ContinuationKind::Historical:
    llvm_unreachable("nothing to clear");
    break;

  case ContinuationKind::ActiveThenSourceFile:
    // Make the \c SourceFile the historical continuation.
    continuation.setPointerAndInt(getSourceFileScope(),
                                  ContinuationKind::Historical);
    break;
  }
}

ASTScope::ASTScope(const ASTScope *parent, ArrayRef<ASTScope *> children)
    : ASTScope(ASTScopeKind::Preexpanded, parent) {
  assert(children.size() > 1 && "Don't use this without multiple nodes");

  // Add child nodes, reparenting them to this node.
  storedChildren.reserve(children.size());
  for (auto child : children) {
    child->parentAndExpanded.setPointer(this);
    storedChildren.push_back(child);
  }

  // Note that this node has already been expanded.
  parentAndExpanded.setInt(true);

  // Register the destructor.
  ASTContext &ctx = parent->getASTContext();
  ctx.addDestructorCleanup(storedChildren);

  // Make sure the children were properly sorted.
  assert(std::is_sorted(children.begin(), children.end(),
                        [&](ASTScope *s1, ASTScope *s2) {
    return ctx.SourceMgr.isBeforeInBuffer(s1->getSourceRange().Start,
                                          s2->getSourceRange().Start);
  }));
}

/// Determine whether we should completely skip the given element in a
/// \c BraceStmt.
static bool shouldSkipBraceStmtElement(ASTNode element) {
  if (auto decl = element.dyn_cast<Decl *>())
    return isa<VarDecl>(decl);

  return false;
}

/// Determine whether the given abstract storage declaration has accessors.
static bool hasAccessors(AbstractStorageDecl *asd) {
  return asd->getBracesRange().isValid();
}

void ASTScope::expand() const {
  assert(!isExpanded() && "Already expanded the children of this node");
  ASTContext &ctx = getASTContext();

#ifndef NDEBUG
  auto verificationError = [&]() -> llvm::raw_ostream& {
    return llvm::errs() << "ASTScope verification error in source file '"
      << getSourceFile().getFilename()
      << "': ";
  };
#endif

  // Local function to add a child to the list of children.
  bool previouslyEmpty = storedChildren.empty();
  auto addChild = [&](ASTScope *child) -> bool {
    assert(child->getParent() == this && "Wrong parent");

    // If we have a continuation and the child can steal it, transfer the
    // continuation to that child.
    bool stoleContinuation = false;
    if (getActiveContinuation() && child->canStealContinuation()) {
      assert(!child->getActiveContinuation() &&
             "Child cannot have a continuation already");
      child->continuation = this->continuation;
      this->clearActiveContinuation();
      stoleContinuation = true;
    }

#ifndef NDEBUG
    // Check invariants in asserting builds.
    SourceManager &sourceMgr = ctx.SourceMgr;

    // Check for containment of the child within the parent.
    if (!sourceMgr.rangeContains(getSourceRange(), child->getSourceRange())) {
      auto &out = verificationError() << "child not contained in its parent\n";
      out << "***Child node***\n";
      child->print(out);
      out << "***Parent node***\n";
      this->print(out);
      abort();
    }

    // If there was a previous child, check it's source range.
    if (!storedChildren.empty()) {
      auto prevChild = storedChildren.back();
      SourceRange prevChildRange = prevChild->getSourceRange();
      SourceRange childRange = child->getSourceRange();

      // This new child must come after the previous child.
      if (sourceMgr.isBeforeInBuffer(childRange.Start, prevChildRange.End)) {
        auto &out = verificationError() << "unexpected out-of-order nodes\n";
        out << "***Child node***\n";
        child->print(out);
        out << "***Previous child node***\n";
        prevChild->print(out);
        out << "***Parent node***\n";
        this->print(out);
        abort();
      }

      // The previous child must not overlap this child.
      if (sourceMgr.isBeforeInBuffer(childRange.End, prevChildRange.End)) {
        auto &out = verificationError() << "unexpected child overlap\n";
        out << "***Child node***\n";
        child->print(out);
        out << "***Previous child node***\n";
        prevChild->print(out);
        out << "***Parent node***\n";
        this->print(out);
        abort();
      }
    }
#endif

    // Add the child.
    storedChildren.push_back(child);

    return stoleContinuation;
  };

  // Local function to add the accessors of the variables in the given pattern
  // as children.
  auto addAccessors = [&](Pattern *pattern) {
    // Create children for the accessors of any variables in the pattern that
    // have them.
    pattern->forEachVariable([&](VarDecl *var) {
      if (hasAccessors(var)) {
        addChild(new (ctx) ASTScope(this, var));
      }
    });
  };

  if (!parentAndExpanded.getInt()) {
  // Expand the children in the current scope.
  switch (getKind()) {
  case ASTScopeKind::Preexpanded:
    llvm_unreachable("Node should be pre-expanded");

  case ASTScopeKind::SourceFile: {
    if (!getHistoricalContinuation()) {
      /// Add declarations to the list of children directly.
      for (unsigned i : range(sourceFile.nextElement,
                              sourceFile.file->Decls.size())) {
        Decl *decl = sourceFile.file->Decls[i];

        // If the declaration is a top-level code declaration, turn the source
        // file into a continuation. We're done.
        if (isa<TopLevelCodeDecl>(decl)) {
          addActiveContinuation(this);
          break;
        }

        // Note the next element to be consumed.
        sourceFile.nextElement = i + 1;

        // Create a child node for this declaration.
        if (ASTScope *child = createIfNeeded(this, decl))
          (void)addChild(child);
      }
    }
    break;
  }

  case ASTScopeKind::ExtensionGenericParams: {
    // Create a child node.
    if (ASTScope *child = createIfNeeded(this, extension))
      addChild(child);
    break;
  }

  case ASTScopeKind::TypeOrExtensionBody:
    for (auto member : iterableDeclContext->getMembers()) {
      // Create a child node for this declaration.
      if (ASTScope *child = createIfNeeded(this, member))
        addChild(child);
    }
    break;

  case ASTScopeKind::GenericParams:
    // Create a child of the generic parameters, if needed.
    if (auto child = createIfNeeded(this, genericParams.decl))
      addChild(child);
    break;

  case ASTScopeKind::TypeDecl:
    // Create the child of the function, if any.
    if (auto child = createIfNeeded(this, typeDecl))
      addChild(child);
    break;

  case ASTScopeKind::AbstractFunctionDecl:
    // Create the child of the function, if any.
    if (auto child = createIfNeeded(this, abstractFunction))
      addChild(child);
    break;

  case ASTScopeKind::AbstractFunctionParams:
    // Create a child of the function parameters, which may eventually be
    // the function body.
    if (auto child = createIfNeeded(this, abstractFunctionParams.decl))
      addChild(child);
    break;

  case ASTScopeKind::DefaultArgument:
    // Create a child for the default argument expression.
    if (auto child = createIfNeeded(this, parameter->getDefaultValue()))
      addChild(child);
    break;

  case ASTScopeKind::AbstractFunctionBody:
    // Create a child for the actual body.
    if (auto child = createIfNeeded(this, abstractFunction->getBody()))
      addChild(child);
    break;

  case ASTScopeKind::PatternBinding: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];

    // Create a child for the initializer, if present.
    if (patternEntry.getInitAsWritten() &&
        patternEntry.getInitAsWritten()->getSourceRange().isValid()) {
      addChild(new (ctx) ASTScope(ASTScopeKind::PatternInitializer, this,
                                  patternBinding.decl, patternBinding.entry));
    }

    // If there is an active continuation, nest the remaining pattern bindings.
    if (getActiveContinuation()) {
      // Note: the accessors will follow the pattern binding.
      addChild(new (ctx) ASTScope(ASTScopeKind::AfterPatternBinding, this,
                                  patternBinding.decl, patternBinding.entry));
    } else {
      // Otherwise, add the accessors immediately.
      addAccessors(patternEntry.getPattern());
    }
    break;
  }

  case ASTScopeKind::PatternInitializer: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];

    // Create a child for the initializer expression.
    if (auto child = createIfNeeded(this, patternEntry.getInitAsWritten()))
      addChild(child);
    break;
  }

  case ASTScopeKind::AfterPatternBinding: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];

    // Add accessors for the variables in this pattern.
    addAccessors(patternEntry.getPattern());

    // Create a child for the next pattern binding.
    if (auto child = createIfNeeded(this, patternBinding.decl))
      addChild(child);
    break;
  }

  case ASTScopeKind::BraceStmt:
    // Expanding a brace statement means setting it as its own continuation,
    // unless that's already been done.
    addActiveContinuation(this);
    break;

  case ASTScopeKind::IfStmt:
    // The first conditional clause or, failing that, the 'then' clause.
    if (!ifStmt->getCond().empty()) {
      addChild(new (ctx) ASTScope(this, ifStmt, 0,
                                  /*isGuardContinuation=*/false));
    } else {
      if (auto thenChild = createIfNeeded(this, ifStmt->getThenStmt()))
        addChild(thenChild);
    }

    // Add the 'else' branch, if needed.
    if (auto elseChild = createIfNeeded(this, ifStmt->getElseStmt()))
      addChild(elseChild);

    break;

  case ASTScopeKind::ConditionalClause: {
    // If this is a boolean conditional not in a guard continuation, add a
    // child for the expression.
    if (!conditionalClause.isGuardContinuation) {
      const auto &cond =
        conditionalClause.stmt->getCond()[conditionalClause.index];
      if (auto booleanChild = createIfNeeded(this, cond.getBooleanOrNull()))
        addChild(booleanChild);
    }

    // If there's another conditional clause, add it as the child.
    unsigned nextIndex = conditionalClause.index + 1;
    if (nextIndex < conditionalClause.stmt->getCond().size()) {
      addChild(new (ctx) ASTScope(this, conditionalClause.stmt, nextIndex,
                                  conditionalClause.isGuardContinuation));
      break;
    }

    // There aren't any additional conditional clauses. Add the appropriate
    // nested scope based on the kind of statement.
    if (auto ifStmt = dyn_cast<IfStmt>(conditionalClause.stmt)) {
      if (auto child = createIfNeeded(this, ifStmt->getThenStmt()))
        addChild(child);
    } else if (auto whileStmt = dyn_cast<WhileStmt>(conditionalClause.stmt)) {
      if (auto child = createIfNeeded(this, whileStmt->getBody()))
        addChild(child);
    } else {
      // Note: guard statements have the continuation nested under the last
      // condition.
      assert(isa<GuardStmt>(conditionalClause.stmt) &&
             "unknown labeled conditional statement");
    }
    break;
  }

  case ASTScopeKind::GuardStmt:
    // Add a child to describe the guard condition.
    addChild(new (ctx) ASTScope(this, guard, 0,
                                /*isGuardContinuation=*/false));

    // Add a child for the 'guard' body, which always exits.
    if (auto bodyChild = createIfNeeded(this, guard->getBody()))
      addChild(bodyChild);

    // Add a child to describe the guard condition for the continuation.
    addChild(new (ctx) ASTScope(this, guard, 0,
                                /*isGuardContinuation=*/true));
    break;

  case ASTScopeKind::RepeatWhileStmt:
    // Add a child for the loop body.
    if (auto bodyChild = createIfNeeded(this, repeatWhile->getBody()))
      addChild(bodyChild);

    // Add a child for the loop condition.
    if (auto conditionChild = createIfNeeded(this, repeatWhile->getCond()))
      addChild(conditionChild);

    break;

  case ASTScopeKind::ForEachStmt:
    // Add a child for the sequence.
    if (auto seqChild = createIfNeeded(this, forEach->getSequence()))
      addChild(seqChild);

    // Add a child describing the scope of the pattern.
    addChild(new (ctx) ASTScope(ASTScopeKind::ForEachPattern, this, forEach));
    break;

  case ASTScopeKind::ForEachPattern:
    // Add a child for the 'where' clause.
    if (auto whereChild = createIfNeeded(this, forEach->getWhere()))
      addChild(whereChild);

    // Add a child for the body.
    if (auto bodyChild = createIfNeeded(this, forEach->getBody()))
      addChild(bodyChild);

    break;

  case ASTScopeKind::DoCatchStmt:
    // Add a child for the body.
    if (auto bodyChild = createIfNeeded(this, doCatch->getBody()))
      addChild(bodyChild);

    // Add children for each of the 'catch' clauses.
    for (auto catchClause : doCatch->getCatches()) {
      if (auto catchChild = createIfNeeded(this, catchClause))
        addChild(catchChild);
    }
    break;

  case ASTScopeKind::CatchStmt:
    // Add a child for the guard expression, if there is one.
    if (auto guardChild = createIfNeeded(this, catchStmt->getGuardExpr()))
      addChild(guardChild);

    // Add a child for the catch body.
    if (auto bodyChild = createIfNeeded(this, catchStmt->getBody()))
      addChild(bodyChild);

    break;

  case ASTScopeKind::SwitchStmt:
    // Add a child for the subject expression.
    if (auto subjectChild = createIfNeeded(this, switchStmt->getSubjectExpr()))
      addChild(subjectChild);

    // Add children for each of the cases.
    for (auto caseStmt : switchStmt->getCases()) {
      if (auto caseChild = createIfNeeded(this, caseStmt))
        addChild(caseChild);
    }
    break;

  case ASTScopeKind::CaseStmt:
    // Add children for the items.
    for (auto &caseItem : caseStmt->getMutableCaseLabelItems()) {
      if (auto guardChild = createIfNeeded(this, caseItem.getGuardExpr()))
        addChild(guardChild);
    }

    // Add a child for the case body.
    if (auto bodyChild = createIfNeeded(this, caseStmt->getBody()))
      addChild(bodyChild);
    break;

  case ASTScopeKind::Accessors: {
    // Add children for all of the explicitly-written accessors.
    for (auto accessor : abstractStorageDecl->getAllAccessors()) {
      if (accessor->isImplicit() || accessor->getStartLoc().isInvalid())
        continue;
      if (auto accessorChild = createIfNeeded(this, accessor))
        addChild(accessorChild);
    }
    break;
  }

  case ASTScopeKind::Closure:
    // Add the child for a body.
    if (auto bodyChild = createIfNeeded(this, closure->getBody()))
      addChild(bodyChild);
    break;

  case ASTScopeKind::TopLevelCode:
    /// Add a child for the body.
    if (auto bodyChild = createIfNeeded(this, topLevelCode->getBody()))
      addChild(bodyChild);
    break;
  }
  }

  // Enumerate any continuation scopes associated with this parent.
  enumerateContinuationScopes(addChild);

  // If this is the first time we've added children, notify the ASTContext
  // that there's a SmallVector that needs to be cleaned up.
  // FIXME: If we had access to SmallVector::isSmall(), we could do better.
  if (previouslyEmpty && !storedChildren.empty())
    getASTContext().addDestructorCleanup(storedChildren);

  // The scope is considered "expanded" at this point, although there might be
  // further work to do if there is an active continuation.
  if (getKind() != ASTScopeKind::SourceFile || getHistoricalContinuation())
    parentAndExpanded.setInt(true);
}

bool ASTScope::isExpanded() const {
  // If the 'expanded' bit is not set, we haven't expanded.
  if (!parentAndExpanded.getInt()) return false;

  // If there is an active continuation, we're expanded if it's not the
  // source-file continuation or if we're at the end of the list of
  // declarations.
  if (auto continuation = getActiveContinuation()) {
    return continuation->getKind() != ASTScopeKind::SourceFile ||
           (continuation->sourceFile.nextElement
              == continuation->sourceFile.file->Decls.size());
  }

  // If it's a source file that has never been a continuation, check whether
  // we're at the last declaration.
  return getKind() != ASTScopeKind::SourceFile ||
         ((sourceFile.nextElement == sourceFile.file->Decls.size()) &&
          !getHistoricalContinuation());
}

/// Create the AST scope for a source file, which is the root of the scope
/// tree.
ASTScope *ASTScope::createRoot(SourceFile *sourceFile) {
  ASTContext &ctx = sourceFile->getASTContext();

  // Create the scope.
  ASTScope *scope = new (ctx) ASTScope(sourceFile, 0);
  scope->sourceFile.file = sourceFile;
  scope->sourceFile.nextElement = 0;

  return scope;
}

// FIXME: This is a vestige of multiple parameter lists.
static ParamDecl *getParameter(AbstractFunctionDecl *func,
                               unsigned listIndex,
                               unsigned paramIndex) {
  // Dig out the actual parameter.
  if (auto *selfDecl = func->getImplicitSelfDecl()) {
    if (listIndex == 0) {
      assert(paramIndex == 0);
      return selfDecl;
    }

    assert(listIndex == 1);
    return func->getParameters()->get(paramIndex);
  }

  assert(listIndex == 0);
  return func->getParameters()->get(paramIndex);
}

/// Find the parameter list and parameter index (into that list) corresponding
/// to the next parameter.
static Optional<std::pair<unsigned, unsigned>>
findNextParameter(AbstractFunctionDecl *func, unsigned listIndex,
                  unsigned paramIndex) {
  unsigned paramOffset = 1;

  if (func->hasImplicitSelfDecl()) {
    if (listIndex > 1)
      return None;

    if (listIndex == 0) {
      ++listIndex;
      paramIndex = 0;
      paramOffset = 0;
    }

  } else {
    if (listIndex > 0)
      return None;
  }

  if (paramIndex + paramOffset < func->getParameters()->size())
    return std::make_pair(listIndex, paramIndex + paramOffset);

  ++listIndex;
  return None;
}

/// Determine whether the given parent is the accessor node for an abstract
/// storage declaration or is directly descended from it.
static bool parentDirectDescendedFromAbstractStorageDecl(
              const ASTScope *parent,
              const AbstractStorageDecl *decl) {
  while (true) {
    switch (parent->getKind()) {
    case ASTScopeKind::Preexpanded:
    case ASTScopeKind::AbstractFunctionDecl:
    case ASTScopeKind::AbstractFunctionParams:
    case ASTScopeKind::GenericParams:
      // Keep looking.
      parent = parent->getParent();
      continue;

    case ASTScopeKind::Accessors:
      return (parent->getAbstractStorageDecl() == decl);

    case ASTScopeKind::SourceFile:
    case ASTScopeKind::TypeDecl:
    case ASTScopeKind::ExtensionGenericParams:
    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::DefaultArgument:
    case ASTScopeKind::AbstractFunctionBody:
    case ASTScopeKind::PatternBinding:
    case ASTScopeKind::PatternInitializer:
    case ASTScopeKind::AfterPatternBinding:
    case ASTScopeKind::BraceStmt:
    case ASTScopeKind::ConditionalClause:
    case ASTScopeKind::IfStmt:
    case ASTScopeKind::GuardStmt:
    case ASTScopeKind::RepeatWhileStmt:
    case ASTScopeKind::ForEachStmt:
    case ASTScopeKind::ForEachPattern:
    case ASTScopeKind::DoCatchStmt:
    case ASTScopeKind::CatchStmt:
    case ASTScopeKind::SwitchStmt:
    case ASTScopeKind::CaseStmt:
    case ASTScopeKind::Closure:
    case ASTScopeKind::TopLevelCode:
      // Not a direct descendant.
      return false;
    }
  }
}

/// Determine whether the given parent is the node for a specific abstract
/// function declaration or is directly descended from it.
static bool parentDirectDescendedFromAbstractFunctionDecl(
              const ASTScope *parent,
              const AbstractFunctionDecl *decl) {
  while (true) {
    switch (parent->getKind()) {
    case ASTScopeKind::Preexpanded:
    case ASTScopeKind::AbstractFunctionParams:
    case ASTScopeKind::DefaultArgument:
    case ASTScopeKind::AbstractFunctionBody:
    case ASTScopeKind::GenericParams:
      // Keep looking.
      parent = parent->getParent();
      continue;

    case ASTScopeKind::AbstractFunctionDecl:
      return (parent->getAbstractFunctionDecl() == decl);

    case ASTScopeKind::SourceFile:
    case ASTScopeKind::TypeDecl:
    case ASTScopeKind::ExtensionGenericParams:
    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::PatternBinding:
    case ASTScopeKind::PatternInitializer:
    case ASTScopeKind::AfterPatternBinding:
    case ASTScopeKind::Accessors:
    case ASTScopeKind::BraceStmt:
    case ASTScopeKind::ConditionalClause:
    case ASTScopeKind::IfStmt:
    case ASTScopeKind::GuardStmt:
    case ASTScopeKind::RepeatWhileStmt:
    case ASTScopeKind::ForEachStmt:
    case ASTScopeKind::ForEachPattern:
    case ASTScopeKind::DoCatchStmt:
    case ASTScopeKind::CatchStmt:
    case ASTScopeKind::SwitchStmt:
    case ASTScopeKind::CaseStmt:
    case ASTScopeKind::Closure:
    case ASTScopeKind::TopLevelCode:
      // Not a direct descendant.
      return false;
    }
  }
}

/// Determine whether the given parent is the node for a specific type
/// declaration or is directly descended from it.
static bool parentDirectDescendedFromTypeDecl(const ASTScope *parent,
                                              const TypeDecl *decl) {
  while (true) {
    switch (parent->getKind()) {
    case ASTScopeKind::Preexpanded:
    case ASTScopeKind::GenericParams:
      // Keep looking.
      parent = parent->getParent();
      continue;

    case ASTScopeKind::TypeDecl:
      return (parent->getTypeDecl() == decl);

    case ASTScopeKind::SourceFile:
    case ASTScopeKind::AbstractFunctionDecl:
    case ASTScopeKind::AbstractFunctionParams:
    case ASTScopeKind::DefaultArgument:
    case ASTScopeKind::AbstractFunctionBody:
    case ASTScopeKind::ExtensionGenericParams:
    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::PatternBinding:
    case ASTScopeKind::PatternInitializer:
    case ASTScopeKind::AfterPatternBinding:
    case ASTScopeKind::Accessors:
    case ASTScopeKind::BraceStmt:
    case ASTScopeKind::ConditionalClause:
    case ASTScopeKind::IfStmt:
    case ASTScopeKind::GuardStmt:
    case ASTScopeKind::RepeatWhileStmt:
    case ASTScopeKind::ForEachStmt:
    case ASTScopeKind::ForEachPattern:
    case ASTScopeKind::DoCatchStmt:
    case ASTScopeKind::CatchStmt:
    case ASTScopeKind::SwitchStmt:
    case ASTScopeKind::CaseStmt:
    case ASTScopeKind::Closure:
    case ASTScopeKind::TopLevelCode:
      // Not a direct descendant.
      return false;
    }
  }
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, Decl *decl) {
  if (!decl) return nullptr;

  // Implicit declarations don't have source information for name lookup.
  if (decl->isImplicit()) return nullptr;

  // Accessors are always nested within their abstract storage declaration.
  if (auto accessor = dyn_cast<AccessorDecl>(decl)) {
    if (!parentDirectDescendedFromAbstractStorageDecl(
            parent, accessor->getStorage()))
      return nullptr;
  }

  ASTContext &ctx = decl->getASTContext();

  // If this is a type declaration for which we have not introduced a TypeDecl
  // scope, add it now.
  if (auto typeDecl = dyn_cast<TypeDecl>(decl)) {
    if (!parentDirectDescendedFromTypeDecl(parent, typeDecl)) {
      return new (ctx) ASTScope(parent, typeDecl);
    }
  }

  // If this is a function declaration for which we have not introduced
  // an AbstractFunctionDecl scope, add it now.
  if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    if (!parentDirectDescendedFromAbstractFunctionDecl(parent, func)) {
      return new (ctx) ASTScope(ASTScopeKind::AbstractFunctionDecl, parent,
                                func);
    }
  }

  // Local function to handle generic parameters.
  auto nextGenericParam =
      [&](GenericParamList *genericParams, Decl *decl) -> ASTScope * {
    if (!genericParams) return nullptr;

    unsigned index = (parent->getKind() == ASTScopeKind::GenericParams &&
                      parent->genericParams.decl == decl)
                        ? parent->genericParams.index + 1
                        : 0;
    if (index < genericParams->size())
      return new (ctx) ASTScope(parent, genericParams, decl, index);

    return nullptr;
  };

  // Create the inner scope.
  switch (decl->getKind()) {
  case DeclKind::Import:
  case DeclKind::EnumCase:
  case DeclKind::PrecedenceGroup:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Module:
  case DeclKind::Param:
  case DeclKind::EnumElement:
  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
  case DeclKind::MissingMember:
    // These declarations do not introduce scopes.
    return nullptr;

  case DeclKind::Var:
    // Always handled by a pattern-binding declaration.
    return nullptr;

  case DeclKind::Extension: {
    auto ext = cast<ExtensionDecl>(decl);

    // If we already have a scope of the (possible) generic parameters,
    // add the body.
    if (parent->getKind() == ASTScopeKind::ExtensionGenericParams)
      return new (ctx) ASTScope(parent, cast<IterableDeclContext>(ext));

    // Otherwise, form the extension's generic parameters scope.
    return new (ctx) ASTScope(parent, ext);
  }

  case DeclKind::TopLevelCode:
    return new (ctx) ASTScope(parent, cast<TopLevelCodeDecl>(decl));

  case DeclKind::Protocol:
    cast<ProtocolDecl>(decl)->createGenericParamsIfMissing();
    LLVM_FALLTHROUGH;

  case DeclKind::Class:
  case DeclKind::Enum:
  case DeclKind::Struct: {
    auto nominal = cast<NominalTypeDecl>(decl);

    // If we have a generic type and our parent isn't describing our generic
    // parameters, build the generic parameter scope.
    if (auto scope = nextGenericParam(nominal->getGenericParams(), nominal))
      return scope;

    return new (ctx) ASTScope(parent, cast<IterableDeclContext>(nominal));
  }

  case DeclKind::TypeAlias: {
    // If we have a generic typealias and our parent isn't describing our
    // generic parameters, build the generic parameter scope.
    auto typeAlias = cast<TypeAliasDecl>(decl);
    if (auto scope = nextGenericParam(typeAlias->getGenericParams(), typeAlias))
      return scope;

    // Typealiases don't introduce any other scopes.
    return nullptr;
  }

  case DeclKind::Func:
  case DeclKind::Accessor:
  case DeclKind::Constructor:
  case DeclKind::Destructor: {
    auto abstractFunction = cast<AbstractFunctionDecl>(decl);

    // If we have a generic function and our parent isn't describing our generic
    // parameters or function parameters, build the generic parameter scope.
    if (parent->getKind() != ASTScopeKind::AbstractFunctionParams ||
        parent->abstractFunctionParams.decl != decl) {
      if (auto scope = nextGenericParam(abstractFunction->getGenericParams(),
                                        abstractFunction))
        return scope;
    }

    // Figure out which parameter is next is the next one down.
    Optional<std::pair<unsigned, unsigned>> nextParameter = None;
    if (parent->getKind() == ASTScopeKind::AbstractFunctionParams &&
        parent->abstractFunctionParams.decl == decl) {
      nextParameter =
        findNextParameter(parent->abstractFunctionParams.decl,
                          parent->abstractFunctionParams.listIndex,
                          parent->abstractFunctionParams.paramIndex);

    } else if (abstractFunction->hasImplicitSelfDecl() ||
               abstractFunction->getParameters()->size() > 0) {
      nextParameter = std::make_pair(0, 0);
    }

    if (nextParameter) {
      auto *currentParam = getParameter(abstractFunction,
                                        nextParameter->first,
                                        nextParameter->second);

      // Determine whether there is a default argument.
      ASTScope *defaultArgumentScope = nullptr;
      if (currentParam->getDefaultValue())
        defaultArgumentScope = new (ctx) ASTScope(parent, currentParam);

      // If there is another parameter to visit, do so now.
      ASTScope *afterParamScope = new (ctx) ASTScope(parent, abstractFunction,
                                                     nextParameter->first,
                                                     nextParameter->second);

      // If we have a default argument, use a pre-expanded node.
      if (defaultArgumentScope) {
        ASTScope *children[2] = { defaultArgumentScope, afterParamScope };
        return new (ctx) ASTScope(parent, children);
      }

      return afterParamScope;
    }

    // Function body, if present.
    if (abstractFunction->hasBody())
      return new (ctx) ASTScope(ASTScopeKind::AbstractFunctionBody, parent,
                                abstractFunction);
    return nullptr;
  }

  case DeclKind::PatternBinding: {
    auto patternBinding = cast<PatternBindingDecl>(decl);

    // When the parent has an active continuation, bindings nest.
    if (parent->getActiveContinuation()) {
      // Find the next pattern binding.
      unsigned entry = (parent->getKind() == ASTScopeKind::AfterPatternBinding&&
                        parent->patternBinding.decl == decl)
                          ? parent->patternBinding.entry + 1
                          : 0;
      if (entry < patternBinding->getPatternList().size())
        return new (ctx) ASTScope(ASTScopeKind::PatternBinding, parent,
                                  patternBinding, entry);

      return nullptr;
    }

    // Elsewhere, explode out the bindings because they're independent.

    // Handle a single binding directly.
    if (patternBinding->getNumPatternEntries() == 1)
      return new (ctx) ASTScope(ASTScopeKind::PatternBinding, parent,
                                patternBinding, 0);


    // Pre-expand when there are multiple bindings.
    SmallVector<ASTScope *, 4> bindings;
    for (auto entry : range(patternBinding->getNumPatternEntries())) {
      bindings.push_back(new (ctx) ASTScope(ASTScopeKind::PatternBinding,
                                            parent, patternBinding, entry));
    }

    return new (ctx) ASTScope(parent, bindings);
  }

  case DeclKind::Subscript: {
    auto asd = cast<AbstractStorageDecl>(decl);
    if (hasAccessors(asd))
      return new (ctx) ASTScope(parent, asd);
    return nullptr;
  }
  }

  llvm_unreachable("Unhandled DeclKind in switch.");
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, Stmt *stmt) {
  if (!stmt) return nullptr;

  ASTContext &ctx = parent->getASTContext();
  switch (stmt->getKind()) {
  case StmtKind::Brace:
    if (stmt->getSourceRange().isInvalid()) return nullptr;
    return new (ctx) ASTScope(parent, cast<BraceStmt>(stmt));

  case StmtKind::Return: {
    auto returnStmt = cast<ReturnStmt>(stmt);
    if (!returnStmt->hasResult()) return nullptr;

    return createIfNeeded(parent, returnStmt->getResult());
  }

  case StmtKind::Defer:
    return createIfNeeded(parent, cast<DeferStmt>(stmt)->getTempDecl());

  case StmtKind::If:
    return new (ctx) ASTScope(parent, cast<IfStmt>(stmt));

  case StmtKind::Guard:
    return new (ctx) ASTScope(parent, cast<GuardStmt>(stmt));

  case StmtKind::While: {
    // If there are no conditions, just create the body.
    auto whileStmt = cast<WhileStmt>(stmt);
    if (whileStmt->getCond().empty())
      return createIfNeeded(parent, whileStmt->getBody());

    // Create a node for the first conditional clause.
    return new (ctx) ASTScope(parent, whileStmt, 0,
                              /*isGuardContinuation=*/false);
  }

  case StmtKind::RepeatWhile:
    return new (ctx) ASTScope(parent, cast<RepeatWhileStmt>(stmt));

  case StmtKind::ForEach:
    return new (ctx) ASTScope(ASTScopeKind::ForEachStmt, parent,
                              cast<ForEachStmt>(stmt));

  case StmtKind::Do:
    return createIfNeeded(parent, cast<DoStmt>(stmt)->getBody());

  case StmtKind::DoCatch:
    return new (ctx) ASTScope(parent, cast<DoCatchStmt>(stmt));

  case StmtKind::Catch:
    return new (ctx) ASTScope(parent, cast<CatchStmt>(stmt));

  case StmtKind::Switch:
    return new (ctx) ASTScope(parent, cast<SwitchStmt>(stmt));

  case StmtKind::Case:
    return new (ctx) ASTScope(parent, cast<CaseStmt>(stmt));

  case StmtKind::Break:
  case StmtKind::Continue:
  case StmtKind::Fallthrough:
  case StmtKind::Fail:
  case StmtKind::Throw:
    // Nothing to do for these statements.
    return nullptr;
  }

  llvm_unreachable("Unhandled StmtKind in switch.");
}

/// Find all of the (non-nested) closures referenced within this expression.
static void findClosures(Expr *expr, SmallVectorImpl<ClosureExpr *> &closures) {
  if (!expr) return;

  /// AST walker that finds top-level closures in an expression.
  class ClosureFinder : public ASTWalker {
    SmallVectorImpl<ClosureExpr *> &closures;

  public:
    ClosureFinder(SmallVectorImpl<ClosureExpr *> &closures) : closures(closures) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (auto closure = dyn_cast<ClosureExpr>(E)) {
        closures.push_back(closure);
        return { false, E };
      }

      return { true, E };
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }

    std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
      return { false, P };
    }

    bool walkToDeclPre(Decl *D) override { return false; }

    bool walkToTypeLocPre(TypeLoc &TL) override { return false; }

    bool walkToTypeReprPre(TypeRepr *T) override { return false; }

    bool walkToParameterListPre(ParameterList *PL) override {
      return false;
    }
  };

  expr->walk(ClosureFinder(closures));
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, Expr *expr) {
  if (!expr) return nullptr;
  return createIfNeeded(parent, llvm::makeArrayRef(expr));
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent,
                                   ArrayRef<Expr *> exprs) {
  SmallVector<ClosureExpr*, 4> closures;

  // Dig out closure expressions within the given expressions.
  for (auto expr: exprs)
    findClosures(expr, closures);
  if (closures.empty())
    return nullptr;

  ASTContext &ctx = parent->getASTContext();
  if (closures.size() == 1)
    return new (ctx) ASTScope(parent, closures[0]);

  // Create the closure scopes for each of the closures.
  SmallVector<ASTScope *, 4> closureScopes;
  for (auto closure : closures)
    closureScopes.push_back(new (ctx) ASTScope(parent, closure));

  return new (ctx) ASTScope(parent, closureScopes);
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, ASTNode node) {
  if (auto decl = node.dyn_cast<Decl *>())
    return createIfNeeded(parent, decl);
  if (auto stmt = node.dyn_cast<Stmt *>())
    return createIfNeeded(parent, stmt);
  return createIfNeeded(parent, node.get<Expr *>());
}

bool ASTScope::canStealContinuation() const {
  switch (getKind()) {
  case ASTScopeKind::Preexpanded:
  case ASTScopeKind::SourceFile:
  case ASTScopeKind::ExtensionGenericParams:
  case ASTScopeKind::TypeOrExtensionBody:
  case ASTScopeKind::GenericParams:
  case ASTScopeKind::AbstractFunctionParams:
  case ASTScopeKind::DefaultArgument:
  case ASTScopeKind::AbstractFunctionBody:
  case ASTScopeKind::PatternInitializer:
  case ASTScopeKind::Accessors:
  case ASTScopeKind::IfStmt:
  case ASTScopeKind::RepeatWhileStmt:
  case ASTScopeKind::ForEachStmt:
  case ASTScopeKind::ForEachPattern:
  case ASTScopeKind::DoCatchStmt:
  case ASTScopeKind::CatchStmt:
  case ASTScopeKind::SwitchStmt:
  case ASTScopeKind::CaseStmt:
  case ASTScopeKind::Closure:
  case ASTScopeKind::TypeDecl:
  case ASTScopeKind::AbstractFunctionDecl:
    // These node kinds don't introduce names that would be visible in a
    // continuation.
    return false;

  case ASTScopeKind::TopLevelCode:
    // Top-level code can steal the continuation from the source file.
    return true;

  case ASTScopeKind::BraceStmt:
    // Brace statements that describe top-level code can steal the continuation
    // from the source file.
    return getParent()->getKind() == ASTScopeKind::TopLevelCode;

  case ASTScopeKind::AfterPatternBinding:
  case ASTScopeKind::PatternBinding:
    // Declarations always steal continuations.
    return true;

  case ASTScopeKind::GuardStmt:
    // Guard statements steal on behalf of their children. How noble.
    return true;

  case ASTScopeKind::ConditionalClause:
    // Guard conditions steal continuations.
    return conditionalClause.isGuardContinuation;
  }

  llvm_unreachable("Unhandled ASTScopeKind in switch.");
}

void ASTScope::enumerateContinuationScopes(
       llvm::function_ref<bool(ASTScope *)> addChild) const {
  while (auto continuation = getActiveContinuation()) {
    // Continue to brace statements.
    if (continuation->getKind() == ASTScopeKind::BraceStmt) {
      // Find the next suitable child in the brace statement.
      auto continuationElements = continuation->braceStmt.stmt->getElements();
      for (unsigned i : range(continuation->braceStmt.nextElement,
                              continuationElements.size())) {
        continuation->braceStmt.nextElement = i + 1;

        // Skip this element if it's useless.
        if (shouldSkipBraceStmtElement(continuationElements[i])) continue;

        // Try to create this child.
        if (auto child = createIfNeeded(this, continuationElements[i])) {
          // Add this child.
          if (addChild(child)) return;
        }
      }

      // We've exhausted this continuation; remove it.
      removeActiveContinuation();
      continue;
    }

    // Continue within a source file containing top-level code.
    if (continuation->getKind() == ASTScopeKind::SourceFile) {
      auto continuationDecls
        = llvm::makeArrayRef(continuation->sourceFile.file->Decls);
      for (unsigned i : range(continuation->sourceFile.nextElement,
                              continuationDecls.size())) {
        // Note the next element to be consumed.
        continuation->sourceFile.nextElement = i + 1;

        Decl *decl = continuation->sourceFile.file->Decls[i];

        // Try to create this child.
        if (auto child = createIfNeeded(this, decl)) {
          // Add this child.
          if (addChild(child)) return;
        }
      }

      // The source file is never truly exhausted; just return.
      return;
    }

    llvm_unreachable("Unhandled continuation scope");
  }
}

ASTContext &ASTScope::getASTContext() const {
  switch (kind) {
  case ASTScopeKind::SourceFile:
    return sourceFile.file->getASTContext();

  case ASTScopeKind::TypeDecl:
    return typeDecl->getASTContext();

  case ASTScopeKind::ExtensionGenericParams:
    return extension->getASTContext();

  case ASTScopeKind::TypeOrExtensionBody:
    return getParent()->getASTContext();

  case ASTScopeKind::GenericParams:
    return genericParams.decl->getASTContext();

  case ASTScopeKind::AbstractFunctionDecl:
  case ASTScopeKind::AbstractFunctionBody:
    return abstractFunction->getASTContext();

  case ASTScopeKind::AbstractFunctionParams:
    return abstractFunctionParams.decl->getASTContext();

  case ASTScopeKind::DefaultArgument:
      return parameter->getASTContext();

  case ASTScopeKind::PatternBinding:
  case ASTScopeKind::PatternInitializer:
  case ASTScopeKind::AfterPatternBinding:
    return patternBinding.decl->getASTContext();

  case ASTScopeKind::Preexpanded:
  case ASTScopeKind::BraceStmt:
  case ASTScopeKind::IfStmt:
  case ASTScopeKind::ConditionalClause:
  case ASTScopeKind::GuardStmt:
  case ASTScopeKind::RepeatWhileStmt:
  case ASTScopeKind::ForEachStmt:
  case ASTScopeKind::ForEachPattern:
  case ASTScopeKind::DoCatchStmt:
  case ASTScopeKind::CatchStmt:
  case ASTScopeKind::SwitchStmt:
  case ASTScopeKind::CaseStmt:
  case ASTScopeKind::Closure:
    return getParent()->getASTContext();

  case ASTScopeKind::Accessors:
    return abstractStorageDecl->getASTContext();

  case ASTScopeKind::TopLevelCode:
    return static_cast<Decl *>(topLevelCode)->getASTContext();
  }

  llvm_unreachable("Unhandled ASTScopeKind in switch.");
}

const ASTScope *ASTScope::getSourceFileScope() const {
  auto result = this;
  while (result->getKind() != ASTScopeKind::SourceFile)
    result = result->getParent();

  return result;
}

SourceFile &ASTScope::getSourceFile() const {
  return *getSourceFileScope()->sourceFile.file;
}

SourceRange ASTScope::getSourceRangeImpl() const {
  switch (kind) {
  case ASTScopeKind::Preexpanded:
    return SourceRange(children().front()->getSourceRange().Start,
                       children().back()->getSourceRange().End);

  case ASTScopeKind::SourceFile:
    if (auto bufferID = sourceFile.file->getBufferID()) {
      auto charRange = getASTContext().SourceMgr.getRangeForBuffer(*bufferID);
      return SourceRange(charRange.getStart(), charRange.getEnd());
    }

    if (sourceFile.file->Decls.empty()) return SourceRange();

    // Use the source ranges of the declarations in the file.
    return SourceRange(sourceFile.file->Decls.front()->getStartLoc(),
                       sourceFile.file->Decls.back()->getEndLoc());

  case ASTScopeKind::TypeDecl:
    return typeDecl->getSourceRange();

  case ASTScopeKind::ExtensionGenericParams: {
    // The generic parameters of an extension are available from the ':' of
    // the inheritance clause (if available), or else that from the
    // 'where' (if present) or from the start of the body.
    // FIXME: Approximating the ':' with the start of the first inherited entry.
    SourceLoc startLoc;
    if (!extension->getInherited().empty() &&
        extension->getInherited().front().getSourceRange().Start.isValid())
      startLoc = extension->getInherited().front().getSourceRange().Start;
    if (auto trailingWhere = extension->getTrailingWhereClause())
      startLoc = trailingWhere->getWhereLoc();
    else
      startLoc = extension->getBraces().Start;

    return SourceRange(startLoc, extension->getEndLoc());
  }
      
  case ASTScopeKind::TypeOrExtensionBody:
    if (auto ext = dyn_cast<ExtensionDecl>(iterableDeclContext))
      return ext->getBraces();

    return cast<NominalTypeDecl>(iterableDeclContext)->getBraces();

  case ASTScopeKind::GenericParams:
    // A protocol's generic parameter list is not written in source, and
    // is visible from the start of the body.
    if (auto *protoDecl = dyn_cast<ProtocolDecl>(genericParams.decl)) {
      return SourceRange(protoDecl->getBraces().Start,
                         protoDecl->getEndLoc());
    }

    // Explicitly-written generic parameters are in scope following their
    // definition.
    return SourceRange(genericParams.params->getParams()[genericParams.index]
                         ->getEndLoc(),
                       genericParams.decl->getEndLoc());

  case ASTScopeKind::AbstractFunctionDecl: {
    // For an accessor, all of the parameters are implicit, so start them at
    // the start location of the accessor.
    if (isa<AccessorDecl>(abstractFunction))
      return SourceRange(abstractFunction->getLoc(),
                         abstractFunction->getEndLoc());

    return abstractFunction->getSourceRange();
  }

  case ASTScopeKind::AbstractFunctionParams: {
    SourceLoc endLoc = abstractFunctionParams.decl->getEndLoc();

    // For an accessor, all of the parameters are implicit, so start them at
    // the start location of the accessor.
    if (isa<AccessorDecl>(abstractFunctionParams.decl))
      return SourceRange(abstractFunctionParams.decl->getLoc(), endLoc);

    // For the 'self' parameter of a member function, use the start of the
    // first parameter list... or the 'deinit' keyword for deinitializers.
    // FIXME: Why oh why don't deinitializers have a parameter list?
    if (abstractFunctionParams.listIndex == 0 &&
        abstractFunctionParams.decl->getDeclContext()->isTypeContext()) {
      SourceLoc startLoc;
      if (isa<DestructorDecl>(abstractFunctionParams.decl)) {
        startLoc = abstractFunctionParams.decl->getNameLoc();
      } else {
        startLoc = abstractFunctionParams.decl->getParameters()
                     ->getLParenLoc();
      }
      return SourceRange(startLoc, endLoc);
    }

    // Otherwise, find the end of this parameter.
    auto param = abstractFunctionParams.decl->getParameters()
                     ->get(abstractFunctionParams.paramIndex);
    return SourceRange(param->getEndLoc(), endLoc);
  }

  case ASTScopeKind::DefaultArgument:
    return parameter->getDefaultValue()->getSourceRange();

  case ASTScopeKind::AbstractFunctionBody:
    return abstractFunction->getBodySourceRange();

  case ASTScopeKind::PatternBinding: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];

    return patternEntry.getSourceRange();
  }

  case ASTScopeKind::PatternInitializer: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];

    return patternEntry.getInitAsWritten()->getSourceRange();
  }

  case ASTScopeKind::AfterPatternBinding: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];

    return SourceRange(patternEntry.getSourceRange(/*omitAccessors*/true).End,
                       patternEntry.getSourceRange().End);
  }

  case ASTScopeKind::BraceStmt:
    // The brace statements that represent closures start their scope at the
    // 'in' keyword, when present.
    if (getParent()->getKind() == ASTScopeKind::Closure &&
        getParent()->closure->getInLoc().isValid())
      return SourceRange(getParent()->closure->getInLoc(),
                         braceStmt.stmt->getEndLoc());

    return braceStmt.stmt->getSourceRange();

  case ASTScopeKind::IfStmt:
    return ifStmt->getSourceRange();

  case ASTScopeKind::ConditionalClause: {
    // For a guard continuation, the scope extends from the end of the 'else'
    // to the end of the continuation.
    if (conditionalClause.isGuardContinuation) {
      const ASTScope *guard = this;
      do {
        guard = guard->getParent();
      } while (guard->getKind() != ASTScopeKind::GuardStmt);


      return SourceRange(guard->guard->getBody()->getEndLoc());
    }

    // Determine the start location, which is either the beginning of the next
    // conditional or something statement-specific.
    auto conditionals = conditionalClause.stmt->getCond();
    unsigned nextIndex = conditionalClause.index + 1;
    SourceLoc startLoc;
    if (conditionals[conditionalClause.index].getKind()
          == StmtConditionElement::CK_PatternBinding &&
        nextIndex < conditionals.size()) {
      startLoc = conditionals[nextIndex].getStartLoc();
    } else if (conditionals[conditionalClause.index].getKind()
                 != StmtConditionElement::CK_PatternBinding) {
      startLoc = conditionals[conditionalClause.index].getStartLoc();
    }

    // For 'guard' statements, the conditional clause.
    if (auto guard = dyn_cast<GuardStmt>(conditionalClause.stmt)) {
      // If we didn't have a condition clause to start the new scope, use the
      // end of the guard statement itself.
      if (startLoc.isInvalid())
        startLoc = guard->getBody()->getStartLoc();

      return SourceRange(startLoc, guard->getBody()->getStartLoc());
    }

    // For 'if' statements, the conditional clause covers the 'then' branch.
    if (auto ifStmt = dyn_cast<IfStmt>(conditionalClause.stmt)) {
      // If we didn't have a conditional clause to start the new scope, use
      // the beginning of the 'then' clause.
      if (startLoc.isInvalid())
        startLoc = ifStmt->getThenStmt()->getStartLoc();

      return SourceRange(startLoc, ifStmt->getThenStmt()->getEndLoc());
    }

    // For 'while' statements, the conditional clause covers the body.
    auto whileStmt = cast<WhileStmt>(conditionalClause.stmt);
    // If we didn't have a conditional clause to start the new scope, use
    // the beginning of the body.
    if (startLoc.isInvalid())
      startLoc = whileStmt->getBody()->getStartLoc();
    return SourceRange(startLoc, whileStmt->getBody()->getEndLoc());
  }

  case ASTScopeKind::GuardStmt:
    return guard->getSourceRange();

  case ASTScopeKind::RepeatWhileStmt:
    return repeatWhile->getSourceRange();

  case ASTScopeKind::ForEachStmt:
    return forEach->getSourceRange();

  case ASTScopeKind::ForEachPattern:
    // The scope of the pattern extends from the 'where' expression (if present)
    // until the end of the body.
    if (forEach->getWhere())
      return SourceRange(forEach->getWhere()->getStartLoc(),
                         forEach->getBody()->getEndLoc());

    // Otherwise, scope of the pattern covers the body.
    return forEach->getBody()->getSourceRange();

  case ASTScopeKind::DoCatchStmt:
    return doCatch->getSourceRange();

  case ASTScopeKind::CatchStmt:
    // The scope of the pattern extends from the 'where' (if present)
    // to the end of the body.
    if (catchStmt->getGuardExpr())
      return SourceRange(catchStmt->getWhereLoc(),
                         catchStmt->getBody()->getEndLoc());

    // Otherwise, the scope of the pattern encompasses the body.
    return catchStmt->getBody()->getSourceRange();

  case ASTScopeKind::SwitchStmt:
    return switchStmt->getSourceRange();

  case ASTScopeKind::CaseStmt:
    // The scope of the case statement begins at the first guard expression,
    // if there is one, and extends to the end of the body.
    // FIXME: Figure out what to do about multiple pattern bindings. We might
    // want a more restrictive rule in those cases.
    for (const auto &caseItem : caseStmt->getCaseLabelItems()) {
      if (auto guardExpr = caseItem.getGuardExpr())
        return SourceRange(guardExpr->getStartLoc(),
                           caseStmt->getBody()->getEndLoc());
    }

    // Otherwise, it covers the body.
    return caseStmt->getBody()->getSourceRange();

  case ASTScopeKind::Accessors:
    return abstractStorageDecl->getBracesRange();

  case ASTScopeKind::Closure:
    if (closure->getInLoc().isValid())
      return SourceRange(closure->getInLoc(), closure->getEndLoc());

    return closure->getSourceRange();

  case ASTScopeKind::TopLevelCode:
    return topLevelCode->getSourceRange();
  }

  llvm_unreachable("Unhandled ASTScopeKind in switch.");
}

/// Find the innermost enclosing scope that contains this source location.
const ASTScope *ASTScope::findInnermostEnclosingScope(SourceLoc loc) const {
  ASTContext &ctx = getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  // Search up the tree to find the nearest parent that contains this source
  // location.
  const ASTScope *searchNode = this;
  while (!sourceMgr.rangeContainsTokenLoc(searchNode->getSourceRange(), loc))
    searchNode = searchNode->getParent();

  while (true) {
    // Expand the children of the search node.
    if (!searchNode->isExpanded()) searchNode->expand();

    // Use binary search to find the child that contains this location.
    struct CompareLocs {
      SourceManager &sourceMgr;

      bool operator()(const ASTScope *scope, SourceLoc loc) {
        return sourceMgr.isBeforeInBuffer(scope->getSourceRange().End, loc);
      }

      bool operator()(SourceLoc loc, const ASTScope *scope) {
        return sourceMgr.isBeforeInBuffer(loc, scope->getSourceRange().End);
      }
    };
    auto child = std::lower_bound(searchNode->children().begin(),
                                  searchNode->children().end(),
                                  loc, CompareLocs { sourceMgr });

    // If we found a child whose source range encloses the given location,
    // continue with that child.
    if (child != searchNode->children().end() &&
        sourceMgr.rangeContainsTokenLoc((*child)->getSourceRange(), loc)) {
      searchNode = *child;
      continue;
    }

    // Otherwise, our current search node is the best we could find.
    assert(sourceMgr.rangeContainsTokenLoc(searchNode->getSourceRange(), loc));
    return searchNode;
  };
}

DeclContext *ASTScope::getDeclContext() const {
  switch (getKind()) {
  case ASTScopeKind::SourceFile:
    return sourceFile.file;

  case ASTScopeKind::TypeDecl:
    if (auto typeAlias = dyn_cast<TypeAliasDecl>(typeDecl))
      return typeAlias;

    return nullptr;

  case ASTScopeKind::TypeOrExtensionBody:
    if (auto nominal = dyn_cast<NominalTypeDecl>(iterableDeclContext))
      return nominal;

    return cast<ExtensionDecl>(iterableDeclContext);

  case ASTScopeKind::AbstractFunctionDecl:
    return abstractFunction;

  case ASTScopeKind::DefaultArgument:
    return parameter->getDefaultArgumentInitContext();

  case ASTScopeKind::PatternInitializer:
    return patternBinding.decl->getPatternList()[patternBinding.entry]
             .getInitContext();

  case ASTScopeKind::Closure:
    return closure;

  case ASTScopeKind::Accessors:
    // FIXME: Somewhat odd modeling because Subscripts don't have their
    // own nodes. Maybe they should.
    if (auto subscript = dyn_cast<SubscriptDecl>(abstractStorageDecl))
      return subscript;

    return nullptr;

  case ASTScopeKind::TopLevelCode:
    return topLevelCode;

  case ASTScopeKind::ExtensionGenericParams:
  case ASTScopeKind::GenericParams:
  case ASTScopeKind::AbstractFunctionParams:
  case ASTScopeKind::PatternBinding:
  case ASTScopeKind::AfterPatternBinding:
  case ASTScopeKind::Preexpanded:
  case ASTScopeKind::BraceStmt:
  case ASTScopeKind::IfStmt:
  case ASTScopeKind::ConditionalClause:
  case ASTScopeKind::GuardStmt:
  case ASTScopeKind::RepeatWhileStmt:
  case ASTScopeKind::ForEachStmt:
  case ASTScopeKind::ForEachPattern:
  case ASTScopeKind::DoCatchStmt:
  case ASTScopeKind::CatchStmt:
  case ASTScopeKind::SwitchStmt:
  case ASTScopeKind::CaseStmt:
  case ASTScopeKind::AbstractFunctionBody:
    return nullptr;
  }

  llvm_unreachable("Unhandled ASTScopeKind in switch.");
}

DeclContext *ASTScope::getInnermostEnclosingDeclContext() const {
  for (const ASTScope *scope = this; ; scope = scope->getParent()) {
    if (auto dc = scope->getDeclContext()) return dc;
  }
  llvm_unreachable("Top-most scope is a declaration context");
}

SmallVector<ValueDecl *, 4> ASTScope::getLocalBindings() const {
  SmallVector<ValueDecl *, 4> result;

  auto handlePattern = [&](const Pattern *pattern) {
    if (!pattern) return;
    pattern->forEachVariable([&](VarDecl *var) {
        result.push_back(var);
      });
  };

  switch (getKind()) {
  case ASTScopeKind::Preexpanded:
  case ASTScopeKind::SourceFile:
  case ASTScopeKind::AbstractFunctionDecl:
  case ASTScopeKind::TypeDecl:
  case ASTScopeKind::TypeOrExtensionBody:
  case ASTScopeKind::DefaultArgument:
  case ASTScopeKind::AbstractFunctionBody:
  case ASTScopeKind::PatternBinding:
  case ASTScopeKind::IfStmt:
  case ASTScopeKind::GuardStmt:
  case ASTScopeKind::RepeatWhileStmt:
  case ASTScopeKind::ForEachStmt:
  case ASTScopeKind::DoCatchStmt:
  case ASTScopeKind::SwitchStmt:
  case ASTScopeKind::Accessors:
  case ASTScopeKind::TopLevelCode:
    // No local declarations.
    break;

  case ASTScopeKind::ExtensionGenericParams: {
    // If the source range containing the extension parameters is empty,
    // do nothing.
    SourceRange range = getSourceRangeImpl();
    if (range.Start == range.End)
      break;

    // Bind this extension, if we haven't done so already.
    if (!extension->getExtendedType())
      if (auto resolver = extension->getASTContext().getLazyResolver())
        resolver->bindExtension(extension);

    // If there are generic parameters, add them.
    for (auto genericParams = extension->getGenericParams();
         genericParams;
         genericParams = genericParams->getOuterParameters()) {
      for (auto param : genericParams->getParams())
        result.push_back(param);
    }
    break;
  }

  case ASTScopeKind::GenericParams:
    result.push_back(genericParams.params->getParams()[genericParams.index]);
    break;

  case ASTScopeKind::AbstractFunctionParams:
    result.push_back(
      getParameter(abstractFunctionParams.decl,
                   abstractFunctionParams.listIndex,
                   abstractFunctionParams.paramIndex));
    break;

  case ASTScopeKind::AfterPatternBinding:
    handlePattern(patternBinding.decl->getPattern(patternBinding.entry));
    break;

  case ASTScopeKind::ConditionalClause:
    handlePattern(conditionalClause.stmt->getCond()[conditionalClause.index]
                    .getPatternOrNull());
    break;

  case ASTScopeKind::BraceStmt:
    // All types and functions are visible anywhere within their brace
    // statements. It's up to capture analysis to determine what is usable.
    for (auto element : braceStmt.stmt->getElements()) {
      if (auto decl = element.dyn_cast<Decl *>()) {
        if (isa<AbstractFunctionDecl>(decl) || isa<TypeDecl>(decl))
          result.push_back(cast<ValueDecl>(decl));
      }
    }
    break;

  case ASTScopeKind::ForEachPattern:
    handlePattern(forEach->getPattern());
    break;

  case ASTScopeKind::CatchStmt:
    handlePattern(catchStmt->getErrorPattern());
    break;

  case ASTScopeKind::CaseStmt:
    for (const auto &item : caseStmt->getCaseLabelItems())
      handlePattern(item.getPattern());
    break;

  case ASTScopeKind::PatternInitializer: {
    // 'self' is available within the pattern initializer of a 'lazy' variable.
    auto *initContext = cast_or_null<PatternBindingInitializer>(
      patternBinding.decl->getPatternList()[0].getInitContext());
    if (initContext) {
      if (auto *selfParam = initContext->getImplicitSelfDecl())
        result.push_back(selfParam);
    }

    break;
  }

  case ASTScopeKind::Closure:
    // Note: Parameters all at once is different from functions, but it's not
    // relevant because there are no default arguments.
    for (auto param : *closure->getParameters())
      result.push_back(param);
    break;
  }

  return result;
}

void ASTScope::expandAll() const {
  if (!isExpanded())
    expand();

  for (auto child : children())
    child->expandAll();
}

void ASTScope::print(llvm::raw_ostream &out, unsigned level,
                     bool lastChild, bool printChildren) const {
  SourceManager &sourceMgr = getASTContext().SourceMgr;

  // Indent for levels 2+.
  if (level > 1) out.indent((level-1) * 2);

  // Print child marker and leading '-' for levels 1+.
  if (level > 0) {
    out << (lastChild ? '`' : '|') << '-';
  }

  // Local function to print the scope kind
  auto printScopeKind = [&](StringRef name) {
    out << name;
  };

  // Print the address of the node.
  auto printAddress = [&](const void *address) {
    out << " " << address;
  };

  // Print the source location of the node.
  auto printRange = [&]() {
    auto range = getSourceRange();
    if (range.isInvalid()) {
      out << " [invalid source range]";
      return;
    }

    auto startLineAndCol = sourceMgr.getLineAndColumn(range.Start);
    auto endLineAndCol = sourceMgr.getLineAndColumn(range.End);

    out << " [" << startLineAndCol.first << ":" << startLineAndCol.second
        << " - " << endLineAndCol.first << ":" << endLineAndCol.second << "]";
  };

  // Print the scope kind and any salient information.
  switch (kind) {
  case ASTScopeKind::Preexpanded:
    printScopeKind("Preexpanded");
    printAddress(this);
    printRange();
    break;

  case ASTScopeKind::SourceFile:
    printScopeKind("SourceFile");
    printAddress(sourceFile.file);
    out << " '" << sourceFile.file->getFilename() << "'";
    printRange();
    break;

  case ASTScopeKind::TypeDecl:
    printScopeKind("TypeDecl");
    printAddress(typeDecl);
    out << " " << typeDecl->getFullName();
    printRange();
    break;

  case ASTScopeKind::ExtensionGenericParams:
    printScopeKind("ExtensionGenericParams");
    printAddress(extension);
    out << " extension of '";
    if (auto typeRepr = extension->getExtendedTypeLoc().getTypeRepr())
      typeRepr->print(out);
    else
      extension->getExtendedType()->print(out);
    out << "'";
    printRange();
    break;

  case ASTScopeKind::TypeOrExtensionBody: {
    printScopeKind("TypeOrExtensionBody");
    if (auto ext = dyn_cast<ExtensionDecl>(iterableDeclContext)) {
      printAddress(ext);
      out << " extension of '";
      if (auto typeRepr = ext->getExtendedTypeLoc().getTypeRepr())
        typeRepr->print(out);
      else
        ext->getExtendedType()->print(out);
      out << "'";
      printRange();
    } else {
      auto nominal = cast<NominalTypeDecl>(iterableDeclContext);
      printAddress(nominal);
      out << " '" << nominal->getName() << "'";
      printRange();
    }
    break;
  }

  case ASTScopeKind::GenericParams:
    printScopeKind("GenericParams");
    printAddress(genericParams.params);
    out << " param " << genericParams.index;
    printRange();
    break;

  case ASTScopeKind::AbstractFunctionDecl:
    printScopeKind("AbstractFunctionDecl");
    printAddress(abstractFunction);
    out << " " << abstractFunction->getFullName();
    printRange();
    break;

  case ASTScopeKind::AbstractFunctionParams:
    printScopeKind("AbstractFunctionParams");
    printAddress(abstractFunctionParams.decl);
    out << " " << abstractFunctionParams.decl->getFullName()
        << " param " << abstractFunctionParams.listIndex << ":"
        << abstractFunctionParams.paramIndex;
    printRange();
    break;

  case ASTScopeKind::DefaultArgument:
    printScopeKind("DefaultArgument");
    printAddress(parameter);
    printRange();
    break;

  case ASTScopeKind::AbstractFunctionBody:
    printScopeKind("AbstractFunctionBody");
    printAddress(abstractFunction);
    out << " " << abstractFunction->getFullName();
    printRange();
    break;

  case ASTScopeKind::PatternBinding:
    printScopeKind("PatternBinding");
    printAddress(patternBinding.decl);
    out << " entry " << patternBinding.entry;
    printRange();
    break;

  case ASTScopeKind::PatternInitializer:
    printScopeKind("PatternInitializer");
    printAddress(patternBinding.decl);
    out << " entry " << patternBinding.entry;
    printRange();
    break;

  case ASTScopeKind::AfterPatternBinding:
    printScopeKind("AfterPatternBinding");
    printAddress(patternBinding.decl);
    out << " entry " << patternBinding.entry;
    printRange();
    break;

  case ASTScopeKind::BraceStmt:
    printScopeKind("BraceStmt");
    printAddress(braceStmt.stmt);
    printRange();
    break;

  case ASTScopeKind::IfStmt:
    printScopeKind("IfStmt");
    printAddress(ifStmt);
    printRange();
    break;

  case ASTScopeKind::ConditionalClause:
    printScopeKind("ConditionalClause");
    printAddress(conditionalClause.stmt);
    out << " index " << conditionalClause.index;
    if (conditionalClause.isGuardContinuation)
      out << " guard-continuation";
    printRange();
    break;

  case ASTScopeKind::GuardStmt:
    printScopeKind("GuardStmt");
    printAddress(guard);
    printRange();
    break;

  case ASTScopeKind::RepeatWhileStmt:
    printScopeKind("RepeatWhileStmt");
    printAddress(repeatWhile);
    printRange();
    break;

  case ASTScopeKind::ForEachStmt:
    printScopeKind("ForEachStmt");
    printAddress(forEach);
    printRange();
    break;

  case ASTScopeKind::ForEachPattern:
    printScopeKind("ForEachPattern");
    printAddress(forEach);
    printRange();
    break;

  case ASTScopeKind::DoCatchStmt:
    printScopeKind("DoCatchStmt");
    printAddress(doCatch);
    printRange();
    break;

  case ASTScopeKind::CatchStmt:
    printScopeKind("CatchStmt");
    printAddress(catchStmt);
    printRange();
    break;

  case ASTScopeKind::SwitchStmt:
    printScopeKind("SwitchStmt");
    printAddress(switchStmt);
    printRange();
    break;

  case ASTScopeKind::CaseStmt:
    printScopeKind("CaseStmt");
    printAddress(caseStmt);
    printRange();
    break;

  case ASTScopeKind::Accessors:
    printScopeKind("Accessors");
    printAddress(abstractStorageDecl);
    out << " ";
    abstractStorageDecl->dumpRef(out);
    printRange();
    break;

  case ASTScopeKind::Closure:
    printScopeKind("Closure");
    printAddress(closure);
    printRange();
    break;

  case ASTScopeKind::TopLevelCode:
    printScopeKind("TopLevelCode");
    printAddress(topLevelCode);
    printRange();
    break;
  }

  // Was this scope expanded?
  out << (isExpanded() ? " expanded" : " unexpanded");

  out << "\n";

  if (printChildren) {
    // Print the children. In some cases, we can be "unexpanded" but still have
    // children.
    for (unsigned i : indices(storedChildren)) {
      storedChildren[i]->print(out, level + 1,
                               /*lastChild=*/i == storedChildren.size()-1);
    }
  }
}

void ASTScope::dump() const {
  print(llvm::errs(), 0, false);
}

void *ASTScope::operator new(size_t bytes, const ASTContext &ctx,
                             unsigned alignment) {
  return ctx.Allocate(bytes, alignment);
}

