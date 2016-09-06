//===--- ASTScope.cpp - Swift AST Scope -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/AST/Initializer.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/STLExtras.h"
#include <algorithm>
using namespace swift;

ASTScope::ASTScope(const ASTScope *parent, ArrayRef<ASTScope *> children)
    : ASTScope(ASTScopeKind::Preexpanded, parent) {
  assert(children.size() > 1 && "Don't use this without multiple nodes");

  // Add child nodes, reparenting them to this node.
  storedChildren.reserve(children.size());
  for (auto child : children ) {
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

/// Determines whether the given AST node introduces a continuation.
static bool introducesContinuation(ASTNode element) {
  if (auto decl = element.dyn_cast<Decl *>()) {
    // Declarations in local contexts introduce continuations.
    return decl->getDeclContext()->isLocalContext();
  }

  if (auto stmt = element.dyn_cast<Stmt *>()) {
    // Guard statements introduce continuations.
    if (isa<GuardStmt>(stmt)) return true;

    return false;
  }

  return false;
}

/// Determine whether the given abstract storage declaration has accessors.
static bool hasAccessors(AbstractStorageDecl *asd) {
  switch (asd->getStorageKind()) {
  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithObservers:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::Computed:
  case AbstractStorageDecl::ComputedWithMutableAddress:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::StoredWithObservers:
    return true;

  case AbstractStorageDecl::Stored:
  case AbstractStorageDecl::StoredWithTrivialAccessors:
    return false;
  }
}

void ASTScope::expand() const {
  assert(!isExpanded() && "Already expanded the children of this node");
  ASTContext &ctx = getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

#ifndef NDEBUG
  auto verificationError = [&]() -> llvm::raw_ostream& {
    return llvm::errs() << "ASTScope verification error in source file '"
      << getSourceFile().getFilename()
      << "': ";
  };
#endif

  // Local function to add a child to the list of children.
  bool previouslyEmpty = storedChildren.empty();
  auto addChild = [&](ASTScope *child) {
    assert(child->getParent() == this && "Wrong parent");

#ifndef NDEBUG
    // Check invariants in asserting builds.

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
  };

  // Expand the children in the current scope.
  switch (kind) {
  case ASTScopeKind::Preexpanded:
    llvm_unreachable("Node should be pre-expanded");

  case ASTScopeKind::SourceFile: {
    /// Add all of the new declarations to the list of children.
    for (Decl *decl : llvm::makeArrayRef(sourceFile.file->Decls)
                        .slice(sourceFile.nextElement)) {
      // Create a child node for this declaration.
      if (ASTScope *child = createIfNeeded(this, decl))
        addChild(child);
    }

    // Make sure we don't visit these children again.
    sourceFile.nextElement = sourceFile.file->Decls.size();
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

  case ASTScopeKind::PatternBinding: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];

    // Create a child for the initializer, if present.
    if (patternEntry.getInit() &&
        patternEntry.getInit()->getSourceRange().isValid())
      addChild(new (ctx) ASTScope(ASTScopeKind::PatternInitializer, this,
                                  patternBinding.decl, patternBinding.entry));

    // Create children for the accessors of any variables in the pattern that
    // have them.
    patternEntry.getPattern()->forEachVariable([&](VarDecl *var) {
      if (hasAccessors(var))
        addChild(new (ctx) ASTScope(this, var));
    });

    // If the pattern binding is in a local context, we nest the remaining
    // pattern bindings.
    if (patternBinding.decl->getDeclContext()->isLocalContext()) {
      addChild(new (ctx) ASTScope(ASTScopeKind::AfterPatternBinding, this,
                                  patternBinding.decl, patternBinding.entry));
    }
    break;
  }

  case ASTScopeKind::PatternInitializer:
    // Create a child for the initializer expression.
    if (auto child =
            createIfNeeded(this,
                           patternBinding.decl->getInit(patternBinding.entry)))
      addChild(child);
    break;

  case ASTScopeKind::AfterPatternBinding: {
    // Create a child for the next pattern binding.
    if (auto child = createIfNeeded(this, patternBinding.decl))
      addChild(child);
    break;
  }

  case ASTScopeKind::BraceStmt: {
    // Find the first element that requires a scope.
    auto elements = braceStmt.stmt->getElements();
    for (unsigned i : range(braceStmt.nextElement, elements.size())) {
      braceStmt.nextElement = i + 1;

      // Skip this brace element if it's unnecessary.
      if (shouldSkipBraceStmtElement(elements[i])) continue;

      // Try to create the child. If it succeeds, we're done.
      if (auto child = createIfNeeded(this, elements[i])) {
        addChild(child);

        // If this element introduces a continuation, it's expansion will
        // handle the remainder of the brace statement.
        if (introducesContinuation(elements[i])) break;
      }
    }
    break;
  }

  case ASTScopeKind::LocalDeclaration:
    // Add the contents of the declaration itself.
    if (auto child = createIfNeeded(this, localDeclaration))
      addChild(child);
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

  case ASTScopeKind::ForStmt:
    // The for statement encloses the scope introduced by its initializers.
    addChild(new (ctx) ASTScope(ASTScopeKind::ForStmtInitializer,
                                this, forStmt));
    break;

  case ASTScopeKind::ForStmtInitializer:
    // Add a child for the condition, if present.
    if (auto cond = forStmt->getCond()) {
      if (auto condChild = createIfNeeded(this, cond.get()))
        addChild(condChild);
    }

    // Add a child for the increment, if present.
    if (auto incr = forStmt->getIncrement()) {
      if (auto incrChild = createIfNeeded(this, incr.get()))
        addChild(incrChild);
    }

    // Add a child for the body.
    if (auto bodyChild = createIfNeeded(this, forStmt->getBody()))
      addChild(bodyChild);
    break;

  case ASTScopeKind::Accessors: {
    // Add children for all of the the explicitly-written accessors.
    SmallVector<ASTScope *, 4> accessors;
    auto addAccessor = [&](FuncDecl *accessor) {
      if (!accessor) return;
      if (accessor->isImplicit()) return;
      if (accessor->getStartLoc().isInvalid()) return;

      if (auto accessorChild = createIfNeeded(this, accessor))
        accessors.push_back(accessorChild);
    };

    addAccessor(abstractStorageDecl->getGetter());
    addAccessor(abstractStorageDecl->getSetter());
    addAccessor(abstractStorageDecl->getMaterializeForSetFunc());
    if (abstractStorageDecl->hasAddressors()) {
      addAccessor(abstractStorageDecl->getAddressor());
      addAccessor(abstractStorageDecl->getMutableAddressor());
    }
    if (abstractStorageDecl->hasObservers()) {
      addAccessor(abstractStorageDecl->getDidSetFunc());
      addAccessor(abstractStorageDecl->getWillSetFunc());
    }

    // Sort the accessors, because they can come in any order.
    std::sort(accessors.begin(), accessors.end(),
      [&](ASTScope *s1, ASTScope *s2) {
        return ctx.SourceMgr.isBeforeInBuffer(s1->getSourceRange().Start,
                                              s2->getSourceRange().Start);
    });

    // Add the accessors.
    for (auto accessor : accessors)
      addChild(accessor);

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

  // Enumerate any continuation scopes associated with this parent.
  enumerateContinuationScopes(addChild);

  // If this is the first time we've added children, notify the ASTContext
  // that there's a SmallVector that needs to be cleaned up.
  // FIXME: If we had access to SmallVector::isSmall(), we could do better.
  if (previouslyEmpty && !storedChildren.empty())
    getASTContext().addDestructorCleanup(storedChildren);

  // Anything but a SourceFile is considered "expanded" at this point; source
  // files can grow due to the REPL.
  if (kind != ASTScopeKind::SourceFile)
    parentAndExpanded.setInt(true);
}

bool ASTScope::isExpanded() const {
  // If the 'expanded' bit is set, we've expanded already.
  if (parentAndExpanded.getInt()) return true;

  // Source files are expanded when there are no new declarations to process.
  if (kind == ASTScopeKind::SourceFile &&
      sourceFile.nextElement == sourceFile.file->Decls.size())
    return true;

  return false;
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

/// Find the parameter list and parameter index (into that list) corresponding
/// to the next parameter.
static Optional<std::pair<unsigned, unsigned>>
findNextParameter(AbstractFunctionDecl *func, unsigned listIndex,
                  unsigned paramIndex) {
  auto paramLists = func->getParameterLists();
  unsigned paramOffset = 1;
  while (listIndex < paramLists.size()) {
    auto currentList = paramLists[listIndex];

    // If there is a parameter in this list, return it.
    if (paramIndex + paramOffset < currentList->size()) {
      return std::make_pair(listIndex, paramIndex + paramOffset);
    }

    // Move on to the next list.
    ++listIndex;
    paramIndex = 0;
    paramOffset = 0;
  }

  return None;
}

/// Determine whether the given parent is a local declaration or is
/// directly descended from one.
static bool parentDirectDescendedFromLocalDeclaration(const ASTScope *parent,
                                                      const Decl *decl) {
  while (true) {
    switch (parent->getKind()) {
    case ASTScopeKind::Preexpanded:
    case ASTScopeKind::AbstractFunctionDecl:
    case ASTScopeKind::AbstractFunctionParams:
    case ASTScopeKind::GenericParams:
    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::Accessors:
      // Keep looking.
      parent = parent->getParent();
      continue;

    case ASTScopeKind::LocalDeclaration:
      return (parent->getLocalDeclaration() == decl);

    case ASTScopeKind::SourceFile:
    case ASTScopeKind::DefaultArgument:
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
    case ASTScopeKind::ForStmt:
    case ASTScopeKind::ForStmtInitializer:
    case ASTScopeKind::Closure:
    case ASTScopeKind::TopLevelCode:
      // Not a direct descendant.
      return false;
    }
  }
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
    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::DefaultArgument:
    case ASTScopeKind::LocalDeclaration:
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
    case ASTScopeKind::ForStmt:
    case ASTScopeKind::ForStmtInitializer:
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
    case ASTScopeKind::GenericParams:
      // Keep looking.
      parent = parent->getParent();
      continue;

    case ASTScopeKind::AbstractFunctionDecl:
      return (parent->getAbstractFunctionDecl() == decl);

    case ASTScopeKind::SourceFile:
    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::LocalDeclaration:
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
    case ASTScopeKind::ForStmt:
    case ASTScopeKind::ForStmtInitializer:
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
  bool isAccessor = false;
  if (auto func = dyn_cast<FuncDecl>(decl)) {
    if (func->isAccessor()) {
      isAccessor = true;
      if (!parentDirectDescendedFromAbstractStorageDecl(
             parent, func->getAccessorStorageDecl()))
        return nullptr;
    }
  }

  // If this is a local declaration for which we have not yet produced a
  // local declaration scope, introduce that local declaration scope now.
  //
  // Note that pattern bindings are handled as a special case, because they
  // potentially introduce several levels of bindings.
  ASTContext &ctx = decl->getASTContext();
  bool inLocalContext = decl->getDeclContext()->isLocalContext();
  if (!isa<PatternBindingDecl>(decl) && !isa<AbstractStorageDecl>(decl) &&
      inLocalContext && !isAccessor &&
      !parentDirectDescendedFromLocalDeclaration(parent, decl)) {
    auto scope = new (ctx) ASTScope(ASTScopeKind::LocalDeclaration,
                                    parent);
    scope->localDeclaration = decl;
    return scope;
  }

  // If this is a function declaration for which we have not introduced
  // an AbstractFunctionDecl scope, add it now.
  if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    if (!parentDirectDescendedFromAbstractFunctionDecl(parent, func)) {
      return new (ctx) ASTScope(parent, func);
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
    // These declarations do not introduce scopes.
    return nullptr;

  case DeclKind::Var:
    // Always handled by a pattern-binding declaration.
    return nullptr;

  case DeclKind::Extension:
    return new (ctx) ASTScope(parent, cast<ExtensionDecl>(decl));

  case DeclKind::TopLevelCode: {
    // Drop top-level statements containing just an IfConfigStmt.
    // FIXME: The modeling of IfConfig is weird.
    auto topLevelCode = cast<TopLevelCodeDecl>(decl);
    auto braceStmt = topLevelCode->getBody();
    auto elements = braceStmt->getElements();
    if (elements.size() == 1 &&
        elements[0].is<Stmt *>() &&
        isa<IfConfigStmt>(elements[0].get<Stmt *>()))
      return nullptr;

    return new (ctx) ASTScope(parent, topLevelCode);
  }

  case DeclKind::Class:
  case DeclKind::Enum:
  case DeclKind::Struct: {
    auto nominal = cast<NominalTypeDecl>(decl);

    // If we have a generic type and our parent isn't describing our generic
    // parameters, build the generic parameter scope.
    if (auto scope = nextGenericParam(nominal->getGenericParams(), nominal))
      return scope;

    return new (ctx) ASTScope(parent, nominal);
  }

  case DeclKind::Protocol:
    return new (ctx) ASTScope(parent, cast<ProtocolDecl>(decl));

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
    Optional<std::pair<unsigned, unsigned>> nextParameter;
    if (parent->getKind() == ASTScopeKind::AbstractFunctionParams &&
        parent->abstractFunctionParams.decl == decl) {
      nextParameter =
        findNextParameter(parent->abstractFunctionParams.decl,
                          parent->abstractFunctionParams.listIndex,
                          parent->abstractFunctionParams.paramIndex);

    } else if (abstractFunction->getParameterList(0)->size() > 0) {
      nextParameter = std::make_pair(0, 0);
    } else {
      nextParameter = findNextParameter(abstractFunction, 0, 0);
    }

    if (nextParameter) {
      // Dig out the actual parameter.
      ParamDecl *currentParam =
        abstractFunction->getParameterList(nextParameter->first)
          ->get(nextParameter->second);

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
    return createIfNeeded(parent, abstractFunction->getBody());
  }

  case DeclKind::PatternBinding: {
    auto patternBinding = cast<PatternBindingDecl>(decl);

    // Within a local context, bindings nest.
    if (inLocalContext) {
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

  case StmtKind::For:
    return new (ctx) ASTScope(ASTScopeKind::ForStmt, parent,
                              cast<ForStmt>(stmt));

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
  case StmtKind::IfConfig:
  case StmtKind::Fail:
  case StmtKind::Throw:
    // Nothing to do for these statements.
    return nullptr;
  }
}

/// Find all of the (non-nested) closures referenced within this expression.
static SmallVector<ClosureExpr *, 4> findClosures(Expr *expr) {
  SmallVector<ClosureExpr *, 4> closures;
  if (!expr) return closures;

  /// AST walker that finds top-level closures in an expression.
  class ClosureFinder : public ASTWalker {
    SmallVectorImpl<ClosureExpr *> &closures;

  public:
    ClosureFinder(SmallVectorImpl<ClosureExpr *> &closures) : closures(closures) { }

    virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (auto closure = dyn_cast<ClosureExpr>(E)) {
        closures.push_back(closure);
        return { false, E };
      }

      return { true, E };
    }

    virtual std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }

    virtual std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
      return { false, P };
    }

    virtual bool walkToDeclPre(Decl *D) override { return false; }

    virtual bool walkToTypeLocPre(TypeLoc &TL) override { return false; }

    virtual bool walkToTypeReprPre(TypeRepr *T) override { return false; }

    virtual bool walkToParameterListPre(ParameterList *PL) override {
      return false;
    }
  };

  expr->walk(ClosureFinder(closures));
  return closures;
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, Expr *expr) {
  if (!expr) return nullptr;

  // Dig out closure expressions within the given expression.
  auto closures = findClosures(expr);
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

bool ASTScope::isContinuationScope() const {
  switch (getKind()) {
  case ASTScopeKind::Preexpanded:
  case ASTScopeKind::SourceFile:
  case ASTScopeKind::TypeOrExtensionBody:
  case ASTScopeKind::GenericParams:
  case ASTScopeKind::AbstractFunctionDecl:
  case ASTScopeKind::AbstractFunctionParams:
  case ASTScopeKind::DefaultArgument:
  case ASTScopeKind::PatternBinding:
  case ASTScopeKind::PatternInitializer:
  case ASTScopeKind::Accessors:
  case ASTScopeKind::BraceStmt:
  case ASTScopeKind::IfStmt:
  case ASTScopeKind::GuardStmt:
  case ASTScopeKind::RepeatWhileStmt:
  case ASTScopeKind::ForEachStmt:
  case ASTScopeKind::ForEachPattern:
  case ASTScopeKind::DoCatchStmt:
  case ASTScopeKind::CatchStmt:
  case ASTScopeKind::SwitchStmt:
  case ASTScopeKind::CaseStmt:
  case ASTScopeKind::ForStmt:
  case ASTScopeKind::ForStmtInitializer:
  case ASTScopeKind::Closure:
  case ASTScopeKind::TopLevelCode:
    // These node kinds never have a viable continuation.
    return false;

  case ASTScopeKind::AfterPatternBinding:
    // The last parameter binding in a local scope can have a continuation.
    return patternBinding.decl->getDeclContext()->isLocalContext() &&
      patternBinding.entry == patternBinding.decl->getNumPatternEntries() - 1;

  case ASTScopeKind::LocalDeclaration:
    // Local declarations can always have a continuation.
    return true;

  case ASTScopeKind::ConditionalClause:
    // The last conditional clause of a 'guard' statement can have a
    // continuation if it is marked accordingly.
    return conditionalClause.isGuardContinuation &&
      conditionalClause.index + 1 == conditionalClause.stmt->getCond().size();
  }
}

void ASTScope::enumerateContinuationScopes(
       llvm::function_ref<void(ASTScope *)> fn) const {
  // Only consider scopes that can be continuation scopes.
  if (!isContinuationScope()) return;

  // Look for the nearest enclosing BraceStmt; that's where we continue from.
  const ASTScope *continuation = getParent();
  while (true) {
    switch (continuation->getKind()) {
    case ASTScopeKind::Preexpanded:
    case ASTScopeKind::SourceFile:
    case ASTScopeKind::DefaultArgument:
    case ASTScopeKind::PatternInitializer:
    case ASTScopeKind::IfStmt:
    case ASTScopeKind::RepeatWhileStmt:
    case ASTScopeKind::ForEachStmt:
    case ASTScopeKind::ForEachPattern:
    case ASTScopeKind::DoCatchStmt:
    case ASTScopeKind::CatchStmt:
    case ASTScopeKind::SwitchStmt:
    case ASTScopeKind::CaseStmt:
    case ASTScopeKind::ForStmt:
    case ASTScopeKind::ForStmtInitializer:
    case ASTScopeKind::Closure:
    case ASTScopeKind::TopLevelCode:
      // These scopes are hard barriers; if we hit one, there is nothing to
      // continue to.
      return;

    case ASTScopeKind::Accessors:
      // Non-local variables/subscripts are hard barriers; there's nothing
      // to continue to.
      if (!abstractStorageDecl->getDeclContext()->isLocalContext())
        return;

      SWIFT_FALLTHROUGH;

    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::AbstractFunctionDecl:
    case ASTScopeKind::AbstractFunctionParams:
    case ASTScopeKind::GenericParams:
    case ASTScopeKind::PatternBinding:
    case ASTScopeKind::AfterPatternBinding:
    case ASTScopeKind::LocalDeclaration:
    case ASTScopeKind::ConditionalClause:
    case ASTScopeKind::GuardStmt:
      // Continue looking for the continuation parent.
      continuation = continuation->getParent();
      continue;

    case ASTScopeKind::BraceStmt:
      // Found it. We're done.
      break;
    }

    break;
  }
  assert(continuation->getKind() == ASTScopeKind::BraceStmt);

  // Find the next suitable child in the brace statement.
  auto continuationElements = continuation->braceStmt.stmt->getElements();
  for (unsigned i : range(continuation->braceStmt.nextElement,
                          continuationElements.size())) {
    continuation->braceStmt.nextElement = i + 1;

    // Skip this element if it's useless.
    if (shouldSkipBraceStmtElement(continuationElements[i])) continue;

    // Try to create this child.
    if (auto child = createIfNeeded(this, continuationElements[i])) {
      // List this child.
      fn(child);

      // If this child introduces a continuation itself, we're done.
      if (introducesContinuation(continuationElements[i])) return;
    }
  }
}

ASTContext &ASTScope::getASTContext() const {
  switch (kind) {
  case ASTScopeKind::SourceFile:
    return sourceFile.file->getASTContext();

  case ASTScopeKind::TypeOrExtensionBody:
    return getParent()->getASTContext();

  case ASTScopeKind::GenericParams:
    return genericParams.decl->getASTContext();

  case ASTScopeKind::AbstractFunctionDecl:
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
  case ASTScopeKind::ForStmt:
  case ASTScopeKind::ForStmtInitializer:
  case ASTScopeKind::Closure:
    return getParent()->getASTContext();

  case ASTScopeKind::LocalDeclaration:
    return localDeclaration->getASTContext();

  case ASTScopeKind::Accessors:
    return abstractStorageDecl->getASTContext();

  case ASTScopeKind::TopLevelCode:
    return static_cast<Decl *>(topLevelCode)->getASTContext();
  }
}

SourceFile &ASTScope::getSourceFile() const {
  if (kind == ASTScopeKind::SourceFile)
    return *sourceFile.file;

  return getParent()->getSourceFile();
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

    return SourceRange();

  case ASTScopeKind::TypeOrExtensionBody:
    if (auto ext = dyn_cast<ExtensionDecl>(iterableDeclContext))
      return ext->getBraces();

    return cast<NominalTypeDecl>(iterableDeclContext)->getBraces();

  case ASTScopeKind::GenericParams:
    return SourceRange(genericParams.params->getParams()[genericParams.index]
                         ->getEndLoc(),
                       genericParams.decl->getEndLoc());

  case ASTScopeKind::AbstractFunctionDecl: {
    // For an accessor, all of the parameters are implicit, so start them at
    // the start location of the accessor.
    if (isa<FuncDecl>(abstractFunction) &&
        cast<FuncDecl>(abstractFunction)->isAccessor())
      return SourceRange(abstractFunction->getLoc(),
                         abstractFunction->getEndLoc());

    return abstractFunction->getSourceRange();
  }

  case ASTScopeKind::AbstractFunctionParams: {
    SourceLoc endLoc = abstractFunctionParams.decl->getEndLoc();

    // For an accessor, all of the parameters are implicit, so start them at
    // the start location of the accessor.
    if (isa<FuncDecl>(abstractFunctionParams.decl) &&
        cast<FuncDecl>(abstractFunctionParams.decl)->isAccessor())
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
        startLoc = abstractFunctionParams.decl->getParameterList(1)
                     ->getLParenLoc();
      }
      return SourceRange(startLoc, endLoc);
    }

    // Otherwise, find the end of this parameter.
    auto param = abstractFunctionParams.decl->getParameterList(
                   abstractFunctionParams.listIndex)
                     ->get(abstractFunctionParams.paramIndex);
    return SourceRange(param->getEndLoc(), endLoc);
  }

  case ASTScopeKind::DefaultArgument:
    return parameter->getDefaultValue()->getSourceRange();

  case ASTScopeKind::PatternBinding: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];

    SourceRange range = patternEntry.getSourceRange();

    // Local pattern bindings cover the entire binding + continuation.
    if (patternBinding.decl->getDeclContext()->isLocalContext()) {
      range.End = getParent()->getSourceRange().End;
    }

    return range;
  }

  case ASTScopeKind::PatternInitializer:
    return patternBinding.decl->getInit(patternBinding.entry)->getSourceRange();

  case ASTScopeKind::AfterPatternBinding: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];
    // The scope of the binding begins at the end of the binding.
    SourceLoc startLoc = patternEntry.getSourceRange().End;

    // And extends to the end of the parent range.
    return SourceRange(startLoc, getParent()->getSourceRange().End);
  }
  case ASTScopeKind::BraceStmt:
    // The brace statements that represent closures start their scope at the
    // 'in' keyword, when present.
    if (getParent()->getKind() == ASTScopeKind::Closure &&
        getParent()->closure->getInLoc().isValid())
      return SourceRange(getParent()->closure->getInLoc(),
                         braceStmt.stmt->getEndLoc());

    return braceStmt.stmt->getSourceRange();

  case ASTScopeKind::LocalDeclaration:
    return SourceRange(localDeclaration->getStartLoc(),
                       getParent()->getSourceRange().End);

  case ASTScopeKind::IfStmt:
    return ifStmt->getSourceRange();

  case ASTScopeKind::ConditionalClause: {
    // For a guard continuation, the scope extends from the end of the 'else'
    // to the end of the continuation.
    if (conditionalClause.isGuardContinuation) {
      // Find the 'guard' parent.
      const ASTScope *guard = this;
      do {
        guard = guard->getParent();
      } while (guard->getKind() != ASTScopeKind::GuardStmt);

      return SourceRange(guard->guard->getBody()->getEndLoc(),
                         getParent()->getSourceRange().End);
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

    // For 'guard' statements, the conditional clause covers the continuation.
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
    return SourceRange(guard->getStartLoc(), getParent()->getSourceRange().End);

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

  case ASTScopeKind::ForStmt:
    return forStmt->getSourceRange();

  case ASTScopeKind::ForStmtInitializer:
    return SourceRange(forStmt->getFirstSemicolonLoc(), forStmt->getEndLoc());

  case ASTScopeKind::Accessors:
    return abstractStorageDecl->getBracesRange();

  case ASTScopeKind::Closure:
    if (closure->getInLoc().isValid())
      return SourceRange(closure->getInLoc(), closure->getEndLoc());

    return closure->getSourceRange();

  case ASTScopeKind::TopLevelCode:
    return topLevelCode->getSourceRange();
  }
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
  case ASTScopeKind::ForStmt:
  case ASTScopeKind::ForStmtInitializer:
  case ASTScopeKind::LocalDeclaration:
    return nullptr;
  }
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
  case ASTScopeKind::TypeOrExtensionBody:
  case ASTScopeKind::AbstractFunctionDecl:
  case ASTScopeKind::DefaultArgument:
  case ASTScopeKind::PatternBinding:
  case ASTScopeKind::PatternInitializer:
  case ASTScopeKind::BraceStmt:
  case ASTScopeKind::IfStmt:
  case ASTScopeKind::GuardStmt:
  case ASTScopeKind::RepeatWhileStmt:
  case ASTScopeKind::ForEachStmt:
  case ASTScopeKind::DoCatchStmt:
  case ASTScopeKind::SwitchStmt:
  case ASTScopeKind::ForStmt:
  case ASTScopeKind::Accessors:
  case ASTScopeKind::TopLevelCode:
    // No local declarations.
    break;

  case ASTScopeKind::GenericParams:
    result.push_back(genericParams.params->getParams()[genericParams.index]);
    break;

  case ASTScopeKind::AbstractFunctionParams:
    result.push_back(abstractFunctionParams.decl->getParameterList(
                         abstractFunctionParams.listIndex)
                       ->get(abstractFunctionParams.paramIndex));
    break;

  case ASTScopeKind::AfterPatternBinding:
    handlePattern(patternBinding.decl->getPattern(patternBinding.entry));
    break;

  case ASTScopeKind::LocalDeclaration:
    result.push_back(cast<ValueDecl>(localDeclaration));
    break;

  case ASTScopeKind::ConditionalClause:
    handlePattern(conditionalClause.stmt->getCond()[conditionalClause.index]
                    .getPatternOrNull());
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

  case ASTScopeKind::ForStmtInitializer:
    for (auto decl : forStmt->getInitializerVarDecls())
      result.push_back(cast<ValueDecl>(decl));
    break;

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

  case ASTScopeKind::LocalDeclaration:
    printScopeKind("LocalDeclaration");
    printAddress(localDeclaration);
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

  case ASTScopeKind::ForStmt:
    printScopeKind("ForStmt");
    printAddress(forStmt);
    printRange();
    break;

  case ASTScopeKind::ForStmtInitializer:
    printScopeKind("ForStmtInitializer");
    printAddress(forStmt);
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

