//===--- Pattern.cpp - Swift Language Pattern-Matching ASTs ---------------===//
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
//  This file implements the Pattern class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Pattern.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

#define PATTERN(Id, _) \
  static_assert(IsTriviallyDestructible<Id##Pattern>::value, \
                "Patterns are BumpPtrAllocated; the d'tor is never called");
#include "swift/AST/PatternNodes.def"

/// Diagnostic printing of PatternKinds.
llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS, PatternKind kind) {
  switch (kind) {
  case PatternKind::Paren:
    return OS << "parenthesized pattern";
  case PatternKind::Tuple:
    return OS << "tuple pattern";
  case PatternKind::Named:
    return OS << "pattern variable binding";
  case PatternKind::Any:
    return OS << "'_' pattern";
  case PatternKind::Typed:
    return OS << "pattern type annotation";
  case PatternKind::Is:
    return OS << "prefix 'is' pattern";
  case PatternKind::Expr:
    return OS << "expression pattern";
  case PatternKind::Var:
    return OS << "'var' binding pattern";
  case PatternKind::EnumElement:
    return OS << "enum case matching pattern";
  case PatternKind::OptionalSome:
    return OS << "optional .Some matching pattern";
  case PatternKind::Bool:
    return OS << "bool matching pattern";
  }
  llvm_unreachable("bad PatternKind");
}

StringRef Pattern::getKindName(PatternKind K) {
  switch (K) {
#define PATTERN(Id, Parent) case PatternKind::Id: return #Id;
#include "swift/AST/PatternNodes.def"
  }
  llvm_unreachable("bad PatternKind");
}

// Metaprogram to verify that every concrete class implements
// a 'static bool classof(const Pattern*)'.
template <bool fn(const Pattern*)> struct CheckClassOfPattern {
  static const bool IsImplemented = true;
};
template <> struct CheckClassOfPattern<Pattern::classof> {
  static const bool IsImplemented = false;
};

#define PATTERN(ID, PARENT) \
static_assert(CheckClassOfPattern<ID##Pattern::classof>::IsImplemented, \
              #ID "Pattern is missing classof(const Pattern*)");
#include "swift/AST/PatternNodes.def"

// Metaprogram to verify that every concrete class implements
// 'SourceRange getSourceRange()'.
typedef const char (&TwoChars)[2];
template<typename Class> 
inline char checkSourceRangeType(SourceRange (Class::*)() const);
inline TwoChars checkSourceRangeType(SourceRange (Pattern::*)() const);

/// getSourceRange - Return the full source range of the pattern.
SourceRange Pattern::getSourceRange() const {
  switch (getKind()) {
#define PATTERN(ID, PARENT) \
case PatternKind::ID: \
static_assert(sizeof(checkSourceRangeType(&ID##Pattern::getSourceRange)) == 1, \
              #ID "Pattern is missing getSourceRange()"); \
return cast<ID##Pattern>(this)->getSourceRange();
#include "swift/AST/PatternNodes.def"
  }
  
  llvm_unreachable("pattern type not handled!");
}

void Pattern::setDelayedInterfaceType(Type interfaceTy, DeclContext *dc) {
  assert(interfaceTy->hasTypeParameter() && "Not an interface type");
  Ty = interfaceTy;
  ASTContext &ctx = interfaceTy->getASTContext();
  ctx.DelayedPatternContexts[this] = dc;
  Bits.Pattern.hasInterfaceType = true;
}

Type Pattern::getType() const {
  assert(hasType());

  // If this pattern has an interface type, map it into the context type.
  if (Bits.Pattern.hasInterfaceType) {
    ASTContext &ctx = Ty->getASTContext();

    // Retrieve the generic environment to use for the mapping.
    auto found = ctx.DelayedPatternContexts.find(this);
    assert(found != ctx.DelayedPatternContexts.end());
    auto dc = found->second;

    if (auto genericEnv = dc->getGenericEnvironmentOfContext()) {
      ctx.DelayedPatternContexts.erase(this);
      Ty = genericEnv->mapTypeIntoContext(Ty);
      const_cast<Pattern*>(this)->Bits.Pattern.hasInterfaceType = false;
    }
  }

  return Ty;
}

/// getLoc - Return the caret location of the pattern.
SourceLoc Pattern::getLoc() const {
  switch (getKind()) {
#define PATTERN(ID, PARENT) \
  case PatternKind::ID: \
    if (&Pattern::getLoc != &ID##Pattern::getLoc) \
      return cast<ID##Pattern>(this)->getLoc(); \
    break;
#include "swift/AST/PatternNodes.def"
  }

  return getStartLoc();
}

void Pattern::collectVariables(SmallVectorImpl<VarDecl *> &variables) const {
  forEachVariable([&](VarDecl *VD) { variables.push_back(VD); });
}

VarDecl *Pattern::getSingleVar() const {
  auto pattern = getSemanticsProvidingPattern();
  if (auto named = dyn_cast<NamedPattern>(pattern))
    return named->getDecl();

  return nullptr;
}

namespace {
  class WalkToVarDecls : public ASTWalker {
    const std::function<void(VarDecl*)> &fn;
  public:
    
    WalkToVarDecls(const std::function<void(VarDecl*)> &fn)
    : fn(fn) {}
    
    Pattern *walkToPatternPost(Pattern *P) override {
      // Handle vars.
      if (auto *Named = dyn_cast<NamedPattern>(P))
        fn(Named->getDecl());
      return P;
    }

    // Only walk into an expression insofar as it doesn't open a new scope -
    // that is, don't walk into a closure body.
    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (isa<ClosureExpr>(E)) {
        return { false, E };
      }
      return { true, E };
    }

    // Don't walk into anything else.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }
    bool walkToTypeLocPre(TypeLoc &TL) override { return false; }
    bool walkToTypeReprPre(TypeRepr *T) override { return false; }
    bool walkToParameterListPre(ParameterList *PL) override { return false; }
    bool walkToDeclPre(Decl *D) override { return false; }
  };
} // end anonymous namespace


/// \brief apply the specified function to all variables referenced in this
/// pattern.
void Pattern::forEachVariable(llvm::function_ref<void(VarDecl *)> fn) const {
  switch (getKind()) {
  case PatternKind::Any:
  case PatternKind::Bool:
    return;

  case PatternKind::Is:
    if (auto SP = cast<IsPattern>(this)->getSubPattern())
      SP->forEachVariable(fn);
    return;

  case PatternKind::Named:
    fn(cast<NamedPattern>(this)->getDecl());
    return;

  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    return getSemanticsProvidingPattern()->forEachVariable(fn);

  case PatternKind::Tuple:
    for (auto elt : cast<TuplePattern>(this)->getElements())
      elt.getPattern()->forEachVariable(fn);
    return;

  case PatternKind::EnumElement:
    if (auto SP = cast<EnumElementPattern>(this)->getSubPattern())
      SP->forEachVariable(fn);
    return;

    case PatternKind::OptionalSome:
    cast<OptionalSomePattern>(this)->getSubPattern()->forEachVariable(fn);
    return;

  case PatternKind::Expr:
    // An ExprPattern only exists before sema has resolved a refutable pattern
    // into a concrete pattern.  We have to use an AST Walker to find the
    // VarDecls buried down inside of it.
    const_cast<Pattern*>(this)->walk(WalkToVarDecls(fn));
    return;
  }
}

/// \brief apply the specified function to all pattern nodes recursively in
/// this pattern.  This is a pre-order traversal.
void Pattern::forEachNode(llvm::function_ref<void(Pattern*)> f) {
  f(this);

  switch (getKind()) {
  // Leaf patterns have no recursion.
  case PatternKind::Any:
  case PatternKind::Named:
  case PatternKind::Expr:// FIXME: expr nodes are not modeled right in general.
  case PatternKind::Bool:
    return;

  case PatternKind::Is:
    if (auto SP = cast<IsPattern>(this)->getSubPattern())
      SP->forEachNode(f);
    return;

  case PatternKind::Paren:
    return cast<ParenPattern>(this)->getSubPattern()->forEachNode(f);
  case PatternKind::Typed:
    return cast<TypedPattern>(this)->getSubPattern()->forEachNode(f);
  case PatternKind::Var:
    return cast<VarPattern>(this)->getSubPattern()->forEachNode(f);

  case PatternKind::Tuple:
    for (auto elt : cast<TuplePattern>(this)->getElements())
      elt.getPattern()->forEachNode(f);
    return;

  case PatternKind::EnumElement: {
    auto *OP = cast<EnumElementPattern>(this);
    if (OP->hasSubPattern())
      OP->getSubPattern()->forEachNode(f);
    return;
  }
  case PatternKind::OptionalSome:
    cast<OptionalSomePattern>(this)->getSubPattern()->forEachNode(f);
    return;
  }
}

bool Pattern::hasStorage() const {
  bool HasStorage = false;
  forEachVariable([&](VarDecl *VD) {
    if (VD->hasStorage())
      HasStorage = true;
  });

  return HasStorage;
}

/// Return true if this is a non-resolved ExprPattern which is syntactically
/// irrefutable.
static bool isIrrefutableExprPattern(const ExprPattern *EP) {
  // If the pattern has a registered match expression, it's
  // a type-checked ExprPattern.
  if (EP->getMatchExpr()) return false;

  auto expr = EP->getSubExpr();
  while (true) {
    // Drill into parens.
    if (auto parens = dyn_cast<ParenExpr>(expr)) {
      expr = parens->getSubExpr();
      continue;
    }

      // A '_' is an untranslated AnyPattern.
    if (isa<DiscardAssignmentExpr>(expr))
      return true;

    // Everything else is non-exhaustive.
    return false;
  }
}

/// Return true if this pattern (or a subpattern) is refutable.
bool Pattern::isRefutablePattern() const {
  bool foundRefutablePattern = false;
  const_cast<Pattern*>(this)->forEachNode([&](Pattern *Node) {

    // If this is an always matching 'is' pattern, then it isn't refutable.
    if (auto *is = dyn_cast<IsPattern>(Node))
      if (is->getCastKind() == CheckedCastKind::Coercion ||
          is->getCastKind() == CheckedCastKind::BridgingCoercion)
        return;

    // If this is an ExprPattern that isn't resolved yet, do some simple
    // syntactic checks.
    // FIXME: This is unsound, since type checking will turn other more
    // complicated patterns into non-refutable forms.
    if (auto *ep = dyn_cast<ExprPattern>(Node))
      if (isIrrefutableExprPattern(ep))
        return;

    switch (Node->getKind()) {
#define PATTERN(ID, PARENT) case PatternKind::ID: break;
#define REFUTABLE_PATTERN(ID, PARENT) \
case PatternKind::ID: foundRefutablePattern = true; break;
#include "swift/AST/PatternNodes.def"
    }
  });
    
  return foundRefutablePattern;
}

/// Standard allocator for Patterns.
void *Pattern::operator new(size_t numBytes, const ASTContext &C) {
  return C.Allocate(numBytes, alignof(Pattern));
}

/// Find the name directly bound by this pattern.  When used as a
/// tuple element in a function signature, such names become part of
/// the type.
Identifier Pattern::getBoundName() const {
  if (auto *NP = dyn_cast<NamedPattern>(getSemanticsProvidingPattern()))
    return NP->getBoundName();
  return Identifier();
}

Identifier NamedPattern::getBoundName() const {
  return Var->getName();
}


/// Allocate a new pattern that matches a tuple.
TuplePattern *TuplePattern::create(ASTContext &C, SourceLoc lp,
                                   ArrayRef<TuplePatternElt> elts, SourceLoc rp,
                                   Optional<bool> implicit) {
  if (!implicit.hasValue())
    implicit = !lp.isValid();

  unsigned n = elts.size();
  void *buffer = C.Allocate(totalSizeToAlloc<TuplePatternElt>(n),
                            alignof(TuplePattern));
  TuplePattern *pattern = ::new (buffer) TuplePattern(lp, n, rp, *implicit);
  std::uninitialized_copy(elts.begin(), elts.end(),
                          pattern->getTrailingObjects<TuplePatternElt>());
  return pattern;
}

Pattern *TuplePattern::createSimple(ASTContext &C, SourceLoc lp,
                                    ArrayRef<TuplePatternElt> elements,
                                    SourceLoc rp,
                                    Optional<bool> implicit) {
  assert(lp.isValid() == rp.isValid());

  if (elements.size() == 1 &&
      elements[0].getPattern()->getBoundName().empty()) {
    auto &first = const_cast<TuplePatternElt&>(elements.front());
    return new (C) ParenPattern(lp, first.getPattern(), rp, implicit);
  }

  return create(C, lp, elements, rp, implicit);
}

SourceRange TuplePattern::getSourceRange() const {
  if (LPLoc.isValid())
    return { LPLoc, RPLoc };
  auto Fields = getElements();
  if (Fields.empty())
    return {};
  return { Fields.front().getPattern()->getStartLoc(),
           Fields.back().getPattern()->getEndLoc() };
}

TypedPattern::TypedPattern(Pattern *pattern, TypeRepr *tr,
                           Optional<bool> implicit)
  : Pattern(PatternKind::Typed), SubPattern(pattern), PatTypeRepr(tr) {
  if (implicit ? *implicit : tr && !tr->getSourceRange().isValid())
    setImplicit();
  Bits.TypedPattern.IsPropagatedType = false;
}

TypeLoc TypedPattern::getTypeLoc() const {
  TypeLoc loc = TypeLoc(PatTypeRepr);

  if (hasType())
    loc.setType(getType());

  return loc;
}

SourceLoc TypedPattern::getLoc() const {
  if (SubPattern->isImplicit() && PatTypeRepr)
    return PatTypeRepr->getSourceRange().Start;

  return SubPattern->getLoc();
}

SourceRange TypedPattern::getSourceRange() const {
  if (isImplicit() || isPropagatedType()) {
    // If a TypedPattern is implicit, then its type is definitely implicit, so
    // we should ignore its location.  On the other hand, the sub-pattern can
    // be explicit or implicit.
    return SubPattern->getSourceRange();
  }

  if (!PatTypeRepr)
    return SourceRange();

  if (SubPattern->isImplicit())
    return PatTypeRepr->getSourceRange();

  return { SubPattern->getSourceRange().Start,
           PatTypeRepr->getSourceRange().End };
}

/// Construct an ExprPattern.
ExprPattern::ExprPattern(Expr *e, bool isResolved, Expr *matchExpr,
                         VarDecl *matchVar,
                         Optional<bool> implicit)
  : Pattern(PatternKind::Expr), SubExprAndIsResolved(e, isResolved),
    MatchExpr(matchExpr), MatchVar(matchVar) {
  assert(!matchExpr || e->isImplicit() == matchExpr->isImplicit());
  if (implicit.hasValue() ? *implicit : e->isImplicit())
    setImplicit();
}

SourceLoc ExprPattern::getLoc() const {
  return getSubExpr()->getLoc();
}

SourceRange ExprPattern::getSourceRange() const {
  return getSubExpr()->getSourceRange();
}

// See swift/Basic/Statistic.h for declaration: this enables tracing Patterns, is
// defined here to avoid too much layering violation / circular linkage
// dependency.

struct PatternTraceFormatter : public UnifiedStatsReporter::TraceFormatter {
  void traceName(const void *Entity, raw_ostream &OS) const {
    if (!Entity)
      return;
    const Pattern *P = static_cast<const Pattern *>(Entity);
    if (const NamedPattern *NP = dyn_cast<NamedPattern>(P)) {
      OS << NP->getBoundName();
    }
  }
  void traceLoc(const void *Entity, SourceManager *SM,
                clang::SourceManager *CSM, raw_ostream &OS) const {
    if (!Entity)
      return;
    const Pattern *P = static_cast<const Pattern *>(Entity);
    P->getSourceRange().print(OS, *SM, false);
  }
};

static PatternTraceFormatter TF;

template<>
const UnifiedStatsReporter::TraceFormatter*
FrontendStatsTracer::getTraceFormatter<const Pattern *>() {
  return &TF;
}
