//===--- PreCheckTarget.cpp - Pre-checking pass ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Pre-checking resolves unqualified name references, type expressions and
// operators.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "TypeCheckConcurrency.h"
#include "TypeCheckType.h"
#include "TypoCorrection.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Parse/Confusables.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;
using namespace constraints;

//===----------------------------------------------------------------------===//
// High-level entry points.
//===----------------------------------------------------------------------===//

static unsigned getNumArgs(ValueDecl *value) {
  if (auto *func = dyn_cast<FuncDecl>(value))
    return func->getParameters()->size();
  return ~0U;
}

static bool matchesDeclRefKind(ValueDecl *value, DeclRefKind refKind) {
  switch (refKind) {
  // An ordinary reference doesn't ignore anything.
  case DeclRefKind::Ordinary:
    return true;

  // A binary-operator reference only honors FuncDecls with a certain type.
  case DeclRefKind::BinaryOperator:
    return (getNumArgs(value) == 2);

  case DeclRefKind::PrefixOperator:
    return (!value->getAttrs().hasAttribute<PostfixAttr>() &&
            getNumArgs(value) == 1);

  case DeclRefKind::PostfixOperator:
    return (value->getAttrs().hasAttribute<PostfixAttr>() &&
            getNumArgs(value) == 1);
  }
  llvm_unreachable("bad declaration reference kind");
}

static bool containsDeclRefKind(LookupResult &lookupResult,
                                DeclRefKind refKind) {
  for (auto candidate : lookupResult) {
    ValueDecl *D = candidate.getValueDecl();
    if (!D)
      continue;
    if (matchesDeclRefKind(D, refKind))
      return true;
  }
  return false;
}

/// Emit a diagnostic with a fixit hint for an invalid binary operator, showing
/// how to split it according to splitCandidate.
static void diagnoseBinOpSplit(ASTContext &Context, UnresolvedDeclRefExpr *UDRE,
                               std::pair<unsigned, bool> splitCandidate,
                               Diag<Identifier, Identifier, bool> diagID) {

  unsigned splitLoc = splitCandidate.first;
  bool isBinOpFirst = splitCandidate.second;
  StringRef nameStr = UDRE->getName().getBaseIdentifier().str();
  auto startStr = nameStr.substr(0, splitLoc);
  auto endStr = nameStr.drop_front(splitLoc);

  // One valid split found, it is almost certainly the right answer.
  auto diag = Context.Diags.diagnose(
      UDRE->getLoc(), diagID, Context.getIdentifier(startStr),
      Context.getIdentifier(endStr), isBinOpFirst);
  // Highlight the whole operator.
  diag.highlight(UDRE->getLoc());
  // Insert whitespace on the left if the binop is at the start, or to the
  // right if it is end.
  if (isBinOpFirst)
    diag.fixItInsert(UDRE->getLoc(), " ");
  else
    diag.fixItInsertAfter(UDRE->getLoc(), " ");

  // Insert a space between the operators.
  diag.fixItInsert(UDRE->getLoc().getAdvancedLoc(splitLoc), " ");
}

/// If we failed lookup of a binary operator, check to see it to see if
/// it is a binary operator juxtaposed with a unary operator (x*-4) that
/// needs whitespace.  If so, emit specific diagnostics for it and return true,
/// otherwise return false.
static bool diagnoseOperatorJuxtaposition(UnresolvedDeclRefExpr *UDRE,
                                          DeclContext *DC) {
  Identifier name = UDRE->getName().getBaseIdentifier();
  StringRef nameStr = name.str();
  if (!name.isOperator() || nameStr.size() < 2)
    return false;

  bool isBinOp = UDRE->getRefKind() == DeclRefKind::BinaryOperator;

  // If this is a binary operator, relex the token, to decide whether it has
  // whitespace around it or not.  If it does "x +++ y", then it isn't likely to
  // be a case where a space was forgotten.
  auto &Context = DC->getASTContext();
  if (isBinOp) {
    auto tok = Lexer::getTokenAtLocation(Context.SourceMgr, UDRE->getLoc());
    if (tok.getKind() != tok::oper_binary_unspaced)
      return false;
  }

  // Okay, we have a failed lookup of a multicharacter operator. Check to see if
  // lookup succeeds if part is split off, and record the matches found.
  //
  // In the case of a binary operator, the bool indicated is false if the
  // first half of the split is the unary operator (x!*4) or true if it is the
  // binary operator (x*+4).
  std::vector<std::pair<unsigned, bool>> WorkableSplits;

  // Check all the potential splits.
  for (unsigned splitLoc = 1, e = nameStr.size(); splitLoc != e; ++splitLoc) {
    // For it to be a valid split, the start and end section must be valid
    // operators, splitting a unicode code point isn't kosher.
    auto startStr = nameStr.substr(0, splitLoc);
    auto endStr = nameStr.drop_front(splitLoc);
    if (!Lexer::isOperator(startStr) || !Lexer::isOperator(endStr))
      continue;

    DeclNameRef startName(Context.getIdentifier(startStr));
    DeclNameRef endName(Context.getIdentifier(endStr));

    // Perform name lookup for the first and second pieces.  If either fail to
    // be found, then it isn't a valid split.
    auto startLookup = TypeChecker::lookupUnqualified(
        DC, startName, UDRE->getLoc(), defaultUnqualifiedLookupOptions);
    if (!startLookup) continue;
    auto endLookup = TypeChecker::lookupUnqualified(DC, endName, UDRE->getLoc(),
                                                    defaultUnqualifiedLookupOptions);
    if (!endLookup) continue;

    // If the overall operator is a binary one, then we're looking at
    // juxtaposed binary and unary operators.
    if (isBinOp) {
      // Look to see if the candidates found could possibly match.
      if (containsDeclRefKind(startLookup, DeclRefKind::PostfixOperator) &&
          containsDeclRefKind(endLookup, DeclRefKind::BinaryOperator))
        WorkableSplits.push_back({ splitLoc, false });

      if (containsDeclRefKind(startLookup, DeclRefKind::BinaryOperator) &&
          containsDeclRefKind(endLookup, DeclRefKind::PrefixOperator))
        WorkableSplits.push_back({ splitLoc, true });
    } else {
      // Otherwise, it is two of the same kind, e.g. "!!x" or "!~x".
      if (containsDeclRefKind(startLookup, UDRE->getRefKind()) &&
          containsDeclRefKind(endLookup, UDRE->getRefKind()))
        WorkableSplits.push_back({ splitLoc, false });
    }
  }

  switch (WorkableSplits.size()) {
  case 0:
    // No splits found, can't produce this diagnostic.
    return false;
  case 1:
    // One candidate: produce an error with a fixit on it.
    if (isBinOp)
      diagnoseBinOpSplit(Context, UDRE, WorkableSplits[0],
                         diag::unspaced_binary_operator_fixit);
    else
      Context.Diags.diagnose(
          UDRE->getLoc().getAdvancedLoc(WorkableSplits[0].first),
          diag::unspaced_unary_operator);
    return true;

  default:
    // Otherwise, we have to produce a series of notes listing the various
    // options.
    Context.Diags
        .diagnose(UDRE->getLoc(), isBinOp ? diag::unspaced_binary_operator
                                          : diag::unspaced_unary_operator)
        .highlight(UDRE->getLoc());

    if (isBinOp) {
      for (auto candidateSplit : WorkableSplits)
        diagnoseBinOpSplit(Context, UDRE, candidateSplit,
                           diag::unspaced_binary_operators_candidate);
    }
    return true;
  }
}

static bool diagnoseRangeOperatorMisspell(DiagnosticEngine &Diags,
                                          UnresolvedDeclRefExpr *UDRE) {
  auto name = UDRE->getName().getBaseIdentifier();
  if (!name.isOperator())
    return false;

  auto corrected = StringRef();
  if (name.str() == ".." || name.str() == "...." ||
      name.str() == ".…" || name.str() == "…" || name.str() == "….")
    corrected = "...";
  else if (name.str() == "...<" || name.str() == "....<" ||
           name.str() == "…<")
    corrected = "..<";

  if (!corrected.empty()) {
    Diags
        .diagnose(UDRE->getLoc(), diag::cannot_find_in_scope_corrected,
                  UDRE->getName(), true, corrected)
        .highlight(UDRE->getSourceRange())
        .fixItReplace(UDRE->getSourceRange(), corrected);

    return true;
  }
  return false;
}

static bool diagnoseNonexistentPowerOperator(DiagnosticEngine &Diags,
                                             UnresolvedDeclRefExpr *UDRE,
                                             DeclContext *DC) {
  auto name = UDRE->getName().getBaseIdentifier();
  if (!(name.isOperator() && name.is("**")))
    return false;

  DC = DC->getModuleScopeContext();

  auto &ctx = DC->getASTContext();
  DeclNameRef powerName(ctx.getIdentifier("pow"));

  // Look if 'pow(_:_:)' exists within current context.
  auto lookUp = TypeChecker::lookupUnqualified(
      DC, powerName, UDRE->getLoc(), defaultUnqualifiedLookupOptions);
  if (lookUp) {
    Diags.diagnose(UDRE->getLoc(), diag::nonexistent_power_operator)
        .highlight(UDRE->getSourceRange());
    return true;
  }

  return false;
}

static bool diagnoseIncDecOperator(DiagnosticEngine &Diags,
                                   UnresolvedDeclRefExpr *UDRE) {
  auto name = UDRE->getName().getBaseIdentifier();
  if (!name.isOperator())
    return false;

  auto corrected = StringRef();
  if (name.str() == "++")
    corrected = "+= 1";
  else if (name.str() == "--")
    corrected = "-= 1";

  if (!corrected.empty()) {
    Diags
        .diagnose(UDRE->getLoc(), diag::cannot_find_in_scope_corrected,
                  UDRE->getName(), true, corrected)
        .highlight(UDRE->getSourceRange());

    return true;
  }
  return false;
}

static bool findNonMembers(ArrayRef<LookupResultEntry> lookupResults,
                           DeclRefKind refKind, bool breakOnMember,
                           SmallVectorImpl<ValueDecl *> &ResultValues,
                           llvm::function_ref<bool(ValueDecl *)> isValid) {
  bool AllDeclRefs = true;
  for (auto Result : lookupResults) {
    // If we find a member, then all of the results aren't non-members.
    bool IsMember =
        (Result.getBaseDecl() && !isa<ModuleDecl>(Result.getBaseDecl()));
    if (IsMember) {
      AllDeclRefs = false;
      if (breakOnMember)
        break;
      continue;
    }

    ValueDecl *D = Result.getValueDecl();
    if (!isValid(D))
      return false;

    if (matchesDeclRefKind(D, refKind))
      ResultValues.push_back(D);
  }

  return AllDeclRefs;
}

namespace {
enum class MemberChainKind {
  OptionalBind,     // A 'x?.y' optional binding chain
  UnresolvedMember, // A '.foo.bar' chain
};
} // end anonymous namespace

/// Find the next element in a chain of members. If this expression is (or
/// could be) the base of such a chain, this will return \c nullptr.
static Expr *getMemberChainSubExpr(Expr *expr, MemberChainKind kind) {
  assert(expr && "getMemberChainSubExpr called with null expr!");
  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(expr))
    return UDE->getBase();
  if (auto *CE = dyn_cast<CallExpr>(expr))
    return CE->getFn();
  if (auto *BOE = dyn_cast<BindOptionalExpr>(expr))
    return BOE->getSubExpr();
  if (auto *FVE = dyn_cast<ForceValueExpr>(expr))
    return FVE->getSubExpr();
  if (auto *SE = dyn_cast<SubscriptExpr>(expr))
    return SE->getBase();
  if (auto *DSE = dyn_cast<DotSelfExpr>(expr))
    return DSE->getSubExpr();
  if (auto *USE = dyn_cast<UnresolvedSpecializeExpr>(expr))
    return USE->getSubExpr();
  if (auto *CCE = dyn_cast<CodeCompletionExpr>(expr))
    return CCE->getBase();

  if (kind == MemberChainKind::OptionalBind) {
    // We allow postfix operators to be part of the optional member chain, e.g:
    //
    //   for?.bar++
    //   x.y?^.foo()
    //
    // Note this behavior is specific to optional chains, we treat e.g
    // `.foo^` as `(.foo)^`.
    if (auto *PO = dyn_cast<PostfixUnaryExpr>(expr))
      return PO->getOperand();

    // Unresolved member chains can themselves be nested in optional chains
    // since optional chains can include postfix operators.
    if (auto *UME = dyn_cast<UnresolvedMemberChainResultExpr>(expr))
      return UME->getSubExpr();
  }

  return nullptr;
}

UnresolvedMemberExpr *TypeChecker::getUnresolvedMemberChainBase(Expr *expr) {
  if (auto *subExpr =
          getMemberChainSubExpr(expr, MemberChainKind::UnresolvedMember)) {
    return getUnresolvedMemberChainBase(subExpr);
  }
  return dyn_cast<UnresolvedMemberExpr>(expr);
}

static bool isBindOptionalMemberChain(Expr *expr) {
  if (isa<BindOptionalExpr>(expr))
    return true;

  if (auto *base = getMemberChainSubExpr(expr, MemberChainKind::OptionalBind))
    return isBindOptionalMemberChain(base);

  return false;
}

/// Whether this expression sits at the end of a chain of member accesses.
static bool isMemberChainTail(Expr *expr, Expr *parent, MemberChainKind kind) {
  assert(expr && "isMemberChainTail called with null expr!");
  // If this expression's parent is not itself part of a chain (or, this expr
  // has no parent expr), this must be the tail of the chain.
  return !parent || getMemberChainSubExpr(parent, kind) != expr;
}

static bool isValidForwardReference(ValueDecl *D, DeclContext *DC,
                                    ValueDecl **localDeclAfterUse) {
  *localDeclAfterUse = nullptr;

  // References to variables injected by lldb are always valid.
  if (isa<VarDecl>(D) && cast<VarDecl>(D)->isDebuggerVar())
    return true;

  // If we find something in the current context, it must be a forward
  // reference, because otherwise if it was in scope, it would have
  // been returned by the call to ASTScope::lookupLocalDecls() above.
  if (D->getDeclContext()->isLocalContext()) {
    do {
      if (D->getDeclContext() == DC) {
        *localDeclAfterUse = D;
        return false;
      }

      // If we're inside of a 'defer' context, walk up to the parent
      // and check again. We don't want 'defer' bodies to forward
      // reference bindings in the immediate outer scope.
    } while (isa<FuncDecl>(DC) &&
             cast<FuncDecl>(DC)->isDeferBody() &&
             (DC = DC->getParent()));
  }
  return true;
}

/// Checks whether this is a BinaryExpr with operator `&` and returns the
/// BinaryExpr, if so.
static BinaryExpr *getCompositionExpr(Expr *expr) {
  if (auto *binaryExpr = dyn_cast<BinaryExpr>(expr)) {
    // look at the name of the operator, if it is a '&' we can create the
    // composition TypeExpr
    auto fn = binaryExpr->getFn();
    if (auto Overload = dyn_cast<OverloadedDeclRefExpr>(fn)) {
      if (llvm::any_of(Overload->getDecls(), [](auto *decl) -> bool {
            return decl->getBaseName() == "&";
          }))
        return binaryExpr;
    } else if (auto *Decl = dyn_cast<UnresolvedDeclRefExpr>(fn)) {
      if (Decl->getName().isSimpleName() &&
          Decl->getName().getBaseName() == "&")
        return binaryExpr;
    }
  }

  return nullptr;
}

/// Diagnoses an unqualified `init` expression.
///
/// \param initExpr The \c init expression.
/// \param dc The declaration context of \p initExpr.
///
/// \returns An expression matching `self.init` or `super.init` that can be used
/// to recover, or `nullptr` if cannot recover.
static UnresolvedDotExpr *
diagnoseUnqualifiedInit(UnresolvedDeclRefExpr *initExpr, DeclContext *dc,
                        ASTContext &ctx) {
  const auto loc = initExpr->getLoc();

  enum class Suggestion : unsigned {
    None = 0,
    Self = 1,
    Super = 2,
  };

  Suggestion suggestion = [dc]() {
    NominalTypeDecl *nominal = nullptr;
    {
      auto *typeDC = dc->getInnermostTypeContext();
      if (!typeDC) {
        // No type context--no suggestion.
        return Suggestion::None;
      }

      nominal = typeDC->getSelfNominalTypeDecl();
    }

    auto *classDecl = dyn_cast<ClassDecl>(nominal);
    if (!classDecl || !classDecl->hasSuperclass()) {
      // No class or no superclass--suggest 'self.'.
      return Suggestion::Self;
    }

    if (auto *initDecl = dyn_cast<ConstructorDecl>(dc)) {
      if (initDecl->getAttrs().hasAttribute<ConvenienceAttr>()) {
        // Innermost context is a convenience initializer--suggest 'self.'.
        return Suggestion::Self;
      } else {
        // Innermost context is a designated initializer--suggest 'super.'.
        return Suggestion::Super;
      }
    }

    // Class context but innermost context is not an initializer--suggest
    // 'self.'. 'super.' might be possible too, but is far lesss likely to be
    // the right answer.
    return Suggestion::Self;
  }();

  auto diag =
      ctx.Diags.diagnose(loc, diag::unqualified_init, (unsigned)suggestion);

  Expr *base = nullptr;
  switch (suggestion) {
  case Suggestion::None:
    return nullptr;
  case Suggestion::Self:
    diag.fixItInsert(loc, "self.");
    base = new (ctx)
        UnresolvedDeclRefExpr(DeclNameRef(ctx.Id_self), DeclRefKind::Ordinary,
                              initExpr->getNameLoc());
    base->setImplicit(true);
    break;
  case Suggestion::Super:
    diag.fixItInsert(loc, "super.");
    base = new (ctx) SuperRefExpr(/*Self=*/nullptr, loc, /*Implicit=*/true);
    break;
  }

  return new (ctx)
      UnresolvedDotExpr(base, /*dotloc=*/SourceLoc(), initExpr->getName(),
                        initExpr->getNameLoc(), /*implicit=*/true);
}

/// Bind an UnresolvedDeclRefExpr by performing name lookup and
/// returning the resultant expression. Context is the DeclContext used
/// for the lookup.
Expr *TypeChecker::resolveDeclRefExpr(UnresolvedDeclRefExpr *UDRE,
                                      DeclContext *DC) {
  auto &Context = DC->getASTContext();
  DeclNameRef Name = UDRE->getName();
  SourceLoc Loc = UDRE->getLoc();

  auto errorResult = [&]() -> Expr * {
    return new (Context) ErrorExpr(UDRE->getSourceRange());
  };

  TypeChecker::checkForForbiddenPrefix(Context, Name.getBaseName());

  // Try and recover if we have an unqualified 'init'.
  if (Name.getBaseName().isConstructor()) {
    auto *recoveryExpr = diagnoseUnqualifiedInit(UDRE, DC, Context);
    if (!recoveryExpr)
      return errorResult();

    return recoveryExpr;
  }

  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  DeclNameRef LookupName = Name;
  if (Name.isCompoundName()) {
    auto &context = DC->getASTContext();

    // Remove any $ prefixes for lookup
    SmallVector<Identifier, 4> lookupLabels;
    for (auto label : Name.getArgumentNames()) {
      if (label.hasDollarPrefix()) {
        auto unprefixed = label.str().drop_front();
        lookupLabels.push_back(context.getIdentifier(unprefixed));
      } else {
        lookupLabels.push_back(label);
      }
    }

    DeclName lookupName(context, Name.getBaseName(), lookupLabels);
    LookupName = DeclNameRef(lookupName);
  }

  // Perform standard value name lookup.
  NameLookupOptions lookupOptions = defaultUnqualifiedLookupOptions;
  // TODO: Include all of the possible members to give a solver a
  //       chance to diagnose name shadowing which requires explicit
  //       name/module qualifier to access top-level name.
  lookupOptions |= NameLookupFlags::IncludeOuterResults;

  LookupResult Lookup;

  bool AllDeclRefs = true;
  SmallVector<ValueDecl*, 4> ResultValues;

  // First, look for a local binding in scope.
  if (Loc.isValid() && !Name.isOperator()) {
    ASTScope::lookupLocalDecls(DC->getParentSourceFile(),
                               LookupName.getFullName(), Loc,
                               /*stopAfterInnermostBraceStmt=*/false,
                               ResultValues);
    for (auto *localDecl : ResultValues) {
      Lookup.add(LookupResultEntry(localDecl), /*isOuter=*/false);
    }
  }

  if (!Lookup) {
    // Now, look for all local bindings, even forward references, as well
    // as type members and top-level declarations.
    if (Loc.isInvalid())
      DC = DC->getModuleScopeContext();

    Lookup = TypeChecker::lookupUnqualified(DC, LookupName, Loc, lookupOptions);

    ValueDecl *localDeclAfterUse = nullptr;
    AllDeclRefs =
        findNonMembers(Lookup.innerResults(), UDRE->getRefKind(),
                       /*breakOnMember=*/true, ResultValues,
                       [&](ValueDecl *D) {
                         return isValidForwardReference(D, DC, &localDeclAfterUse);
                       });

    // If local declaration after use is found, check outer results for
    // better matching candidates.
    if (ResultValues.empty() && localDeclAfterUse) {
      auto innerDecl = localDeclAfterUse;
      while (localDeclAfterUse) {
        if (Lookup.outerResults().empty()) {
          Context.Diags.diagnose(Loc, diag::use_local_before_declaration, Name);
          Context.Diags.diagnose(innerDecl, diag::decl_declared_here,
                                 localDeclAfterUse);
          return errorResult();
        }

        Lookup.shiftDownResults();
        ResultValues.clear();
        localDeclAfterUse = nullptr;
        AllDeclRefs =
            findNonMembers(Lookup.innerResults(), UDRE->getRefKind(),
                           /*breakOnMember=*/true, ResultValues,
                           [&](ValueDecl *D) {
                             return isValidForwardReference(D, DC, &localDeclAfterUse);
                           });
      }
    }
  }

  if (!Lookup) {
    // If we failed lookup of an operator, check to see if this is a range
    // operator misspelling. Otherwise try to diagnose a juxtaposition
    // e.g. (x*-4) that needs whitespace.
    if (diagnoseRangeOperatorMisspell(Context.Diags, UDRE) ||
        diagnoseIncDecOperator(Context.Diags, UDRE) ||
        diagnoseOperatorJuxtaposition(UDRE, DC) ||
        diagnoseNonexistentPowerOperator(Context.Diags, UDRE, DC)) {
      return errorResult();
    }

    // Try ignoring access control.
    NameLookupOptions relookupOptions = lookupOptions;
    relookupOptions |= NameLookupFlags::IgnoreAccessControl;
    auto inaccessibleResults =
        TypeChecker::lookupUnqualified(DC, LookupName, Loc, relookupOptions);
    if (inaccessibleResults) {
      // FIXME: What if the unviable candidates have different levels of access?
      const ValueDecl *first = inaccessibleResults.front().getValueDecl();
      auto accessLevel =
          first->getFormalAccessScope().accessLevelForDiagnostics();
      Context.Diags.diagnose(Loc, diag::candidate_inaccessible, first,
                             accessLevel);

      // FIXME: If any of the candidates (usually just one) are in the same
      // module we could offer a fix-it.
      for (auto lookupResult : inaccessibleResults) {
        auto *VD = lookupResult.getValueDecl();
        VD->diagnose(diag::decl_declared_here, VD);
      }

      // Don't try to recover here; we'll get more access-related diagnostics
      // downstream if the type of the inaccessible decl is also inaccessible.
      return errorResult();
    }

    // Try ignoring missing imports.
    relookupOptions |= NameLookupFlags::IgnoreMissingImports;
    auto nonImportedResults =
        TypeChecker::lookupUnqualified(DC, LookupName, Loc, relookupOptions);
    if (nonImportedResults) {
      const ValueDecl *first = nonImportedResults.front().getValueDecl();
      maybeDiagnoseMissingImportForMember(first, DC, Loc);

      // Don't try to recover here; we'll get more access-related diagnostics
      // downstream if the type of the inaccessible decl is also inaccessible.
      return errorResult();
    }

    // TODO: Name will be a compound name if it was written explicitly as
    // one, but we should also try to propagate labels into this.
    DeclNameLoc nameLoc = UDRE->getNameLoc();

    Identifier simpleName = Name.getBaseIdentifier();
    const char *buffer = simpleName.get();
    llvm::SmallString<64> expectedIdentifier;
    bool isConfused = false;
    uint32_t codepoint;
    uint32_t firstConfusableCodepoint = 0;
    int totalCodepoints = 0;
    int offset = 0;
    while ((codepoint = validateUTF8CharacterAndAdvance(buffer,
                                                        buffer +
                                                        strlen(buffer)))
           != ~0U) {
      int length = (buffer - simpleName.get()) - offset;
      if (auto expectedCodepoint =
          confusable::tryConvertConfusableCharacterToASCII(codepoint)) {
        if (firstConfusableCodepoint == 0) {
          firstConfusableCodepoint = codepoint;
        }
        isConfused = true;
        expectedIdentifier += expectedCodepoint;
      } else {
        expectedIdentifier += (char)codepoint;
      }

      totalCodepoints++;

      offset += length;
    }

    auto emitBasicError = [&] {
      
      if (Name.isSimpleName(Context.Id_self)) {
        // `self` gets diagnosed with a different error when it can't be found.
        Context.Diags
            .diagnose(Loc, diag::cannot_find_self_in_scope)
            .highlight(UDRE->getSourceRange());
      } else {
        Context.Diags
            .diagnose(Loc, diag::cannot_find_in_scope, Name,
                      Name.isOperator())
            .highlight(UDRE->getSourceRange());
      }

      if (!Context.LangOpts.DisableExperimentalClangImporterDiagnostics) {
        Context.getClangModuleLoader()->diagnoseTopLevelValue(
            Name.getFullName());
      }
    };

    if (!isConfused) {
      if (Name.isSimpleName(Context.Id_Self)) {
        if (DeclContext *typeContext = DC->getInnermostTypeContext()){
          Type SelfType = typeContext->getSelfInterfaceType();

          if (typeContext->getSelfClassDecl() &&
              !typeContext->getSelfClassDecl()->isForeignReferenceType())
            SelfType = DynamicSelfType::get(SelfType, Context);
          return new (Context)
              TypeExpr(new (Context) SelfTypeRepr(SelfType, Loc));
        }
      }

      TypoCorrectionResults corrections(Name, nameLoc);

      // FIXME: Don't perform typo correction inside macro arguments, because it
      // will invoke synthesizing declarations in this scope, which will attempt to
      // expand this macro which leads to circular reference errors.
      if (!namelookup::isInMacroArgument(DC->getParentSourceFile(), UDRE->getLoc())) {
        TypeChecker::performTypoCorrection(DC, UDRE->getRefKind(), Type(),
                                           lookupOptions, corrections);
      }

      if (auto typo = corrections.claimUniqueCorrection()) {
        auto diag = Context.Diags.diagnose(
            Loc, diag::cannot_find_in_scope_corrected, Name,
            Name.isOperator(), typo->CorrectedName.getBaseIdentifier().str());
        diag.highlight(UDRE->getSourceRange());
        typo->addFixits(diag);
      } else {
        emitBasicError();
      }

      corrections.noteAllCandidates();
    } else {
      emitBasicError();

      if (totalCodepoints == 1) {
        auto charNames = confusable::getConfusableAndBaseCodepointNames(
            firstConfusableCodepoint);
        Context.Diags
            .diagnose(Loc, diag::single_confusable_character,
                      UDRE->getName().isOperator(), simpleName.str(),
                      charNames.first, expectedIdentifier, charNames.second)
            .fixItReplace(Loc, expectedIdentifier);
      } else {
        Context.Diags
            .diagnose(Loc, diag::confusable_character,
                      UDRE->getName().isOperator(), simpleName.str(),
                      expectedIdentifier)
            .fixItReplace(Loc, expectedIdentifier);
      }
    }

    // TODO: consider recovering from here.  We may want some way to suppress
    // downstream diagnostics, though.

    return errorResult();
  }

  // FIXME: Need to refactor the way we build an AST node from a lookup result!

  auto buildTypeExpr = [&](TypeDecl *D) -> Expr * {
    // FIXME: This is odd.
    if (isa<ModuleDecl>(D)) {
      return new (Context) DeclRefExpr(
          D, UDRE->getNameLoc(),
          /*Implicit=*/false, AccessSemantics::Ordinary, D->getInterfaceType());
    }

    auto *LookupDC = Lookup[0].getDeclContext();
    bool makeTypeValue = false;

    if (isa<GenericTypeParamDecl>(D) &&
        cast<GenericTypeParamDecl>(D)->isValue()) {
      makeTypeValue = true;
    }

    if (UDRE->isImplicit()) {
      return TypeExpr::createImplicitForDecl(
          UDRE->getNameLoc(), D, LookupDC,
          // It might happen that LookupDC is null if this is checking
          // synthesized code, in that case, don't map the type into context,
          // but return as is -- the synthesis should ensure the type is
          // correct.
          LookupDC ? LookupDC->mapTypeIntoContext(D->getInterfaceType())
                   : D->getInterfaceType());
    } else {
      if (makeTypeValue) {
        return TypeValueExpr::createForDecl(UDRE->getNameLoc(), D, LookupDC);
      } else {
        return TypeExpr::createForDecl(UDRE->getNameLoc(), D, LookupDC);
      }
    }
  };

  // If we have an unambiguous reference to a type decl, form a TypeExpr.
  if (Lookup.size() == 1 && UDRE->getRefKind() == DeclRefKind::Ordinary &&
      isa<TypeDecl>(Lookup[0].getValueDecl())) {
    return buildTypeExpr(cast<TypeDecl>(Lookup[0].getValueDecl()));
  }

  if (AllDeclRefs) {
    // Diagnose uses of operators that found no matching candidates.
    if (ResultValues.empty()) {
      assert(UDRE->getRefKind() != DeclRefKind::Ordinary);
      Context.Diags.diagnose(
          Loc, diag::use_nonmatching_operator, Name,
          UDRE->getRefKind() == DeclRefKind::BinaryOperator
              ? 0
              : UDRE->getRefKind() == DeclRefKind::PrefixOperator ? 1 : 2);
      return errorResult();
    }

    // For operators, sort the results so that non-generic operations come
    // first.
    // Note: this is part of a performance hack to prefer non-generic operators
    // to generic operators, because the former is far more efficient to check.
    if (UDRE->getRefKind() != DeclRefKind::Ordinary) {
      std::stable_sort(ResultValues.begin(), ResultValues.end(),
                       [&](ValueDecl *x, ValueDecl *y) -> bool {
        auto xGeneric = x->getInterfaceType()->getAs<GenericFunctionType>();
        auto yGeneric = y->getInterfaceType()->getAs<GenericFunctionType>();
        if (static_cast<bool>(xGeneric) != static_cast<bool>(yGeneric)) {
          return xGeneric? false : true;
        }

        if (!xGeneric)
          return false;

        unsigned xDepth = xGeneric->getGenericSignature()->getMaxDepth();
        unsigned yDepth = yGeneric->getGenericSignature()->getMaxDepth();
        return xDepth < yDepth;
      });
    }

    // Filter out macro declarations without `#` if there are valid
    // non-macro results.
    if (llvm::any_of(ResultValues,
                     [](const ValueDecl *D) { return !isa<MacroDecl>(D); })) {
      ResultValues.erase(
          llvm::remove_if(ResultValues,
                          [](const ValueDecl *D) { return isa<MacroDecl>(D); }),
          ResultValues.end());

      // If there is only one type reference in results, let's handle
      // this in a special way.
      if (ResultValues.size() == 1 &&
          UDRE->getRefKind() == DeclRefKind::Ordinary &&
          isa<TypeDecl>(ResultValues.front())) {
        return buildTypeExpr(cast<TypeDecl>(ResultValues.front()));
      }
    }

    // If we are in an @_unsafeInheritExecutor context, swap out
    // declarations for their _unsafeInheritExecutor_ counterparts if they
    // exist.
    if (enclosingUnsafeInheritsExecutor(DC)) {
      introduceUnsafeInheritExecutorReplacements(
          DC, UDRE->getNameLoc().getBaseNameLoc(), ResultValues);
    }

    return buildRefExpr(ResultValues, DC, UDRE->getNameLoc(),
                        UDRE->isImplicit(), UDRE->getFunctionRefInfo());
  }

  ResultValues.clear();
  bool AllMemberRefs = true;
  ValueDecl *Base = nullptr;
  DeclContext *BaseDC = nullptr;
  for (auto Result : Lookup) {
    auto ThisBase = Result.getBaseDecl();

    // Track the base for member declarations.
    if (ThisBase && !isa<ModuleDecl>(ThisBase)) {
      auto Value = Result.getValueDecl();
      ResultValues.push_back(Value);
      if (Base && ThisBase != Base) {
        AllMemberRefs = false;
        break;
      }

      Base = ThisBase;
      BaseDC = Result.getDeclContext();
      continue;
    }

    AllMemberRefs = false;
    break;
  }

  if (AllMemberRefs) {
    Expr *BaseExpr;
    if (auto PD = dyn_cast<ProtocolDecl>(Base)) {
      auto selfParam = PD->getGenericParams()->getParams().front();
      BaseExpr = TypeExpr::createImplicitForDecl(
          UDRE->getNameLoc(), selfParam,
          /*DC*/ nullptr,
          DC->mapTypeIntoContext(selfParam->getInterfaceType()));
    } else if (auto NTD = dyn_cast<NominalTypeDecl>(Base)) {
      BaseExpr = TypeExpr::createImplicitForDecl(
          UDRE->getNameLoc(), NTD, BaseDC,
          DC->mapTypeIntoContext(NTD->getInterfaceType()));
    } else {
      BaseExpr = new (Context) DeclRefExpr(Base, UDRE->getNameLoc(),
                                           /*Implicit=*/true);
    }

    auto isInClosureContext = [&](ValueDecl *decl) -> bool {
      auto *DC = decl->getDeclContext();
      do {
        if (dyn_cast<ClosureExpr>(DC))
          return true;
      } while ((DC = DC->getParent()));

      return false;
    };

    llvm::SmallVector<ValueDecl *, 4> outerAlternatives;
    (void)findNonMembers(Lookup.outerResults(), UDRE->getRefKind(),
                         /*breakOnMember=*/false, outerAlternatives,
                         /*isValid=*/[&](ValueDecl *choice) -> bool {
                           // Values that are defined in a closure
                           // that hasn't been type-checked yet,
                           // cannot be outer candidates.
                           if (isInClosureContext(choice)) {
                             return choice->hasInterfaceType() &&
                                    !choice->isInvalid();
                           }
                           return !choice->isInvalid();
                         });

    // Otherwise, form an UnresolvedDotExpr and sema will resolve it based on
    // type information.
    return new (Context) UnresolvedDotExpr(
        BaseExpr, SourceLoc(), Name, UDRE->getNameLoc(), UDRE->isImplicit(),
        Context.AllocateCopy(outerAlternatives));
  }
  
  // FIXME: If we reach this point, the program we're being handed is likely
  // very broken, but it's still conceivable that this may happen due to
  // invalid shadowed declarations.
  //
  // Make sure we emit a diagnostic, since returning an ErrorExpr without
  // producing one will break things downstream.
  Context.Diags.diagnose(Loc, diag::ambiguous_decl_ref, Name);
  for (auto Result : Lookup) {
    auto *Decl = Result.getValueDecl();
    Context.Diags.diagnose(Decl, diag::decl_declared_here, Decl);
  }
  return errorResult();
}

/// If an expression references 'self.init' or 'super.init' in an
/// initializer context, returns the implicit 'self' decl of the constructor.
/// Otherwise, return nil.
VarDecl *
TypeChecker::getSelfForInitDelegationInConstructor(DeclContext *DC,
                                                   UnresolvedDotExpr *ctorRef) {
  // If the reference isn't to a constructor, we're done.
  if (!ctorRef->getName().getBaseName().isConstructor())
    return nullptr;

  if (auto ctorContext =
          dyn_cast_or_null<ConstructorDecl>(DC->getInnermostMethodContext())) {
    auto nestedArg = ctorRef->getBase();
    if (auto inout = dyn_cast<InOutExpr>(nestedArg))
      nestedArg = inout->getSubExpr();
    if (nestedArg->isSuperExpr())
      return ctorContext->getImplicitSelfDecl();
    if (auto declRef = dyn_cast<DeclRefExpr>(nestedArg))
      if (declRef->getDecl()->getName() == DC->getASTContext().Id_self)
        return ctorContext->getImplicitSelfDecl();
  }
  return nullptr;
}

namespace {
/// Update a direct callee expression node that has a function reference kind
/// based on seeing a call to this callee.
template <typename E, typename = decltype(((E *)nullptr)->getFunctionRefInfo())>
void tryUpdateDirectCalleeImpl(E *callee, int) {
  callee->setFunctionRefInfo(
      callee->getFunctionRefInfo().addingApplicationLevel());
}

/// Version of tryUpdateDirectCalleeImpl for when the callee
/// expression type doesn't carry a reference.
template <typename E>
void tryUpdateDirectCalleeImpl(E *callee, ...) {}

/// The given expression is the direct callee of a call expression; mark it to
/// indicate that it has been called.
void markDirectCallee(Expr *callee) {
  while (true) {
    // Look through identity expressions.
    if (auto identity = dyn_cast<IdentityExpr>(callee)) {
      callee = identity->getSubExpr();
      continue;
    }

    // Look through unresolved 'specialize' expressions.
    if (auto specialize = dyn_cast<UnresolvedSpecializeExpr>(callee)) {
      callee = specialize->getSubExpr();
      continue;
    }

    // Look through optional binding.
    if (auto bindOptional = dyn_cast<BindOptionalExpr>(callee)) {
      callee = bindOptional->getSubExpr();
      continue;
    }

    // Look through forced binding.
    if (auto force = dyn_cast<ForceValueExpr>(callee)) {
      callee = force->getSubExpr();
      continue;
    }

    // Calls compose.
    if (auto call = dyn_cast<CallExpr>(callee)) {
      callee = call->getFn();
      continue;
    }

    // We're done.
    break;
  }

  // Cast the callee to its most-specific class, then try to perform an
  // update. If the expression node has a declaration reference in it, the
  // update will succeed. Otherwise, we're done propagating.
  switch (callee->getKind()) {
#define EXPR(Id, Parent)                                                       \
  case ExprKind::Id:                                                           \
    tryUpdateDirectCalleeImpl(cast<Id##Expr>(callee), 0);                      \
    break;
#include "swift/AST/ExprNodes.def"
  }
}

class PreCheckTarget final : public ASTWalker {
  ASTContext &Ctx;
  DeclContext *DC;

  /// A stack of expressions being walked, used to determine where to
  /// insert RebindSelfInConstructorExpr nodes.
  llvm::SmallVector<Expr *, 8> ExprStack;

  /// The 'self' variable to use when rebinding 'self' in a constructor.
  VarDecl *UnresolvedCtorSelf = nullptr;

  /// The expression that will be wrapped by a RebindSelfInConstructorExpr
  /// node when visited.
  Expr *UnresolvedCtorRebindTarget = nullptr;

  /// Keep track of acceptable DiscardAssignmentExpr's.
  llvm::SmallPtrSet<DiscardAssignmentExpr *, 2> CorrectDiscardAssignmentExprs;

  /// Keep track of any out-of-place SingleValueStmtExprs. We populate this as
  /// we encounter SingleValueStmtExprs, and erase them as we walk up to a
  /// valid parent in the post walk.
  llvm::SetVector<SingleValueStmtExpr *> OutOfPlaceSingleValueStmtExprs;

  /// Simplify expressions which are type sugar productions that got parsed
  /// as expressions due to the parser not knowing which identifiers are
  /// type names.
  TypeExpr *simplifyTypeExpr(Expr *E);

  /// Simplify unresolved dot expressions which are nested type productions.
  TypeExpr *simplifyNestedTypeExpr(UnresolvedDotExpr *UDE);

  TypeExpr *simplifyUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *USE);

  /// Simplify a key path expression into a canonical form.
  void resolveKeyPathExpr(KeyPathExpr *KPE);

  /// Simplify constructs like `UInt32(1)` into `1 as UInt32` if
  /// the type conforms to the expected literal protocol.
  ///
  /// \returns Either a transformed expression, or `ErrorExpr` upon type
  /// resolution failure, or `nullptr` if transformation is not applicable.
  Expr *simplifyTypeConstructionWithLiteralArg(Expr *E);

  /// Pull some operator expressions into the optional chain.
  OptionalEvaluationExpr *hoistOptionalEvaluationExprIfNeeded(Expr *E);

  /// Wrap an unresolved member or optional bind chain in an
  /// UnresolvedMemberChainResultExpr or OptionalEvaluationExpr respectively.
  Expr *wrapMemberChainIfNeeded(Expr *E);

  /// Whether the given expression "looks like" a (possibly sugared) type. For
  /// example, `(foo, bar)` "looks like" a type, but `foo + bar` does not.
  bool exprLooksLikeAType(Expr *expr);

  /// Whether the current expression \p E is in a context that might turn out
  /// to be a \c TypeExpr after \c simplifyTypeExpr is called up the tree.
  /// This function allows us to make better guesses about whether invalid
  /// uses of '_' were "supposed" to be \c DiscardAssignmentExprs or patterns,
  /// which results in better diagnostics after type checking.
  bool possiblyInTypeContext(Expr *E);

  /// Whether we can simplify the given discard assignment expr. Not possible
  /// if it's been marked "valid" or if the current state of the AST disallows
  /// such simplification (see \c canSimplifyPlaceholderTypes above).
  bool canSimplifyDiscardAssignmentExpr(DiscardAssignmentExpr *DAE);

  /// In Swift < 5, diagnose and correct invalid multi-argument or
  /// argument-labeled interpolations. Returns \c true if the AST walk should
  /// continue, or \c false if it should be aborted.
  bool correctInterpolationIfStrange(InterpolatedStringLiteralExpr *ISLE);

  /// Scout out the specified destination of an AssignExpr to recursively
  /// identify DiscardAssignmentExpr in legal places.  We can only allow them
  /// in simple pattern-like expressions, so we reject anything complex here.
  void markAcceptableDiscardExprs(Expr *E);

  /// Check and diagnose an invalid SingleValueStmtExpr.
  void checkSingleValueStmtExpr(SingleValueStmtExpr *SVE);

  /// Diagnose any SingleValueStmtExprs in an unsupported position.
  void
  diagnoseOutOfPlaceSingleValueStmtExprs(const SyntacticElementTarget &target);

  /// Mark a given expression as a valid position for a SingleValueStmtExpr.
  void markValidSingleValueStmt(Expr *E);

  /// For the given expr, mark any valid SingleValueStmtExpr children.
  void markAnyValidSingleValueStmts(Expr *E);

  /// For the given statement, mark any valid SingleValueStmtExpr children.
  void markAnyValidSingleValueStmts(Stmt *S);

  PreCheckTarget(DeclContext *dc) : Ctx(dc->getASTContext()), DC(dc) {}

public:
  static std::optional<SyntacticElementTarget>
  check(const SyntacticElementTarget &target) {
    PreCheckTarget checker(target.getDeclContext());
    auto newTarget = target.walk(checker);
    if (!newTarget)
      return std::nullopt;
    // Diagnose any remaining out-of-place SingleValueStmtExprs.
    checker.diagnoseOutOfPlaceSingleValueStmtExprs(*newTarget);
    return *newTarget;
  }

  ASTContext &getASTContext() const { return Ctx; }

  bool walkToClosureExprPre(ClosureExpr *expr, ParentTy &parent);

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::Arguments;
  }

  VarDecl *getImplicitSelfDeclForSuperContext(SourceLoc Loc);

  PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
    auto &diags = Ctx.Diags;

    // Fold sequence expressions.
    if (auto *seqExpr = dyn_cast<SequenceExpr>(expr)) {
      auto result = TypeChecker::foldSequence(seqExpr, DC);
      result = result->walk(*this);
      if (!result)
        return Action::Stop();
      // Already walked.
      return Action::SkipNode(result);
    }

    // FIXME(diagnostics): `InOutType` could appear here as a result
    // of successful re-typecheck of the one of the sub-expressions e.g.
    // `let _: Int = { (s: inout S) in s.bar() }`. On the first
    // attempt to type-check whole expression `s.bar()` - is going
    // to have a base which points directly to declaration of `S`.
    // But when diagnostics attempts to type-check `s.bar()` standalone
    // its base would be transformed into `InOutExpr -> DeclRefExr`,
    // and `InOutType` is going to be recorded in constraint system.
    // One possible way to fix this (if diagnostics still use typecheck)
    // might be to make it so self is not wrapped into `InOutExpr`
    // but instead used as @lvalue type in some case of mutable members.
    if (!expr->isImplicit()) {
      if (isa<MemberRefExpr>(expr) || isa<DynamicMemberRefExpr>(expr)) {
        auto *LE = cast<LookupExpr>(expr);
        if (auto *IOE = dyn_cast<InOutExpr>(LE->getBase()))
          LE->setBase(IOE->getSubExpr());
      }

      if (auto *DSCE = dyn_cast<DotSyntaxCallExpr>(expr)) {
        if (auto *IOE = dyn_cast<InOutExpr>(DSCE->getBase()))
          DSCE->setBase(IOE->getSubExpr());
      }
    }

    // Local function used to finish up processing before returning. Every
    // return site should call through here.
    auto finish = [&](bool recursive, Expr *expr) -> PreWalkResult<Expr *> {
      if (!expr)
        return Action::Stop();

      // If we're going to recurse, record this expression on the stack.
      if (recursive)
        ExprStack.push_back(expr);

      return Action::VisitNodeIf(recursive, expr);
    };

    // Resolve 'super' references.
    if (auto *superRef = dyn_cast<SuperRefExpr>(expr)) {
      auto loc = superRef->getLoc();

      auto *selfDecl = getImplicitSelfDeclForSuperContext(loc);
      if (selfDecl == nullptr)
        return finish(false, new (Ctx) ErrorExpr(loc));

      superRef->setSelf(selfDecl);

      const bool isValidSuper = [&]() -> bool {
        auto *parentExpr = Parent.getAsExpr();
        if (!parentExpr) {
          return false;
        }

        if (isa<UnresolvedDotExpr>(parentExpr) ||
            isa<MemberRefExpr>(parentExpr)) {
          return true;
        } else if (auto *SE = dyn_cast<SubscriptExpr>(parentExpr)) {
          // 'super[]' is valid, but 'x[super]' is not.
          return superRef == SE->getBase();
        }

        return false;
      }();

      // NB: This is done along the happy path because presenting this error
      // in a context where 'super' is not legal to begin with is not helpful.
      if (!isValidSuper) {
        // Diagnose and keep going. It is important for source tooling such
        // as code completion that Sema is able to provide type information
        // for 'super' in arbitrary positions inside expressions.
        diags.diagnose(loc, diag::super_invalid_parent_expr);
      }

      return finish(true, superRef);
    }

    // For closures, type-check the patterns and result type as written,
    // but do not walk into the body. That will be type-checked after
    // we've determine the complete function type.
    if (auto closure = dyn_cast<ClosureExpr>(expr))
      return finish(walkToClosureExprPre(closure, Parent), expr);

    if (auto *unresolved = dyn_cast<UnresolvedDeclRefExpr>(expr))
      return finish(true, TypeChecker::resolveDeclRefExpr(unresolved, DC));

    // Let's try to figure out if `InOutExpr` is out of place early
    // otherwise there is a risk of producing solutions which can't
    // be later applied to AST and would result in the crash in some
    // cases. Such expressions are only allowed in argument positions
    // of function/operator calls.
    if (isa<InOutExpr>(expr)) {
      // If this is an implicit `inout` expression we assume that
      // compiler knowns what it's doing.
      if (expr->isImplicit())
        return finish(true, expr);

      ArrayRef<Expr *> parents = ExprStack;
      auto takeNextParent = [&]() -> Expr * {
        if (parents.empty())
          return nullptr;

        auto parent = parents.back();
        parents = parents.drop_back();
        return parent;
      };
      if (auto *parent = takeNextParent()) {
        SourceLoc lastInnerParenLoc;
        // Unwrap to the outermost paren in the sequence.
        // e.g. `foo(((&bar))`
        while (auto *PE = dyn_cast<ParenExpr>(parent)) {
          auto nextParent = takeNextParent();
          if (!nextParent)
            break;

          lastInnerParenLoc = PE->getLParenLoc();
          parent = nextParent;
        }

        if (isa<ApplyExpr>(parent) || isa<UnresolvedMemberExpr>(parent)) {
          // If outermost paren is associated with a call or
          // a member reference, it might be valid to have `&`
          // before all of the parens.
          if (lastInnerParenLoc.isValid()) {
            auto diag = diags.diagnose(expr->getStartLoc(),
                                       diag::extraneous_address_of);
            diag.fixItExchange(expr->getLoc(), lastInnerParenLoc);
          }
          if (!parents.empty() && isa<KeyPathExpr>(parents[0]))
            diags.diagnose(expr->getStartLoc(),
                           diag::cannot_pass_inout_arg_to_keypath_method);
          return finish(true, expr);
        }

        if (isa<SubscriptExpr>(parent)) {
          diags.diagnose(expr->getStartLoc(),
                         diag::cannot_pass_inout_arg_to_subscript);
          return finish(false, nullptr);
        }
      }

      diags.diagnose(expr->getStartLoc(), diag::extraneous_address_of);
      return finish(false, nullptr);
    }

    if (auto *ISLE = dyn_cast<InterpolatedStringLiteralExpr>(expr)) {
      if (!correctInterpolationIfStrange(ISLE))
        return finish(false, nullptr);
    }

    if (auto *assignment = dyn_cast<AssignExpr>(expr))
      markAcceptableDiscardExprs(assignment->getDest());

    if (auto *SVE = dyn_cast<SingleValueStmtExpr>(expr))
      checkSingleValueStmtExpr(SVE);

    return finish(true, expr);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *expr) override {
    // Remove this expression from the stack.
    assert(ExprStack.back() == expr);
    ExprStack.pop_back();

    // Mark any valid SingleValueStmtExpr children.
    markAnyValidSingleValueStmts(expr);

    // Type check the type parameters in an UnresolvedSpecializeExpr.
    if (auto *us = dyn_cast<UnresolvedSpecializeExpr>(expr)) {
      if (auto *typeExpr = simplifyUnresolvedSpecializeExpr(us))
        return Action::Continue(typeExpr);
    }

    // Check whether this is standalone `self` in init accessor, which
    // is invalid.
    if (auto *DRE = dyn_cast<DeclRefExpr>(expr)) {
      if (auto *accessor = DC->getInnermostPropertyAccessorContext()) {
        if (accessor->isInitAccessor() &&
            accessor->getImplicitSelfDecl() == DRE->getDecl() &&
            !isa_and_nonnull<UnresolvedDotExpr>(Parent.getAsExpr())) {
          Ctx.Diags.diagnose(DRE->getLoc(),
                             diag::invalid_use_of_self_in_init_accessor);
          return Action::Continue(new (Ctx) ErrorExpr(DRE->getSourceRange()));
        }
      }
    }

    // If we're about to step out of a ClosureExpr, restore the DeclContext.
    if (auto *ce = dyn_cast<ClosureExpr>(expr)) {
      assert(DC == ce && "DeclContext imbalance");
      DC = ce->getParent();
    }

    if (auto *apply = dyn_cast<ApplyExpr>(expr)) {
      // Mark the direct callee as being a callee.
      markDirectCallee(apply->getFn());

      // A 'self.init' or 'super.init' application inside a constructor will
      // evaluate to void, with the initializer's result implicitly rebound
      // to 'self'. Recognize the unresolved constructor expression and
      // determine where to place the RebindSelfInConstructorExpr node.
      //
      // When updating this logic, also may need to also update
      // RebindSelfInConstructorExpr::getCalledConstructor.
      VarDecl *self = nullptr;
      if (auto *unresolvedDot =
              dyn_cast<UnresolvedDotExpr>(apply->getSemanticFn())) {
        self = TypeChecker::getSelfForInitDelegationInConstructor(
            DC, unresolvedDot);
      }

      if (self) {
        // Walk our ancestor expressions looking for the appropriate place
        // to insert the RebindSelfInConstructorExpr.
        Expr *target = apply;
        for (auto ancestor : llvm::reverse(ExprStack)) {
          if (isa<IdentityExpr>(ancestor) || isa<ForceValueExpr>(ancestor) ||
              isa<AnyTryExpr>(ancestor)) {
            target = ancestor;
            continue;
          }

          if (isa<RebindSelfInConstructorExpr>(ancestor)) {
            // If we already have a rebind, then we're re-typechecking an
            // expression and are done.
            target = nullptr;
          }

          // No other expression kinds are permitted.
          break;
        }

        // If we found a rebind target, note the insertion point.
        if (target) {
          UnresolvedCtorRebindTarget = target;
          UnresolvedCtorSelf = self;
        }
      }
    }

    auto &ctx = getASTContext();

    // If the expression we've found is the intended target of an
    // RebindSelfInConstructorExpr, wrap it in the
    // RebindSelfInConstructorExpr.
    if (expr == UnresolvedCtorRebindTarget) {
      expr = new (ctx) RebindSelfInConstructorExpr(expr, UnresolvedCtorSelf);
      UnresolvedCtorRebindTarget = nullptr;
      return Action::Continue(expr);
    }

    // Check if there are any BindOptionalExpr in the tree which
    // wrap DiscardAssignmentExpr, such situation corresponds to syntax
    // like - `_? = <value>`, since it doesn't really make
    // sense to have optional assignment to discarded LValue which can
    // never be optional, we can remove BOE from the tree and avoid
    // generating any of the unnecessary constraints.
    if (auto BOE = dyn_cast<BindOptionalExpr>(expr)) {
      if (auto DAE = dyn_cast<DiscardAssignmentExpr>(BOE->getSubExpr()))
        if (CorrectDiscardAssignmentExprs.count(DAE))
          return Action::Continue(DAE);
    }

    // If this is a sugared type that needs to be folded into a single
    // TypeExpr, do it.
    if (auto *simplified = simplifyTypeExpr(expr))
      return Action::Continue(simplified);

    // Diagnose a '_' that isn't on the immediate LHS of an assignment. We
    // skip diagnostics if we've explicitly marked the expression as valid.
    if (auto *DAE = dyn_cast<DiscardAssignmentExpr>(expr)) {
      if (!CorrectDiscardAssignmentExprs.count(DAE)) {
        ctx.Diags.diagnose(expr->getLoc(),
                           diag::discard_expr_outside_of_assignment);
        return Action::Stop();
      }
    }

    if (auto KPE = dyn_cast<KeyPathExpr>(expr)) {
      resolveKeyPathExpr(KPE);
      return Action::Continue(KPE);
    }

    if (auto *result = simplifyTypeConstructionWithLiteralArg(expr)) {
      if (isa<ErrorExpr>(result))
        return Action::Stop();

      return Action::Continue(result);
    }

    if (auto *OEE = hoistOptionalEvaluationExprIfNeeded(expr)) {
      return Action::Continue(OEE);
    }

    expr = wrapMemberChainIfNeeded(expr);

    return Action::Continue(expr);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *stmt) override {
    if (auto *RS = dyn_cast<ReturnStmt>(stmt)) {
      // Pre-check a return statement, which includes potentially turning it
      // into a FailStmt.
      auto &eval = Ctx.evaluator;
      auto *S =
          evaluateOrDefault(eval, PreCheckReturnStmtRequest{RS, DC}, nullptr);
      if (!S)
        return Action::Stop();

      return Action::Continue(S);
    }
    return Action::Continue(stmt);
  }

  PostWalkResult<Stmt *> walkToStmtPost(Stmt *S) override {
    markAnyValidSingleValueStmts(S);
    return Action::Continue(S);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    return Action::VisitChildrenIf(isa<PatternBindingDecl>(D));
  }

  PostWalkAction walkToDeclPost(Decl *D) override {
    // Mark any valid SingleValueStmtExprs for initializations.
    if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
      for (auto idx : range(PBD->getNumPatternEntries()))
        markValidSingleValueStmt(PBD->getInit(idx));
    }
    return Action::Continue();
  }

  PreWalkResult<Pattern *> walkToPatternPre(Pattern *pattern) override {
    // In general we can't walk into patterns due to the fact that we don't
    // currently resolve patterns until constraint generation, and therefore
    // shouldn't walk into any expressions that may turn into patterns.
    // One exception to this is if the parent is an expression. In that case,
    // we are type-checking an expression in an ExprPattern, meaning that
    // the pattern will already be resolved, and that we ought to e.g
    // diagnose any stray '_' expressions nested within it. This then also
    // means we should walk into any child pattern if we walked into the
    // parent pattern.
    return Action::VisitNodeIf(Parent.getAsExpr() || Parent.getAsPattern(),
                               pattern);
  }
};
} // end anonymous namespace

/// Perform prechecking of a ClosureExpr before we dive into it.  This returns
/// true when we want the body to be considered part of this larger expression.
bool PreCheckTarget::walkToClosureExprPre(ClosureExpr *closure,
                                          ParentTy &parent) {
  if (auto *expandedBody = closure->getExpandedBody()) {
    if (Parent.getAsExpr()) {
      // We cannot simply replace the body when closure i.e. is passed
      // as an argument to a call or is a source of an assignment
      // because the source range of the argument list would cross
      // buffer boundaries. One way to avoid that is to inject
      // elements into a new implicit brace statement with the original
      // source locations. Brace statement has to be implicit because its
      // elements are in a different buffer.
      auto sourceRange = closure->getSourceRange();
      closure->setBody(BraceStmt::create(getASTContext(), sourceRange.Start,
                                         expandedBody->getElements(),
                                         sourceRange.End,
                                         /*implicit=*/true));
    } else {
      closure->setBody(expandedBody);
    }
  }

  // Pre-check the closure body.
  (void)evaluateOrDefault(Ctx.evaluator, PreCheckClosureBodyRequest{closure},
                          nullptr);

  // Update the current DeclContext to be the closure we're about to
  // recurse into.
  assert((closure->getParent() == DC ||
          closure->getParent()->isChildContextOf(DC)) &&
         "Decl context isn't correct");
  DC = closure;
  return true;
}

void PreCheckTarget::markValidSingleValueStmt(Expr *E) {
  if (!E)
    return;

  if (auto *SVE = SingleValueStmtExpr::tryDigOutSingleValueStmtExpr(E))
    OutOfPlaceSingleValueStmtExprs.remove(SVE);
}

void PreCheckTarget::checkSingleValueStmtExpr(SingleValueStmtExpr *SVE) {
  // We add all SingleValueStmtExprs we see to the out-of-place list, then
  // erase them as we walk up to valid parents. We do this instead of populating
  // valid positions during the pre-walk to ensure we're looking at the AST
  // after e.g folding SequenceExprs.
  OutOfPlaceSingleValueStmtExprs.insert(SVE);

  // Diagnose invalid SingleValueStmtExprs. This should only happen for
  // expressions in positions that we didn't support prior to their introduction
  // (e.g assignment or *explicit* return).
  auto &Diags = Ctx.Diags;
  auto *S = SVE->getStmt();
  auto mayProduceSingleValue = S->mayProduceSingleValue(Ctx);
  switch (mayProduceSingleValue.getKind()) {
  case IsSingleValueStmtResult::Kind::Valid:
    break;
  case IsSingleValueStmtResult::Kind::UnterminatedBranches: {
    for (auto *branch : mayProduceSingleValue.getUnterminatedBranches()) {
      if (auto *BS = dyn_cast<BraceStmt>(branch)) {
        if (BS->empty()) {
          Diags.diagnose(branch->getStartLoc(),
                         diag::single_value_stmt_branch_empty, S->getKind());
          continue;
        }
      }
      // TODO: The wording of this diagnostic will need tweaking if either
      // implicit last expressions or 'then' statements are enabled by
      // default.
      Diags.diagnose(branch->getEndLoc(),
                     diag::single_value_stmt_branch_must_end_in_result,
                     S->getKind(), isa<SwitchStmt>(S));
    }
    break;
  }
  case IsSingleValueStmtResult::Kind::NonExhaustiveIf: {
    Diags.diagnose(S->getStartLoc(),
                   diag::if_expr_must_be_syntactically_exhaustive);
    break;
  }
  case IsSingleValueStmtResult::Kind::NonExhaustiveDoCatch: {
    Diags.diagnose(S->getStartLoc(),
                   diag::do_catch_expr_must_be_syntactically_exhaustive);
    break;
  }
  case IsSingleValueStmtResult::Kind::HasLabel: {
    // FIXME: We should offer a fix-it to remove (currently we don't track
    // the colon SourceLoc).
    auto label = cast<LabeledStmt>(S)->getLabelInfo();
    Diags.diagnose(label.Loc,
                   diag::single_value_stmt_must_be_unlabeled, S->getKind())
         .highlight(label.Loc);
    break;
  }
  case IsSingleValueStmtResult::Kind::InvalidJumps: {
    // Diagnose each invalid jump.
    for (auto *jump : mayProduceSingleValue.getInvalidJumps()) {
      Diags.diagnose(jump->getStartLoc(),
                     diag::cannot_jump_in_single_value_stmt,
                     jump->getKind(), S->getKind())
           .highlight(jump->getSourceRange());
    }
    break;
  }
  case IsSingleValueStmtResult::Kind::NoResult:
    // This is fine, we will have typed the expression as Void (we verify
    // as such in the ASTVerifier).
    break;
  case IsSingleValueStmtResult::Kind::CircularReference:
    // Already diagnosed.
    break;
  case IsSingleValueStmtResult::Kind::UnhandledStmt:
    break;
  }
}

void PreCheckTarget::markAnyValidSingleValueStmts(Expr *E) {
  auto findAssignment = [&]() -> AssignExpr * {
    // Don't consider assignments if we have a parent expression (as otherwise
    // this would be effectively allowing it in an arbitrary expression
    // position).
    if (Parent.getAsExpr())
      return nullptr;

    // Look through optional exprs, which are present for e.g x?.y = z, as
    // we wrap the entire assign in the optional evaluation of the destination.
    if (auto *OEE = dyn_cast<OptionalEvaluationExpr>(E)) {
      E = OEE->getSubExpr();
      while (auto *IIO = dyn_cast<InjectIntoOptionalExpr>(E))
        E = IIO->getSubExpr();
    }

    // Look through "unsafe" expressions.
    if (auto UE = dyn_cast<UnsafeExpr>(E))
      E = UE->getSubExpr();

    return dyn_cast<AssignExpr>(E);
  };

  if (auto *AE = findAssignment())
    markValidSingleValueStmt(AE->getSrc());
}

void PreCheckTarget::markAnyValidSingleValueStmts(Stmt *S) {
  // Valid in a return/throw/then.
  if (auto *RS = dyn_cast<ReturnStmt>(S)) {
    if (RS->hasResult())
      markValidSingleValueStmt(RS->getResult());
  }
  if (auto *TS = dyn_cast<ThrowStmt>(S))
    markValidSingleValueStmt(TS->getSubExpr());

  if (auto *TS = dyn_cast<ThenStmt>(S))
    markValidSingleValueStmt(TS->getResult());
}

void PreCheckTarget::diagnoseOutOfPlaceSingleValueStmtExprs(
    const SyntacticElementTarget &target) {
  // Top-level SingleValueStmtExprs are allowed in returns, throws, and
  // bindings.
  if (auto *E = target.getAsExpr()) {
    switch (target.getExprContextualTypePurpose()) {
    case CTP_ReturnStmt:
    case CTP_ThrowStmt:
    case CTP_Initialization:
      markValidSingleValueStmt(E);
      break;
    default:
      break;
    }
  }
  for (auto *SVE : OutOfPlaceSingleValueStmtExprs) {
    Ctx.Diags.diagnose(SVE->getLoc(), diag::single_value_stmt_out_of_place,
                       SVE->getStmt()->getKind());
  }
}

TypeExpr *PreCheckTarget::simplifyNestedTypeExpr(UnresolvedDotExpr *UDE) {
  if (!UDE->getName().isSimpleName() ||
      UDE->getName().isSpecial())
    return nullptr;

  auto Name = UDE->getName();
  auto NameLoc = UDE->getNameLoc().getBaseNameLoc();

  // Qualified type lookup with a module base is represented as a DeclRefExpr
  // and not a TypeExpr.
  auto handleNestedTypeLookup = [&](
      TypeDecl *TD, DeclNameLoc ParentNameLoc
    ) -> TypeExpr * {
      // See if the type has a member type with this name.
      auto Result = TypeChecker::lookupMemberType(
          DC, TD->getDeclaredInterfaceType(), Name,
          UDE->getLoc(), defaultMemberLookupOptions);

      // If there is no nested type with this name, we have a lookup of
      // a non-type member, so leave the expression as-is.
      if (Result.size() == 1) {
        return TypeExpr::createForMemberDecl(
            ParentNameLoc, TD, UDE->getNameLoc(), Result.front().Member);
      }

      return nullptr;
  };

  if (auto *DRE = dyn_cast<DeclRefExpr>(UDE->getBase())) {
    if (auto *TD = dyn_cast<TypeDecl>(DRE->getDecl()))
      return handleNestedTypeLookup(TD, DRE->getNameLoc());

    return nullptr;
  }

  // Determine whether there is exactly one type declaration, where all
  // other declarations are macros.
  if (auto *ODRE = dyn_cast<OverloadedDeclRefExpr>(UDE->getBase())) {
    TypeDecl *FoundTD = nullptr;
    for (auto *D : ODRE->getDecls()) {
      if (auto *TD = dyn_cast<TypeDecl>(D)) {
        if (FoundTD)
          return nullptr;

        FoundTD = TD;
        continue;
      }

        // Ignore macros; they can't have any nesting.
      if (isa<MacroDecl>(D))
        continue;

      // Anything else prevents folding.
      return nullptr;
    }

    if (FoundTD)
      return handleNestedTypeLookup(FoundTD, ODRE->getNameLoc());

    return nullptr;
  }

  auto *TyExpr = dyn_cast<TypeExpr>(UDE->getBase());
  if (!TyExpr)
    return nullptr;

  auto *InnerTypeRepr = TyExpr->getTypeRepr();
  if (!InnerTypeRepr)
    return nullptr;

  // Fold 'T.Protocol' into a protocol metatype.
  if (Name.isSimpleName(getASTContext().Id_Protocol)) {
    auto *NewTypeRepr =
        new (getASTContext()) ProtocolTypeRepr(InnerTypeRepr, NameLoc);
    return new (getASTContext()) TypeExpr(NewTypeRepr);
  }

  // Fold 'T.Type' into an existential metatype if 'T' is a protocol,
  // or an ordinary metatype otherwise.
  if (Name.isSimpleName(getASTContext().Id_Type)) {
    auto *NewTypeRepr =
        new (getASTContext()) MetatypeTypeRepr(InnerTypeRepr, NameLoc);
    return new (getASTContext()) TypeExpr(NewTypeRepr);
  }

  // Fold 'T.U' into a nested type.

  // Resolve the TypeRepr to get the base type for the lookup.
  TypeResolutionOptions options(TypeResolverContext::InExpression);
  // Pre-check always allows pack references during TypeExpr folding.
  // CSGen will diagnose cases that appear outside of pack expansion
  // expressions.
  options |= TypeResolutionFlags::AllowPackReferences;
  const auto BaseTy = TypeResolution::resolveContextualType(
      InnerTypeRepr, DC, options,
      [](auto unboundTy) {
        // FIXME: Don't let unbound generic types escape type resolution.
        // For now, just return the unbound generic type.
        return unboundTy;
      },
      // FIXME: Don't let placeholder types escape type resolution.
      // For now, just return the placeholder type.
      PlaceholderType::get,
      // TypeExpr pack elements are opened in CSGen.
      /*packElementOpener*/ nullptr);

  if (BaseTy->mayHaveMembers()) {
    // See if there is a member type with this name.
    auto Result = TypeChecker::lookupMemberType(DC, BaseTy, Name,
                                                UDE->getLoc(),
                                                defaultMemberLookupOptions);

    // If there is no nested type with this name, we have a lookup of
    // a non-type member, so leave the expression as-is.
    if (Result.size() == 1) {
      return TypeExpr::createForMemberDecl(InnerTypeRepr, UDE->getNameLoc(),
                                           Result.front().Member);
    }
  }

  return nullptr;
}

TypeExpr *PreCheckTarget::simplifyUnresolvedSpecializeExpr(
    UnresolvedSpecializeExpr *us) {
  // If this is a reference type a specialized type, form a TypeExpr.
  // The base should be a TypeExpr that we already resolved.
  if (auto *te = dyn_cast_or_null<TypeExpr>(us->getSubExpr())) {
    if (auto *declRefTR =
            dyn_cast_or_null<DeclRefTypeRepr>(te->getTypeRepr())) {
      return TypeExpr::createForSpecializedDecl(
          declRefTR, us->getUnresolvedParams(),
          SourceRange(us->getLAngleLoc(), us->getRAngleLoc()), getASTContext());
    }
  }

  return nullptr;
}

/// Whether the given expression "looks like" a (possibly sugared) type. For
/// example, `(foo, bar)` "looks like" a type, but `foo + bar` does not.
bool PreCheckTarget::exprLooksLikeAType(Expr *expr) {
  return isa<OptionalEvaluationExpr>(expr) ||
      isa<BindOptionalExpr>(expr) ||
      isa<ForceValueExpr>(expr) ||
      isa<ParenExpr>(expr) ||
      isa<ArrowExpr>(expr) ||
      isa<PackExpansionExpr>(expr) ||
      isa<PackElementExpr>(expr) ||
      isa<TupleExpr>(expr) ||
      (isa<ArrayExpr>(expr) &&
       cast<ArrayExpr>(expr)->getElements().size() == 1) ||
      (isa<DictionaryExpr>(expr) &&
       cast<DictionaryExpr>(expr)->getElements().size() == 1) ||
      getCompositionExpr(expr);
}

bool PreCheckTarget::possiblyInTypeContext(Expr *E) {
  // Walk back up the stack of parents looking for a valid type context.
  for (auto *ParentExpr : llvm::reverse(ExprStack)) {
    // We're considered to be in a type context if either:
    // - We have a valid parent for a TypeExpr, or
    // - The parent "looks like" a type (and is not a call arg), and we can
    //   reach a valid parent for a TypeExpr if we continue walking.
    if (ParentExpr->isValidParentOfTypeExpr(E))
      return true;

    if (!exprLooksLikeAType(ParentExpr))
      return false;

    E = ParentExpr;
  }
  return false;
}

/// Only allow simplification of a DiscardAssignmentExpr if it hasn't already
/// been explicitly marked as correct, and the current AST state allows it.
bool PreCheckTarget::canSimplifyDiscardAssignmentExpr(
    DiscardAssignmentExpr *DAE) {
  return !CorrectDiscardAssignmentExprs.count(DAE) &&
         possiblyInTypeContext(DAE);
}


/// In Swift < 5, diagnose and correct invalid multi-argument or
/// argument-labeled interpolations. Returns \c true if the AST walk should
/// continue, or \c false if it should be aborted.
bool PreCheckTarget::correctInterpolationIfStrange(
    InterpolatedStringLiteralExpr *ISLE) {
  // These expressions are valid in Swift 5+.
  if (getASTContext().isSwiftVersionAtLeast(5))
    return true;

  /// Diagnoses appendInterpolation(...) calls with multiple
  /// arguments or argument labels and corrects them.
  class StrangeInterpolationRewriter : public ASTWalker {
    ASTContext &Context;

  public:
    StrangeInterpolationRewriter(ASTContext &Ctx) : Context(Ctx) {}

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Expansion;
    }

    virtual PreWalkAction walkToDeclPre(Decl *D) override {
      // We don't want to look inside decls.
      return Action::SkipNode();
    }

    virtual PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
      // One InterpolatedStringLiteralExpr should never be nested inside
      // another except as a child of a CallExpr, and we don't recurse into
      // the children of CallExprs.
      assert(!isa<InterpolatedStringLiteralExpr>(E) &&
             "StrangeInterpolationRewriter found nested interpolation?");

      // We only care about CallExprs.
      if (!isa<CallExpr>(E))
        return Action::Continue(E);

      auto *call = cast<CallExpr>(E);
      auto *args = call->getArgs();

      auto lParen = args->getLParenLoc();
      auto rParen = args->getRParenLoc();

      if (auto callee = dyn_cast<UnresolvedDotExpr>(call->getFn())) {
        if (callee->getName().getBaseName() ==
            Context.Id_appendInterpolation) {

          std::optional<Argument> newArg;
          if (args->size() > 1) {
            auto *secondArg = args->get(1).getExpr();
            Context.Diags
                .diagnose(secondArg->getLoc(),
                          diag::string_interpolation_list_changing)
                .highlightChars(secondArg->getLoc(), rParen);
            Context.Diags
                .diagnose(secondArg->getLoc(),
                          diag::string_interpolation_list_insert_parens)
                .fixItInsertAfter(lParen, "(")
                .fixItInsert(rParen, ")");

            // Make sure we don't have an inout arg somewhere, as that's
            // invalid even with the compatibility fix.
            for (auto arg : *args) {
              if (arg.isInOut()) {
                Context.Diags.diagnose(arg.getExpr()->getStartLoc(),
                                       diag::extraneous_address_of);
                return Action::Stop();
              }
            }

            // Form a new argument tuple from the argument list.
            auto *packed = args->packIntoImplicitTupleOrParen(Context);
            newArg = Argument::unlabeled(packed);
          } else if (args->size() == 1 &&
                     args->front().getLabel() != Identifier()) {
            // Form a new argument that drops the label.
            auto *argExpr = args->front().getExpr();
            newArg = Argument::unlabeled(argExpr);

            SourceLoc argLabelLoc = args->front().getLabelLoc(),
                      argLoc = argExpr->getStartLoc();

            Context.Diags
                .diagnose(argLabelLoc,
                          diag::string_interpolation_label_changing)
                .highlightChars(argLabelLoc, argLoc);
            Context.Diags
                .diagnose(argLabelLoc,
                          diag::string_interpolation_remove_label,
                          args->front().getLabel())
                .fixItRemoveChars(argLabelLoc, argLoc);
          }

          // If newArg is no longer null, we need to build a new
          // appendInterpolation(_:) call that takes it to replace the bad
          // appendInterpolation(...) call.
          if (newArg) {
            auto newCallee = new (Context) UnresolvedDotExpr(
                callee->getBase(), /*dotloc=*/SourceLoc(),
                DeclNameRef(Context.Id_appendInterpolation),
                /*nameloc=*/DeclNameLoc(), /*Implicit=*/true);

            auto *newArgList =
                ArgumentList::create(Context, lParen, {*newArg}, rParen,
                                     /*trailingClosureIdx*/ std::nullopt,
                                     /*implicit*/ false);
            E = CallExpr::create(Context, newCallee, newArgList,
                                 /*implicit=*/false);
          }
        }
      }

      // There is never a CallExpr between an InterpolatedStringLiteralExpr
      // and an un-typechecked appendInterpolation(...) call, so whether we
      // changed E or not, we don't need to recurse any deeper.
      return Action::SkipNode(E);
    }
  };

  return ISLE->getAppendingExpr()->walk(
      StrangeInterpolationRewriter(getASTContext()));
}

/// Scout out the specified destination of an AssignExpr to recursively
/// identify DiscardAssignmentExpr in legal places.  We can only allow them
/// in simple pattern-like expressions, so we reject anything complex here.
void PreCheckTarget::markAcceptableDiscardExprs(Expr *E) {
  if (!E) return;

  if (auto *PE = dyn_cast<ParenExpr>(E))
    return markAcceptableDiscardExprs(PE->getSubExpr());
  if (auto *TE = dyn_cast<TupleExpr>(E)) {
    for (auto &elt : TE->getElements())
      markAcceptableDiscardExprs(elt);
    return;
  }
  if (auto *BOE = dyn_cast<BindOptionalExpr>(E))
    return markAcceptableDiscardExprs(BOE->getSubExpr());
  if (auto *DAE = dyn_cast<DiscardAssignmentExpr>(E))
    CorrectDiscardAssignmentExprs.insert(DAE);

  // Otherwise, we can't support this.
}

VarDecl *PreCheckTarget::getImplicitSelfDeclForSuperContext(SourceLoc Loc) {
  auto *methodContext = DC->getInnermostMethodContext();

  if (auto *typeContext = DC->getInnermostTypeContext()) {
    auto *nominal = typeContext->getSelfNominalTypeDecl();
    auto *classDecl = dyn_cast<ClassDecl>(nominal);

    if (!classDecl) {
      Ctx.Diags.diagnose(Loc, diag::super_in_nonclass_type, nominal);
      return nullptr;
    } else if (!methodContext) {
      Ctx.Diags.diagnose(Loc, diag::super_invalid_context);
      return nullptr;
    } else if (!classDecl->hasSuperclass()) {
      Ctx.Diags.diagnose(
          Loc, diag::super_no_superclass,
          /*isExtension*/ isa<ExtensionDecl>(typeContext->getAsDecl()),
          classDecl);
      return nullptr;
    }
  } else {
    Ctx.Diags.diagnose(Loc, diag::super_invalid_context);
    return nullptr;
  }

  // Do an actual lookup for 'self' in case it shows up in a capture list.
  auto *methodSelf = methodContext->getImplicitSelfDecl();
  auto *lookupSelf = ASTScope::lookupSingleLocalDecl(DC->getParentSourceFile(),
                                                     Ctx.Id_self, Loc);
  if (lookupSelf && lookupSelf != methodSelf) {
    // FIXME: This is the wrong diagnostic for if someone manually declares a
    // variable named 'self' using backticks.
    Ctx.Diags.diagnose(Loc, diag::super_in_closure_with_capture);
    Ctx.Diags.diagnose(lookupSelf->getLoc(),
                       diag::super_in_closure_with_capture_here);
    return nullptr;
  }

  return methodSelf;
}

/// Check whether this expression refers to the ~ operator.
static bool isTildeOperator(Expr *expr) {
  auto nameMatches = [&](DeclName name) {
    return name.isOperator() && name.getBaseName().getIdentifier().is("~");
  };

  if (auto overload = dyn_cast<OverloadedDeclRefExpr>(expr)) {
    return llvm::any_of(overload->getDecls(), [=](auto *decl) -> bool {
      return nameMatches(decl->getName());
    });
  }

  if (auto unresolved = dyn_cast<UnresolvedDeclRefExpr>(expr)) {
    return nameMatches(unresolved->getName().getFullName());
  }

  if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
    return nameMatches(declRef->getDecl()->getName());
  }

  return false;
}

/// Simplify expressions which are type sugar productions that got parsed
/// as expressions due to the parser not knowing which identifiers are
/// type names.
TypeExpr *PreCheckTarget::simplifyTypeExpr(Expr *E) {
  // If it's already a type expression, return it.
  if (auto typeExpr = dyn_cast<TypeExpr>(E))
    return typeExpr;

  // Fold member types.
  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(E)) {
    return simplifyNestedTypeExpr(UDE);
  }

  // Fold '_' into a placeholder type, if we're allowed.
  if (auto *DAE = dyn_cast<DiscardAssignmentExpr>(E)) {
    if (canSimplifyDiscardAssignmentExpr(DAE)) {
      auto *placeholderRepr =
          new (Ctx) PlaceholderTypeRepr(DAE->getLoc());
      return new (Ctx) TypeExpr(placeholderRepr);
    }
  }

  // Fold T? into an optional type when T is a TypeExpr.
  if (isa<OptionalEvaluationExpr>(E) || isa<BindOptionalExpr>(E)) {
    TypeExpr *TyExpr;
    SourceLoc QuestionLoc;
    if (auto *OOE = dyn_cast<OptionalEvaluationExpr>(E)) {
      TyExpr = dyn_cast<TypeExpr>(OOE->getSubExpr());
      QuestionLoc = OOE->getLoc();
    } else {
      TyExpr = dyn_cast<TypeExpr>(cast<BindOptionalExpr>(E)->getSubExpr());
      QuestionLoc = cast<BindOptionalExpr>(E)->getQuestionLoc();
    }
    if (!TyExpr) return nullptr;

    auto *InnerTypeRepr = TyExpr->getTypeRepr();
    assert(!TyExpr->isImplicit() && InnerTypeRepr &&
           "This doesn't work on implicit TypeExpr's, "
           "the TypeExpr should have been built correctly in the first place");
    
    // The optional evaluation is passed through.
    if (isa<OptionalEvaluationExpr>(E))
      return TyExpr;

    auto *NewTypeRepr =
        new (Ctx) OptionalTypeRepr(InnerTypeRepr, QuestionLoc);
    return new (Ctx) TypeExpr(NewTypeRepr);
  }

  // Fold T! into an IUO type when T is a TypeExpr.
  if (auto *FVE = dyn_cast<ForceValueExpr>(E)) {
    auto *TyExpr = dyn_cast<TypeExpr>(FVE->getSubExpr());
    if (!TyExpr) return nullptr;

    auto *InnerTypeRepr = TyExpr->getTypeRepr();
    assert(!TyExpr->isImplicit() && InnerTypeRepr &&
           "This doesn't work on implicit TypeExpr's, "
           "the TypeExpr should have been built correctly in the first place");

    auto *NewTypeRepr = new (Ctx)
        ImplicitlyUnwrappedOptionalTypeRepr(InnerTypeRepr,
                                            FVE->getExclaimLoc());
    return new (Ctx) TypeExpr(NewTypeRepr);
  }

  // Fold (T) into a type T with parens around it.
  if (auto *PE = dyn_cast<ParenExpr>(E)) {
    auto *TyExpr = dyn_cast<TypeExpr>(PE->getSubExpr());
    if (!TyExpr) return nullptr;
    
    TupleTypeReprElement InnerTypeRepr[] = { TyExpr->getTypeRepr() };
    assert(!TyExpr->isImplicit() && InnerTypeRepr[0].Type &&
           "SubscriptExpr doesn't work on implicit TypeExpr's, "
           "the TypeExpr should have been built correctly in the first place");

    auto *NewTypeRepr = TupleTypeRepr::create(Ctx, InnerTypeRepr,
                                              PE->getSourceRange());
    return new (Ctx) TypeExpr(NewTypeRepr);
  }
  
  // Fold a tuple expr like (T1,T2) into a tuple type (T1,T2).
  if (auto *TE = dyn_cast<TupleExpr>(E)) {
    // FIXME: Decide what to do about ().  It could be a type or an expr.
    if (TE->getNumElements() == 0)
      return nullptr;

    SmallVector<TupleTypeReprElement, 4> Elts;
    unsigned EltNo = 0;
    for (auto Elt : TE->getElements()) {
      // Try to simplify the element, e.g. to fold PackExpansionExprs
      // into TypeExprs.
      if (auto simplified = simplifyTypeExpr(Elt))
        Elt = simplified;

      auto *eltTE = dyn_cast<TypeExpr>(Elt);
      if (!eltTE) return nullptr;
      TupleTypeReprElement elt;
      assert(eltTE->getTypeRepr() && !eltTE->isImplicit() &&
             "This doesn't work on implicit TypeExpr's, the "
             "TypeExpr should have been built correctly in the first place");

      // If the tuple element has a label, propagate it.
      elt.Type = eltTE->getTypeRepr();
      elt.Name = TE->getElementName(EltNo);
      elt.NameLoc = TE->getElementNameLoc(EltNo);

      Elts.push_back(elt);
      ++EltNo;
    }
    auto *NewTypeRepr = TupleTypeRepr::create(
        Ctx, Elts, TE->getSourceRange());
    return new (Ctx) TypeExpr(NewTypeRepr);
  }
  

  // Fold [T] into an array type.
  if (auto *AE = dyn_cast<ArrayExpr>(E)) {
    if (AE->getElements().size() != 1)
      return nullptr;

    auto *TyExpr = dyn_cast<TypeExpr>(AE->getElement(0));
    if (!TyExpr)
      return nullptr;

    auto *NewTypeRepr = new (Ctx)
        ArrayTypeRepr(TyExpr->getTypeRepr(),
                      SourceRange(AE->getLBracketLoc(), AE->getRBracketLoc()));
    return new (Ctx) TypeExpr(NewTypeRepr);
  }

  // Fold [K : V] into a dictionary type.
  if (auto *DE = dyn_cast<DictionaryExpr>(E)) {
    if (DE->getElements().size() != 1)
      return nullptr;

    TypeRepr *keyTypeRepr, *valueTypeRepr;
    
    if (auto EltTuple = dyn_cast<TupleExpr>(DE->getElement(0))) {
      auto *KeyTyExpr = dyn_cast<TypeExpr>(EltTuple->getElement(0));
      if (!KeyTyExpr)
        return nullptr;

      auto *ValueTyExpr = dyn_cast<TypeExpr>(EltTuple->getElement(1));
      if (!ValueTyExpr)
        return nullptr;
     
      keyTypeRepr = KeyTyExpr->getTypeRepr();
      valueTypeRepr = ValueTyExpr->getTypeRepr();
    } else {
      auto *TE = dyn_cast<TypeExpr>(DE->getElement(0));
      if (!TE) return nullptr;
      
      auto *TRE = dyn_cast_or_null<TupleTypeRepr>(TE->getTypeRepr());
      while (TRE->isParenType()) {
        TRE = dyn_cast_or_null<TupleTypeRepr>(TRE->getElementType(0));
      }

      assert(TRE->getElements().size() == 2);
      keyTypeRepr = TRE->getElementType(0);
      valueTypeRepr = TRE->getElementType(1);
    }

    auto *NewTypeRepr = new (Ctx) DictionaryTypeRepr(
        keyTypeRepr, valueTypeRepr,
        /*FIXME:colonLoc=*/SourceLoc(),
        SourceRange(DE->getLBracketLoc(), DE->getRBracketLoc()));
    return new (Ctx) TypeExpr(NewTypeRepr);
  }

  // Reinterpret arrow expr T1 -> T2 as function type.
  // FIXME: support 'inout', etc.
  if (auto *AE = dyn_cast<ArrowExpr>(E)) {
    if (!AE->isFolded()) return nullptr;

    auto diagnoseMissingParens = [](ASTContext &ctx, TypeRepr *tyR) {
      if (tyR->isSimpleUnqualifiedIdentifier(ctx.Id_Void)) {
        ctx.Diags.diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
            .fixItReplace(tyR->getStartLoc(), "()");
      } else {
        ctx.Diags.diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
            .highlight(tyR->getSourceRange())
            .fixItInsert(tyR->getStartLoc(), "(")
            .fixItInsertAfter(tyR->getEndLoc(), ")");
      }
    };

    auto extractInputTypeRepr = [&](Expr *E) -> TupleTypeRepr * {
      if (!E)
        return nullptr;
      if (auto *TyE = dyn_cast<TypeExpr>(E)) {
        auto ArgRepr = TyE->getTypeRepr();
        if (auto *TTyRepr = dyn_cast<TupleTypeRepr>(ArgRepr))
          return TTyRepr;
        diagnoseMissingParens(Ctx, ArgRepr);
        return TupleTypeRepr::create(Ctx, {ArgRepr}, ArgRepr->getSourceRange());
      }
      if (auto *TE = dyn_cast<TupleExpr>(E))
        if (TE->getNumElements() == 0)
          return TupleTypeRepr::createEmpty(Ctx, TE->getSourceRange());

      // When simplifying a type expr like "(P1 & P2) -> (P3 & P4) -> Int",
      // it may have been folded at the same time; recursively simplify it.
      if (auto ArgsTypeExpr = simplifyTypeExpr(E)) {
        auto ArgRepr = ArgsTypeExpr->getTypeRepr();
        if (auto *TTyRepr = dyn_cast<TupleTypeRepr>(ArgRepr))
          return TTyRepr;
        diagnoseMissingParens(Ctx, ArgRepr);
        return TupleTypeRepr::create(Ctx, {ArgRepr}, ArgRepr->getSourceRange());
      }
      return nullptr;
    };

    auto extractTypeRepr = [&](Expr *E) -> TypeRepr * {
      if (!E)
        return nullptr;
      if (auto *TyE = dyn_cast<TypeExpr>(E))
        return TyE->getTypeRepr();
      if (auto *TE = dyn_cast<TupleExpr>(E))
        if (TE->getNumElements() == 0)
          return TupleTypeRepr::createEmpty(Ctx, TE->getSourceRange());

      // When simplifying a type expr like "P1 & P2 -> P3 & P4 -> Int",
      // it may have been folded at the same time; recursively simplify it.
      if (auto ArgsTypeExpr = simplifyTypeExpr(E))
        return ArgsTypeExpr->getTypeRepr();
      return nullptr;
    };

    TupleTypeRepr *ArgsTypeRepr = extractInputTypeRepr(AE->getArgsExpr());
    if (!ArgsTypeRepr) {
      Ctx.Diags.diagnose(AE->getArgsExpr()->getLoc(),
                         diag::expected_type_before_arrow);
      auto ArgRange = AE->getArgsExpr()->getSourceRange();
      auto ErrRepr = ErrorTypeRepr::create(Ctx, ArgRange);
      ArgsTypeRepr =
          TupleTypeRepr::create(Ctx, {ErrRepr}, ArgRange);
    }

    TypeRepr *ThrownTypeRepr = nullptr;
    if (auto thrownTypeExpr = AE->getThrownTypeExpr()) {
      ThrownTypeRepr = extractTypeRepr(thrownTypeExpr);
      assert(ThrownTypeRepr && "Parser ensures that this never fails");
    }

    TypeRepr *ResultTypeRepr = extractTypeRepr(AE->getResultExpr());
    if (!ResultTypeRepr) {
      Ctx.Diags.diagnose(AE->getResultExpr()->getLoc(),
                         diag::expected_type_after_arrow);
      ResultTypeRepr =
          ErrorTypeRepr::create(Ctx, AE->getResultExpr()->getSourceRange());
    }

    auto NewTypeRepr = new (Ctx)
        FunctionTypeRepr(nullptr, ArgsTypeRepr, AE->getAsyncLoc(),
                         AE->getThrowsLoc(), ThrownTypeRepr, AE->getArrowLoc(),
                         ResultTypeRepr);
    return new (Ctx) TypeExpr(NewTypeRepr);
  }
  
  // Fold '~P' into a composition type.
  if (auto *unaryExpr = dyn_cast<PrefixUnaryExpr>(E)) {
    if (isTildeOperator(unaryExpr->getFn())) {
      if (auto operand = simplifyTypeExpr(unaryExpr->getOperand())) {
        auto inverseTypeRepr = new (Ctx) InverseTypeRepr(
            unaryExpr->getLoc(), operand->getTypeRepr());
        return new (Ctx) TypeExpr(inverseTypeRepr);
      }
    }
  }

  // Fold 'P & Q' into a composition type
  if (auto *binaryExpr = getCompositionExpr(E)) {
    // The protocols we are composing
    SmallVector<TypeRepr *, 4> Types;

    auto lhsExpr = binaryExpr->getLHS();
    if (auto *lhs = dyn_cast<TypeExpr>(lhsExpr)) {
      Types.push_back(lhs->getTypeRepr());
    } else if (isa<BinaryExpr>(lhsExpr)) {
      // If the lhs is another binary expression, we have a multi element
      // composition: 'A & B & C' is parsed as ((A & B) & C); we get
      // the protocols from the lhs here
      if (auto expr = simplifyTypeExpr(lhsExpr))
        if (auto *repr = dyn_cast<CompositionTypeRepr>(expr->getTypeRepr()))
          // add the protocols to our list
          for (auto proto : repr->getTypes())
            Types.push_back(proto);
        else
          return nullptr;
      else
        return nullptr;
    } else
      return nullptr;

    // Add the rhs which is just a TypeExpr
    auto *rhs = dyn_cast<TypeExpr>(binaryExpr->getRHS());
    if (!rhs) return nullptr;
    Types.push_back(rhs->getTypeRepr());

    auto CompRepr = CompositionTypeRepr::create(Ctx, Types,
                                                lhsExpr->getStartLoc(),
                                                binaryExpr->getSourceRange());
    return new (Ctx) TypeExpr(CompRepr);
  }

  // Fold a pack expansion expr into a TypeExpr when the pattern is a TypeExpr.
  if (auto *expansion = dyn_cast<PackExpansionExpr>(E)) {
    if (auto *pattern = dyn_cast<TypeExpr>(expansion->getPatternExpr())) {
      auto *repr = new (Ctx) PackExpansionTypeRepr(expansion->getStartLoc(),
                                                   pattern->getTypeRepr());
      return new (Ctx) TypeExpr(repr);
    }
  }

  // Fold a PackElementExpr into a TypeExpr when the element is a TypeExpr
  if (auto *element = dyn_cast<PackElementExpr>(E)) {
    if (auto *refExpr = dyn_cast<TypeExpr>(element->getPackRefExpr())) {
      auto *repr = new (Ctx) PackElementTypeRepr(element->getStartLoc(),
                                                 refExpr->getTypeRepr());
      return new (Ctx) TypeExpr(repr);
    }
  }

  return nullptr;
}

void PreCheckTarget::resolveKeyPathExpr(KeyPathExpr *KPE) {
  if (KPE->isObjC())
    return;
  
  if (!KPE->getComponents().empty())
    return;

  TypeRepr *rootType = nullptr;
  SmallVector<KeyPathExpr::Component, 4> components;
  auto &DE = getASTContext().Diags;

  // Pre-order visit of a sequence foo.bar[0]?.baz, which means that the
  // components are pushed in reverse order.
  auto traversePath = [&](Expr *expr, bool isInParsedPath,
                          bool emitErrors = true) {
    Expr *outermostExpr = expr;
    // We can end up in scenarios where the key path has contextual type,
    // but is missing a leading dot. This can happen when we have an
    // implicit TypeExpr or an implicit DeclRefExpr.
    auto diagnoseMissingDot = [&]() {
      DE.diagnose(expr->getLoc(),
                  diag::expr_swift_keypath_not_starting_with_dot)
          .fixItInsert(expr->getStartLoc(), ".");
    };
    while (1) {
      // Base cases: we've reached the top.
      if (auto TE = dyn_cast<TypeExpr>(expr)) {
        assert(!isInParsedPath);
        rootType = TE->getTypeRepr();
        if (TE->isImplicit() && !KPE->expectsContextualRoot())
          diagnoseMissingDot();
        return;
      } else if (isa<KeyPathDotExpr>(expr)) {
        assert(isInParsedPath);
        // Nothing here: the type is either the root, or is inferred.
        return;
      } else if (!KPE->expectsContextualRoot() && expr->isImplicit() &&
                 isa<DeclRefExpr>(expr)) {
        assert(!isInParsedPath);
        diagnoseMissingDot();
        return;
      }

      // Recurring cases:
      if (auto SE = dyn_cast<DotSelfExpr>(expr)) {
        // .self, the identity component.
        components.push_back(KeyPathExpr::Component::forIdentity(
          SE->getSelfLoc()));
        expr = SE->getSubExpr();
      } else if (auto UDE = dyn_cast<UnresolvedDotExpr>(expr)) {
        // .foo, .foo() or .foo(val value: Int)
        components.push_back(KeyPathExpr::Component::forUnresolvedMember(
            UDE->getName(), UDE->getFunctionRefInfo(), UDE->getLoc()));

        expr = UDE->getBase();
      } else if (auto CCE = dyn_cast<CodeCompletionExpr>(expr)) {
        components.push_back(
            KeyPathExpr::Component::forCodeCompletion(CCE->getLoc()));

        expr = CCE->getBase();
        if (!expr) {
          // We are completing on the key path's base. Stop iterating.
          return;
        }
      } else if (auto SE = dyn_cast<SubscriptExpr>(expr)) {
        // .[0] or just plain [0]
        components.push_back(KeyPathExpr::Component::forUnresolvedSubscript(
            getASTContext(), SE->getArgs()));

        expr = SE->getBase();
      } else if (auto BOE = dyn_cast<BindOptionalExpr>(expr)) {
        // .? or ?
        components.push_back(KeyPathExpr::Component::forUnresolvedOptionalChain(
            BOE->getQuestionLoc()));

        expr = BOE->getSubExpr();
      } else if (auto FVE = dyn_cast<ForceValueExpr>(expr)) {
        // .! or !
        components.push_back(KeyPathExpr::Component::forUnresolvedOptionalForce(
            FVE->getExclaimLoc()));

        expr = FVE->getSubExpr();
      } else if (auto OEE = dyn_cast<OptionalEvaluationExpr>(expr)) {
        // Do nothing: this is implied to exist as the last expression, by the
        // BindOptionalExprs, but is irrelevant to the components.
        (void)outermostExpr;
        assert(OEE == outermostExpr);
        expr = OEE->getSubExpr();
      } else if (auto AE = dyn_cast<ApplyExpr>(expr)) {
        // foo(), foo(val value: Int) or unapplied foo
        components.push_back(KeyPathExpr::Component::forUnresolvedApply(
            getASTContext(), AE->getArgs()));
        expr = AE->getFn();
      } else {
        if (emitErrors) {
          // \(<expr>) may be an attempt to write a string interpolation outside
          // of a string literal; diagnose this case specially.
          if (isa<ParenExpr>(expr) || isa<TupleExpr>(expr)) {
            DE.diagnose(expr->getLoc(),
                        diag::expr_string_interpolation_outside_string);
          } else {
            DE.diagnose(expr->getLoc(),
                        diag::expr_swift_keypath_invalid_component);
          }
        }
        components.push_back(KeyPathExpr::Component());
        return;
      }
    }
  };

  auto root = KPE->getParsedRoot();
  auto path = KPE->getParsedPath();

  if (path) {
    traversePath(path, /*isInParsedPath=*/true);

    // This path looks like \Foo.Bar.[0].baz, which means Foo.Bar has to be a
    // type.
    if (root) {
      if (auto TE = dyn_cast<TypeExpr>(root)) {
        rootType = TE->getTypeRepr();
      } else {
        // FIXME: Probably better to catch this case earlier and force-eval as
        // TypeExpr.
        DE.diagnose(root->getLoc(),
                    diag::expr_swift_keypath_not_starting_with_type);

        // Traverse this path for recovery purposes: it may be a typo like
        // \Foo.property.[0].
        traversePath(root, /*isInParsedPath=*/false,
                     /*emitErrors=*/false);
      }
    }
  } else {
    traversePath(root, /*isInParsedPath=*/false);
  }

  // Key paths must be spelled with at least one component.
  if (components.empty()) {
    // Passes further down the pipeline expect keypaths to always have at least
    // one component, so stuff an invalid component in the AST for recovery.
    components.push_back(KeyPathExpr::Component());
  }

  std::reverse(components.begin(), components.end());

  KPE->setExplicitRootType(rootType);
  KPE->setComponents(getASTContext(), components);
}

Expr *PreCheckTarget::simplifyTypeConstructionWithLiteralArg(Expr *E) {
  // If constructor call is expected to produce an optional let's not attempt
  // this optimization because literal initializers aren't failable.
  if (!getASTContext().LangOpts.isSwiftVersionAtLeast(5)) {
    if (!ExprStack.empty()) {
      auto *parent = ExprStack.back();
      if (isa<BindOptionalExpr>(parent) || isa<ForceValueExpr>(parent))
        return nullptr;
    }
  }

  auto *call = dyn_cast<CallExpr>(E);
  if (!call)
    return nullptr;

  auto *typeExpr = dyn_cast<TypeExpr>(call->getFn());
  if (!typeExpr)
    return nullptr;

  auto *unaryArg = call->getArgs()->getUnlabeledUnaryExpr();
  if (!unaryArg)
    return nullptr;

  auto *literal = dyn_cast<LiteralExpr>(unaryArg->getSemanticsProvidingExpr());
  if (!literal)
    return nullptr;

  auto *protocol = TypeChecker::getLiteralProtocol(getASTContext(), literal);
  if (!protocol)
    return nullptr;

  Type castTy;
  if (auto precheckedTy = typeExpr->getInstanceType()) {
    castTy = precheckedTy;
  } else {
    const auto result = TypeResolution::resolveContextualType(
        typeExpr->getTypeRepr(), DC, TypeResolverContext::InExpression,
        [](auto unboundTy) {
          // FIXME: Don't let unbound generic types escape type resolution.
          // For now, just return the unbound generic type.
          return unboundTy;
        },
        // FIXME: Don't let placeholder types escape type resolution.
        // For now, just return the placeholder type.
        PlaceholderType::get,
        // Pack elements for CoerceExprs are opened in CSGen.
        /*packElementOpener*/ nullptr);

    if (result->hasError())
      return new (getASTContext())
          ErrorExpr(typeExpr->getSourceRange(), result, typeExpr);

    castTy = result;
  }

  if (!castTy->getAnyNominal())
    return nullptr;

  // Don't bother to convert deprecated selector syntax.
  if (auto selectorTy = getASTContext().getSelectorType()) {
    if (castTy->isEqual(selectorTy))
      return nullptr;
  }

  return lookupConformance(castTy, protocol)
             ? CoerceExpr::forLiteralInit(getASTContext(), literal,
                                          call->getSourceRange(),
                                          typeExpr->getTypeRepr())
             : nullptr;
}

/// Pull some operator expressions into the optional chain if needed.
///
///   foo? = newFoo // LHS of the assignment operator
///   foo?.bar += value // LHS of 'assignment: true' precedence group operators.
///
/// In such cases, the operand is constructed to be an 'OperatorEvaluationExpr'
/// wrapping the actual operand. This function hoist it and wraps the entire
/// expression with it. Returns the result 'OperatorEvaluationExpr', or nullptr
/// if 'expr' didn't match the condition.
OptionalEvaluationExpr *
PreCheckTarget::hoistOptionalEvaluationExprIfNeeded(Expr *expr) {
  if (auto *assignE = dyn_cast<AssignExpr>(expr)) {
    if (auto *OEE = dyn_cast<OptionalEvaluationExpr>(assignE->getDest())) {
      assignE->setDest(OEE->getSubExpr());
      OEE->setSubExpr(assignE);
      return OEE;
    }
  } else if (auto *binaryE = dyn_cast<BinaryExpr>(expr)) {
    if (auto *OEE = dyn_cast<OptionalEvaluationExpr>(binaryE->getLHS())) {
      if (auto *precedence = TypeChecker::lookupPrecedenceGroupForInfixOperator(
              DC, binaryE, /*diagnose=*/false)) {
        if (precedence->isAssignment()) {
          binaryE->getArgs()->setExpr(0, OEE->getSubExpr());
          OEE->setSubExpr(binaryE);
          return OEE;
        }
      }
    }
  }
  return nullptr;
}

Expr *PreCheckTarget::wrapMemberChainIfNeeded(Expr *E) {
  auto *parent = Parent.getAsExpr();
  Expr *wrapped = E;

  // If the parent is already wrapped, we've already formed the member chain.
  if (parent && (isa<OptionalEvaluationExpr>(parent) ||
                 isa<UnresolvedMemberChainResultExpr>(parent))) {
    return E;
  }

  // If we find an unresolved member chain, wrap it in an
  // UnresolvedMemberChainResultExpr.
  if (isMemberChainTail(E, parent, MemberChainKind::UnresolvedMember)) {
    if (auto *UME = TypeChecker::getUnresolvedMemberChainBase(E))
      wrapped = new (Ctx) UnresolvedMemberChainResultExpr(E, UME);
  }
  // Wrap optional chain in an OptionalEvaluationExpr.
  if (isMemberChainTail(E, parent, MemberChainKind::OptionalBind)) {
    if (isBindOptionalMemberChain(E))
      wrapped = new (Ctx) OptionalEvaluationExpr(wrapped);
  }
  return wrapped;
}

bool ConstraintSystem::preCheckTarget(SyntacticElementTarget &target) {
  auto *DC = target.getDeclContext();
  auto &ctx = DC->getASTContext();

  FrontendStatsTracer StatsTracer(ctx.Stats, "precheck-target");
  auto newTarget = PreCheckTarget::check(target);
  if (!newTarget)
    return true;

  target = *newTarget;
  return false;
}
