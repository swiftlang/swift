//===--- TypeCheckConstraints.cpp - Constraint-based Type Checking --------===//
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
// This file provides high-level entry points that use constraint
// systems for type checking, as well as a few miscellaneous helper
// functions that support the constraint system.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "TypeCheckType.h"
#include "TypoCorrection.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeCheckerDebugConsumer.h"
#include "swift/Basic/Statistic.h"
#include "swift/Parse/Confusables.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/Format.h"
#include <iterator>
#include <map>
#include <memory>
#include <utility>
#include <tuple>

using namespace swift;
using namespace constraints;

//===----------------------------------------------------------------------===//
// Type variable implementation.
//===----------------------------------------------------------------------===//
#pragma mark Type variable implementation

void TypeVariableType::Implementation::print(llvm::raw_ostream &OS) {
  getTypeVariable()->print(OS, PrintOptions());
}

SavedTypeVariableBinding::SavedTypeVariableBinding(TypeVariableType *typeVar)
  : TypeVarAndOptions(typeVar, typeVar->getImpl().getRawOptions()),
    ParentOrFixed(typeVar->getImpl().ParentOrFixed) { }

void SavedTypeVariableBinding::restore() {
  auto *typeVar = getTypeVariable();
  typeVar->getImpl().setRawOptions(getOptions());
  typeVar->getImpl().ParentOrFixed = ParentOrFixed;
}

GenericTypeParamType *
TypeVariableType::Implementation::getGenericParameter() const {
  // Check whether we have a path that terminates at a generic parameter
  // locator.
  if (!locator || locator->getPath().empty() ||
      locator->getPath().back().getKind() != ConstraintLocator::GenericParameter)
    return nullptr;

  // Retrieve the archetype.
  return locator->getPath().back().getGenericParameter();
}

// Only allow allocation of resolved overload set list items using the
// allocator in ASTContext.
void *ResolvedOverloadSetListItem::operator new(size_t bytes,
                                                ConstraintSystem &cs,
                                                unsigned alignment) {
  return cs.getAllocator().Allocate(bytes, alignment);
}

void *operator new(size_t bytes, ConstraintSystem& cs,
                   size_t alignment) {
  return cs.getAllocator().Allocate(bytes, alignment);
}

bool constraints::computeTupleShuffle(ArrayRef<TupleTypeElt> fromTuple,
                                      ArrayRef<TupleTypeElt> toTuple,
                                      SmallVectorImpl<int> &sources,
                                      SmallVectorImpl<unsigned> &variadicArgs) {
  const int unassigned = -3;
  
  SmallVector<bool, 4> consumed(fromTuple.size(), false);
  sources.clear();
  variadicArgs.clear();
  sources.assign(toTuple.size(), unassigned);

  // Match up any named elements.
  for (unsigned i = 0, n = toTuple.size(); i != n; ++i) {
    const auto &toElt = toTuple[i];

    // Skip unnamed elements.
    if (!toElt.hasName())
      continue;

    // Find the corresponding named element.
    int matched = -1;
    {
      int index = 0;
      for (auto field : fromTuple) {
        if (field.getName() == toElt.getName() && !consumed[index]) {
          matched = index;
          break;
        }
        ++index;
      }
    }
    if (matched == -1)
      continue;

    // Record this match.
    sources[i] = matched;
    consumed[matched] = true;
  }  

  // Resolve any unmatched elements.
  unsigned fromNext = 0, fromLast = fromTuple.size();
  auto skipToNextAvailableInput = [&] {
    while (fromNext != fromLast && consumed[fromNext])
      ++fromNext;
  };
  skipToNextAvailableInput();

  for (unsigned i = 0, n = toTuple.size(); i != n; ++i) {
    // Check whether we already found a value for this element.
    if (sources[i] != unassigned)
      continue;

    const auto &elt2 = toTuple[i];

    // Variadic tuple elements match the rest of the input elements.
    if (elt2.isVararg()) {
      // Collect the remaining (unnamed) inputs.
      while (fromNext != fromLast) {
        // Labeled elements can't be adopted into varargs even if
        // they're non-mandatory.  There isn't a really strong reason
        // for this, though.
        if (fromTuple[fromNext].hasName())
          return true;

        variadicArgs.push_back(fromNext);
        consumed[fromNext] = true;
        skipToNextAvailableInput();
      }
      sources[i] = TupleShuffleExpr::Variadic;
      
      // Keep looking at subsequent arguments.  Non-variadic arguments may
      // follow the variadic one.
      continue;
    }

    // If there aren't any more inputs, we are done.
    if (fromNext == fromLast) {
      return true;
    }

    // Otherwise, assign this input to the next output element.

    // Fail if the input element is named and we're trying to match it with
    // something with a different label.
    if (fromTuple[fromNext].hasName() && elt2.hasName())
      return true;

    sources[i] = fromNext;
    consumed[fromNext] = true;
    skipToNextAvailableInput();
  }

  // Complain if we didn't reach the end of the inputs.
  if (fromNext != fromLast) {
    return true;
  }

  // If we got here, we should have claimed all the arguments.
  assert(std::find(consumed.begin(), consumed.end(), false) == consumed.end());
  return false;
}

Expr *ConstraintLocatorBuilder::trySimplifyToExpr() const {
  SmallVector<LocatorPathElt, 4> pathBuffer;
  Expr *anchor = getLocatorParts(pathBuffer);
  ArrayRef<LocatorPathElt> path = pathBuffer;

  Expr *targetAnchor;
  SmallVector<LocatorPathElt, 4> targetPathBuffer;
  SourceRange range;

  simplifyLocator(anchor, path, targetAnchor, targetPathBuffer, range);
  return (path.empty() ? anchor : nullptr);
}

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
    if (!D || !D->hasInterfaceType())
      continue;
    if (matchesDeclRefKind(D, refKind))
      return true;
  }
  return false;
}

/// Emit a diagnostic with a fixit hint for an invalid binary operator, showing
/// how to split it according to splitCandidate.
static void diagnoseBinOpSplit(UnresolvedDeclRefExpr *UDRE,
                               std::pair<unsigned, bool> splitCandidate,
                               Diag<Identifier, Identifier, bool> diagID,
                               TypeChecker &TC) {

  unsigned splitLoc = splitCandidate.first;
  bool isBinOpFirst = splitCandidate.second;
  StringRef nameStr = UDRE->getName().getBaseIdentifier().str();
  auto startStr = nameStr.substr(0, splitLoc);
  auto endStr = nameStr.drop_front(splitLoc);

  // One valid split found, it is almost certainly the right answer.
  auto diag = TC.diagnose(UDRE->getLoc(), diagID,
                          TC.Context.getIdentifier(startStr),
                          TC.Context.getIdentifier(endStr), isBinOpFirst);
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
                                          DeclContext *DC,
                                          TypeChecker &TC) {
  Identifier name = UDRE->getName().getBaseIdentifier();
  StringRef nameStr = name.str();
  if (!name.isOperator() || nameStr.size() < 2)
    return false;

  bool isBinOp = UDRE->getRefKind() == DeclRefKind::BinaryOperator;

  // If this is a binary operator, relex the token, to decide whether it has
  // whitespace around it or not.  If it does "x +++ y", then it isn't likely to
  // be a case where a space was forgotten.
  if (isBinOp) {
    auto tok = Lexer::getTokenAtLocation(TC.Context.SourceMgr, UDRE->getLoc());
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

    auto startName = TC.Context.getIdentifier(startStr);
    auto endName = TC.Context.getIdentifier(endStr);

    // Perform name lookup for the first and second pieces.  If either fail to
    // be found, then it isn't a valid split.
    NameLookupOptions LookupOptions = defaultUnqualifiedLookupOptions;
    // This is only used for diagnostics, so always use KnownPrivate.
    LookupOptions |= NameLookupFlags::KnownPrivate;
    auto startLookup = TC.lookupUnqualified(DC, startName, UDRE->getLoc(),
                                       LookupOptions);
    if (!startLookup) continue;
    auto endLookup = TC.lookupUnqualified(DC, endName, UDRE->getLoc(),
                                          LookupOptions);
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
      diagnoseBinOpSplit(UDRE, WorkableSplits[0],
                         diag::unspaced_binary_operator_fixit, TC);
    else
      TC.diagnose(UDRE->getLoc().getAdvancedLoc(WorkableSplits[0].first),
                  diag::unspaced_unary_operator);
    return true;

  default:
    // Otherwise, we have to produce a series of notes listing the various
    // options.
    TC.diagnose(UDRE->getLoc(), isBinOp ? diag::unspaced_binary_operator :
                diag::unspaced_unary_operator)
      .highlight(UDRE->getLoc());

    if (isBinOp) {
      for (auto candidateSplit : WorkableSplits)
        diagnoseBinOpSplit(UDRE, candidateSplit,
                           diag::unspaced_binary_operators_candidate, TC);
    }
    return true;
  }
}


static bool diagnoseRangeOperatorMisspell(UnresolvedDeclRefExpr *UDRE,
                                          TypeChecker &TC) {
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
    TC.diagnose(UDRE->getLoc(), diag::use_unresolved_identifier_corrected,
                name, true, corrected)
      .highlight(UDRE->getSourceRange())
      .fixItReplace(UDRE->getSourceRange(), corrected);

    return true;
  }
  return false;
}

static bool diagnoseIncDecOperator(UnresolvedDeclRefExpr *UDRE,
                                   TypeChecker &TC) {
  auto name = UDRE->getName().getBaseIdentifier();
  if (!name.isOperator())
    return false;

  auto corrected = StringRef();
  if (name.str() == "++")
    corrected = "+= 1";
  else if (name.str() == "--")
    corrected = "-= 1";

  if (!corrected.empty()) {
    TC.diagnose(UDRE->getLoc(), diag::use_unresolved_identifier_corrected,
                name, true, corrected)
      .highlight(UDRE->getSourceRange());

    return true;
  }
  return false;
}

static bool findNonMembers(TypeChecker &TC,
                           ArrayRef<LookupResultEntry> lookupResults,
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

    if (!D->hasInterfaceType())
      TC.validateDecl(D);

    // FIXME: Circularity hack.
    if (!D->hasInterfaceType()) {
      AllDeclRefs = false;
      continue;
    }

    if (matchesDeclRefKind(D, refKind))
      ResultValues.push_back(D);
  }

  return AllDeclRefs;
}

/// Whether we should be looking at the outer results for a function called \c
/// name.
///
/// This is very restrictive because it's a source compatibility issue (see the
/// if (AllConditionalConformances) { (void)findNonMembers(...); } below).
static bool shouldConsiderOuterResultsFor(DeclName name) {
  const StringRef specialNames[] = {"min", "max"};
  for (auto specialName : specialNames)
    if (name.isSimpleName(specialName))
      return true;

  return false;
}

/// Bind an UnresolvedDeclRefExpr by performing name lookup and
/// returning the resultant expression. Context is the DeclContext used
/// for the lookup.
Expr *TypeChecker::
resolveDeclRefExpr(UnresolvedDeclRefExpr *UDRE, DeclContext *DC) {
  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  DeclName Name = UDRE->getName();
  SourceLoc Loc = UDRE->getLoc();

  // Perform standard value name lookup.
  NameLookupOptions lookupOptions = defaultUnqualifiedLookupOptions;
  if (isa<AbstractFunctionDecl>(DC))
    lookupOptions |= NameLookupFlags::KnownPrivate;
  if (shouldConsiderOuterResultsFor(Name))
    lookupOptions |= NameLookupFlags::IncludeOuterResults;

  auto Lookup = lookupUnqualified(DC, Name, Loc, lookupOptions);

  if (!Lookup) {
    // If we failed lookup of an operator, check to see if this is a range
    // operator misspelling. Otherwise try to diagnose a juxtaposition
    // e.g. (x*-4) that needs whitespace.
    if (diagnoseRangeOperatorMisspell(UDRE, *this) ||
        diagnoseIncDecOperator(UDRE, *this) ||
        diagnoseOperatorJuxtaposition(UDRE, DC, *this)) {
      return new (Context) ErrorExpr(UDRE->getSourceRange());
    }

    // Try ignoring access control.
    NameLookupOptions relookupOptions = lookupOptions;
    relookupOptions |= NameLookupFlags::KnownPrivate;
    relookupOptions |= NameLookupFlags::IgnoreAccessControl;
    LookupResult inaccessibleResults = lookupUnqualified(DC, Name, Loc,
                                                         relookupOptions);
    if (inaccessibleResults) {
      // FIXME: What if the unviable candidates have different levels of access?
      const ValueDecl *first = inaccessibleResults.front().getValueDecl();
      diagnose(Loc, diag::candidate_inaccessible, Name,
               first->getFormalAccessScope().accessLevelForDiagnostics());

      // FIXME: If any of the candidates (usually just one) are in the same
      // module we could offer a fix-it.
      for (auto lookupResult : inaccessibleResults) {
        diagnose(lookupResult.getValueDecl(), diag::decl_declared_here,
                 lookupResult.getValueDecl()->getFullName());
      }

      // Don't try to recover here; we'll get more access-related diagnostics
      // downstream if the type of the inaccessible decl is also inaccessible.
      return new (Context) ErrorExpr(UDRE->getSourceRange());
    }

    // TODO: Name will be a compound name if it was written explicitly as
    // one, but we should also try to propagate labels into this.
    DeclNameLoc nameLoc = UDRE->getNameLoc();

    Identifier simpleName = Name.getBaseIdentifier();
    const char *buffer = simpleName.get();
    llvm::SmallString<64> expectedIdentifier;
    bool isConfused = false;
    uint32_t codepoint;
    int offset = 0;
    while ((codepoint = validateUTF8CharacterAndAdvance(buffer,
                                                        buffer +
                                                        strlen(buffer)))
           != ~0U) {
      int length = (buffer - simpleName.get()) - offset;
      if (auto expectedCodepoint =
          confusable::tryConvertConfusableCharacterToASCII(codepoint)) {
        isConfused = true;
        expectedIdentifier += expectedCodepoint;
      } else {
        expectedIdentifier += (char)codepoint;
      }

      offset += length;
    }

    auto emitBasicError = [&] {
      diagnose(Loc, diag::use_unresolved_identifier, Name, Name.isOperator())
        .highlight(UDRE->getSourceRange());
    };

    if (!isConfused) {
      TypoCorrectionResults corrections(*this, Name, nameLoc);
      performTypoCorrection(DC, UDRE->getRefKind(), Type(),
                            lookupOptions, corrections);

      if (auto typo = corrections.claimUniqueCorrection()) {
        auto diag = diagnose(Loc, diag::use_unresolved_identifier_corrected,
                             Name, Name.isOperator(),
                             typo->CorrectedName.getBaseIdentifier().str());
        diag.highlight(UDRE->getSourceRange());
        typo->addFixits(diag);
      } else {
        emitBasicError();
      }

      corrections.noteAllCandidates();
    } else {
      emitBasicError();

      diagnose(Loc, diag::confusable_character,
               UDRE->getName().isOperator(), simpleName.str(),
               expectedIdentifier)
        .fixItReplace(Loc, expectedIdentifier);
    }

    // TODO: consider recovering from here.  We may want some way to suppress
    // downstream diagnostics, though.

    return new (Context) ErrorExpr(UDRE->getSourceRange());
  }

  // FIXME: Need to refactor the way we build an AST node from a lookup result!

  SmallVector<ValueDecl*, 4> ResultValues;
  ValueDecl *localDeclAfterUse = nullptr;
  auto isValid = [&](ValueDecl *D) {
    // FIXME: The source-location checks won't make sense once
    // EnableASTScopeLookup is the default.
    //
    // Note that we allow forward references to types, because they cannot
    // capture.
    if (Loc.isValid() && D->getLoc().isValid() &&
        D->getDeclContext()->isLocalContext() &&
        D->getDeclContext() == DC &&
        Context.SourceMgr.isBeforeInBuffer(Loc, D->getLoc()) &&
        !isa<TypeDecl>(D)) {
      localDeclAfterUse = D;
      return false;
    }
    return true;
  };
  bool AllDeclRefs = findNonMembers(
      *this, Lookup.innerResults(), UDRE->getRefKind(), /*breakOnMember=*/true,
      ResultValues, isValid);

  // If local declaration after use is found, check outer results for
  // better matching candidates.
  if (localDeclAfterUse) {
    auto innerDecl = localDeclAfterUse;

    // Perform a thorough lookup if outer results was not included before.
    if (!lookupOptions.contains(NameLookupFlags::IncludeOuterResults)) {
      auto option = lookupOptions;
      option |= NameLookupFlags::IncludeOuterResults;
      Lookup = lookupUnqualified(DC, Name, Loc, option);
    }

    while (localDeclAfterUse) {
      if (Lookup.outerResults().empty()) {
        diagnose(Loc, diag::use_local_before_declaration, Name);
        diagnose(innerDecl, diag::decl_declared_here, Name);
        Expr *error = new (Context) ErrorExpr(UDRE->getSourceRange());
        return error;
      }

      Lookup.shiftDownResults();
      ResultValues.clear();
      localDeclAfterUse = nullptr;
      AllDeclRefs = findNonMembers(
          *this, Lookup.innerResults(), UDRE->getRefKind(), /*breakOnMember=*/true,
          ResultValues, isValid);
    }

    // Drop outer results if they are not supposed to be included.
    if (!lookupOptions.contains(NameLookupFlags::IncludeOuterResults)) {
      Lookup.filter([&](LookupResultEntry Result, bool isOuter) {
          return !isOuter;
      });
    }
  }

  // If we have an unambiguous reference to a type decl, form a TypeExpr.
  if (Lookup.size() == 1 && UDRE->getRefKind() == DeclRefKind::Ordinary &&
      isa<TypeDecl>(Lookup[0].getValueDecl())) {
    auto *D = cast<TypeDecl>(Lookup[0].getValueDecl());
    // FIXME: This is odd.
    if (isa<ModuleDecl>(D)) {
      return new (Context) DeclRefExpr(D, UDRE->getNameLoc(),
                                       /*Implicit=*/false,
                                       AccessSemantics::Ordinary,
                                       D->getInterfaceType());
    }

    return TypeExpr::createForDecl(Loc, D,
                                   Lookup[0].getDeclContext(),
                                   UDRE->isImplicit());
  }

  if (AllDeclRefs) {
    // Diagnose uses of operators that found no matching candidates.
    if (ResultValues.empty()) {
      assert(UDRE->getRefKind() != DeclRefKind::Ordinary);
      diagnose(Loc, diag::use_nonmatching_operator,
               Name,
               UDRE->getRefKind() == DeclRefKind::BinaryOperator ? 0 :
               UDRE->getRefKind() == DeclRefKind::PrefixOperator ? 1 : 2);
      return new (Context) ErrorExpr(UDRE->getSourceRange());
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

        unsigned xDepth = xGeneric->getGenericParams().back()->getDepth();
        unsigned yDepth = yGeneric->getGenericParams().back()->getDepth();
        return xDepth < yDepth;
      });
    }

    return buildRefExpr(ResultValues, DC, UDRE->getNameLoc(),
                        UDRE->isImplicit(), UDRE->getFunctionRefKind());
  }

  ResultValues.clear();
  bool AllMemberRefs = true;
  bool AllConditionalConformances = true;
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

      // Check if this result is derived through a conditional conformance,
      // meaning it comes from a protocol (or extension) where there's a
      // conditional conformance for the type with the method in question
      // (NB. that type may not be the type associated with DC, for tested types
      // with static methods).
      if (auto Proto = Value->getDeclContext()->getSelfProtocolDecl()) {
        auto contextSelfType =
            BaseDC->getInnermostTypeContext()->getDeclaredInterfaceType();
        auto conformance = conformsToProtocol(
            contextSelfType, Proto, DC,
            ConformanceCheckFlags::InExpression |
                ConformanceCheckFlags::SkipConditionalRequirements);

        if (!conformance || conformance->getConditionalRequirements().empty()) {
          AllConditionalConformances = false;
        }
      }

      continue;
    }

    AllMemberRefs = false;
    break;
  }

  if (AllMemberRefs) {
    Expr *BaseExpr;
    if (auto PD = dyn_cast<ProtocolDecl>(Base)) {
      BaseExpr = TypeExpr::createForDecl(Loc,
                                         PD->getGenericParams()->getParams().front(),
                                         /*DC*/nullptr,
                                         /*isImplicit=*/true);
    } else if (auto NTD = dyn_cast<NominalTypeDecl>(Base)) {
      BaseExpr = TypeExpr::createForDecl(Loc, NTD, BaseDC, /*isImplicit=*/true);
    } else {
      BaseExpr = new (Context) DeclRefExpr(Base, UDRE->getNameLoc(),
                                           /*Implicit=*/true);
    }

    // We *might* include any non-members that we found in outer contexts in
    // some special cases, for backwards compatibility: first, we have to be
    // looking for one of the special names
    // ('shouldConsiderOuterResultsFor(Name)'), and second, all of the inner
    // results need to come from conditional conformances. The second condition
    // is how the problem here was encountered: a type ('Range') was made to
    // conditionally conform to a new protocol ('Sequence'), which introduced
    // some extra methods ('min' and 'max') that shadowed global functions that
    // people regularly called within extensions to that type (usually adding
    // 'clamp').
    llvm::SmallVector<ValueDecl *, 4> outerAlternatives;
    if (AllConditionalConformances) {
      (void)findNonMembers(*this, Lookup.outerResults(), UDRE->getRefKind(),
                           /*breakOnMember=*/false, outerAlternatives,
                           /*isValid=*/[&](ValueDecl *) { return true; });
    }

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
  diagnose(Loc, diag::ambiguous_decl_ref, Name);
  for (auto Result : Lookup) {
    auto *Decl = Result.getValueDecl();
    diagnose(Decl, diag::decl_declared_here, Decl->getFullName());
  }
  return new (Context) ErrorExpr(UDRE->getSourceRange());
}

/// If an expression references 'self.init' or 'super.init' in an
/// initializer context, returns the implicit 'self' decl of the constructor.
/// Otherwise, return nil.
VarDecl *
TypeChecker::getSelfForInitDelegationInConstructor(DeclContext *DC,
                                                   UnresolvedDotExpr *ctorRef) {
  // If the reference isn't to a constructor, we're done.
  if (ctorRef->getName().getBaseName() != DeclBaseName::createConstructor())
    return nullptr;

  if (auto ctorContext
        = dyn_cast_or_null<ConstructorDecl>(DC->getInnermostMethodContext())) {
    auto nestedArg = ctorRef->getBase();
    if (auto inout = dyn_cast<InOutExpr>(nestedArg))
      nestedArg = inout->getSubExpr();
    if (nestedArg->isSuperExpr())
      return ctorContext->getImplicitSelfDecl();
    if (auto declRef = dyn_cast<DeclRefExpr>(nestedArg))
      if (declRef->getDecl()->getFullName() == Context.Id_self)
        return ctorContext->getImplicitSelfDecl();
  }
  return nullptr;
}

namespace {
  /// Update the function reference kind based on adding a direct call to a
  /// callee with this kind.
  FunctionRefKind addingDirectCall(FunctionRefKind kind) {
    switch (kind) {
    case FunctionRefKind::Unapplied:
      return FunctionRefKind::SingleApply;

    case FunctionRefKind::SingleApply:
    case FunctionRefKind::DoubleApply:
      return FunctionRefKind::DoubleApply;

    case FunctionRefKind::Compound:
      return FunctionRefKind::Compound;
    }

    llvm_unreachable("Unhandled FunctionRefKind in switch.");
  }

  /// Update a direct callee expression node that has a function reference kind
  /// based on seeing a call to this callee.
  template<typename E,
           typename = decltype(((E*)nullptr)->getFunctionRefKind())> 
  void tryUpdateDirectCalleeImpl(E *callee, int) {
    callee->setFunctionRefKind(addingDirectCall(callee->getFunctionRefKind()));
  }

  /// Version of tryUpdateDirectCalleeImpl for when the callee
  /// expression type doesn't carry a reference.
  template<typename E> 
  void tryUpdateDirectCalleeImpl(E *callee, ...) { }

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

      // Coercions can be used for disambiguation.
      if (auto coerce = dyn_cast<CoerceExpr>(callee)) {
        callee = coerce->getSubExpr();
        continue;
      }

      // We're done.
      break;
    }
                                
    // Cast the callee to its most-specific class, then try to perform an
    // update. If the expression node has a declaration reference in it, the
    // update will succeed. Otherwise, we're done propagating.
    switch (callee->getKind()) {
#define EXPR(Id, Parent)                                  \
    case ExprKind::Id:                                    \
      tryUpdateDirectCalleeImpl(cast<Id##Expr>(callee), 0); \
      break;
#include "swift/AST/ExprNodes.def"
    }
  }

  class PreCheckExpression : public ASTWalker {
    TypeChecker &TC;
    DeclContext *DC;

    Expr *ParentExpr;

    /// A stack of expressions being walked, used to determine where to
    /// insert RebindSelfInConstructorExpr nodes.
    llvm::SmallVector<Expr *, 8> ExprStack;

    /// The 'self' variable to use when rebinding 'self' in a constructor.
    VarDecl *UnresolvedCtorSelf = nullptr;

    /// The expression that will be wrapped by a RebindSelfInConstructorExpr
    /// node when visited.
    Expr *UnresolvedCtorRebindTarget = nullptr;

    /// The expressions that are direct arguments of call expressions.
    llvm::SmallPtrSet<Expr *, 4> CallArgs;

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
    Expr *simplifyTypeConstructionWithLiteralArg(Expr *E);

  public:
    PreCheckExpression(TypeChecker &tc, DeclContext *dc, Expr *parent)
        : TC(tc), DC(dc), ParentExpr(parent) {}

    bool walkToClosureExprPre(ClosureExpr *expr);

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // If this is a call, record the argument expression.
      if (auto call = dyn_cast<ApplyExpr>(expr)) {
        if (!isa<SelfApplyExpr>(expr)) {
          CallArgs.insert(call->getArg());
        }
      }

      // If this is an unresolved member with a call argument (e.g.,
      // .some(x)), record the argument expression.
      if (auto unresolvedMember = dyn_cast<UnresolvedMemberExpr>(expr)) {
        if (auto arg = unresolvedMember->getArgument())
          CallArgs.insert(arg);
      }

      // Local function used to finish up processing before returning. Every
      // return site should call through here.
      auto finish = [&](bool recursive, Expr *expr) {
        // If we're going to recurse, record this expression on the stack.
        if (recursive)
          ExprStack.push_back(expr);

        return std::make_pair(recursive, expr);
      };

      // For capture lists, we typecheck the decls they contain.
      if (auto captureList = dyn_cast<CaptureListExpr>(expr)) {
        // Validate the capture list.
        for (auto capture : captureList->getCaptureList()) {
          TC.typeCheckDecl(capture.Init);
          TC.typeCheckDecl(capture.Var);
        }

        // Since closure expression is contained by capture list
        // let's handle it directly to avoid walking into capture
        // list itself.
        captureList->getClosureBody()->walk(*this);
        return finish(false, expr);
      }

      // For closures, type-check the patterns and result type as written,
      // but do not walk into the body. That will be type-checked after
      // we've determine the complete function type.
      if (auto closure = dyn_cast<ClosureExpr>(expr))
        return finish(walkToClosureExprPre(closure), expr);

      if (auto unresolved = dyn_cast<UnresolvedDeclRefExpr>(expr)) {
        TC.checkForForbiddenPrefix(unresolved);
        return finish(true, TC.resolveDeclRefExpr(unresolved, DC));
      }

      if (auto PlaceholderE = dyn_cast<EditorPlaceholderExpr>(expr)) {
        if (!PlaceholderE->getTypeLoc().isNull()) {
          if (!TC.validateType(PlaceholderE->getTypeLoc(),
                               TypeResolution::forContextual(DC), None))
            expr->setType(PlaceholderE->getTypeLoc().getType());
        }
        return finish(true, expr);
      }

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

        if (TC.isExprBeingDiagnosed(ParentExpr) ||
            TC.isExprBeingDiagnosed(expr))
          return finish(true, expr);

        auto parents = ParentExpr->getParentMap();

        auto result = parents.find(expr);
        if (result != parents.end()) {
          auto *parent = result->getSecond();

          if (isa<SequenceExpr>(parent))
            return finish(true, expr);

          if (isa<TupleExpr>(parent) || isa<ParenExpr>(parent)) {
            auto call = parents.find(parent);
            if (call != parents.end()) {
              if (isa<ApplyExpr>(call->getSecond()) ||
                  isa<UnresolvedMemberExpr>(call->getSecond()))
                return finish(true, expr);

              if (isa<SubscriptExpr>(call->getSecond())) {
                TC.diagnose(expr->getStartLoc(),
                            diag::cannot_pass_inout_arg_to_subscript);
                return finish(false, nullptr);
              }
            }
          }
        }

        TC.diagnose(expr->getStartLoc(), diag::extraneous_address_of);
        return finish(false, nullptr);
      }

      return finish(true, expr);
    }

    Expr *walkToExprPost(Expr *expr) override {
      // Remove this expression from the stack.
      assert(ExprStack.back() == expr);
      ExprStack.pop_back();

      // Mark the direct callee as being a callee.
      if (auto *call = dyn_cast<CallExpr>(expr))
        markDirectCallee(call->getFn());

      // Fold sequence expressions.
      if (auto *seqExpr = dyn_cast<SequenceExpr>(expr)) {
        auto result = TC.foldSequence(seqExpr, DC);
        return result->walk(*this);
      }

      // Type check the type parameters in an UnresolvedSpecializeExpr.
      if (auto *us = dyn_cast<UnresolvedSpecializeExpr>(expr)) {
        if (auto *typeExpr = simplifyUnresolvedSpecializeExpr(us))
          return typeExpr;
      }
      
      // If we're about to step out of a ClosureExpr, restore the DeclContext.
      if (auto *ce = dyn_cast<ClosureExpr>(expr)) {
        assert(DC == ce && "DeclContext imbalance");
        DC = ce->getParent();
      }

      // Strip off any AutoClosures that were produced by a previous type check
      // so that we don't choke in CSGen.
      // FIXME: we shouldn't double typecheck, but it looks like code completion
      // may do so in some circumstances. rdar://21466394
      if (auto autoClosure = dyn_cast<AutoClosureExpr>(expr))
        return autoClosure->getSingleExpressionBody();

      // A 'self.init' or 'super.init' application inside a constructor will
      // evaluate to void, with the initializer's result implicitly rebound
      // to 'self'. Recognize the unresolved constructor expression and
      // determine where to place the RebindSelfInConstructorExpr node.
      // When updating this logic, also update
      // RebindSelfInConstructorExpr::getCalledConstructor.
      if (auto unresolvedDot = dyn_cast<UnresolvedDotExpr>(expr)) {
        if (auto self
              = TC.getSelfForInitDelegationInConstructor(DC, unresolvedDot)) {
          // Walk our ancestor expressions looking for the appropriate place
          // to insert the RebindSelfInConstructorExpr.
          Expr *target = nullptr;
          bool foundApply = false;
          bool foundRebind = false;
          for (auto ancestor : reversed(ExprStack)) {
            if (isa<RebindSelfInConstructorExpr>(ancestor)) {
              // If we already have a rebind, then we're re-typechecking an
              // expression and are done.
              foundRebind = true;
              break;
            }

            // Recognize applications.
            if (auto apply = dyn_cast<ApplyExpr>(ancestor)) {
              // If we already saw an application, we're done.
              if (foundApply)
                break;

              // If the function being called is not our unresolved initializer
              // reference, we're done.
              if (apply->getSemanticFn() != unresolvedDot)
                break;

              foundApply = true;
              target = ancestor;
              continue;
            }

            // Look through identity, force-value, and 'try' expressions.
            if (isa<IdentityExpr>(ancestor) ||
                isa<ForceValueExpr>(ancestor) ||
                isa<AnyTryExpr>(ancestor)) {
              if (!CallArgs.count(ancestor)) {
                if (target)
                  target = ancestor;
                continue;
              }
            }

            // No other expression kinds are permitted.
            break;
          }

          // If we found a rebind target, note the insertion point.
          if (target && !foundRebind) {
            UnresolvedCtorRebindTarget = target;
            UnresolvedCtorSelf = self;
          }
        }
      }

      // If the expression we've found is the intended target of an
      // RebindSelfInConstructorExpr, wrap it in the
      // RebindSelfInConstructorExpr.
      if (expr == UnresolvedCtorRebindTarget) {
        expr = new (TC.Context) RebindSelfInConstructorExpr(expr,
                                                            UnresolvedCtorSelf);
        UnresolvedCtorRebindTarget = nullptr;
        return expr;
      }

      // Double check if there are any BindOptionalExpr remaining in the
      // tree (see comment below for more details), if there are no BOE
      // expressions remaining remove OptionalEvaluationExpr from the tree.
      if (auto OEE = dyn_cast<OptionalEvaluationExpr>(expr)) {
        bool hasBindOptional = false;
        OEE->forEachChildExpr([&](Expr *expr) -> Expr * {
          if (isa<BindOptionalExpr>(expr))
            hasBindOptional = true;
          // If at least a single BOE was found, no reason
          // to walk any further in the tree.
          return hasBindOptional ? nullptr : expr;
        });

        return hasBindOptional ? OEE : OEE->getSubExpr();
      }

      // Check if there are any BindOptionalExpr in the tree which
      // wrap DiscardAssignmentExpr, such situation corresponds to syntax
      // like - `_? = <value>`, since it doesn't really make
      // sense to have optional assignment to discarded LValue which can
      // never be optional, we can remove BOE from the tree and avoid
      // generating any of the unnecessary constraints.
      if (auto BOE = dyn_cast<BindOptionalExpr>(expr)) {
        if (auto DAE = dyn_cast<DiscardAssignmentExpr>(BOE->getSubExpr()))
          return DAE;
      }

      // If this is a sugared type that needs to be folded into a single
      // TypeExpr, do it.
      if (auto *simplified = simplifyTypeExpr(expr))
        return simplified;

      if (auto KPE = dyn_cast<KeyPathExpr>(expr)) {
        resolveKeyPathExpr(KPE);
        return KPE;
      }

      if (auto *simplified = simplifyTypeConstructionWithLiteralArg(expr))
        return simplified;

      return expr;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      // Never walk into statements.
      return { false, stmt };
    }
  };
} // end anonymous namespace

/// Perform prechecking of a ClosureExpr before we dive into it.  This returns
/// true for single-expression closures, where we want the body to be considered
/// part of this larger expression.
bool PreCheckExpression::walkToClosureExprPre(ClosureExpr *closure) {
  auto *PL = closure->getParameters();

  // Validate the parameters.
  TypeResolutionOptions options(TypeResolverContext::ClosureExpr);
  options |= TypeResolutionFlags::AllowUnspecifiedTypes;
  options |= TypeResolutionFlags::AllowUnboundGenerics;
  bool hadParameterError = false;

  auto resolution = TypeResolution::forContextual(closure);
  if (TC.typeCheckParameterList(PL, resolution, options)) {
    // If we encounter an error validating the parameter list, don't bail.
    // Instead, go on to validate any potential result type, and bail
    // afterwards.  This allows for better diagnostics, and keeps the
    // closure expression type well-formed.
    hadParameterError = true;
  }

  // Validate the result type, if present.
  if (closure->hasExplicitResultType() &&
      TC.validateType(closure->getExplicitResultTypeLoc(), resolution,
                      TypeResolverContext::InExpression)) {
    return false;
  }

  if (hadParameterError)
    return false;

  // If the closure has a multi-statement body, we don't walk into it
  // here.
  if (!closure->hasSingleExpressionBody())
    return false;

  // Update the current DeclContext to be the closure we're about to
  // recurse into.
  assert(DC == closure->getParent() && "Decl context isn't correct");
  DC = closure;
  return true;
}

TypeExpr *PreCheckExpression::simplifyNestedTypeExpr(UnresolvedDotExpr *UDE) {
  if (!UDE->getName().isSimpleName() ||
      UDE->getName().isSpecial())
    return nullptr;

  auto Name = UDE->getName().getBaseIdentifier();
  auto NameLoc = UDE->getNameLoc().getBaseNameLoc();

  // Qualified type lookup with a module base is represented as a DeclRefExpr
  // and not a TypeExpr.
  if (auto *DRE = dyn_cast<DeclRefExpr>(UDE->getBase())) {
    if (auto *TD = dyn_cast<TypeDecl>(DRE->getDecl())) {
      auto lookupOptions = defaultMemberLookupOptions;
      if (isa<AbstractFunctionDecl>(DC) ||
          isa<AbstractClosureExpr>(DC))
        lookupOptions |= NameLookupFlags::KnownPrivate;

      // See if the type has a member type with this name.
      auto Result = TC.lookupMemberType(DC,
                                        TD->getDeclaredInterfaceType(),
                                        Name,
                                        lookupOptions);

      // If there is no nested type with this name, we have a lookup of
      // a non-type member, so leave the expression as-is.
      if (Result.size() == 1) {
        return TypeExpr::createForMemberDecl(DRE->getNameLoc().getBaseNameLoc(),
                                             TD, NameLoc,
                                             Result.front().Member);
      }
    }

    return nullptr;
  }

  auto *TyExpr = dyn_cast<TypeExpr>(UDE->getBase());
  if (!TyExpr)
    return nullptr;

  auto *InnerTypeRepr = TyExpr->getTypeRepr();
  if (!InnerTypeRepr)
    return nullptr;

  // Fold 'T.Protocol' into a protocol metatype.
  if (Name == TC.Context.Id_Protocol) {
    auto *NewTypeRepr =
      new (TC.Context) ProtocolTypeRepr(InnerTypeRepr, NameLoc);
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }

  // Fold 'T.Type' into an existential metatype if 'T' is a protocol,
  // or an ordinary metatype otherwise.
  if (Name == TC.Context.Id_Type) {
    auto *NewTypeRepr =
      new (TC.Context) MetatypeTypeRepr(InnerTypeRepr, NameLoc);
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }

  // Fold 'T.U' into a nested type.
  if (auto *ITR = dyn_cast<IdentTypeRepr>(InnerTypeRepr)) {
    // Resolve the TypeRepr to get the base type for the lookup.
    // Disable availability diagnostics here, because the final
    // TypeRepr will be resolved again when generating constraints.
    TypeResolutionOptions options(TypeResolverContext::InExpression);
    options |= TypeResolutionFlags::AllowUnboundGenerics;
    options |= TypeResolutionFlags::AllowUnavailable;
    auto resolution = TypeResolution::forContextual(DC);
    auto BaseTy = resolution.resolveType(InnerTypeRepr, options);

    if (BaseTy && BaseTy->mayHaveMembers()) {
      auto lookupOptions = defaultMemberLookupOptions;
      if (isa<AbstractFunctionDecl>(DC) ||
          isa<AbstractClosureExpr>(DC))
        lookupOptions |= NameLookupFlags::KnownPrivate;

      // See if there is a member type with this name.
      auto Result = TC.lookupMemberType(DC,
                                        BaseTy,
                                        Name,
                                        lookupOptions);

      // If there is no nested type with this name, we have a lookup of
      // a non-type member, so leave the expression as-is.
      if (Result.size() == 1) {
        return TypeExpr::createForMemberDecl(ITR, NameLoc,
                                             Result.front().Member);
      }
    }
  }

  return nullptr;
}

TypeExpr *PreCheckExpression::simplifyUnresolvedSpecializeExpr(
    UnresolvedSpecializeExpr *us) {
  SmallVector<TypeRepr *, 4> genericArgs;
  for (auto &type : us->getUnresolvedParams()) {
    genericArgs.push_back(type.getTypeRepr());
  }

  auto angleRange = SourceRange(us->getLAngleLoc(), us->getRAngleLoc());

  // If this is a reference type a specialized type, form a TypeExpr.

  // The base should be a TypeExpr that we already resolved.
  if (auto *te = dyn_cast<TypeExpr>(us->getSubExpr())) {
    if (auto *ITR = dyn_cast_or_null<IdentTypeRepr>(te->getTypeRepr())) {
      return TypeExpr::createForSpecializedDecl(ITR, genericArgs, angleRange,
                                                TC.Context);
    }
  }

  return nullptr;
}

/// Simplify expressions which are type sugar productions that got parsed
/// as expressions due to the parser not knowing which identifiers are
/// type names.
TypeExpr *PreCheckExpression::simplifyTypeExpr(Expr *E) {
  // Don't try simplifying a call argument, because we don't want to
  // simplify away the required ParenExpr/TupleExpr.
  if (CallArgs.count(E) > 0) return nullptr;

  // Fold member types.
  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(E)) {
    return simplifyNestedTypeExpr(UDE);
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
      new (TC.Context) OptionalTypeRepr(InnerTypeRepr, QuestionLoc);
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }

  // Fold T! into an IUO type when T is a TypeExpr.
  if (auto *FVE = dyn_cast<ForceValueExpr>(E)) {
    auto *TyExpr = dyn_cast<TypeExpr>(FVE->getSubExpr());
    if (!TyExpr) return nullptr;

    auto *InnerTypeRepr = TyExpr->getTypeRepr();
    assert(!TyExpr->isImplicit() && InnerTypeRepr &&
           "This doesn't work on implicit TypeExpr's, "
           "the TypeExpr should have been built correctly in the first place");

    auto *NewTypeRepr =
      new (TC.Context) ImplicitlyUnwrappedOptionalTypeRepr(InnerTypeRepr,
                                                          FVE->getExclaimLoc());
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }

  // Fold (T) into a type T with parens around it.
  if (auto *PE = dyn_cast<ParenExpr>(E)) {
    auto *TyExpr = dyn_cast<TypeExpr>(PE->getSubExpr());
    if (!TyExpr) return nullptr;
    
    TupleTypeReprElement InnerTypeRepr[] = { TyExpr->getTypeRepr() };
    assert(!TyExpr->isImplicit() && InnerTypeRepr[0].Type &&
           "SubscriptExpr doesn't work on implicit TypeExpr's, "
           "the TypeExpr should have been built correctly in the first place");
    
    auto *NewTypeRepr = TupleTypeRepr::create(TC.Context,
                                              InnerTypeRepr,
                                              PE->getSourceRange());
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }
  
  // Fold a tuple expr like (T1,T2) into a tuple type (T1,T2).
  if (auto *TE = dyn_cast<TupleExpr>(E)) {
    if (TE->hasTrailingClosure() ||
        // FIXME: Decide what to do about ().  It could be a type or an expr.
        TE->getNumElements() == 0)
      return nullptr;

    SmallVector<TupleTypeReprElement, 4> Elts;
    unsigned EltNo = 0;
    for (auto Elt : TE->getElements()) {
      auto *eltTE = dyn_cast<TypeExpr>(Elt);
      if (!eltTE) return nullptr;
      TupleTypeReprElement elt;
      assert(eltTE->getTypeRepr() && !eltTE->isImplicit() &&
             "This doesn't work on implicit TypeExpr's, the "
             "TypeExpr should have been built correctly in the first place");

      // If the tuple element has a label, propagate it.
      elt.Type = eltTE->getTypeRepr();
      Identifier name = TE->getElementName(EltNo);
      if (!name.empty()) {
        elt.Name = name;
        elt.NameLoc = TE->getElementNameLoc(EltNo);
      }

      Elts.push_back(elt);
      ++EltNo;
    }
    auto *NewTypeRepr = TupleTypeRepr::create(TC.Context, Elts,
                                              TE->getSourceRange(),
                                              SourceLoc(), Elts.size());
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }
  

  // Fold [T] into an array type.
  if (auto *AE = dyn_cast<ArrayExpr>(E)) {
    if (AE->getElements().size() != 1)
      return nullptr;

    auto *TyExpr = dyn_cast<TypeExpr>(AE->getElement(0));
    if (!TyExpr)
      return nullptr;

    auto *NewTypeRepr =
      new (TC.Context) ArrayTypeRepr(TyExpr->getTypeRepr(), 
                                     SourceRange(AE->getLBracketLoc(),
                                                 AE->getRBracketLoc()));
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));

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
      if (!TRE || TRE->getEllipsisLoc().isValid()) return nullptr;
      while (TRE->isParenType()) {
        TRE = dyn_cast_or_null<TupleTypeRepr>(TRE->getElementType(0));
        if (!TRE || TRE->getEllipsisLoc().isValid()) return nullptr;
      }

      assert(TRE->getElements().size() == 2);
      keyTypeRepr = TRE->getElementType(0);
      valueTypeRepr = TRE->getElementType(1);
    }

    auto *NewTypeRepr =
      new (TC.Context) DictionaryTypeRepr(keyTypeRepr, valueTypeRepr,
                                          /*FIXME:colonLoc=*/SourceLoc(),
                                          SourceRange(DE->getLBracketLoc(),
                                                      DE->getRBracketLoc()));
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }

  // Reinterpret arrow expr T1 -> T2 as function type.
  // FIXME: support 'inout', etc.
  if (auto *AE = dyn_cast<ArrowExpr>(E)) {
    if (!AE->isFolded()) return nullptr;

    auto diagnoseMissingParens = [](TypeChecker &TC, TypeRepr *tyR) {
      bool isVoid = false;
      if (const auto Void = dyn_cast<SimpleIdentTypeRepr>(tyR)) {
        if (Void->getIdentifier().str() == "Void") {
          isVoid = true;
        }
      }

      if (isVoid) {
        TC.diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
            .fixItReplace(tyR->getStartLoc(), "()");
      } else {
        TC.diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
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
        diagnoseMissingParens(TC, ArgRepr);
        return TupleTypeRepr::create(TC.Context, {ArgRepr},
                                     ArgRepr->getSourceRange());
      }
      if (auto *TE = dyn_cast<TupleExpr>(E))
        if (TE->getNumElements() == 0)
          return TupleTypeRepr::createEmpty(TC.Context, TE->getSourceRange());

      // When simplifying a type expr like "(P1 & P2) -> (P3 & P4) -> Int",
      // it may have been folded at the same time; recursively simplify it.
      if (auto ArgsTypeExpr = simplifyTypeExpr(E)) {
        auto ArgRepr = ArgsTypeExpr->getTypeRepr();
        if (auto *TTyRepr = dyn_cast<TupleTypeRepr>(ArgRepr))
          return TTyRepr;
        diagnoseMissingParens(TC, ArgRepr);
        return TupleTypeRepr::create(TC.Context, {ArgRepr},
                                     ArgRepr->getSourceRange());
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
          return TupleTypeRepr::createEmpty(TC.Context, TE->getSourceRange());

      // When simplifying a type expr like "P1 & P2 -> P3 & P4 -> Int",
      // it may have been folded at the same time; recursively simplify it.
      if (auto ArgsTypeExpr = simplifyTypeExpr(E))
        return ArgsTypeExpr->getTypeRepr();
      return nullptr;
    };

    TupleTypeRepr *ArgsTypeRepr = extractInputTypeRepr(AE->getArgsExpr());
    if (!ArgsTypeRepr) {
      TC.diagnose(AE->getArgsExpr()->getLoc(),
                  diag::expected_type_before_arrow);
      auto ArgRange = AE->getArgsExpr()->getSourceRange();
      auto ErrRepr =
          new (TC.Context) ErrorTypeRepr(ArgRange);
      ArgsTypeRepr = TupleTypeRepr::create(TC.Context, {ErrRepr}, ArgRange);
    }

    TypeRepr *ResultTypeRepr = extractTypeRepr(AE->getResultExpr());
    if (!ResultTypeRepr) {
      TC.diagnose(AE->getResultExpr()->getLoc(),
                  diag::expected_type_after_arrow);
      ResultTypeRepr =
        new (TC.Context) ErrorTypeRepr(AE->getResultExpr()->getSourceRange());
    }

    auto NewTypeRepr =
      new (TC.Context) FunctionTypeRepr(nullptr, ArgsTypeRepr,
                                        AE->getThrowsLoc(), AE->getArrowLoc(),
                                        ResultTypeRepr);
    return new (TC.Context) TypeExpr(TypeLoc(NewTypeRepr, Type()));
  }
  
  // Fold 'P & Q' into a composition type
  if (auto *binaryExpr = dyn_cast<BinaryExpr>(E)) {
    bool isComposition = false;
    // look at the name of the operator, if it is a '&' we can create the
    // composition TypeExpr
    auto fn = binaryExpr->getFn();
    if (auto Overload = dyn_cast<OverloadedDeclRefExpr>(fn)) {
      for (auto Decl : Overload->getDecls())
        if (Decl->getBaseName() == "&") {
          isComposition = true;
          break;
        }
    } else if (auto *Decl = dyn_cast<UnresolvedDeclRefExpr>(fn)) {
      if (Decl->getName().isSimpleName() &&
          Decl->getName().getBaseName() == "&")
        isComposition = true;
    }

    if (isComposition) {
      // The protocols we are composing
      SmallVector<TypeRepr *, 4> Types;

      auto lhsExpr = binaryExpr->getArg()->getElement(0);
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
      auto *rhs = dyn_cast<TypeExpr>(binaryExpr->getArg()->getElement(1));
      if (!rhs) return nullptr;
      Types.push_back(rhs->getTypeRepr());

      auto CompRepr = CompositionTypeRepr::create(TC.Context, Types,
          lhsExpr->getStartLoc(), binaryExpr->getSourceRange());
      return new (TC.Context) TypeExpr(TypeLoc(CompRepr, Type()));
    }
  }

  return nullptr;
}

void PreCheckExpression::resolveKeyPathExpr(KeyPathExpr *KPE) {
  if (KPE->isObjC())
    return;
  
  if (!KPE->getComponents().empty())
    return;

  TypeRepr *rootType = nullptr;
  SmallVector<KeyPathExpr::Component, 4> components;

  // Pre-order visit of a sequence foo.bar[0]?.baz, which means that the
  // components are pushed in reverse order.
  auto traversePath = [&](Expr *expr, bool isInParsedPath,
                          bool emitErrors = true) {
    Expr *outermostExpr = expr;
    while (1) {
      // Base cases: we've reached the top.
      if (auto TE = dyn_cast<TypeExpr>(expr)) {
        assert(!isInParsedPath);
        rootType = TE->getTypeRepr();
        return;
      } else if (isa<KeyPathDotExpr>(expr)) {
        assert(isInParsedPath);
        // Nothing here: the type is either the root, or is inferred.
        return;
      }

      // Recurring cases:
      if (auto SE = dyn_cast<DotSelfExpr>(expr)) {
        // .self, the identity component.
        components.push_back(KeyPathExpr::Component::forIdentity(
          SE->getSelfLoc()));
        expr = SE->getSubExpr();
      } else if (auto UDE = dyn_cast<UnresolvedDotExpr>(expr)) {
        // .foo
        components.push_back(KeyPathExpr::Component::forUnresolvedProperty(
            UDE->getName(), UDE->getLoc()));

        expr = UDE->getBase();
      } else if (auto SE = dyn_cast<SubscriptExpr>(expr)) {
        // .[0] or just plain [0]
        components.push_back(
            KeyPathExpr::Component::forUnresolvedSubscriptWithPrebuiltIndexExpr(
                TC.Context,
                SE->getIndex(), SE->getArgumentLabels(), SE->getLoc()));

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
      } else {
        if (emitErrors) {
          // \(<expr>) may be an attempt to write a string interpolation outside
          // of a string literal; diagnose this case specially.
          if (isa<ParenExpr>(expr) || isa<TupleExpr>(expr)) {
            TC.diagnose(expr->getLoc(),
                        diag::expr_string_interpolation_outside_string);
          } else {
            TC.diagnose(expr->getLoc(),
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
        TC.diagnose(root->getLoc(),
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
    TC.diagnose(KPE->getLoc(), diag::expr_swift_keypath_empty);
    // Passes further down the pipeline expect keypaths to always have at least
    // one component, so stuff an invalid component in the AST for recovery.
    components.push_back(KeyPathExpr::Component());
  }

  std::reverse(components.begin(), components.end());

  KPE->setRootType(rootType);
  KPE->resolveComponents(TC.Context, components);
}

Expr *PreCheckExpression::simplifyTypeConstructionWithLiteralArg(Expr *E) {
  // If constructor call is expected to produce an optional let's not attempt
  // this optimization because literal initializers aren't failable.
  if (!TC.getLangOpts().isSwiftVersionAtLeast(5)) {
    if (!ExprStack.empty()) {
      auto *parent = ExprStack.back();
      if (isa<BindOptionalExpr>(parent) || isa<ForceValueExpr>(parent))
        return nullptr;
    }
  }

  auto *call = dyn_cast<CallExpr>(E);
  if (!call || call->getNumArguments() != 1)
    return nullptr;

  auto *typeExpr = dyn_cast<TypeExpr>(call->getFn());
  if (!typeExpr)
    return nullptr;

  auto *argExpr = call->getArg()->getSemanticsProvidingExpr();
  auto *literal = dyn_cast<LiteralExpr>(argExpr);
  if (!literal)
    return nullptr;

  auto *protocol = TC.getLiteralProtocol(literal);
  if (!protocol)
    return nullptr;

  Type type;
  if (typeExpr->getTypeLoc().wasValidated()) {
    type = typeExpr->getTypeLoc().getType();
  } else if (auto *rep = typeExpr->getTypeRepr()) {
    TypeResolutionOptions options(TypeResolverContext::InExpression);
    options |= TypeResolutionFlags::AllowUnboundGenerics;
    auto resolution = TypeResolution::forContextual(DC);
    type = resolution.resolveType(rep, options);
    typeExpr->getTypeLoc().setType(type);
  }

  if (!type)
    return nullptr;

  // Don't bother to convert deprecated selector syntax.
  if (auto selectorTy = TC.getObjCSelectorType(DC)) {
    if (type->isEqual(selectorTy))
      return nullptr;
  }

  ConformanceCheckOptions options;
  options |= ConformanceCheckFlags::InExpression;
  options |= ConformanceCheckFlags::SkipConditionalRequirements;

  auto result = TC.conformsToProtocol(type, protocol, DC, options);
  if (!result || !result->isConcrete())
    return nullptr;

  return CoerceExpr::forLiteralInit(TC.Context, argExpr, call->getSourceRange(),
                                    typeExpr->getTypeLoc());
}

/// Pre-check the expression, validating any types that occur in the
/// expression and folding sequence expressions.
bool TypeChecker::preCheckExpression(Expr *&expr, DeclContext *dc) {
  PreCheckExpression preCheck(*this, dc, expr);
  // Perform the pre-check.
  if (auto result = expr->walk(preCheck)) {
    expr = result;
    return false;
  }
  return true;
}

ExprTypeCheckListener::~ExprTypeCheckListener() { }

bool ExprTypeCheckListener::builtConstraints(ConstraintSystem &cs, Expr *expr) {
  return false;
}

Expr *ExprTypeCheckListener::foundSolution(Solution &solution, Expr *expr) {
  return expr;
}

Expr *ExprTypeCheckListener::appliedSolution(Solution &solution, Expr *expr) {
  return expr;
}

void ParentConditionalConformance::diagnoseConformanceStack(
    DiagnosticEngine &diags, SourceLoc loc,
    ArrayRef<ParentConditionalConformance> conformances) {
  for (auto history : reversed(conformances)) {
    diags.diagnose(loc, diag::requirement_implied_by_conditional_conformance,
                   history.ConformingType, history.Protocol);
  }
}

GenericRequirementsCheckListener::~GenericRequirementsCheckListener() {}

bool GenericRequirementsCheckListener::shouldCheck(RequirementKind kind,
                                                   Type first, Type second) {
  return true;
}

void GenericRequirementsCheckListener::satisfiedConformance(
                                          Type depTy, Type replacementTy,
                                          ProtocolConformanceRef conformance) {
}

bool GenericRequirementsCheckListener::diagnoseUnsatisfiedRequirement(
    const Requirement &req, Type first, Type second,
    ArrayRef<ParentConditionalConformance> parents) {
  return false;
}

#pragma mark High-level entry points
Type TypeChecker::typeCheckExpression(Expr *&expr, DeclContext *dc,
                                      TypeLoc convertType,
                                      ContextualTypePurpose convertTypePurpose,
                                      TypeCheckExprOptions options,
                                      ExprTypeCheckListener *listener,
                                      ConstraintSystem *baseCS) {
  FrontendStatsTracer StatsTracer(Context.Stats, "typecheck-expr", expr);
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);

  // First, pre-check the expression, validating any types that occur in the
  // expression and folding sequence expressions.
  if (preCheckExpression(expr, dc))
    return Type();

  // Construct a constraint system from this expression.
  ConstraintSystemOptions csOptions = ConstraintSystemFlags::AllowFixes;

  if (options.contains(TypeCheckExprFlags::SuppressDiagnostics))
    csOptions |= ConstraintSystemFlags::SuppressDiagnostics;

  if (options.contains(TypeCheckExprFlags::AllowUnresolvedTypeVariables))
    csOptions |= ConstraintSystemFlags::AllowUnresolvedTypeVariables;

  ConstraintSystem cs(*this, dc, csOptions);
  cs.baseCS = baseCS;

  // Verify that a purpose was specified if a convertType was.  Note that it is
  // ok to have a purpose without a convertType (which is used for call
  // return types).
  assert((!convertType.getType() || convertTypePurpose != CTP_Unused) &&
         "Purpose for conversion type was not specified");

  // Take a look at the conversion type to check to make sure it is sensible.
  if (auto type = convertType.getType()) {
    // If we're asked to convert to an UnresolvedType, then ignore the request.
    // This happens when CSDiags nukes a type.
    if (type->is<UnresolvedType>() ||
        (type->is<MetatypeType>() && type->hasUnresolvedType())) {
      convertType = TypeLoc();
      convertTypePurpose = CTP_Unused;
    }
  }

  // Tell the constraint system what the contextual type is.  This informs
  // diagnostics and is a hint for various performance optimizations.
  cs.setContextualType(expr, convertType, convertTypePurpose);

  // If the convertType is *only* provided for that hint, then null it out so
  // that we don't later treat it as an actual conversion constraint.
  if (options.contains(TypeCheckExprFlags::ConvertTypeIsOnlyAHint))
    convertType = TypeLoc();

  // If the client can handle unresolved type variables, leave them in the
  // system.
  auto allowFreeTypeVariables = FreeTypeVariableBinding::Disallow;
  if (options.contains(TypeCheckExprFlags::AllowUnresolvedTypeVariables))
    allowFreeTypeVariables = FreeTypeVariableBinding::UnresolvedType;

  Type convertTo = convertType.getType();
  if (options.contains(TypeCheckExprFlags::ExpressionTypeMustBeOptional)) {
    assert(!convertTo && "convertType and type check options conflict");
    auto *convertTypeLocator = cs.getConstraintLocator(
        cs.getConstraintLocator(expr), ConstraintLocator::ContextualType);
    Type var = cs.createTypeVariable(convertTypeLocator);
    convertTo = getOptionalType(expr->getLoc(), var);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(expr, convertTo, listener, viable,
               allowFreeTypeVariables))
    return Type();

  // If the client allows the solution to have unresolved type expressions,
  // check for them now.  We cannot apply the solution with unresolved TypeVars,
  // because they will leak out into arbitrary places in the resultant AST.
  if (options.contains(TypeCheckExprFlags::AllowUnresolvedTypeVariables) &&
      (viable.size() != 1 ||
       (convertType.getType() && convertType.getType()->hasUnresolvedType()))) {
    return ErrorType::get(Context);
  }

  auto result = expr;
  auto &solution = viable[0];
  if (listener) {
    result = listener->foundSolution(solution, result);
    if (!result)
      return Type();
  }

  if (options.contains(TypeCheckExprFlags::SkipApplyingSolution))
    return solution.simplifyType(cs.getType(expr));

  // Apply the solution to the expression.
  result = cs.applySolution(
      solution, result, convertType.getType(),
      options.contains(TypeCheckExprFlags::IsDiscarded),
      options.contains(TypeCheckExprFlags::SkipMultiStmtClosures));
  if (!result) {
    // Failure already diagnosed, above, as part of applying the solution.
    return Type();
  }

  // If there's a listener, notify it that we've applied the solution.
  if (listener) {
    result = listener->appliedSolution(solution, result);
    if (!result) {
      return Type();
    }
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
    log << "\n";
  }

  // Unless the client has disabled them, perform syntactic checks on the
  // expression now.
  if (!cs.shouldSuppressDiagnostics() &&
      !options.contains(TypeCheckExprFlags::DisableStructuralChecks)) {
    bool isExprStmt = options.contains(TypeCheckExprFlags::IsExprStmt);
    performSyntacticExprDiagnostics(*this, result, dc, isExprStmt);
  }

  expr = result;
  return cs.getType(expr);
}

Type TypeChecker::typeCheckParameterDefault(Expr *&defaultValue,
                                            DeclContext *DC, Type paramType,
                                            bool isAutoClosure, bool canFail) {
  assert(paramType && !paramType->hasError());

  if (isAutoClosure) {
    class AutoClosureListener : public ExprTypeCheckListener {
      DeclContext *DC;
      FunctionType *ParamType;

    public:
      AutoClosureListener(DeclContext *DC, FunctionType *paramType)
          : DC(DC), ParamType(paramType) {}

      Expr *appliedSolution(constraints::Solution &solution,
                            Expr *expr) override {
        auto &cs = solution.getConstraintSystem();
        auto *closure = cs.TC.buildAutoClosureExpr(DC, expr, ParamType);
        cs.cacheExprTypes(closure);
        return closure;
      }
    };

    auto *fnType = paramType->castTo<FunctionType>();
    AutoClosureListener listener(DC, fnType);
    return typeCheckExpression(defaultValue, DC,
                               TypeLoc::withoutLoc(fnType->getResult()),
                               canFail ? CTP_DefaultParameter : CTP_CannotFail,
                               TypeCheckExprOptions(), &listener);
  }

  return typeCheckExpression(defaultValue, DC, TypeLoc::withoutLoc(paramType),
                             canFail ? CTP_DefaultParameter : CTP_CannotFail);
}

Type TypeChecker::
getTypeOfExpressionWithoutApplying(Expr *&expr, DeclContext *dc,
                                   ConcreteDeclRef &referencedDecl,
                                 FreeTypeVariableBinding allowFreeTypeVariables,
                                   ExprTypeCheckListener *listener) {
  FrontendStatsTracer StatsTracer(Context.Stats, "typecheck-expr-no-apply", expr);
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);
  referencedDecl = nullptr;

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc, ConstraintSystemFlags::SuppressDiagnostics);

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  const Type originalType = expr->getType();
  const bool needClearType = originalType && originalType->hasError();
  const auto recoverOriginalType = [&] () {
    if (needClearType)
      expr->setType(originalType);
  };

  // If the previous checking gives the expr error type, clear the result and
  // re-check.
  if (needClearType)
    expr->setType(Type());
  if (cs.solve(expr, /*convertType*/Type(), listener, viable,
               allowFreeTypeVariables)) {
    recoverOriginalType();
    return Type();
  }

  // Get the expression's simplified type.
  auto &solution = viable[0];
  auto &solutionCS = solution.getConstraintSystem();
  Type exprType = solution.simplifyType(solutionCS.getType(expr));

  assert(exprType && !exprType->hasTypeVariable() &&
         "free type variable with FreeTypeVariableBinding::GenericParameters?");

  if (exprType->hasError()) {
    recoverOriginalType();
    return Type();
  }

  // Dig the declaration out of the solution.
  auto semanticExpr = expr->getSemanticsProvidingExpr();
  auto topLocator = cs.getConstraintLocator(semanticExpr);
  referencedDecl = solution.resolveLocatorToDecl(topLocator);

  if (!referencedDecl.getDecl()) {
    // Do another check in case we have a curried call from binding a function
    // reference to a variable, for example:
    //
    //   class C {
    //     func instanceFunc(p1: Int, p2: Int) {}
    //   }
    //   func t(c: C) {
    //     C.instanceFunc(c)#^COMPLETE^#
    //   }
    //
    // We need to get the referenced function so we can complete the argument
    // labels. (Note that the requirement to have labels in the curried call
    // seems inconsistent with the removal of labels from function types.
    // If this changes the following code could be removed).
    if (auto *CE = dyn_cast<CallExpr>(semanticExpr)) {
      if (auto *UDE = dyn_cast<UnresolvedDotExpr>(CE->getFn())) {
        if (isa<TypeExpr>(UDE->getBase())) {
          auto udeLocator = cs.getConstraintLocator(UDE);
          auto udeRefDecl = solution.resolveLocatorToDecl(udeLocator);
          if (auto *FD = dyn_cast_or_null<FuncDecl>(udeRefDecl.getDecl())) {
            if (FD->isInstanceMember())
              referencedDecl = udeRefDecl;
          }
        }
      }
    }
  }

  // Recover the original type if needed.
  recoverOriginalType();
  return exprType;
}

void TypeChecker::getPossibleTypesOfExpressionWithoutApplying(
    Expr *&expr, DeclContext *dc, SmallPtrSetImpl<TypeBase *> &types,
    FreeTypeVariableBinding allowFreeTypeVariables,
    ExprTypeCheckListener *listener) {
  FrontendStatsTracer StatsTracer(Context.Stats, "get-possible-types-no-apply", expr);
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);

  // Construct a constraint system from this expression.
  ConstraintSystemOptions options;
  options |= ConstraintSystemFlags::ReturnAllDiscoveredSolutions;
  options |= ConstraintSystemFlags::SuppressDiagnostics;

  ConstraintSystem cs(*this, dc, options);

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;

  const Type originalType = expr->getType();
  if (originalType && originalType->hasError())
    expr->setType(Type());

  cs.solve(expr, /*convertType*/ Type(), listener, viable,
           allowFreeTypeVariables);

  for (auto &solution : viable) {
    auto exprType = solution.simplifyType(cs.getType(expr));
    assert(exprType && !exprType->hasTypeVariable());
    types.insert(exprType.getPointer());
  }
}

static Type getTypeOfCompletionOperatorImpl(TypeChecker &TC, DeclContext *DC,
                                            Expr *expr,
                                            ConcreteDeclRef &referencedDecl) {
  ASTContext &Context = TC.Context;

  FrontendStatsTracer StatsTracer(Context.Stats,
                                  "typecheck-completion-operator", expr);
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);

  // Construct a constraint system from this expression.
  ConstraintSystem CS(TC, DC, ConstraintSystemFlags::SuppressDiagnostics);
  expr = CS.generateConstraints(expr);
  if (!expr)
    return Type();

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->dump(log);
    log << "\n";
    CS.print(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (CS.solve(expr, viable, FreeTypeVariableBinding::Disallow))
    return Type();

  auto &solution = viable[0];
  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(log);
  }

  // Fill the results.
  Expr *opExpr = cast<ApplyExpr>(expr)->getFn();
  referencedDecl =
      solution.resolveLocatorToDecl(CS.getConstraintLocator(opExpr));

  // Return '(ArgType[, ArgType]) -> ResultType' as a function type.
  // We don't use the type of the operator expression because we want the types
  // of the *arguments* instead of the types of the parameters.
  Expr *argsExpr = cast<ApplyExpr>(expr)->getArg();
  SmallVector<FunctionType::Param, 2> argTypes;
  if (auto *PE = dyn_cast<ParenExpr>(argsExpr)) {
    argTypes.emplace_back(solution.simplifyType(CS.getType(PE->getSubExpr())));
  } else if (auto *TE = dyn_cast<TupleExpr>(argsExpr)) {
    for (auto arg : TE->getElements())
      argTypes.emplace_back(solution.simplifyType(CS.getType(arg)));
  }

  return FunctionType::get(argTypes, solution.simplifyType(CS.getType(expr)));
}

/// \brief Return the type of operator function for specified LHS, or a null
/// \c Type on error.
Type TypeChecker::getTypeOfCompletionOperator(DeclContext *DC, Expr *LHS,
                                              Identifier opName,
                                              DeclRefKind refKind,
                                              ConcreteDeclRef &referencedDecl) {

  // For the infix operator, find the actual LHS from pre-folded LHS.
  if (refKind == DeclRefKind::BinaryOperator)
    LHS = findLHS(DC, LHS, opName);

  if (!LHS)
    return Type();

  auto LHSTy = LHS->getType();

  // FIXME: 'UnresolvedType' still might be typechecked by an operator.
  if (!LHSTy || LHSTy->is<UnresolvedType>())
    return Type();

  // Meta types and function types cannot be a operand of operator expressions.
  if (LHSTy->is<MetatypeType>() || LHSTy->is<AnyFunctionType>())
    return Type();

  auto Loc = LHS->getEndLoc();

  // Build temporary expression to typecheck.
  // We allocate these expressions on the stack because we know they can't
  // escape and there isn't a better way to allocate scratch Expr nodes.
  FixedTypeExpr dummyLHS(LHS->getSourceRange(), LHSTy);
  UnresolvedDeclRefExpr UDRE(opName, refKind, DeclNameLoc(Loc));
  auto *opExpr = resolveDeclRefExpr(&UDRE, DC);

  switch (refKind) {

  case DeclRefKind::PostfixOperator: {
    // (postfix_unary_expr
    //   (declref_expr name=<operator>)
    //   (paren_expr
    //     (<LHS>)))
    ParenExpr Args(SourceLoc(), &dummyLHS, SourceLoc(),
                   /*hasTrailingClosure=*/false);
    PostfixUnaryExpr postfixExpr(opExpr, &Args);
    return getTypeOfCompletionOperatorImpl(*this, DC, &postfixExpr,
                                           referencedDecl);
  }

  case DeclRefKind::BinaryOperator: {
    // (binary_expr
    //   (declref_expr name=<opName>)
    //   (tuple_expr
    //     (<LHS>)
    //     (code_completion_expr)))
    CodeCompletionExpr dummyRHS(Loc);
    auto Args = TupleExpr::create(
        Context, SourceLoc(), {&dummyLHS, &dummyRHS}, {}, {}, SourceLoc(),
        /*hasTrailingClosure=*/false, /*isImplicit=*/true);
    BinaryExpr binaryExpr(opExpr, Args, /*isImplicit=*/true);

    Type ty =
        getTypeOfCompletionOperatorImpl(*this, DC, &binaryExpr, referencedDecl);
    if (!ty)
      return ty;

    auto funcTy = ty->castTo<AnyFunctionType>();
    Type rhsTy = funcTy->getParams()[1].getPlainType();
    Type resultTy = funcTy->getResult();

    // Don't complete optional operators on non-optional types.
    if (!LHSTy->getRValueType()->getOptionalObjectType()) {
      // 'T ?? T'
      if (opName.str() == "??")
        return Type();
      // 'T == nil'
      if (auto NT = rhsTy->getNominalOrBoundGenericNominal())
        if (NT->getName() == Context.Id_OptionalNilComparisonType)
          return Type();
    }

    // If the right-hand side and result type are both type parameters, we're
    // not providing a useful completion.
    if (resultTy->isTypeParameter() && rhsTy->isTypeParameter())
      return Type();

    return ty;
  }
  default:
    llvm_unreachable("Invalid DeclRefKind for operator completion");
  }
}

bool TypeChecker::typeCheckBinding(Pattern *&pattern, Expr *&initializer,
                                   DeclContext *DC, bool skipApplyingSolution) {

  /// Type checking listener for pattern binding initializers.
  class BindingListener : public ExprTypeCheckListener {
    Pattern *&pattern;
    Expr *&initializer;

    /// The locator we're using.
    ConstraintLocator *Locator;

    /// The type of the initializer.
    llvm::PointerIntPair<Type, 1, bool> InitTypeAndInOut;

  public:
    explicit BindingListener(Pattern *&pattern, Expr *&initializer)
      : pattern(pattern), initializer(initializer),
        Locator(nullptr), InitTypeAndInOut(Type(), false) { }

    Type getInitType() const { return InitTypeAndInOut.getPointer(); }
    bool isInOut() const { return InitTypeAndInOut.getInt(); }

    bool builtConstraints(ConstraintSystem &cs, Expr *expr) override {
      // Save the locator we're using for the expression.
      Locator = cs.getConstraintLocator(expr);

      // Collect constraints from the pattern.
      InitTypeAndInOut.setPointer(cs.generateConstraints(pattern, Locator));
      InitTypeAndInOut.setInt(expr->isSemanticallyInOutExpr());
      if (!InitTypeAndInOut.getPointer())
        return true;

      assert(!InitTypeAndInOut.getPointer()->is<InOutType>());
      // Add a conversion constraint between the types.
      cs.addConstraint(ConstraintKind::Conversion, cs.getType(expr),
                       InitTypeAndInOut.getPointer(), Locator,
                       /*isFavored*/true);

      // The expression has been pre-checked; save it in case we fail later.
      initializer = expr;
      return false;
    }

    Expr *foundSolution(Solution &solution, Expr *expr) override {
      // Figure out what type the constraints decided on.
      auto ty = solution.simplifyType(InitTypeAndInOut.getPointer());
      InitTypeAndInOut.setPointer(
          ty->getRValueType()->reconstituteSugar(/*recursive =*/false));
      InitTypeAndInOut.setInt(expr->isSemanticallyInOutExpr());

      // Just keep going.
      return expr;
    }

    Expr *appliedSolution(Solution &solution, Expr *expr) override {
      // Convert the initializer to the type of the pattern.
      // ignoreTopLevelInjection = Binding->isConditional()
      expr = solution.coerceToType(expr, InitTypeAndInOut.getPointer(), Locator,
                                   false /* ignoreTopLevelInjection */);
      if (!expr) {
        return nullptr;
      }

      assert(solution.getConstraintSystem().getType(expr)->isEqual(InitTypeAndInOut.getPointer()));

      initializer = expr;
      return expr;
    }
  };

  assert(initializer && "type-checking an uninitialized binding?");
  BindingListener listener(pattern, initializer);

  TypeLoc contextualType;
  auto contextualPurpose = CTP_Unused;
  TypeCheckExprOptions flags = TypeCheckExprFlags::ConvertTypeIsOnlyAHint;

  if (pattern->hasType()) {
    contextualType = TypeLoc::withoutLoc(pattern->getType());
    contextualPurpose = CTP_Initialization;

    // If we already had an error, don't repeat the problem.
    if (contextualType.getType()->hasError())
      return true;

    // Only provide a TypeLoc if it makes sense to allow diagnostics.
    if (auto *typedPattern = dyn_cast<TypedPattern>(pattern)) {
      const Pattern *inner = typedPattern->getSemanticsProvidingPattern();
      if (isa<NamedPattern>(inner) || isa<AnyPattern>(inner))
        contextualType = typedPattern->getTypeLoc();
    }
  } else if (isa<OptionalSomePattern>(pattern)) {
    flags |= TypeCheckExprFlags::ExpressionTypeMustBeOptional;
  }
    
  // Type-check the initializer.
  if (skipApplyingSolution)
    flags |= TypeCheckExprFlags::SkipApplyingSolution;

  auto resultTy = typeCheckExpression(initializer, DC, contextualType,
                                      contextualPurpose, flags, &listener);
  assert(!listener.isInOut());

  if (resultTy) {
    TypeResolutionOptions options =
        isa<EditorPlaceholderExpr>(initializer->getSemanticsProvidingExpr())
        ? TypeResolverContext::EditorPlaceholderExpr
        : TypeResolverContext::InExpression;
    options |= TypeResolutionFlags::OverrideType;

    // FIXME: initTy should be the same as resultTy; now that typeCheckExpression()
    // returns a Type and not bool, we should be able to simplify the listener
    // implementation here.
    auto initTy = listener.getInitType();
    if (initTy->hasDependentMember())
      return true;

    // Apply the solution to the pattern as well.
    if (coercePatternToType(pattern, TypeResolution::forContextual(DC), initTy,
                            options, TypeLoc())) {
      return true;
    }
  }

  if (!resultTy && !initializer->getType())
    initializer->setType(ErrorType::get(Context));

  // If the type of the pattern is inferred, assign error types to the pattern
  // and its variables, to prevent it from being referenced by the constraint
  // system.
  if (!resultTy &&
      (!pattern->hasType() || pattern->getType()->hasUnboundGenericType())) {
    pattern->setType(ErrorType::get(Context));
    pattern->forEachVariable([&](VarDecl *var) {
      // Don't change the type of a variable that we've been able to
      // compute a type for.
      if (var->hasType() &&
          !var->getType()->hasUnboundGenericType() &&
          !var->getType()->hasError())
        return;

      var->markInvalid();
    });
  }

  return !resultTy;
}

bool TypeChecker::typeCheckPatternBinding(PatternBindingDecl *PBD,
                                          unsigned patternNumber,
                                          bool skipApplyingSolution) {
  auto &ctx = PBD->getASTContext();
  const auto &pbe = PBD->getPatternList()[patternNumber];
  Pattern *pattern = PBD->getPattern(patternNumber);
  Expr *init = PBD->getInit(patternNumber);

  if (!init) {
    PBD->setInvalid();
    return true;
  }

  // Enter an initializer context if necessary.
  PatternBindingInitializer *initContext = nullptr;
  DeclContext *DC = PBD->getDeclContext();
  if (!DC->isLocalContext()) {
    initContext = cast_or_null<PatternBindingInitializer>(pbe.getInitContext());
    if (initContext)
      DC = initContext;
  }

  bool hadError = typeCheckBinding(pattern, init, DC, skipApplyingSolution);
  PBD->setPattern(patternNumber, pattern, initContext);
  PBD->setInit(patternNumber, init);

  // Add the attribute that preserves the "has an initializer" value across
  // module generation, as required for TBDGen.
  PBD->getPattern(patternNumber)->forEachVariable([&](VarDecl *VD) {
    if (VD->hasStorage() && !VD->getAttrs().hasAttribute<HasInitialValueAttr>())
      VD->getAttrs().add(new (ctx) HasInitialValueAttr(/*IsImplicit=*/true));
  });

  if (!hadError) {
    // If we're performing an binding to a weak or unowned variable from a
    // constructor call, emit a warning that the instance will be immediately
    // deallocated.
    diagnoseUnownedImmediateDeallocation(*this, pattern, pbe.getEqualLoc(),
                                         init);

    // If we entered an initializer context, contextualize any
    // auto-closures we might have created.
    if (initContext) {
      // Check safety of error-handling in the declaration, too.
      checkInitializerErrorHandling(initContext, init);
      (void)contextualizeInitializer(initContext, init);
    }
  } else {
    PBD->setInvalid();
  }

  PBD->setInitializerChecked(patternNumber);
  return hadError;
}

bool TypeChecker::typeCheckForEachBinding(DeclContext *dc, ForEachStmt *stmt) {
  /// Type checking listener for for-each binding.
  class BindingListener : public ExprTypeCheckListener {
    /// The for-each statement.
    ForEachStmt *Stmt;

    /// The locator we're using.
    ConstraintLocator *Locator;

    /// The type of the initializer.
    Type InitType;

    /// The type of the sequence.
    Type SequenceType;

  public:
    explicit BindingListener(ForEachStmt *stmt) : Stmt(stmt) { }

    bool builtConstraints(ConstraintSystem &cs, Expr *expr) override {
      // Save the locator we're using for the expression.
      Locator = cs.getConstraintLocator(expr);

      // The expression type must conform to the Sequence.
      auto &tc = cs.getTypeChecker();
      ProtocolDecl *sequenceProto
        = tc.getProtocol(Stmt->getForLoc(), KnownProtocolKind::Sequence);
      if (!sequenceProto) {
        return true;
      }

      SequenceType = cs.createTypeVariable(Locator);
      cs.addConstraint(ConstraintKind::Conversion, cs.getType(expr),
                       SequenceType, Locator);
      cs.addConstraint(ConstraintKind::ConformsTo, SequenceType,
                       sequenceProto->getDeclaredType(), Locator);

      auto iteratorLocator =
        cs.getConstraintLocator(Locator,
                                ConstraintLocator::SequenceIteratorProtocol);
      auto elementLocator =
        cs.getConstraintLocator(iteratorLocator,
                                ConstraintLocator::GeneratorElementType);

      // Collect constraints from the element pattern.
      auto pattern = Stmt->getPattern();
      InitType = cs.generateConstraints(pattern, elementLocator);
      if (!InitType)
        return true;
      
      // Manually search for the iterator witness. If no iterator/element pair
      // exists, solve for them.
      Type iteratorType;
      Type elementType;
      
      NameLookupOptions lookupOptions = defaultMemberTypeLookupOptions;
      if (isa<AbstractFunctionDecl>(cs.DC))
        lookupOptions |= NameLookupFlags::KnownPrivate;

      auto sequenceType = cs.getType(expr)->getRValueType();

      // Look through one level of optional; this improves recovery but doesn't
      // change the result.
      if (auto sequenceObjectType = sequenceType->getOptionalObjectType())
        sequenceType = sequenceObjectType;

      // If the sequence type is an existential, we should not attempt to
      // look up the member type at all, since we cannot represent associated
      // types of existentials.
      //
      // We will diagnose it later.
      if (!sequenceType->isExistentialType() &&
          (sequenceType->mayHaveMembers() ||
           sequenceType->isTypeVariableOrMember())) {
        ASTContext &ctx = tc.Context;
        auto iteratorAssocType =
          cast<AssociatedTypeDecl>(
            sequenceProto->lookupDirect(ctx.Id_Iterator).front());

        auto subs = sequenceType->getContextSubstitutionMap(
          cs.DC->getParentModule(),
          sequenceProto);
        iteratorType = iteratorAssocType->getDeclaredInterfaceType()
          .subst(subs);

        if (iteratorType) {
          auto iteratorProto =
            tc.getProtocol(Stmt->getForLoc(),
                           KnownProtocolKind::IteratorProtocol);
          if (!iteratorProto)
            return true;

          auto elementAssocType =
            cast<AssociatedTypeDecl>(
              iteratorProto->lookupDirect(ctx.Id_Element).front());

          elementType = iteratorType->getTypeOfMember(
                          cs.DC->getParentModule(),
                          elementAssocType,
                          elementAssocType->getDeclaredInterfaceType());
        }
      }

      if (elementType.isNull()) {
        elementType = cs.createTypeVariable(elementLocator);
      }

      // Add a conversion constraint between the element type of the sequence
      // and the type of the element pattern.
      cs.addConstraint(ConstraintKind::Conversion, elementType, InitType,
                       elementLocator);

      Stmt->setSequence(expr);
      return false;
    }

    Expr *appliedSolution(Solution &solution, Expr *expr) override {
      // Figure out what types the constraints decided on.
      auto &cs = solution.getConstraintSystem();
      auto &tc = cs.getTypeChecker();
      InitType = solution.simplifyType(InitType);
      SequenceType = solution.simplifyType(SequenceType);

      // Perform any necessary conversions of the sequence (e.g. [T]! -> [T]).
      if (tc.convertToType(expr, SequenceType, cs.DC)) {
        return nullptr;
      }

      cs.cacheExprTypes(expr);

      // Apply the solution to the iteration pattern as well.
      Pattern *pattern = Stmt->getPattern();
      TypeResolutionOptions options(TypeResolverContext::ForEachStmt);
      options |= TypeResolutionFlags::OverrideType;
      if (tc.coercePatternToType(pattern, TypeResolution::forContextual(cs.DC),
                                 InitType, options)) {
        return nullptr;
      }

      Stmt->setPattern(pattern);
      Stmt->setSequence(expr);

      cs.setExprTypes(expr);
      return expr;
    }
  };

  BindingListener listener(stmt);
  Expr *seq = stmt->getSequence();
  assert(seq && "type-checking an uninitialized for-each statement?");

  // Type-check the for-each loop sequence and element pattern.
  auto resultTy = typeCheckExpression(seq, dc, &listener);
  return !resultTy;
}

bool TypeChecker::typeCheckCondition(Expr *&expr, DeclContext *dc) {
  // If this expression is already typechecked and has an i1 type, then it has
  // already got its conversion from Boolean back to i1.  Just re-typecheck
  // it.
  if (expr->getType() && expr->getType()->isBuiltinIntegerType(1)) {
    auto resultTy = typeCheckExpression(expr, dc);
    return !resultTy;
  }

  /// Expression type checking listener for conditions.
  class ConditionListener : public ExprTypeCheckListener {
    Expr *OrigExpr = nullptr;

  public:
    // Add the appropriate Boolean constraint.
    bool builtConstraints(ConstraintSystem &cs, Expr *expr) override {
      // Save the original expression.
      OrigExpr = expr;
      
      // Otherwise, the result must be convertible to Bool.
      auto boolDecl = cs.getASTContext().getBoolDecl();
      if (!boolDecl)
        return true;
      
      // Condition must convert to Bool.
      cs.addConstraint(ConstraintKind::Conversion, cs.getType(expr),
                       boolDecl->getDeclaredType(),
                       cs.getConstraintLocator(expr));
      return false;
    }

    // Convert the result to a Builtin.i1.
    Expr *appliedSolution(constraints::Solution &solution,
                          Expr *expr) override {
      auto &cs = solution.getConstraintSystem();

      auto converted =
        solution.convertBooleanTypeToBuiltinI1(expr,
                                             cs.getConstraintLocator(OrigExpr));
      cs.setExprTypes(converted);
      return converted;
    }
    
  };

  ConditionListener listener;
  auto resultTy = typeCheckExpression(expr, dc, &listener);
  return !resultTy;
}

bool TypeChecker::typeCheckStmtCondition(StmtCondition &cond, DeclContext *dc,
                                         Diag<> diagnosticForAlwaysTrue) {
  bool hadError = false;
  bool hadAnyFalsable = false;
  for (auto &elt : cond) {
    if (elt.getKind() == StmtConditionElement::CK_Availability) {
      hadAnyFalsable = true;
      continue;
    }

    if (auto E = elt.getBooleanOrNull()) {
      hadError |= typeCheckCondition(E, dc);
      elt.setBoolean(E);
      hadAnyFalsable = true;
      continue;
    }

    // This is cleanup goop run on the various paths where type checking of the
    // pattern binding fails.
    auto typeCheckPatternFailed = [&] {
      hadError = true;
      elt.getPattern()->setType(ErrorType::get(Context));
      elt.getInitializer()->setType(ErrorType::get(Context));

      elt.getPattern()->forEachVariable([&](VarDecl *var) {
        // Don't change the type of a variable that we've been able to
        // compute a type for.
        if (var->hasType() && !var->getType()->hasError())
          return;
        var->markInvalid();
      });
    };

    // Resolve the pattern.
    auto *pattern = resolvePattern(elt.getPattern(), dc,
                                   /*isStmtCondition*/true);
    if (!pattern) {
      typeCheckPatternFailed();
      continue;
    }
    elt.setPattern(pattern);

    // Check the pattern, it allows unspecified types because the pattern can
    // provide type information.
    TypeResolutionOptions options(TypeResolverContext::InExpression);
    options |= TypeResolutionFlags::AllowUnspecifiedTypes;
    options |= TypeResolutionFlags::AllowUnboundGenerics;
    if (typeCheckPattern(pattern, dc, options)) {
      typeCheckPatternFailed();
      continue;
    }

    // If the pattern didn't get a type, it's because we ran into some
    // unknown types along the way. We'll need to check the initializer.
    auto init = elt.getInitializer();
    hadError |= typeCheckBinding(pattern, init, dc,
                                 /*skipApplyingSolution*/false);
    elt.setPattern(pattern);
    elt.setInitializer(init);
    hadAnyFalsable |= pattern->isRefutablePattern();
  }

  
  // If the binding is not refutable, and there *is* an else, reject it as
  // unreachable.
  if (!hadAnyFalsable && !hadError)
    diagnose(cond[0].getStartLoc(), diagnosticForAlwaysTrue);
  
  return false;
}

/// Find the '~=` operator that can compare an expression inside a pattern to a
/// value of a given type.
bool TypeChecker::typeCheckExprPattern(ExprPattern *EP, DeclContext *DC,
                                       Type rhsType) {
  FrontendStatsTracer StatsTracer(Context.Stats, "typecheck-expr-pattern", EP);
  PrettyStackTracePattern stackTrace(Context, "type-checking", EP);

  // Create a 'let' binding to stand in for the RHS value.
  auto *matchVar = new (Context) VarDecl(/*IsStatic*/false,
                                         VarDecl::Specifier::Let,
                                         /*IsCaptureList*/false,
                                         EP->getLoc(),
                                         Context.getIdentifier("$match"),
                                         DC);
  matchVar->setType(rhsType);
  matchVar->setInterfaceType(rhsType->mapTypeOutOfContext());

  matchVar->setImplicit();
  EP->setMatchVar(matchVar);
  matchVar->setHasNonPatternBindingInit();

  // Find '~=' operators for the match.
  auto lookupOptions = defaultUnqualifiedLookupOptions;
  lookupOptions |= NameLookupFlags::KnownPrivate;
  auto matchLookup = lookupUnqualified(DC, Context.Id_MatchOperator,
                                       SourceLoc(), lookupOptions);
  if (!matchLookup) {
    diagnose(EP->getLoc(), diag::no_match_operator);
    return true;
  }
  
  SmallVector<ValueDecl*, 4> choices;
  for (auto &result : matchLookup) {
    choices.push_back(result.getValueDecl());
  }
  
  if (choices.empty()) {
    diagnose(EP->getLoc(), diag::no_match_operator);
    return true;
  }
  
  // Build the 'expr ~= var' expression.
  // FIXME: Compound name locations.
  auto *matchOp = buildRefExpr(choices, DC, DeclNameLoc(EP->getLoc()),
                               /*Implicit=*/true, FunctionRefKind::Compound);
  auto *matchVarRef = new (Context) DeclRefExpr(matchVar,
                                                DeclNameLoc(EP->getLoc()),
                                                /*Implicit=*/true);
  
  Expr *matchArgElts[] = {EP->getSubExpr(), matchVarRef};
  auto *matchArgs
    = TupleExpr::create(Context, EP->getSubExpr()->getSourceRange().Start,
                        matchArgElts, { }, { },
                        EP->getSubExpr()->getSourceRange().End,
                        /*HasTrailingClosure=*/false, /*Implicit=*/true);
  
  Expr *matchCall = new (Context) BinaryExpr(matchOp, matchArgs,
                                             /*Implicit=*/true);
  
  // Check the expression as a condition.
  bool hadError = typeCheckCondition(matchCall, DC);
  // Save the type-checked expression in the pattern.
  EP->setMatchExpr(matchCall);
  // Set the type on the pattern.
  EP->setType(rhsType);
  return hadError;
}

static Type replaceArchetypesWithTypeVariables(ConstraintSystem &cs,
                                               Type t) {
  llvm::DenseMap<SubstitutableType *, TypeVariableType *> types;

  return t.subst(
    [&](SubstitutableType *origType) -> Type {
      auto found = types.find(origType);
      if (found != types.end())
        return found->second;

      if (auto archetypeType = dyn_cast<ArchetypeType>(origType)) {
        if (archetypeType->getParent())
          return Type();

        auto locator = cs.getConstraintLocator(nullptr);
        auto replacement = cs.createTypeVariable(locator);

        if (auto superclass = archetypeType->getSuperclass()) {
          cs.addConstraint(ConstraintKind::Subtype, replacement,
                           superclass, locator);
        }
        for (auto proto : archetypeType->getConformsTo()) {
          cs.addConstraint(ConstraintKind::ConformsTo, replacement,
                           proto->getDeclaredType(), locator);
        }
        types[origType] = replacement;
        return replacement;
      }

      // FIXME: Remove this case
      assert(cast<GenericTypeParamType>(origType));
      auto locator = cs.getConstraintLocator(nullptr);
      auto replacement = cs.createTypeVariable(locator);
      types[origType] = replacement;
      return replacement;
    },
    MakeAbstractConformanceForGenericType());
}

bool TypeChecker::typesSatisfyConstraint(Type type1, Type type2,
                                         bool openArchetypes,
                                         ConstraintKind kind, DeclContext *dc,
                                         bool *unwrappedIUO) {
  assert(!type1->hasTypeVariable() && !type2->hasTypeVariable() &&
         "Unexpected type variable in constraint satisfaction testing");

  ConstraintSystem cs(*this, dc, ConstraintSystemOptions());
  if (openArchetypes) {
    type1 = replaceArchetypesWithTypeVariables(cs, type1);
    type2 = replaceArchetypesWithTypeVariables(cs, type2);
  }

  cs.addConstraint(kind, type1, type2, cs.getConstraintLocator(nullptr));

  if (openArchetypes) {
    assert(!unwrappedIUO && "FIXME");
    SmallVector<Solution, 4> solutions;
    return !cs.solve(nullptr, solutions, FreeTypeVariableBinding::Allow);
  }

  if (auto solution = cs.solveSingle()) {
    if (unwrappedIUO)
      *unwrappedIUO = solution->getFixedScore().Data[SK_ForceUnchecked] > 0;

    return true;
  }

  return false;
}

bool TypeChecker::isSubtypeOf(Type type1, Type type2, DeclContext *dc) {
  return typesSatisfyConstraint(type1, type2,
                                /*openArchetypes=*/false,
                                ConstraintKind::Subtype, dc);
}

bool TypeChecker::isConvertibleTo(Type type1, Type type2, DeclContext *dc,
                                  bool *unwrappedIUO) {
  return typesSatisfyConstraint(type1, type2,
                                /*openArchetypes=*/false,
                                ConstraintKind::Conversion, dc,
                                unwrappedIUO);
}

bool TypeChecker::isExplicitlyConvertibleTo(Type type1, Type type2,
                                            DeclContext *dc) {
  return (typesSatisfyConstraint(type1, type2,
                                 /*openArchetypes=*/false,
                                 ConstraintKind::Conversion, dc) ||
          isObjCBridgedTo(type1, type2, dc));
}

bool TypeChecker::isObjCBridgedTo(Type type1, Type type2, DeclContext *dc,
                                  bool *unwrappedIUO) {
  return (typesSatisfyConstraint(type1, type2,
                                 /*openArchetypes=*/false,
                                 ConstraintKind::BridgingConversion,
                                 dc, unwrappedIUO));
}

bool TypeChecker::checkedCastMaySucceed(Type t1, Type t2, DeclContext *dc) {
  auto kind = typeCheckCheckedCast(t1, t2, CheckedCastContextKind::None, dc,
                                   SourceLoc(), nullptr, SourceRange());
  return (kind != CheckedCastKind::Unresolved);
}

Expr *TypeChecker::addImplicitLoadExpr(
    Expr *expr,
    std::function<Type(Expr *)> getType,
    std::function<void(Expr *, Type)> setType) {
  class LoadAdder : public ASTWalker {
  private:
    using GetTypeFn = std::function<Type(Expr *)>;
    using SetTypeFn = std::function<void(Expr *, Type)>;

    TypeChecker &TC;
    GetTypeFn getType;
    SetTypeFn setType;

  public:
    LoadAdder(TypeChecker &TC, GetTypeFn getType, SetTypeFn setType)
      : TC(TC), getType(getType), setType(setType) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (isa<ParenExpr>(E) || isa<ForceValueExpr>(E))
        return { true, E };

      // Since load expression is created by walker,
      // it's safe to stop as soon as it encounters first one
      // because it would be the one it just created.
      if (isa<LoadExpr>(E))
        return { false, nullptr };

      return { false, createLoadExpr(E) };
    }

    Expr *walkToExprPost(Expr *E) override {
      if (auto *FVE = dyn_cast<ForceValueExpr>(E))
        setType(E, getType(FVE->getSubExpr())->getOptionalObjectType());

      if (auto *PE = dyn_cast<ParenExpr>(E))
        setType(E, ParenType::get(TC.Context, getType(PE->getSubExpr())));

      return E;
    }

  private:
    LoadExpr *createLoadExpr(Expr *E) {
      auto objectType = getType(E)->getRValueType();
      auto *LE = new (TC.Context) LoadExpr(E, objectType);
      setType(LE, objectType);
      return LE;
    }
  };

  return expr->walk(LoadAdder(*this, getType, setType));
}

Expr *TypeChecker::coerceToRValue(Expr *expr,
                               llvm::function_ref<Type(Expr *)> getType,
                               llvm::function_ref<void(Expr *, Type)> setType) {
  Type exprTy = getType(expr);

  // If expr has no type, just assume it's the right expr.
  if (!exprTy)
    return expr;

  // If the type is already materializable, then we're already done.
  if (!exprTy->hasLValueType())
    return expr;

  // Walk into force optionals and coerce the source.
  if (auto *FVE = dyn_cast<ForceValueExpr>(expr)) {
    auto sub = coerceToRValue(FVE->getSubExpr(), getType, setType);
    FVE->setSubExpr(sub);
    setType(FVE, getType(sub)->getOptionalObjectType());
    return FVE;
  }

  // Load lvalues.
  if (exprTy->is<LValueType>())
    return addImplicitLoadExpr(expr, getType, setType);

  // Walk into parenthesized expressions to update the subexpression.
  if (auto paren = dyn_cast<IdentityExpr>(expr)) {
    auto sub =  coerceToRValue(paren->getSubExpr(), getType, setType);
    paren->setSubExpr(sub);
    setType(paren, getType(sub));
    return paren;
  }

  // Walk into 'try' and 'try!' expressions to update the subexpression.
  if (auto tryExpr = dyn_cast<AnyTryExpr>(expr)) {
    auto sub = coerceToRValue(tryExpr->getSubExpr(), getType, setType);
    tryExpr->setSubExpr(sub);
    if (isa<OptionalTryExpr>(tryExpr) && !getType(sub)->hasError())
      setType(tryExpr, OptionalType::get(getType(sub)));
    else
      setType(tryExpr, getType(sub));
    return tryExpr;
  }

  // Walk into tuples to update the subexpressions.
  if (auto tuple = dyn_cast<TupleExpr>(expr)) {
    bool anyChanged = false;
    for (auto &elt : tuple->getElements()) {
      // Materialize the element.
      auto oldType = getType(elt);
      elt = coerceToRValue(elt, getType, setType);

      // If the type changed at all, make a note of it.
      if (getType(elt).getPointer() != oldType.getPointer()) {
        anyChanged = true;
      }
    }

    // If any of the types changed, rebuild the tuple type.
    if (anyChanged) {
      SmallVector<TupleTypeElt, 4> elements;
      elements.reserve(tuple->getElements().size());
      for (unsigned i = 0, n = tuple->getNumElements(); i != n; ++i) {
        Type type = getType(tuple->getElement(i));
        Identifier name = tuple->getElementName(i);
        elements.push_back(TupleTypeElt(type, name));
      }
      setType(tuple, TupleType::get(elements, Context));
    }

    return tuple;
  }

  // Nothing to do.
  return expr;
}

bool TypeChecker::convertToType(Expr *&expr, Type type, DeclContext *dc,
                                Optional<Pattern*> typeFromPattern) {
  // TODO: need to add kind arg?
  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc, ConstraintSystemFlags::AllowFixes);
  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  cs.addConstraint(ConstraintKind::Conversion, expr->getType(), type,
                   cs.getConstraintLocator(expr));

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->dump(log);
    log << "\n";
    cs.print(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if ((cs.solve(expr, viable) || viable.size() != 1) &&
      cs.salvage(viable, expr)) {
    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(log);
  }

  cs.cacheExprTypes(expr);

  // Perform the conversion.
  Expr *result = solution.coerceToType(expr, type,
                                       cs.getConstraintLocator(expr),
                                       /*ignoreTopLevelInjection*/false,
                                       typeFromPattern);
  if (!result) {
    return true;
  }

  cs.setExprTypes(expr);

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
    log << "\n";
  }

  expr = result;
  return false;
}

//===----------------------------------------------------------------------===//
// Debugging
//===----------------------------------------------------------------------===//
#pragma mark Debugging

void Solution::dump() const {
  dump(llvm::errs());
}

void Solution::dump(raw_ostream &out) const {
  ASTContext &ctx = getConstraintSystem().getASTContext();
  llvm::SaveAndRestore<bool> debugSolver(ctx.LangOpts.DebugConstraintSolver,
                                         true);

  SourceManager *sm = &ctx.SourceMgr;

  out << "Fixed score: " << FixedScore << "\n";

  out << "Type variables:\n";
  for (auto binding : typeBindings) {
    auto &typeVar = binding.first->getImpl();
    out.indent(2);
    typeVar.print(out);
    out << " as ";
    binding.second.print(out);
    if (auto *locator = typeVar.getLocator()) {
      out << " @ ";
      locator->dump(&ctx.SourceMgr, out);
    }
    out << "\n";
  }

  out << "\n";
  out << "Overload choices:\n";
  for (auto ovl : overloadChoices) {
    out.indent(2);
    if (ovl.first)
      ovl.first->dump(sm, out);
    out << " with ";

    auto choice = ovl.second.choice;
    switch (choice.getKind()) {
    case OverloadChoiceKind::Decl:
    case OverloadChoiceKind::DeclViaDynamic:
    case OverloadChoiceKind::DeclViaBridge:
    case OverloadChoiceKind::DeclViaUnwrappedOptional:
      choice.getDecl()->dumpRef(out);
      out << " as ";
      if (choice.getBaseType())
        out << choice.getBaseType()->getString() << ".";

      out << choice.getDecl()->getBaseName() << ": "
          << ovl.second.openedType->getString() << "\n";
      break;

    case OverloadChoiceKind::BaseType:
      out << "base type " << choice.getBaseType()->getString() << "\n";
      break;

    case OverloadChoiceKind::KeyPathApplication:
      out << "key path application root "
          << choice.getBaseType()->getString() << "\n";
      break;

    case OverloadChoiceKind::DynamicMemberLookup:
      out << "dynamic member lookup root "
          << choice.getBaseType()->getString()
          << " name='" << choice.getName() << "'\n";
      break;
  
    case OverloadChoiceKind::TupleIndex:
      out << "tuple " << choice.getBaseType()->getString() << " index "
        << choice.getTupleIndex() << "\n";
      break;
    }
    out << "\n";
  }

  out << "\n";
  out << "Constraint restrictions:\n";
  for (auto &restriction : ConstraintRestrictions) {
    out.indent(2) << restriction.first.first
                  << " to " << restriction.first.second
                  << " is " << getName(restriction.second) << "\n";
  }

  out << "\nDisjunction choices:\n";
  for (auto &choice : DisjunctionChoices) {
    out.indent(2);
    choice.first->dump(sm, out);
    out << " is #" << choice.second << "\n";
  }

  if (!OpenedTypes.empty()) {
    out << "\nOpened types:\n";
    for (const auto &opened : OpenedTypes) {
      out.indent(2);
      opened.first->dump(sm, out);
      out << " opens ";
      interleave(opened.second.begin(), opened.second.end(),
                 [&](OpenedType opened) {
                   opened.first->print(out);
                   out << " -> ";
                   opened.second->print(out);
                 },
                 [&]() {
                   out << ", ";
                 });
      out << "\n";
    }
  }

  if (!OpenedExistentialTypes.empty()) {
    out << "\nOpened existential types:\n";
    for (const auto &openedExistential : OpenedExistentialTypes) {
      out.indent(2);
      openedExistential.first->dump(sm, out);
      out << " opens to " << openedExistential.second->getString();
      out << "\n";
    }
  }

  if (!DefaultedConstraints.empty()) {
    out << "\nDefaulted constraints: ";
    interleave(DefaultedConstraints, [&](ConstraintLocator *locator) {
      locator->dump(sm, out);
    }, [&] {
      out << ", ";
    });
  }

  if (!Fixes.empty()) {
    out << "\nFixes:\n";
    for (auto *fix : Fixes) {
      out.indent(2);
      fix->print(out);
      out << "\n";
    }
  }
}

void ConstraintSystem::dump() {
  print(llvm::errs());
}

void ConstraintSystem::dump(Expr *E) {
  auto getTypeOfExpr = [&](const Expr *E) -> Type {
    if (hasType(E))
      return getType(E);
    return Type();
  };
  auto getTypeOfTypeLoc = [&](const TypeLoc &TL) -> Type {
    if (hasType(TL))
      return getType(TL);
    return Type();
  };

  E->dump(llvm::errs(), getTypeOfExpr, getTypeOfTypeLoc);
}

void ConstraintSystem::print(raw_ostream &out) {
  // Print all type variables as $T0 instead of _ here.
  llvm::SaveAndRestore<bool> X(getASTContext().LangOpts.DebugConstraintSolver,
                               true);
  
  out << "Score: " << CurrentScore << "\n";

  if (contextualType.getType()) {
    out << "Contextual Type: " << contextualType.getType();
    if (TypeRepr *TR = contextualType.getTypeRepr()) {
      out << " at ";
      TR->getSourceRange().print(out, getASTContext().SourceMgr, /*text*/false);
    }
    out << "\n";
  }

  out << "Type Variables:\n";
  for (auto tv : TypeVariables) {
    out.indent(2);
    tv->getImpl().print(out);
    if (tv->getImpl().canBindToLValue())
      out << " [lvalue allowed]";
    if (tv->getImpl().canBindToInOut())
      out << " [inout allowed]";
    auto rep = getRepresentative(tv);
    if (rep == tv) {
      if (auto fixed = getFixedType(tv)) {
        out << " as ";
        fixed->print(out);
      } else {
        getPotentialBindings(tv).dump(out, 1);
      }
    } else {
      out << " equivalent to ";
      rep->print(out);
    }

    if (auto *locator = tv->getImpl().getLocator()) {
      out << " @ ";
      locator->dump(&TC.Context.SourceMgr, out);
    }

    out << "\n";
  }

  out << "\nActive Constraints:\n";
  for (auto &constraint : ActiveConstraints) {
    out.indent(2);
    constraint.print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }

  out << "\nInactive Constraints:\n";
  for (auto &constraint : InactiveConstraints) {
    out.indent(2);
    constraint.print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }

  if (solverState && !solverState->hasRetiredConstraints()) {
    out << "\nRetired Constraints:\n";
    solverState->forEachRetired([&](Constraint &constraint) {
      out.indent(2);
      constraint.print(out, &getTypeChecker().Context.SourceMgr);
      out << "\n";
    });
  }

  if (resolvedOverloadSets) {
    out << "Resolved overloads:\n";

    // Otherwise, report the resolved overloads.
    for (auto resolved = resolvedOverloadSets;
         resolved; resolved = resolved->Previous) {
      auto &choice = resolved->Choice;
      out << "  selected overload set choice ";
      switch (choice.getKind()) {
      case OverloadChoiceKind::Decl:
      case OverloadChoiceKind::DeclViaDynamic:
      case OverloadChoiceKind::DeclViaBridge:
      case OverloadChoiceKind::DeclViaUnwrappedOptional:
        if (choice.getBaseType())
          out << choice.getBaseType()->getString() << ".";
        out << choice.getDecl()->getBaseName() << ": "
            << resolved->BoundType->getString() << " == "
            << resolved->ImpliedType->getString() << "\n";
        break;

      case OverloadChoiceKind::BaseType:
        out << "base type " << choice.getBaseType()->getString() << "\n";
        break;

      case OverloadChoiceKind::KeyPathApplication:
        out << "key path application root "
            << choice.getBaseType()->getString() << "\n";
        break;

      case OverloadChoiceKind::DynamicMemberLookup:
        out << "dynamic member lookup:"
            << choice.getBaseType()->getString() << "  name="
            << choice.getName() << "\n";
        break;

      case OverloadChoiceKind::TupleIndex:
        out << "tuple " << choice.getBaseType()->getString() << " index "
            << choice.getTupleIndex() << "\n";
        break;
      }
    }
    out << "\n";
  }

  if (!DisjunctionChoices.empty()) {
    out << "\nDisjunction choices:\n";
    for (auto &choice : DisjunctionChoices) {
      out.indent(2);
      choice.first->dump(&getTypeChecker().Context.SourceMgr, out);
      out << " is #" << choice.second << "\n";
    }
  }

  if (!OpenedTypes.empty()) {
    out << "\nOpened types:\n";
    for (const auto &opened : OpenedTypes) {
      out.indent(2);
      opened.first->dump(&getTypeChecker().Context.SourceMgr, out);
      out << " opens ";
      interleave(opened.second.begin(), opened.second.end(),
                 [&](OpenedType opened) {
                   opened.first->print(out);
                   out << " -> ";
                   opened.second->print(out);
                 },
                 [&]() {
                   out << ", ";
                 });
      out << "\n";
    }
  }

  if (!OpenedExistentialTypes.empty()) {
    out << "\nOpened existential types:\n";
    for (const auto &openedExistential : OpenedExistentialTypes) {
      out.indent(2);
      openedExistential.first->dump(&getTypeChecker().Context.SourceMgr, out);
      out << " opens to " << openedExistential.second->getString();
      out << "\n";
    }
  }

  if (!DefaultedConstraints.empty()) {
    out << "\nDefaulted constraints: ";
    interleave(DefaultedConstraints, [&](ConstraintLocator *locator) {
      locator->dump(&getTypeChecker().Context.SourceMgr, out);
    }, [&] {
      out << ", ";
    });
  }

  if (failedConstraint) {
    out << "\nFailed constraint:\n";
    out.indent(2);
    failedConstraint->print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }

  if (!Fixes.empty()) {
    out << "\nFixes:\n";
    for (auto *fix : Fixes) {
      out.indent(2);
      fix->print(out);
      out << "\n";
    }
  }
}

/// Determine the semantics of a checked cast operation.
CheckedCastKind TypeChecker::typeCheckCheckedCast(Type fromType,
                                 Type toType,
                                 CheckedCastContextKind contextKind,
                                 DeclContext *dc,
                                 SourceLoc diagLoc,
                                 Expr *fromExpr,
                                 SourceRange diagToRange) {
  SourceRange diagFromRange;
  if (fromExpr)
    diagFromRange = fromExpr->getSourceRange();
  
  // If the from/to types are equivalent or convertible, this is a coercion.
  bool unwrappedIUO = false;
  if (fromType->isEqual(toType) ||
      (isConvertibleTo(fromType, toType, dc, &unwrappedIUO) &&
       !unwrappedIUO)) {
    return CheckedCastKind::Coercion;
  }
  
  // Check for a bridging conversion.
  // Anything bridges to AnyObject.
  if (toType->isAnyObject())
    return CheckedCastKind::BridgingCoercion;
  
  if (isObjCBridgedTo(fromType, toType, dc, &unwrappedIUO) && !unwrappedIUO){
    return CheckedCastKind::BridgingCoercion;
  }

  Type origFromType = fromType;
  Type origToType = toType;

  // Determine whether we should suppress diagnostics.
  bool suppressDiagnostics = (contextKind == CheckedCastContextKind::None);

  bool optionalToOptionalCast = false;

  // Local function to indicate failure.
  auto failed = [&] {
    if (suppressDiagnostics) {
      return CheckedCastKind::Unresolved;
    }

    // Explicit optional-to-optional casts always succeed because a nil
    // value of any optional type can be cast to any other optional type.
    if (optionalToOptionalCast)
      return CheckedCastKind::ValueCast;

    diagnose(diagLoc, diag::downcast_to_unrelated, origFromType, origToType)
      .highlight(diagFromRange)
      .highlight(diagToRange);

    return CheckedCastKind::ValueCast;
  };

  // Strip optional wrappers off of the destination type in sync with
  // stripping them off the origin type.
  while (auto toValueType = toType->getOptionalObjectType()) {
    // Complain if we're trying to increase optionality, e.g.
    // casting an NSObject? to an NSString??.  That's not a subtype
    // relationship.
    auto fromValueType = fromType->getOptionalObjectType();
    if (!fromValueType) {
      if (!suppressDiagnostics) {
        diagnose(diagLoc, diag::downcast_to_more_optional,
                 origFromType, origToType)
          .highlight(diagFromRange)
          .highlight(diagToRange);
      }
      return CheckedCastKind::Unresolved;
    }

    toType = toValueType;
    fromType = fromValueType;
    optionalToOptionalCast = true;
  }
  
  // On the other hand, casts can decrease optionality monadically.
  unsigned extraFromOptionals = 0;
  while (auto fromValueType = fromType->getOptionalObjectType()) {
    fromType = fromValueType;
    ++extraFromOptionals;
  }

  // If the unwrapped from/to types are equivalent or bridged, this isn't a real
  // downcast. Complain.
  if (extraFromOptionals > 0) {
    switch (typeCheckCheckedCast(fromType, toType,
                                 CheckedCastContextKind::None, dc,
                                 SourceLoc(), nullptr, SourceRange())) {
    case CheckedCastKind::Coercion:
    case CheckedCastKind::BridgingCoercion: {
      // FIXME: Add a Fix-It, when the caller provides us with enough
      // information.
      if (!suppressDiagnostics) {
        bool isBridged =
          !fromType->isEqual(toType) && !isConvertibleTo(fromType, toType, dc);

        switch (contextKind) {
        case CheckedCastContextKind::None:
          llvm_unreachable("suppressing diagnostics");

        case CheckedCastContextKind::ForcedCast: {
          std::string extraFromOptionalsStr(extraFromOptionals, '!');
          auto diag = diagnose(diagLoc, diag::downcast_same_type,
                               origFromType, origToType,
                               extraFromOptionalsStr,
                               isBridged);
          diag.highlight(diagFromRange);
          diag.highlight(diagToRange);

          /// Add the '!''s needed to adjust the type.
          diag.fixItInsertAfter(diagFromRange.End,
                                std::string(extraFromOptionals, '!'));
          if (isBridged) {
            // If it's bridged, we still need the 'as' to perform the bridging.
            diag.fixItReplaceChars(diagLoc, diagLoc.getAdvancedLocOrInvalid(3),
                                   "as");
          } else {
            // Otherwise, implicit conversions will handle it in most cases.
            SourceLoc afterExprLoc = Lexer::getLocForEndOfToken(Context.SourceMgr,
                                                                diagFromRange.End);

            diag.fixItRemove(SourceRange(afterExprLoc, diagToRange.End));
          }
          break;
        }

        case CheckedCastContextKind::ConditionalCast:
          // If we're only unwrapping a single optional, that optional value is
          // effectively carried through to the underlying conversion, making this
          // the moral equivalent of a map. Complain that one can do this with
          // 'as' more effectively.
          if (extraFromOptionals == 1) {
            // A single optional is carried through. It's better to use 'as' to
            // the appropriate optional type.
            auto diag = diagnose(diagLoc, diag::conditional_downcast_same_type,
                                 origFromType, origToType,
                                 fromType->isEqual(toType) ? 0
                                               : isBridged ? 2
                                               : 1);
            diag.highlight(diagFromRange);
            diag.highlight(diagToRange);

            if (isBridged) {
              // For a bridged cast, replace the 'as?' with 'as'.
              diag.fixItReplaceChars(diagLoc, diagLoc.getAdvancedLocOrInvalid(3),
                                     "as");

              // Make sure we'll cast to the appropriately-optional type by adding
              // the '?'.
              // FIXME: Parenthesize!
              diag.fixItInsertAfter(diagToRange.End, "?");
            } else {
              // Just remove the cast; implicit conversions will handle it.
              SourceLoc afterExprLoc =
                Lexer::getLocForEndOfToken(Context.SourceMgr, diagFromRange.End);

              if (afterExprLoc.isValid() && diagToRange.isValid())
                diag.fixItRemove(SourceRange(afterExprLoc, diagToRange.End));
            }
          }

          // If there is more than one extra optional, don't do anything: this
          // conditional cast is trying to unwrap some levels of optional;
          // let the runtime handle it.
          break;

        case CheckedCastContextKind::IsExpr:
          // If we're only unwrapping a single optional, we could have just
          // checked for 'nil'.
          if (extraFromOptionals == 1) {
            auto diag = diagnose(diagLoc, diag::is_expr_same_type,
                                 origFromType, origToType);
            diag.highlight(diagFromRange);
            diag.highlight(diagToRange);

            diag.fixItReplace(SourceRange(diagLoc, diagToRange.End), "!= nil");

            // Add parentheses if needed.
            if (!fromExpr->canAppendPostfixExpression()) {
              diag.fixItInsert(fromExpr->getStartLoc(), "(");
              diag.fixItInsertAfter(fromExpr->getEndLoc(), ")");
            }
          }

          // If there is more than one extra optional, don't do anything: this
          // is performing a deeper check that the runtime will handle.
          break;

        case CheckedCastContextKind::IsPattern:
        case CheckedCastContextKind::EnumElementPattern:
          // Note: Don't diagnose these, because the code is testing whether
          // the optionals can be unwrapped.
          break;
        }
      }

      // Treat this as a value cast so we preserve the semantics.
      return CheckedCastKind::ValueCast;
    }

    case CheckedCastKind::ArrayDowncast:
    case CheckedCastKind::DictionaryDowncast:
    case CheckedCastKind::SetDowncast:
    case CheckedCastKind::ValueCast:
      break;

    case CheckedCastKind::Unresolved:
      return failed();
    }
  }

  // Check for casts between specific concrete types that cannot succeed.
  ConstraintSystem cs(*this, dc, ConstraintSystemOptions());

  if (auto toElementType = cs.isArrayType(toType)) {
    if (auto fromElementType = cs.isArrayType(fromType)) {
      switch (typeCheckCheckedCast(*fromElementType, *toElementType,
                                   CheckedCastContextKind::None, dc,
                                   SourceLoc(), nullptr, SourceRange())) {
      case CheckedCastKind::Coercion:
        return CheckedCastKind::Coercion;

      case CheckedCastKind::BridgingCoercion:
        return CheckedCastKind::BridgingCoercion;

      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
      case CheckedCastKind::ValueCast:
        return CheckedCastKind::ArrayDowncast;

      case CheckedCastKind::Unresolved:
        return failed();
      }
    }
  }

  if (auto toKeyValue = cs.isDictionaryType(toType)) {
    if (auto fromKeyValue = cs.isDictionaryType(fromType)) {
      bool hasCoercion = false;
      enum { NoBridging, BridgingCoercion }
        hasBridgingConversion = NoBridging;
      bool hasCast = false;
      switch (typeCheckCheckedCast(fromKeyValue->first, toKeyValue->first,
                                   CheckedCastContextKind::None, dc,
                                   SourceLoc(), nullptr, SourceRange())) {
      case CheckedCastKind::Coercion:
        hasCoercion = true;
        break;

      case CheckedCastKind::BridgingCoercion:
        hasBridgingConversion = std::max(hasBridgingConversion,
                                         BridgingCoercion);
        break;

      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
      case CheckedCastKind::ValueCast:
        hasCast = true;
        break;

      case CheckedCastKind::Unresolved:
        return failed();
      }

      switch (typeCheckCheckedCast(fromKeyValue->second, toKeyValue->second,
                                   CheckedCastContextKind::None, dc,
                                   SourceLoc(), nullptr, SourceRange())) {
      case CheckedCastKind::Coercion:
        hasCoercion = true;
        break;

      case CheckedCastKind::BridgingCoercion:
        hasBridgingConversion = std::max(hasBridgingConversion,
                                         BridgingCoercion);
        break;

      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
      case CheckedCastKind::ValueCast:
        hasCast = true;
        break;

      case CheckedCastKind::Unresolved:
        return failed();
      }

      if (hasCast) return CheckedCastKind::DictionaryDowncast;
      switch (hasBridgingConversion) {
      case NoBridging:
        break;
      case BridgingCoercion:
        return CheckedCastKind::BridgingCoercion;
      }
      assert(hasCoercion && "Not a coercion?");
      return CheckedCastKind::Coercion;
    }
  }

  if (auto toElementType = cs.isSetType(toType)) {
    if (auto fromElementType = cs.isSetType(fromType)) {
      switch (typeCheckCheckedCast(*fromElementType, *toElementType,
                                   CheckedCastContextKind::None, dc,
                                   SourceLoc(), nullptr, SourceRange())) {
      case CheckedCastKind::Coercion:
        return CheckedCastKind::Coercion;

      case CheckedCastKind::BridgingCoercion:
        return CheckedCastKind::BridgingCoercion;
      
      case CheckedCastKind::ArrayDowncast:
      case CheckedCastKind::DictionaryDowncast:
      case CheckedCastKind::SetDowncast:
      case CheckedCastKind::ValueCast:
        return CheckedCastKind::SetDowncast;

      case CheckedCastKind::Unresolved:
        return failed();
      }
    }
  }

  // If we can bridge through an Objective-C class, do so.
  if (Type bridgedToClass = getDynamicBridgedThroughObjCClass(dc, fromType,
                                                              toType)) {
    switch (typeCheckCheckedCast(bridgedToClass, fromType,
                                 CheckedCastContextKind::None, dc, SourceLoc(),
                                 nullptr, SourceRange())) {
    case CheckedCastKind::ArrayDowncast:
    case CheckedCastKind::BridgingCoercion:
    case CheckedCastKind::Coercion:
    case CheckedCastKind::DictionaryDowncast:
    case CheckedCastKind::SetDowncast:
    case CheckedCastKind::ValueCast:
      return CheckedCastKind::ValueCast;

    case CheckedCastKind::Unresolved:
      break;
    }
  }

  // If we can bridge through an Objective-C class, do so.
  if (Type bridgedFromClass = getDynamicBridgedThroughObjCClass(dc, toType,
                                                                fromType)) {
    switch (typeCheckCheckedCast(toType, bridgedFromClass,
                                 CheckedCastContextKind::None, dc, SourceLoc(),
                                 nullptr, SourceRange())) {
    case CheckedCastKind::ArrayDowncast:
    case CheckedCastKind::BridgingCoercion:
    case CheckedCastKind::Coercion:
    case CheckedCastKind::DictionaryDowncast:
    case CheckedCastKind::SetDowncast:
    case CheckedCastKind::ValueCast:
      return CheckedCastKind::ValueCast;

    case CheckedCastKind::Unresolved:
      break;
    }
  }

  // Strip metatypes. If we can cast two types, we can cast their metatypes.
  bool metatypeCast = false;
  while (auto toMetatype = toType->getAs<MetatypeType>()) {
    auto fromMetatype = fromType->getAs<MetatypeType>();
    if (!fromMetatype)
      break;
    
    metatypeCast = true;
    toType = toMetatype->getInstanceType();
    fromType = fromMetatype->getInstanceType();
  }
  
  // Strip an inner layer of potentially existential metatype.
  bool toExistentialMetatype = false;
  bool fromExistentialMetatype = false;
  if (auto toMetatype = toType->getAs<AnyMetatypeType>()) {
    toExistentialMetatype = toMetatype->is<ExistentialMetatypeType>();
    if (auto fromMetatype = fromType->getAs<AnyMetatypeType>()) {
      fromExistentialMetatype = fromMetatype->is<ExistentialMetatypeType>();
      toType = toMetatype->getInstanceType();
      fromType = fromMetatype->getInstanceType();
    }
  }

  bool toArchetype = toType->is<ArchetypeType>();
  bool fromArchetype = fromType->is<ArchetypeType>();
  bool toExistential = toType->isExistentialType();
  bool fromExistential = fromType->isExistentialType();
  
  // If we're doing a metatype cast, it can only be existential if we're
  // casting to/from the existential metatype. 'T.self as P.Protocol'
  // can only succeed if T is exactly the type P, so is a concrete cast,
  // whereas 'T.self as P.Type' succeeds for types conforming to the protocol
  // P, and is an existential cast.
  if (metatypeCast) {
    toExistential &= toExistentialMetatype;
    fromExistential &= fromExistentialMetatype;
  }

  // Casts to or from generic types can't be statically constrained in most
  // cases, because there may be protocol conformances we don't statically
  // know about.
  if (toExistential || fromExistential || fromArchetype || toArchetype) {
    // Cast to and from AnyObject always succeed.
    if (toType->isAnyObject() || fromType->isAnyObject())
      return CheckedCastKind::ValueCast;

    bool toRequiresClass;
    if (toType->isExistentialType())
      toRequiresClass = toType->getExistentialLayout().requiresClass();
    else
      toRequiresClass = toType->mayHaveSuperclass();

    bool fromRequiresClass;
    if (fromType->isExistentialType())
      fromRequiresClass = fromType->getExistentialLayout().requiresClass();
    else
      fromRequiresClass = fromType->mayHaveSuperclass();

    // If neither type is class-constrained, anything goes.
    if (!fromRequiresClass && !toRequiresClass)
        return CheckedCastKind::ValueCast;

    if (!fromRequiresClass && toRequiresClass) {
      // If source type is abstract, anything goes.
      if (fromExistential || fromArchetype)
        return CheckedCastKind::ValueCast;

      // Otherwise, we're casting a concrete non-class type to a
      // class-constrained archetype or existential, which will
      // probably fail, but we'll try more casts below.
    }

    if (fromRequiresClass && !toRequiresClass) {
      // If destination type is abstract, anything goes.
      if (toExistential || toArchetype)
        return CheckedCastKind::ValueCast;

      // Otherwise, we're casting a class-constrained archetype
      // or existential to a non-class concrete type, which
      // will probably fail, but we'll try more casts below.
    }

    if (fromRequiresClass && toRequiresClass) {
      // Ok, we are casting between class-like things. Let's see if we have
      // explicit superclass bounds.
      Type toSuperclass;
      if (toType->getClassOrBoundGenericClass())
        toSuperclass = toType;
      else
        toSuperclass = toType->getSuperclass();

      Type fromSuperclass;
      if (fromType->getClassOrBoundGenericClass())
        fromSuperclass = fromType;
      else
        fromSuperclass = fromType->getSuperclass();

      // Unless both types have a superclass bound, we have no further
      // information.
      if (!toSuperclass || !fromSuperclass)
        return CheckedCastKind::ValueCast;

      // Compare superclass bounds.
      if (typesSatisfyConstraint(toSuperclass, fromSuperclass,
                                 /*openArchetypes=*/true,
                                 ConstraintKind::Subtype, dc))
        return CheckedCastKind::ValueCast;

      // An upcast is also OK.
      if (typesSatisfyConstraint(fromSuperclass, toSuperclass,
                                 /*openArchetypes=*/true,
                                 ConstraintKind::Subtype, dc))
        return CheckedCastKind::ValueCast;
    }
  }

  if (cs.isAnyHashableType(toType) || cs.isAnyHashableType(fromType)) {
    return CheckedCastKind::ValueCast;
  }

  // If the destination type can be a supertype of the source type, we are
  // performing what looks like an upcast except it rebinds generic
  // parameters.
  if (!metatypeCast &&
      typesSatisfyConstraint(fromType, toType,
                             /*openArchetypes=*/true,
                             ConstraintKind::Subtype, dc)) {
    return CheckedCastKind::ValueCast;
  }

  // If the destination type can be a subtype of the source type, we have
  // a downcast.
  if (typesSatisfyConstraint(toType, fromType,
                             /*openArchetypes=*/true,
                             ConstraintKind::Subtype, dc)) {
    return CheckedCastKind::ValueCast;
  }
  
  // Objective-C metaclasses are subclasses of NSObject in the ObjC runtime,
  // so casts from NSObject to potentially-class metatypes may succeed.
  if (auto nsObject = cs.TC.getNSObjectType(dc)) {
    if (fromType->isEqual(nsObject)) {
      if (auto toMeta = toType->getAs<MetatypeType>()) {
        if (toMeta->getInstanceType()->mayHaveSuperclass()
            || toMeta->getInstanceType()->is<ArchetypeType>())
          return CheckedCastKind::ValueCast;
      }
      if (toType->is<ExistentialMetatypeType>())
        return CheckedCastKind::ValueCast;
    }
  }

  // We can conditionally cast from NSError to an Error-conforming
  // type.  This is handled in the runtime, so it doesn't need a special cast
  // kind.
  if (Context.LangOpts.EnableObjCInterop) {
    if (auto errorTypeProto = Context.getProtocol(KnownProtocolKind::Error)) {
      if (conformsToProtocol(toType, errorTypeProto, dc,
                             (ConformanceCheckFlags::InExpression|
                              ConformanceCheckFlags::Used))) {
        auto nsError = Context.getNSErrorDecl();
        if (nsError) {
          if (!nsError->hasInterfaceType()) {
            auto resolver = Context.getLazyResolver();
            assert(resolver);
            resolver->resolveDeclSignature(nsError);
          }
          Type NSErrorTy = nsError->getDeclaredInterfaceType();
          if (isSubtypeOf(fromType, NSErrorTy, dc)
              // Don't mask "always true" warnings if NSError is cast to
              // Error itself.
              && !isSubtypeOf(fromType, toType, dc))
            return CheckedCastKind::ValueCast;
        }
      }
    }
  }

  // The runtime doesn't support casts to CF types and always lets them succeed.
  // This "always fails" diagnosis makes no sense when paired with the CF
  // one.
  auto clas = toType->getClassOrBoundGenericClass();
  if (clas && clas->getForeignClassKind() == ClassDecl::ForeignKind::CFType)
    return CheckedCastKind::ValueCast;
  
  // Don't warn on casts that change the generic parameters of ObjC generic
  // classes. This may be necessary to force-fit ObjC APIs that depend on
  // covariance, or for APIs where the generic parameter annotations in the
  // ObjC headers are inaccurate.
  if (clas && clas->usesObjCGenericsModel()) {
    if (fromType->getClassOrBoundGenericClass() == clas)
      return CheckedCastKind::ValueCast;
  }

  return failed();
}

/// If the expression is an implicit call to _forceBridgeFromObjectiveC or
/// _conditionallyBridgeFromObjectiveC, returns the argument of that call.
static Expr *lookThroughBridgeFromObjCCall(ASTContext &ctx, Expr *expr) {
  auto call = dyn_cast<CallExpr>(expr);
  if (!call || !call->isImplicit())
    return nullptr;

  auto callee = call->getCalledValue();
  if (!callee)
    return nullptr;

  if (callee == ctx.getForceBridgeFromObjectiveC(nullptr) ||
      callee == ctx.getConditionallyBridgeFromObjectiveC(nullptr))
    return cast<TupleExpr>(call->getArg())->getElement(0);

  return nullptr;
}

/// If the expression has the effect of a forced downcast, find the
/// underlying forced downcast expression.
ForcedCheckedCastExpr *swift::findForcedDowncast(ASTContext &ctx, Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();
  
  // Simple case: forced checked cast.
  if (auto forced = dyn_cast<ForcedCheckedCastExpr>(expr)) {
    return forced;
  }

  // If we have an implicit force, look through it.
  if (auto forced = dyn_cast<ForceValueExpr>(expr)) {
    if (forced->isImplicit()) {
      expr = forced->getSubExpr();
    }
  }

  // Skip through optional evaluations and binds.
  auto skipOptionalEvalAndBinds = [](Expr *expr) -> Expr* {
    do {
      if (!expr->isImplicit())
        break;

      if (auto optionalEval = dyn_cast<OptionalEvaluationExpr>(expr)) {
        expr = optionalEval->getSubExpr();
        continue;
      }

      if (auto bindOptional = dyn_cast<BindOptionalExpr>(expr)) {
        expr = bindOptional->getSubExpr();
        continue;
      }
      
      break;
    } while (true);

    return expr;
  };

  auto sub = skipOptionalEvalAndBinds(expr);
  
  // If we have an explicit cast, we're done.
  if (auto *FCE = dyn_cast<ForcedCheckedCastExpr>(sub))
    return FCE;

  // Otherwise, try to look through an implicit _forceBridgeFromObjectiveC() call.
  if (auto arg = lookThroughBridgeFromObjCCall(ctx, sub)) {
    sub = skipOptionalEvalAndBinds(arg);
    if (auto *FCE = dyn_cast<ForcedCheckedCastExpr>(sub))
      return FCE;
  }

  return nullptr;
}
