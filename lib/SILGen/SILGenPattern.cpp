//===--- SILGenPattern.cpp - Pattern matching codegen ---------------------===//
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

#define DEBUG_TYPE "patternmatch-silgen"
#include "Cleanup.h"
#include "ExitableFullExpr.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "SILGen.h"
#include "Scope.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/SILOptions.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/ProfileCounter.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FormattedStream.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                             Pattern Utilities
//===----------------------------------------------------------------------===//

// TODO: These routines should probably be refactored into their own file since
// they have nothing to do with the implementation of SILGenPattern
// specifically.

/// Shallow-dump a pattern node one level deep for debug purposes.
static void dumpPattern(const Pattern *p, llvm::raw_ostream &os) {
  if (!p) {
    // We use null to represent a synthetic wildcard.
    os << '_';
    return;
  }
  p = p->getSemanticsProvidingPattern();
  switch (p->getKind()) {
  case PatternKind::Any:
    os << '_';
    return;
  case PatternKind::Expr:
    os << "<expr>";
    return;
  case PatternKind::Named:
    os << "var " << cast<NamedPattern>(p)->getBoundName();
    return;
  case PatternKind::Tuple: {
    unsigned numFields = cast<TuplePattern>(p)->getNumElements();
    if (numFields == 0)
      os << "()";
    else if (numFields == 1)
      os << "(_)";
    else {
      os << '(';
      for (unsigned i = 0; i < numFields - 1; ++i)
        os << ',';
      os << ')';
    }
    return;
  }
  case PatternKind::Is:
    os << "is ";
    cast<IsPattern>(p)->getCastTypeLoc().getType()->print(os);
    break;
  case PatternKind::EnumElement: {
    auto eep = cast<EnumElementPattern>(p);
    os << '.' << eep->getName();
    return;
  }

  case PatternKind::OptionalSome:
    os << ".some";
    return;

  case PatternKind::Bool:
    os << (cast<BoolPattern>(p)->getValue() ? "true" : "false");
    return;

  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    llvm_unreachable("not semantic");
  }
}

/// Is the given specializable pattern directly refutable, as opposed
/// to containing some refutability in a nested position?
static bool isDirectlyRefutablePattern(const Pattern *p) {
  if (!p) return false;

  switch (p->getKind()) {
  case PatternKind::Any:
  case PatternKind::Named:
  case PatternKind::Expr:
    llvm_unreachable("non-specializable patterns");
  
  // Tuple and nominal-type patterns are not themselves directly refutable.
  case PatternKind::Tuple:
    return false;

  // isa and enum-element patterns are refutable, at least in theory.
  case PatternKind::Is:
  case PatternKind::EnumElement:
  case PatternKind::OptionalSome:
  case PatternKind::Bool:
    return true;

  // Recur into simple wrapping patterns.
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    return isDirectlyRefutablePattern(p->getSemanticsProvidingPattern());
  }  
  llvm_unreachable("bad pattern");
}

const unsigned AlwaysRefutable = ~0U;

/// Return the number of times a pattern must be specialized
/// before becoming irrefutable.
///
/// \return AlwaysRefutable if the pattern is never irrefutable
static unsigned getNumSpecializationsRecursive(const Pattern *p, unsigned n) {
  // n is partially here to make simple cases tail-recursive, but it
  // also gives us a simple opportunity to bail out early when we see
  // an always-refutable pattern.
  if (n == AlwaysRefutable) return n;

  switch (p->getKind()) {
  // True wildcards.
  case PatternKind::Any:
  case PatternKind::Named:
    return n;

  // Expressions are always-refutable wildcards.
  case PatternKind::Expr:
    return AlwaysRefutable;
  
  // Tuple and nominal-type patterns are not themselves directly refutable.
  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(p);
    for (auto &elt : tuple->getElements())
      n = getNumSpecializationsRecursive(elt.getPattern(), n);
    return n;
  }
  
  // isa and enum-element patterns are refutable, at least in theory.
  case PatternKind::Is: {
    auto isa = cast<IsPattern>(p);
    n++;
    if (auto sub = isa->getSubPattern())
      return getNumSpecializationsRecursive(sub, n);
    return n;
  }
  case PatternKind::EnumElement: {
    auto en = cast<EnumElementPattern>(p);
    n++;
    if (en->hasSubPattern())
      n = getNumSpecializationsRecursive(en->getSubPattern(), n);
    return n;
  }
  case PatternKind::OptionalSome: {
    auto en = cast<OptionalSomePattern>(p);
    return getNumSpecializationsRecursive(en->getSubPattern(), n+1);
  }
  case PatternKind::Bool:
    return n+1;

  // Recur into simple wrapping patterns.
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    return getNumSpecializationsRecursive(p->getSemanticsProvidingPattern(), n);
  }  
  llvm_unreachable("bad pattern");
}

/// Return the number of times a pattern must be specialized
/// before becoming irrefutable.
///
/// \return AlwaysRefutable if the pattern is never irrefutable
static unsigned getNumSpecializations(const Pattern *p) {
  return (p ? getNumSpecializationsRecursive(p, 0) : 0);
}

/// True if a pattern is a wildcard, meaning it matches any value. '_' and
/// variable patterns are wildcards. We also consider ExprPatterns to be
/// wildcards; we test the match expression as a guard outside of the normal
/// pattern clause matrix. When destructuring wildcard patterns, we also use
/// nullptr to represent newly-constructed wildcards.
static bool isWildcardPattern(const Pattern *p) {
  if (!p)
    return true;

  switch (p->getKind()) {
  // Simple wildcards.
  case PatternKind::Any:
  case PatternKind::Expr:
  case PatternKind::Named:
    return true;
  
  // Non-wildcards.
  case PatternKind::Tuple:
  case PatternKind::Is:
  case PatternKind::EnumElement:
  case PatternKind::OptionalSome:
  case PatternKind::Bool:
    return false;

  // Recur into simple wrapping patterns.
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    return isWildcardPattern(p->getSemanticsProvidingPattern());
  }

  llvm_unreachable("Unhandled PatternKind in switch.");
}

/// Check to see if the given pattern is a specializing pattern,
/// and return a semantic pattern for it.
Pattern *getSpecializingPattern(Pattern *p) {
  // Empty entries are basically AnyPatterns.
  if (!p) return nullptr;

  p = p->getSemanticsProvidingPattern();
  return (isWildcardPattern(p) ? nullptr : p);
}

/// Given a pattern stored in a clause matrix, check to see whether it
/// can be specialized the same way as the first one.
static Pattern *getSimilarSpecializingPattern(Pattern *p, Pattern *first) {
  // Empty entries are basically AnyPatterns.
  if (!p) return nullptr;

  assert(first && getSpecializingPattern(first) == first);

  // Map down to the semantics-providing pattern.
  p = p->getSemanticsProvidingPattern();

  // If the patterns are exactly the same kind, we might be able to treat them
  // similarly.
  switch (p->getKind()) {
  case PatternKind::EnumElement:
  case PatternKind::OptionalSome: {
    // If one is an OptionalSomePattern and one is an EnumElementPattern, then
    // they are the same since the OptionalSomePattern is just sugar for
    // .Some(x).
    if ((isa<OptionalSomePattern>(p) && isa<EnumElementPattern>(first)) ||
        (isa<OptionalSomePattern>(first) && isa<EnumElementPattern>(p)))
      return p;
    LLVM_FALLTHROUGH;
  }
  case PatternKind::Tuple:
  case PatternKind::Named:
  case PatternKind::Any:
  case PatternKind::Bool:
  case PatternKind::Expr: {
    // These kinds are only similar to the same kind.
    if (p->getKind() == first->getKind())
      return p;
    return nullptr;
  }
  case PatternKind::Is: {
    auto pIs = cast<IsPattern>(p);
    // 'is' patterns are only similar to matches to the same type.
    if (auto firstIs = dyn_cast<IsPattern>(first)) {
      if (firstIs->getCastTypeLoc().getType()
            ->isEqual(pIs->getCastTypeLoc().getType()))
        return p;
    }
    return nullptr;
  }
    
  case PatternKind::Paren:
  case PatternKind::Var:
  case PatternKind::Typed:
    llvm_unreachable("not semantic");
  }

  llvm_unreachable("Unhandled PatternKind in switch.");
}

//===----------------------------------------------------------------------===//
//                           SILGenPattern Emission
//===----------------------------------------------------------------------===//

namespace {

/// A row which we intend to specialize.
struct RowToSpecialize {
  /// The pattern from this row which we are specializing upon.
  Pattern *Pattern;

  /// The index of the target row.
  unsigned RowIndex;

  /// Whether the row will be irrefutable after this specialization.
  bool Irrefutable;

  /// Profile Count of hte row we intend to specialize.
  ProfileCounter Count;
};

/// Changes that we wish to apply to a row which we have specialized.
struct SpecializedRow {
  /// The patterns which should replace the specialized pattern.
  SmallVector<Pattern *, 4> Patterns;

  /// The index of the target row.
  unsigned RowIndex;
};

/// An array of arguments.
using ArgArray = ArrayRef<ConsumableManagedValue>;

/// A callback which dispatches a failure case.
using FailureHandler =
  std::function<void(SILLocation failureLoc)>;

/// A callback which redispatches a set of specialized rows.
using SpecializationHandler =
  std::function<void(ArgArray values, ArrayRef<SpecializedRow> rowChanges,
                     const FailureHandler &contDest)>;

class ClauseMatrix;
class ClauseRow;

/// A class controlling the emission of the decision tree for a pattern match
/// statement (switch, if/let, or while/let condition).
///
/// The value cleanup rules during pattern match emission are complicated
/// because we're trying to allow as much borrowing/forwarding of
/// values as possible, so that we only need to actually copy/retain
/// values as late as possible.  This means we end up having to do
/// a pretty delicate dance to manage the active set of cleanups.
///
/// We split values into three categories:
///   - TakeAlways (which are owned by the current portion of the
///     decision tree)
///   - CopyOnSuccess (which are not owned at all by the current
///     portion of the decision tree)
///   - TakeOnSuccess (which are owned only if the decision tree
///     actually passes all guards and enters a case block)
/// In particular, it is important that a TakeOnSuccess value not be
/// destructively modified unless success is assured.
///
/// Whenever the decision tree branches, it must forward values down
/// correctly.  A TakeAlways value becomes TakeOnSuccess for all but
/// last branch of the tree.
///
/// Values should be forwarded down the decision tree with the
/// appropriate cleanups.  CopyOnSuccess values should not have
/// attached cleanups.  TakeAlways or TakeOnSuccess values should have
/// cleanups when their types are non-trivial.  When a value is
/// forwarded down into a branch of the decision tree, its cleanup
/// might be deactivated within that subtree; to protect against the
/// cleanup being removed when this happens, the cleanup must be first
/// put in the PersistentlyActive state before the emission of the
/// subtree, then restored to its current state when the subtree is
/// finished.
///
/// The set of active cleanups should always be instantaneously
/// consistent: that is, there should always be exactly one cleanup
/// tracking a +1 value.  It's okay to deactivate a cleanup for a
/// TakeOnSuccess value and then introduce new cleanups for all of its
/// subobjects.  Jumps outside of the decision tree entirely will be
/// fine: the jump will simply destroy the subobjects instead of the
/// aggregate.  However, jumps to somewhere else within the decision
/// tree require careful attention if the jump could lead to a
/// cleanups depth outside the subobject cleanups (causing them to be
/// run) but inside the old cleanup (in which case it will be
/// reactivated).  Therefore, such borrowings must be "unforwarded"
/// during the emission of such jumps by disabling the new cleanups
/// and re-enabling the outer cleanup.  It's okay to re-enable
/// cleanups like this because these jumps only occur when a branch of
/// the decision tree fails with a non-exhaustive match, which means
/// the value should have been passed down as TakeOnSuccess, and the
/// decision tree is not allowed to destructively modify objects that
/// are TakeOnSuccess when failure is still a possibility.
class PatternMatchEmission {
  PatternMatchEmission(const PatternMatchEmission &) = delete;
  PatternMatchEmission &operator=(const PatternMatchEmission &) = delete;

  SILGenFunction &SGF;
  
  /// PatternMatchStmt - The 'switch', or do-catch statement that we're emitting
  /// this pattern match for.
  Stmt *PatternMatchStmt;
  CleanupsDepth PatternMatchStmtDepth;
  llvm::MapVector<CaseStmt*, std::pair<SILBasicBlock*, bool>> SharedCases;

  llvm::DenseMap<VarDecl*, SILValue> Temporaries;

  using CompletionHandlerTy =
    llvm::function_ref<void(PatternMatchEmission &, ArgArray, ClauseRow &)>;
  CompletionHandlerTy CompletionHandler;
public:
  
  PatternMatchEmission(SILGenFunction &SGF, Stmt *S,
                       CompletionHandlerTy completionHandler)
    : SGF(SGF), PatternMatchStmt(S),
      CompletionHandler(completionHandler) {}

  Optional<SILLocation> getSubjectLocationOverride(SILLocation loc) const {
    if (auto *Switch = dyn_cast<SwitchStmt>(PatternMatchStmt))
      if (!Switch->isImplicit())
        return SILLocation(Switch->getSubjectExpr());
    return None;
  }

  void emitDispatch(ClauseMatrix &matrix, ArgArray args,
                    const FailureHandler &failure);

  void initSharedCaseBlockDest(CaseStmt *caseBlock, bool hasFallthroughTo);

  void emitAddressOnlyAllocations();

  void emitAddressOnlyInitialization(VarDecl *dest, SILValue value);

  JumpDest getSharedCaseBlockDest(CaseStmt *caseStmt);

  void emitSharedCaseBlocks();

  void emitCaseBody(CaseStmt *caseBlock);

private:
  void emitWildcardDispatch(ClauseMatrix &matrix, ArgArray args, unsigned row,
                            const FailureHandler &failure);

  void bindRefutablePatterns(const ClauseRow &row, ArgArray args,
                             const FailureHandler &failure);

  void emitGuardBranch(SILLocation loc, Expr *guard,
                       const FailureHandler &failure);

  void bindIrrefutablePatterns(const ClauseRow &row, ArgArray args,
                               bool forIrrefutableRow, bool hasMultipleItems);

  void bindVariable(Pattern *pattern, VarDecl *var,
                    ConsumableManagedValue value, bool isIrrefutable,
                    bool hasMultipleItems);

  void emitSpecializedDispatch(ClauseMatrix &matrix, ArgArray args,
                               unsigned &lastRow, unsigned column,
                               const FailureHandler &failure);
  void emitTupleObjectDispatch(ArrayRef<RowToSpecialize> rows,
                               ConsumableManagedValue src,
                               const SpecializationHandler &handleSpec,
                               const FailureHandler &failure);
  void emitTupleDispatch(ArrayRef<RowToSpecialize> rows,
                         ConsumableManagedValue src,
                         const SpecializationHandler &handleSpec,
                         const FailureHandler &failure);
  void emitIsDispatch(ArrayRef<RowToSpecialize> rows,
                      ConsumableManagedValue src,
                      const SpecializationHandler &handleSpec,
                      const FailureHandler &failure);
  void emitEnumElementDispatchWithOwnership(
      ArrayRef<RowToSpecialize> rows, ConsumableManagedValue src,
      const SpecializationHandler &handleSpec, const FailureHandler &failure,
      ProfileCounter defaultCaseCount);
  void emitEnumElementDispatch(ArrayRef<RowToSpecialize> rows,
                               ConsumableManagedValue src,
                               const SpecializationHandler &handleSpec,
                               const FailureHandler &failure,
                               ProfileCounter defaultCaseCount);
  void emitBoolDispatch(ArrayRef<RowToSpecialize> rows,
                        ConsumableManagedValue src,
                        const SpecializationHandler &handleSpec,
                        const FailureHandler &failure);
};

/// A handle to a row in a clause matrix. Does not own memory; use of the
/// ClauseRow must be dominated by its originating ClauseMatrix.
///
/// TODO: This should be refactored into a more general formulation that uses a
/// child template pattern to inject our logic. This will then allow us to
/// inject "mock" objects in a unittest file.
class ClauseRow {
  friend class ClauseMatrix;
  
  Stmt *ClientData;
  Pattern *CasePattern;
  Expr *CaseGuardExpr;
  
  
  /// HasFallthroughTo - True if there is a fallthrough into this case.
  bool HasFallthroughTo;


  /// The number of remaining specializations until this row becomes
  /// irrefutable.
  unsigned NumRemainingSpecializations;

  SmallVector<Pattern*, 4> Columns;

public:
  ClauseRow(Stmt *clientData, Pattern *CasePattern, Expr *CaseGuardExpr,
            bool HasFallthroughTo)
    : ClientData(clientData),
      CasePattern(CasePattern),
      CaseGuardExpr(CaseGuardExpr),
      HasFallthroughTo(HasFallthroughTo) {
    Columns.push_back(CasePattern);
    if (CaseGuardExpr)
      NumRemainingSpecializations = AlwaysRefutable;
    else 
      NumRemainingSpecializations = getNumSpecializations(Columns[0]);
  }

  template<typename T>
  T *getClientData() const {
    return static_cast<T*>(ClientData);
  }

  Pattern *getCasePattern() const { return CasePattern; }
  Expr *getCaseGuardExpr() const { return CaseGuardExpr; }
  bool hasFallthroughTo() const { return HasFallthroughTo; }
  
  ArrayRef<Pattern *> getColumns() const {
    return Columns;
  }
  MutableArrayRef<Pattern *> getColumns() {
    return Columns;
  }

  /// Specialize the given column to the given array of new columns.
  ///
  /// Places the new columns using the column-specialization algorithm.
  void specializeInPlace(unsigned column, ArrayRef<Pattern *> newColumns) {
    // We assume that this method always removes one level of pattern
    // and replacing it with its direct sub-patterns.  Therefore, we
    // can adjust the number of remaining specializations very easily.
    //
    // We don't need to test whether NumRemainingSpecializations is
    // AlwaysRefutable before decrementing because we only ever test
    // this value against zero.
    if (isDirectlyRefutablePattern(Columns[column]))
      NumRemainingSpecializations--;

    if (newColumns.size() == 1) {
      Columns[column] = newColumns[0];
    } else if (newColumns.empty()) {
      if (column + 1 == Columns.size()) {
        Columns.pop_back();
      } else {
        Columns[column] = Columns.pop_back_val();
      }
    } else {
      Columns[column] = newColumns[0];
      Columns.append(newColumns.begin() + 1, newColumns.end());
    }
  }

  /// Is this row currently irrefutable?
  bool isIrrefutable() const {
    return NumRemainingSpecializations == 0;
  }

  /// Will this row be irrefutable after we single-step specialize the
  /// given column?
  bool isIrrefutableAfterSpecializing(unsigned column) const {
    if (NumRemainingSpecializations == 1)
      return isDirectlyRefutablePattern(Columns[column]);
    return NumRemainingSpecializations == 0;
  }
  
  Pattern * const *begin() const {
    return getColumns().begin();
  }
  Pattern * const *end() const {
    return getColumns().end();
  }
  
  Pattern **begin() {
    return getColumns().begin();
  }
  Pattern **end() {
    return getColumns().end();
  }
  
  Pattern *operator[](unsigned column) const {
    return getColumns()[column];
  }
  Pattern *&operator[](unsigned column) {
    return getColumns()[column];
  }
  unsigned columns() const {
    return Columns.size();
  }

  LLVM_ATTRIBUTE_USED void dump() const { return print(llvm::errs()); }
  void print(llvm::raw_ostream &out) const;
};

/// A clause matrix. This matrix associates subpattern rows to their
/// corresponding guard expressions, and associates destination basic block
/// and columns to their associated subject value.
class ClauseMatrix {
  SmallVector<ClauseRow *, 4> Rows;

  ClauseMatrix(const ClauseMatrix &) = delete;
  ClauseMatrix &operator=(const ClauseMatrix &) = delete;
  ClauseMatrix() = default;
public:
  /// Create a clause matrix from the given pattern-row storage.
  /// (actively matched values) and enough initial capacity for the
  /// given number of rows. The clause matrix will be initialized with zero rows
  /// and a column for every occurrence. Rows can be added using addRows.
  explicit ClauseMatrix(MutableArrayRef<ClauseRow> rows) {
    for (ClauseRow &row : rows) {
      Rows.push_back(&row);
    }
  }

  ClauseMatrix(ClauseMatrix &&) = default;
  ClauseMatrix &operator=(ClauseMatrix &&) = default;
  
  unsigned rows() const { return Rows.size(); }

  ClauseRow &operator[](unsigned row) {
    return *Rows[row];
  }
  const ClauseRow &operator[](unsigned row) const {
    return *Rows[row];
  }

  /// Destructively specialize the rows of this clause matrix.  The
  /// rows should not be used in this matrix afterwards.
  ClauseMatrix specializeRowsInPlace(unsigned column,
                                     ArrayRef<SpecializedRow> newRows) {
    assert(!newRows.empty() && "specializing for an empty set of rows?");

    ClauseMatrix innerMatrix;
    for (unsigned i = 0, e = newRows.size(); i != e; ++i) {
      assert((i == 0 || newRows[i - 1].RowIndex < newRows[i].RowIndex) &&
             "specialized rows are out of order?");

      ClauseRow *rowData = Rows[newRows[i].RowIndex];
      rowData->specializeInPlace(column, newRows[i].Patterns);
      innerMatrix.Rows.push_back(rowData);
    }
    return innerMatrix;
  }

  LLVM_ATTRIBUTE_USED void dump() const { return print(llvm::errs()); }
  void print(llvm::raw_ostream &out) const;
};

} // end anonymous namespace

void ClauseRow::print(llvm::raw_ostream &out) const {
  out << "[ ";
  for (const Pattern *column : *this) {
    dumpPattern(column, out);
    out << ' ';
  }
  out << "]\n";
}

void ClauseMatrix::print(llvm::raw_ostream &out) const {
  if (Rows.empty()) { return; }

  // Tabulate the strings for each column, row-major.
  // We need to pad the strings out like a real matrix.
  SmallVector<std::vector<std::string>, 4> patternStrings;
  SmallVector<size_t, 4> columnSizes;

  patternStrings.resize(Rows.size());
    
  llvm::formatted_raw_ostream fos(out);
    
  for (unsigned r = 0, rend = rows(); r < rend; ++r) {
    const ClauseRow &row = (*this)[r];
    auto &rowStrings = patternStrings[r];

    // Make sure that column sizes has an entry for all our columns.
    if (row.columns() > columnSizes.size())
      columnSizes.resize(row.columns(), 0);
    rowStrings.reserve(row.columns());

    for (unsigned c = 0, cend = row.columns(); c < cend; ++c) {
      rowStrings.push_back("");
      std::string &str = rowStrings.back();
      {
        llvm::raw_string_ostream ss(str);
        dumpPattern(row[c], ss);
        ss.flush();
      }

      columnSizes[c] = std::max(columnSizes[c], str.size());
    }
  }

  for (unsigned r = 0, rend = rows(); r < rend; ++r) {
    fos << "[ ";
    for (unsigned c = 0, cend = patternStrings[r].size(); c < cend; ++c) {
      unsigned start = fos.getColumn();
      fos << patternStrings[r][c];
      fos.PadToColumn(start + columnSizes[c] + 1);
    }
    fos << "]\n";
  }
  fos.flush();
}

/// Forward a value down into a branch of the decision tree that may
/// fail and lead back to other branch(es).
///
/// Essentially equivalent to forwardIntoIrrefutableSubtree, except it
/// converts AlwaysTake to TakeOnSuccess.
static ConsumableManagedValue
forwardIntoSubtree(SILGenFunction &SGF, SILLocation loc,
                   CleanupStateRestorationScope &scope,
                   ConsumableManagedValue outerCMV) {
  ManagedValue outerMV = outerCMV.getFinalManagedValue();
  if (!outerMV.hasCleanup()) return outerCMV;

  auto consumptionKind = outerCMV.getFinalConsumption();
  (void)consumptionKind;
  assert((consumptionKind == CastConsumptionKind::TakeAlways ||
          consumptionKind == CastConsumptionKind::TakeOnSuccess) &&
         "non-+1 consumption with a cleanup?");
  scope.pushCleanupState(outerMV.getCleanup(),
                         CleanupState::PersistentlyActive);

  // If SILOwnership is enabled and we have an object, borrow instead of take on
  // success.
  if (SGF.F.getModule().getOptions().EnableSILOwnership &&
      outerMV.getType().isObject()) {
    return {outerMV.borrow(SGF, loc), CastConsumptionKind::BorrowAlways};
  }

  // Success means that we won't end up in the other branch,
  // but failure doesn't.
  return { outerMV, CastConsumptionKind::TakeOnSuccess };
}

/// Forward a value down into an irrefutable branch of the decision tree.
///
/// Essentially equivalent to forwardIntoSubtree, except it preserves
/// AlwaysTake consumption.
static void forwardIntoIrrefutableSubtree(SILGenFunction &SGF,
                                          CleanupStateRestorationScope &scope,
                                          ConsumableManagedValue outerCMV) {
  ManagedValue outerMV = outerCMV.getFinalManagedValue();
  if (!outerMV.hasCleanup()) return;

  assert(outerCMV.getFinalConsumption() != CastConsumptionKind::CopyOnSuccess
         && "copy-on-success value with cleanup?");
  scope.pushCleanupState(outerMV.getCleanup(),
                         CleanupState::PersistentlyActive);

}

namespace {

class ArgForwarderBase {
  SILGenFunction &SGF;
  CleanupStateRestorationScope Scope;

protected:
  ArgForwarderBase(SILGenFunction &SGF) : SGF(SGF), Scope(SGF.Cleanups) {}

  ConsumableManagedValue forward(ConsumableManagedValue value,
                                 SILLocation loc) {
    return forwardIntoSubtree(SGF, loc, Scope, value);
  }

  void forwardIntoIrrefutable(ConsumableManagedValue value) {
    return forwardIntoIrrefutableSubtree(SGF, Scope, value);
  }
};

/// A RAII-ish object for forwarding a bunch of arguments down to one
/// side of a branch.
class ArgForwarder : private ArgForwarderBase {
  ArgArray OuterArgs;
  SmallVector<ConsumableManagedValue, 4> ForwardedArgsBuffer;

public:
  ArgForwarder(SILGenFunction &SGF, ArgArray outerArgs, SILLocation loc,
               bool isFinalUse)
      : ArgForwarderBase(SGF), OuterArgs(outerArgs) {
    // If this is a final use along this path, we don't need to change
    // any of the args.  However, we do need to make sure that the
    // cleanup state gets restored later, because being final on this
    // path isn't the same as being final along all paths.
    if (isFinalUse) {
      for (auto &outerArg : outerArgs)
        forwardIntoIrrefutable(outerArg);
    } else {
      ForwardedArgsBuffer.reserve(outerArgs.size());
      for (auto &outerArg : outerArgs)
        ForwardedArgsBuffer.push_back(forward(outerArg, loc));
    }
  }

  ArgArray getForwardedArgs() const {
    if (didForwardArgs()) return ForwardedArgsBuffer;
    return OuterArgs;
  }

private:
  bool didForwardArgs() const { return !ForwardedArgsBuffer.empty(); }
};

/// A RAII-ish object for forwarding a bunch of arguments down to one
/// side of a branch.
class SpecializedArgForwarder : private ArgForwarderBase {
  ArgArray OuterArgs;
  bool IsFinalUse;
  SmallVector<ConsumableManagedValue, 4> ForwardedArgsBuffer;

public:
  /// Construct a specialized arg forwarder for a (locally) successful
  /// dispatch.
  SpecializedArgForwarder(SILGenFunction &SGF, ArgArray outerArgs,
                          unsigned column, ArgArray newArgs, SILLocation loc,
                          bool isFinalUse)
      : ArgForwarderBase(SGF), OuterArgs(outerArgs), IsFinalUse(isFinalUse) {
    assert(column < outerArgs.size());

    ForwardedArgsBuffer.reserve(outerArgs.size() - 1 + newArgs.size());

    // Place the new columns with the column-specialization algorithm:
    //  - place the first new column (if any) in the same position as the
    //    original column;
    //  - if there are no new columns, and the removed column was not
    //    the last column, the last column is moved to the removed column.

    // The outer columns before the specialized column.
    for (unsigned i = 0, e = column; i != e; ++i)
      ForwardedArgsBuffer.push_back(forward(outerArgs[i], loc));

    // The specialized column.
    if (!newArgs.empty()) {
      ForwardedArgsBuffer.push_back(newArgs[0]);
      newArgs = newArgs.slice(1);
    } else if (column + 1 < outerArgs.size()) {
      ForwardedArgsBuffer.push_back(forward(outerArgs.back(), loc));
      outerArgs = outerArgs.slice(0, outerArgs.size() - 1);
    }

    // The rest of the outer columns.
    for (unsigned i = column + 1, e = outerArgs.size(); i != e; ++i)
      ForwardedArgsBuffer.push_back(forward(outerArgs[i], loc));

    // The rest of the new args.
    ForwardedArgsBuffer.append(newArgs.begin(), newArgs.end());
  }

  /// Returns the forward arguments.  The new rows are placed using
  /// the column-specialization algorithm.
  ArgArray getForwardedArgs() const {
    return ForwardedArgsBuffer;
  }

private:
  ConsumableManagedValue forward(ConsumableManagedValue value,
                                 SILLocation loc) {
    if (IsFinalUse) {
      ArgForwarderBase::forwardIntoIrrefutable(value);
      return value;
    } else {
      return ArgForwarderBase::forward(value, loc);
    }
  }
};

/// A RAII-ish object for undoing the forwarding of cleanups along a
/// failure path.
class ArgUnforwarder {
  SILGenFunction &SGF;
  CleanupStateRestorationScope Scope;
public:
  ArgUnforwarder(SILGenFunction &SGF) : SGF(SGF), Scope(SGF.Cleanups) {}

  static bool requiresUnforwarding(SILGenFunction &SGF,
                                   ConsumableManagedValue operand) {
    return operand.hasCleanup() &&
           operand.getFinalConsumption()
             == CastConsumptionKind::TakeOnSuccess;
  }

  /// Given that an aggregate was divided into a set of borrowed
  /// values which are now being tracked individually, temporarily
  /// disable all of the borrowed-value cleanups and restore the
  /// aggregate cleanup.
  void unforwardBorrowedValues(ConsumableManagedValue aggregate,
                               ArgArray subobjects) {
    if (!requiresUnforwarding(SGF, aggregate))
      return;
    Scope.pushCleanupState(aggregate.getCleanup(), CleanupState::Active);
    for (auto &subobject : subobjects) {
      if (subobject.hasCleanup())
        Scope.pushCleanupState(subobject.getCleanup(), CleanupState::Dormant);
    }
  }
};

} // end anonymous namespace

/// Return the dispatchable length of the given column.
static unsigned getConstructorPrefix(const ClauseMatrix &matrix,
                                     unsigned firstRow, unsigned column) {
  assert(firstRow < matrix.rows() &&
         "getting column constructor prefix in matrix with no rows remaining?");

  // Require the first row to be a non-wildcard.
  auto first = getSpecializingPattern(matrix[firstRow][column]);
  if (!first) return 0;

  // Then count the number of rows with the same kind of pattern.
  unsigned row = firstRow + 1;
  for (unsigned rend = matrix.rows(); row < rend; ++row) {
    if (!getSimilarSpecializingPattern(matrix[row][column], first))
      break;
  }
  return row - firstRow;
}
    
/// Select the "necessary column", Maranget's term for the column
/// most likely to give an optimal decision tree.
///
/// \return None if we didn't find a meaningful necessary column
static Optional<unsigned>
chooseNecessaryColumn(const ClauseMatrix &matrix, unsigned firstRow) {
  assert(firstRow < matrix.rows() &&
         "choosing necessary column of matrix with no rows remaining?");

  // First of all, if we have zero or one columns, this is trivial
  // to decide.
  auto numColumns = matrix[firstRow].columns();
  if (numColumns <= 1) {
    if (numColumns == 1 && !isWildcardPattern(matrix[firstRow][0])) {
      return 0;
    }
    return None;
  }

  // Use the "constructor prefix" heuristic from Maranget to pick the
  // necessary column. The column with the most pattern nodes prior to a
  // wildcard turns out to be a good and cheap-to-calculate heuristic for
  // generating an optimal decision tree.  We ignore patterns that aren't
  // similar to the head pattern.
  Optional<unsigned> bestColumn;
  unsigned longestConstructorPrefix = 0;
  for (unsigned c = 0; c != numColumns; ++c) {
    unsigned constructorPrefix = getConstructorPrefix(matrix, firstRow, c);
    if (constructorPrefix > longestConstructorPrefix) {
      longestConstructorPrefix = constructorPrefix;
      bestColumn = c;
    }
  }

  return bestColumn;
}

/// Recursively emit a decision tree from the given pattern matrix.
void PatternMatchEmission::emitDispatch(ClauseMatrix &clauses, ArgArray args,
                                        const FailureHandler &outerFailure) {
  unsigned firstRow = 0;
  while (true) {
    // If there are no rows remaining, then we fail.
    if (firstRow == clauses.rows()) {
      outerFailure(clauses[clauses.rows() - 1].getCasePattern());
      return;
    }
    
    // Try to find a "necessary column".
    Optional<unsigned> column = chooseNecessaryColumn(clauses, firstRow);

    // Emit the subtree in its own scope.
    ExitableFullExpr scope(SGF, CleanupLocation(PatternMatchStmt));
    auto innerFailure = [&](SILLocation loc) {
      if (firstRow == clauses.rows()) return outerFailure(loc);
      SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc);
    };

    // If there is no necessary column, just emit the first row.
    if (!column) {
      unsigned wildcardRow = firstRow++;
      emitWildcardDispatch(clauses, args, wildcardRow, innerFailure);
    } else {
      // Otherwise, specialize on the necessary column.
      emitSpecializedDispatch(clauses, args, firstRow, column.getValue(),
                              innerFailure);
    }

    assert(!SGF.B.hasValidInsertionPoint());
    SILBasicBlock *contBB = scope.exit();
    // If the continuation block has no uses, ...
    if (contBB->pred_empty()) {
      // If we have no more rows to emit, clear the IP and destroy the
      // continuation block.
      if (firstRow == clauses.rows()) {
        SGF.B.clearInsertionPoint();
        SGF.eraseBasicBlock(contBB);
        return;
      }
      
      // Otherwise, if there is no fallthrough, then the next row is
      // unreachable: emit a dead code diagnostic.
      if (!clauses[firstRow].hasFallthroughTo()) {
        SourceLoc Loc;
        bool isDefault = false;
        if (auto *S = clauses[firstRow].getClientData<Stmt>()) {
          Loc = S->getStartLoc();
          if (auto *CS = dyn_cast<CaseStmt>(S))
            isDefault = CS->isDefault();
        } else {
          Loc = clauses[firstRow].getCasePattern()->getStartLoc();
        }
        SGF.SGM.diagnose(Loc, diag::unreachable_case, isDefault);
      }
    }
  }
}

/// Emit the decision tree for a row containing only non-specializing
/// patterns.
///
/// \param matrixArgs - appropriate for the entire clause matrix, not
///   just this one row
void PatternMatchEmission::emitWildcardDispatch(ClauseMatrix &clauses,
                                                ArgArray matrixArgs,
                                                unsigned row,
                                                const FailureHandler &failure) {
  // Get appropriate arguments.
  ArgForwarder forwarder(SGF, matrixArgs, clauses[row].getCasePattern(),
                         /*isFinalUse*/ row + 1 == clauses.rows());
  ArgArray args = forwarder.getForwardedArgs();

  // Bind all the refutable patterns first.  We want to do this first
  // so that we can treat the rest of the bindings as inherently
  // successful if we don't have a guard.  This approach assumes that
  // expression patterns can't refer to bound arguments.
  bindRefutablePatterns(clauses[row], args, failure);

  // Okay, the rest of the bindings are irrefutable if there isn't a guard.
  Expr *guardExpr = clauses[row].getCaseGuardExpr();
  bool hasGuard = guardExpr != nullptr;
  assert(!hasGuard || !clauses[row].isIrrefutable());

  auto stmt = clauses[row].getClientData<Stmt>();
  assert(isa<CaseStmt>(stmt) || isa<CatchStmt>(stmt));

  auto *caseStmt = dyn_cast<CaseStmt>(stmt);
  bool hasMultipleItems =
      caseStmt && (clauses[row].hasFallthroughTo() ||
                   caseStmt->getCaseLabelItems().size() > 1);

  // Bind the rest of the patterns.
  bindIrrefutablePatterns(clauses[row], args, !hasGuard, hasMultipleItems);

  // Emit the guard branch, if it exists.
  if (guardExpr) {
    this->emitGuardBranch(guardExpr, guardExpr, failure);
  }

  // Enter the row.
  CompletionHandler(*this, args, clauses[row]);
  assert(!SGF.B.hasValidInsertionPoint());
}

/// Bind all the refutable patterns in the given row.
void PatternMatchEmission::
bindRefutablePatterns(const ClauseRow &row, ArgArray args,
                      const FailureHandler &failure) {
  assert(row.columns() == args.size());
  for (unsigned i = 0, e = args.size(); i != e; ++i) {
    if (!row[i]) // We use null patterns to mean artificial AnyPatterns
      continue;

    Pattern *pattern = row[i]->getSemanticsProvidingPattern();
    switch (pattern->getKind()) {
    // Irrefutable patterns that we'll handle in a later pass.
    case PatternKind::Any:
      break;
    case PatternKind::Named:
      break;

    case PatternKind::Expr: {
      ExprPattern *exprPattern = cast<ExprPattern>(pattern);
      DebugLocOverrideRAII LocOverride{SGF.B,
                                       getSubjectLocationOverride(pattern)};
      FullExpr scope(SGF.Cleanups, CleanupLocation(pattern));
      bindVariable(pattern, exprPattern->getMatchVar(), args[i],
                   /*isForSuccess*/ false, /* hasMultipleItems */ false);
      emitGuardBranch(pattern, exprPattern->getMatchExpr(), failure);
      break;
    }
    default:
      llvm_unreachable("bad pattern kind");
    }
  }
}

/// Bind all the irrefutable patterns in the given row, which is nothing
/// but wildcard patterns.
///
/// Note that forIrrefutableRow can be true even if !row.isIrrefutable()
/// because we might have already bound all the refutable parts.
void PatternMatchEmission::bindIrrefutablePatterns(const ClauseRow &row,
                                                   ArgArray args,
                                                   bool forIrrefutableRow,
                                                   bool hasMultipleItems) {
  assert(row.columns() == args.size());
  for (unsigned i = 0, e = args.size(); i != e; ++i) {
    if (!row[i]) // We use null patterns to mean artificial AnyPatterns
      continue;

    Pattern *pattern = row[i]->getSemanticsProvidingPattern();
    switch (pattern->getKind()) {
    case PatternKind::Any: // We can just drop Any values.
      break;
    case PatternKind::Expr: // Ignore expression patterns, which we should have
                            // bound in an earlier pass.
      break;
    case PatternKind::Named: {
      NamedPattern *named = cast<NamedPattern>(pattern);
      bindVariable(pattern, named->getDecl(), args[i], forIrrefutableRow,
                   hasMultipleItems);
      break;
    }
    default:
      llvm_unreachable("bad pattern kind");
    }
  }
}

/// Should we take control of the mang
static bool shouldTake(ConsumableManagedValue value, bool isIrrefutable) {
  switch (value.getFinalConsumption()) {
  case CastConsumptionKind::TakeAlways: return true;
  case CastConsumptionKind::TakeOnSuccess: return isIrrefutable;
  case CastConsumptionKind::CopyOnSuccess: return false;
  case CastConsumptionKind::BorrowAlways: return false;
  }
  llvm_unreachable("bad consumption kind");
}

/// Bind a variable into the current scope.
void PatternMatchEmission::bindVariable(Pattern *pattern, VarDecl *var,
                                        ConsumableManagedValue value,
                                        bool isIrrefutable,
                                        bool hasMultipleItems) {
  // If this binding is one of multiple patterns, each individual binding
  // will just be let, and then the chosen value will get forwarded into
  // a var box in the final shared case block.
  bool immutable = var->isLet() || hasMultipleItems;

  // Initialize the variable value.
  InitializationPtr init = SGF.emitInitializationForVarDecl(var, immutable);
  CanType formalValueType = pattern->getType()->getCanonicalType();
  RValue rv(SGF, pattern, formalValueType, value.getFinalManagedValue());
  if (shouldTake(value, isIrrefutable)) {
    std::move(rv).forwardInto(SGF, pattern, init.get());
  } else {
    std::move(rv).copyInto(SGF, pattern, init.get());
  }
}

/// Evaluate a guard expression and, if it returns false, branch to
/// the given destination.
void PatternMatchEmission::emitGuardBranch(SILLocation loc, Expr *guard,
                                           const FailureHandler &failure) {
  SILBasicBlock *falseBB = SGF.B.splitBlockForFallthrough();
  SILBasicBlock *trueBB = SGF.B.splitBlockForFallthrough();

  // Emit the match test.
  SILValue testBool;
  {
    FullExpr scope(SGF.Cleanups, CleanupLocation(guard));
    testBool = SGF.emitRValueAsSingleValue(guard).getUnmanagedValue();
  }

  SGF.B.createCondBranch(loc, testBool, trueBB, falseBB);

  SGF.B.setInsertionPoint(falseBB);
  failure(loc);

  SGF.B.setInsertionPoint(trueBB);
}

/// Perform specialized dispatch on the particular column.
///
/// \param matrixArgs - appropriate for the entire clause matrix, not
///   just these specific rows
void PatternMatchEmission::emitSpecializedDispatch(ClauseMatrix &clauses,
                                                   ArgArray matrixArgs,
                                                   unsigned &lastRow,
                                                   unsigned column,
                                               const FailureHandler &failure) {
  // HEY! LISTEN!
  //
  // When a pattern specializes its submatrix (like an 'as' or enum element
  // pattern), it *must* chain the FailureHandler for its inner submatrixes
  // through our `failure` handler if it manipulates any cleanup state.
  // Here's an example from emitEnumElementDispatch:
  //
  //       const FailureHandler *innerFailure = &failure;
  //       FailureHandler specializedFailure = [&](SILLocation loc) {
  //         ArgUnforwarder unforwarder(SGF);
  //         unforwarder.unforwardBorrowedValues(src, origCMV);
  //         failure(loc);
  //       };
  //
  //       if (ArgUnforwarder::requiresUnforwarding(src))
  //         innerFailure = &specializedFailure;
  //
  // Note that the inner failure handler either is exactly the outer failure
  // or performs the work necessary to clean up after the failed specialized
  // decision tree immediately before chaining onto the outer failure.
  // It is specifically NOT correct to do something like this:
  //
  //       /* DON'T DO THIS */
  //       ExitableFullExpr scope;
  //       FailureHandler innerFailure = [&](SILLocation loc) {
  //         emitBranchAndCleanups(scope, loc);
  //       };
  //       ...
  //       /* DON'T DO THIS */
  //       scope.exit();
  //       ArgUnforwarder unforwarder(SGF);
  //       unforwarder.unforwardBorrowedValues(src, origCMV);
  //       failure(loc);
  //       /* DON'T DO THIS */
  //
  // since the cleanup state changes performed by ArgUnforwarder will
  // occur too late.
  
  unsigned firstRow = lastRow;

  // Collect the rows to specialize.
  SmallVector<RowToSpecialize, 4> rowsToSpecialize;
  auto addRowToSpecialize = [&](Pattern *pattern, unsigned rowIndex) {
    assert(getSpecializingPattern(clauses[rowIndex][column]) == pattern);
    bool irrefutable = clauses[rowIndex].isIrrefutableAfterSpecializing(column);
    auto caseBlock = clauses[rowIndex].getClientData<CaseStmt>();
    ProfileCounter count = ProfileCounter();
    if (caseBlock) {
      count = SGF.loadProfilerCount(caseBlock);
    }
    rowsToSpecialize.push_back({pattern, rowIndex, irrefutable, count});
  };

  ProfileCounter defaultCaseCount = ProfileCounter();
  Pattern *firstSpecializer = getSpecializingPattern(clauses[firstRow][column]);
  assert(firstSpecializer && "specializing unspecializable row?");
  addRowToSpecialize(firstSpecializer, firstRow);

  // Take a prefix of rows that share the same semantic kind of pattern.
  for (++lastRow; lastRow != clauses.rows(); ++lastRow) {
    Pattern *specializer =
      getSimilarSpecializingPattern(clauses[lastRow][column], firstSpecializer);
    if (!specializer) {
      auto caseBlock = clauses[lastRow].getClientData<CaseStmt>();
      if (caseBlock) {
        defaultCaseCount = SGF.loadProfilerCount(caseBlock);
      }
      break;
    }
    addRowToSpecialize(specializer, lastRow);
  }
  assert(lastRow - firstRow == rowsToSpecialize.size());

  // Forward just the specialized argument right now.  We'll forward
  // the rest in the handler.
  bool isFinalUse = (lastRow == clauses.rows());
  ArgForwarder outerForwarder(SGF, matrixArgs[column], firstSpecializer,
                              isFinalUse);
  auto arg = outerForwarder.getForwardedArgs()[0];

  SpecializationHandler handler = [&](ArrayRef<ConsumableManagedValue> newArgs,
                                      ArrayRef<SpecializedRow> rows,
                                      const FailureHandler &innerFailure) {
    // These two operations must follow the same rules for column
    // placement because 'arguments' are parallel to the matrix columns.
    // We use the column-specialization algorithm described in
    // specializeInPlace.
    ClauseMatrix innerClauses = clauses.specializeRowsInPlace(column, rows);

    SpecializedArgForwarder innerForwarder(SGF, matrixArgs, column, newArgs,
                                           firstSpecializer, isFinalUse);
    ArgArray innerArgs = innerForwarder.getForwardedArgs();

    emitDispatch(innerClauses, innerArgs, innerFailure);
  };

  switch (firstSpecializer->getKind()) {
  case PatternKind::Any:
  case PatternKind::Expr:
  case PatternKind::Named:
    llvm_unreachable("cannot specialize wildcard pattern");

  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    llvm_unreachable("non-semantic pattern kind!");
  
  case PatternKind::Tuple:
    return emitTupleDispatch(rowsToSpecialize, arg, handler, failure);
  case PatternKind::Is:
    return emitIsDispatch(rowsToSpecialize, arg, handler, failure);
  case PatternKind::EnumElement:
  case PatternKind::OptionalSome:
    return emitEnumElementDispatch(rowsToSpecialize, arg, handler, failure,
                                   defaultCaseCount);
  case PatternKind::Bool:
    return emitBoolDispatch(rowsToSpecialize, arg, handler, failure);
  }
  llvm_unreachable("bad pattern kind");
};

/// Given that we've broken down a source value into this subobject,
/// and that we were supposed to use the given consumption rules on
/// it, construct an appropriate managed value.
static ConsumableManagedValue
getManagedSubobject(SILGenFunction &SGF, SILValue value,
                    const TypeLowering &valueTL,
                    CastConsumptionKind consumption) {
  switch (consumption) {
  case CastConsumptionKind::BorrowAlways:
  case CastConsumptionKind::CopyOnSuccess:
    return {ManagedValue::forUnmanaged(value), consumption};
  case CastConsumptionKind::TakeAlways:
  case CastConsumptionKind::TakeOnSuccess:
    return {SGF.emitManagedRValueWithCleanup(value, valueTL), consumption};
  }
}

static ConsumableManagedValue
emitReabstractedSubobject(SILGenFunction &SGF, SILLocation loc,
                          ConsumableManagedValue value,
                          const TypeLowering &valueTL,
                          AbstractionPattern abstraction,
                          CanType substFormalType) {
  // Return if there's no abstraction.  (The first condition is just
  // a fast path.)
  if (value.getType().getASTType() == substFormalType ||
      value.getType() == SGF.getLoweredType(substFormalType))
    return value;

  // Otherwise, turn to +1 and re-abstract.
  ManagedValue mv = SGF.getManagedValue(loc, value);
  return ConsumableManagedValue::forOwned(
           SGF.emitOrigToSubstValue(loc, mv, abstraction, substFormalType));
}

void PatternMatchEmission::emitTupleObjectDispatch(
    ArrayRef<RowToSpecialize> rows, ConsumableManagedValue src,
    const SpecializationHandler &handleCase,
    const FailureHandler &outerFailure) {
  // Construct the specialized rows.
  SmallVector<SpecializedRow, 4> specializedRows;
  specializedRows.resize(rows.size());
  for (unsigned i = 0, e = rows.size(); i != e; ++i) {
    specializedRows[i].RowIndex = rows[i].RowIndex;

    auto pattern = cast<TuplePattern>(rows[i].Pattern);
    for (auto &elt : pattern->getElements()) {
      specializedRows[i].Patterns.push_back(elt.getPattern());
    }
  }

  auto firstPat = rows[0].Pattern;
  SILLocation loc = firstPat;

  // Final consumption here will be either BorrowAlways or TakeAlways.
  ManagedValue v = src.getFinalManagedValue();

  SmallVector<ConsumableManagedValue, 8> destructured;
  SGF.B.emitDestructureValueOperation(
      loc, v, [&](unsigned index, ManagedValue v) {
        destructured.push_back({v, src.getFinalConsumption()});
      });

  // Since we did all of our work at +0, we just send down the outer failure.
  handleCase(destructured, specializedRows, outerFailure);
}

/// Perform specialized dispatch for tuples.
///
/// This is simple; all the tuples have the same structure.
void PatternMatchEmission::
emitTupleDispatch(ArrayRef<RowToSpecialize> rows, ConsumableManagedValue src,
                  const SpecializationHandler &handleCase,
                  const FailureHandler &outerFailure) {
  auto firstPat = rows[0].Pattern;
  SILLocation loc = firstPat;

  // If our source is an address that is loadable, perform a load_borrow.
  if (src.getType().isAddress() && src.getType().isLoadable(SGF.getModule())) {
    src = {SGF.B.createLoadBorrow(loc, src.getFinalManagedValue()),
           CastConsumptionKind::BorrowAlways};
  }

  // Then if we have an object...
  if (src.getType().isObject()) {
    // Make sure that if we ahve a copy_on_success, non-trivial value that we do
    // not have a value with @owned ownership.
    assert((!src.getType().isTrivial(SGF.getModule()) ||
            src.getFinalConsumption() != CastConsumptionKind::CopyOnSuccess ||
            src.getOwnershipKind() != ValueOwnershipKind::Owned) &&
           "@owned value without cleanup + copy_on_success");

    // If we have are asked to perform TakeOnSuccess, borrow the value instead.
    //
    // The reason why do this for TakeOnSuccess is that we want to not have to
    // deal with unforwarding of aggregate tuples in failing cases since that
    // causes ownership invariants to be violated since we already forwarded the
    // aggregate to create cleanups on its elements.
    //
    // In contrast, we do still want to allow for TakeAlways variants to not
    // need to borrow, so we do not borrow if we take always.
    if (!src.getType().isTrivial(SGF.getModule()) &&
        src.getFinalConsumption() == CastConsumptionKind::TakeOnSuccess) {
      src = {src.getFinalManagedValue().borrow(SGF, loc),
             CastConsumptionKind::BorrowAlways};
    }

    // Then perform a forward or reborrow destructure on the object.
    return emitTupleObjectDispatch(rows, src, handleCase, outerFailure);
  }

  // At this point we know that we must have an address only type, since we
  // would have loaded it earlier.
  SILValue v = src.getFinalManagedValue().forward(SGF);
  assert(v->getType().isAddressOnly(SGF.getModule()) &&
         "Loadable values were handled earlier");

  SmallVector<ConsumableManagedValue, 4> destructured;

  // Break down the values.
  auto tupleSILTy = v->getType();
  for (unsigned i : range(tupleSILTy.castTo<TupleType>()->getNumElements())) {
    SILType fieldTy = tupleSILTy.getTupleElementType(i);
    auto &fieldTL = SGF.getTypeLowering(fieldTy);

    SILValue member = SGF.B.createTupleElementAddr(loc, v, i, fieldTy);
    if (!fieldTL.isAddressOnly()) {
      member =
          fieldTL.emitLoad(SGF.B, loc, member, LoadOwnershipQualifier::Take);
    }
    auto memberCMV = getManagedSubobject(SGF, member, fieldTL,
                                         src.getFinalConsumption());
    destructured.push_back(memberCMV);
  }

  // Construct the specialized rows.
  SmallVector<SpecializedRow, 4> specializedRows;
  specializedRows.resize(rows.size());
  for (unsigned i = 0, e = rows.size(); i != e; ++i) {
    specializedRows[i].RowIndex = rows[i].RowIndex;

    auto pattern = cast<TuplePattern>(rows[i].Pattern);
    for (auto &elt : pattern->getElements()) {
      specializedRows[i].Patterns.push_back(elt.getPattern());
    }
  }

  // Maybe revert to the original cleanups during failure branches.
  const FailureHandler *innerFailure = &outerFailure;
  FailureHandler specializedFailure = [&](SILLocation loc) {
    ArgUnforwarder unforwarder(SGF);
    unforwarder.unforwardBorrowedValues(src, destructured);
    outerFailure(loc);
  };
  if (ArgUnforwarder::requiresUnforwarding(SGF, src))
    innerFailure = &specializedFailure;

  // Recurse.
  handleCase(destructured, specializedRows, *innerFailure);
}

static CanType getTargetType(const RowToSpecialize &row) {
  auto type = cast<IsPattern>(row.Pattern)->getCastTypeLoc().getType();
  return type->getCanonicalType();
}

static ConsumableManagedValue
emitCastOperand(SILGenFunction &SGF, SILLocation loc,
                      ConsumableManagedValue src, CanType sourceType,
                      CanType targetType,
                      SmallVectorImpl<ConsumableManagedValue> &borrowedValues) {
  // Reabstract to the most general abstraction, and put it into a
  // temporary if necessary.

  // Figure out if we need the value to be in a temporary.
  bool requiresAddress = !canUseScalarCheckedCastInstructions(SGF.SGM.M,
                                                        sourceType, targetType);

  AbstractionPattern abstraction = SGF.SGM.M.Types.getMostGeneralAbstraction();
  auto &srcAbstractTL = SGF.getTypeLowering(abstraction, sourceType);

  bool hasAbstraction = (src.getType() != srcAbstractTL.getLoweredType());

  // Fast path: no re-abstraction required.
  if (!hasAbstraction && (!requiresAddress || src.getType().isAddress())) {
    return src;
  }

  std::unique_ptr<TemporaryInitialization> init;
  SGFContext ctx;
  if (requiresAddress) {
    init = SGF.emitTemporary(loc, srcAbstractTL);

    // Okay, if all we need to do is drop the value in an address,
    // this is easy.
    if (!hasAbstraction) {
      ManagedValue finalValue = src.getFinalManagedValue();
      if (finalValue.getOwnershipKind() == ValueOwnershipKind::Guaranteed)
        finalValue = finalValue.copy(SGF, loc);
      SGF.B.emitStoreValueOperation(loc, finalValue.forward(SGF),
                                    init->getAddress(),
                                    StoreOwnershipQualifier::Init);
      init->finishInitialization(SGF);
      ConsumableManagedValue result =
        { init->getManagedAddress(), src.getFinalConsumption() };
      if (ArgUnforwarder::requiresUnforwarding(SGF, src))
        borrowedValues.push_back(result);
      return result;
    }

    ctx = SGFContext(init.get());
  }

  assert(hasAbstraction);
  assert(src.getType().isObject() &&
         "address-only type with abstraction difference?");

  // Produce the value at +1.
  ManagedValue substValue = SGF.getManagedValue(loc, src);
  ManagedValue origValue = 
    SGF.emitSubstToOrigValue(loc, substValue, abstraction, sourceType);
  return ConsumableManagedValue::forOwned(origValue);
}

/// Perform specialized dispatch for a sequence of IsPatterns.
void PatternMatchEmission::emitIsDispatch(ArrayRef<RowToSpecialize> rows,
                                      ConsumableManagedValue src,
                                      const SpecializationHandler &handleCase,
                                      const FailureHandler &failure) {
  CanType sourceType = rows[0].Pattern->getType()->getCanonicalType();
  CanType targetType = getTargetType(rows[0]);

  // Make any abstraction modifications necessary for casting.
  SmallVector<ConsumableManagedValue, 4> borrowedValues;
  ConsumableManagedValue operand =
    emitCastOperand(SGF, rows[0].Pattern, src, sourceType, targetType,
                          borrowedValues);

  // Emit the 'is' check.

  // Build the specialized-rows array.
  SmallVector<SpecializedRow, 4> specializedRows;
  specializedRows.reserve(rows.size());
  for (auto &row : rows) {
    assert(getTargetType(row) == targetType
           && "can only specialize on one type at a time");
    auto is = cast<IsPattern>(row.Pattern);
    specializedRows.push_back({});
    specializedRows.back().RowIndex = row.RowIndex;
    specializedRows.back().Patterns.push_back(is->getSubPattern());
  }

  SILLocation loc = rows[0].Pattern;

  ConsumableManagedValue castOperand = operand.asBorrowedOperand();
  
  // Chain inner failures onto the outer failure.
  const FailureHandler *innerFailure = &failure;
  FailureHandler specializedFailure = [&](SILLocation loc) {
    ArgUnforwarder unforwarder(SGF);
    unforwarder.unforwardBorrowedValues(src, borrowedValues);
    failure(loc);
  };
  if (ArgUnforwarder::requiresUnforwarding(SGF, src))
    innerFailure = &specializedFailure;
  
  // Perform a conditional cast branch.
  SGF.emitCheckedCastBranch(
      loc, castOperand, sourceType, targetType, SGFContext(),
      // Success block: recurse.
      [&](ManagedValue castValue) {
        handleCase(ConsumableManagedValue::forOwned(castValue), specializedRows,
                   *innerFailure);
        assert(!SGF.B.hasValidInsertionPoint() && "did not end block");
      },
      // Failure block: branch out to the continuation block.
      [&](Optional<ManagedValue> mv) { (*innerFailure)(loc); }, rows[0].Count);
}

namespace {
  struct CaseInfo {
    SmallVector<SpecializedRow, 2> SpecializedRows;
    Pattern *FirstMatcher;
    bool Irrefutable = false;
  };

  class CaseBlocks {
    // These vectors are completely parallel, but the switch instructions want
    // only the first two, so we split them up.
    SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 4> CaseBBs;
    SmallVector<ProfileCounter, 4> CaseCounts;
    SmallVector<CaseInfo, 4> CaseInfos;
    SILBasicBlock *DefaultBB = nullptr;

  public:
    /// Create destination blocks for switching over the cases in an enum
    /// defined by \p rows.
    CaseBlocks(SILGenFunction &SGF,
               ArrayRef<RowToSpecialize> rows,
               CanType sourceType,
               SILBasicBlock *curBB);

    ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>>
    getCaseBlocks() const {
      return CaseBBs;
    }

    ArrayRef<ProfileCounter> getCounts() const { return CaseCounts; }

    SILBasicBlock *getDefaultBlock() const { return DefaultBB; }

    void forEachCase(llvm::function_ref<void(EnumElementDecl *,
                                             SILBasicBlock *,
                                             const CaseInfo &)> op) const {
      for_each(CaseBBs, CaseInfos,
               [op](std::pair<EnumElementDecl *, SILBasicBlock *> casePair,
                    const CaseInfo &info) {
        op(casePair.first, casePair.second, info);
      });
    }

    bool hasAnyRefutableCase() const {
      return llvm::any_of(CaseInfos, [](const CaseInfo &info) {
        return !info.Irrefutable;
      });
    }
  };
} // end anonymous namespace

CaseBlocks::CaseBlocks(
    SILGenFunction &SGF,
    ArrayRef<RowToSpecialize> rows,
    CanType sourceType,
    SILBasicBlock *curBB) {

  CaseBBs.reserve(rows.size());
  CaseInfos.reserve(rows.size());
  CaseCounts.reserve(rows.size());

  auto enumDecl = sourceType.getEnumOrBoundGenericEnum();

  llvm::SmallDenseMap<EnumElementDecl *, unsigned, 16> caseToIndex;
  for (auto &row : rows) {
    EnumElementDecl *formalElt;
    Pattern *subPattern = nullptr;
    if (auto eep = dyn_cast<EnumElementPattern>(row.Pattern)) {
      formalElt = eep->getElementDecl();
      subPattern = eep->getSubPattern();
    } else {
      auto *osp = cast<OptionalSomePattern>(row.Pattern);
      formalElt = osp->getElementDecl();
      subPattern = osp->getSubPattern();
    }
    assert(formalElt->getParentEnum() == enumDecl);

    unsigned index = CaseInfos.size();
    auto insertionResult = caseToIndex.insert({formalElt, index});
    if (!insertionResult.second) {
      index = insertionResult.first->second;
    } else {
      curBB = SGF.createBasicBlockAfter(curBB);
      CaseBBs.push_back({formalElt, curBB});
      CaseInfos.push_back(CaseInfo());
      CaseInfos.back().FirstMatcher = row.Pattern;
      CaseCounts.push_back(row.Count);
    }
    assert(caseToIndex[formalElt] == index);
    assert(CaseBBs[index].first == formalElt);

    auto &info = CaseInfos[index];
    info.Irrefutable = (info.Irrefutable || row.Irrefutable);
    info.SpecializedRows.push_back(SpecializedRow());
    auto &specRow = info.SpecializedRows.back();
    specRow.RowIndex = row.RowIndex;

    // Use the row pattern, if it has one.
    if (subPattern) {
      specRow.Patterns.push_back(subPattern);
      // It's also legal to write:
      //   case .Some { ... }
      // which is an implicit wildcard.
    } else {
      specRow.Patterns.push_back(nullptr);
    }
  }

  assert(CaseBBs.size() == CaseInfos.size());

  // Check to see if the enum may have values beyond the cases we can see
  // at compile-time. This includes future cases (for resilient enums) and
  // random values crammed into C enums.
  bool canAssumeExhaustive =
      enumDecl->isEffectivelyExhaustive(SGF.getModule().getSwiftModule(),
                                        SGF.F.getResilienceExpansion());
  if (canAssumeExhaustive) {
    // Check that Sema didn't let any cases slip through.
    canAssumeExhaustive = llvm::all_of(enumDecl->getAllElements(),
                                       [&](const EnumElementDecl *elt) {
      return caseToIndex.count(elt);
    });
  }

  if (!canAssumeExhaustive)
    DefaultBB = SGF.createBasicBlockAfter(curBB);
}

/// Perform specialized dispatch for a sequence of EnumElementPattern or an
/// OptionalSomePattern.
void PatternMatchEmission::emitEnumElementDispatchWithOwnership(
    ArrayRef<RowToSpecialize> rows, ConsumableManagedValue src,
    const SpecializationHandler &handleCase, const FailureHandler &outerFailure,
    ProfileCounter defaultCastCount) {
  assert(src.getFinalConsumption() != CastConsumptionKind::TakeOnSuccess &&
         "SIL ownership does not support TakeOnSuccess");

  CanType sourceType = rows[0].Pattern->getType()->getCanonicalType();

  // Collect the cases and specialized rows.
  CaseBlocks blocks{SGF, rows, sourceType, SGF.B.getInsertionBB()};

  SILLocation loc = PatternMatchStmt;
  loc.setDebugLoc(rows[0].Pattern);
  // SEMANTIC SIL TODO: Once we have the representation of a switch_enum that
  // can take a +0 value, this extra copy should be a borrow.
  SILValue srcValue = src.getFinalManagedValue().copy(SGF, loc).forward(SGF);
  SGF.B.createSwitchEnum(loc, srcValue, blocks.getDefaultBlock(),
                         blocks.getCaseBlocks(), blocks.getCounts(),
                         defaultCastCount);

  // Okay, now emit all the cases.
  blocks.forEachCase([&](EnumElementDecl *elt, SILBasicBlock *caseBB,
                         const CaseInfo &caseInfo) {
    SILLocation loc = caseInfo.FirstMatcher;
    auto &specializedRows = caseInfo.SpecializedRows;

    SGF.B.setInsertionPoint(caseBB);

    // We're in conditionally-executed code; enter a scope.
    Scope scope(SGF.Cleanups, CleanupLocation::get(loc));

    // Create a BB argument or 'unchecked_take_enum_data_addr'
    // instruction to receive the enum case data if it has any.

    SILType eltTy;
    bool hasElt = false;
    if (elt->hasAssociatedValues()) {
      eltTy = src.getType().getEnumElementType(elt, SGF.SGM.M);
      hasElt = !eltTy.getASTType()->isVoid();
    }

    ConsumableManagedValue eltCMV, origCMV;

    // Empty cases.  Try to avoid making an empty tuple value if it's
    // obviously going to be ignored.  This assumes that we won't even
    // try to touch the value in such cases, although we may touch the
    // cleanup (enough to see that it's not present).
    if (!hasElt) {
      bool hasNonAny = false;
      for (auto &specRow : specializedRows) {
        auto pattern = specRow.Patterns[0];
        if (pattern &&
            !isa<AnyPattern>(pattern->getSemanticsProvidingPattern())) {
          hasNonAny = true;
          break;
        }
      }

      SILValue result;
      if (hasNonAny) {
        result = SGF.emitEmptyTuple(loc);
      } else {
        result = SILUndef::get(SGF.SGM.Types.getEmptyTupleType(), SGF.SGM.M);
      }
      origCMV = ConsumableManagedValue::forUnmanaged(result);
      eltCMV = origCMV;

      // Okay, specialize on the argument.
    } else {
      auto *eltTL = &SGF.getTypeLowering(eltTy);

      SILValue eltValue =
          caseBB->createPhiArgument(eltTy, ValueOwnershipKind::Owned);

      // We performed a copy early, so we get a +1 value here.
      origCMV = getManagedSubobject(SGF, eltValue, *eltTL,
                                    CastConsumptionKind::TakeAlways);
      eltCMV = origCMV;

      // If the payload is boxed, project it.
      if (elt->isIndirect() || elt->getParentEnum()->isIndirect()) {
        SILValue boxedValue =
            SGF.B.createProjectBox(loc, origCMV.getValue(), 0);
        eltTL = &SGF.getTypeLowering(boxedValue->getType());
        if (eltTL->isLoadable()) {
          ManagedValue newLoadedBoxValue = SGF.B.createLoadBorrow(
              loc, ManagedValue::forUnmanaged(boxedValue));
          boxedValue = newLoadedBoxValue.getUnmanagedValue();
        }

        // The boxed value may be shared, so we always have to copy it.
        eltCMV = getManagedSubobject(SGF, boxedValue, *eltTL,
                                     CastConsumptionKind::CopyOnSuccess);
      }

      // Reabstract to the substituted type, if needed.

      CanType substEltTy =
          sourceType
              ->getTypeOfMember(SGF.SGM.M.getSwiftModule(), elt,
                                elt->getArgumentInterfaceType())
              ->getCanonicalType();

      AbstractionPattern origEltTy =
          (elt->getParentEnum()->isOptionalDecl()
               ? AbstractionPattern(substEltTy)
               : SGF.SGM.M.Types.getAbstractionPattern(elt));

      eltCMV = emitReabstractedSubobject(SGF, loc, eltCMV, *eltTL, origEltTy,
                                         substEltTy);
    }

    handleCase(eltCMV, specializedRows, outerFailure);
    assert(!SGF.B.hasValidInsertionPoint() && "did not end block");
  });

  // Emit the default block if we needed one.
  if (SILBasicBlock *defaultBB = blocks.getDefaultBlock()) {
    SGF.B.setInsertionPoint(defaultBB);
    SGF.B.createOwnedPhiArgument(src.getType());
    outerFailure(rows.back().Pattern);
  }
}

/// Perform specialized dispatch for a sequence of EnumElementPattern or an
/// OptionalSomePattern.
void PatternMatchEmission::emitEnumElementDispatch(
    ArrayRef<RowToSpecialize> rows, ConsumableManagedValue src,
    const SpecializationHandler &handleCase, const FailureHandler &outerFailure,
    ProfileCounter defaultCaseCount) {
  // If sil ownership is enabled and we have that our source type is an object,
  // use the dispatch code path.
  if (SGF.getOptions().EnableSILOwnership && src.getType().isObject()) {
    return emitEnumElementDispatchWithOwnership(rows, src, handleCase,
                                                outerFailure, defaultCaseCount);
  }

  CanType sourceType = rows[0].Pattern->getType()->getCanonicalType();

  // Collect the cases and specialized rows.
  CaseBlocks blocks{SGF, rows, sourceType, SGF.B.getInsertionBB()};

  // Emit the switch_enum{_addr} instruction.
  bool addressOnlyEnum = src.getType().isAddress();

  // We lack a SIL instruction to nondestructively project data from an
  // address-only enum, so we can only do so in place if we're allowed to take
  // the source always. Copy the source if we can't.
  if (addressOnlyEnum) {
    switch (src.getFinalConsumption()) {
    case CastConsumptionKind::TakeAlways:
    case CastConsumptionKind::CopyOnSuccess:
    case CastConsumptionKind::BorrowAlways:
      // No change to src necessary.
      break;

    case CastConsumptionKind::TakeOnSuccess:
      // If any of the specialization cases is refutable, we must copy.
      if (!blocks.hasAnyRefutableCase())
        break;

      src = ConsumableManagedValue(ManagedValue::forUnmanaged(src.getValue()),
                                   CastConsumptionKind::CopyOnSuccess);
      break;
    }
  }

  SILValue srcValue = src.getFinalManagedValue().forward(SGF);
  SILLocation loc = PatternMatchStmt;
  loc.setDebugLoc(rows[0].Pattern);
  if (addressOnlyEnum) {
    SGF.B.createSwitchEnumAddr(loc, srcValue, blocks.getDefaultBlock(),
                               blocks.getCaseBlocks(), blocks.getCounts(),
                               defaultCaseCount);
  } else {
    SGF.B.createSwitchEnum(loc, srcValue, blocks.getDefaultBlock(),
                           blocks.getCaseBlocks(), blocks.getCounts(),
                           defaultCaseCount);
  }

  // Okay, now emit all the cases.
  blocks.forEachCase([&](EnumElementDecl *elt, SILBasicBlock *caseBB,
                         const CaseInfo &caseInfo) {
    SILLocation loc = caseInfo.FirstMatcher;
    auto &specializedRows = caseInfo.SpecializedRows;

    SGF.B.setInsertionPoint(caseBB);

    // We're in conditionally-executed code; enter a scope.
    Scope scope(SGF.Cleanups, CleanupLocation::get(loc));
      
    // Create a BB argument or 'unchecked_take_enum_data_addr'
    // instruction to receive the enum case data if it has any.

    SILType eltTy;
    bool hasElt = false;
    if (elt->hasAssociatedValues()) {
      eltTy = src.getType().getEnumElementType(elt, SGF.SGM.M);
      hasElt = !eltTy.getASTType()->isVoid();
    }

    ConsumableManagedValue eltCMV, origCMV;

    // Empty cases.  Try to avoid making an empty tuple value if it's
    // obviously going to be ignored.  This assumes that we won't even
    // try to touch the value in such cases, although we may touch the
    // cleanup (enough to see that it's not present).
    if (!hasElt) {
      bool hasNonAny = false;
      for (auto &specRow : specializedRows) {
        auto pattern = specRow.Patterns[0];
        if (pattern &&
            !isa<AnyPattern>(pattern->getSemanticsProvidingPattern())) {
          hasNonAny = true;
          break;
        }
      }

      SILValue result;
      if (hasNonAny) {
        result = SGF.emitEmptyTuple(loc);
      } else {
        result = SILUndef::get(SGF.SGM.Types.getEmptyTupleType(), SGF.SGM.M);
      }
      origCMV = ConsumableManagedValue::forUnmanaged(result);
      eltCMV = origCMV;

    // Okay, specialize on the argument.
    } else {
      auto *eltTL = &SGF.getTypeLowering(eltTy);

      // Normally we'd just use the consumption of the source
      // because the difference between TakeOnSuccess and TakeAlways
      // doesn't matter for irrefutable rows.  But if we need to
      // re-abstract, we'll see a lot of benefit from figuring out
      // that we can use TakeAlways here.
      auto eltConsumption = src.getFinalConsumption();
      if (caseInfo.Irrefutable &&
          eltConsumption == CastConsumptionKind::TakeOnSuccess) {
        eltConsumption = CastConsumptionKind::TakeAlways;
      }

      SILValue eltValue;
      if (addressOnlyEnum) {
        // We can only project destructively from an address-only enum, so
        // copy the value if we can't consume it.
        // TODO: Should have a more efficient way to copy payload
        // nondestructively from an enum.
        switch (eltConsumption) {
        case CastConsumptionKind::TakeAlways:
          eltValue = SGF.B.createUncheckedTakeEnumDataAddr(loc, srcValue,
                                                           elt, eltTy);
          break;
        case CastConsumptionKind::BorrowAlways:
          // If we reach this point, we know that we have a loadable
          // element type from an enum with mixed address
          // only/loadable cases. Since we had an address only type,
          // we assume that we will not have BorrowAlways since
          // address only types do not support BorrowAlways.
          llvm_unreachable("not allowed");
        case CastConsumptionKind::CopyOnSuccess: {
          auto copy = SGF.emitTemporaryAllocation(loc, srcValue->getType());
          SGF.B.createCopyAddr(loc, srcValue, copy,
                               IsNotTake, IsInitialization);
          // We can always take from the copy.
          eltConsumption = CastConsumptionKind::TakeAlways;
          eltValue = SGF.B.createUncheckedTakeEnumDataAddr(loc, copy,
                                                           elt, eltTy);
          break;
        }

        // We can't conditionally take, since UncheckedTakeEnumDataAddr
        // invalidates the enum.
        case CastConsumptionKind::TakeOnSuccess:
          llvm_unreachable("not allowed");
        }
        
        // Load a loadable data value.
        if (eltTL->isLoadable())
          eltValue = eltTL->emitLoad(SGF.B, loc, eltValue,
                                     LoadOwnershipQualifier::Take);
      } else {
        eltValue = caseBB->createPhiArgument(eltTy, ValueOwnershipKind::Owned);
      }

      origCMV = getManagedSubobject(SGF, eltValue, *eltTL, eltConsumption);
      eltCMV = origCMV;

      // If the payload is boxed, project it.

      if (elt->isIndirect() || elt->getParentEnum()->isIndirect()) {
        SILValue boxedValue = SGF.B.createProjectBox(loc, origCMV.getValue(), 0);
        eltTL = &SGF.getTypeLowering(boxedValue->getType());
        if (eltTL->isLoadable()) {
          UnenforcedAccess access;
          SILValue accessAddress =
            access.beginAccess(SGF, loc, boxedValue, SILAccessKind::Read);
          ManagedValue newLoadedBoxValue = SGF.B.createLoadBorrow(
            loc, ManagedValue::forUnmanaged(accessAddress));
          boxedValue = newLoadedBoxValue.getUnmanagedValue();
          access.endAccess(SGF);
        }

        // The boxed value may be shared, so we always have to copy it.
        eltCMV = getManagedSubobject(SGF, boxedValue, *eltTL,
                                     CastConsumptionKind::CopyOnSuccess);
      }

      // Reabstract to the substituted type, if needed.

      CanType substEltTy =
        sourceType->getTypeOfMember(SGF.SGM.M.getSwiftModule(), elt,
                                    elt->getArgumentInterfaceType())
                  ->getCanonicalType();

      AbstractionPattern origEltTy =
          (elt->getParentEnum()->isOptionalDecl()
               ? AbstractionPattern(substEltTy)
               : SGF.SGM.M.Types.getAbstractionPattern(elt));

      eltCMV = emitReabstractedSubobject(SGF, loc, eltCMV, *eltTL,
                                         origEltTy, substEltTy);
    }

    const FailureHandler *innerFailure = &outerFailure;
    FailureHandler specializedFailure = [&](SILLocation loc) {
      ArgUnforwarder unforwarder(SGF);
      unforwarder.unforwardBorrowedValues(src, origCMV);
      outerFailure(loc);
    };
    if (ArgUnforwarder::requiresUnforwarding(SGF, src))
      innerFailure = &specializedFailure;

    handleCase(eltCMV, specializedRows, *innerFailure);
    assert(!SGF.B.hasValidInsertionPoint() && "did not end block");
  });

  // Emit the default block if we needed one.
  if (SILBasicBlock *defaultBB = blocks.getDefaultBlock()) {
    SGF.B.setInsertionPoint(defaultBB);
    outerFailure(rows.back().Pattern);
  }
}

/// Perform specialized dispatch for a sequence of EnumElementPattern or an
/// OptionalSomePattern.
void PatternMatchEmission::
emitBoolDispatch(ArrayRef<RowToSpecialize> rows, ConsumableManagedValue src,
                 const SpecializationHandler &handleCase,
                 const FailureHandler &outerFailure) {

  struct CaseInfo {
    Pattern *FirstMatcher;
    bool Irrefutable = false;
    SmallVector<SpecializedRow, 2> SpecializedRows;
  };

  SILBasicBlock *curBB = SGF.B.getInsertionBB();
  auto &Context = SGF.getASTContext();

  // Collect the cases and specialized rows.
  //
  // These vectors are completely parallel, but the switch
  // instructions want only the first information, so we split them up.
  SmallVector<std::pair<SILValue, SILBasicBlock*>, 4> caseBBs;
  SmallVector<CaseInfo, 4> caseInfos;
  SILBasicBlock *defaultBB = nullptr;

  caseBBs.reserve(rows.size());
  caseInfos.reserve(rows.size());

  // Create destination blocks for all the cases.
  unsigned caseToIndex[2] = { ~0U, ~0U };
  for (auto &row : rows) {
    bool isTrue = cast<BoolPattern>(row.Pattern)->getValue();

    unsigned index = caseInfos.size();
    if (caseToIndex[isTrue] != ~0U) {
      // We already had an entry for this bool value.
      index = caseToIndex[isTrue];
    } else {
      caseToIndex[isTrue] = index;
    
      curBB = SGF.createBasicBlockAfter(curBB);
      auto *IL = SGF.B.createIntegerLiteral(PatternMatchStmt,
                                    SILType::getBuiltinIntegerType(1, Context),
                                            isTrue ? 1 : 0);
      caseBBs.push_back({SILValue(IL), curBB});
      caseInfos.resize(caseInfos.size() + 1);
      caseInfos.back().FirstMatcher = row.Pattern;
    }

    auto &info = caseInfos[index];
    info.Irrefutable = (info.Irrefutable || row.Irrefutable);
    info.SpecializedRows.resize(info.SpecializedRows.size() + 1);
    auto &specRow = info.SpecializedRows.back();
    specRow.RowIndex = row.RowIndex;

    specRow.Patterns.push_back(nullptr);
  }

  assert(caseBBs.size() == caseInfos.size());

  // Check to see if we need a default block.
  if (caseBBs.size() < 2)
    defaultBB = SGF.createBasicBlockAfter(curBB);

  // Emit the switch_value
  SILLocation loc = PatternMatchStmt;
  loc.setDebugLoc(rows[0].Pattern);
  SILValue srcValue = src.getFinalManagedValue().forward(SGF);

  // Extract the i1 from the Bool struct.
  StructDecl *BoolStruct = cast<StructDecl>(Context.getBoolDecl());
  auto Members = BoolStruct->lookupDirect(Context.Id_value_);
  assert(Members.size() == 1 &&
         "Bool should have only one property with name '_value'");
  auto Member = dyn_cast<VarDecl>(Members[0]);
  assert(Member &&"Bool should have a property with name '_value' of type Int1");
  auto *ETI = SGF.B.createStructExtract(loc, srcValue, Member);

  SGF.B.createSwitchValue(loc, SILValue(ETI), defaultBB, caseBBs);

  // Okay, now emit all the cases.
  for (unsigned i = 0, e = caseInfos.size(); i != e; ++i) {
    auto &caseInfo = caseInfos[i];
    auto &specializedRows = caseInfo.SpecializedRows;

    SILBasicBlock *caseBB = caseBBs[i].second;
    SGF.B.setInsertionPoint(caseBB);

    // We're in conditionally-executed code; enter a scope.
    Scope scope(SGF.Cleanups, CleanupLocation::get(loc));

    SILValue result
      = SILUndef::get(SGF.SGM.Types.getEmptyTupleType(), SGF.SGM.M);
    ConsumableManagedValue CMV =
      ConsumableManagedValue::forUnmanaged(result);

    handleCase(CMV, specializedRows, outerFailure);
    assert(!SGF.B.hasValidInsertionPoint() && "did not end block");
  }

  // Emit the default block if we needed one.
  if (defaultBB) {
    SGF.B.setInsertionPoint(defaultBB);
    outerFailure(rows.back().Pattern);
  }
}

/// Emit the body of a case statement at the current insertion point.
void PatternMatchEmission::emitCaseBody(CaseStmt *caseBlock) {
  SGF.emitStmt(caseBlock->getBody());

  // Implicitly break out of the pattern match statement.
  if (SGF.B.hasValidInsertionPoint()) {
    // Case blocks without trailing braces have a line location of the last
    // instruction in the case block.
    SILLocation cleanupLoc =
            RegularLocation::getAutoGeneratedLocation(caseBlock->getEndLoc());
    if (auto *braces = dyn_cast<BraceStmt>(caseBlock->getBody()))
      if (braces->getNumElements() == 1 &&
          dyn_cast_or_null<DoStmt>(braces->getElement(0).dyn_cast<Stmt *>()))
        cleanupLoc = CleanupLocation(caseBlock);
    SGF.emitBreakOutOf(cleanupLoc, PatternMatchStmt);
  }
}

void PatternMatchEmission::initSharedCaseBlockDest(CaseStmt *caseBlock,
                                                   bool hasFallthroughTo) {
  auto result = SharedCases.insert({caseBlock, {nullptr, hasFallthroughTo}});
  assert(result.second);

  auto *block = SGF.createBasicBlock();
  result.first->second.first = block;
    
  // Add args for any pattern variables
  if (caseBlock->hasBoundDecls()) {
    auto pattern = caseBlock->getCaseLabelItems()[0].getPattern();
    pattern->forEachVariable([&](VarDecl *V) {
      if (!V->hasName())
        return;

      // We don't pass address-only values in basic block arguments.
      SILType ty = SGF.getLoweredType(V->getType());
      if (ty.isAddressOnly(SGF.F.getModule()))
        return;
      block->createPhiArgument(ty, ValueOwnershipKind::Owned, V);
    });
  }
}

/// Retrieve the jump destination for a shared case block.
JumpDest PatternMatchEmission::getSharedCaseBlockDest(CaseStmt *caseBlock) {
  auto result = SharedCases.find(caseBlock);
  assert(result != SharedCases.end());

  auto *block = result->second.first;
  assert(block);

  return JumpDest(block, PatternMatchStmtDepth,
                  CleanupLocation(PatternMatchStmt));
}

void PatternMatchEmission::emitAddressOnlyAllocations() {
  for (auto &entry: SharedCases) {
    CaseStmt *caseBlock = entry.first;

    // If we have a shared case with bound decls, then the 0th pattern has the
    // order of variables that are the incoming BB arguments. Setup the VarLocs
    // to point to the incoming args and setup initialization so any args needing
    // cleanup will get that as well.
    if (caseBlock->hasBoundDecls()) {
      auto pattern = caseBlock->getCaseLabelItems()[0].getPattern();
      pattern->forEachVariable([&](VarDecl *V) {
        if (!V->hasName())
          return;

        SILType ty = SGF.getLoweredType(V->getType());
        if (ty.isNull()) {
          // If we're making the shared block on behalf of a previous case's
          // fallthrough, caseBlock's VarDecl's won't be in the SGF yet, so
          // determine phi types by using current vars of the same name.
          for (auto var : SGF.VarLocs) {
            auto varDecl = dyn_cast<VarDecl>(var.getFirst());
            if (varDecl && varDecl->hasName() && varDecl->getName() == V->getName()) {
              ty = var.getSecond().value->getType();
              if (var.getSecond().box) {
                ty = ty.getObjectType();
              }
            }
          }
        }
        if (ty.isAddressOnly(SGF.F.getModule())) {
          assert(!Temporaries[V]);
          Temporaries[V] = SGF.emitTemporaryAllocation(V, ty);
          return;
        }
      });
    }
  }

  // Now we have all of our cleanups entered, so we can record the
  // depth.
  PatternMatchStmtDepth = SGF.getCleanupsDepth();
}

void PatternMatchEmission::
emitAddressOnlyInitialization(VarDecl *dest, SILValue value) {
  auto found = Temporaries.find(dest);
  assert(found != Temporaries.end());
  SGF.B.createCopyAddr(dest, value, found->second, IsNotTake, IsInitialization);
}

/// Emit all the shared case statements.
void PatternMatchEmission::emitSharedCaseBlocks() {
  for (auto &entry: SharedCases) {
    CaseStmt *caseBlock = entry.first;
    SILBasicBlock *caseBB = entry.second.first;
    bool hasFallthroughTo = entry.second.second;
    assert(caseBB->empty());

    // If this case can only have one predecessor, then merge it into that
    // predecessor.  We rely on the SIL CFG here, because unemitted shared case
    // blocks might fallthrough into this one.
    if (!hasFallthroughTo && caseBlock->getCaseLabelItems().size() == 1) {
      SILBasicBlock *predBB = caseBB->getSinglePredecessorBlock();
      assert(predBB && "Should only have 1 predecessor because it isn't shared");
      assert(isa<BranchInst>(predBB->getTerminator()) &&
             "Should have uncond branch to shared block");
      predBB->getTerminator()->eraseFromParent();
      caseBB->eraseFromParent();

      // Emit the case body into the predecessor's block.
      SGF.B.setInsertionPoint(predBB);
      
    } else {
      // FIXME: Figure out why this is necessary.
      if (caseBB->pred_empty()) {
        SGF.eraseBasicBlock(caseBB);
        continue;
      }

      // Otherwise, move the block to after the first predecessor.
      auto predBB = *caseBB->pred_begin();
      caseBB->moveAfter(predBB);

      // Then emit the case body into the caseBB.
      SGF.B.setInsertionPoint(caseBB);
    }
    
    assert(SGF.getCleanupsDepth() == PatternMatchStmtDepth);

    // If we have a shared case with bound decls, then the 0th pattern has the
    // order of variables that are the incoming BB arguments. Setup the VarLocs
    // to point to the incoming args and setup initialization so any args needing
    // cleanup will get that as well.
    if (caseBlock->hasBoundDecls()) {
      Scope scope(SGF.Cleanups, CleanupLocation(caseBlock));
      auto pattern = caseBlock->getCaseLabelItems()[0].getPattern();
      unsigned argIndex = 0;
      pattern->forEachVariable([&](VarDecl *V) {
        if (!V->hasName())
          return;

        SILType ty = SGF.getLoweredType(V->getType());

        // Initialize mv at +1. We always pass values in at +1 for today into
        // shared blocks.
        ManagedValue mv;
        if (ty.isAddressOnly(SGF.F.getModule())) {
          // There's no basic block argument, since we don't allow basic blocks
          // to have address arguments.
          //
          // Instead, we map the variable to a temporary alloc_stack in
          // emitAddressOnlyAllocations(), and store into it at each
          // predecessor block.
          //
          // There's nothing to do here, since the value should already have
          // been initialized on entry.
          auto found = Temporaries.find(V);
          assert(found != Temporaries.end());
          mv = SGF.emitManagedRValueWithCleanup(found->second);
        } else {
          SILValue arg = caseBB->getArgument(argIndex++);
          assert(arg.getOwnershipKind() == ValueOwnershipKind::Owned ||
                 arg.getOwnershipKind() == ValueOwnershipKind::Trivial);
          mv = SGF.emitManagedRValueWithCleanup(arg);
        }

        if (V->isLet()) {
          // Just emit a let and leave the cleanup alone.
          SGF.VarLocs[V].value = mv.getValue();
        } else {
          // The pattern variables were all emitted as lets and one got passed in,
          // now we finally alloc a box for the var and forward in the chosen value.
          SGF.VarLocs.erase(V);
          auto newVar = SGF.emitInitializationForVarDecl(V, V->isLet());
          newVar->copyOrInitValueInto(SGF, V, mv, /*isInit*/ true);
          newVar->finishInitialization(SGF);
        }
      });
      emitCaseBody(caseBlock);
    } else {
      emitCaseBody(caseBlock);
    }
    
    assert(SGF.getCleanupsDepth() == PatternMatchStmtDepth);
  }
}

namespace {
  class FallthroughFinder : public ASTWalker {
    bool &Result;
  public:
    FallthroughFinder(bool &Result) : Result(Result) {}

    // We walk through statements.  If we find a fallthrough, then we got what
    // we came for.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      if (isa<FallthroughStmt>(S))
        Result = true;
      
      return { true, S };
    }

    // Expressions, patterns and decls cannot contain fallthrough statements, so
    // there is no reason to walk into them.
    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      return { false, E };
    }
    std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
      return { false, P };
    }

    bool walkToDeclPre(Decl *D) override { return false; }
    bool walkToTypeLocPre(TypeLoc &TL) override { return false; }
    bool walkToTypeReprPre(TypeRepr *T) override { return false; }
  };
} // end anonymous namespace


static bool containsFallthrough(Stmt *S) {
  bool Result = false;
  S->walk(FallthroughFinder(Result));
  return Result;
}


/// Context info used to emit FallthroughStmts.
/// Since fallthrough-able case blocks must not bind variables, they are always
/// emitted in the outermost scope of the switch.
class Lowering::PatternMatchContext {
public:
  PatternMatchEmission &Emission;
};

static void emitDiagnoseOfUnexpectedEnumCaseValue(SILGenFunction &SGF,
                                                  SILLocation loc,
                                                  ManagedValue value,
                                                  const EnumDecl *enumDecl) {
  ASTContext &ctx = SGF.getASTContext();
  auto diagnoseFailure = ctx.getDiagnoseUnexpectedEnumCaseValue(nullptr);
  if (!diagnoseFailure) {
    SGF.B.createBuiltinTrap(loc);
    return;
  }

  assert(enumDecl->isObjC());
  assert(enumDecl->hasRawType());
  assert(value.getType().isTrivial(SGF.getModule()));

  // Get the enum type as an Any.Type value.
  CanType switchedValueSwiftType = value.getType().getASTType();
  SILType metatypeType = SGF.getLoweredType(
      CanMetatypeType::get(switchedValueSwiftType,
                           MetatypeRepresentation::Thick));
  SILValue metatype = SGF.B.createMetatype(loc, metatypeType);

  // Bitcast the enum value to its raw type. (This is only safe for @objc
  // enums.)
  SILType loweredRawType = SGF.getLoweredType(enumDecl->getRawType());
  assert(loweredRawType.isTrivial(SGF.getModule()));
  assert(loweredRawType.isObject());
  auto rawValue = SGF.B.createUncheckedTrivialBitCast(loc, value,
                                                      loweredRawType);
  auto materializedRawValue = rawValue.materialize(SGF, loc);

  auto genericSig = diagnoseFailure->getGenericSignature();
  auto subs = SubstitutionMap::get(
      genericSig,
      [&](SubstitutableType *type) -> Type {
        auto genericParam = cast<GenericTypeParamType>(type);
        assert(genericParam->getDepth() == 0);
        assert(genericParam->getIndex() < 2);
        switch (genericParam->getIndex()) {
        case 0:
          return switchedValueSwiftType;

        case 1:
          return enumDecl->getRawType();

        default:
          llvm_unreachable("wrong generic signature for expected case value");
        }
      },
      LookUpConformanceInSignature(*genericSig));

  SGF.emitApplyOfLibraryIntrinsic(loc, diagnoseFailure, subs,
                                  {ManagedValue::forUnmanaged(metatype),
                                   materializedRawValue},
                                  SGFContext());
}

static void emitDiagnoseOfUnexpectedEnumCase(SILGenFunction &SGF,
                                             SILLocation loc,
                                             ManagedValue value) {
  ASTContext &ctx = SGF.getASTContext();
  auto diagnoseFailure = ctx.getDiagnoseUnexpectedEnumCase(nullptr);
  if (!diagnoseFailure) {
    SGF.B.createBuiltinTrap(loc);
    return;
  }

  // Get the switched-upon value's type.
  CanType switchedValueSwiftType = value.getType().getASTType();
  SILType metatypeType = SGF.getLoweredType(
      CanMetatypeType::get(switchedValueSwiftType,
                           MetatypeRepresentation::Thick));
  ManagedValue metatype = SGF.B.createValueMetatype(loc, metatypeType, value);

  auto diagnoseSignature = diagnoseFailure->getGenericSignature();
  auto genericArgsMap = SubstitutionMap::get(
      diagnoseSignature,
      [&](SubstitutableType *type) -> Type { return switchedValueSwiftType; },
      LookUpConformanceInSignature(*diagnoseSignature));

  SGF.emitApplyOfLibraryIntrinsic(loc, diagnoseFailure, genericArgsMap,
                                  metatype,
                                  SGFContext());
}

void SILGenFunction::emitSwitchStmt(SwitchStmt *S) {
  LLVM_DEBUG(llvm::dbgs() << "emitting switch stmt\n";
             S->print(llvm::dbgs());
             llvm::dbgs() << '\n');
  // If the subject expression is uninhabited, we're already dead.
  // Emit an unreachable in place of the switch statement.
  if (S->getSubjectExpr()->getType()->isStructurallyUninhabited()) {
    emitIgnoredExpr(S->getSubjectExpr());
    B.createUnreachable(S);
    return;
  }

  auto completionHandler = [&](PatternMatchEmission &emission,
                               ArgArray argArray,
                               ClauseRow &row) {
    auto caseBlock = row.getClientData<CaseStmt>();
    emitProfilerIncrement(caseBlock);
    
    // Certain case statements can be entered along multiple paths, either
    // because they have multiple labels or because of fallthrough.  When we
    // need multiple entrance path, we factor the paths with a shared block.
    if (!caseBlock->hasBoundDecls()) {
      // Don't emit anything yet, we emit it at the cleanup level of the switch
      // statement.
      JumpDest sharedDest = emission.getSharedCaseBlockDest(caseBlock);
      Cleanups.emitBranchAndCleanups(sharedDest, caseBlock);
    } else if (row.hasFallthroughTo() || caseBlock->getCaseLabelItems().size() > 1) {
      JumpDest sharedDest =
          emission.getSharedCaseBlockDest(caseBlock);

      // Generate the arguments from this row's pattern in the case block's
      // expected order, and keep those arguments from being cleaned up, as
      // we're passing the +1 along to the shared case block dest. (The
      // cleanups still happen, as they are threaded through here messily,
      // but the explicit retains here counteract them, and then the
      // retain/release pair gets optimized out.)
      ArrayRef<CaseLabelItem> labelItems = caseBlock->getCaseLabelItems();
      SmallVector<SILValue, 4> args;
      SmallVector<VarDecl *, 4> expectedVarOrder;
      SmallVector<VarDecl *, 4> vars;
      labelItems[0].getPattern()->collectVariables(expectedVarOrder);
      row.getCasePattern()->collectVariables(vars);

      SILModule &M = F.getModule();
      for (auto expected : expectedVarOrder) {
        if (!expected->hasName())
          continue;
        for (auto *var : vars) {
          if (var->hasName() && var->getName() == expected->getName()) {
            SILValue value = VarLocs[var].value;
            SILType type = value->getType();

            // If we have an address-only type, initialize the temporary
            // allocation. We're not going to pass the address as a block
            // argument.
            if (type.isAddressOnly(M)) {
              emission.emitAddressOnlyInitialization(expected, value);
              break;
            }

            // If we have a loadable address, perform a load [copy].
            if (type.isAddress()) {
              value = B.emitLoadValueOperation(CurrentSILLoc, value,
                                               LoadOwnershipQualifier::Copy);
              args.push_back(value);
              break;
            }

            value = B.emitCopyValueOperation(CurrentSILLoc, value);
            args.push_back(value);
            break;
          }
        }
      }

      Cleanups.emitBranchAndCleanups(sharedDest, caseBlock, args);
    } else {
      // However, if we don't have a fallthrough or a multi-pattern 'case', we
      // can just emit the body inline and save some dead blocks.
      // Emit the statement here.
      emission.emitCaseBody(caseBlock);
    }
  };

  PatternMatchEmission emission(*this, S, completionHandler);

  // Add a row for each label of each case.
  SmallVector<ClauseRow, 8> clauseRows;
  clauseRows.reserve(S->getRawCases().size());
  bool hasFallthrough = false;
  for (auto caseBlock : S->getCases()) {
    if (!caseBlock->hasBoundDecls() ||
        caseBlock->getCaseLabelItems().size() > 1 ||
        hasFallthrough) {
      emission.initSharedCaseBlockDest(caseBlock, hasFallthrough);
    }

    for (auto &labelItem : caseBlock->getCaseLabelItems()) {
      clauseRows.emplace_back(caseBlock,
                              const_cast<Pattern*>(labelItem.getPattern()),
                              const_cast<Expr*>(labelItem.getGuardExpr()),
                              hasFallthrough);
    }
    
    hasFallthrough = containsFallthrough(caseBlock->getBody());
  }

  // Emit alloc_stacks for address-only variables appearing in
  // multiple-entry case blocks.
  emission.emitAddressOnlyAllocations();

  SILBasicBlock *contBB = createBasicBlock();
  emitProfilerIncrement(S);
  JumpDest contDest(contBB, Cleanups.getCleanupsDepth(), CleanupLocation(S));

  Scope switchScope(Cleanups, CleanupLocation(S));

  // Enter a break/continue scope.  If we wanted a continue
  // destination, it would probably be out here.
  BreakContinueDestStack.push_back({S, contDest, JumpDest(S)});

  PatternMatchContext switchContext = { emission };
  SwitchStack.push_back(&switchContext);

  // Emit the subject value. Dispatching will consume it.
  ManagedValue subjectMV = emitRValueAsSingleValue(S->getSubjectExpr());
  auto subject = ConsumableManagedValue::forOwned(subjectMV);

  auto failure = [&](SILLocation location) {
    // If we fail to match anything, we trap. This can happen with a switch
    // over an @objc enum, which may contain any value of its underlying type,
    // or a switch over a non-frozen Swift enum when the user hasn't written a
    // catch-all case.
    SWIFT_DEFER { B.createUnreachable(location); };

    // Special case: if it's a single @objc enum, we can print the raw value.
    CanType ty = S->getSubjectExpr()->getType()->getCanonicalType();
    if (auto *singleEnumDecl = ty->getEnumOrBoundGenericEnum()) {
      if (singleEnumDecl->isObjC()) {
        emitDiagnoseOfUnexpectedEnumCaseValue(*this, location,
                                              subject.getFinalManagedValue(),
                                              singleEnumDecl);
        return;
      }
    }
    emitDiagnoseOfUnexpectedEnumCase(*this, location,
                                     subject.getFinalManagedValue());
  };

  // Set up an initial clause matrix.
  ClauseMatrix clauses(clauseRows);

  // Recursively specialize and emit the clause matrix.
  emission.emitDispatch(clauses, subject, failure);
  assert(!B.hasValidInsertionPoint());

  switchScope.pop();

  // Then emit the case blocks shared by multiple pattern cases.
  emission.emitSharedCaseBlocks();

  // Bookkeeping.
  SwitchStack.pop_back();
  BreakContinueDestStack.pop_back();

  // If the continuation block has no predecessors, this
  // point is not reachable.
  if (contBB->pred_empty()) {
    eraseBasicBlock(contBB);
  } else {
    B.emitBlock(contBB);
  }
}

void SILGenFunction::emitSwitchFallthrough(FallthroughStmt *S) {
  assert(!SwitchStack.empty() && "fallthrough outside of switch?!");
  PatternMatchContext *context = SwitchStack.back();
  
  // Get the destination block.
  CaseStmt *caseStmt = S->getFallthroughDest();
  JumpDest sharedDest =
  context->Emission.getSharedCaseBlockDest(caseStmt);

  if (!caseStmt->hasBoundDecls()) {
    Cleanups.emitBranchAndCleanups(sharedDest, S);
  } else {
    // Generate branch args to pass along current vars to fallthrough case.
    SILModule &M = F.getModule();
    ArrayRef<CaseLabelItem> labelItems = caseStmt->getCaseLabelItems();
    SmallVector<SILValue, 4> args;
    SmallVector<VarDecl *, 4> expectedVarOrder;
    labelItems[0].getPattern()->collectVariables(expectedVarOrder);
    
    for (auto *expected : expectedVarOrder) {
      if (!expected->hasName())
        continue;
      for (auto var : VarLocs) {
        auto varDecl = dyn_cast<VarDecl>(var.getFirst());
        if (varDecl && varDecl->hasName() && varDecl->getName() == expected->getName()) {
          SILValue value = var.getSecond().value;

          if (value->getType().isAddressOnly(M)) {
            context->Emission.emitAddressOnlyInitialization(expected, value);
          } else if (var.getSecond().box) {
            auto &lowering = getTypeLowering(value->getType());
            auto argValue = lowering.emitLoad(B, CurrentSILLoc, value, LoadOwnershipQualifier::Copy);
            args.push_back(argValue);
          } else {
            auto argValue = B.emitCopyValueOperation(CurrentSILLoc, value);
            args.push_back(argValue);
          }
          break;
        }
      }
    }
    Cleanups.emitBranchAndCleanups(sharedDest, S, args);
  }
}


/// Emit a sequence of catch clauses.
void SILGenFunction::emitCatchDispatch(DoCatchStmt *S, ManagedValue exn,
                                       ArrayRef<CatchStmt*> clauses,
                                       JumpDest catchFallthroughDest) {
  auto completionHandler = [&](PatternMatchEmission &emission,
                               ArgArray argArray,
                               ClauseRow &row) {
    auto clause = row.getClientData<CatchStmt>();
    emitProfilerIncrement(clause->getBody());
    emitStmt(clause->getBody());

    // If we fell out of the catch clause, branch to the fallthrough dest.
    if (B.hasValidInsertionPoint()) {
      Cleanups.emitBranchAndCleanups(catchFallthroughDest, clause->getBody());
    }
  };

  PatternMatchEmission emission(*this, S, completionHandler);

  // Add a row for each clause.
  std::vector<ClauseRow> clauseRows;
  clauseRows.reserve(clauses.size());
  for (CatchStmt *clause : clauses) {
    clauseRows.emplace_back(clause,
                            clause->getErrorPattern(),
                            clause->getGuardExpr(),
                            /*hasFallthroughTo*/false);
  }

  // Set up an initial clause matrix.
  ClauseMatrix clauseMatrix(clauseRows);
  ConsumableManagedValue subject;
  if (F.getModule().getOptions().EnableSILOwnership &&
      exn.getType().isObject()) {
    subject = {exn.borrow(*this, S), CastConsumptionKind::BorrowAlways};
  } else {
    subject = {exn, CastConsumptionKind::TakeOnSuccess};
  }

  auto failure = [&](SILLocation location) {
    // If we fail to match anything, just rethrow the exception.

    // If the throw destination is not valid, then the PatternMatchEmission
    // logic is emitting an unreachable block but didn't prune the failure BB.
    // Mark it as such.
    if (!ThrowDest.isValid()) {
      B.createUnreachable(S);
      return;
    }

    // Don't actually kill the exception's cleanup.
    CleanupStateRestorationScope scope(Cleanups);
    if (exn.hasCleanup()) {
      scope.pushCleanupState(exn.getCleanup(),
                             CleanupState::PersistentlyActive);
    }
    emitThrow(S, exn);
  };

  // Recursively specialize and emit the clause matrix.
  emission.emitDispatch(clauseMatrix, subject, failure);
  assert(!B.hasValidInsertionPoint());
}


