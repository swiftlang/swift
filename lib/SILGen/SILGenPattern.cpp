//===--- SILGenPattern.cpp - Pattern matching codegen ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patternmatch-silgen"
#include "SILGen.h"
#include "Scope.h"
#include "Cleanup.h"
#include "ExitableFullExpr.h"
#include "Initialization.h"
#include "RValue.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FormattedStream.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/SILOptions.h"
#include "swift/AST/Types.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;


/// "Specialize" a column from an array to be a span of new columns:
/// essentially, remove it and add the new columns in its place.
///

/// To allow any parallel data structures to be efficiently updated
/// in place, if there is exactly one new column, it simply replaces
/// the existing entry
template <class T>
static ArrayRef<T> specializeColumn(ArrayRef<T> input, size_t column,
                                    ArrayRef<T> newColumns,
                                    SmallVectorImpl<T> &buffer) {
  assert(column < input.size());

  // Avoid copying any data if we're replacing the last column with
  // nothing.
  if (newColumns.empty()) {
    if (column + 1 == input.size())
      return input.slice(0, input.size() - 1);
  }

  buffer.clear();

  // Any earlier columns stay in place.
  buffer.append(input.begin(), input.begin() + column);

  // If there are no new columns:
  if (newColumns.empty()) {
    // If the removed column was the last in the old array, we'd be done.
    // But we have an even faster path for this above.
    assert(input.begin() + column + 1 != input.end());

    // Otherwise, move the last old column to this position.
    buffer.push_back(input.back());
    input = input.slice(0, input.size() - 1);

  // Otherwise, put the first new column in the vacated position.
  } else {
    buffer.push_back(newColumns[0]);
    newColumns = newColumns.slice(1);
  }

  // The rest of the input columns stay in place.
  buffer.append(input.begin() + column + 1, input.end());

  // Followed by any new columns required.
  buffer.append(newColumns.begin(), newColumns.end());

  return buffer;
}

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
    os << "var " << cast<NamedPattern>(p)->getBodyName();
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
  case PatternKind::NominalType: {
    auto np = cast<NominalTypePattern>(p);
    np->getType()->print(os);
    os << '(';
    interleave(np->getElements(),
               [&](const NominalTypePattern::Element &elt) {
                 os << elt.getProperty()->getName() << ":";
               },
               [&]{ os << ", "; });
    os << ')';
    return;
  }
  case PatternKind::EnumElement: {
    auto eep = cast<EnumElementPattern>(p);
    os << '.' << eep->getName();
    return;
  }

  case PatternKind::OptionalSome:
    os << ".Some";
    return;

  case PatternKind::Bool: {
    auto bp = cast<BoolPattern>(p);
    os << bp->getName();
    return;
  }

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
  case PatternKind::NominalType:
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
  case PatternKind::NominalType: {
    auto nom = cast<NominalTypePattern>(p);
    for (auto &elt : nom->getElements())
      n = getNumSpecializationsRecursive(elt.getSubPattern(), n);
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
  case PatternKind::Bool: {
    auto bp = cast<BoolPattern>(p);
    n++;
    if (bp->hasSubPattern())
      n = getNumSpecializationsRecursive(bp->getSubPattern(), n);
    return n;
  }

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
  case PatternKind::NominalType:
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

  // If the patterns are exactly the same kind, they can be treated similarly.
  if (p->getKind() == first->getKind())
    return p;

  // If one is an OptionalSomePattern and one is an EnumElementPattern, then
  // they are the same since the OptionalSomePattern is just sugar for .Some(x).
  if ((isa<OptionalSomePattern>(p) && isa<EnumElementPattern>(first)) ||
      (isa<OptionalSomePattern>(first) && isa<EnumElementPattern>(p)))
    return p;

  // Otherwise, they are different.
  return nullptr;
}

namespace {

/// A row which we intend to specialize.
struct RowToSpecialize {
  /// The pattern from this row which we are specializing upon.
  Pattern *Pattern;

  /// The index of the target row.
  unsigned RowIndex;

  /// Whether the row will be irrefutable after this specialization.
  bool Irrefutable;
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
  
  /// PatternMatchStmt - The 'switch', 'if', or while that we're emitting this
  /// pattern match for.
  Stmt *PatternMatchStmt;
  CleanupsDepth PatternMatchStmtDepth;
  llvm::MapVector<CaseStmt*, std::pair<SILBasicBlock*, bool>> SharedCases;

  using CompletionHandlerTy =
    llvm::function_ref<void(PatternMatchEmission &, ClauseRow &)>;
  CompletionHandlerTy CompletionHandler;
public:
  
  PatternMatchEmission(SILGenFunction &SGF, Stmt *S,
                       CompletionHandlerTy completionHandler)
    : SGF(SGF), PatternMatchStmt(S),
      PatternMatchStmtDepth(SGF.getCleanupsDepth()),
      CompletionHandler(completionHandler) {}

  void emitDispatch(ClauseMatrix &matrix, ArgArray args,
                    const FailureHandler &failure);

  JumpDest getSharedCaseBlockDest(CaseStmt *caseStmt, bool hasFallthroughTo);
  void emitSharedCaseBlocks();

  void emitCaseBody(CaseStmt *caseBlock);

private:
  void emitWildcardDispatch(ClauseMatrix &matrix, ArgArray args, unsigned row,
                            const FailureHandler &failure);

  void bindRefutablePatterns(const ClauseRow &row, ArgArray args,
                             const FailureHandler &failure);
  void bindRefutablePattern(Pattern *pattern, ConsumableManagedValue v,
                            const FailureHandler &failure);
  void bindExprPattern(ExprPattern *pattern, ConsumableManagedValue v,
                       const FailureHandler &failure);
  void emitGuardBranch(SILLocation loc, Expr *guard,
                       const FailureHandler &failure);

  void bindIrrefutablePatterns(const ClauseRow &row, ArgArray args,
                               bool forIrrefutableRow);
  void bindIrrefutablePattern(Pattern *pattern, ConsumableManagedValue v,
                              bool forIrrefutableRow);
  void bindNamedPattern(NamedPattern *pattern, ConsumableManagedValue v,
                        bool forIrrefutableRow);

  void bindVariable(SILLocation loc, VarDecl *var,
                    ConsumableManagedValue value, CanType formalValueType,
                    bool isForSuccess);

  void emitSpecializedDispatch(ClauseMatrix &matrix, ArgArray args,
                               unsigned &lastRow, unsigned column,
                               const FailureHandler &failure);
  void emitTupleDispatch(ArrayRef<RowToSpecialize> rows,
                         ConsumableManagedValue src,
                         const SpecializationHandler &handleSpec,
                         const FailureHandler &failure);
  void emitNominalTypeDispatch(ArrayRef<RowToSpecialize> rows,
                               ConsumableManagedValue src,
                               const SpecializationHandler &handleSpec,
                               const FailureHandler &failure);
  void emitIsDispatch(ArrayRef<RowToSpecialize> rows,
                      ConsumableManagedValue src,
                      const SpecializationHandler &handleSpec,
                      const FailureHandler &failure);
  void emitEnumElementDispatch(ArrayRef<RowToSpecialize> rows,
                               ConsumableManagedValue src,
                               const SpecializationHandler &handleSpec,
                               const FailureHandler &failure);
  void emitBoolDispatch(ArrayRef<RowToSpecialize> rows,
                        ConsumableManagedValue src,
                        const SpecializationHandler &handleSpec,
                        const FailureHandler &failure);
};

/// A handle to a row in a clause matrix. Does not own memory; use of the
/// ClauseRow must be dominated by its originating ClauseMatrix.
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

  /// Remove a column.
  void removeColumn(unsigned index) {
    Columns.erase(Columns.begin() + index);
  }

  /// Add new columns to the end of the row.
  void addColumns(ArrayRef<Pattern *> columns) {
    Columns.append(columns.begin(), columns.end());
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
        dumpPattern(row[r], ss);
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
static
ConsumableManagedValue forwardIntoSubtree(CleanupStateRestorationScope &scope,
                                          ConsumableManagedValue outerCMV) {
  ManagedValue outerMV = outerCMV.getFinalManagedValue();
  if (!outerMV.hasCleanup()) return outerCMV;

  assert(outerCMV.getFinalConsumption() != CastConsumptionKind::CopyOnSuccess
         && "copy-on-success value with cleanup?");
  scope.pushCleanupState(outerMV.getCleanup(),
                         CleanupState::PersistentlyActive);

  // Success means that we won't end up in the other branch,
  // but failure doesn't.
  return { outerMV, CastConsumptionKind::TakeOnSuccess };
}

/// Forward a value down into an irrefutable branch of the decision tree.
///
/// Essentially equivalent to forwardIntoSubtree, except it preserves
/// AlwaysTake consumption.
static void forwardIntoIrrefutableSubtree(CleanupStateRestorationScope &scope,
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
  CleanupStateRestorationScope Scope;
protected:
  ArgForwarderBase(SILGenFunction &SGF)
    : Scope(SGF.Cleanups) {}

  ConsumableManagedValue forward(ConsumableManagedValue value) {
    return forwardIntoSubtree(Scope, value);
  }

  void forwardIntoIrrefutable(ConsumableManagedValue value) {
    return forwardIntoIrrefutableSubtree(Scope, value);
  }
};

/// A RAII-ish object for forwarding a bunch of arguments down to one
/// side of a branch.
class ArgForwarder : private ArgForwarderBase {
  ArgArray OuterArgs;
  SmallVector<ConsumableManagedValue, 4> ForwardedArgsBuffer;

public:
  ArgForwarder(SILGenFunction &SGF, ArgArray outerArgs, bool isFinalUse)
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
        ForwardedArgsBuffer.push_back(forward(outerArg));
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
                          unsigned column, ArgArray newArgs,
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
      ForwardedArgsBuffer.push_back(forward(outerArgs[i]));

    // The specialized column.
    if (!newArgs.empty()) {
      ForwardedArgsBuffer.push_back(newArgs[0]);
      newArgs = newArgs.slice(1);
    } else if (column + 1 < outerArgs.size()) {
      ForwardedArgsBuffer.push_back(forward(outerArgs.back()));
      outerArgs = outerArgs.slice(0, outerArgs.size() - 1);
    }

    // The rest of the outer columns.
    for (unsigned i = column + 1, e = outerArgs.size(); i != e; ++i)
      ForwardedArgsBuffer.push_back(forward(outerArgs[i]));

    // The rest of the new args.
    ForwardedArgsBuffer.append(newArgs.begin(), newArgs.end());
  }

  /// Returns the forward arguments.  The new rows are placed using
  /// the column-specialization algorithm.
  ArgArray getForwardedArgs() const {
    return ForwardedArgsBuffer;
  }

private:
  ConsumableManagedValue forward(ConsumableManagedValue value) {
    if (IsFinalUse) {
      ArgForwarderBase::forwardIntoIrrefutable(value);
      return value;
    } else {
      return ArgForwarderBase::forward(value);
    }
  }
};

/// A RAII-ish object for undoing the forwarding of cleanups along a
/// failure path.
class ArgUnforwarder {
  CleanupStateRestorationScope Scope;
public:
  ArgUnforwarder(SILGenFunction &SGF) : Scope(SGF.Cleanups) {}

  static bool requiresUnforwarding(ConsumableManagedValue operand) {
    return (operand.hasCleanup() &&
            operand.getFinalConsumption()
              == CastConsumptionKind::TakeOnSuccess);
  }

  /// Given that an aggregate was divided into a set of borrowed
  /// values which are now being tracked individually, temporarily
  /// disable all of the borrowed-value cleanups and restore the
  /// aggregate cleanup.
  void unforwardBorrowedValues(ConsumableManagedValue aggregate,
                               ArgArray subobjects) {
    if (!requiresUnforwarding(aggregate)) return;
    Scope.pushCleanupState(aggregate.getCleanup(), CleanupState::Active);
    for (auto &subobject : subobjects) {
      if (subobject.hasCleanup())
        Scope.pushCleanupState(subobject.getCleanup(), CleanupState::Dormant);
    }
  }
};

}

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
  ArgForwarder forwarder(SGF, matrixArgs, row + 1 == clauses.rows());
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

  // Bind the rest of the patterns.
  bindIrrefutablePatterns(clauses[row], args, !hasGuard);

  // Emit the guard branch, if it exists.
  if (guardExpr) {
    emitGuardBranch(guardExpr, guardExpr, failure);
  }

  // Enter the row.
  CompletionHandler(*this, clauses[row]);
  assert(!SGF.B.hasValidInsertionPoint());
}

/// Bind all the irrefutable patterns in the given row, which is
/// nothing but wildcard patterns.
void PatternMatchEmission::
bindRefutablePatterns(const ClauseRow &row, ArgArray args,
                      const FailureHandler &failure) {
  assert(row.columns() == args.size());
  for (unsigned i = 0, e = args.size(); i != e; ++i) {
    bindRefutablePattern(row[i], args[i], failure);
  }
}

/// Bind a refutable wildcard pattern to a given value.
void PatternMatchEmission::bindRefutablePattern(Pattern *pattern,
                                                ConsumableManagedValue value,
                                                const FailureHandler &failure) {
  // We use null patterns to mean artificial AnyPatterns.
  if (!pattern) return;

  pattern = pattern->getSemanticsProvidingPattern();
  switch (pattern->getKind()) {
  // Non-wildcard patterns.
  case PatternKind::Tuple:
  case PatternKind::NominalType:
  case PatternKind::EnumElement:
  case PatternKind::OptionalSome:
  case PatternKind::Bool:
  case PatternKind::Is:
    llvm_unreachable("didn't specialize specializable pattern?");

  // Non-semantic patterns.
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    llvm_unreachable("should have skipped non-semantic pattern");

  // Refutable patterns that we'll handle in a later pass.
  case PatternKind::Any:
  case PatternKind::Named:
    return;

  case PatternKind::Expr:
    bindExprPattern(cast<ExprPattern>(pattern), value, failure);
    return;
  }
  llvm_unreachable("bad pattern kind");
}

/// Check whether an expression pattern is satisfied.
void PatternMatchEmission::bindExprPattern(ExprPattern *pattern,
                                           ConsumableManagedValue value,
                                           const FailureHandler &failure) {
  FullExpr scope(SGF.Cleanups, CleanupLocation(pattern));
  bindVariable(pattern, pattern->getMatchVar(), value,
               pattern->getType()->getCanonicalType(),
               /*isForSuccess*/ false);
  emitGuardBranch(pattern, pattern->getMatchExpr(), failure);
}

/// Bind all the irrefutable patterns in the given row, which is nothing
/// but wildcard patterns.
///
/// Note that forIrrefutableRow can be true even if !row.isIrrefutable()
/// because we might have already bound all the refutable parts.
void PatternMatchEmission::bindIrrefutablePatterns(const ClauseRow &row,
                                                   ArgArray args,
                                                   bool forIrrefutableRow) {
  assert(row.columns() == args.size());
  for (unsigned i = 0, e = args.size(); i != e; ++i) {
    bindIrrefutablePattern(row[i], args[i], forIrrefutableRow);
  }
}

/// Bind an irrefutable wildcard pattern to a given value.
void PatternMatchEmission::bindIrrefutablePattern(Pattern *pattern,
                                                  ConsumableManagedValue value,
                                                  bool forIrrefutableRow) {
  // We use null patterns to mean artifical AnyPatterns.
  if (!pattern) return;

  pattern = pattern->getSemanticsProvidingPattern();
  switch (pattern->getKind()) {
  // Non-wildcard patterns.
  case PatternKind::Tuple:
  case PatternKind::NominalType:
  case PatternKind::EnumElement:
  case PatternKind::OptionalSome:
  case PatternKind::Bool:
  case PatternKind::Is:
    llvm_unreachable("didn't specialize specializable pattern?");

  // Non-semantic patterns.
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    llvm_unreachable("should have skipped non-semantic pattern");

  // We can just drop Any values.
  case PatternKind::Any:
    return;

  // Ignore expression patterns, which we should have bound in an
  // earlier pass.
  case PatternKind::Expr:
    return;

  case PatternKind::Named:
    bindNamedPattern(cast<NamedPattern>(pattern), value, forIrrefutableRow);
    return;
  }
  llvm_unreachable("bad pattern kind");
}

/// Bind a named pattern to a given value.
void PatternMatchEmission::bindNamedPattern(NamedPattern *pattern,
                                            ConsumableManagedValue value,
                                            bool forIrrefutableRow) {
  bindVariable(pattern, pattern->getDecl(), value,
               pattern->getType()->getCanonicalType(), forIrrefutableRow);
}

/// Should we take control of the mang
static bool shouldTake(ConsumableManagedValue value, bool isIrrefutable) {
  switch (value.getFinalConsumption()) {
  case CastConsumptionKind::TakeAlways: return true;
  case CastConsumptionKind::TakeOnSuccess: return isIrrefutable;
  case CastConsumptionKind::CopyOnSuccess: return false;
  }
  llvm_unreachable("bad consumption kind");
}

/// Bind a variable into the current scope.
void PatternMatchEmission::bindVariable(SILLocation loc, VarDecl *var,
                                        ConsumableManagedValue value,
                                        CanType formalValueType,
                                        bool isIrrefutable) {
  // Initialize the variable value.
  InitializationPtr init = SGF.emitInitializationForVarDecl(var, Type());

  RValue rv(SGF, loc, formalValueType, value.getFinalManagedValue());
  if (shouldTake(value, isIrrefutable)) {
    std::move(rv).forwardInto(SGF, init.get(), loc);
  } else {
    std::move(rv).copyInto(SGF, init.get(), loc);
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
  unsigned firstRow = lastRow;

  // Collect the rows to specialize.
  SmallVector<RowToSpecialize, 4> rowsToSpecialize;
  auto addRowToSpecialize = [&](Pattern *pattern, unsigned rowIndex) {
    assert(getSpecializingPattern(clauses[rowIndex][column]) == pattern);
    bool irrefutable = clauses[rowIndex].isIrrefutableAfterSpecializing(column);
    rowsToSpecialize.push_back({pattern, rowIndex, irrefutable});
  };

  Pattern *firstSpecializer = getSpecializingPattern(clauses[firstRow][column]);
  assert(firstSpecializer && "specializing unspecializable row?");
  addRowToSpecialize(firstSpecializer, firstRow);

  // Take a prefix of rows that share the same semantic kind of pattern.
  for (++lastRow; lastRow != clauses.rows(); ++lastRow) {
    Pattern *specializer =
      getSimilarSpecializingPattern(clauses[lastRow][column], firstSpecializer);
    if (!specializer) break;
    addRowToSpecialize(specializer, lastRow);
  }
  assert(lastRow - firstRow == rowsToSpecialize.size());

  // Forward just the specialized argument right now.  We'll forward
  // the rest in the handler.
  bool isFinalUse = (lastRow == clauses.rows());
  ArgForwarder outerForwarder(SGF, matrixArgs[column], isFinalUse);
  auto arg = outerForwarder.getForwardedArgs()[0];

  SpecializationHandler handler = [&](ArrayRef<ConsumableManagedValue> newArgs,
                                      ArrayRef<SpecializedRow> rows,
                                      const FailureHandler &innerFailure) {
    // These two operations must follow the same rules for column
    // placement because 'arguments' are parallel to the matrix colums.
    // We use the column-specialization algorithm described in
    // specializeInPlace.
    ClauseMatrix innerClauses = clauses.specializeRowsInPlace(column, rows);

    SpecializedArgForwarder innerForwarder(SGF, matrixArgs, column, newArgs,
                                           isFinalUse);
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
  case PatternKind::NominalType:
    return emitNominalTypeDispatch(rowsToSpecialize, arg, handler, failure);
  case PatternKind::EnumElement:
  case PatternKind::OptionalSome:
    return emitEnumElementDispatch(rowsToSpecialize, arg, handler, failure);
  case PatternKind::Bool:
    return emitBoolDispatch(rowsToSpecialize, arg, handler, failure);
  }
  llvm_unreachable("bad pattern kind");
};

/// Given that we've broken down a source value into this subobject,
/// and that we were supposed to use the given consumption rules on
/// it, construct an appropriate managed value.
static ConsumableManagedValue
getManagedSubobject(SILGenFunction &gen, SILValue value,
                    const TypeLowering &valueTL,
                    CastConsumptionKind consumption) {
  if (consumption != CastConsumptionKind::CopyOnSuccess) {
    return { gen.emitManagedRValueWithCleanup(value, valueTL),
             consumption };
  } else {
    return ConsumableManagedValue::forUnmanaged(value);
  }
}

static ConsumableManagedValue
emitReabstractedSubobject(SILGenFunction &gen, SILLocation loc,
                          ConsumableManagedValue value,
                          const TypeLowering &valueTL,
                          AbstractionPattern abstraction,
                          CanType substFormalType) {
  // Return if there's no abstraction.  (The first condition is just
  // a fast path.)
  if (value.getType().getSwiftRValueType() == substFormalType ||
      value.getType() == gen.getLoweredType(substFormalType))
    return value;

  // Otherwise, turn to +1 and re-abstract.
  ManagedValue mv = gen.getManagedValue(loc, value);
  return ConsumableManagedValue::forOwned(
           gen.emitOrigToSubstValue(loc, mv, abstraction, substFormalType));
}

/// Perform specialized dispatch for tuples.
///
/// This is simple; all the tuples have the same structure.
void PatternMatchEmission::
emitTupleDispatch(ArrayRef<RowToSpecialize> rows, ConsumableManagedValue src,
                  const SpecializationHandler &handleCase,
                  const FailureHandler &outerFailure) {
  auto firstPat = rows[0].Pattern;
  auto sourceType = cast<TupleType>(firstPat->getType()->getCanonicalType());
  SILLocation loc = firstPat;

  SILValue v = src.getFinalManagedValue().forward(SGF);
  SmallVector<ConsumableManagedValue, 4> destructured;

  // Break down the values.
  auto tupleSILTy = v.getType();
  for (unsigned i = 0, e = sourceType->getNumElements(); i < e; ++i) {
    SILType fieldTy = tupleSILTy.getTupleElementType(i);
    auto &fieldTL = SGF.getTypeLowering(fieldTy);

    SILValue member;
    if (tupleSILTy.isAddress()) {
      member = SGF.B.createTupleElementAddr(loc, v, i, fieldTy);
      if (!fieldTL.isAddressOnly())
        member = SGF.B.createLoad(loc, member);
    } else {
      member = SGF.B.createTupleExtract(loc, v, i, fieldTy);
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
  if (ArgUnforwarder::requiresUnforwarding(src))
    innerFailure = &specializedFailure;

  // Recurse.
  handleCase(destructured, specializedRows, *innerFailure);
}

/// Perform specialized dispatch for a sequence of NominalTypePatterns.
void PatternMatchEmission::
emitNominalTypeDispatch(ArrayRef<RowToSpecialize> rows,
                        ConsumableManagedValue src,
                        const SpecializationHandler &handleCase,
                        const FailureHandler &outerFailure) {
  // First, collect all the properties we'll need to match on.
  // Also remember the first pattern which matched that property.
  llvm::SmallVector<std::pair<VarDecl*, Pattern*>, 4> properties;
  llvm::DenseMap<VarDecl*, unsigned> propertyIndexes;
  for (auto &row : rows) {
    for (auto &elt : cast<NominalTypePattern>(row.Pattern)->getElements()) {
      VarDecl *property = elt.getProperty();

      // Try to insert the property in the map at the next available
      // index.  If the entry already exists, it won't change.
      auto result = propertyIndexes.insert({property, properties.size()});
      if (result.second) {
        properties.push_back({property,
                              const_cast<Pattern*>(elt.getSubPattern())});
      }
    }
  }

  // Get values for all the properties.
  SmallVector<ConsumableManagedValue, 4> destructured;
  for (auto &entry : properties) {
    VarDecl *property = entry.first;
    Pattern *firstMatcher = entry.second;

    // FIXME: does this properly handle getters at all?
    ManagedValue aggMV = src.asUnmanagedValue();

    SILLocation loc = firstMatcher;

    // TODO: project stored properties directly
    auto val = SGF.emitRValueForPropertyLoad(loc, aggMV, false,
                                             property,
                                             // FIXME: No generic substitions.
                                             {}, AccessSemantics::Ordinary,
                                             firstMatcher->getType(),
                                             // TODO: Avoid copies on
                                             // address-only types.
                                             SGFContext());
    destructured.push_back(ConsumableManagedValue::forOwned(val));
  }

  // Construct the specialized rows.
  SmallVector<SpecializedRow, 4> specializedRows;
  specializedRows.resize(rows.size());
  for (unsigned i = 0, e = rows.size(); i != e; ++i) {
    specializedRows[i].RowIndex = rows[i].RowIndex;
    specializedRows[i].Patterns.resize(destructured.size(), nullptr);

    auto pattern = cast<NominalTypePattern>(rows[i].Pattern);
    for (auto &elt : pattern->getElements()) {
      auto propertyIndex = propertyIndexes.find(elt.getProperty())->second;
      assert(!specializedRows[i].Patterns[propertyIndex]);
      specializedRows[i].Patterns[propertyIndex] =
        const_cast<Pattern*>(elt.getSubPattern());
    }
  }

  // Maybe revert to the original cleanups during failure branches.
  const FailureHandler *innerFailure = &outerFailure;
  FailureHandler specializedFailure = [&](SILLocation loc) {
    ArgUnforwarder unforwarder(SGF);
    unforwarder.unforwardBorrowedValues(src, destructured);
    outerFailure(loc);
  };
  if (ArgUnforwarder::requiresUnforwarding(src))
    innerFailure = &specializedFailure;

  // Recurse.
  handleCase(destructured, specializedRows, *innerFailure);
}

static CanType getTargetType(const RowToSpecialize &row) {
  auto type = cast<IsPattern>(row.Pattern)->getCastTypeLoc().getType();
  return type->getCanonicalType();
}

static ConsumableManagedValue
emitSerialCastOperand(SILGenFunction &SGF, SILLocation loc,
                      ConsumableManagedValue src, CanType sourceType,
                      ArrayRef<RowToSpecialize> rows,
                      SmallVectorImpl<ConsumableManagedValue> &borrowedValues) {
  // Reabstract to the most general abstraction, and put it into a
  // temporary if necessary.

  // Figure out if we need the value to be in a temporary.
  bool requiresAddress = false;
  for (auto &row : rows) {
    CanType targetType = getTargetType(row);
    if (!canUseScalarCheckedCastInstructions(SGF.SGM.M, sourceType,
                                             targetType)) {
      requiresAddress = true;
      break;
    }
  }

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
      SGF.B.createStore(loc, src.getFinalManagedValue().forward(SGF),
                        init->getAddress());
      init->finishInitialization(SGF);
      ConsumableManagedValue result =
        { init->getManagedAddress(), src.getFinalConsumption() };
      if (ArgUnforwarder::requiresUnforwarding(src))
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
  // Collect the types to which we're going to cast.
  CanType sourceType = rows[0].Pattern->getType()->getCanonicalType();

  // Make any abstraction modifications necessary for casting a bunch
  // of times.
  SmallVector<ConsumableManagedValue, 4> borrowedValues;
  ConsumableManagedValue operand =
    emitSerialCastOperand(SGF, rows[0].Pattern, src, sourceType, rows,
                          borrowedValues);

  // Emit all of the 'is' checks.
  for (unsigned specBegin = 0, numRows = rows.size(); specBegin != numRows; ) {
    CanType targetType = getTargetType(rows[specBegin]);

    // Find all the immediately following rows that are checking for
    // exactly the same type.
    unsigned specEnd = specBegin + 1;
    for (; specEnd != numRows; ++specEnd) {
      if (getTargetType(rows[specEnd]) != targetType)
        break;
    }

    // Build the specialized-rows array.
    SmallVector<SpecializedRow, 4> specializedRows;
    specializedRows.resize(specEnd - specBegin);
    for (unsigned i = specBegin; i != specEnd; ++i) {
      auto &specRow = specializedRows[i - specBegin];
      auto is = cast<IsPattern>(rows[i].Pattern);
      specRow.RowIndex = rows[i].RowIndex;
      specRow.Patterns.push_back(is->getSubPattern());
    }

    SILLocation loc = rows[specBegin].Pattern;
    auto cleanupLoc = CleanupLocation::getCleanupLocation(loc);

    CleanupStateRestorationScope forwardingScope(SGF.Cleanups);
    ConsumableManagedValue castOperand = operand.asBorrowedOperand();
    
    // Enter an exitable scope.  On failure, we'll just go on to the next case.
    ExitableFullExpr scope(SGF, cleanupLoc);
    FailureHandler innerFailure = [&](SILLocation loc) {
      // The cleanup for the cast operand was pushed outside of this
      // jump dest, so we don't have to undo any cleanup-splitting here.
      SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc);
    };

    // Perform a conditional cast branch.
    SGF.emitCheckedCastBranch(loc, castOperand,
                              sourceType, targetType, SGFContext(),
      // Success block: recurse.
      [&](ManagedValue castValue) {
        handleCase(ConsumableManagedValue::forOwned(castValue),
                   specializedRows, innerFailure);
        assert(!SGF.B.hasValidInsertionPoint() && "did not end block");
      },
      // Failure block: branch out to the continuation block.
      [&] { innerFailure(loc); });

    // Dispatch continues on the "false" block.
    scope.exit();

    // Continue where we left off.
    specBegin = specEnd;
  }

  ArgUnforwarder unforwarder(SGF);
  if (ArgUnforwarder::requiresUnforwarding(src)) {
    unforwarder.unforwardBorrowedValues(src, borrowedValues);
  }

  failure(rows.back().Pattern);
}

/// Perform specialized dispatch for a sequence of EnumElementPattern or an
/// OptionalSomePattern.
void PatternMatchEmission::
emitEnumElementDispatch(ArrayRef<RowToSpecialize> rows,
                        ConsumableManagedValue src,
                        const SpecializationHandler &handleCase,
                        const FailureHandler &outerFailure) {

  CanType sourceType = rows[0].Pattern->getType()->getCanonicalType();

  struct CaseInfo {
    Pattern *FirstMatcher;
    bool Irrefutable = false;
    SmallVector<SpecializedRow, 2> SpecializedRows;
  };

  SILBasicBlock *curBB = SGF.B.getInsertionBB();

  // Collect the cases and specialized rows.
  //
  // These vectors are completely parallel, but the switch
  // instructions want only the first information, so we split them up.
  SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 4> caseBBs;
  SmallVector<CaseInfo, 4> caseInfos;
  SILBasicBlock *defaultBB = nullptr;

  caseBBs.reserve(rows.size());
  caseInfos.reserve(rows.size());

  {
    // Create destination blocks for all the cases.
    llvm::DenseMap<EnumElementDecl*, unsigned> caseToIndex;
    for (auto &row : rows) {    
      EnumElementDecl *elt;
      Pattern *subPattern = nullptr;
      if (auto eep = dyn_cast<EnumElementPattern>(row.Pattern)) {
        elt = eep->getElementDecl();
        subPattern = eep->getSubPattern();
      } else {
        auto *osp = cast<OptionalSomePattern>(row.Pattern);
        elt = osp->getElementDecl();
        subPattern = osp->getSubPattern();
      }

      unsigned index = caseInfos.size();
      auto insertionResult = caseToIndex.insert({elt, index});
      if (!insertionResult.second) {
        index = insertionResult.first->second;
      } else {
        curBB = SGF.createBasicBlock(curBB);
        caseBBs.push_back({elt, curBB});
        caseInfos.resize(caseInfos.size() + 1);
        caseInfos.back().FirstMatcher = row.Pattern;
      }
      assert(caseToIndex[elt] == index);
      assert(caseBBs[index].first == elt);

      auto &info = caseInfos[index];
      info.Irrefutable = (info.Irrefutable || row.Irrefutable);
      info.SpecializedRows.resize(info.SpecializedRows.size() + 1);
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

    // Check to see if we need a default block.
    // FIXME: If the enum is resilient, then we always need a default block.
    for (auto elt : sourceType.getEnumOrBoundGenericEnum()->getAllElements()) {
      if (!caseToIndex.count(elt)) {
        defaultBB = SGF.createBasicBlock(curBB);
        break;
      }
    }
  }

  assert(caseBBs.size() == caseInfos.size());

  // Emit the switch_enum{_addr} instruction.
  bool addressOnlyEnum = src.getType().isAddress();
  SILValue srcValue = src.getFinalManagedValue().forward(SGF);
  SILLocation loc = PatternMatchStmt;
  loc.setDebugLoc(rows[0].Pattern);
  if (addressOnlyEnum) {
    SGF.B.createSwitchEnumAddr(loc, srcValue, defaultBB, caseBBs);
  } else {
    SGF.B.createSwitchEnum(loc, srcValue, defaultBB, caseBBs);
  }

  // Okay, now emit all the cases.
  for (unsigned i = 0, e = caseInfos.size(); i != e; ++i) {
    auto &caseInfo = caseInfos[i];
    SILLocation loc = caseInfo.FirstMatcher;
    auto &specializedRows = caseInfo.SpecializedRows;

    EnumElementDecl *elt = caseBBs[i].first;
    SILBasicBlock *caseBB = caseBBs[i].second;
    SGF.B.setInsertionPoint(caseBB);

    // We're in conditionally-executed code; enter a scope.
    Scope scope(SGF.Cleanups, CleanupLocation::getCleanupLocation(loc));
      
    // Create a BB argument or 'unchecked_take_enum_data_addr'
    // instruction to receive the enum case data if it has any.

    SILType eltTy;
    bool hasElt = false;
    if (elt->hasArgumentType()) {
      eltTy = src.getType().getEnumElementType(elt, SGF.SGM.M);
      hasElt = !eltTy.getSwiftRValueType()->isVoid();
    }

    ConsumableManagedValue eltCMV;
    ConsumableManagedValue origCMV;

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
      auto &eltTL = SGF.getTypeLowering(eltTy);

      SILValue eltValue;
      if (addressOnlyEnum) {
        // FIXME: this is not okay to do if we're not consuming.
        eltValue = SGF.B.createUncheckedTakeEnumDataAddr(loc, srcValue,
                                                         elt, eltTy);
        // Load a loadable data value.
        if (eltTL.isLoadable())
          eltValue = SGF.B.createLoad(loc, eltValue);
      } else {
        eltValue = new (SGF.F.getModule()) SILArgument(caseBB, eltTy);
      }

      // Normally we'd just use the consumption of the source
      // because the difference between TakeOnSuccess and TakeAlways
      // doesn't matter for irrefutable rows.  But if we need to
      // re-abstract, we'll see a lot of benefit from figuring out
      // that we can use TakeAlways here.
      auto eltConsumption = src.getFinalConsumption();
      if (caseInfo.Irrefutable &&
          eltConsumption == CastConsumptionKind::TakeOnSuccess)
        eltConsumption = CastConsumptionKind::TakeAlways;

      origCMV = getManagedSubobject(SGF, eltValue, eltTL, eltConsumption);

      // Reabstract to the substituted type, if needed.
      CanType substEltTy =
        sourceType->getTypeOfMember(SGF.SGM.M.getSwiftModule(),
                                    elt, nullptr,
                                    elt->getArgumentInterfaceType())
                  ->getCanonicalType();

      eltCMV = emitReabstractedSubobject(SGF, loc, origCMV, eltTL,
                                  AbstractionPattern(elt->getArgumentType()),
                                         substEltTy);
    }

    const FailureHandler *innerFailure = &outerFailure;
    FailureHandler specializedFailure = [&](SILLocation loc) {
      ArgUnforwarder unforwarder(SGF);
      unforwarder.unforwardBorrowedValues(src, origCMV);
      outerFailure(loc);
    };
    if (ArgUnforwarder::requiresUnforwarding(src))
      innerFailure = &specializedFailure;

    handleCase(eltCMV, specializedRows, *innerFailure);
    assert(!SGF.B.hasValidInsertionPoint() && "did not end block");
  }

  // Emit the default block if we needed one.
  if (defaultBB) {
    SGF.B.setInsertionPoint(defaultBB);
    outerFailure(rows.back().Pattern);
  }
}

/// Perform specialized dispatch for a sequence of EnumElementPattern or an
/// OptionalSomePattern.
void PatternMatchEmission::
emitBoolDispatch(ArrayRef<RowToSpecialize> rows,
                        ConsumableManagedValue src,
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

  // Define the exhaustive set of Bool values.
  SmallVector<BooleanLiteralExpr *, 2> BoolValues;
  BoolValues.push_back(new (Context)
                           BooleanLiteralExpr(false, SourceLoc(), true));
  BoolValues.push_back(new (Context)
                           BooleanLiteralExpr(true, SourceLoc(), true));

  caseBBs.reserve(rows.size());
  caseInfos.reserve(rows.size());

  // Create destination blocks for all the cases.
  llvm::DenseMap<Expr*, unsigned> caseToIndex;
  llvm::DenseMap<BooleanLiteralExpr*, SILValue> elt2SILValue;
  for (auto &row : rows) {
    BooleanLiteralExpr *elt;
    Pattern *subPattern = nullptr;
    SILValue eltSILValue;
    auto bp = dyn_cast<BoolPattern>(row.Pattern);
    assert(bp && "It should be a bool pattern");

    elt = dyn_cast<BooleanLiteralExpr>(bp->getBoolValue());
    elt = elt->getValue() ? BoolValues[1] : BoolValues[0];
    assert(elt && "Case expression should be a boolean literal");
    subPattern = bp->getSubPattern();

    unsigned index = caseInfos.size();
    auto insertionResult = caseToIndex.insert({elt, index});
    if (!insertionResult.second) {
      index = insertionResult.first->second;
    } else {
      curBB = SGF.createBasicBlock(curBB);
      auto *IL = SGF.B.createIntegerLiteral(
          PatternMatchStmt,
          SILType::getBuiltinIntegerType(1, Context),
          APInt(1, elt->getValue()? 1 : 0, 1));
      eltSILValue = SILValue(IL, 0);
      elt2SILValue.insert({elt, eltSILValue});
      caseBBs.push_back({eltSILValue, curBB});
      caseInfos.resize(caseInfos.size() + 1);
      caseInfos.back().FirstMatcher = row.Pattern;
    }
    assert(caseToIndex[elt] == index);
    assert(caseBBs[index].first == elt2SILValue.lookup(elt));

    auto &info = caseInfos[index];
    info.Irrefutable = (info.Irrefutable || row.Irrefutable);
    info.SpecializedRows.resize(info.SpecializedRows.size() + 1);
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

  // Check to see if we need a default block.
  for (auto val : BoolValues) {
    if (!caseToIndex.count(val)) {
      defaultBB = SGF.createBasicBlock(curBB);
      break;
    }
  }

  assert(caseBBs.size() == caseInfos.size());

  // Emit the switch_value
  SILLocation loc = PatternMatchStmt;
  loc.setDebugLoc(rows[0].Pattern);
  SILValue srcValue = src.getFinalManagedValue().forward(SGF);

  // Extract the i1 from the Bool struct.
  StructDecl *BoolStruct = dyn_cast<StructDecl>(Context.getBoolDecl());
  auto Members = BoolStruct->lookupDirect(Context.getIdentifier("value"));
  assert(Members.size() == 1 &&
         "Bool should have only one property with name 'value'");
  auto Member = dyn_cast<VarDecl>(Members[0]);
  assert(Member &&
         "Bool should have a property with name 'value' of type Int1");
  auto *ETI = SGF.B.createStructExtract(loc, srcValue, Member);

  SGF.B.createSwitchValue(loc, SILValue(ETI, 0), defaultBB, caseBBs);

  // Okay, now emit all the cases.
  for (unsigned i = 0, e = caseInfos.size(); i != e; ++i) {
    auto &caseInfo = caseInfos[i];
    SILLocation loc = caseInfo.FirstMatcher;
    auto &specializedRows = caseInfo.SpecializedRows;

    SILBasicBlock *caseBB = caseBBs[i].second;
    SGF.B.setInsertionPoint(caseBB);

    // We're in conditionally-executed code; enter a scope.
    Scope scope(SGF.Cleanups, CleanupLocation::getCleanupLocation(loc));

    SILType eltTy;
    bool hasElt = false;

    ConsumableManagedValue eltCMV;
    ConsumableManagedValue origCMV;

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

    }

    const FailureHandler *innerFailure = &outerFailure;
    FailureHandler specializedFailure = [&](SILLocation loc) {
      ArgUnforwarder unforwarder(SGF);
      unforwarder.unforwardBorrowedValues(src, origCMV);
      outerFailure(loc);
    };
    if (ArgUnforwarder::requiresUnforwarding(src))
      innerFailure = &specializedFailure;

    handleCase(eltCMV, specializedRows, *innerFailure);
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
    SGF.emitBreakOutOf(CleanupLocation(caseBlock), PatternMatchStmt);
  }
}

/// Retrieve the jump destination for a shared case block.
JumpDest PatternMatchEmission::getSharedCaseBlockDest(CaseStmt *caseBlock,
                                                      bool hasFallthroughTo) {
  assert(!caseBlock->hasBoundDecls() &&
         "getting shared case destination for block with bound vars?");

  auto result = SharedCases.insert({caseBlock, {nullptr, hasFallthroughTo}});

  // If there's already an entry, use that.
  SILBasicBlock *block;
  if (!result.second) {
    block = result.first->second.first;
    assert(block);
  } else {
    // Create the shared destination at the first place that might
    // have needed it.
    block = SGF.createBasicBlock();
    result.first->second.first = block;
  }

  return JumpDest(block, PatternMatchStmtDepth,
                  CleanupLocation(PatternMatchStmt));
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
      SILBasicBlock *predBB = caseBB->getSinglePredecessor();
      assert(predBB && "Should only have 1 predecesor because it isn't shared");
      assert(isa<BranchInst>(predBB->getTerminator()) &&
             "Should have uncond branch to shared block");
      predBB->getTerminator()->eraseFromParent();
      caseBB->eraseFromParent();

      // Emit the case body into the predecessor's block.
      SGF.B.setInsertionPoint(predBB);
      
    } else {
      // Otherwise, move the block to after the first predecessor.
      assert(!caseBB->pred_empty() && "Emitted an unused shared block?");
      auto predBB = *caseBB->pred_begin();
      caseBB->moveAfter(predBB);

      // Then emit the case body into the caseBB.
      SGF.B.setInsertionPoint(caseBB);
    }
    
    assert(SGF.getCleanupsDepth() == PatternMatchStmtDepth);
    emitCaseBody(caseBlock);
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
    bool walkToTypeReprPre(TypeRepr *T) override { return false; }
  };
}


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

void SILGenFunction::emitSwitchStmt(SwitchStmt *S) {
  DEBUG(llvm::dbgs() << "emitting switch stmt\n";
        S->print(llvm::dbgs());
        llvm::dbgs() << '\n');
  SILBasicBlock *contBB = createBasicBlock();
  emitProfilerIncrement(S);
  JumpDest contDest(contBB, Cleanups.getCleanupsDepth(), CleanupLocation(S));

 
  auto completionHandler = [&](PatternMatchEmission &emission,
                               ClauseRow &row) {
    auto caseBlock = row.getClientData<CaseStmt>();
    emitProfilerIncrement(caseBlock);
    
    // Certain case statements can be entered along multiple paths, either
    // because they have multiple labels or because of fallthrough.  When we
    // need multiple entrance path, we factor the paths with a shared block.
    if (!caseBlock->hasBoundDecls()) {
      // Don't emit anything yet, we emit it at the cleanup level of the switch
      // statement.
      JumpDest sharedDest = emission.getSharedCaseBlockDest(caseBlock,
                                                        row.hasFallthroughTo());
      Cleanups.emitBranchAndCleanups(sharedDest, caseBlock);
    } else {
      // However, if we don't have a fallthrough or a multi-pattern 'case', we
      // can just emit the body inline and save some dead blocks.
      // Emit the statement here.
      emission.emitCaseBody(caseBlock);
      
      // If we don't need a shared block and we have
    }
  };

  PatternMatchEmission emission(*this, S, completionHandler);
  
  Scope switchScope(Cleanups, CleanupLocation(S));

  // Enter a break/continue scope.  If we wanted a continue
  // destination, it would probably be out here.
  BreakContinueDestStack.push_back({S, contDest, JumpDest(S)});

  PatternMatchContext switchContext = { emission };
  SwitchStack.push_back(&switchContext);

  // Emit the subject value. Dispatching will consume it.
  ManagedValue subjectMV = emitRValueAsSingleValue(S->getSubjectExpr());
  auto subject = ConsumableManagedValue::forOwned(subjectMV);

  // Add a row for each label of each case.
  // We use std::vector because it supports emplace_back; moving
  // a ClauseRow is expensive.
  std::vector<ClauseRow> clauseRows;
  clauseRows.reserve(S->getCases().size());
  bool hasFallthrough = false;
  for (auto caseBlock : S->getCases()) {
    for (auto &labelItem : caseBlock->getCaseLabelItems()) {
      clauseRows.emplace_back(caseBlock,
                              const_cast<Pattern*>(labelItem.getPattern()),
                              const_cast<Expr*>(labelItem.getGuardExpr()),
                              hasFallthrough);
    }
    
    hasFallthrough = containsFallthrough(caseBlock->getBody());
  }

  // Set up an initial clause matrix.
  ClauseMatrix clauses(clauseRows);

  auto failure = [&](SILLocation location) {
    // If we fail to match anything, we can just emit unreachable.
    // This will be a dataflow error if we can reach here.
    B.createUnreachable(S);
  };

  // Recursively specialize and emit the clause matrix.
  emission.emitDispatch(clauses, subject, failure);
  assert(!B.hasValidInsertionPoint());

  switchScope.pop();

  // Emit any shared case blocks we generated.
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
    context->Emission.getSharedCaseBlockDest(caseStmt, true);
  Cleanups.emitBranchAndCleanups(sharedDest, S);
}


/// Recursively emit the pieces of a condition in an if/while stmt.
static void
emitStmtConditionWithBodyRec(Stmt *CondStmt, unsigned CondElement,
                             JumpDest SuccessDest, JumpDest CondFailDest,
                             SILGenFunction &gen) {
  Stmt *Body;
  StmtCondition Condition;
  
  if (auto *If = dyn_cast<IfStmt>(CondStmt)) {
    Body = If->getThenStmt();
    Condition = If->getCond();
  } else {
    auto *While = cast<WhileStmt>(CondStmt);
    Body = While->getBody();
    Condition = While->getCond();
  }
  
  // In the base case, we have already emitted all of the pieces of the
  // condition.  There is nothing to do except to finally emit the Body, which
  // will be in the scope of any emitted patterns.
  if (CondElement == Condition.size()) {
    gen.emitProfilerIncrement(Body);
    gen.emitStmt(Body);
    
    // Finish the "true part" by cleaning up any temporaries and jumping to the
    // continuation block.
    if (gen.B.hasValidInsertionPoint()) {
      RegularLocation L(Body);
      L.pointToEnd();
      gen.Cleanups.emitBranchAndCleanups(SuccessDest, L);
    }
    return;
  }

  // In the recursive case, we emit a condition guard, which is either a boolean
  // expression or a refutable pattern binding. Handle boolean conditions first.
  if (auto *expr = Condition[CondElement].getCondition()) {
    // Evaluate the condition as an i1 value (guaranteed by Sema).
    SILValue V;
    {
      FullExpr Scope(gen.Cleanups, CleanupLocation(expr));
      V = gen.emitRValue(expr).forwardAsSingleValue(gen, expr);
    }
    assert(V.getType().castTo<BuiltinIntegerType>()->isFixedWidth(1) &&
           "Sema forces conditions to have Builtin.i1 type");
    
    // Just branch on the condition.  On failure, we unwind any active cleanups,
    // on success we fall through to a new block.
    SILBasicBlock *ContBB = gen.createBasicBlock();
    SILBasicBlock *FailBB = CondFailDest.getBlock();
    
    // If earlier parts of the condition have already emitted cleanups, then
    // we need to run them on the exit from this boolean condition, and will
    // need a block to emit the cleanups into.  Otherwise, we can get away with
    // a direct jump and avoid creating a pointless block.
    if (gen.Cleanups.hasAnyActiveCleanups(CondFailDest.getDepth()))
      FailBB = gen.createBasicBlock();
    
    gen.B.createCondBranch(expr, V, ContBB, FailBB);

    // Emit cleanups on the failure path if needed.
    if (FailBB != CondFailDest.getBlock()) {
      gen.B.emitBlock(FailBB);
      gen.Cleanups.emitBranchAndCleanups(CondFailDest, CondStmt);
    }

    // Finally, emit the continue block and keep emitting the rest of the
    // condition.
    gen.B.emitBlock(ContBB);
    return emitStmtConditionWithBodyRec(CondStmt, CondElement+1,
                                        SuccessDest, CondFailDest, gen);
  }
  
  // Otherwise, we have a pattern initialized by an optional.  Emit the optional
  // expression and test its presence.
  auto *PBD = Condition[CondElement].getBinding();

  // The handler that generates code when the match succeeds.  This
  // simply continues emission of the rest of the condition.
  auto completionHandler = [&](PatternMatchEmission &emission, ClauseRow &row) {
    emitStmtConditionWithBodyRec(CondStmt, CondElement+1,
                                 SuccessDest, CondFailDest, gen);
  };
  
  PatternMatchEmission emission(gen, CondStmt, completionHandler);
  
  assert(PBD->getNumPatternEntries() == 1 &&
         "statement conditionals only have a single entry right now");
  auto *ThePattern = PBD->getPatternList()[0].ThePattern;
  auto *Init = PBD->getPatternList()[0].Init;
  
  
  // Emit the initializer value being matched against. Dispatching will consume
  // it.
  ManagedValue subjectMV = gen.emitRValueAsSingleValue(Init);
  auto subject = ConsumableManagedValue::forOwned(subjectMV);
  
  // Add a row for the pattern we want to match against.
  ClauseRow row(/*caseBlock*/nullptr, ThePattern, /*where expr*/nullptr,
                /*hasFallthroughTo*/false);
  
  SILBasicBlock *MatchFailureBB = 0;
  
  // In the match failure case, we wind back out of the cleanup blocks.
  auto matchFailure = [&](SILLocation location) {
    MatchFailureBB = gen.B.getInsertionBB();
    gen.Cleanups.emitBranchAndCleanups(CondFailDest, CondStmt);
  };
  
  // Finally, emit the pattern binding, and recursively emit the rest of the
  // condition and the body of the statement.
  ClauseMatrix clauses(row);
  emission.emitDispatch(clauses, subject, matchFailure);
  assert(!gen.B.hasValidInsertionPoint());
  
  
  // Our match failure case often ends up with a switch_enum (or whatever) to
  // the match failure block, which is then just a jump to another block.  Check
  // to see if the match failure block is trivial, and if so, clean it up.
  if (MatchFailureBB && MatchFailureBB->getNumBBArg() == 0)
    if (auto *BI = dyn_cast<BranchInst>(&MatchFailureBB->getInstList().front()))
      if (auto *SinglePred = MatchFailureBB->getSinglePredecessor()) {
        if (BI->getDestBB()->getNumBBArg() == 0) {
          for (SILSuccessor &elt : SinglePred->getSuccessors()) {
            if (elt == MatchFailureBB)
              elt = BI->getDestBB();
          }

          // Remove the branch to placate eraseBasicBlock()'s
          // assertion that the block being removed is empty.
          BI->eraseFromParent();
          gen.eraseBasicBlock(MatchFailureBB);
        }
      }
}

/// Emit the code to evaluate a general StmtCondition, and then a "Body" guarded
/// by the condition that executes when the condition is true.  This ensures
/// that bound variables are in scope when the body runs, but are cleaned up
/// after it is done.
///
/// This can produce a number of basic blocks:
///   1) the insertion point is the block in which all of the predicates
///      evalute to true and any patterns match and have their buffers
///      initialized, and the Body has been emitted.  All bound variables are in
///      scope for the body, and are cleaned up before the insertion point is
///      done.
///   2) the returned list of blocks indicates the destruction order for any
///      contained pattern bindings.  Jumping to the first block in the list
///      will release all bound values.  The last block in the list will
///      continue execution after the condition fails and is fully cleaned up.
///
void SILGenFunction::emitStmtConditionWithBody(Stmt *S,SILBasicBlock *SuccessBB,
                                               SILBasicBlock *FailBB) {
  assert(B.hasValidInsertionPoint() &&
         "emitting condition at unreachable point");
  
  // Enter a scope for pattern variables.
  Scope trueScope(Cleanups, S);
  
  JumpDest SuccessDest(SuccessBB, getCleanupsDepth(), CleanupLocation(S));
  JumpDest FailDest(FailBB, getCleanupsDepth(), CleanupLocation(S));
  emitStmtConditionWithBodyRec(S, 0, SuccessDest, FailDest, *this);
}

/// Emit a sequence of catch clauses.
void SILGenFunction::emitCatchDispatch(Stmt *S, ManagedValue exn,
                                       ArrayRef<CatchStmt*> clauses,
                                       JumpDest catchFallthroughDest) {
  auto completionHandler = [&](PatternMatchEmission &emission,
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

  ConsumableManagedValue subject = { exn, CastConsumptionKind::TakeOnSuccess };
  auto failure = [&](SILLocation location) {
    // If we fail to match anything, just rethrow the exception.
    if (ThrowDest.isValid()) {
      // Don't actually kill the exception's cleanup.
      CleanupStateRestorationScope scope(Cleanups);
      if (exn.hasCleanup()) {
        scope.pushCleanupState(exn.getCleanup(),
                               CleanupState::PersistentlyActive);
      }
      emitThrow(S, exn);
      return;
    }

    SGM.diagnose(S, diag::nonexhaustive_catch);
    B.createUnreachable(location);
  };

  // Recursively specialize and emit the clause matrix.
  emission.emitDispatch(clauseMatrix, subject, failure);
  assert(!B.hasValidInsertionPoint());
}


