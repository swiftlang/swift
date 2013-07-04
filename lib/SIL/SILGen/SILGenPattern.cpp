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

#include "SILGen.h"
#include "Initialization.h"
#include "RValue.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/FormattedStream.h"
#include "swift/AST/Diagnostics.h"

using namespace swift;
using namespace Lowering;

namespace {
  
static bool isWildcardPattern(const Pattern *p);
  
/// An abstract representation of a constructor that can be reversed by a
/// pattern match. Patterns with equivalent destructuring behavior (such as
/// '(_, _, _)' and '(a, b, c)') map to equivalent PatternConstructor values.
class PatternConstructor {
public:
  enum class Kind : uint8_t {
    /// This pattern is a wildcard and doesn't match a constructor.
    Wildcard,
    /// This pattern destructures a tuple. Tuple constructors are irrefutable
    /// and always form a signature.
    Tuple,
  };

private:
  Kind kind;
  union {
    struct {
      unsigned arity;
    } tuple;
  };

  /// Constructor used by DenseMapInfo.
  explicit PatternConstructor(Kind kind) : kind(kind) {}
  friend struct llvm::DenseMapInfo<PatternConstructor>;

  explicit PatternConstructor(unsigned tupleArity)
    : kind(Kind::Tuple), tuple{tupleArity}
  {}
public:
  PatternConstructor() : kind(Kind::Wildcard) {}
  
  static PatternConstructor forWildcard() {
    return PatternConstructor();
  }

  static PatternConstructor forTuple(unsigned arity) {
    return PatternConstructor(arity);
  }

  /// Get the PatternConstructor value for a pattern.
  static PatternConstructor forPattern(const Pattern *p) {
    // We use null Pattern* pointers to represent destructured wildcards.
    if (!p)
      return PatternConstructor::forWildcard();
    
    switch (p->getKind()) {
    // Wildcard patterns don't destructure.
    case PatternKind::Any:
    case PatternKind::Expr:
      return PatternConstructor::forWildcard();
    // TODO These are wildcards, but we don't implement variable binding yet.
    case PatternKind::Named:
      llvm_unreachable("not implemented");
      // return PatternConstructor::forWildcard();
    
    case PatternKind::Tuple:
      return PatternConstructor::forTuple(
                        p->getType()->getAs<TupleType>()->getFields().size());
    // TODO
    case PatternKind::NominalType:
    case PatternKind::Isa:
      llvm_unreachable("not implemented");
    
    // Recur into simple wrapping patterns.
    case PatternKind::Paren:
      return forPattern(cast<ParenPattern>(p)->getSubPattern());
    case PatternKind::Typed:
      return forPattern(cast<TypedPattern>(p)->getSubPattern());
    case PatternKind::Var:
      return forPattern(cast<VarPattern>(p)->getSubPattern());
    }
  }

  Kind getKind() const { return kind; }

  unsigned getTupleArity() const {
    assert(kind == Kind::Tuple);
    return tuple.arity;
  }
  
  /// Emit a conditional branch testing if a value matches this constructor.
  /// On the true branch, destructure the value. On return the insertion point
  /// is inside the true branch. The false branch is returned.
  ///
  /// If the constructor is irrefutable, this performs an unconditional branch
  /// and returns null.
  SILBasicBlock *emitBranchAndDestructure(SILGenFunction &gen, SILValue v,
                                      SmallVectorImpl<SILValue> &destructured) {
    switch (getKind()) {
    case Kind::Wildcard:
      llvm_unreachable("wildcards shouldn't get here");

    case Kind::Tuple: {
      // Tuples are irrefutable; destructure without branching.
      auto tupleTy = v.getType().getAs<TupleType>();
      if (v.getType().isAddressOnly(gen.F.getModule())) {
        for (unsigned i = 0, e = getTupleArity(); i < e; ++i) {
          auto &field = tupleTy->getFields()[i];
          SILType fieldTy = gen.getLoweredType(field.getType());
          SILValue member = gen.B.createTupleElementAddr(SILLocation(),
                                                         v, i,
                                                         fieldTy);
          if (!fieldTy.isAddressOnly(gen.F.getModule()))
            member = gen.B.createLoad(SILLocation(), member);
          destructured.push_back(member);
        }
      } else {
        for (unsigned i = 0, e = getTupleArity(); i < e; ++i) {
          auto &field = tupleTy->getFields()[i];
          SILType fieldTy = gen.getLoweredLoadableType(field.getType());
          SILValue member = gen.B.createTupleExtract(SILLocation(),
                                                     v, i,
                                                     fieldTy);
          destructured.push_back(member);
        }
      }
      return nullptr;
    }
    }
  }
  
  /// Destructure a pattern that has this pattern constructor.
  /// Destructured wildcards are represented with null Pattern* pointers.
  void destructurePattern(const Pattern *p,
                          SmallVectorImpl<const Pattern *> &destructured) {
    switch (getKind()) {
    case Kind::Wildcard:
      llvm_unreachable("wildcards shouldn't get here");

    case Kind::Tuple: {
      // Explode a wildcard into component wildcards.
      if (isWildcardPattern(p)) {
        for (unsigned i = 0; i < getTupleArity(); ++i)
          destructured.push_back(nullptr);
        return;
      }
      // If it's not a wildcard, it must be a tuple pattern. Destructure into
      // the tuple fields.
      auto tp = cast<TuplePattern>(p);
      std::transform(tp->getFields().begin(), tp->getFields().end(),
                     std::back_inserter(destructured),
                     [&](const TuplePatternElt &e) -> const Pattern * {
                       return e.getPattern();
                     });
      return;
    }
    }
  }

  /// True if the pattern matches a real constructor and isn't a wildcard.
  explicit operator bool() const { return kind != Kind::Wildcard; }

  bool operator==(PatternConstructor c) {
    if (getKind() != c.getKind())
      return false;
    // Placeholder kind values used for DenseMap placeholders.
    if (getKind() == Kind(-1) || getKind() == Kind(-2))
      return true;
    switch (getKind()) {
    case Kind::Wildcard:
      return true;
    case Kind::Tuple:
      return getTupleArity() == c.getTupleArity();
    }
  }
  
  bool operator!=(PatternConstructor c) {
    return !(*this == c);
  }  
};

/// True if the pattern is a wildcard.
static bool isWildcardPattern(const Pattern *p) {
  return PatternConstructor::forPattern(p).getKind()
    == PatternConstructor::Kind::Wildcard;
}

/// A pair of an ExprPattern node and the SILValue it will test.
struct ExprGuard {
  const ExprPattern *pattern;
  SILValue value;
  
  bool operator==(ExprGuard o) const {
    return pattern == o.pattern && value == o.value;
  }
  
  bool operator!=(ExprGuard o) const {
    return pattern != o.pattern || value != o.value;
  }
};

/// A handle to a row in a clause matrix. Does not own memory; use of the
/// ClauseRow must be dominated by its originating ClauseMatrix.
class ClauseRow {
  friend class ClauseMatrix;
  
  /// The in-memory layout of a clause row prefix.
  struct Prefix {
    /// The location information for the row.
    SILLocation loc;
    /// The destination BB for if the pattern row matches.
    SILBasicBlock *dest;
    /// The guard expression for the row, or null if there is no guard.
    Expr *guardExpr;
    /// The ExprPatterns within the pattern and their matching values.
    unsigned firstExprGuard, lastExprGuard;
  };
  
  Prefix *row;
  unsigned columnCount;
  ArrayRef<ExprGuard> exprGuards;
  
  // ClauseRows should only be vended by ClauseMatrix::operator[].
  ClauseRow(Prefix *row, unsigned columnCount, ArrayRef<ExprGuard> guards)
    : row(row), columnCount(columnCount), exprGuards(guards)
  {}
  
public:
  ClauseRow() = default;
  
  SILLocation getLoc() const {
    return row->loc;
  }
  SILBasicBlock *getDest() const {
    return row->dest;
  }
  Expr *getGuard() const {
    return row->guardExpr;
  }
  bool hasGuard() const {
    return row->guardExpr;
  }
  
  ArrayRef<ExprGuard> getExprGuards() const {
    return exprGuards;
  }
  
  /// Emit dispatch to the row's destination. If the row has no guard, the
  /// branch is unconditional, and this terminates the current block and
  /// returns true. Otherwise, it dispatches conditionally on the guard, leaves
  /// the insertion branch on the not-taken path, and returns false.
  bool emitDispatch(SILGenFunction &gen) const {
    // If there is no guard, branch unconditionally.
    if (!hasGuard() && exprGuards.empty()) {
      gen.B.createBranch(getLoc(), getDest());
      return true;
    }

    // Create a new BB for the guard-failed case.
    SILBasicBlock *falseBB = new (gen.F.getModule()) SILBasicBlock(&gen.F);
    
    if (!exprGuards.empty()) {
      // If we have a guard, we'll jump to it if all the expr patterns check
      // out. Otherwise we can jump directly to the destination.
      SILBasicBlock *guardBB = getDest();
      if (hasGuard())
        guardBB = new (gen.F.getModule()) SILBasicBlock(&gen.F);
      // Test ExprPatterns from the row in an "and" chain.
      for (unsigned i = 0, e = exprGuards.size(); i < e; ++i) {
        ExprGuard eg = exprGuards[i];
        
        // The last pattern test jumps to the guard.
        SILBasicBlock *nextBB = guardBB;
        if (i < e - 1)
          nextBB = new (gen.F.getModule()) SILBasicBlock(&gen.F);
        
        SILValue testBool;
        {
          FullExpr scope(gen.Cleanups);

          // TODO: We should emit every guard once and share code if it covers
          // multiple patterns.
          // TODO: It'd be nice not to need the temp var here.
          
          // Allocate the temporary match variable.
          auto init = gen.emitLocalVariableWithCleanup(eg.pattern->getMatchVar());
          init->getAddress();
          // Copy the tested value into the variable.
          ManagedValue(eg.value, ManagedValue::Unmanaged).copyInto(gen,
                                                             init->getAddress());
          init->finishInitialization(gen);
          
          // Emit the match test.
          testBool = gen.visit(eg.pattern->getMatchExpr())
            .getUnmanagedSingleValue(gen);
          
        }
        // We don't need the variable after this.
        gen.VarLocs.erase(eg.pattern->getMatchVar());

        // If the test succeeds, we move on to the next test; otherwise, we
        // fail and move to the next pattern.
        gen.B.createCondBranch(getLoc(), testBool, nextBB, falseBB);
        if (i < e - 1)
          gen.B.emitBlock(nextBB);
      }
      
      if (hasGuard())
        gen.B.emitBlock(guardBB);
    }
    
    if (hasGuard()) {
      SILValue guardBool;
      {
        FullExpr scope(gen.Cleanups);
          
        // Emit the guard.
        // TODO: We should emit every guard once and share code if it covers
        // multiple patterns.
        guardBool = gen.visit(getGuard()).getUnmanagedSingleValue(gen);
      }
      
      // Branch either to the row destination or the new BB.
      gen.B.createCondBranch(getLoc(), guardBool, getDest(), falseBB);
    }
    
    // Continue codegen on the false branch.
    gen.B.emitBlock(falseBB);
    return false;
  }

  ArrayRef<const Pattern *> getColumns() const {
    return {reinterpret_cast<const Pattern * const*>(row + 1), columnCount};
  }
  MutableArrayRef<const Pattern *> getColumns() {
    return {reinterpret_cast<const Pattern **>(row + 1), columnCount};
  }
  
  const Pattern *operator[](unsigned column) const {
    return getColumns()[column];
  }
  const Pattern *&operator[](unsigned column) {
    return getColumns()[column];
  }
  unsigned columns() const {
    return columnCount;
  }
};

/// Get a pattern as an ExprPattern, unwrapping semantically transparent
/// pattern nodes.
const ExprPattern *getAsExprPattern(const Pattern *p) {
  if (!p) return nullptr;

  switch (p->getKind()) {
  case PatternKind::Expr:
    return cast<ExprPattern>(p);
  
  case PatternKind::Tuple:
  case PatternKind::Named:
  case PatternKind::Any:
  case PatternKind::Isa:
  case PatternKind::NominalType:
    return nullptr;

  // Recur into simple wrapping patterns.
  case PatternKind::Paren:
    return getAsExprPattern(cast<ParenPattern>(p)->getSubPattern());
  case PatternKind::Typed:
    return getAsExprPattern(cast<TypedPattern>(p)->getSubPattern());
  case PatternKind::Var:
    return getAsExprPattern(cast<VarPattern>(p)->getSubPattern());
  }
}

/// A clause matrix. This matrix associates subpattern rows to their
/// corresponding guard expressions, and associates destination basic block
/// and columns to their associated subject value.
class ClauseMatrix {
  /// The capacity of the memory buffer in rows/columns.
  unsigned rowCapacity, columnCapacity;
  /// The inhabited part of the memory buffer.
  unsigned rowCount, columnCount;
  /// The memory buffer containing the matrix data.
  void *data;
  /// A list of ExprPatterns and the associated SILValue they test. Rows
  /// reference into slices of this vector to indicate which ExprPatterns form
  /// part of their guard.
  std::vector<ExprGuard> exprGuards;
  
  // The memory layout of data is as follows:
  
  // - columnCapacity SILValues, for the occurrence vector. The first
  //   columnCount entries are populated.
  size_t getOccurrenceVectorStride() const {
    return columnCapacity * sizeof(SILValue);
  }
  MutableArrayRef<SILValue> getMutableOccurrences() {
    return {reinterpret_cast<SILValue*>(data), columnCount};
  }
  
  // - rowCapacity rows, each of which contains:
  //   - a ClauseRow::Prefix, followed by
  //   - columnCapacity Pattern*s, for the clause patterns.
  
  size_t getRowStride() const {
    return sizeof(ClauseRow::Prefix) + columnCapacity * sizeof(Pattern*);
  }
  ClauseRow::Prefix *getMutableRowPrefix(unsigned i) {
    assert(i < rowCount && "row access out of bounds");
    char *firstRow = reinterpret_cast<char*>(data) + getOccurrenceVectorStride();
    return reinterpret_cast<ClauseRow::Prefix*>(firstRow + getRowStride() * i);
  }
  
  size_t getDataSize() const {
    return getOccurrenceVectorStride() + getRowStride() * rowCapacity;
  }
  
  ClauseMatrix(const ClauseMatrix &) = delete;
  ClauseMatrix &operator=(const ClauseMatrix &) = delete;
public:
  /// Allocate a clause matrix over the given set of occurrences
  /// (actively matched values) and enough initial capacity for the
  /// given number of rows. The clause matrix will be initialized with zero rows
  /// and a column for every occurrence. Rows can be added using addRows.
  explicit ClauseMatrix(ArrayRef<SILValue> occurrences,
                        unsigned rowCapacity)
    : rowCapacity(rowCapacity), columnCapacity(occurrences.size()),
      rowCount(0), columnCount(columnCapacity)
  {
    // Allocate the data buffer.
    data = malloc(getDataSize());
    assert(data);
    
    // Initialize the occurrence vector.
    MutableArrayRef<SILValue> occurrenceBuf = getMutableOccurrences();
    for (unsigned i = 0, e = occurrences.size(); i < e; ++i) {
      occurrenceBuf[i] = occurrences[i];
    }
  }
  
  ~ClauseMatrix() {
    free(data);
  }
  
  ClauseMatrix(ClauseMatrix &&m)
    : rowCapacity(m.rowCapacity), columnCapacity(m.columnCapacity),
      rowCount(m.rowCount), columnCount(m.columnCount),
      data(m.data)
  {
    m.data = nullptr;
  }

  unsigned rows() const { return rowCount; }
  unsigned columns() const { return columnCount; }

  /// Append a row to the matrix.
  void addRow(SILLocation loc, SILBasicBlock *dest, Expr *guardExpr,
              ArrayRef<const Pattern*> cols,
              ArrayRef<ExprGuard> parentExprGuards = {}) {
    assert(cols.size() == columnCount && "new row has wrong number of columns");

    // Grow storage if necessary.
    if (rowCount == rowCapacity) {
      rowCapacity *= 2;
      data = realloc(data, getDataSize());
      assert(data);
    }
    
    // Collect guards introduced by ExprPatterns in this row.
    unsigned exprGuardStart = exprGuards.size();
    exprGuards.insert(exprGuards.end(),
                      parentExprGuards.begin(), parentExprGuards.end());
    for (unsigned i = 0, e = cols.size(); i < e; ++i) {
      auto *ep = getAsExprPattern(cols[i]);
      if (!ep)
        continue;
      ExprGuard eg{ep, getOccurrences()[i]};
      if (std::find(parentExprGuards.begin(), parentExprGuards.end(), eg)
           == parentExprGuards.end())
        exprGuards.push_back(eg);
    }
    unsigned exprGuardEnd = exprGuards.size();
    
    // Initialize the next row.
    ClauseRow::Prefix *row = getMutableRowPrefix(rowCount++);
    ::new (row) ClauseRow::Prefix{loc, dest, guardExpr,
                                  exprGuardStart, exprGuardEnd};
    MutableArrayRef<const Pattern*> columnsBuf{
      reinterpret_cast<const Pattern**>(row+1),
      columnCount
    };
    
    for (unsigned i = 0; i < columnCount; ++i)
      columnsBuf[i] = cols[i];
  }
  
  /// Remove a row from the matrix.
  void removeRow(unsigned r) {
    assert(r < rowCount && "removeRow out of bounds");
    memmove(getMutableRowPrefix(r), getMutableRowPrefix(r+1),
            getRowStride() * (rowCount - (r+1)));
    --rowCount;
  }
  
  ArrayRef<SILValue> getOccurrences() const {
    return const_cast<ClauseMatrix*>(this)->getMutableOccurrences();
  }
  
  ClauseRow operator[](unsigned row) {
    auto *prefix = getMutableRowPrefix(row);
    return {getMutableRowPrefix(row), columnCount,
            {&exprGuards[prefix->firstExprGuard],
             prefix->lastExprGuard - prefix->firstExprGuard}};
  }
  const ClauseRow operator[](unsigned row) const {
    return const_cast<ClauseMatrix&>(*this)[row];
  }
  
  /// Specialize this matrix's first column on a constructor, and emit a branch
  /// conditional on the constructor matching the current value. Given an n-ary
  /// constructor form c(x1...xn), this performs the following row-wise
  /// transformation to create a new matrix:
  ///
  /// If the row is [ c(x1...xn)   p1 p2 ... -> Dest ],
  /// expand to     [ x1 x2 ... xn p1 p2 ... -> Dest ].
  ///
  /// If the row is [ _            p1 p2 ... -> Dest ],
  /// expand to     [ _  _  ... _  p1 p2 ... -> Dest ].
  ///
  /// If the row is [ c'(...)      p1 p2 ... -> Dest ] where c' != c,
  /// remove the column.
  ///
  /// The first skipRows rows are removed prior to the transformation.
  /// We treat ExprPatterns as wildcards with a guard.
  ///
  /// Returns a pair of the
  /// specialized clause matrix and the false branch for the constructor test.
  /// If the constructor is irrefutable, as for a tuple, struct, class, or
  /// singleton oneof, the branch block will be null.
  std::pair<ClauseMatrix, SILBasicBlock /*nullable*/ *>
  emitSpecializedBranch(SILGenFunction &gen,
                        PatternConstructor c, unsigned skipRows) const {
    assert(columnCount >= 1 && "can't specialize a matrix with no columns");
    assert(skipRows < rowCount && "can't skip more rows than we have");
    
    // Test and destructure the first column's occurrence.
    SmallVector<SILValue, 4> newOccurrences;
    SILBasicBlock *falseBB = c.emitBranchAndDestructure(gen,
                                                        getOccurrences()[0],
                                                        newOccurrences);
    // Forward the remaining columns' occurrences.
    std::copy(getOccurrences().begin() + 1, getOccurrences().end(),
              std::back_inserter(newOccurrences));
    
    // Build the specialized clause matrix.
    std::pair<ClauseMatrix, SILBasicBlock*> result{
      ClauseMatrix{newOccurrences, rowCount},
      falseBB
    };
    ClauseMatrix &specialized = result.first;
    
    for (unsigned r = skipRows; r < rowCount; ++r) {
      auto row = (*this)[r];
      auto columns = (*this)[r].getColumns();
      // If the pattern in this row isn't a wildcard and matches a different
      // constructor, it is removed from the specialized matrix.
      auto c1 = PatternConstructor::forPattern(columns[0]);
      if (c1 && c != c1)
        continue;
      
      // Destructure matching constructors and wildcards.
      SmallVector<const Pattern*, 4> newPatterns;
      c.destructurePattern(columns[0], newPatterns);
      
      // Forward the remaining pattern columns from the row.
      std::copy(columns.begin() + 1, columns.end(),
                std::back_inserter(newPatterns));
      
      // Append the new row.
      specialized.addRow(row.getLoc(), row.getDest(), row.getGuard(),
                         newPatterns, row.getExprGuards());
    }
    
    return result;
  }
  
  /// Transform this matrix into its default. The following row-wise
  /// transformation on the matrix is performed in-place:
  ///
  /// If the row is [ _ p1 p2 ... -> Dest ],
  /// reduce to       [ p1 p2 ... -> Dest ].
  ///
  /// If the row is [ c(...) p1 p2 ... -> Dest ] for any constructor c,
  /// remove the row.
  ///
  /// This collects all the wildcard rows not exclusively associated with any
  /// one specialized matrix.
  ///
  /// The first skipRows rows are removed prior to the transformation.
  /// We treat ExprPatterns as wildcards with a guard.
  void reduceToDefault(unsigned skipRows) {
    assert(columnCount >= 1 && "can't default a matrix with no columns");
    assert(skipRows < rowCount && "can't skip more rows than we have");

    // Drop the first occurrence.
    memmove(&getMutableOccurrences()[0], &getMutableOccurrences()[1],
            sizeof(SILValue) * (columnCount - 1));
    
    // Discard the skipped rows.
    memmove(getMutableRowPrefix(0), getMutableRowPrefix(skipRows),
            getRowStride() * (rowCount - skipRows));
    rowCount -= skipRows;
    
    unsigned r = 0;
    while (r < rowCount) {
      // Filter specialized rows.
      if (!isWildcardPattern((*this)[r][0])) {
        removeRow(r);
        continue;
      }
      
      // Discard the head wildcard of wildcard rows.
      auto row = (*this)[r];
      memmove(&row[0], &row[1], sizeof(Pattern*) * (columnCount - 1));
      ++r;
    }
    
    // Shrink the column count.
    --columnCount;
  }  
};

} // end anonymous namespace

namespace llvm {
  template<> struct DenseMapInfo<PatternConstructor> {
    static PatternConstructor getEmptyKey() {
      return PatternConstructor(PatternConstructor::Kind(-1));
    }
    static PatternConstructor getTombstoneKey() {
      return PatternConstructor(PatternConstructor::Kind(-2));
    }
    static unsigned getHashValue(PatternConstructor c) {
      switch (c.getKind()) {
      case PatternConstructor::Kind::Wildcard:
        return 0;
      case PatternConstructor::Kind::Tuple:
        return DenseMapInfo<unsigned>::getHashValue(c.getTupleArity());
      }
    }
    static bool isEqual(PatternConstructor a, PatternConstructor b) {
      return a == b;
    }
  };
}

/// True if a set of constructors forms a signature for the type.
static bool
constructorsFormSignature(const llvm::DenseSet<PatternConstructor> &set) {
  if (set.empty())
    return false;
  switch (set.begin()->getKind()) {
  // Tuple constructors trivially form a signature.
  case PatternConstructor::Kind::Tuple:
    return true;
      
  case PatternConstructor::Kind::Wildcard:
    llvm_unreachable("shouldn't have specialized on a wildcard");
  }
}

/// Recursively emit a decision tree from the given pattern matrix.
static void emitDecisionTree(SILGenFunction &gen,
                             ClauseMatrix &&clauses) {
  // If there are no rows, then we fail. This will be a dataflow error if we
  // can reach here.
  if (clauses.rows() == 0) {
    gen.B.createUnreachable();
    return;
  }
  
  // If the first rows are all wildcards (or empty), then try their
  // guards. ExprPatterns are treated as wildcards with guards.
  unsigned r = 0, rows = clauses.rows();
  for (; r < rows; ++r) {
    ClauseRow row = clauses[r];
    bool wildcardRow = true;
    for (unsigned c = 0, cols = clauses.columns(); c < cols; ++c) {
      if (!isWildcardPattern(row[c])) {
        wildcardRow = false;
        break;
      }
    }
  
    if (!wildcardRow)
      break;
  
    // If the row has a guard, emit it, and try the next row if it fails.
    if (row.emitDispatch(gen))
      return;
  }
  
  // If there are no rows remaining, fail. This will be a dataflow error if we
  // can reach here.
  if (r >= rows) {
    gen.B.createUnreachable();
    return;
  }

  // If we get this far, we have at least one non-wildcard row with at least
  // one column.
  assert(clauses.rows() >= 1 && clauses.columns() >= 1 &&
         "empty clause matrices should have been handled above");
  
  // Specialize on the next necessary column to continue testing the match.
  // TODO: We should choose the necessary column using one or more of Maranget's
  // heuristics and specialize on that column. For now we just do naive
  // left-to-right specialization.
  llvm::DenseSet<PatternConstructor> specialized;
  unsigned skipRows = r;
  for (; r < rows; ++r) {
    auto c = PatternConstructor::forPattern(clauses[r][0]);
    if (!c) continue;
    // If we've seen this constructor already, skip it.
    if (specialized.count(c))
      continue;
    specialized.insert(c);
    
    auto specialization = clauses.emitSpecializedBranch(gen, c, skipRows);
    ClauseMatrix &submatrix = specialization.first;
    SILBasicBlock *nextBB = specialization.second;
    // If the constructor is irrefutable, tail call.
    if (!nextBB)
      return emitDecisionTree(gen, std::move(submatrix));
    // Otherwise, emit the submatrix into the true branch, then continue on the
    // false branch.
    emitDecisionTree(gen, std::move(submatrix));
    assert(!gen.B.hasValidInsertionPoint()
           && "recursive emitDecisionTree did not terminate all its BBs");
    gen.B.emitBlock(nextBB);
  }
  
  // If the set of specialized constructors form a signature for the type (i.e.,
  // they're exhaustive), then we're done.
  if (constructorsFormSignature(specialized)) {
    // FIXME: Kill this BB.
    gen.B.createUnreachable();
    return;
  }
  
  // Otherwise, recur into the default matrix.
  clauses.reduceToDefault(skipRows);
  return emitDecisionTree(gen, std::move(clauses));
}

void SILGenFunction::emitSwitchStmt(SwitchStmt *S) {
  Scope OuterSwitchScope(Cleanups);
  
  // Emit the subject value.
  ManagedValue subject = visit(S->getSubjectExpr()).getAsSingleValue(*this);
  
  // Emit the skeleton of the switch. Map cases to blocks so we can handle
  // fallthrough statements.
  FallthroughDest::Map caseBodyBlocks;

  for (auto *C : S->getCases())
    // Emit the condition for this case.
    caseBodyBlocks[C] = new (F.getModule()) SILBasicBlock(&F);

  SILBasicBlock *contBB = new (F.getModule()) SILBasicBlock(&F);
  
  // Set up an initial clause matrix.
  ClauseMatrix clauses(subject.getValue(), S->getCases().size());
  
  for (auto *caseBlock : S->getCases())
    for (auto *label : caseBlock->getCaseLabels())
      for (auto *pattern : label->getPatterns())
        clauses.addRow(caseBlock, caseBodyBlocks[caseBlock],
                       label->getGuardExpr(), pattern);
  
  // Emit the decision tree.
  emitDecisionTree(*this, std::move(clauses));
  assert(!B.hasValidInsertionPoint() &&
         "emitDecisionTree did not terminate all its BBs");
  
  // Emit the case bodies.
  FallthroughDestStack.emplace_back(caseBodyBlocks, getCleanupsDepth());
  for (auto &caseAndBlock : caseBodyBlocks) {
    CaseStmt *c = caseAndBlock.first;
    SILBasicBlock *bb = caseAndBlock.second;
    
    B.emitBlock(bb);
    Scope CaseScope(Cleanups);
    visit(c->getBody());
    if (B.hasValidInsertionPoint())
      B.createBranch(c, contBB);
  }
  FallthroughDestStack.pop_back();
  
  assert(!B.hasValidInsertionPoint() &&
         "not all case blocks were terminated");
  B.emitBlock(contBB);
}
