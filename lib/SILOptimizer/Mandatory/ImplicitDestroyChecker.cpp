//===--- ImplicitDestroyChecker.cpp - Diagnose implicit destroys ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Diagnoses destroys of values that can't be destroyed implicitly (see
/// `SILType::isImplicitlyDestroyable()`), such as values of `~Deinitable`
/// types.
///
/// This pass runs right after the move-only checker, which guarantees that no
/// path consumes a noncopyable value more than once, and that every path that
/// doesn't consume the value ends its lifetime with an explicit
/// `destroy_value` or `destroy_addr`. A call or any other legal transfer of the
/// value is a consume, not a destroy. "Consumed on every path" therefore
/// reduces to a simple rule: no destroy on a path that doesn't end in
/// `unreachable`.
///
/// Some destroys only destroy a value's stored properties, which is fine if
/// each of them can be destroyed implicitly. That's the case after
/// `discard self`, and when the move-only checker destroys a value before
/// reinitializing all of its stored properties one by one.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-implicit-destroy-checker"

#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/VariableNameUtils.h"
#include "llvm/ADT/MapVector.h"

using namespace swift;

namespace {

/// Returns the first stored property of a value of \p type that can't be
/// destroyed implicitly, or null if there isn't one.
static VarDecl *getNonImplicitlyDestroyableStoredProperty(
    SILType type, const SILFunction &fn) {
  auto *structDecl = type.getStructOrBoundGenericStruct();
  for (auto *field : structDecl->getStoredProperties()) {
    auto fieldType = type.getFieldType(field, fn.getModule(),
                                       fn.getTypeExpansionContext());
    if (!fieldType.isImplicitlyDestroyable())
      return field;
  }
  return nullptr;
}

/// Returns true if \p value is the result of `drop_deinit`, which only leaves
/// trivially-destroyed stored properties to destroy.
static bool isDroppedDeinit(SILValue value) {
  while (true) {
    if (isa<DropDeinitInst>(value))
      return true;
    if (isa<BeginAccessInst>(value) || isa<MoveValueInst>(value) ||
        isa<MarkUnresolvedNonCopyableValueInst>(value)) {
      value = cast<SingleValueInstruction>(value)->getOperand(0);
      continue;
    }
    return false;
  }
}

/// Strips access markers and, if \p projected isn't null, the projections of
/// stored properties from \p address.
static SILValue stripAccessAndProjections(SILValue address, bool *projected) {
  while (true) {
    if (auto *access = dyn_cast<BeginAccessInst>(address)) {
      address = access->getSource();
      continue;
    }
    if (projected &&
        (isa<StructElementAddrInst>(address) ||
         isa<TupleElementAddrInst>(address))) {
      *projected = true;
      address = cast<SingleValueInstruction>(address)->getOperand(0);
      continue;
    }
    return address;
  }
}

/// Returns true if \p destroy destroys a struct that's about to be
/// reinitialized one stored property at a time.
static bool isBeforeMemberwiseReinit(DestroyAddrInst *destroy) {
  if (!destroy->getOperand()->getType().getStructOrBoundGenericStruct())
    return false;

  auto base = stripAccessAndProjections(destroy->getOperand(), nullptr);
  for (auto *inst = destroy->getNextInstruction(); inst;
       inst = inst->getNextInstruction()) {
    SILValue dest;
    if (auto *store = dyn_cast<StoreInst>(inst))
      dest = store->getDest();
    else if (auto *copy = dyn_cast<CopyAddrInst>(inst))
      dest = copy->getDest();
    if (!dest)
      continue;

    bool projected = false;
    if (stripAccessAndProjections(dest, &projected) == base)
      return projected;
  }
  return false;
}

/// Returns the value whose implicit destroy \p inst is, if it's a destroy of a
/// value that can't be destroyed implicitly. If \p inst only destroys the
/// value's stored properties, sets \p field to the first one of them that
/// can't be destroyed implicitly.
static SILValue getNonImplicitlyDestroyableValue(SILInstruction *inst,
                                                 const SILFunction &fn,
                                                 VarDecl *&field) {
  field = nullptr;
  SILValue value;
  if (auto *dvi = dyn_cast<DestroyValueInst>(inst)) {
    if (dvi->isDeadEnd())
      return SILValue();
    value = dvi->getOperand();
  } else if (auto *dai = dyn_cast<DestroyAddrInst>(inst)) {
    value = dai->getOperand();
  } else {
    return SILValue();
  }

  auto type = value->getType().getObjectType();
  if (type.isImplicitlyDestroyable())
    return SILValue();

  // Some destroys only destroy the stored properties.
  if (isDroppedDeinit(value))
    return SILValue();
  if (auto *dai = dyn_cast<DestroyAddrInst>(inst)) {
    if (isBeforeMemberwiseReinit(dai)) {
      field = getNonImplicitlyDestroyableStoredProperty(type, fn);
      if (!field)
        return SILValue();
    }
  }

  return value;
}

/// Returns true if \p root is a variable, so that diagnostics can name it.
static bool isVariable(SILValue root) {
  if (auto *arg = dyn_cast<SILFunctionArgument>(root))
    return arg->getDecl();
  if (auto *inst = root->getDefiningInstruction())
    return bool(DebugVarCarryingInst(inst));
  return false;
}

/// Returns the source range of the body of \p fn, if it has one.
static SourceRange getBodyRange(SILFunction *fn) {
  auto loc = fn->getLocation();
  if (auto *afd = loc.getAsASTNode<AbstractFunctionDecl>())
    return afd->getBodySourceRange();
  if (auto *closure = loc.getAsASTNode<AbstractClosureExpr>())
    if (auto *body = closure->getBody())
      return body->getSourceRange();
  return loc.getSourceRange();
}

static bool isInRange(SILFunction *fn, SourceRange range, SourceLoc loc) {
  return loc.isValid() &&
         (range.isInvalid() ||
          fn->getASTContext().SourceMgr.containsLoc(range, loc));
}

static SourceLoc getDiagnosticLoc(SILValue root, SILFunction *fn) {
  if (auto *arg = dyn_cast<SILFunctionArgument>(root)) {
    if (!arg->isClosureCapture()) {
      if (auto *decl = arg->getDecl())
        return decl->getLoc();
    }
  }

  SourceLoc loc;
  if (auto *inst = root->getDefiningInstruction())
    loc = inst->getLoc().getSourceLoc();

  // A closure's captures are declared in the enclosing function, but the
  // obligation to consume them belongs to the closure.
  if (!isInRange(fn, fn->getLocation().getSourceRange(), loc))
    return fn->getLocation().getSourceLoc();
  return loc;
}

/// Returns the location of the path exit for a note about \p destroy.
///
/// The destroy's own location is best when it points at the code that drops
/// the value, such as `_ = consume x`, an assignment, or a `return`. Some
/// destroys carry the location of the variable's declaration instead, so in
/// that case use the statement that exits the function on that path.
static SourceLoc getPathExitLoc(SILInstruction *destroy, SourceLoc declLoc) {
  auto *fn = destroy->getFunction();
  auto bodyRange = getBodyRange(fn);

  auto loc = destroy->getLoc().getSourceLoc();
  if (isInRange(fn, bodyRange, loc) && loc != declLoc)
    return loc;

  BasicBlockWorklist worklist(destroy->getParent());
  while (auto *block = worklist.pop()) {
    auto *term = block->getTerminator();
    auto termLoc = term->getLoc();
    if (auto *stmt = termLoc.getAsASTNode<Stmt>()) {
      if (isa<ReturnStmt>(stmt) || isa<ThrowStmt>(stmt))
        return termLoc.getSourceLoc();
    }
    if (term->isFunctionExiting()) {
      if (isInRange(fn, bodyRange, termLoc.getSourceLoc()))
        return termLoc.getSourceLoc();
      return bodyRange.End;
    }
    for (auto *succ : block->getSuccessorBlocks())
      worklist.pushIfNotVisited(succ);
  }
  return loc;
}

class ImplicitDestroyChecker : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();

    // Don't rerun diagnostics on deserialized functions.
    if (fn->wasDeserializedCanonical())
      return;

    // This runs even without `NondeinitableTypes`, because a client can still
    // drop a `~Deinitable` value that an API returns.

    // If an earlier pass already diagnosed this function, don't add noise.
    if (fn->hasSemanticsAttr(semantics::NO_MOVEONLY_DIAGNOSTICS))
      return;

    // Most functions have no destroys to diagnose, so find them before
    // computing the dead-end blocks.
    SmallVector<std::tuple<SILInstruction *, SILValue, VarDecl *>, 4>
        candidates;
    for (auto &block : *fn) {
      for (auto &inst : block) {
        VarDecl *field;
        if (auto value = getNonImplicitlyDestroyableValue(&inst, *fn, field))
          candidates.push_back({&inst, value, field});
      }
    }
    if (candidates.empty())
      return;

    DeadEndBlocks deadEndBlocks(fn);

    // Group the destroys by the variable or stored property that they
    // destroy, so that each one gets one error with a note for each path.
    using Key = std::pair<SILValue, VarDecl *>;
    llvm::MapVector<Key, SmallVector<SILInstruction *, 2>> destroys;
    llvm::DenseMap<Key, std::string> names;
    for (auto [inst, value, field] : candidates) {
      if (deadEndBlocks.isDeadEnd(inst->getParent()))
        continue;

      SILValue root = value;
      std::optional<std::string> name;
      if (auto nameAndRoot = VariableNameInferrer::inferNameAndRoot(value)) {
        root = nameAndRoot->second;
        if (isVariable(root))
          name = nameAndRoot->first.str().str();
      }

      Key key{root, field};
      if (name) {
        if (field)
          *name += "." + field->getName().str().str();
        names[key] = *name;
      }
      destroys[key].push_back(inst);
    }

    auto &diags = fn->getASTContext().Diags;
    for (auto &[key, rootDestroys] : destroys) {
      auto root = key.first;
      auto loc = getDiagnosticLoc(root, fn);
      if (loc.isInvalid())
        loc = rootDestroys.front()->getLoc().getSourceLoc();

      auto name = names.find(key);
      if (name != names.end()) {
        diags.diagnose(loc, diag::sil_implicit_destroy_not_consumed,
                       name->second);
      } else {
        diags.diagnose(loc, diag::sil_implicit_destroy_unnamed_not_consumed,
                       root->getType().getASTType());
      }

      // Paths can share an exit, as the cases of a `switch` do.
      llvm::SmallPtrSet<const void *, 4> exitLocs;
      for (auto *destroy : rootDestroys) {
        auto exitLoc = getPathExitLoc(destroy, loc);
        if (exitLoc.isValid() &&
            exitLocs.insert(exitLoc.getOpaquePointerValue()).second)
          diags.diagnose(exitLoc, diag::sil_implicit_destroy_path_exit);
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createImplicitDestroyChecker() {
  return new ImplicitDestroyChecker();
}
