//===--- NestedSemanticFunctionCheck.cpp - Disallow nested @_semantic -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This pass raises a diagnostic error if any semantic functions in the current
/// module are improperly nested. A semantic function has an @_semantic
/// attribute. Semantic functions may call other semantic functions, but
/// semantic and non-semantic call frames may not be interleaved.
///
/// @_semantics(...)
/// func funcA() {
///   funcB()
/// }
/// // Error: funcB is called by a semantic function and calls
/// // a semantic function.
/// func funcB() {
///   funcC()
/// }
/// @_semantics(...)
/// func funcC() {}
///
/// For the pass pipeline to function well, @_semantic calls need to be
/// processed top-down, while inlining proceeds bottom-up. With proper nesting,
/// this is easily accomplished, but with improper nesting, semantic function
/// can be prematurely "hidden".
///
/// For simplicity, don't bother checking whether semantic functions in one
/// module call non-semantic functions in a different module. Really,
/// "optimizable" @_semantics should only exist in the standard library.
///
/// TODO: The @_semantics tag should only ever be used to convey function
/// semantics that are important to optimizations and which cannot be determined
/// from the function body. Make sure this tag is never used for compiler hints
/// or informational attributes, which do not require special pass pipeline and
/// module serialization behavior.
///
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/PerformanceInlinerUtils.h"

using namespace swift;

namespace {

class NestedSemanticFunctionCheck : public SILFunctionTransform {
  // Mark all functions that may indirectly call a semantic function, ignoring
  // simple wrappers, but are not themselves an optimizable semantic function.
  //
  // This could be a very large set. It is built gradually during bottom-up
  // function transforms, then deleted once all functions are processed before
  // excuting the next pass pipeline.
  llvm::SmallPtrSet<SILFunction *, 8> mayCallSemanticFunctions;

  // Mark semantic functions and wrappers around semantic calls.
  llvm::SmallPtrSet<SILFunction *, 8> semanticFunctions;

public:
  void run() override;

protected:
  void checkSemanticFunction(SILFunction *f);
};

} // end anonymous namespace

/// This is a semantic function. Diagnose any calls to mayCallSemanticFunctions.
void NestedSemanticFunctionCheck::checkSemanticFunction(SILFunction *f) {
  for (auto &bb : *f) {
    for (auto &i : bb) {
      auto apply = FullApplySite::isa(&i);
      if (!apply) {
        continue;
      }
      auto callee = apply.getReferencedFunctionOrNull();
      if (!callee) {
        continue;
      }
      // Semantic function calling non-semantic function with improperly
      // nested semantic calls underneath.
      if (mayCallSemanticFunctions.count(callee)) {
        ASTContext &astContext = f->getASTContext();
        astContext.Diags.diagnose(apply.getLoc().getSourceLoc(),
                                  diag::semantic_function_improper_nesting);
      }
    }
  }
}

// If this is a semantic function, diagnose it immediately. Otherwise propagate
// the mayCallSemanticFunctions and semanticCallWrappers sets upward in the call
// tree.
void NestedSemanticFunctionCheck::run() {
  auto *f = getFunction();
  if (!semanticFunctions.count(f) && isOptimizableSemanticFunction(f)) {
    semanticFunctions.insert(f);
    checkSemanticFunction(f);
    return;
  }
  // Add this function to mayCallSemanticFunctions if needed.
  if (mayCallSemanticFunctions.count(f)) {
    return;
  }
  for (auto &bb : *f) {
    for (auto &i : bb) {
      auto apply = FullApplySite::isa(&i);
      if (!apply) {
        continue;
      }
      // If this is a trivial wrapper around a semantic call, don't add it
      // immediately to the mayCallSemanticFunctions set, but add it to
      // semanticCallWrappers so that its caller will be added to
      // mayCallSemanticFunctions.
      if (isNestedSemanticCall(apply)) {
        semanticFunctions.insert(f);
        continue;
      }
      auto callee = apply.getReferencedFunctionOrNull();
      if (!callee) {
        continue;
      }
      // Propagate mayCallSemanticFunctions.
      if (mayCallSemanticFunctions.count(callee)
          || semanticFunctions.count(callee)) {
        mayCallSemanticFunctions.insert(f);
      }
    }
  }
}

SILTransform *swift::createNestedSemanticFunctionCheck() {
  return new NestedSemanticFunctionCheck();
}
