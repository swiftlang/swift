//===------------------------ UnitTestRunner.cpp --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// TO ADD A NEW TEST, search in this file for [new_tests]
//
// Provides a mechanism for doing faux unit tests.  The idea is to emulate the
// basic functionality of calling a function and validating its results/effects.
//
// This is done via the test_specification instruction.  Using one or more
// instances of it in your function, you can specify which test (i.e. UnitTest
// subclass) should be run and what arguments should be provided to it.  The
// test grabs the arguments it expects out of the test::Arguments instance it is
// provided.  It calls some function or functions.  It then prints out
// interesting results.  These results can then be FileCheck'd.
//
// CASE STUDY:
// Here's an example of how it works:
// 1) A test test/SILOptimizer/interesting_functionality.sil runs this pass:
//     // RUN: %target-sil-opt -unit-test-runner %s 2>&1 | %FileCheck %s
// 2) A function in interesting_functionality.sil contains the
//    test_specification instruction.
//      sil @f : $() -> () {
//      ...
//      test_specification "my-neato-utility 43 @trace[2] @function[other_fun]"
//      ...
//      }
// 3) UnitTestRunner sees the name "my-neato-utility", instantiates the
//    corresponding UnitTest subclass MyNeatoUtilityTest, and calls ::invoke()
//    on it, passing an test::Arguments that contains
//      (43 : unsigned long, someValue : SILValue, other_fun : SILFunction *)
//
// 4) MyNeatoUtilityTest calls takeUInt(), takeValue(), and takeFunction() on
//    the test::Arguments instance.
//      auto count = arguments.takeUInt();
//      auto target = arguments.takeValue();
//      auto callee = arguments.takeFunction();
// 5) MyNeatoUtilityTest calls myNeatoUtility, passing these values along.
//      myNeatoUtility(count, target, callee);
// 6) MyNeatoUtilityTest then dumps out the current function, which was modified
//    in the process.
//      getFunction()->dump();
// 7) The test file test/SILOptimizer/interesting_functionality.sil matches the
//    expected contents of the modified function:
//    // CHECK-LABEL: sil @f
//    // CHECK-NOT:     function_ref @other_fun
//
// [new_tests] TESTING MORE FUNCTIONALITY:
//
// 1) Add a new UnitTest subclass.
//    - In the section "Unit Tests Subclasses" section ordered alphabetically.
//    - Add a constructor: NewTest(UnitTestRunner *pass) : UnitTest(pass) {}
//    - Implement invoke: void invoke(test::Arguments &arguments) override {...}
//    - Call the take{TYPE} methods to get the arguments you need.
//    - Call your function with those arguments.
// 2) Add a new ADD_UNIT_TEST_SUBCLASS line.
//    - Ordered alphabetically.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Type.h"
#include "swift/Basic/TaggedUnion.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILArgumentArrayRef.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBridging.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Transforms/SimplifyCFG.h"
#include "swift/SILOptimizer/Utils/CanonicalizeBorrowScope.h"
#include "swift/SILOptimizer/Utils/CanonicalizeOSSALifetime.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "swift/SILOptimizer/Utils/ParseTestSpecification.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include <iterator>
#include <memory>

using namespace swift;
using namespace swift::test;

namespace {

class UnitTestRunner;

class UnitTest {
  UnitTestRunner *pass;

protected:
  template <typename Analysis>
  Analysis *getAnalysis();
  UnitTestRunner *getPass() { return pass; }
  SILFunction *getFunction();

public:
  UnitTest(UnitTestRunner *pass) : pass(pass){};
  virtual ~UnitTest() {}
  virtual void invoke(Arguments &arguments) = 0;
};

class UnitTestRunner : public SILFunctionTransform {
  void printTestLifetime(bool begin, unsigned testIndex, unsigned testCount,
                         StringRef name, ArrayRef<StringRef> components) {
    StringRef word = begin ? "begin" : "end";
    llvm::errs() << word << " running test " << testIndex + 1 << " of "
                 << testCount << " on " << getFunction()->getName() << ": "
                 << name << " with: ";
    for (unsigned long index = 0, size = components.size(); index < size;
         ++index) {
      llvm::errs() << components[index];
      if (index != size - 1) {
        llvm::errs() << ", ";
      }
    }
    llvm::errs() << "\n";
  }

  template <typename Doit>
  void withTest(StringRef name, Doit doit);

  void runTest(StringRef name, Arguments &arguments) {
    withTest(name, [&](auto *test) { test->invoke(arguments); });
  }

  void run() override {
    llvm::SmallVector<UnparsedSpecification, 2> testSpecifications;
    getTestSpecifications(getFunction(), testSpecifications);
    Arguments arguments;
    SmallVector<StringRef, 4> components;
    for (unsigned long index = 0, size = testSpecifications.size();
         index < size; ++index) {
      components.clear();
      arguments.clear();
      auto testSpecification = testSpecifications[index];
      test::parseTestArgumentsFromSpecification(
          getFunction(), testSpecification, arguments, components);
      auto name = arguments.takeString();
      ArrayRef<StringRef> argumentStrings = components;
      argumentStrings = argumentStrings.drop_front();
      printTestLifetime(/*begin=*/true, /*index=*/index, /*size=*/size, name,
                        argumentStrings);
      runTest(name, arguments);
      printTestLifetime(/*begin=*/false, /*index=*/index, /*size=*/size, name,
                        argumentStrings);
    }
  }
  friend class UnitTest;
};

//===----------------------------------------------------------------------===//
// MARK: Unit Test Subclasses                                                 {{
//===----------------------------------------------------------------------===//

// Arguments:
// - bool: pruneDebug
// - bool: maximizeLifetimes
// - SILValue: value to canonicalize
// Dumps:
// - function after value canonicalization
struct CanonicalizeOSSALifetimeTest : UnitTest {
  CanonicalizeOSSALifetimeTest(UnitTestRunner *pass) : UnitTest(pass) {}
  void invoke(Arguments &arguments) override {
    auto *accessBlockAnalysis = getAnalysis<NonLocalAccessBlockAnalysis>();
    auto *dominanceAnalysis = getAnalysis<DominanceAnalysis>();
    DominanceInfo *domTree = dominanceAnalysis->get(getFunction());
    auto pruneDebug = arguments.takeBool();
    auto maximizeLifetimes = arguments.takeBool();
    InstructionDeleter deleter;
    CanonicalizeOSSALifetime canonicalizer(pruneDebug, maximizeLifetimes, accessBlockAnalysis,
                                           domTree, deleter);
    auto value = arguments.takeValue();
    canonicalizer.canonicalizeValueLifetime(value);
    getFunction()->dump();
  }
};

// Arguments: NONE
// Dumps:
// - the function
struct DumpFunction : UnitTest {
  DumpFunction(UnitTestRunner *pass) : UnitTest(pass) {}
  void invoke(Arguments &arguments) override { getFunction()->dump(); }
};

// Arguments: NONE
// Dumps: the index of the self argument of the current function
struct FunctionGetSelfArgumentIndex : UnitTest {
  FunctionGetSelfArgumentIndex(UnitTestRunner *pass) : UnitTest(pass) {}
  void invoke(Arguments &arguments) override {
    auto index =
        SILFunction_getSelfArgumentIndex(BridgedFunction{getFunction()});
    llvm::errs() << "self argument index = " << index << "\n";
  }
};

// Arguments:
// - instruction
// Dumps:
// - instruction
// - whether it's a deinit barrier
struct IsDeinitBarrierTest : UnitTest {
  IsDeinitBarrierTest(UnitTestRunner *pass) : UnitTest(pass) {}
  void invoke(Arguments &arguments) override {
    auto *instruction = arguments.takeInstruction();
    auto *analysis = getAnalysis<BasicCalleeAnalysis>();
    auto isBarrier = isDeinitBarrier(instruction, analysis);
    instruction->dump();
    auto *boolString = isBarrier ? "true" : "false";
    llvm::errs() << boolString << "\n";
  }
};

// Arguments:
// - variadic list of - instruction: a last user
// Dumps:
// - the insertion points
struct PrunedLivenessBoundaryWithListOfLastUsersInsertionPointsTest : UnitTest {
  PrunedLivenessBoundaryWithListOfLastUsersInsertionPointsTest(
      UnitTestRunner *pass)
      : UnitTest(pass) {}
  void invoke(Arguments &arguments) override {
    PrunedLivenessBoundary boundary;
    while (arguments.hasUntaken()) {
      boundary.lastUsers.push_back(arguments.takeInstruction());
    }
    boundary.visitInsertionPoints(
        [](SILBasicBlock::iterator point) { point->dump(); });
  }
};

struct ShrinkBorrowScopeTest : UnitTest {
  ShrinkBorrowScopeTest(UnitTestRunner *pass) : UnitTest(pass) {}
  void invoke(Arguments &arguments) override {
    auto instruction = arguments.takeValue();
    auto expected = arguments.takeBool();
    auto *bbi = cast<BeginBorrowInst>(instruction);
    auto *analysis = getAnalysis<BasicCalleeAnalysis>();
    SmallVector<CopyValueInst *, 4> modifiedCopyValueInsts;
    InstructionDeleter deleter(
        InstModCallbacks().onDelete([&](auto *instruction) {
          llvm::errs() << "DELETED:\n";
          instruction->dump();
        }));
    auto shrunk =
        shrinkBorrowScope(*bbi, deleter, analysis, modifiedCopyValueInsts);
    unsigned index = 0;
    for (auto *cvi : modifiedCopyValueInsts) {
      auto expectedCopy = arguments.takeValue();
      llvm::errs() << "rewritten copy " << index << ":\n";
      llvm::errs() << "expected:\n";
      expectedCopy->print(llvm::errs());
      llvm::errs() << "got:\n";
      cvi->dump();
      assert(cvi == expectedCopy);
      ++index;
    }
    assert(expected == shrunk && "didn't shrink expectedly!?");
  }
};

struct SimplifyCFGCanonicalizeSwitchEnum : UnitTest {
  SimplifyCFGCanonicalizeSwitchEnum(UnitTestRunner *pass) : UnitTest(pass) {}
  void invoke(Arguments &arguments) override {
    auto *passToRun = cast<SILFunctionTransform>(createSimplifyCFG());
    passToRun->injectPassManager(getPass()->getPassManager());
    passToRun->injectFunction(getFunction());
    SimplifyCFG(*getFunction(), *passToRun, /*VerifyAll=*/false,
                /*EnableJumpThread=*/false)
        .canonicalizeSwitchEnums();
  }
};


// Arguments:
// - string: list of characters, each of which specifies subsequent arguments
//           - A: (block) argument
//           - F: function
//           - B: block
//           - I: instruction
//           - V: value
//           - O: operand
//           - b: boolean
//           - u: unsigned
//           - s: string
// - ...
// - an argument of the type specified in the initial string
// - ...
// Dumps:
// - for each argument (after the initial string)
//   - its type
//   - something to identify the instance (mostly this means calling dump)
struct TestSpecificationTest : UnitTest {
  TestSpecificationTest(UnitTestRunner *pass) : UnitTest(pass) {}
  void invoke(Arguments &arguments) override {
    auto expectedFields = arguments.takeString();
    for (auto expectedField : expectedFields) {
      switch (expectedField) {
      case 'A': {
        auto *argument = arguments.takeBlockArgument();
        llvm::errs() << "argument:\n";
        argument->dump();
        break;
      }
      case 'F': {
        auto *function = arguments.takeFunction();
        llvm::errs() << "function: " << function->getName() << "\n";
        break;
      }
      case 'B': {
        auto *block = arguments.takeBlock();
        llvm::errs() << "block:\n";
        block->dump();
        break;
      }
      case 'I': {
        auto *instruction = arguments.takeInstruction();
        llvm::errs() << "instruction: ";
        instruction->dump();
        break;
      }
      case 'V': {
        auto value = arguments.takeValue();
        llvm::errs() << "value: ";
        value->dump();
        break;
      }
      case 'O': {
        auto *operand = arguments.takeOperand();
        llvm::errs() << "operand: ";
        operand->print(llvm::errs());
        break;
      }
      case 'u': {
        auto u = arguments.takeUInt();
        llvm::errs() << "uint: " << u << "\n";
        break;
      }
      case 'b': {
        auto b = arguments.takeBool();
        llvm::errs() << "bool: " << b << "\n";
        break;
      }
      case 's': {
        auto s = arguments.takeString();
        llvm::errs() << "string: " << s << "\n";
        break;
      }
      default:
        llvm_unreachable("unknown field type was expected?!");
      }
    }
  }
};

// Arguments:
// - SILValue: phi
// Dumps:
// - function
// - the adjacent phis
struct VisitAdjacentReborrowsOfPhiTest : UnitTest {
  VisitAdjacentReborrowsOfPhiTest(UnitTestRunner *pass) : UnitTest(pass) {}
  void invoke(Arguments &arguments) override {
    getFunction()->dump();
    visitAdjacentReborrowsOfPhi(cast<SILPhiArgument>(arguments.takeValue()),
                                [](auto *argument) -> bool {
                                  argument->dump();
                                  return true;
                                });
  }
};

/// [new_tests] Add the new UnitTest subclass above this line. 
///             Please sort alphabetically by to help reduce merge conflicts.

//===----------------------------------------------------------------------===//
// MARK: Unit Test Subclasses                                                 }}
//===----------------------------------------------------------------------===//

template <typename Doit>
void UnitTestRunner::withTest(StringRef name, Doit doit) {
#define ADD_UNIT_TEST_SUBCLASS(STRING, SUBCLASS)                               \
  if (name == STRING) {                                                        \
    SUBCLASS it{this};                                                         \
    doit(&it);                                                                 \
    return;                                                                    \
  }

    ADD_UNIT_TEST_SUBCLASS("canonicalize-ossa-lifetime", CanonicalizeOSSALifetimeTest)
    ADD_UNIT_TEST_SUBCLASS("dump-function", DumpFunction)
    ADD_UNIT_TEST_SUBCLASS("function-get-self-argument-index", FunctionGetSelfArgumentIndex)
    ADD_UNIT_TEST_SUBCLASS("is-deinit-barrier", IsDeinitBarrierTest)
    ADD_UNIT_TEST_SUBCLASS("pruned-liveness-boundary-with-list-of-last-users-insertion-points", PrunedLivenessBoundaryWithListOfLastUsersInsertionPointsTest)
    ADD_UNIT_TEST_SUBCLASS("shrink-borrow-scope", ShrinkBorrowScopeTest)
    ADD_UNIT_TEST_SUBCLASS("simplify-cfg-canonicalize-switch-enum", SimplifyCFGCanonicalizeSwitchEnum)
    ADD_UNIT_TEST_SUBCLASS("test-specification-parsing", TestSpecificationTest)
    ADD_UNIT_TEST_SUBCLASS("visit-adjacent-reborrows-of-phi", VisitAdjacentReborrowsOfPhiTest)
    /// [new_tests] Add the new mapping from string to subclass above this line.
    ///             Please sort alphabetically by name to help reduce merge
    ///             conflicts.

#undef ADD_UNIT_TEST_SUBCLASS
  }


template <typename Analysis>
Analysis *UnitTest::getAnalysis() {
  return pass->getAnalysis<Analysis>();
}

SILFunction *UnitTest::getFunction() { return getPass()->getFunction(); }

} // anonymous namespace

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

SILTransform *swift::createUnitTestRunner() { return new UnitTestRunner(); }
