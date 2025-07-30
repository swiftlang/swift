//===---- Test.h - Testing based on specify_test insts -*- C++ ----*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// TO ADD A NEW TEST, just add a new FunctionTest instance.
// - In the source file containing the functionality you want to test.
// - #include "swift/SIL/Test.h"
// - namespace swift::test {
//   static FunctionTest MyNewTest(
//     "my_new_test",
//     [](auto &function, auto &arguments, auto &test) {
//   });
//   } // end namespace swift::test
//
//===----------------------------------------------------------------------===//
//
// Provides a mechanism for writing tests against compiler code in the context
// of a function.  The goal is to get the same effect as calling a function and
// checking its output.
//
// This is done via the specify_test instruction.  Using one or more instances
// of it in your test case's SIL function, you can specify which test (instance
// of FunctionTest) should be run and what arguments should be provided to it.
// For full details of the specify_test instruction's grammar, see SIL.rst.
//
// The test grabs the arguments it expects out of the test::Arguments instance
// it is provided.  It calls some function or functions.  It then prints out
// interesting results.  These results can then be FileCheck'd.
//
// CASE STUDY:
// Here's an example of how it works:
// 0) A source file, NeatUtils.cpp contains
//
//    static void myNeatoUtility(unsigned, SILValue, SILFunction *) { ... }
//
//    and
//
//    static FunctionTest MyNeatoUtilityTest(
//      "my_neato_utility",
//      [](auto &function, auto &arguments, auto &test) {
//         // The code here is described in detail below.
//         // See 4).
//         auto count = arguments.takeUInt();
//         auto target = arguments.takeValue();
//         auto callee = arguments.takeFunction();
//         // See 5)
//         myNeatoUtility(count, target, callee);
//         // See 6)
//         function.dump();
//      });
// 1) A test test/SILOptimizer/interesting_functionality_unit.sil runs the
//    TestRunner pass:
//     // RUN: %target-sil-opt -test-runner %s -o /dev/null 2>&1 | %FileCheck %s
// 2) A function in interesting_functionality_unit.sil contains the
//    specify_test instruction.
//      sil @f : $() -> () {
//      ...
//      specify_test "my_neato_utility 43 %2 @function[other_fun]"
//      ...
//      }
// 3) TestRunner finds the FunctionTest instance MyNeatoUtilityTest registered
//    under the name "my_neato_utility", and calls ::run() on it, passing first
//    the function, last the test::FunctionTest instance, AND in the middle,
//    most importantly a test::Arguments instance that contains
//
//      (43 : unsigned long, someValue : SILValue, other_fun : SILFunction *)
//
// 4) MyNeatoUtilityTest calls takeUInt(), takeValue(), and takeFunction() on
//    the test::Arguments instance.
//      auto count = arguments.takeUInt();
//      auto target = arguments.takeValue();
//      auto callee = arguments.takeFunction();
//    WARNING: Don't call more than one of these in a single expression!  The
//             order of evaluation is implementation defined, but the values
//             must be taken out of the arguments in the order they appear in
//             the test.
// 5) MyNeatoUtilityTest calls myNeatoUtility, passing these values along.
//      myNeatoUtility(count, target, callee);
// 6) MyNeatoUtilityTest then dumps out the current function, which was modified
//    in the process.
//      function.dump()
// 7) The test file test/SILOptimizer/interesting_functionality_unit.sil matches
// the
//    expected contents of the modified function:
//    // CHECK-LABEL: sil @f
//    // CHECK-NOT:     function_ref @other_fun
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_TEST_H
#define SWIFT_SIL_TEST_H

#include "swift/SIL/ParseTestSpecification.h"
#include "swift/SIL/SILBridging.h"

namespace swift {

class SILFunction;
class SILFunctionTransform;
class SILPassManager;
class DominanceAnalysis;
class DominanceInfo;
class DeadEndBlocks;

namespace test {

struct Arguments;
class TestRunner;

/// A test that is run when the corresponding specify_test instruction is
/// processed by the TestRunner pass.
///
/// Tests are instantiated once at program launch.  At that time, they are
/// stored in a registry name -> test.  When an specify_test instruction
/// naming a particular test is processed, that test is run.
///
/// This is a value type because we have no persistent location in which to
/// store SwiftCompilerSources tests.
class FunctionTest final {
public:
  /// Wraps a test lambda.
  ///
  /// There are three arguments in order of priority:
  /// - SILFunction & - the function with the specify_test instruction
  /// - Arguments & - the resolved list of args specified by the instruction
  /// - FunctionTest & - the test being run; used to find less commonly used
  ///                    values such as the results of analyses
  using Invocation = void (*)(SILFunction &, Arguments &, FunctionTest &);

  using NativeSwiftInvocation = void *;

private:
  /// The lambda to be run.
  TaggedUnion<Invocation, NativeSwiftInvocation> invocation;

  bool isSwiftSILTest = false;

public:
  /// Creates a test that will run \p invocation and stores it in the global
  /// registry.
  ///
  /// To create a new test, just write
  ///
  ///     namespace swift::test {
  ///     static FunctionTest myTest("my_new_test", [](
  ///       SILFunction &function, Arguments &arguments, FunctionTest &test){
  ///         // test code
  ///       });
  ///     } // end namespace swift::test
  FunctionTest(StringRef name, Invocation invocation);

  /// Creates a test that will run \p invocation and stores it in the global
  /// registry.
  static void createNativeSwiftFunctionTest(StringRef name,
                                            NativeSwiftInvocation invocation,
                                            bool isSILTest);

  /// Computes and returns the function's dominance tree.
  DominanceInfo *getDominanceInfo();

  /// The function's dead-end blocks.
  DeadEndBlocks *getDeadEndBlocks();

  /// Returns the active pass manager.
  SILPassManager *getPassManager();

  /// Returns the indicated analysis.
  ///
  /// NOTE: This function can only be called from files that can import
  ///       SILFunctionTransform--those in SILOptimizer and libraries that
  ///       depend on it.  See `Layering` below for more details.
  template <typename Analysis, typename Transform = SILFunctionTransform>
  Analysis *getAnalysis();

  SILFunctionTransform *getPass();

  SILContext *getSILContext();

//===----------------------------------------------------------------------===//
//=== MARK: Implementation Details                                         ===
//===----------------------------------------------------------------------===//
//
// To read, write, and debug function test failures, see above.
//
// The following implementation details have to do with executing these tests
// and handling library layering.

// TestRunner interface:
private:
  struct Dependencies;

  /// Run the stored \p invocation lambda.
  void run(SILFunction &function, Arguments &arguments,
           SILFunctionTransform &pass, Dependencies &dependencies);

  /// Retrieve the test with named \p name from the global registry.
  static FunctionTest get(StringRef name);

  /// The instance of the TestRunner pass currently running this test.  Only
  /// non-null when FunctionTest::run is executing.
  SILFunctionTransform *pass;
  /// The function which the TestRunner pass is currently processing.  Only
  /// non-null when FunctionTest::run is executing.
  SILFunction *function;

  friend class TestRunner;

// Layering:
//
// Dealing with the differences between the SIL and SILOptimizer libraries.
//
// Motivation: 1) Enable writing FunctionTests inline in any source file that
//                can import the SIL library's headers.
//             2) Allow tests to access the tools that are visible in the source
//                file where they are written.
//
// Two examples:
// A) Tests in the SIL library (or libraries that can import its headers) must
//    be able to access the results of analyses, e.g. DominanceInfo.
// B) Tests in the SILOptimizer library (or libraries that can import its
//    headers) must be able to access analyses themselves, e.g.
//    DominanceAnalysis.
//
// Because analyses aren't part of the SIL library, the code that computes such
// a result can't called from one of it's source files.  For example, this
// isn't possible in some SIL library cpp file
//
//     test->getPass()->getAnalysis<DominanceAnalysis>()->get(function);
//
// because DominanceAnalysis isn't visible in SIL (it's defined in
// SILOptimizer).  Indeed calling anything on getPass() isn't possible because
// SILFunctionTransform isn't visible in SIL (again, defined in SILOptimizer).
//
// This is further exacerbated by the fact that getting an analysis from a pass
// requires instantiating a template function.
//
// There are two consequences:
//
// 1) Dependencies must be provided directly (i.e. not via analyses) to tests
//    in the SIL library.
//
//    For example ::getDominanceInfo in `struct Dependencies` below.
//
// 2) The code that allows tests in the SILOptimizer library to access analyses
//    must not be instantiated when it's imported into the SIL library.
//
//    Concretely, the "extra" template argument `typename Transform` on
//    getAnalysis and the thunk in the impl:: namespace below.
private:
  /// Functions for getting tools that are visible in the SIL library.
  ///
  /// The implementation is provided in the SILOptimizer libary where analyses
  /// are visible: TestRunner::FunctionTestDependenciesImpl.
  struct Dependencies {
    virtual DominanceInfo *getDominanceInfo() = 0;
    virtual DeadEndBlocks *getDeadEndBlocks() = 0;
    virtual SILPassManager *getPassManager() = 0;
    virtual SILContext *getSILContext() = 0;
    virtual ~Dependencies(){};
  };

  /// The vendor for dependencies provided to the test by TestRunner.  Only
  /// non-null when FunctionTest::run is executing.
  Dependencies *dependencies;

public:
  /// Creates a test that will run \p invocation.  For use by
  /// createNativeSwiftFunctionTest.
  FunctionTest(StringRef name, NativeSwiftInvocation invocation, bool isSILTest);
};

/// Thunks for delaying template instantiation.
///
/// Because C++ lacks "runtime concepts" (Swift's protocols), it's not possible
/// for the Dependencies struct to have a method like
///
///     template <typename Analysis>
///     virtual Analysis *getAnalysis() = 0;
///
/// In order to give tests access to analyses, SILFunctionTransform::getAnalysis
/// template must be instantiated by the test code.  This could be done by
/// making the test-running pass available directly to test code.  Here,
/// instead, it is done by providing a passthrough on test.  But this is
/// complicated by library layering:
///
/// Calling methods directly on `FunctionTest::pass` is illegal from the SIL
/// library (or any library that can't import SILOptimizer headers, where
/// SILFunctionTransform is defined):
///
///     member access into incomplete type 'swift::SILFunctionTransform'
///
/// Instead, these thunks are called with the pass as an argument.  Because the
/// static type of pass in these thunks is just  "pointer to `typename
/// Transform`", it's legal to call methods on the pass here: these templates
/// can only be instantiated in libraries that can import SILOptimizer.
namespace impl {
template <typename Analysis, typename Transform>
Analysis *getAnalysisFromTransform(Transform *pass) {
  return pass->template getAnalysis<Analysis>();
}
} // end namespace impl

template <typename Analysis, typename Transform>
Analysis *FunctionTest::getAnalysis() {
  return impl::getAnalysisFromTransform<Analysis, Transform>(pass);
}

inline SILFunctionTransform *FunctionTest::getPass() { return pass; }
} // namespace test
} // namespace swift

#endif
