//===--- SILOptions.h - Swift Language SILGen and SIL options ---*- C++ -*-===//
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
//
// This file defines the options which control the generation, processing,
// and optimization of SIL.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SILOPTIONS_H
#define SWIFT_AST_SILOPTIONS_H

#include <string>
#include <climits>

namespace swift {

class SILOptions {
public:
  /// Controls the aggressiveness of the performance inliner.
  int InlineThreshold = -1;

  /// The number of threads for multi-threaded code generation.
  int NumThreads = 0;
  
  enum LinkingMode {
    /// Skip SIL linking.
    LinkNone,

    /// Perform normal SIL linking.
    LinkNormal,

    /// Link all functions during SIL linking.
    LinkAll
  };

  /// Controls how  perform SIL linking.
  LinkingMode LinkMode = LinkNormal;

  /// Remove all runtime assertions during optimizations.
  bool RemoveRuntimeAsserts = false;

  /// Controls whether the SIL ARC optimizations are run.
  bool EnableARCOptimizations = true;

  /// Controls whether or not paranoid verification checks are run.
  bool VerifyAll = false;

  /// Dump SIL after each transform.
  bool PrintAll = false;
  
  /// Dump pass number and name before each transform.
  bool PrintPassName = false;

  /// Time each transform invocation in the pass manager.
  bool TimeTransforms = false;
  
  /// Stop optimizing after the Nth pass. For debugging purposes.
  unsigned NumOptPassesToRun = UINT_MAX;

  /// Are we debugging sil serialization.
  bool DebugSerialization = false;

  enum AssertConfiguration: unsigned {
    // Used by standard library code to distinguish between a debug and release
    // build.
    Debug = 0,   // Enables all asserts.
    Release = 1, // Disables asserts.
    Fast = 2,    // Disables asserts, library precondition, and runtime checks.

    // Leave the assert_configuration instruction around.
    DisableReplacement = UINT_MAX
  };

  /// The assert configuration controls how assertions behave.
  unsigned AssertConfig = Debug;

  /// Should we print out instruction counts if -print-stats is passed in?
  bool PrintInstCounts = false;

  /// Should we optimize function signatures?
  bool EnableFuncSigOpts = true;

  /// Should we emit self as a guaranteed parameter?
  bool EnableGuaranteedSelf = true;

  /// Instrument code to generate profiling information.
  bool GenerateProfile = false;

  /// Emit a mapping of profile counters for use in coverage.
  bool EmitProfileCoverageMapping = false;

  /// Should we use a pass pipeline passed in via a json file? Null by default.
  StringRef ExternalPassPipelineFilename;
};

} // end namespace swift

#endif
