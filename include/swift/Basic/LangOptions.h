//===--- LangOptions.h - Language & configuration options -------*- C++ -*-===//
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
//  This file defines the LangOptions class, which provides various
//  language and configuration flags.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LANGOPTIONS_H
#define SWIFT_LANGOPTIONS_H

#include "llvm/ADT/StringMap.h"
#include <map>

namespace swift {
  
  /// \brief A collection of options that affect the language dialect and
  /// provide compiler debugging facilities.
  class LangOptions {
  public:
    
    /// \brief Whether we are debugging the constraint solver.
    ///
    /// This option enables verbose debugging output from the constraint
    /// solver.
    bool DebugConstraintSolver = false;

    /// \brief Specific solution attempt for which the constraint
    /// solver should be debugged.
    unsigned DebugConstraintSolverAttempt = 0;

    /// \brief Perform all dynamic allocations using malloc/free instead of
    /// optimized custom allocator, so that memory debugging tools can be used.
    bool UseMalloc = false;
    
    /// \brief Enable experimental "switch" pattern-matching features.
    bool EnableExperimentalPatterns = false;

    /// \brief Enable features useful for running in the debugger.
    bool DebuggerSupport = false;

    /// \brief Keep comments during lexing and attach them to declarations.
    bool AttachCommentsToDecls = false;
    
    /// \brief Implicit target configuration options.  There are currently two
    ///   supported target configuration values:
    ///     os - The active os target (OSX or IOS)
    ///     arch - The active arch target (X64, I386, ARM, ARM64)
    std::map<std::string, std::string> TargetConfigOptions;
    
    /// \brief Explicit build configuration options, initialized via the '-D'
    /// compiler flag.
    std::map<std::string, std::string> BuildConfigOptions;
    
    /// \brief A convenience method for determining if a given build
    /// configuration has been defined
    bool hasBuildConfig(llvm::StringRef name) {
      return BuildConfigOptions.count(name) != 0;
    }
  };
}

#endif // LLVM_SWIFT_LANGOPTIONS_H

