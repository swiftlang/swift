//===--- LangOptions.h - Language & configuration options -------*- C++ -*-===//
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
//
//  This file defines the LangOptions class, which provides various
//  language and configuration flags.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_LANGOPTIONS_H
#define SWIFT_BASIC_LANGOPTIONS_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Version.h"
#include "clang/Basic/VersionTuple.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include <string>
#include <vector>

namespace swift {
  /// \brief A collection of options that affect the language dialect and
  /// provide compiler debugging facilities.
  class LangOptions {
  public:

    /// \brief The target we are building for.
    ///
    /// This represents the minimum deployment target.
    llvm::Triple Target;

    ///
    /// Language features
    ///

    /// \brief User-overridable language version to compile for.
    version::Version EffectiveLanguageVersion = version::Version::getCurrentLanguageVersion();

    /// \brief Disable API availability checking.
    bool DisableAvailabilityChecking = false;

    /// \brief Disable typo correction.
    bool DisableTypoCorrection = false;
    
    /// Should access control be respected?
    bool EnableAccessControl = true;

    /// Enable 'availability' restrictions for App Extensions.
    bool EnableAppExtensionRestrictions = false;

    ///
    /// Support for alternate usage modes
    ///

    /// \brief Enable features useful for running in the debugger.
    bool DebuggerSupport = false;

    /// Allows using identifiers with a leading dollar.
    bool EnableDollarIdentifiers = false;

    /// \brief Allow throwing call expressions without annotation with 'try'.
    bool EnableThrowWithoutTry = false;

    /// \brief Enable features useful for running playgrounds.
    // FIXME: This should probably be limited to the particular SourceFile.
    bool Playground = false;

    /// \brief Keep comments during lexing and attach them to declarations.
    bool AttachCommentsToDecls = false;

    /// Whether to include initializers when code-completing a postfix
    /// expression.
    bool CodeCompleteInitsInPostfixExpr = false;

    ///
    /// Flags for use by tests
    ///

    /// Enable Objective-C Runtime interop code generation and build
    /// configuration options.
    bool EnableObjCInterop = true;

    /// Enables checking that uses of @objc require importing
    /// the Foundation module.
    /// This is enabled by default because SILGen can crash in such a case, but
    /// it gets disabled when compiling the Swift core stdlib.
    bool EnableObjCAttrRequiresFoundation = true;

    /// If true, <code>@testable import Foo</code> produces an error if \c Foo
    /// was not compiled with -enable-testing.
    bool EnableTestableAttrRequiresTestableModule = true;

    ///
    /// Flags for developers
    ///

    /// \brief Whether we are debugging the constraint solver.
    ///
    /// This option enables verbose debugging output from the constraint
    /// solver.
    bool DebugConstraintSolver = false;

    /// \brief Specific solution attempt for which the constraint
    /// solver should be debugged.
    unsigned DebugConstraintSolverAttempt = 0;

    /// \brief Enable the iterative type checker.
    bool IterativeTypeChecker = false;

    /// Debug the generic signatures computed by the archetype builder.
    bool DebugGenericSignatures = false;

    /// Triggers llvm fatal_error if typechecker tries to typecheck a decl or an
    /// identifier reference with the provided prefix name.
    /// This is for testing purposes.
    std::string DebugForbidTypecheckPrefix;

    /// Number of parallel processes performing AST verification.
    unsigned ASTVerifierProcessCount = 1U;

    /// ID of the current process for the purposes of AST verification.
    unsigned ASTVerifierProcessId = 1U;

    /// \brief The upper bound, in bytes, of temporary data that can be
    /// allocated by the constraint solver.
    unsigned SolverMemoryThreshold = 512 * 1024 * 1024;

    unsigned SolverBindingThreshold = 1024 * 1024;

    /// \brief Perform all dynamic allocations using malloc/free instead of
    /// optimized custom allocator, so that memory debugging tools can be used.
    bool UseMalloc = false;

    /// \brief Enable experimental property behavior feature.
    bool EnableExperimentalPropertyBehaviors = false;

    /// \brief Staging flag for class resilience, which we do not want to enable
    /// fully until more code is in place, to allow the standard library to be
    /// tested with value type resilience only.
    bool EnableClassResilience = false;

    /// Should we check the target OSs of serialized modules to see that they're
    /// new enough?
    bool EnableTargetOSChecking = true;

    /// Should we use \c ASTScope-based resolution for unqualified name lookup?
    bool EnableASTScopeLookup = false;

    /// Whether to use the import as member inference system
    ///
    /// When importing a global, try to infer whether we can import it as a
    /// member of some type instead. This includes inits, computed properties,
    /// and methods.
    bool InferImportAsMember = false;

    /// If set to true, compile with the SIL Opaque Values enabled.
    /// This is for bootstrapping. It can't be in SILOptions because the
    /// TypeChecker uses it to set resolve the ParameterConvention.
    bool EnableSILOpaqueValues = false;

    /// Sets the target we are building for and updates platform conditions
    /// to match.
    ///
    /// \returns A pair - the first element is true if the OS was invalid.
    /// The second element is true if the Arch was invalid.
    std::pair<bool, bool> setTarget(llvm::Triple triple);

    /// Returns the minimum platform version to which code will be deployed.
    ///
    /// This is only implemented on certain OSs. If no target has been
    /// configured, returns v0.0.0.
    clang::VersionTuple getMinPlatformVersion() const {
      unsigned major, minor, revision;
      if (Target.isMacOSX()) {
        Target.getMacOSXVersion(major, minor, revision);
      } else if (Target.isiOS()) {
        Target.getiOSVersion(major, minor, revision);
      } else if (Target.isWatchOS()) {
        Target.getOSVersion(major, minor, revision);
      } else if (Target.isOSLinux() || Target.isOSFreeBSD() ||
                 Target.isAndroid() || Target.isOSWindows() ||
                 Target.isPS4() || Target.getTriple().empty()) {
        major = minor = revision = 0;
      } else {
        llvm_unreachable("Unsupported target OS");
      }
      return clang::VersionTuple(major, minor, revision);
    }

    /// Sets an implicit platform condition.
    ///
    /// There are currently three supported platform conditions:
    /// - os: The active os target (OSX or iOS)
    /// - arch: The active arch target (x86_64, i386, arm, arm64)
    /// - _runtime: Runtime support (_ObjC or _Native)
    void addPlatformConditionValue(StringRef Name, StringRef Value) {
      assert(!Name.empty() && !Value.empty());
      PlatformConditionValues.emplace_back(Name, Value);
    }

    /// Removes all values added with addPlatformConditionValue.
    void clearAllPlatformConditionValues() {
      PlatformConditionValues.clear();
    }
    
    /// Returns the value for the given platform condition or an empty string.
    StringRef getPlatformConditionValue(StringRef Name) const;

    /// Explicit conditional compilation flags, initialized via the '-D'
    /// compiler flag.
    void addCustomConditionalCompilationFlag(StringRef Name) {
      assert(!Name.empty());
      CustomConditionalCompilationFlags.push_back(Name);
    }

    /// Determines if a given conditional compilation flag has been set.
    bool isCustomConditionalCompilationFlagSet(StringRef Name) const;

    ArrayRef<std::pair<std::string, std::string>>
    getPlatformConditionValues() const {
      return PlatformConditionValues;
    }

    ArrayRef<std::string> getCustomConditionalCompilationFlags() const {
      return CustomConditionalCompilationFlags;
    }

    /// Whether our effective Swift version is in the Swift 3 family
    bool isSwiftVersion3() const {
      return EffectiveLanguageVersion.isVersion3();
    }

    /// Returns true if the 'os' platform condition argument represents
    /// a supported target operating system.
    ///
    /// Note that this also canonicalizes the OS name if the check returns
    /// true.
    ///
    /// \param suggestions Populated with suggested replacements
    /// if a match is not found.
    static bool checkPlatformConditionOS(
      StringRef &OSName, std::vector<StringRef> &suggestions);

    /// Returns true if the 'arch' platform condition argument represents
    /// a supported target architecture.
    ///
    /// \param suggestions Populated with suggested replacements
    /// if a match is not found.
    static bool isPlatformConditionArchSupported(
      StringRef ArchName, std::vector<StringRef> &suggestions);

    /// Returns true if the 'endian' platform condition argument represents
    /// a supported target endianness.
    ///
    /// \param suggestions Populated with suggested replacements
    /// if a match is not found.
    static bool isPlatformConditionEndiannessSupported(
      StringRef endianness, std::vector<StringRef> &suggestions);

  private:
    llvm::SmallVector<std::pair<std::string, std::string>, 3>
        PlatformConditionValues;
    llvm::SmallVector<std::string, 2> CustomConditionalCompilationFlags;
  };
} // end namespace swift

#endif // SWIFT_BASIC_LANGOPTIONS_H
