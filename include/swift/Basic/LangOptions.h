//===--- LangOptions.h - Language & configuration options -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
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

#include "swift/Config.h"
#include "swift/Basic/CycleDiagnosticKind.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Version.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/VersionTuple.h"
#include <string>
#include <vector>

namespace swift {

  /// Kind of implicit platform conditions.
  enum class PlatformConditionKind {
    /// The active os target (OSX, iOS, Linux, etc.)
    OS,
    /// The active arch target (x86_64, i386, arm, arm64, etc.)
    Arch,
    /// The active endianness target (big or little)
    Endianness,
    /// Runtime support (_ObjC or _Native)
    Runtime,
    /// Conditional import of module
    CanImport,
    /// Target Environment (currently just 'simulator' or absent)
    TargetEnvironment,
  };

  /// Describes which Swift 3 Objective-C inference warnings should be
  /// emitted.
  enum class Swift3ObjCInferenceWarnings {
    /// No warnings; this is the default.
    None,
    /// "Minimal" warnings driven by uses of declarations that make use of
    /// the Objective-C entry point directly.
    Minimal,
    /// "Complete" warnings that add "@objc" for every entry point that
    /// Swift 3 would have inferred as "@objc" but Swift 4 will not.
    Complete,
  };

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

    /// \brief Maximum number of typo corrections we are allowed to perform.
    unsigned TypoCorrectionLimit = 10;
    
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

    /// Whether to use heuristics to decide whether to show call-pattern
    /// completions.
    bool CodeCompleteCallPatternHeuristics = false;

    ///
    /// Flags for use by tests
    ///

    /// Enable Objective-C Runtime interop code generation and build
    /// configuration options.
    bool EnableObjCInterop = true;

    /// On Darwin platforms, use the pre-stable ABI's mark bit for Swift
    /// classes instead of the stable ABI's bit.
    bool UseDarwinPreStableABIBit = !bool(SWIFT_DARWIN_ENABLE_STABLE_ABI_BIT);

    /// Enables checking that uses of @objc require importing
    /// the Foundation module.
    /// This is enabled by default because SILGen can crash in such a case, but
    /// it gets disabled when compiling the Swift core stdlib.
    bool EnableObjCAttrRequiresFoundation = true;

    /// If true, <code>@testable import Foo</code> produces an error if \c Foo
    /// was not compiled with -enable-testing.
    bool EnableTestableAttrRequiresTestableModule = true;

    /// If true, the 'dynamic' attribute is added to all applicable
    /// declarations.
    bool EnableImplicitDynamic = false;

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

    /// \brief Enable named lazy member loading.
    bool NamedLazyMemberLoading = true;

    /// Debug the generic signatures computed by the generic signature builder.
    bool DebugGenericSignatures = false;

    /// Triggers llvm fatal_error if typechecker tries to typecheck a decl or an
    /// identifier reference with the provided prefix name.
    /// This is for testing purposes.
    std::string DebugForbidTypecheckPrefix;

    /// \brief How to diagnose cycles encountered
    CycleDiagnosticKind EvaluatorCycleDiagnostics =
        CycleDiagnosticKind::NoDiagnose;

    /// \brief The path to which we should emit GraphViz output for the complete
    /// request-evaluator graph.
    std::string RequestEvaluatorGraphVizPath;

    /// \brief The upper bound, in bytes, of temporary data that can be
    /// allocated by the constraint solver.
    unsigned SolverMemoryThreshold = 512 * 1024 * 1024;

    unsigned SolverBindingThreshold = 1024 * 1024;

    /// \brief The upper bound to number of sub-expressions unsolved
    /// before termination of the shrink phrase of the constraint solver.
    unsigned SolverShrinkUnsolvedThreshold = 10;

    /// Disable the shrink phase of the expression type checker.
    bool SolverDisableShrink = false;

    /// Disable constraint system performance hacks.
    bool DisableConstraintSolverPerformanceHacks = false;

    /// \brief Enable experimental operator designated types feature.
    bool EnableOperatorDesignatedTypes = false;

    /// \brief Enable constraint solver support for experimental
    ///        operator protocol designator feature.
    bool SolverEnableOperatorDesignatedTypes = false;

    /// The maximum depth to which to test decl circularity.
    unsigned MaxCircularityDepth = 500;

    /// \brief Perform all dynamic allocations using malloc/free instead of
    /// optimized custom allocator, so that memory debugging tools can be used.
    bool UseMalloc = false;

    /// \brief Enable experimental property behavior feature.
    bool EnableExperimentalPropertyBehaviors = false;

    /// \brief Staging flag for treating inout parameters as Thread Sanitizer
    /// accesses.
    bool DisableTsanInoutInstrumentation = false;

    /// Should we check the target OSs of serialized modules to see that they're
    /// new enough?
    bool EnableTargetOSChecking = true;

    /// Whether to attempt to recover from missing cross-references and other
    /// errors when deserializing from a Swift module.
    ///
    /// This is a staging flag; eventually it will be removed.
    bool EnableDeserializationRecovery = true;

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
    
    /// Enables key path resilience.
    bool EnableKeyPathResilience = true;

    /// If set to true, the diagnosis engine can assume the emitted diagnostics
    /// will be used in editor. This usually leads to more aggressive fixit.
    bool DiagnosticsEditorMode = false;

    /// Whether to enable Swift 3 @objc inference, e.g., for members of
    /// Objective-C-derived classes and 'dynamic' members.
    bool EnableSwift3ObjCInference = false;

    /// Warn about cases where Swift 3 would infer @objc but later versions
    /// of Swift do not.
    Swift3ObjCInferenceWarnings WarnSwift3ObjCInference =
      Swift3ObjCInferenceWarnings::None;

    /// Diagnose implicit 'override'.
    bool WarnImplicitOverrides = false;

    /// Diagnose uses of NSCoding with classes that have unstable mangled names.
    bool EnableNSKeyedArchiverDiagnostics = true;

    /// Diagnose switches over non-frozen enums that do not have catch-all
    /// cases.
    bool EnableNonFrozenEnumExhaustivityDiagnostics = false;

    /// Regex for the passes that should report passed and missed optimizations.
    ///
    /// These are shared_ptrs so that this class remains copyable.
    std::shared_ptr<llvm::Regex> OptimizationRemarkPassedPattern;
    std::shared_ptr<llvm::Regex> OptimizationRemarkMissedPattern;

    /// When a conversion from String to Substring fails, emit a fix-it to append
    /// the void subscript '[]'.
    /// FIXME: Remove this flag when void subscripts are implemented.
    /// This is used to guard preemptive testing for the fix-it.
    bool FixStringToSubstringConversions = false;

    /// Whether collect tokens during parsing for syntax coloring.
    bool CollectParsedToken = false;

    /// Whether to parse syntax tree. If the syntax tree is built, the generated
    /// AST may not be correct when syntax nodes are reused as part of
    /// incrementals parsing.
    bool BuildSyntaxTree = false;

    /// Whether to verify the parsed syntax tree and emit related diagnostics.
    bool VerifySyntaxTree = false;

    /// Scaffolding to permit experimentation with finer-grained dependencies
    /// and faster rebuilds.
    bool EnableExperimentalDependencies = false;

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
    llvm::VersionTuple getMinPlatformVersion() const {
      unsigned major, minor, revision;
      if (Target.isMacOSX()) {
        Target.getMacOSXVersion(major, minor, revision);
      } else if (Target.isiOS()) {
        Target.getiOSVersion(major, minor, revision);
      } else if (Target.isWatchOS()) {
        Target.getOSVersion(major, minor, revision);
      } else if (Target.isOSLinux() || Target.isOSFreeBSD() ||
                 Target.isAndroid() || Target.isOSWindows() ||
                 Target.isPS4() || Target.isOSHaiku() ||
                 Target.getTriple().empty()) {
        major = minor = revision = 0;
      } else {
        llvm_unreachable("Unsupported target OS");
      }
      return llvm::VersionTuple(major, minor, revision);
    }

    /// Sets an implicit platform condition.
    void addPlatformConditionValue(PlatformConditionKind Kind, StringRef Value) {
      assert(!Value.empty());
      PlatformConditionValues.emplace_back(Kind, Value);
    }

    /// Removes all values added with addPlatformConditionValue.
    void clearAllPlatformConditionValues() {
      PlatformConditionValues.clear();
    }
    
    /// Returns the value for the given platform condition or an empty string.
    StringRef getPlatformConditionValue(PlatformConditionKind Kind) const;

    /// Check whether the given platform condition matches the given value.
    bool checkPlatformCondition(PlatformConditionKind Kind, StringRef Value) const;

    /// Explicit conditional compilation flags, initialized via the '-D'
    /// compiler flag.
    void addCustomConditionalCompilationFlag(StringRef Name) {
      assert(!Name.empty());
      CustomConditionalCompilationFlags.push_back(Name);
    }

    /// Determines if a given conditional compilation flag has been set.
    bool isCustomConditionalCompilationFlagSet(StringRef Name) const;

    ArrayRef<std::pair<PlatformConditionKind, std::string>>
    getPlatformConditionValues() const {
      return PlatformConditionValues;
    }

    ArrayRef<std::string> getCustomConditionalCompilationFlags() const {
      return CustomConditionalCompilationFlags;
    }

    /// Whether our effective Swift version is at least 'major'.
    ///
    /// This is usually the check you want; for example, when introducing
    /// a new language feature which is only visible in Swift 5, you would
    /// check for isSwiftVersionAtLeast(5).
    bool isSwiftVersionAtLeast(unsigned major, unsigned minor = 0) const {
      return EffectiveLanguageVersion.isVersionAtLeast(major, minor);
    }

    /// Returns true if the given platform condition argument represents
    /// a supported target operating system.
    ///
    /// \param suggestions Populated with suggested replacements
    /// if a match is not found.
    static bool checkPlatformConditionSupported(
      PlatformConditionKind Kind, StringRef Value,
      std::vector<StringRef> &suggestions);

    /// Return a hash code of any components from these options that should
    /// contribute to a Swift Bridging PCH hash.
    llvm::hash_code getPCHHashComponents() const {
      auto code = llvm::hash_value(Target.str());
      SmallString<16> Scratch;
      llvm::raw_svector_ostream OS(Scratch);
      OS << EffectiveLanguageVersion;
      code = llvm::hash_combine(code, OS.str());
      return code;
    }

  private:
    llvm::SmallVector<std::pair<PlatformConditionKind, std::string>, 5>
        PlatformConditionValues;
    llvm::SmallVector<std::string, 2> CustomConditionalCompilationFlags;
  };
} // end namespace swift

#endif // SWIFT_BASIC_LANGOPTIONS_H
