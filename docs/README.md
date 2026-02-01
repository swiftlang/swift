# Documentation Index

This page contains a curated index of the documentation found in the Swift
compiler repository.

Sometimes documentation is not enough.
Especially if you are a new contributor, you might run into roadblocks
which are not addressed by the existing documentation.
If you are stuck, please use the [development category][] on the Swift forums
to ask for help!

[development category]: https://forums.swift.org/c/development

## Building the Toolchain

- [GettingStarted.md](/docs/HowToGuides/GettingStarted.md):
  Describes how to set up a working Swift development environment
  for Linux and macOS, and get an edit-build-test-debug loop going.
- Building for Android:
  - [Android.md](/docs/Android.md):
    How to run some simple programs and the Swift test suite on an Android device.
  - [AndroidBuild.md](/docs/AndroidBuild.md):
    How to build the Swift SDK for Android on Windows.
- Building for Windows:
  - [WindowsBuild.md](/docs/WindowsBuild.md):
    How to build Swift on Windows using Visual Studio.
- Building for OpenBSD:
  - [OpenBSD.md](/docs/OpenBSD.md):
    Overview of specific steps for building on OpenBSD.

## Contributing

- [FirstPullRequest.md](/docs/HowToGuides/FirstPullRequest.md):
  Describes how to submit your first pull request. This is the place to start
  if you're new to the project!
- [Branches.md](/docs/Branches.md):
  Describes how different branches are setup and what the automerger does.
- [ContinuousIntegration.md](/docs/ContinuousIntegration.md):
  Describes the continuous integration setup, including the `@swift_ci` bot.
- [DebuggingTheCompiler.md](/docs/DebuggingTheCompiler.md):
  Describes a variety of techniques for debugging.
- [FAQ.md](/docs/HowToGuides/FAQ.md):
  Answers "How do I do X?" for a variety of common tasks.
- [Lexicon.md](/docs/Lexicon.md):
  Canonical reference for terminology used throughout the project.
- [Testing.md](/docs/Testing.md):
  Information about running and developing tests in the compiler's test suite.

## Reference Guides

- [Backtracing.rst](/docs/Backtracing.rst):
  Describes Swift's backtracing and crash reporting support.
- [CompilerPerformance.md](/docs/CompilerPerformance.md):
  Thoroughly discusses different ways of measuring compiler performance
  and common pitfalls.
- [Generics/](/docs/Generics/): "Compiling Swift Generics", a book about
  the implementation of parameteric polymorphism in the Swift compiler.
  Also covers the compilation pipeline, request evaluator, and type system in
  general.
- [LibraryEvolution.rst](/docs/LibraryEvolution.rst):
  Specifies what changes can be made without breaking binary compatibility.
- [RequestEvaluator.md](/docs/RequestEvaluator.md):
  Describes the request evaluator architecture, which is used for
  lazy type-checking and efficient caching.
- [SIL/](/docs/SIL/): Documentation about SIL, the Swift intermediate language.
- [TypeChecker.md](/docs/TypeChecker.md):
  Provides an overview of the expression type checker.
- [UnderscoredAttributes.md](/docs/ReferenceGuides/UnderscoredAttributes.md):
  Documents semantics for underscored (unstable) attributes in the language.

## The Swift ABI

- [CallingConventionSummary.rst](/docs/ABI/CallingConventionSummary.rst):
  A concise summary of the calling conventions used for C/C++, Objective-C
  and Swift on Apple platforms.  Contains references to source documents,
  where further detail is required.
- [CallingConvention.rst](/docs/ABI/CallingConvention.rst):
  This whitepaper discusses the Swift calling convention (high-level semantics;
  ownership transfer; physical representation; function signature lowering).
- [KeyPaths.md](/docs/ABI/KeyPaths.md):
  Describes the layout of key path objects (instantiated by the runtime,
  and therefore not strictly ABI). \
  **TODO:** The layout of key path patterns (emitted by the compiler,
  to represent key path literals) isn't documented yet.
- [Mangling.rst](/docs/ABI/Mangling.rst):
  Describes the stable mangling scheme, which produces unique symbols for
  ABI-public declarations.
- [TypeLayout.rst](/docs/ABI/TypeLayout.rst):
  Describes the algorithms/strategies for fragile struct and tuple layout;
  class layout; fragile enum layout; and existential container layout.
- [TypeMetadata.rst](/docs/ABI/TypeMetadata.rst):
  Describes the fields, values, and layout of metadata records, which can be
  used (by reflection and debugger tools) to discover information about types.

## SIL Optimizer

- [OptimizerDesign.md](/docs/OptimizerDesign.md):
  Describes the design of the optimizer pipeline.
- [HighLevelSILOptimizations.rst](/docs/HighLevelSILOptimizations.rst):
  Describes how the optimizer understands the semantics of high-level
  operations on [currency](/docs/Lexicon.md#currency-type) data types and
  optimizes accordingly.
  Includes a thorough discussion of the `@_semantics` attribute.
- [HowToUpdateDebugInfo.md](/docs/HowToUpdateDebugInfo.md): A guide for SIL
  optimization pass authors for how to properly update debug info in SIL
  program transformations.
- [OptimizerCountersAnalysis.md](/docs/OptimizerCountersAnalysis.md):
  TODO: Consider breaking up into a how-to guide
  on dumping and analyzing the counters
  and an explanation for the counter collection system.
- [TransparentAttr.md](/docs/TransparentAttr.md):
  Documents the semantics of the `@_transparent` attribute.

## Swift Driver

- [Driver.md](/docs/Driver.md):
  Provides an overview of the driver, compilation model, and the compiler's
  command-line options. Useful for integration into build systems other than
  SwiftPM or Xcode.
- [DriverInternals.md](/docs/DriverInternals.md):
  Provides a bird's eye view of the driver's implementation.
  <!-- NOTE: Outdated -->
- [DriverParseableOutput.md](/docs/DriverParseableOutput.md):
  Describes the output format of the driver's `-parseable-output` flag,
  which is suitable for consumption by editors and IDEs.
- [DependencyAnalysis.md](/docs/DependencyAnalysis.md):
  Describes different kinds of dependencies across files in the same module,
  important for understanding incremental builds.

## Objective-C Interop

- [ObjCInterop.md](/docs/ObjCInterop.md)
  Documents how Swift interoperates with ObjC code and the ObjC runtime.
- [CToSwiftNameTranslation.md](/docs/CToSwiftNameTranslation.md):
  Describes how C and ObjC entities are imported into Swift
  by the Clang Importer.
- [CToSwiftNameTranslation-OmitNeedlessWords.md](/docs/CToSwiftNameTranslation-OmitNeedlessWords.md):
  Describes how the "Omit Needless Words" algorithm works,
  making imported names more idiomatic.
- [HowSwiftImportsCAPIs.md](/docs/HowSwiftImportsCAPIs.md):
  Contains a thorough description of the mapping between C/ObjC entities and
  Swift entities.

## Rationales and Manifestos

- [ABIStabilityManifesto.md](/docs/ABIStabilityManifesto.md):
  Describes the goals and design for ABI stability.
- [LibraryEvolutionManifesto.md](/docs/LibraryEvolutionManifesto.md):
  Describes the goals and design for Library Evolution.
- [BuildManifesto.md](/docs/BuildManifesto.md):
  Provides an outline for modularizing the build system for the Swift toolchain.
- [CppInteroperabilityManifesto.md](/docs/CppInteroperability/CppInteroperabilityManifesto.md):
  Describes the motivation and design for first-class Swift-C++ interoperability.
- [DifferentiableProgramming.md](/docs/DifferentiableProgramming.md):
  Outlines a vision and design for first-class differentiable programming in Swift.
- [ErrorHandlingRationale.md](/docs/ErrorHandlingRationale.md):
  Surveys error-handling in a variety of languages, and describes the rationale
  behind the design of error handling in Swift.
- [OwnershipManifesto.md](/docs/OwnershipManifesto.md):
  Provides a framework for understanding ownership in Swift,
  and highlights potential future directions for the language.
- [SequencesAndCollections.rst](/docs/SequencesAndCollections.rst):
  Provides background on the design of different collection-related protocols.
- [StdlibRationales.rst](/docs/StdlibRationales.rst):
  Provides rationale for common questions/complaints regarding design decisions
  in the Swift stdlib.
- [StringManifesto.md](/docs/StringManifesto.md):
  Provides a long-term vision for the `String` type.
- [WeakReferences.md](/docs/WeakReferences.md):
  Discusses weak references, including the designs in different languages,
  and proposes changes to Swift (pre-1.0).
  <!-- NOTE: Outdated -->

## Uncategorized

- [Diagnostics.md](/docs/Diagnostics.md):
  Describes how to write diagnostic messages and associated documentation.
- [DifferentiableProgrammingImplementation.md](/docs/DifferentiableProgrammingImplementation.md):
  Describes how automatic differentiation is implemented in the Swift compiler.
- [DocumentationComments.md](/docs/DocumentationComments.md):
  Describes the format of Swift's documentation markup, including
  specially-recognized sections.
- [ExternalResources.md](/docs/ExternalResources.md): Contains links to various
  materials found outside of the Swift repository.
- [Literals.md](/docs/Literals.md):
  Describes type-checking and inference specifically for literals.
- [Serialization.md](/docs/Serialization.md):
  Gives an overview of the LLVM bitcode format used for swiftmodules.
  - [StableBitcode.md](/docs/StableBitcode.md):
    Describes how to maintain compatibility when changing the serialization
    format.
- [SwiftLocalRefactoring.md](/docs/refactoring/SwiftLocalRefactoring.md):
  Describes how refactorings work and how they can be tested.
- [RunningIncludeWhatYouUse.md](/docs/HowToGuides/RunningIncludeWhatYouUse.md):
  Describes how to run [include-what-you-use](https://include-what-you-use.org)
  on the Swift project.
- [libFuzzerIntegration.md](/docs/libFuzzerIntegration.md):
  Using `libFuzzer` to fuzz Swift code.
- [DynamicCasting.md](/docs/DynamicCasting.md):
  Behavior of the dynamic casting operators `is`, `as?`, and `as!`.
- [WebAssembly.md](/docs/WebAssembly.md):
  Explains some decisions that were made while implementing the WebAssembly target.
