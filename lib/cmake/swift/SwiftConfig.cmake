# This file provides information and services to the final user.



set(SWIFT_VERSION 5.6)
set(SWIFT_MAIN_SRC_DIR /Users/ktoso/code/swift-project/swift)

set(SWIFT_INCLUDE_DIRS "/Users/ktoso/code/swift-project/swift/include;/Users/ktoso/code/swift-project/swift/include")
set(SWIFT_LIBRARY_DIRS "/Users/ktoso/code/swift-project/swift/./lib")

# These variables are duplicated, but they must match the LLVM variables of the
# same name. The variables ending in "S" could some day become lists, and are
# preserved for convention and compatibility.
set(SWIFT_INCLUDE_DIR "/Users/ktoso/code/swift-project/swift/include;/Users/ktoso/code/swift-project/swift/include")
set(SWIFT_LIBRARY_DIR "/Users/ktoso/code/swift-project/swift/./lib")

set(SWIFT_CMAKE_DIR "/Users/ktoso/code/swift-project/swift/cmake/modules")
set(SWIFT_BINARY_DIR "/Users/ktoso/code/swift-project/swift")

set(BOOTSTRAPPING_MODE "OFF")

set(CMARK_TARGETS_FILE /Users/ktoso/code/swift-project/build/Ninja-RelWithDebInfoAssert/cmark-macosx-arm64/src/cmarkTargets.cmake)
if(NOT TARGET libcmark_static AND EXISTS ${CMARK_TARGETS_FILE})
  include(${CMARK_TARGETS_FILE})
endif()

if(NOT TARGET swift)
  set(SWIFT_EXPORTED_TARGETS "swiftCompatibility50-macosx-x86_64;swiftCompatibility50-macosx-arm64;swiftCompatibility51-macosx-x86_64;swiftCompatibility51-macosx-arm64;swiftCompatibilityDynamicReplacements-macosx-x86_64;swiftCompatibilityDynamicReplacements-macosx-arm64;swiftCompatibilityConcurrency-macosx-x86_64;swiftCompatibilityConcurrency-macosx-arm64;swiftCore-macosx-x86_64;swiftCore-macosx-arm64;swiftSwiftOnoneSupport-macosx-x86_64;swiftSwiftOnoneSupport-macosx-arm64;swiftRemoteMirror-macosx-x86_64;swiftRemoteMirror-macosx-arm64;swiftDarwin-macosx-x86_64;swiftDarwin-macosx-arm64;swift-frontend;sil-opt;swift-dependency-tool;swift-demangle;swift-demangle-yamldump;swift-def-to-yaml-converter;swift-serialize-diagnostics;sil-func-extractor;sil-llvm-gen;sil-nm;sil-passpipeline-dumper;swift-llvm-opt;swift-ast-script;swift-refactor;swift-syntax-parser-test;swift-ide-test;swift-remoteast-test;lldb-moduleimport-test;swift-syntax-test;sourcekitd;swift-stdlib-tool;swift-reflection-dump;swiftCommandLineSupport-macosx-x86_64;swiftCommandLineSupport-macosx-arm64;swiftReflection-macosx-x86_64;swiftReflection-macosx-arm64;swiftSwiftPrivate-macosx-x86_64;swiftSwiftPrivate-macosx-arm64;swiftRuntimeUnittest-macosx-x86_64;swiftRuntimeUnittest-macosx-arm64;swiftStdlibUnicodeUnittest-macosx-x86_64;swiftStdlibUnicodeUnittest-macosx-arm64;swiftStdlibCollectionUnittest-macosx-x86_64;swiftStdlibCollectionUnittest-macosx-arm64;swiftSwiftPrivateLibcExtras-macosx-x86_64;swiftSwiftPrivateLibcExtras-macosx-arm64;swiftSwiftPrivateThreadExtras-macosx-x86_64;swiftSwiftPrivateThreadExtras-macosx-arm64;swiftStdlibUnittest-macosx-x86_64;swiftStdlibUnittest-macosx-arm64;swiftOSLogTestHelper-macosx-x86_64;swiftOSLogTestHelper-macosx-arm64;swiftStdlibUnittestFoundationExtras-macosx-x86_64;swiftStdlibUnittestFoundationExtras-macosx-arm64;swiftSwiftReflectionTest-macosx-x86_64;swiftSwiftReflectionTest-macosx-arm64;swiftAPIDigester;swiftAST;swiftASTSectionImporter;swiftBasic;swiftClangImporter;swiftDemangling;swiftDependencyScan;swiftDriver;swiftDriverTool;swiftFrontend;swiftFrontendTool;swiftIndex;swiftIDE;swiftImmediate;swiftIRGen;swiftLLVMPasses;swiftLocalization;swiftMarkup;swiftMigrator;swiftOption;swiftParse;swiftPrintAsObjC;swiftRemoteAST;swiftSema;swiftSerialization;swiftDemangle;swiftReflection;swiftRemoteMirror;swiftSIL;swiftSILGen;swiftSILOptimizer;swiftSymbolGraphGen;swiftSyntax;swiftSyntaxParse;swiftTBDGen;swiftCompilerStub;libSwiftScan;libSwiftSyntaxParser;SourceKitCore;SourceKitSwiftLang;SourceKitSupport;sourcekitdAPI;sourcekitdInProc")
  include("/Users/ktoso/code/swift-project/swift/lib/cmake/swift/SwiftExports.cmake")
endif()
