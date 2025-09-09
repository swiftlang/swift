//===--- TargetInfo.cpp - Target information printing --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/TargetInfo.h"
#include "swift/Basic/Version.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Frontend/Frontend.h"

#include "clang/Basic/TargetInfo.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

/// Print information about a
static void printCompatibilityLibrary(
    llvm::VersionTuple runtimeVersion, llvm::VersionTuple maxVersion,
    StringRef filter, StringRef libraryName, bool forceLoad,
    bool &printedAny, llvm::raw_ostream &out) {
  if (runtimeVersion > maxVersion)
    return;

  if (printedAny) {
    out << ",";
  }

  out << "\n";
  out << "      {";

  out << "\n        \"libraryName\": \"";
  swift::writeEscaped(libraryName, out);
  out << "\",";

  out << "\n        \"filter\": \"";
  swift::writeEscaped(filter, out);
  out << "\"";

  if (!forceLoad) {
    out << ",\n        \"forceLoad\": false";
  }

  out << "\n      }";

  printedAny = true;
}

namespace swift {
namespace targetinfo {
/// Print information about the selected target in JSON.
void printTargetInfo(const CompilerInvocation &invocation,
                     llvm::raw_ostream &out) {
  out << "{\n";

  // Compiler version, as produced by --version.
  out << "  \"compilerVersion\": \"";
  writeEscaped(version::getSwiftFullVersion(version::Version::getCurrentLanguageVersion()), out);
  out << "\",\n";

  // Distribution tag, if any.
  StringRef tag = version::getCurrentCompilerTag();
  if (!tag.empty()) {
    out << "  \"swiftCompilerTag\": \"";
    writeEscaped(tag, out);
    out << "\",\n";
  }

  // Target triple and target variant triple.
  auto runtimeVersion =
    invocation.getIRGenOptions().AutolinkRuntimeCompatibilityLibraryVersion;
  auto &langOpts = invocation.getLangOptions();
  out << "  \"target\": ";
  printTripleInfo(invocation, langOpts.Target, runtimeVersion, out);
  out << ",\n";

  if (auto &variant = langOpts.TargetVariant) {
    out << "  \"targetVariant\": ";
    printTripleInfo(invocation, *variant, runtimeVersion, out);
    out << ",\n";
  }

  // Various paths.
  auto &searchOpts = invocation.getSearchPathOptions();
  out << "  \"paths\": {\n";

  if (!searchOpts.getSDKPath().empty()) {
    out << "    \"sdkPath\": \"";
    writeEscaped(searchOpts.getSDKPath(), out);
    out << "\",\n";
  }

  auto outputPaths = [&](StringRef name, const std::vector<std::string> &paths){
    out << "    \"" << name << "\": [\n";
    llvm::interleave(paths, [&out](const std::string &path) {
      out << "      \"";
      writeEscaped(path, out);
      out << "\"";
    }, [&out] {
      out << ",\n";
    });
    out << "\n    ],\n";
  };

  outputPaths("runtimeLibraryPaths", searchOpts.RuntimeLibraryPaths);
  outputPaths("runtimeLibraryImportPaths",
              searchOpts.getRuntimeLibraryImportPaths());

  out << "    \"runtimeResourcePath\": \"";
  writeEscaped(searchOpts.RuntimeResourcePath, out);
  out << "\"\n";

  out << "  }\n";

  out << "}\n";
}

// Print information about the target triple in JSON.
void printTripleInfo(const CompilerInvocation &invocation,
                     const llvm::Triple &triple,
                     std::optional<llvm::VersionTuple> runtimeVersion,
                     llvm::raw_ostream &out) {
  out << "{\n";

  out << "    \"triple\": \"";
  writeEscaped(triple.getTriple(), out);
  out << "\",\n";

  out << "    \"unversionedTriple\": \"";
  writeEscaped(getUnversionedTriple(triple).getTriple(), out);
  out << "\",\n";

  out << "    \"moduleTriple\": \"";
  writeEscaped(getTargetSpecificModuleTriple(triple).getTriple(), out);
  out << "\",\n";

  out << "    \"platform\": \"" << getPlatformNameForTriple(triple) << "\",\n";
  out << "    \"arch\": \"" << swift::getMajorArchitectureName(triple)
      << "\",\n";

  clang::DiagnosticsEngine DE{new clang::DiagnosticIDs(),
                              new clang::DiagnosticOptions(),
                              new clang::IgnoringDiagConsumer()};
  std::shared_ptr<clang::TargetOptions> TO =
      std::make_shared<clang::TargetOptions>();
  TO->Triple = triple.str();
  clang::TargetInfo *TI = clang::TargetInfo::CreateTargetInfo(DE, TO);
  out << "    \"pointerWidthInBits\": "
      << TI->getPointerWidth(clang::LangAS::Default) << ",\n";
  out << "    \"pointerWidthInBytes\": "
      << TI->getPointerWidth(clang::LangAS::Default) / TI->getCharWidth()
      << ",\n";

  if (runtimeVersion) {
    out << "    \"swiftRuntimeCompatibilityVersion\": \"";
    writeEscaped(runtimeVersion->getAsString(), out);
    out << "\",\n";

    // Compatibility libraries that need to be linked.
    out << "    \"compatibilityLibraries\": [";
    bool printedAnyCompatibilityLibrary = false;
#define BACK_DEPLOYMENT_LIB(Version, Filter, LibraryName, ForceLoad)           \
  printCompatibilityLibrary(*runtimeVersion, llvm::VersionTuple Version,       \
                            #Filter, LibraryName, ForceLoad,                   \
                            printedAnyCompatibilityLibrary, out);
#include "swift/Frontend/BackDeploymentLibs.def"

    if (printedAnyCompatibilityLibrary)
      out << "\n   ";
    out << " ],\n";
  } else {
    out << "    \"compatibilityLibraries\": [ ],\n";
  }

  if (tripleBTCFIByDefaultInOpenBSD(triple)) {
#if SWIFT_OPENBSD_BTCFI
     out << "    \"openbsdBTCFIEnabled\": true,\n";
#else
     out << "    \"openbsdBTCFIEnabled\": false,\n";
#endif
  } else {
     out << "    \"openbsdBTCFIEnabled\": false,\n";
  }

  out << "    \"librariesRequireRPath\": "
      << (tripleRequiresRPathForSwiftLibrariesInOS(triple) ? "true" : "false")
      << "\n";

  out << "  }";
}
} // namespace targetinfo
} // namespace swift
