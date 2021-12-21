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
#include "swift/Frontend/Frontend.h"

#include "llvm/Support/raw_ostream.h"

using namespace swift;

/// Print information about a
static void printCompatibilityLibrary(
    llvm::VersionTuple runtimeVersion, llvm::VersionTuple maxVersion,
    StringRef filter, StringRef libraryName, bool &printedAny,
    llvm::raw_ostream &out) {
  if (runtimeVersion > maxVersion)
    return;

  if (printedAny) {
    out << ",";
  }

  out << "\n";
  out << "      {\n";

  out << "        \"libraryName\": \"";
  out.write_escaped(libraryName);
  out << "\",\n";

  out << "        \"filter\": \"";
  out.write_escaped(filter);
  out << "\"\n";
  out << "      }";

  printedAny = true;
}

/// Print information about the selected target in JSON.
void targetinfo::printTargetInfo(const CompilerInvocation &invocation,
                                 llvm::raw_ostream &out) {
  out << "{\n";

  // Compiler version, as produced by --version.
  out << "  \"compilerVersion\": \"";
  out.write_escaped(version::getSwiftFullVersion(
                                                 version::Version::getCurrentLanguageVersion()));
  out << "\",\n";

  // Target triple and target variant triple.
  auto runtimeVersion =
    invocation.getIRGenOptions().AutolinkRuntimeCompatibilityLibraryVersion;
  auto &langOpts = invocation.getLangOptions();
  out << "  \"target\": ";
  printTripleInfo(langOpts.Target, runtimeVersion, out);
  out << ",\n";

  if (auto &variant = langOpts.TargetVariant) {
    out << "  \"targetVariant\": ";
    printTripleInfo(*variant, runtimeVersion, out);
    out << ",\n";
  }

  // Various paths.
  auto &searchOpts = invocation.getSearchPathOptions();
  out << "  \"paths\": {\n";

  if (!searchOpts.SDKPath.empty()) {
    out << "    \"sdkPath\": \"";
    out.write_escaped(searchOpts.SDKPath);
    out << "\",\n";
  }

  auto outputPaths = [&](StringRef name, const std::vector<std::string> &paths){
    out << "    \"" << name << "\": [\n";
    llvm::interleave(paths, [&out](const std::string &path) {
      out << "      \"";
      out.write_escaped(path);
      out << "\"";
    }, [&out] {
      out << ",\n";
    });
    out << "\n    ],\n";
  };

  outputPaths("runtimeLibraryPaths", searchOpts.RuntimeLibraryPaths);
  outputPaths("runtimeLibraryImportPaths",
              searchOpts.RuntimeLibraryImportPaths);

  out << "    \"runtimeResourcePath\": \"";
  out.write_escaped(searchOpts.RuntimeResourcePath);
  out << "\"\n";

  out << "  }\n";

  out << "}\n";
}

// Print information about the target triple in JSON.
void targetinfo::printTripleInfo(const llvm::Triple &triple,
                                 llvm::Optional<llvm::VersionTuple> runtimeVersion,
                                 llvm::raw_ostream &out) {
  out << "{\n";

  out << "    \"triple\": \"";
  out.write_escaped(triple.getTriple());
  out << "\",\n";

  out << "    \"unversionedTriple\": \"";
  out.write_escaped(getUnversionedTriple(triple).getTriple());
  out << "\",\n";

  out << "    \"moduleTriple\": \"";
  out.write_escaped(getTargetSpecificModuleTriple(triple).getTriple());
  out << "\",\n";

  if (runtimeVersion) {
    out << "    \"swiftRuntimeCompatibilityVersion\": \"";
    out.write_escaped(runtimeVersion->getAsString());
    out << "\",\n";

    // Compatibility libraries that need to be linked.
    out << "    \"compatibilityLibraries\": [";
    bool printedAnyCompatibilityLibrary = false;
    #define BACK_DEPLOYMENT_LIB(Version, Filter, LibraryName)           \
      printCompatibilityLibrary(                                        \
        *runtimeVersion, llvm::VersionTuple Version, #Filter, LibraryName, \
        printedAnyCompatibilityLibrary, out);
    #include "swift/Frontend/BackDeploymentLibs.def"

    if (printedAnyCompatibilityLibrary) {
      out << "\n   ";
    }
    out << " ],\n";
  } else {
    out << "    \"compatibilityLibraries\": [ ],\n";
  }

  out << "    \"librariesRequireRPath\": "
      << (tripleRequiresRPathForSwiftLibrariesInOS(triple) ? "true" : "false")
      << "\n";

  out << "  }";
}
