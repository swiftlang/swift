// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -scan-dependencies -module-name Client -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx13.0 -experimental-clang-codegen-dependency Versioned -I %t -module-cache-path %t/cache %t/client.swift -o %t/older.json
// RUN: %FileCheck %s --check-prefix=OLDER --implicit-check-not=HighSupport --implicit-check-not=MissingSwiftModule --implicit-check-not='"swift": "Swift"' < %t/older.json
// RUN: %target-swift-frontend -scan-dependencies -module-name Client -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx15.0 -experimental-clang-codegen-dependency Versioned -I %t -module-cache-path %t/cache %t/client.swift -o %t/newer.json
// RUN: %FileCheck %s --check-prefix=NEWER --implicit-check-not=OldSupport --implicit-check-not=MissingSwiftModule --implicit-check-not='"swift": "Swift"' < %t/newer.json

// The second scan discovers dependencies from the deployment-target headers.
// Swift source imports and overlays are already handled by the semantic scan.
// OLDER: "clang": "OldSupport"
// NEWER: "clang": "HighSupport"

//--- client.swift
import MissingSwiftModule

//--- module.modulemap
module Versioned { header "Versioned.h" export * }
module HighSupport { header "HighSupport.h" export * }
module OldSupport { header "OldSupport.h" export * }

//--- Versioned.h
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 150000
#include "HighSupport.h"
#else
#include "OldSupport.h"
#endif

//--- HighSupport.h
int newerRuntime(void);

//--- OldSupport.h
int olderRuntime(void);
