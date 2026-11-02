// The Windows SDK installs its headers under `...\Windows Kits\10\Include\...`,
// and Swift puts the WinSDK module map and API notes there. `VsDevCmd.bat`
// writes the same directory into %INCLUDE% in lowercase: `...\include\...`.
// Under caching the API notes must still apply to the module's macros.

// REQUIRES: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -Xcc -fmodule-map-file=%t/sdk/Include/10.0/module.modulemap \
// RUN:   -Xcc -isystem -Xcc %t/sdk/include/10.0/um \
// RUN:   -Xcc -isystem -Xcc %t/sdk/include/10.0/shared

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:WinBase > %t/WinBase.cmd
// RUN: %swift_frontend_plain @%t/WinBase.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %target-swift-frontend-plain -module-name Test \
// RUN:   -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift @%t/MyApp.cmd -swift-version 5 \
// RUN:   -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -typecheck

//--- main.swift
import WinBase

func startupFlags() -> DWORD {
  // Without the API notes the macro is imported as CInt.
  return STARTF_USESTDHANDLES
}

//--- sdk/Include/10.0/module.modulemap
module WinBase [system] { header "um/winbase.h" export * }

//--- sdk/Include/10.0/WinBase.apinotes
Name: WinBase
Globals:
  - Name: STARTF_USESTDHANDLES
    Type: DWORD

//--- sdk/Include/10.0/um/WinBase.h
#include <minwindef.h>
#define STARTF_USESTDHANDLES 0x00000100

//--- sdk/Include/10.0/shared/minwindef.h
typedef unsigned long DWORD;
