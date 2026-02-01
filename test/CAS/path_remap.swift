// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include -sdk %t/sdk \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp -scanner-prefix-map-paths %t/sdk /^sdk -enable-cross-import-overlays

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json _B_A > %t/BA.cmd
// RUN: %swift_frontend_plain @%t/BA.cmd

// RUN: %FileCheck %s --check-prefix=SDK-REMAP --input-file=%t/A.cmd
// RUN: %FileCheck %s --check-prefix=SDK-REMAP --input-file=%t/B.cmd
// RUN: %FileCheck %s --check-prefix=SDK-REMAP --input-file=%t/BA.cmd

// SDK-REMAP: -isysroot
// SDK-REMAP-NEXT: -Xcc
// SDK-REMAP-NEXT: /^sdk

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %FileCheck %s --check-prefix=SDK-REMAP --input-file=%t/MyApp.cmd

// RUN: %target-swift-frontend \
// RUN:   -c -o %t/main.o -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp.cmd -enable-cross-import-overlays

// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 0 -- \
// RUN:   %target-swift-frontend \
// RUN:   -c -o %t/main.o -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp.cmd -enable-cross-import-overlays > %t/key.casid

// RUN: %swift-scan-test -action replay_result -cas-path %t/cas -id @%t/key.casid -- \
// RUN:   %target-swift-frontend \
// RUN:   -c -o %t/main.o -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp.cmd -enable-cross-import-overlays

//--- main.swift
import A
import B

#warning("This is a warning")

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func a() { }

//--- include/B.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name B -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func b() { }

//--- include/_B_A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name _B_A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func b_a() { }

//--- include/B.swiftcrossimport/A.swiftoverlay
%YAML 1.2
---
version: 1
modules:
  - name: _B_A
//--- sdk/SDKSettings.json
{
  "CanonicalName": "macosx26.1",
  "Version": "26.1",
  "IsBaseSDK": "YES",
  "DisplayName": "macOS 26.1",
  "MinimalDisplayName": "26.1",
  "SupportedTargets": {
    "macosx": {
      "PlatformFamilyName": "macOS",
      "PlatformFamilyDisplayName": "macOS",
      "Archs": ["x86_64", "x86_64h", "arm64", "arm64e"], "LLVMTargetTripleVendor": "apple", "LLVMTargetTripleSys": "macos", "LLVMTargetTripleEnvironment": "",
      "BuildVersionPlatformID": "1",
      "ClangRuntimeLibraryPlatformName": "osx",
      "SystemPrefix": "",
      "DefaultDeploymentTarget": "26.1",
      "RecommendedDeploymentTarget": "11.0",
      "MinimumDeploymentTarget": "10.13","MaximumDeploymentTarget": "26.1.99",
      "ValidDeploymentTargets": ["10.13", "10.14", "10.15", "11.0", "11.1", "11.2", "11.3", "11.4", "11.5", "12.0", "12.2", "12.3", "12.4", "13.0", "13.1", "13.2", "13.3", "13.4", "13.5", "14.0", "14.1", "14.2", "14.3", "14.4", "14.5", "14.6", "15.0", "15.1", "15.2", "15.3", "15.4", "15.5", "15.6", "26.0", "26.1"]
    },
    "iosmac": {
      "Archs": ["x86_64", "x86_64h", "arm64", "arm64e"], "LLVMTargetTripleVendor": "apple", "LLVMTargetTripleSys": "ios", "LLVMTargetTripleEnvironment": "macabi",
      "BuildVersionPlatformID": "6",
      "ClangRuntimeLibraryPlatformName": "osx",
      "SystemPrefix": "/System/iOSSupport",
      "DefaultDeploymentTarget": "26.1",
      "RecommendedDeploymentTarget": "14.2",
      "MinimumDeploymentTarget": "13.1", "MaximumDeploymentTarget": "26.1.99",
      "ValidDeploymentTargets": ["13.1", "13.2", "13.3", "13.3.1", "13.4", "13.5", "14.0", "14.1", "14.2", "14.3", "14.4", "14.5", "14.6", "14.7", "15.0", "15.2", "15.3", "15.4", "15.5", "15.6", "16.0", "16.1", "16.2", "16.3", "16.4", "16.5", "16.6", "17.0", "17.1", "17.2", "17.3", "17.4", "17.5", "17.6", "18.0", "18.1", "18.2", "18.3", "18.4", "18.5", "18.6", "26.0", "26.1"]
    }
  },
  "VersionMap": {
    "macOS_iOSMac": {"10.15": "13.1", "10.15.1": "13.2", "10.15.2": "13.3", "10.15.3": "13.3.1", "10.15.4": "13.4", "10.15.5": "13.5",                                 "11.0": "14.2", "11.0.1": "14.2", "11.1": "14.3", "11.2": "14.4", "11.3": "14.5", "11.4": "14.6", "11.5": "14.7", "12.0": "15.0", "12.0.1": "15.0", "12.1": "15.2", "12.2": "15.3", "12.3": "15.4", "12.4": "15.5", "12.5": "15.6", "13.0": "16.1",                 "13.1": "16.2", "13.2": "16.3", "13.3": "16.4", "13.4": "16.5", "13.5": "16.6", "14.0": "17.0", "14.1": "17.1", "14.2": "17.2", "14.3": "17.3", "14.4": "17.4", "14.5": "17.5", "14.6": "17.6", "15.0": "18.0", "15.1": "18.1", "15.2": "18.2", "15.3": "18.3", "15.4": "18.4", "15.5": "18.5", "15.6": "18.6", "26.0": "26.0", "26.1": "26.1"},
    "iOSMac_macOS": {"13.1": "10.15", "13.2": "10.15.1", "13.3": "10.15.2", "13.3.1": "10.15.3", "13.4": "10.15.4", "13.5": "10.15.5", "14.0": "11.0", "14.1": "11.0", "14.2": "11.0",                   "14.3": "11.1", "14.4": "11.2", "14.5": "11.3", "14.6": "11.4", "14.7": "11.5", "15.0": "12.0",                   "15.2": "12.1", "15.3": "12.2", "15.4": "12.3", "15.5": "12.4", "15.6": "12.5", "16.0": "13.0", "16.1": "13.0", "16.2": "13.1", "16.3": "13.2", "16.4": "13.3", "16.5": "13.4", "16.6": "13.5", "17.0": "14.0", "17.1": "14.1", "17.2": "14.2", "17.3": "14.3", "17.4": "14.4", "17.5": "14.5", "17.6": "14.6", "18.0": "15.0", "18.1": "15.1", "18.2": "15.2", "18.3": "15.3", "18.4": "15.4", "18.5": "15.5", "18.6": "15.6", "26.0": "26.0", "26.1": "26.1"}
  },
  "DefaultDeploymentTarget": "26.1",
  "MaximumDeploymentTarget": "26.1.99"
}
