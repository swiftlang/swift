// REQUIRES: OS=xros || OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include -sdk %t/sdk \
// RUN:   -scanner-prefix-map-paths %t /^tmp -scanner-prefix-map-paths %t/sdk /^sdk -target arm64-apple-xros1.0

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/Test.cmd

// RUN: %target-swift-frontend -target arm64-apple-xros1.0 \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name Test -sdk /^sdk \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/Test.cmd -dump-availability-scopes 2>&1 | %FileCheck %s

// CHECK: (root version=1.0
// CHECK:   (decl version=1.1 decl=foo()
// CHECK:     (condition_following_availability version=2.0

//--- main.swift
import A
@available(iOS 17.4, *)
public func foo() {
  if #available(visionOS 2.0, *) {
    a()
  }
}

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
@available(macOS 15.0, iOS 17.1, *)
public func a() { }

//--- sdk/SDKSettings.json
{
  "CanonicalName": "xros26.0",
  "Version": "26.0",
  "IsBaseSDK": "YES",
  "DisplayName": "visionOS 26.0",
  "MinimalDisplayName": "26.0",
  "SupportedTargets": {
    "xros": {
      "PlatformFamilyName": "xrOS",
      "PlatformFamilyDisplayName": "visionOS",
      "Archs": ["arm64e", "arm64"], "LLVMTargetTripleVendor": "apple", "LLVMTargetTripleSys": "xros", "LLVMTargetTripleEnvironment": "",
      "BuildVersionPlatformID": "11",
      "ClangRuntimeLibraryPlatformName": "xros",
      "SystemPrefix": "",
      "DefaultDeploymentTarget": "26.0",
      "RecommendedDeploymentTarget": "1.0",
      "MinimumDeploymentTarget": "1.0", "MaximumDeploymentTarget": "26.0.99",
      "ValidDeploymentTargets": ["1.0", "1.1", "1.2", "1.3", "2.0", "2.1", "2.2", "2.3", "2.4", "2.5", "2.6", "26.0"]
    }
  },
  "VersionMap": {
    "visionOS_iOS": {"1.0": "17.1", "1.1": "17.4", "1.2": "17.5", "2.0": "18.0", "2.1": "18.1", "2.2": "18.2", "2.3": "18.3", "2.4": "18.4", "2.5": "18.5", "2.6": "18.6", "26.0": "26.0"},
    "iOS_visionOS": {"17.1": "1.0", "17.4": "1.1", "17.5": "1.2", "18.0": "2.0", "18.1": "2.1", "18.2": "2.2", "18.3": "2.3", "18.4": "2.4", "18.5": "2.5", "18.6": "2.6", "26.0": "26.0"},
    "xrOS_iOS": {"1.0": "17.1", "1.1": "17.4", "1.2": "17.5", "2.0": "18.0", "2.1": "18.1", "2.2": "18.2", "2.3": "18.3", "2.4": "18.4", "2.5": "18.5", "2.6": "18.6", "26.0": "26.0"},
    "iOS_xrOS": {"17.1": "1.0", "17.4": "1.1", "17.5": "1.2", "18.0": "2.0", "18.1": "2.1", "18.2": "2.2", "18.3": "2.3", "18.4": "2.4", "18.5": "2.5", "18.6": "2.6", "26.0": "26.0"}
  },
  "DefaultDeploymentTarget": "26.0",
  "MaximumDeploymentTarget": "26.0.99"
}
