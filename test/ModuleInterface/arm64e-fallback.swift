// RUN: not %target-typecheck-verify-swift -target arm64e-apple-ios13.0 -F %S/Inputs 2>&1 | %FileCheck %s

// REQUIRES: OS=ios
// UNSUPPORTED: OS=maccatalyst

import DummyFramework
// CHECK: could not find module 'DummyFramework' for target 'arm64e-apple-ios'; found: arm64-apple-ios, arm64
