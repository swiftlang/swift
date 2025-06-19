// RUN: rm -rf %t
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -plugin-path %swift-plugin-dir %t/blessed.swift -module-name main -I %t/Inputs -o %t/out -Xcc -std=c++20  -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers -verify

// REQUIRES: swift_feature_SafeInteropWrappers

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu, OS=linux-android, OS=linux-androideabi

//--- Inputs/swiftify-non-public.h
#pragma once

#include "swift/bridging"
#include <span>

using IntSpan = std::span<const int>;

class SWIFT_PRIVATE_FILEID("main/blessed.swift") MyClass {
private:
    void takesSpan(IntSpan s [[clang::noescape]]) {}
};

//--- Inputs/module.modulemap
module SwiftifyNonPublic {
    requires cplusplus
    header "swiftify-non-public.h"
}

//--- blessed.swift
import CxxStdlib
import SwiftifyNonPublic

extension MyClass {
  mutating func passesSpan(_ s: Span<CInt>) {
    takesSpan(s) // expected-error {{cannot convert value of type}}
  }
}
