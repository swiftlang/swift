// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %swift_src_root/lib/ClangImporter/SwiftBridging -Xcc -std=c++20 -I %t/Inputs  %t/test.swift -strict-memory-safety -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1

// REQUIRES: objc_interop
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: OS=macosx

//--- Inputs/module.modulemap
module Test {
    header "nonescapable.h"
    requires cplusplus
}

//--- Inputs/nonescapable.h
#pragma once

#include <simd/simd.h>
#include <vector>

using VecOfSimd = std::vector<simd::float3>;
using MySimd = simd::float3;

//--- test.swift
import Test
import CxxStdlib

func simdConsideredSafe(x : MySimd) {
    let _ = x
}

func simdVecConsideredSafe(x : VecOfSimd) {
    let _ = x
}