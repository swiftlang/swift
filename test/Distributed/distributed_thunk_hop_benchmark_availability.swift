// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/TestsUtils.swiftmodule -module-name TestsUtils -target %target-cpu-apple-macosx12.0 %S/../../benchmark/utils/TestsUtils.swift
// RUN: %target-swift-frontend -typecheck -target %target-cpu-apple-macosx12.0 -I %t %S/../../benchmark/single-source/DistributedThunkHop.swift
// REQUIRES: OS=macosx && (CPU=x86_64 || CPU=arm64)
// REQUIRES: concurrency
// REQUIRES: distributed

// Ensure the distributed thunk benchmark compiles with a macOS 12 deployment target.
