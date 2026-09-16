// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.0-abi-triple %s | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: concurrency
// UNSUPPORTED: CPU=arm64e

// Reflection metadata for a concurrency type must retain a symbolic reference
// to its descriptor. Otherwise, the linker can dead-strip libswift_Concurrency
// and runtime metadata lookup will return null.

public struct MainActorFunction {
  let function: @MainActor () -> Void
}

// CHECK: @"got.$sScMMn"
