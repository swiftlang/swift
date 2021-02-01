// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -enable-experimental-concurrency | %IRGenFileCheck %s
// REQUIRES: concurrency

// rdar_72047158
// XFAIL: CPU=arm64e

print("TODO") // CHECK: TODO
