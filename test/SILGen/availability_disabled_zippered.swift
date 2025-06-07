// RUN: %target-swift-emit-silgen -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -disable-availability-checking %s -verify
// REQUIRES: OS=macosx

func foo() {
    if #available(macOS 10.15, *) {}
}
