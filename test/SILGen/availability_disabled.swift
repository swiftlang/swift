// RUN: %target-swift-emit-silgen -disable-availability-checking %s -verify

func foo() {
    if #available(macOS 10.15, *) {}
}
