// RUN: %target-swift-emit-silgen -disable-availability-checking %s -verify

func foo() {
    if #available(macOS 10.15, *) {}
}
REQUIRES: updating_for_owned_noescape
