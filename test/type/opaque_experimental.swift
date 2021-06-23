// RUN: %target-swift-frontend -enable-experimental-opaque-return-types -disable-availability-checking -typecheck -verify %s

// Tests for experimental extensions to opaque return type support.

func f0() -> <T> () { }
