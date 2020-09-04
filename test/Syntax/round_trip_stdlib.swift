// <rdar://problem/64269925> test/Syntax/round_trip_stdlib.swift is very slow on ASAN build
// REQUIRES: no_asan
// RUN: %round-trip-syntax-test -d %swift_src_root/stdlib --swift-syntax-test %swift-syntax-test
