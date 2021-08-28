// RUN: not %target-swift-frontend -typecheck -experimental-skip-all-function-bodies %s

#if os(
struct Anything {
