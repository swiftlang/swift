// RUN: not %target-swift-frontend %s -dump-parse
// https://github.com/swiftlang/swift/issues/91100
protocol P {
actor A: P {
    init() {
