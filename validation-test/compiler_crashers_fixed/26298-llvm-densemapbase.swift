// RUN: %target-swift-frontend %s -emit-silgen
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/airspeedswift (airspeedswift)

["1"].map { String($0) }
