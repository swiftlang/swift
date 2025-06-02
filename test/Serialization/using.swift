// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %s -module-name Using -enable-experimental-feature DefaultIsolationPerFile
// RUN: %target-swift-frontend -typecheck -I %t %s -module-name main -DMAIN -verify -enable-experimental-feature DefaultIsolationPerFile

// REQUIRES: swift_feature_DefaultIsolationPerFile

using @MainActor

public func test() {}
