// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %s -module-name FileDefault
// RUN: %target-swift-frontend -typecheck -I %t %s -module-name main -DMAIN -verify

default @MainActor

public func test() {}
