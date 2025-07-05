// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name output -emit-silgen -swift-version 6 | swift-demangle | %FileCheck %s

// REQUIRES: concurrency

@MainActor
public func isEven(_ x: Int) -> Bool {
    x.isMultiple(of: 2)
}

// CHECK: sil [ossa] @output.mainActorFunc(xs: [Swift.Int])
// CHECK-NOT: _checkTaskExecutor
// CHECK: end sil function 'output.mainActorFunc(xs: [Swift.Int]) -> Swift.Int'
@MainActor
public func mainActorFunc(xs: [Int]) -> Int {
    xs.count { isEven($0) }
}
