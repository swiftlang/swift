// https://github.com/swiftlang/swift/issues/51844

func doStuff(_ handler: () -> Void) { }

// CHECK: [[@LINE+1]]:17-[[@LINE+1]]:22 source.refactoring.range.kind.basename
func merge<T>(_ param: T) {
  func makeHandler(_ successHandler: (T) -> T) -> () -> Void {
      return {}
  }
  // RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=%(line+1):42 %s -- %s | %FileCheck %s
  doStufff(makeHandler { (success: T) in param})
  // CHECK: [[@LINE-1]]:42-[[@LINE-1]]:47 source.refactoring.range.kind.basename
}
