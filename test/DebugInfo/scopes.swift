// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

class UIViewController {
}

class UISplitViewController : UIViewController {
  var delegate : UIViewController?
}

class UIWindow {
  var rootViewController: UIViewController?
}

class AppDelegate {
  var window: UIWindow?

  func application() -> Bool {
    // CHECK-DAG: !DILexicalBlock({{.*}}line: [[@LINE+1]], column: 13
    if true {
      // Verify that all debug line table entries for the expression
      // below are in the same scope.
      //
      // CHECK-DAG: !DILocalVariable(name: "splitViewController", scope: ![[S1:[0-9]+]]
      // CHECK-DAG: ![[S2:[0-9]+]] = distinct !DILexicalBlock(scope: ![[S1]]
      // CHECK-DAG: !DILocation(line: [[@LINE+3]], column: 11, scope: ![[S1]])
      // CHECK-DAG: !DILocation(line: [[@LINE+2]], column: 44, scope: ![[S2]])
      // CHECK-DAG: !DILocation(line: [[@LINE+1]], column: 65, scope: ![[S2]])
      let splitViewController = self.window!.rootViewController as! UISplitViewController
    }
    return true
  }
}
