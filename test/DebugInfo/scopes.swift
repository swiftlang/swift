// RUN: %target-swift-frontend -g -emit-ir %s | FileCheck %s

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
    // CHECK-DAG: !MDLexicalBlock({{.*}}line: [[@LINE+1]], column: 13
    if true {
      // Verify that all debug line table entries for the expression
      // below are in the same scope.
      //
      // CHECK-DAG: !{{.*}} = !MDLocation(line: [[@LINE+2]], column: 46, scope: ![[S:.*]])
      // CHECK-DAG: !{{.*}} = !MDLocation(line: [[@LINE+1]], column: 11, scope: ![[S]])
      let splitViewController = self.window!.rootViewController as! UISplitViewController
    }
    return true
  }
}
