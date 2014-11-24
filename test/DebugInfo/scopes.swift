// RUN: %swift -g -emit-ir %s | FileCheck %s
// XFAIL: linux
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
    // CHECK-DAG: "0xb\00[[@LINE+1]]\0013{{.*}}; [ DW_TAG_lexical_block ]
    if true {
      // Verify that all debug line table entries for the expression
      // below are in the same scope.
      //
      // CHECK-DAG: !{{.*}} = metadata !{i32 [[@LINE+3]], i32 33, metadata ![[S:.*]], null}
      // CHECK-DAG: !{{.*}} = metadata !{i32 [[@LINE+2]], i32 46, metadata ![[S]], null}
      // CHECK-DAG: !{{.*}} = metadata !{i32 [[@LINE+1]], i32 11, metadata ![[S]], null}
      let splitViewController = self.window!.rootViewController as UISplitViewController
    }
    return true
  }
}
