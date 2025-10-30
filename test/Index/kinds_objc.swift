// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

@objc class TargetForIBAction {}
// CHECK: [[@LINE-1]]:13 | class(internal)/Swift | TargetForIBAction | [[TargetForIBAction_USR:.*]] | Def |
@objc class TargetForIBSegueAction {}
// CHECK: [[@LINE-1]]:13 | class(internal)/Swift | TargetForIBSegueAction | [[TargetForIBSegueAction_USR:.*]] | Def |
class AttrAnnots {
  @IBOutlet var iboutletString: AnyObject?
  // CHECK: [[@LINE-1]]:17 | instance-property(IB,internal)/Swift | iboutletString |
  @IBAction func someibaction(o: TargetForIBAction) {}
  // CHECK: [[@LINE-1]]:18 | instance-method(IB,internal)/Swift | someibaction(o:) | {{.*}} | Def,Dyn,RelChild,RelIBType | rel: 2
  // CHECK-NEXT: RelIBType | class/Swift | TargetForIBAction | [[TargetForIBAction_USR]]
  @IBSegueAction func someibsegue(coder: Any, o: TargetForIBSegueAction) -> Any {}
  // CHECK: [[@LINE-1]]:23 | instance-method(IB,internal)/Swift | someibsegue(coder:o:) | {{.*}} | Def,Dyn,RelChild,RelIBType | rel: 2
  // CHECK-NEXT: RelIBType | class/Swift | TargetForIBSegueAction | [[TargetForIBSegueAction_USR]]
  @GKInspectable var gkString = "gk"
  // CHECK: [[@LINE-1]]:22 | instance-property(GKI,internal)/Swift | gkString |
}

