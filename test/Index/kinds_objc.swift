// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s
// REQUIRES: objc_interop

@objc class TargetForIBAction {}
// CHECK: [[@LINE-1]]:13 | class/Swift | TargetForIBAction | [[TargetForIBAction_USR:.*]] | Def |
class AttrAnnots {
  @IBOutlet var iboutletString: AnyObject?
  // CHECK: [[@LINE-1]]:17 | instance-property(IB)/Swift | iboutletString |
  @IBAction func someibaction(o: TargetForIBAction) {}
  // CHECK: [[@LINE-1]]:18 | instance-method(IB)/Swift | someibaction(o:) | {{.*}} | Def,Dyn,RelChild,RelIBType | rel: 2
  // CHECK-NEXT: RelIBType | class/Swift | TargetForIBAction | [[TargetForIBAction_USR]]
  @GKInspectable var gkString = "gk"
  // CHECK: [[@LINE-1]]:22 | instance-property(GKI)/Swift | gkString |
}

