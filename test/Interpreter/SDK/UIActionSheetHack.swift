// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: OS=ios

import UIKit

let actionSheet = UIActionSheet(title: nil, delegate: nil, cancelButtonTitle: "ABC", destructiveButtonTitle: "DEF")

// CHECK: <UIActionSheet: 0x{{.+}}>{{$}}
println(actionSheet.description)
// CHECK-NEXT: 0
println(actionSheet.destructiveButtonIndex)
// CHECK-NEXT: 2
println(actionSheet.numberOfButtons)


let actionSheet2 = UIActionSheet(title: nil, delegate: nil, cancelButtonTitle: "ABC", destructiveButtonTitle: "DEF", otherButtonTitles: "G", "H")

// CHECK: <UIActionSheet: 0x{{.+}}>{{$}}
println(actionSheet2.description)
// CHECK-NEXT: 0
println(actionSheet2.destructiveButtonIndex)
// CHECK-NEXT: 4
println(actionSheet2.numberOfButtons)


let alertView = UIAlertView(title: "Error", message: "The operation completed successfully.", delegate: nil, cancelButtonTitle: "Abort")

// CHECK: <UIAlertView: 0x{{.+}}>{{$}}
println(alertView.description)
// CHECK-NEXT: 0
println(alertView.cancelButtonIndex)
// CHECK-NEXT: 1
println(alertView.numberOfButtons)


let alertView2 = UIAlertView(title: "Error", message: "The operation completed successfully.", delegate: nil, cancelButtonTitle: "Abort", otherButtonTitles: "Cry", "Apologize")

// CHECK: <UIAlertView: 0x{{.+}}>{{$}}
println(alertView2.description)
// CHECK-NEXT: 0
println(alertView2.cancelButtonIndex)
// CHECK-NEXT: 3
println(alertView2.numberOfButtons)

