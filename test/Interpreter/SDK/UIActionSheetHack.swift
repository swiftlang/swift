// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=ios

import UIKit

let actionSheet = UIActionSheet(title: nil, delegate: nil, cancelButtonTitle: "ABC", destructiveButtonTitle: "DEF")

// CHECK: <UIActionSheet: 0x{{.+}}>{{$}}
print(actionSheet.description)
// CHECK-NEXT: 0
print(actionSheet.destructiveButtonIndex)
// CHECK-NEXT: 2
print(actionSheet.numberOfButtons)


let actionSheet2 = UIActionSheet(title: nil, delegate: nil, cancelButtonTitle: "ABC", destructiveButtonTitle: "DEF", otherButtonTitles: "G", "H")

// CHECK: <UIActionSheet: 0x{{.+}}>{{$}}
print(actionSheet2.description)
// CHECK-NEXT: 0
print(actionSheet2.destructiveButtonIndex)
// CHECK-NEXT: 4
print(actionSheet2.numberOfButtons)


let alertView = UIAlertView(title: "Error", message: "The operation completed successfully.", delegate: nil, cancelButtonTitle: "Abort")

// CHECK: <UIAlertView: 0x{{.+}}>{{$}}
print(alertView.description)
// CHECK-NEXT: 0
print(alertView.cancelButtonIndex)
// CHECK-NEXT: 1
print(alertView.numberOfButtons)


let alertView2 = UIAlertView(title: "Error", message: "The operation completed successfully.", delegate: nil, cancelButtonTitle: "Abort", otherButtonTitles: "Cry", "Apologize")

// CHECK: <UIAlertView: 0x{{.+}}>{{$}}
print(alertView2.description)
// CHECK-NEXT: 0
print(alertView2.cancelButtonIndex)
// CHECK-NEXT: 3
print(alertView2.numberOfButtons)

