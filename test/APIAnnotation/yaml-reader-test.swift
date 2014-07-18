// RUN: %swift_driver_plain -apinotes -yaml-to-binary %S/Inputs/UIKit.yaml -o %t-ObjectiveC.apinotes | FileCheck %s
// CHECK: UIKit