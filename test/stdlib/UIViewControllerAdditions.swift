// RxUN: %target-build-swift -g %s -o %t/ViewControllerAdditions.out
// RxUN: %target-run %t/ViewControllerAdditions.out %S/Inputs/UIViewControllerAdditions | FileCheck %s
// RUN: %target-run-simple-swift %S/Inputs/UIViewControllerAdditions | FileCheck %s

// REQUIRES: OS=ios

import UIKit

// _TtC1a15View1Controller.nib
class View1Controller : UIViewController { }

// _TtC1a15View2.nib
class View2Controller : UIViewController { }

// a.View3Controller.nib
class View3Controller : UIViewController { }

// a.View4.nib
class View4Controller : UIViewController { }

// View5Controller.nib
class View5Controller : UIViewController { }

// View6.nib
class View6Controller : UIViewController { }

// no nib
class MissingViewController : UIViewController { }

let bundle = NSBundle(path:NSString(UTF8String:C_ARGV[1]))

let v1 = View1Controller(nibName:nil, bundle:bundle)
println("tag 1 \(v1.view.tag) you're it")
// CHECK: tag 1 1 you're it

let v2 = View2Controller(nibName:nil, bundle:bundle)
println("tag 2 \(v2.view.tag) you're it")
// CHECK: tag 2 2 you're it

let v3 = View3Controller(nibName:nil, bundle:bundle)
println("tag 3 \(v3.view.tag) you're it")
// CHECK: tag 3 3 you're it

let v4 = View4Controller(nibName:nil, bundle:bundle)
println("tag 4 \(v4.view.tag) you're it")
// CHECK: tag 4 4 you're it

let v5 = View5Controller(nibName:nil, bundle:bundle)
println("tag 5 \(v5.view.tag) you're it")
// CHECK: tag 5 5 you're it

let v6 = View6Controller(nibName:nil, bundle:bundle)
println("tag 6 \(v6.view.tag) you're it")
// CHECK: tag 6 6 you're it

let v7 = MissingViewController(nibName:nil, bundle:bundle)
println("tag 0 \(v7.view.tag) you're it")
// CHECK: tag 0 0 you're it

