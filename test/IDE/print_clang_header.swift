// RUN: echo '#include "header.h"' > %t.m
// RUN: %target-swift-ide-test -source-filename %s -print-header -header-to-print %S/Inputs/header.h --cc-args -Xclang -triple -Xclang %target-triple -fsyntax-only %t.m -I %S/Inputs > %t.txt
// RUN: FileCheck -input-file=%t.txt %s

// CHECK: func doSomethingInHead(arg: Int32)
// CHECK: class BaseInHead {
// CHECK:   class func doIt(arg: Int32)
// CHECK:   func doIt(arg: Int32)
// CHECK: }

// CHECK: /// Awesome name.
// CHECK: class SameName {
// CHECK: }
// CHECK: protocol SameNameProtocol {
// CHECK: }
