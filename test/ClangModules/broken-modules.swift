// RUN: rm -rf %t && mkdir -p %t
// RUN: not %target-swift-frontend -parse %s -I %S/Inputs/custom-modules/ -show-diagnostics-after-fatal 2> %t/err.txt
// RUN: FileCheck %s < %t/err.txt

import MissingHeader
// CHECK: {{.*}}/Inputs/custom-modules/module.map:{{[0-9]+:[0-9]+}}: error: header 'this-header-does-not-exist.h' not found
// CHECK: roken-modules.swift:[[@LINE-2]]:8: error: could not build Objective-C module 'MissingHeader'
