// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -import-ns-enum -parse -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 -sdk=%S/Inputs %s -verify 

import Foundation

var mince = NSRuncingMode.NSRuncingMince
var quince = NSRuncingMode.NSRuncingQuince
