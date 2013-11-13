// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -import-ns-options -parse -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 -sdk=%S/Inputs %s -verify 
// -- Check that we can successfully round-trip.
// RUN: %swift -import-ns-options -emit-llvm -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 -sdk=%S/Inputs %s >/dev/null

import Foundation

var withMince: NSRuncingOptions = NSRuncingOptions.Mince
var withQuince: NSRuncingOptions = NSRuncingOptions.Quince
