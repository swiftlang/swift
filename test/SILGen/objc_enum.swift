// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -import-ns-enum -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 -sdk=%S/Inputs %s -emit-silgen | FileCheck %s

import gizmo

// CHECK: sil thunk [transparent] @_TOSC16NSRuncingOptions14NSRuncingMinceFMS_S_
// CHECK: sil thunk [transparent] @_TOSC16NSRuncingOptions21NSRuncingQuinceSlicedFMS_S_
// CHECK: sil thunk [transparent] @_TOSC16NSRuncingOptions24NSRuncingQuinceJuliennedFMS_S_
// CHECK: sil thunk [transparent] @_TOSC16NSRuncingOptions20NSRuncingQuinceDicedFMS_S_


var runcing: NSRuncingOptions = .NSRuncingMince
