// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/many-imports -enable-objc-interop -import-objc-header %S/Inputs/many-imports/obsoleted.h 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -enable-objc-interop -emit-pch %S/Inputs/many-imports/obsoleted.h -o %t.pch
// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/many-imports -enable-objc-interop -import-objc-header %t.pch 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/many-imports -enable-objc-interop -import-objc-header %S/Inputs/many-imports/obsoleted.h -pch-output-dir %t/pch 2>&1 | %FileCheck %s

import Module1
import Module2
import Module3
import Module4
import Module5
import Module6
import Module7
import Module8
import Module9
import Module10
import Module11
import Module12
import Module13
import Module14
import Module15
import Module16
import Module17
import Module18
import Module19
import Module20
import Module21
import Module22
import Module23
import Module24
import Module25
import Module26
import Module27
import Module28
import Module29
import Module30
import Module31
import Module32
import Module33
import Module34
import Module35
import Module36
import Module37
import Module38
import Module39
import Module40
import Module41
import Module42
import Module43
import Module44
import Module45
import Module46
import Module47
import Module48
import Module49
import Module50
import Module51
import Module52
import Module53
import Module54
import Module55
import Module56
import Module57
import Module58
import Module59
import Module60
import Module61
import Module62
import Module63
import Module64
import Module65
import Module66
import Module67
import Module68
import Module69
import Module70
import Module71
import Module72
import Module73
import Module74
import Module75
import Module76
import Module77
import Module78
import Module79
import Module80
import Module81
import Module82
import Module83
import Module84
import Module85
import Module86
import Module87
import Module88
import Module89
import Module90
import Module91
import Module92
import Module93
import Module94
import Module95
import Module96
import Module97
import Module98
import Module99
import Module100

// Trigger the diagnostic.
obsoleted()
// CHECK: [[@LINE-1]]:1: error:
// CHECK: explicitly marked unavailable here
// CHECK: void obsoleted() __attribute__((unavailable));
