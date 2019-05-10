// RUN: rm -f %t.*

// REQUIRES: swift_interpreter

// RUN: %target-swift-frontend -typecheck -serialize-diagnostics-path %t.dia %s -verify
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics-path=%t_EQ.dia %s -verify
// RUN: not %swift_driver -serialize-diagnostics-path %t_intepret_mode.dia %s
// RUN: not %swift_driver -serialize-diagnostics-path=%t_EQ_intepret_mode.dia %s

// RUN: diff %t.dia %t_EQ.dia
// RUN: diff %t.dia %t_intepret_mode.dia
// RUN: diff %t.dia %t_EQ_intepret_mode.dia

var x = 1 x = 2   // expected-error {{consecutive statements on a line must be separated by ';'}} {{10-10=;}}
