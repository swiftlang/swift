// RUN: rm -f %t.*

// REQUIRES: swift_interpreter

// RUN: %target-swift-frontend -typecheck -serialize-diagnostics-path %t.dia %s -verify
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics-path=%t_EQ.dia %s -verify
// RUN: not %swift_driver %astgen_if_in_use -serialize-diagnostics-path %t_interpret_mode.dia %s
// RUN: not %swift_driver %astgen_if_in_use -serialize-diagnostics-path=%t_EQ_interpret_mode.dia %s

// RUN: diff %t.dia %t_EQ.dia
// RUN: diff %t.dia %t_interpret_mode.dia
// RUN: diff %t.dia %t_EQ_interpret_mode.dia

var x = 1 x = 2   // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} {{10-10=;}} expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}} {{10-11= \n}}
