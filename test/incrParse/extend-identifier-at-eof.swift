// RUN: %incr-transfer-tree --expected-incremental-syntax-tree %S/Outputs/extend-identifier-at-eof.json %s

func foo() {}
// ATTENTION: This file is testing the EOF token. 
// DO NOT PUT ANYTHING AFTER THE CHANGE, NOT EVEN A NEWLINE
_ = x<<<|||x>>>