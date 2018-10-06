// RUN: %incr-transfer-tree --expected-incremental-syntax-tree %S/Outputs/extend-identifier-at-eof.json %s

func foo() {}
_ = x<<<|||x>>>