// RUN: %swift -cfg-dump %s | FileCheck %s

func bar(x:Int)

func call() {
  bar(42);
}

// CHECK: func_decl call
