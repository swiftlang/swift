// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | FileCheck %s


//CHECK: [readonly] @func1
@effects(readonly) @asmname("func1") func func1() { }

//CHECK: [readnone] @func2
@effects(readnone) @asmname("func2") func func2() { }

//CHECK: [readwrite] @func3
@effects(readwrite) @asmname("func3") func func3() { }

