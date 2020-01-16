// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -parse-stdlib \
// RUN: -debug-info-format=codeview -O -parse-as-library \
// RUN: -module-name DoubleInlines -o - | %FileCheck %s

func condFail(arg: Builtin.Int1, msg: Builtin.RawPointer) {
    Builtin.condfail_message(arg, msg)
}

func callCondFail(arg: Builtin.Int1, msg: Builtin.RawPointer) {
    condFail(arg: arg, msg: msg)
}

// CHECK: define hidden swiftcc void @"$s13DoubleInlines12callCondFail3arg3msgyBi1__BptF"{{.*}} !dbg ![[FUNCSCOPE:.*]] {
// CHECK: tail call void asm sideeffect "", "n"(i32 0) #3, !dbg ![[SCOPEONE:.*]]
// CHECK: ![[FUNCSCOPEOTHER:.*]] = distinct !DISubprogram(name: "condFail",{{.*}}
// CHECK: ![[SCOPEFIVE:.*]] = distinct !DILexicalBlock(scope: ![[FUNCSCOPE]], file: ![[FILE:.*]], line: 9)
// CHECK: ![[SCOPESIX:.*]] = distinct !DILexicalBlock(scope: ![[FUNCSCOPEOTHER]], file: ![[FILE]], line: 5)
// CHECK: ![[SCOPEONE]] = !DILocation(line: 0, scope: ![[SCOPETWO:.*]], inlinedAt: ![[SCOPETHREE:.*]])
// CHECK: ![[SCOPETHREE]] = !DILocation(line: 6, scope: ![[SCOPEFOUR:.*]])
// CHECK: ![[SCOPEFOUR]] = distinct !DILexicalBlock(scope: ![[SCOPEFIVE]], file: ![[FILE]], line: 10)
