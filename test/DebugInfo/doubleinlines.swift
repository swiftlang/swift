// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -enable-builtin-module \
// RUN: -debug-info-format=codeview -O -parse-as-library \
// RUN: -module-name DoubleInlines -o - | %FileCheck %s

func condFail(arg: Builtin.Int1, msg: Builtin.RawPointer) {
    Builtin.condfail_message(arg, msg)
}

func callCondFail(arg: Builtin.Int1, msg: Builtin.RawPointer) {
    condFail(arg: arg, msg: msg)
}

// CHECK: define hidden swiftcc void @"$s13DoubleInlines12callCondFail3arg3msgyBi1__BptF"{{.*}} !dbg ![[FUNC:.*]] {
// CHECK: tail call void asm sideeffect "", "n"(i32 0) #{{[0-9]+}}, !dbg ![[SCOPEONE:.*]]
// CHECK: tail call void @llvm.trap(), !dbg ![[LOCTRAP:.*]]

// CHECK: ![[FUNCSCOPEOTHER:.*]] = distinct !DISubprogram(name: "condFail",{{.*}}
// CHECK: ![[SCOPEONE]] = distinct !DILocation(line: 6, scope: ![[SCOPETWO:.*]], inlinedAt: ![[SCOPETHREE:.*]])
// CHECK: ![[SCOPETHREE]] = !DILocation(line: 6, scope: ![[FUNCSCOPE:.*]])
// CHECK: ![[FUNCSCOPE:[0-9]+]] = distinct !DILexicalBlock(scope: ![[FUNC]],
// CHECK: ![[LOCTRAP]] = !DILocation(line: 6, scope: ![[SCOPETRAP:.*]], inlinedAt: ![[SCOPEONE]])
// CHECK: ![[SCOPETRAP]] = distinct !DISubprogram(name: "Swift runtime failure: unknown program error"

import Builtin
