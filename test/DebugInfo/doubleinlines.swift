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
// CHECK: tail call void @llvm.trap(){{.*}}, !dbg ![[SCOPEONE:.*]]

// CHECK: ![[FUNCSCOPEOTHER:.*]] = distinct !DISubprogram(name: "condFail",{{.*}}
// CHECK: ![[SCOPEONE]] = !DILocation(line: 6, scope: ![[SCOPETRAP:.*]], inlinedAt: ![[SCOPEINLINED:.*]])
// CHECK: ![[SCOPETRAP]] = distinct !DISubprogram(name: "Swift runtime failure: unknown program error"

import Builtin
