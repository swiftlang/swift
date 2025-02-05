// RUN: %target-swiftxx-frontend -typecheck -Xcc -cxx-isystem -Xcc %S/Inputs/c++ -Xcc -stdlib=libc++  -Xcc -nostdinc++ -I %S/Inputs -dump-clang-diagnostics %s 2>&1 | %FileCheck -check-prefix=CLANG_DIAGS %s
// RUN: %target-swiftxx-frontend -emit-ir -Xcc -cxx-isystem -Xcc %S/Inputs/c++ -Xcc -stdlib=libc++ -Xcc -nostdinc++ -I %S/Inputs -Xcc -fignore-exceptions %s | %FileCheck -check-prefix=IR %s

import CxxHeader
import custom_std

// CLANG_DIAGS: clang importer driver args:
// CLANG_DIAGS-SAME: '-cxx-isystem'
// CLANG_DIAGS-SAME: '-nostdinc++'
// CLANG_DIAGS-NOT: argument unused during compilation: '-stdlib=libc++'

public func testCustomString() -> Int {
    let s = std.string("hello")
    return Int(s.size())
}

// IR: %{{.*}} = call ptr @{{"\?\?0\?\$basic_string@D@v42@std@@QEAA@PEBD@Z"|_ZNSt3v4212basic_stringIcEC1EPKc}}(ptr %{{.*}}, ptr %{{.*}})
// IR: call i{{.*}} @{{"\?size@\?\$basic_string@D@v42@std@@QEBA_KXZ"|_ZNKSt3v4212basic_stringIcE4sizeEv}}(ptr %{{.*}})
