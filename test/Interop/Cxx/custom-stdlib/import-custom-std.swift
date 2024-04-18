// RUN: %target-swiftxx-frontend -typecheck -cxx-stdlib-path%S/Inputs/c++ -I %S/Inputs -dump-clang-diagnostics %s 2>&1 | %FileCheck -check-prefix=CLANG_DIAGS %s
// RUN: %target-swiftxx-frontend -emit-ir -cxx-stdlib-path%S/Inputs/c++ -I %S/Inputs -Xcc -fignore-exceptions %s | %FileCheck -check-prefix=IR %s

import CxxHeader
import custom_std

// CLANG_DIAGS: clang importer driver args:
// CLANG_DIAGS-SAME: '-D__swift_use_custom_cxx_stdlib__'
// CLANG_DIAGS-SAME: '-isystem<placeholder-custom-cxx-stdlib-dir>'
// CLANG_DIAGS-SAME: '-nostdinc++'
// CLANG_DIAGS: Adjusted include paths to account for custom C++ stdlib:
// CLANG_DIAGS-SAME: Inputs/c++'

public func testCustomString() -> Int {
    let s = std.string("hello")
    return Int(s.size())
}

// IR: %{{.*}} = call ptr @_ZNSt3v4212basic_stringIcEC1EPKc(ptr %{{.*}}, ptr %{{.*}})
// IR: call i{{.*}} @_ZNKSt3v4212basic_stringIcE4sizeEv(ptr %{{.*}})

// IR: "-lswiftCxx"
// IR-NOT: "-lswiftCxxStdlib"
