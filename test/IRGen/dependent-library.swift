// RUN: %swift -target i686-unknown-windows-msvc -emit-ir -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name dependent -autolink-library oldnames -autolink-library msvcrt %s -o - | %FileCheck %s

// CHECK: !llvm.linker.options = !{![[oldnames:[0-9]+]], ![[msvcrtd:[0-9]+]]}
// CHECK: ![[oldnames]] = !{!"/DEFAULTLIB:oldnames.lib"}
// CHECK: ![[msvcrtd]] = !{!"/DEFAULTLIB:msvcrt.lib"}

