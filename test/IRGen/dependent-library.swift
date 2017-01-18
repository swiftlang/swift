// RUN: %swift -target i686-unknown-windows-msvc -emit-ir -parse-as-library -parse-stdlib -module-name dependent -autolink-library oldnames -autolink-library msvcrt %s -o - | %FileCheck %s

// CHECK: !{i32 6, !"Linker Options", ![[options:[0-9]+]]}
// CHECK: ![[options]] = !{![[oldnames:[0-9]+]], ![[msvcrtd:[0-9]+]]}
// CHECK: ![[oldnames]] = !{!"/DEFAULTLIB:oldnames.lib"}
// CHECK: ![[msvcrtd]] = !{!"/DEFAULTLIB:msvcrt.lib"}

