// RUN: echo '#!%swift_driver_plain' > %t.shebang.swift
// RUN: cat %s >> %t.shebang.swift
// RUN: chmod u+x %t.shebang.swift

// RUN: %t.shebang.swift | FileCheck -check-prefix=NONE %s
// RUN: %t.shebang.swift a b c | FileCheck -check-prefix=THREE-ARGS %s

// REQUIRES: swift_interpreter

println("Begin")
for arg in Process.arguments {
  println(arg)
}
println("End")

// NONE: Begin
// NONE-NEXT: {{.*}}shebang.swift
// NONE-NEXT: End

// THREE-ARGS: Begin
// THREE-ARGS-NEXT: {{.*}}shebang.swift
// THREE-ARGS-NEXT: a
// THREE-ARGS-NEXT: b
// THREE-ARGS-NEXT: c
// THREE-ARGS-NEXT: End
