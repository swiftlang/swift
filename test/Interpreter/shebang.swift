// RUN: rm -rf %t.mcp
// RUN: echo '#!%swift_driver_plain -module-cache-path %t.mcp -i' > %t.shebang.swift
// RUN: cat %s >> %t.shebang.swift
// RUN: chmod u+x %t.shebang.swift

// RUN: %t.shebang.swift | FileCheck -check-prefix=NONE %s
// RUN: %t.shebang.swift a b c | FileCheck -check-prefix=THREE-ARGS %s

// RUN: rm -rf %t.mcp
// RUN: echo '#!%swifti_driver_plain -module-cache-path %t.mcp' > %t.shebang.swifti
// RUN: cat %s >> %t.shebang.swifti
// RUN: chmod u+x %t.shebang.swifti

// RUN: %t.shebang.swifti | FileCheck -check-prefix=NONE %s
// RUN: %t.shebang.swifti a b c | FileCheck -check-prefix=THREE-ARGS %s

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
