// RUN: echo '#'!%swift_driver_plain > %t.shebang.swift
// RUN: cat %S/shebang-env.swift >> %t.shebang.swift
// RUN: chmod u+x %t.shebang.swift

// RUN: %t.shebang.swift | %FileCheck -check-prefix=NONE %S/shebang-env.swift
// RUN: %t.shebang.swift a b c | %FileCheck -check-prefix=THREE-ARGS %S/shebang-env.swift

// REQUIRES: swift_interpreter
// UNSUPPORTED: OS=linux-gnu
// Windows doesn't support shebang lines; it would open the file in whatever
// program is associated with .swift files instead.
// UNSUPPORTED: OS=windows-msvc
