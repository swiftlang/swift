// We should not fail if two distinct file have the same name - this is only an issue in CodeGen
// RUN: %sourcekitd-test -req=cursor -pos=1:7 %S/Inputs/invalid_compiler_args/A/File.swift -- %S/Inputs/invalid_compiler_args/A/File.swift %S/Inputs/invalid_compiler_args/B/File.swift | %FileCheck %s

// We can't do anything if the requested file is not in the compiler arguments
// RUN: not %sourcekitd-test -req=cursor -pos=1:7 %S/Inputs/invalid_compiler_args/A/File.swift --
// RUN: not %sourcekitd-test -req=cursor -pos=1:7 %S/Inputs/invalid_compiler_args/A/File.swift -- %S/Inputs/invalid_compiler_args/B/File.swift

// Specifying a file twice should just ignore one of them
// RUN: %sourcekitd-test -req=cursor -pos=1:7 %S/Inputs/invalid_compiler_args/A/File.swift -- %S/Inputs/invalid_compiler_args/A/File.swift %S/Inputs/invalid_compiler_args/A/File.swift


// CHECK: source.lang.swift.decl.class
