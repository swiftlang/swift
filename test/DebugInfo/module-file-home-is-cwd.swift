// First populate the module cache to avoid issues with implicit modules
// and -fmodule-file-home-is-cwd.
// RUN: %target-swift-frontend -module-cache-path %t.mcp -emit-ir %s \
// RUN:   -g -I %S/Inputs -o /dev/null

// RUN: %target-swift-frontend -module-cache-path %t.mcp -emit-ir %s \
// RUN:   -g -I %S/Inputs -file-compilation-dir /CWD \
// RUN:   -Xcc -Xclang -Xcc -fmodule-file-home-is-cwd \
// RUN:   -o - | %FileCheck %s

import ClangModule.SubModule

let _ = someFunc(0)

// Ensure compilation directory is set to the debug compilation directory.
// CHECK: !DIFile(filename: "ClangModule", directory: "/CWD")
