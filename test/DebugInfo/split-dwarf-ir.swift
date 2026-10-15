// RUN: %target-swift-frontend -primary-file %s -emit-ir -g \
// RUN:   -split-dwarf-output "test.dwo" -o - | %FileCheck %s

// CHECK: !DICompileUnit({{.*}}splitDebugFilename: "test.dwo"

func hello() -> Int { return 42 }
