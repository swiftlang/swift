// RUN: %swift %s -std=axle -emit-llvm | FileCheck %s

func [kernel] kern0() { }
func [kernel] kern1() { }

func [vertex] vert0() { }

func [fragment] frag0() { }

// CHECK: !air.kernel = !{![[KERN0:[0-9]+]], ![[KERN1:[0-9]+]]}
// CHECK: !air.vertex = !{![[VERT0:[0-9]+]]}
// CHECK: !air.fragment = !{![[FRAG0:[0-9]+]]}

// CHECK: ![[KERN0]] = metadata !{void ()* @_T4main5kern0FT_T_, metadata ![[EMPTY:[0-9]+]], metadata ![[EMPTY]]}
// CHECK: ![[EMPTY]] = metadata !{}
// CHECK: ![[KERN1]] = metadata !{void ()* @_T4main5kern1FT_T_, metadata ![[EMPTY]], metadata ![[EMPTY]]}
// CHECK: ![[VERT0]] = metadata !{void ()* @_T4main5vert0FT_T_, metadata ![[EMPTY]], metadata ![[EMPTY]]}
// CHECK: ![[FRAG0]] = metadata !{void ()* @_T4main5frag0FT_T_, metadata ![[EMPTY]], metadata ![[EMPTY]]}
