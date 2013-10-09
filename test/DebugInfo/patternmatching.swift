// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o -
// | FileCheck %s
func classifyPoint2(p : (Double, Double)) {
    switch p {
        case (0, 0):
          println("origin")
        case (_, 0):
          println("on the X axis")
        case (0, _):
          println("on the Y axis")
          println("on the + diagonal")
        // FIXME: Verify that all variables end up in the appropriate scopes.
        // FIXME: metadata !{{{.*}}, metadata ![[SCOPEA:.*]], metadata !"x", {{.*}}} ; [ DW_TAG_auto_variable ] [x] [line [[@LINE+2]]]
        // FIXME: ![[SCOPEA]] = metadata !{{{.*}}, i32 [[@LINE+1]], {{.*}}} ; [ DW_TAG_lexical_block ]
        case (var x, var y) where x == y:
          println("on the + diagonal")
        case (var x, var y) where x == -y:
          println("on the - diagonal")
        case (-10..10, -10..10):
          println("near the origin")
        case (var x, var y):
          println("\(sqrt(x*x + y*y)) units from the origin")
        }
    }
