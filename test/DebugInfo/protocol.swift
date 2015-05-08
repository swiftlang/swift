// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

protocol PointUtils {
  func distanceFromOrigin() -> Float
}


class Point : PointUtils {
    var x : Float
    var y : Float
    init (_x : Float, _y : Float) {
        x = _x;
        y = _y;
    }

    func distanceFromOrigin() -> Float {
        //var distance = sqrt(x*x + y*y)
        var distance: Float = 1.0
        return distance
    }

}

// CHECK-DAG: define hidden i64 @_TF8protocol4mainFT_VSs5Int64() {{.*}} {
func main() -> Int64 {
    var pt = Point(_x: 2.5, _y: 4.25)
// CHECK: [[LOC2D:%[a-zA-Z0-9]+]] = alloca %P8protocol10PointUtils_, align {{(4|8)}}
// CHECK: call void @llvm.dbg.declare(metadata {{.*}} [[LOC2D]], metadata ![[LOC:.*]], metadata !{{[0-9]+}})
    var loc2d : protocol<PointUtils> = pt
    var distance = loc2d.distanceFromOrigin()
    print("hello", appendNewline: false) // Set breakpoint here
    return 0
}

// Self should be artificial.
// CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "self",{{.*}} line: 16
// CHECK-SAME:             DIFlagArtificial

// CHECK: ![[LOC]] = !DILocalVariable(tag: DW_TAG_auto_variable, name: "loc2d",{{.*}} line: [[@LINE-10]]

main()
