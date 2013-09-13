// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

protocol PointUtils {
  func distanceFromOrigin () -> Float
}


class Point : PointUtils {
    var x : Float
    var y : Float
    constructor (_x : Float, _y : Float) {
        x = _x;
        y = _y;
    }

    func distanceFromOrigin () -> Float {
        var distance = sqrt(x*x + y*y)
        return distance
    }

}

// CHECK: define i64 @_T8protocol4mainFT_Si() {
func main() -> Int {
    var pt = Point(2.5, 4.25)
// CHECK: = getelementptr inbounds { %swift.refcounted, %P8protocol10PointUtils_ }
// CHECK-NEXT: call void @llvm.dbg.declare(metadata !{{{.*}}}, metadata ![[LOC:.*]])
// CHECK: ![[LOC]] ={{.*}}[ DW_TAG_auto_variable ] [loc2d] [line [[@LINE+1]]]
    var loc2d : protocol<PointUtils> = pt
    var distance = loc2d.distanceFromOrigin()
    print("hello") // Set breakpoint here
    return 0
}

main()
