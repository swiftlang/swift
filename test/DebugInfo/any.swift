// RUN: %target-swift-frontend %use_no_opaque_pointers %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -o -

func markUsed<T>(_ t: T) {}

func main() {
  // CHECK: call void @llvm.dbg.declare(metadata %Any* {{.*}}, metadata ![[S:.*]], metadata !DIExpression()), !dbg ![[DBG:.*]]
  // CHECK: ![[S]] = !DILocalVariable(name: "s", {{.*}}line: [[@LINE+2]]
  // CHECK: ![[DBG]] = !DILocation(line: [[@LINE+1]], column: 7,
  var s: Any = "hello world"
  var n: Any = 12
  var t: Any = (1,2)
  markUsed("hello world")
}

main()
