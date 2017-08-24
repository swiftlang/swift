// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

func main() {
  // CHECK: call void @llvm.dbg.declare(metadata %Any* {{.*}}, metadata ![[S:.*]], metadata !DIExpression()), !dbg ![[DBG:.*]]
  // CHECK: ![[S]] = !DILocalVariable(name: "s", {{.*}}line: [[@LINE+3]]
  // CHECK: ![[SCOPE:.*]] = distinct !DILexicalBlock({{.*}}line: 5, column: 13)
  // CHECK: ![[DBG]] = !DILocation(line: [[@LINE+1]], column: 7, scope: ![[SCOPE:.*]])
  var s: Any = "hello world"
  var n: Any = 12
  var t: Any = (1,2)
  markUsed("hello world")
}

main()
