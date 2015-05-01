// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

func main() {
  // CHECK: call void @llvm.dbg.declare(metadata %"protocol<>"* {{.*}}, metadata ![[S:.*]], metadata !{{[0-9]+}}), !dbg ![[DBG:.*]]
  // CHECK: ![[S]] = !DILocalVariable(tag: DW_TAG_auto_variable, name: "s", {{.*}}line: [[@LINE+3]]
  // CHECK: ![[SCOPE:.*]] = distinct !DILexicalBlock({{.*}}line: 5, column: 13)
  // CHECK: ![[DBG]] = !DILocation(line: [[@LINE+1]], column: 7, scope: ![[SCOPE]])
  var s: Any = "hello world"
  var n: Any = 12
  var t: Any = (1,2)
  markUsed("hello world")
}

main()
