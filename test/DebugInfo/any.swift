// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func main() {
  // CHECK: call void @llvm.dbg.declare(metadata %"protocol<>"* {{.*}}, metadata ![[S:.*]], metadata !{{[0-9]+}}), !dbg ![[DBG:.*]]
  // CHECK: ![[S]] = !DILocalVariable(tag: DW_TAG_auto_variable, name: "s", {{.*}}line: [[@LINE+3]]
  // CHECK: ![[SCOPE:.*]] = distinct !DILexicalBlock({{.*}}line: 3, column: 13)
  // CHECK: ![[DBG]] = !DILocation(line: [[@LINE+1]], column: 6, scope: ![[SCOPE]])
	var s : Any = "hello world"
	var n : Any = 12
	var t : Any = (1,2)
	println("hello world")
}

main()
