// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func main() {
  // CHECK: call void @llvm.dbg.declare(metadata %"protocol<>"* {{.*}}, metadata ![[S:.*]], metadata !{{[0-9]+}}), !dbg ![[DBG:.*]]
  // CHECK: ![[S]] ={{.*}}[ DW_TAG_auto_variable ] [s] [line [[@LINE+3]]]
  // CHECK: ![[SCOPE:.*]] = !{!"0xb\003\0013\001"{{.*}}} ; [ DW_TAG_lexical_block ]
  // CHECK: ![[DBG]] = !MDLocation(line: [[@LINE+1]], column: 6, scope: ![[SCOPE]])
	var s : Any = "hello world"
	var n : Any = 12
	var t : Any = (1,2)
	println("hello world")
}

main()
