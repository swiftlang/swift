// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// Int1 uses 1 bit, but is aligned at 8 bits.
// CHECK: !MDBasicType(name: "_TtBi1_", size: 1, align: 8, encoding: DW_ATE_unsigned)
func main()
{
	var t = true
	var f = false
	println("hello")
}

main()
