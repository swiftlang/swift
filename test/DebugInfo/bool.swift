// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// Int1 uses 1 bit, but is aligned at 8 bits.
// CHECK: [ DW_TAG_base_type ] [_TtBi1_] [line 0, size 1, align 8, offset 0, enc DW_ATE_unsigned]
func main()
{
	var t = true
	var f = false
	println("hello")
}

main()
