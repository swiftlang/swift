// RUN: %swift -triple x86_64-apple-darwin13 %s -emit-llvm -g -o - | FileCheck %s
// Int1 still uses 8 bits of storage.
// CHECK: [ DW_TAG_base_type ] [Builtin.Int1] [line 0, size 8, align 8, offset 0, enc DW_ATE_unsigned]
func main()
{
	var t = true
	var f = false
	println("hello")
}

main()
