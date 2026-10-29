  .section __TEXT,__swift5_typeref
l_name_8:
  .asciz "Bi8_"
l_name_32:
  .asciz "Bi32_"
l_name_64:
  .asciz "Bi64_"
l_name_128:
  .asciz "Bi128_"

  .section __TEXT,__swift5_builtin
  .p2align 2
l_desc_8:
  .long l_name_8 - l_desc_8
  .long 1          // Size
  .long 0x10001    // AlignmentAndFlags: alignment 1, bitwise takable
  .long 1          // Stride
  .long 0          // NumExtraInhabitants
l_desc_32:
  .long l_name_32 - l_desc_32
  .long 16         // Size, larger than Stride
  .long 0x10001
  .long 4
  .long 0
l_desc_64:
  .long l_name_64 - l_desc_64
  .long 0x80000000
  .long 0x10001
  .long 0x80000000
  .long 0
l_desc_128:
  .long l_name_128 - l_desc_128
  .long 0xfffffff0
  .long 0x10010    // alignment 16, bitwise takable
  .long 0xfffffff0
  .long 0
