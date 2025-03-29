MEMORY
{
   flash_boot  : ORIGIN = 0x00000000, LENGTH = 0x200
   flash_sram  : ORIGIN = 0x00000200, LENGTH = 7K
   flash_stack : ORIGIN = 0x00001e00, LENGTH = 1K
   flash_text  : ORIGIN = 0x00002200, LENGTH = 32K
}

SECTIONS
{
   .boot     : { *(.start) } > flash_boot
   .data     : { *(.bss*) ; *(.data*) ; *(.rodata*) } > flash_sram
   .text     : { *(.text*) } > flash_text
   /DISCARD/ : { *(.swift_modhash*) }
}
