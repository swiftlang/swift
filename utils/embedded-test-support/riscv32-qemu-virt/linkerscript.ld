MEMORY
{
   ram   : ORIGIN = 0x80000000, LENGTH = 128K
}

SECTIONS
{
   .text     : { *(.start*) ; *(.text*) } > ram
   .bss      : { *(.bss*) } > ram
   .data     : { *(.data*) ; *(.rodata*) } > ram
   /DISCARD/ : { *(.swift_modhash*) }
}
