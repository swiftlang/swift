MEMORY
{
   flash  : ORIGIN = 0x08000000, LENGTH = 32K
   sram   : ORIGIN = 0x20000000, LENGTH = 128K
}

SECTIONS
{
   .text     : { *(.vectors*) ; *(.text*) } > flash
   .bss      : { *(.bss*) } > sram
   .data     : { *(.data*) } > sram
   /DISCARD/ : { *(.swift_modhash*) }
}
