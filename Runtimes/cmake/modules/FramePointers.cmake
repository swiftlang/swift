# This module keeps frame pointers in code built for Linux and Android, so that
# the backtracer, which walks the frame pointer chain, can unwind through the
# Swift runtime libraries.  Without the flag, the compiler is free to use the
# frame pointer register as a general purpose register.
#
# The options apply to targets created after the module is included, in the
# including directory and its subdirectories.

add_compile_options(
  "$<$<COMPILE_LANGUAGE:C,CXX>:-fno-omit-frame-pointer>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fno-omit-frame-pointer>")
