# This module keeps frame pointers in code built for platforms other than
# Windows, so that the backtracer, which walks the frame pointer chain, can
# unwind through the Swift runtime libraries.  Without the flag, the compiler
# is free to use the frame pointer register as a general purpose register.
#
# Windows is excluded because it always has unwind data available, and the
# backtracer will use that instead.
#
# The options apply to targets created after the module is included, in the
# including directory and its subdirectories.

add_compile_options(
  "$<$<AND:$<NOT:$<PLATFORM_ID:Windows>>,$<COMPILE_LANGUAGE:C,CXX>>:-fno-omit-frame-pointer>"
  "$<$<AND:$<NOT:$<PLATFORM_ID:Windows>>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xcc -fno-omit-frame-pointer>")
