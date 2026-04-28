include("${CMAKE_CURRENT_LIST_DIR}/Windows-Bootstrap-Stage0.cmake")

set(LLVM_DEFAULT_TARGET_TRIPLE x86_64-unknown-windows-msvc CACHE STRING "")
set(LLVM_TARGETS_TO_BUILD X86 CACHE STRING "")
