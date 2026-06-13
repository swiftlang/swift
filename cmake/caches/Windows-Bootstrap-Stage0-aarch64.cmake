include("${CMAKE_CURRENT_LIST_DIR}/Windows-Bootstrap-Stage0.cmake")

set(LLVM_DEFAULT_TARGET_TRIPLE aarch64-unknown-windows-msvc CACHE STRING "")
set(LLVM_TARGETS_TO_BUILD AArch64 CACHE STRING "")
