cmake_minimum_required(VERSION 3.26)
include("${CMAKE_CURRENT_LIST_DIR}/../../../Runtimes/cmake/modules/SwiftRuntimePageSize.cmake")

if(DEFINED PAGE_SIZE)
  swift_runtime_validate_page_size("${PAGE_SIZE}")
  return()
endif()

function(check_page_size triple configured expected)
  swift_runtime_get_page_size(actual "${triple}" "${configured}")
  if(NOT actual EQUAL expected)
    message(FATAL_ERROR "${triple}: expected ${expected}, got ${actual}")
  endif()
endfunction()

# Defaults follow the target even when several targets share one build host.
check_page_size(aarch64-unknown-linux-gnu "" 65536)
check_page_size(aarch64-swift-linux-musl "" 65536)
check_page_size(x86_64-unknown-linux-gnu "" 4096)
check_page_size(arm64-apple-macosx "" 16384)
check_page_size(x86_64-apple-macosx "" 4096)
check_page_size(aarch64-unknown-linux-android "" 16384)
check_page_size(x86_64-unknown-linux-android "" 16384)
check_page_size(x86_64-unknown-windows-msvc "" 4096)
check_page_size(wasm32-unknown-wasi "" 4096)
check_page_size(aarch64-unknown-linux-gnu 4096 4096)
check_page_size(aarch64-unknown-linux-gnu 16384 16384)
check_page_size(x86_64-unknown-linux-gnu 65536 65536)
check_page_size(aarch64-unknown-linux-gnu 0x10000 65536)

# Regeneration must replace the previous setting, preserving existing driver
# arguments and substitutions without accumulating conflicting linker flags.
file(MAKE_DIRECTORY "${OUTPUT_DIR}")
file(WRITE "${OUTPUT_DIR}/input.lnk.in" "-lswiftCore\n@extra_library@\n")
set(extra_library -lpthread)
swift_runtime_configure_link_file(
  "${OUTPUT_DIR}/input.lnk.in" "${OUTPUT_DIR}/output.lnk" 4096)
swift_runtime_configure_link_file(
  "${OUTPUT_DIR}/input.lnk.in" "${OUTPUT_DIR}/output.lnk" 65536)
file(READ "${OUTPUT_DIR}/output.lnk" actual)
if(NOT actual STREQUAL "-lswiftCore\n-lpthread\n\n-Wl,-z,max-page-size=65536\n")
  message(FATAL_ERROR "Unexpected static runtime link arguments: ${actual}")
endif()
