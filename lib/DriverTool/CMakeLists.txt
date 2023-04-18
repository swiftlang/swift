set(driver_sources_and_options
                driver.cpp
                autolink_extract_main.cpp
                modulewrap_main.cpp
                swift_api_digester_main.cpp
                swift_cache_tool_main.cpp
                swift_indent_main.cpp
                swift_symbolgraph_extract_main.cpp
                swift_api_extract_main.cpp)

set(driver_common_libs
                swiftAPIDigester
                swiftDriver
                swiftFrontendTool
                swiftSymbolGraphGen
                LLVMBitstreamReader)

add_swift_host_library(swiftDriverTool STATIC
  ${driver_sources_and_options}
)
target_link_libraries(swiftDriverTool
                      PUBLIC
                      ${driver_common_libs})

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swiftDriverTool clang-resource-headers)
endif()

set_swift_llvm_is_available(swiftDriverTool)

set(LLVM_TARGET_DEFINITIONS SwiftCacheToolOptions.td)
swift_tablegen(SwiftCacheToolOptions.inc -gen-opt-parser-defs)
swift_add_public_tablegen_target(SwiftCacheToolOptions)

add_dependencies(swiftDriverTool SwiftCacheToolOptions)
