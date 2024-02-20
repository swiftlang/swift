set(MOCK_PLUGIN_LIB_NAME "_swiftMockPlugin")

set(LLVM_EXPORTED_SYMBOL_FILE
    ${CMAKE_CURRENT_SOURCE_DIR}/libMockPlugin.exports)

add_swift_host_library(libMockPlugin SHARED
  MockPlugin.cpp
  c-include-check.c
  LLVM_LINK_COMPONENTS support)

set_target_properties(libMockPlugin
    PROPERTIES
    OUTPUT_NAME ${MOCK_PLUGIN_LIB_NAME})

add_llvm_symbol_exports(libMockPlugin ${LLVM_EXPORTED_SYMBOL_FILE})

add_dependencies(tools libMockPlugin)
# Adds -dead_strip option
add_link_opts(libMockPlugin)
