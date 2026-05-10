# Use an 'internal' name, for now
set(SWIFT_SCAN_LIB_NAME "_InternalSwiftStaticMirror")

set(LLVM_EXPORTED_SYMBOL_FILE
    ${CMAKE_CURRENT_SOURCE_DIR}/libStaticMirror.exports)

add_swift_host_library(libStaticMirror SHARED
  libStaticMirror.cpp
  c-include-check.c
  LLVM_LINK_COMPONENTS object support
)
if(NOT SWIFT_BUILT_STANDALONE AND NOT CMAKE_C_COMPILER_ID MATCHES Clang)
  add_dependencies(libStaticMirror clang)
endif()

add_dependencies(libStaticMirror
    swiftStaticMirror)

target_link_libraries(libStaticMirror PRIVATE
    swiftStaticMirror
    swiftRemoteInspection)

set_target_properties(libStaticMirror
    PROPERTIES
    OUTPUT_NAME ${SWIFT_SCAN_LIB_NAME})

add_llvm_symbol_exports(libStaticMirror ${LLVM_EXPORTED_SYMBOL_FILE})

# Adds -dead_strip option
add_link_opts(libStaticMirror)

add_dependencies(static-mirror-lib libStaticMirror)
swift_install_in_component(TARGETS libStaticMirror
  ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}" COMPONENT static-mirror-lib
  LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}" COMPONENT static-mirror-lib
  RUNTIME DESTINATION "bin" COMPONENT static-mirror-lib)
swift_install_in_component(DIRECTORY "${SWIFT_MAIN_INCLUDE_DIR}/swift-c/StaticMirror/"
                           DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SCAN_LIB_NAME}"
                           COMPONENT static-mirror-lib)
