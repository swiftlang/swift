set(public_headers
  ${SOURCEKITD_SOURCE_DIR}/include/sourcekitd/sourcekitd.h
)

set(EXPORTED_SYMBOL_FILE "${SOURCEKITD_SOURCE_DIR}/bin/sourcekitd.exports")

# FIXME: If you delete sourcekitd.framework, the soft links inside it need
# a CMake reconfigure in order for them to be created.

add_sourcekit_framework(sourcekitd
  ${public_headers}
  sourcekitd.cpp tracer.cpp
  DEPENDS sourcekitdAPI
  LLVM_COMPONENT_DEPENDS support
  MODULEMAP module.modulemap
  INSTALL_IN_COMPONENT sourcekit-xpc-service
)

add_definitions(-DSOURCEKIT_XPCSERVICE_IDENTIFIER="com.apple.SourceKitService.${SOURCEKIT_VERSION_STRING}")

if (SOURCEKIT_BUILT_STANDALONE)
  # Create the symlink necessary to find the swift stdlib.
  swift_create_post_build_symlink(sourcekitd
    IS_DIRECTORY
    SOURCE "${SWIFTLIB_DIR}"
    DESTINATION "${SOURCEKIT_LIBRARY_OUTPUT_INTDIR}/swift"
    COMMENT "Creating symlink necessary to find the swift stdlib.")
endif()

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  if(SWIFT_COMPILER_VERSION)
    set_property(TARGET sourcekitd APPEND_STRING PROPERTY
                 LINK_FLAGS " -Wl,-current_version -Wl,${SWIFT_COMPILER_VERSION}")
  endif()
endif()

