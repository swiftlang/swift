set(EXPORTED_SYMBOL_FILE "${SOURCEKITD_SOURCE_DIR}/bin/sourcekitd.exports")

if (SOURCEKIT_INSTALLING_INPROC)
  add_sourcekit_framework(sourcekitdInProc
    ${SOURCEKITD_SOURCE_DIR}/include/sourcekitd/sourcekitd.h
    sourcekitdInProc.cpp
    DEPENDS SourceKitSwiftLang sourcekitdAPI
    LLVM_COMPONENT_DEPENDS support
    MODULEMAP module.modulemap
    INSTALL_IN_COMPONENT sourcekit-inproc
  )
  set_property(TARGET sourcekitdInProc APPEND_STRING PROPERTY LINK_FLAGS " -fapplication-extension")
else()
  add_sourcekit_library(sourcekitdInProc
    sourcekitdInProc.cpp
    DEPENDS SourceKitSwiftLang sourcekitdAPI
    LLVM_COMPONENT_DEPENDS support
    INSTALL_IN_COMPONENT sourcekit-inproc

    # Note that this cannot be a static library due to the way it looks for the
    # shared library path in order to get at the runtime lib path.
    SHARED
  )
endif()

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  target_link_libraries(sourcekitdInProc PUBLIC BlocksRuntime)
endif()

if (SOURCEKIT_BUILT_STANDALONE)
  # Create the symlinks necessary to find the swift runtime.
  add_custom_command(TARGET sourcekitdInProc PRE_BUILD
    COMMAND ln "-fhs" "${PATH_TO_SWIFT_BUILD}/lib/swift" "${SOURCEKIT_BINARY_DIR}/lib")
endif()

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  if(SWIFT_COMPILER_VERSION)
    set_property(TARGET sourcekitdInProc APPEND_STRING PROPERTY
                 LINK_FLAGS " -Wl,-current_version -Wl,${SWIFT_COMPILER_VERSION}")
  endif()
endif()

