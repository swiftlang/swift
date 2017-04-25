set(EXPORTED_SYMBOL_FILE "${SOURCEKITD_SOURCE_DIR}/bin/sourcekitd.exports")

option(SOURCEKITD_BUILD_STATIC_INPROC
  "Build sourcekitdInProc as a static library (in addition to a shared one)" OFF)

set(sourcekitdInProc_args
  sourcekitdInProc.cpp
  DEPENDS SourceKitSwiftLang sourcekitdAPI
  LLVM_COMPONENT_DEPENDS support coverage
)

if (SOURCEKIT_INSTALLING_INPROC)
  add_sourcekit_framework(sourcekitdInProc
    ${SOURCEKITD_SOURCE_DIR}/include/sourcekitd/sourcekitd.h
    ${sourcekitdInProc_args}
    MODULEMAP module.modulemap
    INSTALL_IN_COMPONENT sourcekit-inproc
  )
  set_property(TARGET sourcekitdInProc APPEND_STRING PROPERTY LINK_FLAGS " -fapplication-extension")
else()
  add_sourcekit_library(sourcekitdInProc
    ${sourcekitdInProc_args}
    INSTALL_IN_COMPONENT sourcekit-inproc
    SHARED
  )
endif()

# While it is possible to build this as a static library,
# to get the runtime paths correct, it must be linked into a binary
# that is in the same directory as the swift library directory.
if (SOURCEKITD_BUILD_STATIC_INPROC)
  add_sourcekit_library(sourcekitdInProc_Static
    ${sourcekitdInProc_args}
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

