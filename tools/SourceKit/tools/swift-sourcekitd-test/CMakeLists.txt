if(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
  set(EXTRA_COMPILE_FLAGS "-I" "${SOURCEKITD_SOURCE_DIR}/include/sourcekitd" "-module-name" "main")
else()
  set(EXTRA_COMPILE_FLAGS "-F" "${SWIFT_LIBRARY_OUTPUT_INTDIR}" "-module-name" "main")
endif()

add_swift_host_tool(swift-sourcekitd-test
  main.swift
  empty.c # FIXME: If there is no C file in the target Xcode skips the linking phase and doesn't create the executable
  COMPILE_FLAGS ${EXTRA_COMPILE_FLAGS}
  SWIFT_MODULE_DEPENDS SwiftLang TestUtils
  HAS_SWIFT_CONTENT
)
