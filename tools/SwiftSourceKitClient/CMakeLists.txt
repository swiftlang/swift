set(EXTRA_COMPILE_FLAGS "-F" "${SWIFT_LIBRARY_OUTPUT_INTDIR}")
set(EXTRA_LINKER_FLAGS "-Xlinker" "-rpath" "-Xlinker" "${SWIFT_LIBRARY_OUTPUT_INTDIR}"
    "-Xlinker" "-F" "-Xlinker" "${SWIFT_LIBRARY_OUTPUT_INTDIR}")

add_swift_library(swiftSwiftSourceKit SHARED
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SourceKitdClient.swift
  SourceKitdRequest.swift
  SourceKitdResponse.swift
  SourceKitdUID.swift

  DEPENDS sourcekitd
  SWIFT_COMPILE_FLAGS ${EXTRA_COMPILE_FLAGS}
  LINK_FLAGS ${EXTRA_LINKER_FLAGS}
  SWIFT_MODULE_DEPENDS Foundation
  INSTALL_IN_COMPONENT swift-syntax
  TARGET_SDKS OSX
  IS_STDLIB)
