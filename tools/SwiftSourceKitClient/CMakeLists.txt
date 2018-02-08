set(EXTRA_COMPILE_FLAGS "-F" "${SWIFT_LIBRARY_OUTPUT_INTDIR}")
set(EXTRA_LINKER_FLAGS "-Xlinker" "-rpath" "-Xlinker" "${SWIFT_LIBRARY_OUTPUT_INTDIR}"
    "-Xlinker" "-F" "-Xlinker" "${SWIFT_LIBRARY_OUTPUT_INTDIR}")

add_swift_library(swiftSwiftLang SHARED
  SwiftLang.swift
  SourceKitdClient.swift
  SourceKitdRequest.swift
  SourceKitdResponse.swift
  SourceKitdUID.swift

  DEPENDS SourceKitService
  SWIFT_COMPILE_FLAGS ${EXTRA_COMPILE_FLAGS}
  LINK_FLAGS ${EXTRA_LINKER_FLAGS}
  SWIFT_MODULE_DEPENDS Foundation
  INSTALL_IN_COMPONENT swift-syntax
  TARGET_SDKS OSX
  IS_STDLIB)
