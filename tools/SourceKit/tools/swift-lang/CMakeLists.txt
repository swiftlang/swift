if(NOT SWIFT_SOURCEKIT_USE_INPROC_LIBRARY AND SWIFT_BUILD_STDLIB)
  set(EXTRA_COMPILE_FLAGS "-F" "${SWIFT_LIBRARY_OUTPUT_INTDIR}")
  set(SOURCEKITD_LINK_LIBS sourcekitd)
  set(INSTALLED_COMP sourcekit-xpc-service)
  set(DEPENDS_LIST "sourcekitd-test")

  add_swift_library(swiftSwiftLang SHARED
    SwiftLang.swift
    SourceKitdClient.swift
    SourceKitdRequest.swift
    SourceKitdResponse.swift
    SourceKitdUID.swift
    UIDs.swift.gyb

    DEPENDS ${DEPENDS_LIST}
    SWIFT_MODULE_DEPENDS_OSX Darwin
    PRIVATE_LINK_LIBRARIES ${SOURCEKITD_LINK_LIBS}
    SWIFT_COMPILE_FLAGS ${EXTRA_COMPILE_FLAGS}
    INSTALL_IN_COMPONENT ${INSTALLED_COMP}
    TARGET_SDKS ${SOURCEKIT_DEFAULT_TARGET_SDK}
    IS_STDLIB)
endif()
