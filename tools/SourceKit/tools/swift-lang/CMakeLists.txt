if(NOT SWIFT_SOURCEKIT_USE_INPROC_LIBRARY AND SWIFT_BUILD_STDLIB AND SWIFT_BUILD_SDK_OVERLAY)
  # The build type of swiftlang should agree with stdlib
  # This setting could avoid us adding additional search paths when building
  # executables using SwiftLang.
  set(SWIFT_LANG_LIBRARY_BUILD_TYPES)
  if(SWIFT_BUILD_DYNAMIC_STDLIB)
    list(APPEND SWIFT_LANG_LIBRARY_BUILD_TYPES SHARED)
  endif()
  if(SWIFT_BUILD_STATIC_STDLIB)
    list(APPEND SWIFT_LANG_LIBRARY_BUILD_TYPES STATIC)
  endif()

  add_swift_target_library(swiftSwiftLang ${SWIFT_LANG_LIBRARY_BUILD_TYPES}
    SwiftLang.swift
    SourceKitdClient.swift
    SourceKitdRequest.swift
    SourceKitdResponse.swift
    SourceKitdUID.swift

    GYB_SOURCES
      UIDs.swift.gyb

    DEPENDS sourcekitd-test
    SWIFT_MODULE_DEPENDS_OSX Darwin Foundation
    PRIVATE_LINK_LIBRARIES sourcekitd
    SWIFT_COMPILE_FLAGS -F${SWIFT_LIBRARY_OUTPUT_INTDIR}
    INSTALL_IN_COMPONENT sourcekit-xpc-service
    DARWIN_INSTALL_NAME_DIR "@rpath"
    TARGET_SDKS ${SOURCEKIT_DEFAULT_TARGET_SDK}
    IS_STDLIB)
endif()
