if(SWIFT_HOST_VARIANT MATCHES "^iphoneos")
  set(SWIFT_LANG_TARGET_SDK "IOS")
elseif(SWIFT_HOST_VARIANT MATCHES "^iphonesimulator")
  set(SWIFT_LANG_TARGET_SDK "IOS_SIMULATOR")
elseif(SWIFT_HOST_VARIANT MATCHES "^macosx")
  set(SWIFT_LANG_TARGET_SDK "OSX")
else()
  message(SEND_ERROR "Unknown SDK for SwiftLang")
endif()

add_swift_target_library(swiftSwiftLang SHARED
  SwiftLang.swift
  SourceKitdClient.swift
  SourceKitdRequest.swift
  SourceKitdResponse.swift
  SourceKitdUID.swift

  GYB_SOURCES
    UIDs.swift.gyb

  DEPENDS sourcekit-xpc-service
  SWIFT_MODULE_DEPENDS_OSX Darwin Foundation
  PRIVATE_LINK_LIBRARIES sourcekitd
  SWIFT_COMPILE_FLAGS -F${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib
  INSTALL_IN_COMPONENT sourcekit-xpc-service
  DARWIN_INSTALL_NAME_DIR "@rpath"
  TARGET_SDKS ${SWIFT_LANG_TARGET_SDK}
  IS_STDLIB)
