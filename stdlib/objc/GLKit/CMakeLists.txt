set(SHARED_LIBRARY ON)

if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
  set(SWIFTGLKIT_UI swiftAppKit)
else()
  set(SWIFTGLKIT_UI swiftUIKit)
endif()

add_swift_library(swiftGLKit INSTALL
  GLKit.swift.gyb
  DEPENDS ${SWIFTGLKIT_UI}
  FRAMEWORK_DEPENDS GLKit)

if (SWIFT_SDKS)
  add_swift_library(swiftGLKit INSTALL TARGET_LIBRARY
    GLKit.swift.gyb
    TARGET_SDKS IOS IOS_SIMULATOR
    DEPENDS swiftUIKit
    FRAMEWORK_DEPENDS GLKit)
endif()
