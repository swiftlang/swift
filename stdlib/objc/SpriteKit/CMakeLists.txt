set(SHARED_LIBRARY ON)

if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
  set(SWIFTSPRITEKIT_UI swiftAppKit)
else()
  set(SWIFTSPRITEKIT_UI swiftUIKit)
endif()

add_swift_library(swiftSpriteKit INSTALL
  SpriteKit.swift
  SpriteKitMirrors.swift.gyb
  DEPENDS ${SWIFTSPRITEKIT_UI} swiftGLKit
  FRAMEWORK_DEPENDS SpriteKit)
if (SWIFT_SDKS)
  add_swift_library(swiftSpriteKit INSTALL TARGET_LIBRARY
    SpriteKit.swift
    SpriteKitMirrors.swift.gyb
    TARGET_SDKS IOS IOS_SIMULATOR
    DEPENDS swiftUIKit swiftGLKit
    FRAMEWORK_DEPENDS SpriteKit)
endif()
