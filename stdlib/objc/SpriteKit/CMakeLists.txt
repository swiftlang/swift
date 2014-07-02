set(SHARED_LIBRARY ON)

if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
  set(SWIFTSPRITEKIT_UI swiftAppKit)
else()
  set(SWIFTSPRITEKIT_UI swiftUIKit)
endif()

add_swift_library(swiftSpriteKit INSTALL
  SpriteKit.swift
  DEPENDS swift_stdlib_core swiftObjectiveC swiftFoundation ${SWIFTSPRITEKIT_UI}  FRAMEWORK_DEPENDS SpriteKit)
