set(SHARED_LIBRARY ON)
set(SWIFTSPRITEKIT_SOURCES
    SpriteKit.swift
)

if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
  set(SWIFTSPRITEKIT_UI swiftAppKit)
else()
  set(SWIFTSPRITEKIT_UI swiftUIKit)
endif()

add_swift_library(swiftSpriteKit INSTALL
  ${SWIFTSPRITEKIT_SOURCES}
  DEPENDS swift_stdlib_core swiftObjectiveC swiftFoundation ${SWIFTSPRITEKIT_UI})

add_swift_optimization_flags(swiftSpriteKit)

target_link_libraries(swiftSpriteKit
  "-framework SpriteKit")
