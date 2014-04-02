set(SHARED_LIBRARY ON)
set(SWIFTSPRITEKIT_SOURCES
    SpriteKit.swift
)

add_swift_library(swiftSpriteKit INSTALL
  ${SWIFTSPRITEKIT_SOURCES}
  DEPENDS swift_stdlib_core swiftObjectiveC swiftFoundation)

add_swift_optimization_flags(swiftSpriteKit)

target_link_libraries(swiftSpriteKit
  "-framework SpriteKit")
