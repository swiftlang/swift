set(SHARED_LIBRARY ON)
add_swift_library(swiftUIKit INSTALL
  UIKit.swift
  DEPENDS swift_stdlib_core swiftObjectiveC swiftFoundation)

add_swift_optimization_flags(swiftUIKit)

# Link against frameworks
target_link_libraries(swiftUIKit
  "-framework UIKit")
