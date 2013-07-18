set(SHARED_LIBRARY ON)
add_swift_library(swiftAppKit INSTALL
  AppKit.swift
  DEPENDS swift_stdlib_core swiftObjectiveC swiftFoundation)

add_swift_optimization_flags(swiftAppKit)

# Link against frameworks
target_link_libraries(swiftAppKit
  "-framework AppKit")
