set(SHARED_LIBRARY ON)
add_swift_library(swiftQuartzCore INSTALL
  QuartzCore.swift
  DEPENDS swift_stdlib_core swiftFoundation)

add_swift_optimization_flags(swiftQuartzCore)

# Link against frameworks
target_link_libraries(swiftQuartzCore
  "-framework QuartzCore")
