set(SHARED_LIBRARY ON)
add_swift_library(swiftFoundation INSTALL
  Foundation.swift
  Foundation-macosx.swift
  BridgeNSString.mm
  DEPENDS swift_stdlib_core swiftObjectiveC)

add_swift_optimization_flags(swiftFoundation)

# Link against frameworks
target_link_libraries(swiftFoundation
  "-framework Foundation")
