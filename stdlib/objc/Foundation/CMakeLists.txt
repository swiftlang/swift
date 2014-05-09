set(SHARED_LIBRARY ON)
set(SWIFTFOUNDATION_SOURCES
    Foundation.swift
    KVO.swift
    NSStringAPI.swift
    Misc.mm
)

add_swift_library(swiftFoundation INSTALL
  ${SWIFTFOUNDATION_SOURCES}
  DEPENDS swift_stdlib_core swiftObjectiveC swiftCoreGraphics swiftDispatch)

add_swift_optimization_flags(swiftFoundation)

# Link against frameworks
target_link_libraries(swiftFoundation
  "-framework Foundation")
