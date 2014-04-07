set(SHARED_LIBRARY ON)
set(SWIFTCOREGRAPHICS_SOURCES
    CoreGraphics.swift
)

add_swift_library(swiftCoreGraphics INSTALL
  ${SWIFTCOREGRAPHICS_SOURCES}
  DEPENDS swift_stdlib_core swiftObjectiveC)

add_swift_optimization_flags(swiftCoreGraphics)

target_link_libraries(swiftCoreGraphics
  "-framework CoreGraphics")
