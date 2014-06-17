set(SHARED_LIBRARY ON)
set(SWIFTUNITTEST_SOURCES
    StdlibUnittest.swift.gyb
)

add_swift_library(swiftStdlibUnittest INSTALL
  ${SWIFTUNITTEST_SOURCES}
  DEPENDS swift_stdlib_core swiftDarwin)

add_swift_optimization_flags(swiftStdlibUnittest)

