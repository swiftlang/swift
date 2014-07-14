set(SHARED_LIBRARY ON)
set(SWIFTUNITTEST_SOURCES
    StdlibUnittest.swift.gyb
)

add_swift_library(swiftStdlibUnittest INSTALL
  ${SWIFTUNITTEST_SOURCES}
  DEPENDS swiftCore swiftDarwin)

