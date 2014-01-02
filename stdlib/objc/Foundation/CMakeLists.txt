set(SHARED_LIBRARY ON)
set(SWIFTFOUNDATION_SOURCES
    Foundation.swift
    NSStringAPI.swift
    Misc.mm
)

if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
  list(APPEND SWIFTFOUNDATION_SOURCES 
    Foundation-macosx.swift)
endif()

add_swift_library(swiftFoundation INSTALL
  ${SWIFTFOUNDATION_SOURCES}
  DEPENDS swift_stdlib_core swiftObjectiveC)

add_swift_optimization_flags(swiftFoundation)

# Link against frameworks
target_link_libraries(swiftFoundation
  "-framework Foundation")
