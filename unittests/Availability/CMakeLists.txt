set(LLVM_LINK_COMPONENTS
    core)
add_swift_unittest(SwiftAvailabilityTests
  VersionRangeLattice.cpp
)

target_link_libraries(SwiftAvailabilityTests
   swiftSema
)
