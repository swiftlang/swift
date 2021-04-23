
add_swift_host_library(LibSwiftOptimizer
  SHARED
  PURE_SWIFT
  PassRegistration.swift
  PassUtils.swift)

target_link_libraries(LibSwiftOptimizer PRIVATE LibSwiftSIL)
