add_library(swiftConcurrencyInternalShims INTERFACE)
target_include_directories(swiftConcurrencyInternalShims INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

install(TARGETS swiftConcurrencyInternalShims
  EXPORT SwiftCoreTargets
  COMPONENT SwiftCore_runtime)

