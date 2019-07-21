set(swift_runtime_compile_flags ${SWIFT_RUNTIME_CORE_CXX_FLAGS})
set(swift_runtime_linker_flags ${SWIFT_RUNTIME_CORE_LINK_FLAGS})

add_swift_target_library(swiftCompatibility50 TARGET_LIBRARY STATIC INSTALL_WITH_SHARED
  ProtocolConformance.cpp
  Overrides.cpp
  C_COMPILE_FLAGS ${swift_runtime_library_compile_flags}
  LINK_FLAGS ${swift_runtime_linker_flags}
  TARGET_SDKS ${SWIFT_APPLE_PLATFORMS}
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT compiler)


