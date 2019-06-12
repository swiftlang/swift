set(swift_runtime_compile_flags ${SWIFT_RUNTIME_CORE_CXX_FLAGS})
set(swift_runtime_linker_flags ${SWIFT_RUNTIME_CORE_LINK_FLAGS})

set(swift_compatibility_swift_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")
list(APPEND swift_compatibility_swift_flags "-DCOMPATIBILITY_LIBRARY" "-DCOMPATIBILITY_LIBRARY_50" "-parse-stdlib")

add_swift_target_library(swiftCompatibility50
  TARGET_LIBRARY STATIC INSTALL_WITH_SHARED HAS_SWIFT_CONTENT
  KeyPath.cpp
  Overrides.cpp
  ProtocolConformance.cpp
  ../core/KeyPathInstantiation.swift
  # The copy_shim_headers target dependency is required to let the
  # build system know that there's a rule to produce the shims
  # directory, but is not sufficient to cause the object file to be rebuilt
  # when the shim header changes.  Therefore, we pass both the target
  # and the generated directory as dependencies.
  FILE_DEPENDS
    copy_shim_headers "${SWIFTLIB_DIR}/shims"
  C_COMPILE_FLAGS ${swift_runtime_library_compile_flags}
  SWIFT_COMPILE_FLAGS ${swift_compatibility_swift_flags}
  LINK_FLAGS ${swift_runtime_linker_flags}
  TARGET_SDKS ${SWIFT_APPLE_PLATFORMS}
  MODULE_DEPENDS Swift
  INSTALL_IN_COMPONENT compiler)
