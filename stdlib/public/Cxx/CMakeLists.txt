add_swift_target_library(swiftCxx ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    CxxSequence.swift

    SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -Xfrontend -enable-experimental-cxx-interop
    # This module should not pull in the C++ standard library, so we disable it explicitly.
    # For functionality that depends on the C++ stdlib, use C++ stdlib overlay (`swiftstd` module).
    -Xcc -nostdinc++

    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    INSTALL_IN_COMPONENT sdk-overlay)

add_subdirectory(std)
# add_subdirectory(cxxshim)
