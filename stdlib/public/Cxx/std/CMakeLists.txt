#
# API Notes for the C++ Standard Library
#

# Keep this list in sync with the files installed in
# Runtimes/Overlay/Cxx/std/CMakeLists.txt.
set(apinotes
  std.apinotes
  std_span.apinotes
  std_string_view.apinotes
)

set(output_dir "${SWIFTLIB_DIR}/apinotes")

set(inputs)
set(outputs)
foreach(input ${apinotes})
  list(APPEND inputs "${CMAKE_CURRENT_SOURCE_DIR}/${input}")
  list(APPEND outputs "${output_dir}/${input}")
endforeach()

add_custom_command_target(unused_var
    CUSTOM_TARGET_NAME CxxStdlib-apinotes
    COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}"
    COMMAND ${CMAKE_COMMAND} "-E" "copy_if_different" ${inputs} "${output_dir}/"
    COMMENT "Copying CxxStdlib API Notes to ${output_dir}"
    OUTPUT ${outputs}
    DEPENDS ${inputs})

swift_install_in_component(FILES ${apinotes}
    DESTINATION "lib/swift/apinotes"
    COMPONENT compiler)

set_property(TARGET CxxStdlib-apinotes PROPERTY FOLDER "Miscellaneous")
add_dependencies(sdk-overlay CxxStdlib-apinotes)
add_dependencies(compiler CxxStdlib-apinotes)


set(swift_cxxstdlib_dependencies)
if(NOT DEFINED SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT OR NOT SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT)
  set(swift_cxxstdlib_dependencies _Builtin_float)
endif()

#
# C++ Standard Library Overlay.
#
# The overlay is fragile (i.e. it does not use library evolution)
# as it's not ABI stable.
set(SWIFT_CXXSTDLIB_SOURCES
    std.swift
    Chrono.swift
    String.swift
)

# Flags shared by the regular and the Embedded Swift builds of the overlay.
set(SWIFT_CXXSTDLIB_FLAGS
    -cxx-interoperability-mode=default

    # This flag is unnecessary when building with newer compilers that allow
    # using C++ symbols in resilient overlays (see f4204568).
    -enable-experimental-feature AssumeResilientCxxTypes
    -enable-experimental-feature Lifetimes
    -enable-experimental-feature AllowUnsafeAttribute
    -strict-memory-safety

    # The varying modularization of the C++ standard library on different
    # platforms makes it difficult to enable MemberImportVisibility for this
    # module
    -disable-upcoming-feature MemberImportVisibility
)

add_swift_target_library(swiftCxxStdlib STATIC NO_LINK_NAME IS_STDLIB IS_SWIFT_ONLY
    ${SWIFT_CXXSTDLIB_SOURCES}

    SWIFT_MODULE_DEPENDS Cxx ${swift_cxxstdlib_dependencies}
    SWIFT_MODULE_DEPENDS_ANDROID Android

    SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${SWIFT_CXXSTDLIB_FLAGS}
    -Xfrontend -module-interface-preserve-types-as-written
    -Xfrontend -disable-module-selectors-in-module-interface

    SWIFT_COMPILE_FLAGS_LINUX
    ${SWIFT_SDK_LINUX_CXX_OVERLAY_SWIFT_COMPILE_FLAGS}

    DEPLOYMENT_VERSION_OSX ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_OSX}
    DEPLOYMENT_VERSION_IOS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_IOS}
    DEPLOYMENT_VERSION_TVOS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_TVOS}
    DEPLOYMENT_VERSION_WATCHOS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_WATCHOS}
    DEPLOYMENT_VERSION_XROS ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_XROS}

    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS ALL_APPLE_PLATFORMS LINUX LINUX_STATIC WINDOWS ANDROID FREEBSD
    MACCATALYST_BUILD_FLAVOR zippered
    INSTALL_IN_COMPONENT compiler
    INSTALL_BINARY_SWIFTMODULE NON_DARWIN_ONLY
    INSTALL_WITH_SHARED
    DEPENDS libstdcxx-modulemap libcxxshim_modulemap CxxStdlib-apinotes)

# stdlib-less builds skip the upper CMakeLists.txt, so the `embedded-libraries`
# target never gets defined.
if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB AND TARGET embedded-libraries)
  add_custom_target(embedded-cxxstdlib)
  add_dependencies(embedded-libraries embedded-cxxstdlib)

  # Unlike the `Cxx` module, this overlay wraps the C++ standard library, so it
  # can only be built for targets that have one. Reuse the same check embedded
  # Concurrency uses rather than a triple list of our own -- but require in
  # addition that the target's *own* SDK provides the library, which is what
  # "supported with no header fixups" means.
  set(_embedded_cxxstdlib_all_triples "${EMBEDDED_STDLIB_TARGET_TRIPLES}")

  foreach(entry ${_embedded_cxxstdlib_all_triples})
    string(REGEX REPLACE "[ \t]+" ";" list "${entry}")
    list(GET list 0 arch)
    list(GET list 1 mod)
    list(GET list 2 triple)

    swift_embedded_libcxx_support("${arch}" "${mod}" "${triple}"
      _embedded_cxxstdlib_has_libcxx _embedded_cxxstdlib_cxx_flags)
    if(NOT _embedded_cxxstdlib_has_libcxx OR _embedded_cxxstdlib_cxx_flags)
      continue()
    endif()

    # Restrict the call below to the current triple by temporarily narrowing the
    # triple list visible to add_embedded_swift_target_library.
    set(EMBEDDED_STDLIB_TARGET_TRIPLES "${entry}")

    add_embedded_swift_target_library(embedded-cxxstdlib swiftCxxStdlib
        STATIC NO_LINK_NAME IS_STDLIB IS_FRAGILE
        ${SWIFT_CXXSTDLIB_SOURCES}

        SWIFT_COMPILE_FLAGS ${SWIFT_CXXSTDLIB_FLAGS}

        # The clang importer needs the target's sysroot to find <string>,
        # <chrono>, and friends; embedded libraries get no SDK by default.
        NEEDS_TARGET_SDK

        DEPENDS embedded-stdlib embedded-cxx libcxxshim_modulemap CxxStdlib-apinotes
        INSTALL_IN_COMPONENT stdlib
    )
  endforeach()

  set(EMBEDDED_STDLIB_TARGET_TRIPLES "${_embedded_cxxstdlib_all_triples}")
endif()
