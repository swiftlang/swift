# Embedded Swift dynamic exclusivity mix-in libraries
if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
  set(EMBEDDED_SWIFT_EXCLUSIVITY_IMPLEMENTATIONS SingleThreaded C11ThreadLocal)
  add_custom_target(embedded-exclusivity)
  add_dependencies(embedded-libraries embedded-exclusivity)

  foreach(entry ${EMBEDDED_STDLIB_TARGET_TRIPLES})
    string(REGEX REPLACE "[ \t]+" ";" list "${entry}")
    list(GET list 0 arch)
    list(GET list 1 mod)
    list(GET list 2 triple)

    if("${mod}" MATCHES "-windows-msvc$")
      continue()
    endif()

    if("${arch}" MATCHES "avr")
      continue()
    endif()

    if (SWIFT_HOST_VARIANT STREQUAL "linux")
      set(extra_c_compile_flags -ffreestanding)
    elseif (SWIFT_HOST_VARIANT STREQUAL "macosx")
      set(extra_c_compile_flags -ffreestanding)
    endif()
    list(APPEND extra_c_compile_flags -nostdinc++)

    set(SWIFT_SDK_embedded_ARCH_${mod}_MODULE "${mod}")
    set(SWIFT_SDK_embedded_LIB_SUBDIR "embedded")
    set(SWIFT_SDK_embedded_ARCH_${mod}_TRIPLE "${triple}")
    if(SWIFT_EMBEDDED_STDLIB_SDKS_FOR_TARGET_TRIPLES)
      set(SWIFT_SDK_embedded_ARCH_${mod}_PATH "${EMBEDDED_STDLIB_SDK_FOR_${triple}}")
    endif()

    foreach(impl ${EMBEDDED_SWIFT_EXCLUSIVITY_IMPLEMENTATIONS})
      # C11 thread-local does not work with -none- triples.
      if ("${impl}" MATCHES "C11ThreadLocal")
        if("${triple}" MATCHES "-none-")
            continue()
        endif()
      endif()

      add_swift_target_library_single(
        embedded-exclusivity-${impl}-${mod}
        swiftExclusivity${impl}
        STATIC
        IS_FRAGILE
        PARTIAL_SOURCES_INTENDED

        ${impl}.cpp

        C_COMPILE_FLAGS ${extra_c_compile_flags}
        MODULE_DIR "${CMAKE_BINARY_DIR}/lib/swift/embedded"
        SDK "embedded"
        ARCHITECTURE "${mod}"
        DEPENDS embedded-stdlib-${mod}
        INSTALL_IN_COMPONENT stdlib
        )
      swift_install_in_component(
        TARGETS embedded-exclusivity-${impl}-${mod}
        DESTINATION "lib/swift/embedded/${mod}"
        COMPONENT "stdlib"
        )
      swift_install_in_component(
        FILES "${SWIFTLIB_DIR}/embedded/${mod}/libswiftExclusivity${impl}.a"
        DESTINATION "lib/swift/embedded/${mod}/"
        COMPONENT "stdlib"
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
        )
      set_property(TARGET embedded-exclusivity-${impl}-${mod} PROPERTY OSX_ARCHITECTURES "${arch}")

      add_dependencies(embedded-exclusivity embedded-exclusivity-${impl}-${mod})
    endforeach()
  endforeach()
endif()
