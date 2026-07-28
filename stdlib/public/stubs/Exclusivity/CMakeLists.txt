# Embedded Swift dynamic exclusivity mix-in libraries
if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
  set(EMBEDDED_SWIFT_EXCLUSIVITY_IMPLEMENTATIONS SingleThreaded C11ThreadLocal)
  add_custom_target(embedded-exclusivity)
  add_dependencies(embedded-libraries embedded-exclusivity)

  foreach(impl ${EMBEDDED_SWIFT_EXCLUSIVITY_IMPLEMENTATIONS})
    add_custom_target(embedded-exclusivity-${impl})
    add_dependencies(embedded-exclusivity embedded-exclusivity-${impl})

    # C11 thread-local does not work with -none- triples.
    set(per_impl_skip_triple)
    if("${impl}" STREQUAL "C11ThreadLocal")
      list(APPEND per_impl_skip_triple "-none-")
    endif()

    add_embedded_swift_target_library(
      embedded-exclusivity-${impl}
      swiftExclusivity${impl}
      PARTIAL_SOURCES_INTENDED
      INSTALL_BINARY

      ${impl}.cpp

      C_COMPILE_FLAGS -nostdinc++
      SKIP_MOD_REGEX "-windows-msvc$"
      SKIP_ARCH_REGEX "avr"
      SKIP_TRIPLE_REGEX ${per_impl_skip_triple}
      ARCHITECTURE_KEY mod
      INSTALL_IN_COMPONENT stdlib
    )
  endforeach()
endif()
