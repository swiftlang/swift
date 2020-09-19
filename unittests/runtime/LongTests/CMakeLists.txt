if(("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SDK}") AND
   ("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "${SWIFT_PRIMARY_VARIANT_ARCH}"))

  set(PLATFORM_SOURCES)
  set(PLATFORM_TARGET_LINK_LIBRARIES)

  if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
#   Placeholder for Darwin Foundation tests (currently none)
#    find_library(FOUNDATION_LIBRARY Foundation)
#    list(APPEND PLATFORM_SOURCES
#      YourTestNameHere.mm
#      )
#    # We need to link swiftCore on Darwin because the runtime still relies on
#    # some stdlib hooks to implement SwiftObject.
#    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES
#      ${FOUNDATION_LIBRARY}
#      swiftStdlibUnittest${SWIFT_PRIMARY_VARIANT_SUFFIX}
#      )

    # Link the Objective-C runtime.
    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES "objc")
  elseif(SWIFT_HOST_VARIANT STREQUAL "freebsd")
    find_library(EXECINFO_LIBRARY execinfo)
    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES
      ${EXECINFO_LIBRARY}
      )
  elseif(SWIFT_HOST_VARIANT STREQUAL windows)
    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES DbgHelp)
  endif()

  add_swift_unittest(SwiftRuntimeLongTests
    LongRefcounting.cpp
    ../Stdlib.cpp
    ${PLATFORM_SOURCES}

    # The runtime tests link to internal runtime symbols, which aren't exported
    # from the swiftCore dylib, so we need to link to both the runtime archive
    # and the stdlib.
    $<TARGET_OBJECTS:swiftRuntime${SWIFT_PRIMARY_VARIANT_SUFFIX}>
    $<TARGET_OBJECTS:swiftLLVMSupport${SWIFT_PRIMARY_VARIANT_SUFFIX}>
    $<TARGET_OBJECTS:swiftDemangling${SWIFT_PRIMARY_VARIANT_SUFFIX}>
    )

  # The local stdlib implementation provides definitions of the swiftCore
  # interfaes to avoid pulling in swiftCore itself.  Build the
  # SwiftRuntimeLongTests with swiftCore_EXPORTS to permit exporting the stdlib
  # interfaces.
  target_compile_definitions(SwiftRuntimeLongTests
                             PRIVATE
                               swiftCore_EXPORTS
                               SWIFT_INLINE_NAMESPACE=__runtime)

  # FIXME: cross-compile for all variants.
  target_link_libraries(SwiftRuntimeLongTests
    PRIVATE
    swiftCore${SWIFT_PRIMARY_VARIANT_SUFFIX}
    ${PLATFORM_TARGET_LINK_LIBRARIES}
    ${swift_runtime_test_extra_libraries}
    )

  if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
    # Because we link libswiftCore with the install_name_dir as /usr/lib/swift
    # it gets installed as this name as a dependency for this unit test. This is
    # incorrect because at load time we look at the system library vs. the
    # just built stdlib that we statically link against. Use install_name_tool
    # to change the dylib name in the unit test.
    add_custom_command(TARGET SwiftRuntimeLongTests
      POST_BUILD COMMAND
      ${CMAKE_INSTALL_NAME_TOOL} -change /usr/lib/swift/libswiftCore.dylib @rpath/libswiftCore.dylib
      $<TARGET_FILE:SwiftRuntimeLongTests>)
  endif()
endif()

