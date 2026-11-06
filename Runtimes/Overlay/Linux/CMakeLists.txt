
defaulted_set(SwiftOverlay_LINUX_LIBC STRING "Select Linux libc flavor. Options are: glibc")
add_subdirectory("${SwiftOverlay_LINUX_LIBC}")

if(NOT BUILD_SHARED_LIBS)
  set(libpthread -lpthread)
  set(android_libraries)
  set(concurrency_libs "-ldispatch -lBlocksRuntime")
  swift_runtime_configure_link_file(
    "${PROJECT_SOURCE_DIR}/static-stdlib-args.lnk.in"
    "${CMAKE_CURRENT_BINARY_DIR}/static-stdlib-args.lnk"
    "${SwiftCore_PAGE_SIZE}")
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/static-stdlib-args.lnk"
    COMPONENT SwiftOverlay_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift_static/${SwiftOverlay_PLATFORM_SUBDIR}")

  swift_runtime_configure_link_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/Resources/static-executable-args.lnk"
    "${CMAKE_CURRENT_BINARY_DIR}/static-executable-args.lnk"
    "${SwiftCore_PAGE_SIZE}")
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/static-executable-args.lnk"
    COMPONENT SwiftOverlay_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift_static/${SwiftOverlay_PLATFORM_SUBDIR}")
endif()
