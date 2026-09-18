
defaulted_set(SwiftOverlay_LINUX_LIBC STRING "Select Linux libc flavor. Options are: glibc")
add_subdirectory("${SwiftOverlay_LINUX_LIBC}")

if(NOT BUILD_SHARED_LIBS)
  set(libpthread -lpthread)
  set(android_libraries)
  set(concurrency_libs "-ldispatch -lBlocksRuntime")
  configure_file(
    "${PROJECT_SOURCE_DIR}/static-stdlib-args.lnk.in"
    static-stdlib-args.lnk
    @ONLY
    NEWLINE_STYLE LF)
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/static-stdlib-args.lnk"
    COMPONENT SwiftOverlay_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift_static/${SwiftOverlay_PLATFORM_SUBDIR}")

  install(FILES Resources/static-executable-args.lnk
    COMPONENT SwiftOverlay_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift_static/${SwiftOverlay_PLATFORM_SUBDIR}")
endif()
