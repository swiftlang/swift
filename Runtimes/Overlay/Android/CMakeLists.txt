
add_subdirectory(clang)
add_subdirectory(Android)
add_subdirectory(Math)

if(NOT BUILD_SHARED_LIBS)
  set(libpthread)
  set(android_libraries -llog)
  set(concurrency_libs)
  configure_file(
    "${PROJECT_SOURCE_DIR}/static-stdlib-args.lnk.in"
    static-stdlib-args.lnk
    @ONLY
    NEWLINE_STYLE LF)
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/static-stdlib-args.lnk"
    COMPONENT SwiftOverlay_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift_static/${SwiftOverlay_PLATFORM_SUBDIR}")
endif()
