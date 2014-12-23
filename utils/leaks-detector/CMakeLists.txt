add_swift_library(LeaksDetector SHARED
    leaksDetector.c)

swift_get_configuration_types(configs)
foreach(config ${configs})
  set(LEAKS_DETECTOR_LIBRARY_PATH
      "${CMAKE_BINARY_DIR}/${config}/lib/libLeaksDetector${CMAKE_SHARED_LIBRARY_SUFFIX}")
  configure_file(
      "${CMAKE_CURRENT_SOURCE_DIR}/leaks-runner.in"
      "${CMAKE_CURRENT_BINARY_DIR}/${config}/leaks-runner"
      @ONLY
      NEWLINE_STYLE UNIX)
  file(COPY "${CMAKE_CURRENT_BINARY_DIR}/${config}/leaks-runner"
      DESTINATION "${CMAKE_BINARY_DIR}/${config}/bin"
      FILE_PERMISSIONS
        OWNER_READ OWNER_WRITE OWNER_EXECUTE
        GROUP_READ GROUP_EXECUTE
        WORLD_READ WORLD_EXECUTE)

endforeach()

