
if (APPLE)
  set(SHARED_LIBRARY YES)
  add_swift_library(LeaksDetector leaksDetector.c)
  get_property(LEAKS_DETECTOR_LIBRARY_PATH TARGET LeaksDetector PROPERTY LOCATION)
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/leaks-runner.in
    ${CMAKE_CURRENT_BINARY_DIR}/leaks-runner
    @ONLY
    NEWLINE_STYLE UNIX)
  add_custom_target(leaks-runner-copy ALL
                    COMMAND mkdir -p ${SWIFT_BINARY_DIR}/bin
                    COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/leaks-runner ${SWIFT_BINARY_DIR}/bin/leaks-runner
                    COMMAND chmod u+x ${SWIFT_BINARY_DIR}/bin/leaks-runner
                    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/leaks-runner)
endif()
