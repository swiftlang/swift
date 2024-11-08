if(SwiftCore_ENABLE_COMMANDLINE_SUPPORT)
  add_library(swiftCommandLineSupport STATIC CommandLine.cpp)
  target_include_directories(swiftCommandLineSupport PRIVATE
    "${SwiftCore_SWIFTC_SOURCE_DIR}/include"
    "${PROJECT_BINARY_DIR}/include")

  target_link_libraries(swiftCommandLineSupport PRIVATE
    swiftShims)

  if(NOT BUILD_SHARED_LIBS)
    install(TARGETS swiftCommandLineSupport)
  endif()
endif()
