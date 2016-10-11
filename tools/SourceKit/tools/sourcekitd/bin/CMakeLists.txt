add_subdirectory(InProc)
if (CMAKE_HOST_APPLE)
  add_subdirectory(XPC)
endif()
