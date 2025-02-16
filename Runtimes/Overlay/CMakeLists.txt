cmake_minimum_required(VERSION 3.26...3.29)

project(SwiftOverlay LANGAUGES C VERSION 6.1)

if(WIN32)
  add_subdirectory(Windows)
endif()
