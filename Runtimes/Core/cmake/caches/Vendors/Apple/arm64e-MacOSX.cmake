if(NOT DEFINED CMAKE_OSX_DEPLOYMENT_TARGET)
  message(SEND_ERROR "CMAKE_OSX_DEPLOYMENT_TARGET not defined")
endif()

set(CMAKE_C_COMPILER_TARGET "arm64e-apple-macosx${CMAKE_OSX_DEPLOYMENT_TARGET}" CACHE STRING "")
set(CMAKE_CXX_COMPILER_TARGET "arm64e-apple-macosx${CMAKE_OSX_DEPLOYMENT_TARGET}" CACHE STRING "")
set(CMAKE_Swift_COMPILER_TARGET "arm64e-apple-macosx${CMAKE_OSX_DEPLOYMENT_TARGET}" CACHE STRING "")

list(APPEND CMAKE_C_FLAGS "-darwin-target-variant" "arm64e-apple-ios13.1-macabi")
list(APPEND CMAKE_CXX_FLAGS "-darwin-target-variant" "arm64e-apple-ios13.1-macabi")
list(APPEND CMAKE_Swift_FLAGS "-target-variant" "arm64e-apple-ios13.1-macabi")

include("${CMAKE_CURRENT_LIST_DIR}/apple-common.cmake")
