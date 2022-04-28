# If you update this, you also need to update the CMakeLists.txt file in
# stdlib/public/Threading

add_swift_host_library(swiftThreading STATIC
  C11.cpp
  Linux.cpp
  Pthreads.cpp
  Win32.cpp)
