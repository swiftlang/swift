add_swift_host_library(swiftThreading STATIC
  C11.cpp
  Darwin.cpp
  Linux.cpp
  Nothreads.cpp
  Pthreads.cpp
  Win32.cpp)
