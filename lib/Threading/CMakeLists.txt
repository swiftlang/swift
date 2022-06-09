# If you update this, you also need to update the CMakeLists.txt file in
# stdlib/public/Threading

# Note that it is *not* an error that Errors.cpp is only listed here.
# It shouldn't be in stdlib/public/Threading because that is an OBJECT_LIBRARY
# and things that use that should be defining their own fatal error handler.

add_swift_host_library(swiftThreading STATIC
  C11.cpp
  Linux.cpp
  Pthreads.cpp
  Win32.cpp
  Errors.cpp)
