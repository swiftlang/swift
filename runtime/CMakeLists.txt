set(SHARED_LIBRARY ON)
add_swift_library(swift_runtime
  Alloc.cpp
  Stubs.cpp
  swift.swift)
