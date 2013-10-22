if (XCODE)
  # Assembly files with Xcode generator require cmake 2.8.10.
  cmake_minimum_required(VERSION 2.8.10)
  set(CMAKE_ASM_COMPILER_ID "Clang")
  enable_language(ASM)
else ()
  # FIXME: CMake assembler rules don't pass things 
  # like architecture flags with the makefile generator.
  SET_SOURCE_FILES_PROPERTIES(FastEntryPoints.s PROPERTIES LANGUAGE C)
endif()

add_swift_library(swift_runtime INSTALL
  FastEntryPoints.s
  Alloc.cpp
  KnownMetadata.cpp
  Metadata.cpp
  Stubs.cpp
  SwiftObject.mm
  Enum.cpp
  Failure.cpp
  BlockShims.mm)

add_swift_optimization_flags(swift_runtime)

set(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS OFF
    CACHE BOOL "Overwrite memory for deallocated Swift objects")
if(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS)
  add_swift_compiler_flags(swift_runtime "-DSWIFT_RUNTIME_CLOBBER_FREED_OBJECTS=1")
endif()
