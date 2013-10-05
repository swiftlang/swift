# FIXME: We'd love to just enable the assembler support in CMake, but
# it doesn't work with the Xcode generator. Lame!
add_swift_library(swift_runtime INSTALL
  FastEntryPoints.s
  Alloc.cpp
  KnownMetadata.cpp
  Metadata.cpp
  Stubs.cpp
  SwiftObject.mm
  Enum.cpp
  BlockShims.mm)

add_swift_optimization_flags(swift_runtime)

set(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS OFF
    CACHE BOOL "Overwrite memory for deallocated Swift objects")
if(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS)
  add_swift_compiler_flags(swift_runtime "-DSWIFT_RUNTIME_CLOBBER_FREED_OBJECTS=1")
endif()

add_swift_compiler_flags(swift_runtime "-isysroot ${MODULES_SDK}")
