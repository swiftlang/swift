# FIXME: We'd love to just enable the assembler support in CMake, but
# it doesn't work with the Xcode generator. Lame!
add_swift_library(swift_runtime
  FastEntryPoints.s
  Alloc.cpp
  KnownMetadata.cpp
  Metadata.cpp
  Stubs.cpp
  ObjCBridge.mm)

if(SWIFT_OPTIMIZED)
  if (CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
   set_target_properties(swift_runtime PROPERTIES
                                      COMPILE_FLAGS "-momit-leaf-frame-pointer")
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
   set_target_properties(swift_runtime PROPERTIES
                                      COMPILE_FLAGS "-momit-leaf-frame-pointer")
 endif()
endif()

# Link against Foundation, for ObjC bridging.
set_target_properties(swift_runtime PROPERTIES 
                                    LINK_FLAGS "-framework Foundation")
add_dependencies(swift_runtime swift)
