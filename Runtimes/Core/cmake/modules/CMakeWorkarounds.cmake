# This file contains workarounds for dealing with bugs between CMake and the
# Swift compiler.

# FIXME: This is a workaround for the broken Swift compiler that cannot
#        typecheck the Swift stdlib without crashing. <ADD RADAR HERE>.
#        This is manipulating undocumented CMake-internal variables that may
#        change behavior at any time and should not be relied on.
set(CMAKE_Swift_NUM_THREADS 0)
if(POLICY CMP0157)
  set(CMAKE_Swift_COMPILE_OBJECT "<CMAKE_Swift_COMPILER> -num-threads ${CMAKE_Swift_NUM_THREADS} -c <DEFINES> <FLAGS> <INCLUDES> <SOURCE>")

  set(CMAKE_Swift_COMPILATION_MODE wholemodule)

  if(NOT SwiftStdlib_NUM_LINK_JOBS MATCHES "^[0-9]+$")
    cmake_host_system_information(RESULT SwiftStdlib_NUM_LINK_JOBS QUERY NUMBER_OF_LOGICAL_CORES)
  endif()

  set(CMAKE_Swift_LINK_EXECUTABLE "<CMAKE_Swift_COMPILER> -num-threads ${SwiftStdlib_NUM_LINK_JOBS} -emit-executable -o <TARGET> <FLAGS> <OBJECTS> <LINK_FLAGS> <LINK_LIBRARIES>")
  set(CMAKE_Swift_CREATE_SHARED_LIBRARY "<CMAKE_Swift_COMPILER> -num-threads ${SwiftStdlib_NUM_LINK_JOBS} -emit-library <CMAKE_SHARED_LIBRARY_Swift_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> ${CMAKE_Swift_IMPLIB_LINKER_FLAGS} <SONAME_FLAG> <TARGET_INSTALLNAME_DIR><TARGET_SONAME> -o <TARGET> <OBJECTS> <LINK_LIBRARIES>")
else()
  set(CMAKE_Swift_CREATE_SHARED_LIBRARY "<CMAKE_Swift_COMPILER> -num-threads ${CMAKE_Swift_NUM_THREADS} -emit-library -o <TARGET> -module-name <SWIFT_MODULE_NAME> -module-link-name <SWIFT_LIBRARY_NAME> -emit-module -emit-module-path <SWIFT_MODULE> -emit-dependencies <DEFINES> <FLAGS> <INCLUDES> <SWIFT_SOURCES> <LINK_FLAGS> <SONAME_FLAG> <TARGET_INSTALLNAME_DIR><TARGET_SONAME> ${CMAKE_Swift_IMPLIB_LINKER_FLAGS} <LINK_LIBRARIES>")

  set(CMAKE_Swift_LINK_EXECUTABLE "<CMAKE_Swift_COMPILER> -num-threads ${CMAKE_Swift_NUM_THREADS} -emit-executable -o <TARGET> -emit-dependencies <DEFINES> <FLAGS> <INCLUDES> <SWIFT_SOURCES> <LINK_FLAGS> <LINK_LIBRARIES>")

  set(CMAKE_Swift_CREATE_STATIC_LIBRARY "<CMAKE_Swift_COMPILER> -num-threads ${CMAKE_Swift_NUM_THREADS} -emit-library -static -o <TARGET> -module-name <SWIFT_MODULE_NAME> -module-link-name <SWIFT_LIBRARY_NAME> -emit-module -emit-module-path <SWIFT_MODULE> -emit-dependencies <DEFINES> <FLAGS> <INCLUDES> <SWIFT_SOURCES> <LINK_FLAGS> <LINK_LIBRARIES>")

  set(CMAKE_Swift_ARCHIVE_CREATE "<CMAKE_AR> crs <TARGET> <OBJECTS>")
  set(CMAKE_Swift_ARCHIVE_FINISH "")

  add_compile_options($<$<COMPILE_LANGUAGE:Swift>:-wmo>)
endif()
