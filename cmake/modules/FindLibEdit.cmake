#.rst:
# FindLibEdit
# -----------
#
# Find libedit library and headers
#
# The module defines the following variables:
#
# ::
#
#   libedit_FOUND         - true if libedit was found
#   libedit_INCLUDE_DIRS  - include search path
#   libedit_LIBRARIES     - libraries to link
#   libedit_VERSION       - version number

if(libedit_INCLUDE_DIRS AND libedit_LIBRARIES)
  set(libedit_FOUND TRUE)
else()
  find_package(PkgConfig QUIET)
  pkg_check_modules(PC_LIBEDIT QUIET libedit)

  find_path(libedit_INCLUDE_DIRS
            NAMES
              histedit.h
            HINTS
              ${PC_LIBEDIT_INCLUDEDIR}
              ${PC_LIBEDIT_INCLUDE_DIRS}
              ${CMAKE_INSTALL_FULL_INCLUDEDIR})
  find_library(libedit_LIBRARIES
               NAMES
                 edit libedit
               HINTS
                 ${PC_LIBEDIT_LIBDIR}
                 ${PC_LIBEDIT_LIBRARY_DIRS}
                 ${CMAKE_INSTALL_FULL_LIBDIR})

  if(libedit_INCLUDE_DIRS AND EXISTS "${libedit_INCLUDE_DIRS}/histedit.h")
    file(STRINGS "${libedit_INCLUDE_DIRS}/histedit.h"
         libedit_major_version_str
         REGEX "^#define[ \t]+LIBEDIT_MAJOR[ \t]+[0-9]+")
    string(REGEX REPLACE "^#define[ \t]+LIBEDIT_MAJOR[ \t]+([0-9]+)" "\\1"
           LIBEDIT_MAJOR_VERSION "${libedit_major_version_str}")

    file(STRINGS "${libedit_INCLUDE_DIRS}/histedit.h"
         libedit_minor_version_str
         REGEX "^#define[ \t]+LIBEDIT_MINOR[ \t]+[0-9]+")
    string(REGEX REPLACE "^#define[ \t]+LIBEDIT_MINOR[ \t]+([0-9]+)" "\\1"
           LIBEDIT_MINOR_VERSION "${libedit_minor_version_str}")

    set(libedit_VERSION_STRING "${libedit_major_version}.${libedit_minor_version}")
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(libedit
                                    REQUIRED_VARS
                                      libedit_INCLUDE_DIRS
                                      libedit_LIBRARIES
                                    VERSION_VAR
                                      libedit_VERSION_STRING)
  mark_as_advanced(libedit_INCLUDE_DIRS libedit_LIBRARIES)
endif()

