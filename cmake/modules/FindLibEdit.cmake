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
#   LibEdit_FOUND          - true if libedit was found
#   LibEdit_INCLUDE_DIRS   - include search path
#   LibEdit_LIBRARIES      - libraries to link
#   LibEdit_VERSION_STRING - version number

if(LibEdit_INCLUDE_DIRS AND LibEdit_LIBRARIES)
  set(LibEdit_FOUND TRUE)
else()
  find_package(PkgConfig QUIET)
  pkg_check_modules(PC_LIBEDIT QUIET libedit)

  find_path(LibEdit_INCLUDE_DIRS
            NAMES
              histedit.h
            HINTS
              ${PC_LIBEDIT_INCLUDEDIR}
              ${PC_LIBEDIT_INCLUDE_DIRS}
              ${CMAKE_INSTALL_FULL_INCLUDEDIR})
  find_library(LibEdit_LIBRARIES
               NAMES
                 edit libedit
               HINTS
                 ${PC_LIBEDIT_LIBDIR}
                 ${PC_LIBEDIT_LIBRARY_DIRS}
                 ${CMAKE_INSTALL_FULL_LIBDIR})

  if(LibEdit_INCLUDE_DIRS AND EXISTS "${LibEdit_INCLUDE_DIRS}/histedit.h")
    file(STRINGS "${LibEdit_INCLUDE_DIRS}/histedit.h"
         libedit_major_version_str
         REGEX "^#define[ \t]+LIBEDIT_MAJOR[ \t]+[0-9]+")
    string(REGEX REPLACE "^#define[ \t]+LIBEDIT_MAJOR[ \t]+([0-9]+)" "\\1"
           LIBEDIT_MAJOR_VERSION "${libedit_major_version_str}")

    file(STRINGS "${LibEdit_INCLUDE_DIRS}/histedit.h"
         libedit_minor_version_str
         REGEX "^#define[ \t]+LIBEDIT_MINOR[ \t]+[0-9]+")
    string(REGEX REPLACE "^#define[ \t]+LIBEDIT_MINOR[ \t]+([0-9]+)" "\\1"
           LIBEDIT_MINOR_VERSION "${libedit_minor_version_str}")

    set(LibEdit_VERSION_STRING "${libedit_major_version}.${libedit_minor_version}")
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(LibEdit
                                    FOUND_VAR
                                      LibEdit_FOUND
                                    REQUIRED_VARS
                                      LibEdit_INCLUDE_DIRS
                                      LibEdit_LIBRARIES
                                    VERSION_VAR
                                      LibEdit_VERSION_STRING)
  mark_as_advanced(LibEdit_INCLUDE_DIRS LibEdit_LIBRARIES)
endif()

