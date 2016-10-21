# Find libicu's libraries

include(FindPackageHandleStandardArgs)

find_package(PkgConfig)

set(ICU_REQUIRED)
foreach(FIND_COMP_ITEM ${ICU_FIND_COMPONENTS})
  string(TOUPPER "${FIND_COMP_ITEM}" UMODULE)
  string(TOLOWER "${FIND_COMP_ITEM}" LMODULE)
  list(APPEND ICU_REQUIRED 
    ICU_${UMODULE}_INCLUDE_DIR ICU_${UMODULE}_LIBRARIES)

  pkg_check_modules(PC_ICU_${UMODULE} QUIET icu-${LMODULE})
  if(${PC_ICU_${UMODULE}_FOUND})
    set(ICU_${UMODULE}_DEFINITIONS ${PC_ICU_${UMODULE}_CFLAGS_OTHER})

    find_path(ICU_${UMODULE}_INCLUDE_DIR unicode
      HINTS ${PC_ICU_${UMODULE}_INCLUDEDIR} ${PC_ICU_${UMODULE}_INCLUDE_DIRS})
    set(ICU_${UMODULE}_INCLUDE_DIRS ${ICU_${UMODULE}_INCLUDE_DIR})

    find_library(ICU_${UMODULE}_LIBRARY NAMES icu${LMODULE}
      HINTS ${PC_ICU_${UMODULE}_LIBDIR} ${PC_ICU_${UMODULE}_LIBRARY_DIRS})
    set(ICU_${UMODULE}_LIBRARIES ${ICU_${UMODULE}_LIBRARY})
  endif()
endforeach()

foreach(sdk ANDROID;FREEBSD;LINUX;WINDOWS)
  foreach(ITEM ${ICU_FIND_COMPONENTS})
    string(TOUPPER "${ITEM}" UMODULE)
    if("${SWIFT_${sdk}_ICU_${UMODULE}_INCLUDE}" STREQUAL "")
      set(SWIFT_${sdk}_ICU_${UMODULE}_INCLUDE ${ICU_${UMODULE}_INCLUDE_DIRS})
    endif()
    if("${SWIFT_${sdk}_ICU_${UMODULE}}" STREQUAL "")
      set(SWIFT_${sdk}_ICU_${UMODULE} ${ICU_${UMODULE}_LIBRARY})
    endif()
  endforeach()
endforeach()

find_package_handle_standard_args(ICU DEFAULT_MSG ${ICU_REQUIRED})
mark_as_advanced(${ICU_REQUIRED})
