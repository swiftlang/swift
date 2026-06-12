# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# Included via CMAKE_USER_MAKE_RULES_OVERRIDE for Windows builds.  CMake can
# add an explicit /MANIFEST:EMBED to compiler-driver shared-library rules.  The
# build script passes /MANIFEST:NO for shared/module links so linker-generated
# RT_MANIFEST #2 resources do not poison private SxS activation; strip the
# explicit embed spelling so it cannot override that flag.

foreach(lang IN ITEMS C CXX Swift)
  foreach(rule IN ITEMS
      CMAKE_${lang}_CREATE_SHARED_LIBRARY
      CMAKE_${lang}_CREATE_SHARED_MODULE)
    if(DEFINED ${rule})
      string(REPLACE " --manifests " " " ${rule} "${${rule}}")
      string(REPLACE " -Xlinker /MANIFEST:EMBED" "" ${rule} "${${rule}}")
      string(REPLACE "-Xlinker /MANIFEST:EMBED " "" ${rule} "${${rule}}")
      string(REPLACE "-Xlinker /MANIFEST:EMBED" "" ${rule} "${${rule}}")
      string(REPLACE " /MANIFEST:EMBED" "" ${rule} "${${rule}}")
      string(REPLACE "/MANIFEST:EMBED " "" ${rule} "${${rule}}")
      string(REPLACE "/MANIFEST:EMBED" "" ${rule} "${${rule}}")
    endif()
  endforeach()
endforeach()
