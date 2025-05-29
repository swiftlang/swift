# SWIFT_VERSION is deliberately /not/ cached so that an existing build directory
# can be reused when a new version of Swift comes out (assuming the user hasn't
# manually set it as part of their own CMake configuration).
set(SWIFT_VERSION_MAJOR 6)
set(SWIFT_VERSION_MINOR 2)
set(SWIFT_VERSION "${SWIFT_VERSION_MAJOR}.${SWIFT_VERSION_MINOR}")

