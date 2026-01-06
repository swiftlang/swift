# Swift Supplemental Libraries

The supplemental libraries are all libraries that are not one of the Core or
overlay libraries. Each supplemental library builds as an independent project.

The supplemental libraries are:
 - CxxInterop
 - Differentiation
 - Distributed
 - Observation
 - StringProcessing
 - Runtime
 - Synchronization

The top-level Supplemental CMakeLists supplies a super-build pattern for
configuring and compiling each of the supplemental library projects through a
single CMake invocation. The `Swift_ENABLE_RUNTIMES` CMake option enables the
specified supplemental libraries. All libraries configured this way are built
with the same compilers, against the same sysroot, with the same target triple
and installed into the same location.

## Super-Build

Configuring each project independently is tedious. The Supplemental directory
contains a Super-Build CMakeLists that invokes the build of each of the
supplemental libraries in the appropriate order, simplifying the process of
building each library.

Important configuration variables:
 - `Swift_ENABLE_RUNTIMES`: Used to configure which runtime libraries are built.

The super-build forwards the following variables to each sub-project
unconditionally:
 - `BUILD_SHARED_LIBS`
 - `CMAKE_BUILD_TYPE`
 - `CMAKE_INSTALL_PREFIX`
 - `CMAKE_COLOR_DIAGNOSTICS`
 - `CMAKE_C_COMPILER`
 - `CMAKE_C_COMPILER_TARGET`
 - `CMAKE_CXX_COMPILER`
 - `CMAKE_CXX_COMPILER_TARGET`
 - `CMAKE_Swift_COMPILER`
 - `CMAKE_Swift_COMPILER_TARGET`

If set, the super-build forwards the following values to each sub-project:

 - `SwiftCore_DIR`: Path to the SwiftCore build directory
 - `CMAKE_MAKE_PROGRAM`: Path to `ninja`

The super-build is for convenience. If more fine-grained control is desired for
configuring a specific runtime library, you may configure that library
independently.
