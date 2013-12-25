#!/bin/bash
set -o pipefail
set -e
#
# Note: Jenkins is set up to restore the repositories to pristine
# state before building, so we rebuild from scratch every time.  If
# you use this script yourself, it will NOT auto-clean before building

# Set up the default path to cmake
CMAKE_DEFAULT="$(which cmake || echo /usr/local/bin/cmake)"

# Declare the set of known settings along with each one's description
#
# If you add a user-settable variable, add it to this list.
#
# a default value of "" indicates that the corresponding variable
# will remain unset unless set explicitly
KNOWN_SETTINGS=(
    # name                      default          description
    build-args                  ""               "arguments to the build tool; defaults to -j8 when CMake generator is \"Unix Makefiles\""
    build-dir                   ""               "out-of-tree build directory; default is in-tree"
    build-type                  Debug            "the CMake build variant: Debug, RelWithDebInfo, Release, etc."
    cmake                       "$CMAKE_DEFAULT" "path to the cmake binary"
    cmake-generator             "Unix Makefiles" "kind of build system to generate; see output of cmake --help for choices"
    incremental                 ""               "when build directories already exist, skip configuration"
    package                     ""               "set to build packages"
    prefix                      "/usr"           "installation prefix"
    skip-build-llvm             ""               "set to skip building LLVM/Clang"
    skip-build-swift            ""               "set to skip building Swift"
    skip-build-sourcekit        ""               "set to skip building SourceKit"
    skip-test-swift             ""               "set to skip testing Swift"
    skip-test-swift-performance ""               "set to skip testing Swift performance"
    skip-package-swift          ""               "set to skip packaging Swift"
    skip-test-sourcekit         ""               "set to skip testing SourceKit"
    skip-package-sourcekit      ""               "set to skip packaging SourceKit"
    workspace                   "${HOME}/src"    "source directory containing llvm, clang, swift, and SourceKit"
    run-with-asan-compiler      ""               "the AddressSanitizer compiler to use (non-asan build if empty string is passed)"
)

function toupper() {
    echo "$@" | tr '[:lower:]' '[:upper:]'    
}

function to_varname() {
    toupper "${1//-/_}"
}

# Set up an "associative array" of settings for error checking, and set
# (or unset) each corresponding variable to its default value
# If the Mac's bash were not stuck in the past, we could "declare -A" an
# associative array, but instead we have to hack it by defining variables
# declare -A IS_KNOWN_SETTING
for ((i = 0; i < ${#KNOWN_SETTINGS[@]}; i += 3)); do
    setting="${KNOWN_SETTINGS[i]}"
    
    default_value="${KNOWN_SETTINGS[$((i+1))]}"
    
    varname="$(to_varname "${setting}")"    # upcase the setting name to get the variable
    eval "${varname}_IS_KNOWN_SETTING=1"

    if [[ "${default_value}" ]] ; then
        # For an explanation of the backslash see http://stackoverflow.com/a/9715377
        eval ${varname}=$\default_value
    else
        unset ${varname}
    fi
done

COMMAND_NAME="$(basename "$0")"

# Print instructions for using this script to stdout
usage() {
    echo "Usage: ${COMMAND_NAME} [--help|-h] [ --SETTING=VALUE | --SETTING VALUE | --SETTING | --release ] *"
    echo
    echo "  Available settings. Each setting corresponds to a variable,"
    echo "  obtained by upcasing its name, in this script.  A variable"
    echo "  with no default listed here will be unset in the script if"
    echo "  not explicitly specified.  A setting passed in the 2nd form"
    echo "  will set its corresponding variable to \"1\"."
    echo

    setting_list="
 | |Setting| Default|Description
 | |-------| -------|-----------
"

    for ((i = 0; i < ${#KNOWN_SETTINGS[@]}; i += 3)); do
        setting_list+="\
 | |--${KNOWN_SETTINGS[i]}| ${KNOWN_SETTINGS[$((i+1))]}|${KNOWN_SETTINGS[$((i+2))]}
"
    done
    echo "${setting_list}" | column -x -s'|' -t
    echo
    echo "Note: when using the form --SETTING VALUE, VALUE must not begin "
    echo "      with a hyphen."
    echo "Note: the \"--release\" option creates a pre-packaged combination"
    echo "      of settings used by the buildbot."
}

# Scan all command-line arguments
while [[ "$1" ]] ; do
    case "$1" in
        -h | --help )
            usage
            exit
            ;;

        # The --release flag enables a release build, which will additionally build
        # a package if the build-and-test succeeds.
        --release)
            BUILD_TYPE=RelWithDebInfo
            PACKAGE=1
            # Include a custom name to avoid picking up stale module files.
            CUSTOM_VERSION_NAME="release $(date -j '+%Y-%m-%d %H-%M-%S')"
            ;;
        
        --* )
            dashless="${1:2}"
            
            # drop suffix beginning with the first "="
            setting="${dashless%%=*}" 

            # compute the variable to set
            varname="$(to_varname "${setting}")"

            # check to see if this is a known option
            known_var_name="${varname}_IS_KNOWN_SETTING"
            if [[ ! "${!known_var_name}" ]] ; then
                echo "Error: Unknown setting: ${setting}" 1>&2
                usage 1>&2
                exit 1
            fi

            # find the intended value
            if [[ "${dashless}" == *=* ]] ; then              # if there's an '=', the value
                value="${dashless#*=}"                        #   is everything after the first '='
            elif [[ "$2" ]] && [[ "${2:0:1}" != "-" ]] ; then # else if the next parameter exists
                value="$2"                                    #    but isn't  an option, use that
                shift
            else                                             # otherwise, the value is 1
                value=1                                  
            fi
            
            # For explanation of backslash see http://stackoverflow.com/a/9715377
            eval ${varname}=$\value
            ;;

        *)
            usage
            exit 1
    esac
    shift
done

# Set this to the install prefix for release builds.
INSTALL_PREFIX="${PREFIX}"

# Set these to the paths of the OS X SDK and toolchain.
SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk
TOOLCHAIN=/Applications/Xcode.app/Contents/Developer/Toolchains/OSX10.9.xctoolchain

# Set this to the path on matte to which release packages should be delivered.
PACKAGE_PATH=/Users/swift-discuss
SOURCEKIT_PACKAGE_PATH=/Users/sourcekit-dev

# Set this to the address to which release announcements should be sent.

# Make sure the variables and directories we expect to exist actually do.
test "$WORKSPACE"
test -d "$WORKSPACE"
test -d "$WORKSPACE/llvm"
test -d "$WORKSPACE/llvm/tools"
test -d "$WORKSPACE/clang"
test -d "$WORKSPACE/swift"
test -d "$WORKSPACE/SourceKit"

# Swift-project products, in the order they must be built
SWIFT_BUILD_PRODUCTS=(swift SourceKit)

# All build products, in the order they must be built
ALL_BUILD_PRODUCTS=(llvm "${SWIFT_BUILD_PRODUCTS[@]}")

# Calculate build directories for each product
for product in "${ALL_BUILD_PRODUCTS[@]}" ; do
    varname="$(toupper "${product}")"_BUILD_DIR
    if [[ "${BUILD_DIR}" ]] ; then
        product_build_dir="${BUILD_DIR}/${product}"
    else
        product_build_dir="${WORKSPACE}/${product}/build"
    fi
    eval "$varname=\$product_build_dir"
done

SWIFT_MODULE_CACHE="${BUILD_DIR:-${WORKSPACE}}/swift-module-cache"

if [[ "$PACKAGE" ]]; then
  # Make sure install-test-script.sh is available alongside us.
  INSTALL_TEST_SCRIPT="$(dirname "$0")/install-test-script.sh"
  RELEASE_NOTES_TXT="$(dirname "$0")/buildbot-release-notes.txt"

  if [ \! -x "$INSTALL_TEST_SCRIPT" ]; then
    echo "Install test script $INSTALL_TEST_SCRIPT is unavailable or not executable!"
    exit 1
  fi

  if [ \! -f "$RELEASE_NOTES_TXT" ]; then
    echo "Release notes file $RELEASE_NOTES_TXT is unavailable!"
    exit 1
  fi
fi

# Symlink clang into the llvm tree.
ln -sf  "$WORKSPACE/clang" "$WORKSPACE/llvm/tools/clang"

# Create a fresh directory for the Swift Clang module cache.
if [ -e "${SWIFT_MODULE_CACHE}" ]; then
  rm -rf "${SWIFT_MODULE_CACHE}" || exit 1
fi
mkdir -p "${SWIFT_MODULE_CACHE}"

# Make extra sure it's empty.
if [ "$(ls -A "${SWIFT_MODULE_CACHE}")" ]; then
  echo "Module cache not empty! Aborting."
  exit 1
fi

# CMake options used for all targets, including LLVM/Clang
COMMON_CMAKE_OPTIONS=(
    -DCMAKE_C_COMPILER="$TOOLCHAIN/usr/bin/clang"
    -DCMAKE_CXX_COMPILER="$TOOLCHAIN/usr/bin/clang++"
    -DCMAKE_BUILD_TYPE="$BUILD_TYPE"
    -DLLVM_ENABLE_ASSERTIONS="ON" 
)

case "${CMAKE_GENERATOR}" in
    'Unix Makefiles')
        BUILD_ARGS="${BUILD_ARGS:--j8}"
        ;;
    Xcode)
        BUILD_TARGET_FLAG=-target
        COMMON_CMAKE_OPTIONS=(
            "${COMMON_CMAKE_OPTIONS[@]}"
            -DCMAKE_XCODE_ATTRIBUTE_GCC_VERSION:STRING=com.apple.compilers.llvm.clang.1_0
            -DCMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LIBRARY:STRING=libc++
            -DCMAKE_OSX_DEPLOYMENT_TARGET=10.8
            -DCMAKE_CONFIGURATION_TYPES="Debug;Release"
        )
        ;;
esac

# Build LLVM and Clang (x86 target only).
if [ \! "$SKIP_BUILD_LLVM" ]; then
  echo "--- Building LLVM and Clang ---"
  if [[ ! -f  "${LLVM_BUILD_DIR}/CMakeCache.txt" ]] ; then
      mkdir -p "${LLVM_BUILD_DIR}"
      (cd "${LLVM_BUILD_DIR}" &&
          "$CMAKE" -G "${CMAKE_GENERATOR}" "${COMMON_CMAKE_OPTIONS[@]}" \
              -DCMAKE_CXX_FLAGS="-stdlib=libc++" \
              -DCMAKE_EXE_LINKER_FLAGS="-stdlib=libc++" \
              -DCMAKE_SHARED_LINKER_FLAGS="-stdlib=libc++" \
              -DLLVM_TARGETS_TO_BUILD="X86;ARM" \
              -DCLANG_REPOSITORY_STRING="$CUSTOM_VERSION_NAME" \
              "$WORKSPACE/llvm" || exit 1)
  fi
  "$CMAKE" --build "${LLVM_BUILD_DIR}" -- ${BUILD_ARGS}
fi

#
# Now build all the Swift products
#

SWIFT_CMAKE_OPTIONS=(
    -DSWIFT_MODULE_CACHE_PATH="${SWIFT_MODULE_CACHE}"
    -DSWIFT_RUN_LONG_TESTS="ON"
)

SOURCEKIT_CMAKE_OPTIONS=(
    -DSOURCEKIT_PATH_TO_SWIFT_SOURCE="$WORKSPACE/swift"
    -DSOURCEKIT_PATH_TO_SWIFT_BUILD="${SWIFT_BUILD_DIR}"
)

ASAN_CMAKE_OPTIONS=(
    -DCMAKE_C_COMPILER="${RUN_WITH_ASAN_COMPILER}"
    -DCMAKE_CXX_COMPILER="${RUN_WITH_ASAN_COMPILER}++"
    -DCMAKE_C_FLAGS="-fsanitize=address -O1 -g -fno-omit-frame-pointer"
    -DCMAKE_CXX_FLAGS="-fsanitize=address -O1 -g -fno-omit-frame-pointer"
    -DCMAKE_SHARED_LINKER_FLAGS="-fsanitize=address"
    -DCMAKE_EXE_LINKER_FLAGS="-fsanitize=address"
    -DSOURCEKIT_USE_INPROC_LIBRARY="ON"
    -DSWIFT_ASAN_BUILD="ON"
)

# Build each one. Note: in this block, names beginning with underscore
# are used indirectly to access product-specific variables.
for product in "${SWIFT_BUILD_PRODUCTS[@]}" ; do
    PRODUCT=$(toupper "${product}")
    
    _SKIP_BUILD_PRODUCT=SKIP_BUILD_${PRODUCT}
    if [[ ! "${!_SKIP_BUILD_PRODUCT}" ]]; then
        
        echo "--- Building ${product} ---"
        _PRODUCT_BUILD_DIR=${PRODUCT}_BUILD_DIR
        
        if [[ ! -f  "${!_PRODUCT_BUILD_DIR}/CMakeCache.txt" ]] ; then
            mkdir -p "${!_PRODUCT_BUILD_DIR}"

            _PRODUCT_CMAKE_OPTIONS=${PRODUCT}_CMAKE_OPTIONS[@]
            if [[ "${RUN_WITH_ASAN_COMPILER}" ]]; then
              _ASAN_OPTIONS=ASAN_CMAKE_OPTIONS[@]
            fi
              
            (cd "${!_PRODUCT_BUILD_DIR}" &&
                "$CMAKE" -G "${CMAKE_GENERATOR}" "${COMMON_CMAKE_OPTIONS[@]}" \
                    "${!_PRODUCT_CMAKE_OPTIONS}" \
                    "${!_ASAN_OPTIONS}" \
                    -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
                    -D${PRODUCT}_PATH_TO_CLANG_SOURCE="$WORKSPACE/llvm/tools/clang" \
                    -D${PRODUCT}_PATH_TO_CLANG_BUILD="${LLVM_BUILD_DIR}" \
                    -D${PRODUCT}_PATH_TO_LLVM_SOURCE="$WORKSPACE/llvm" \
                    -D${PRODUCT}_PATH_TO_LLVM_BUILD="${LLVM_BUILD_DIR}" \
                    "$WORKSPACE/${product}" || exit 1)
        fi
        "$CMAKE" --build "${!_PRODUCT_BUILD_DIR}" -- ${BUILD_ARGS}
    fi
done

# Run the Swift tests.
if [ \! "$SKIP_TEST_SWIFT" ]; then
    echo "--- Running Swift Tests ---"
    
    build_cmd=("$CMAKE" --build "${SWIFT_BUILD_DIR}" -- ${BUILD_ARGS})

    "${build_cmd[@]}" ${BUILD_TARGET_FLAG} SwiftUnitTests
    
    if [[ "${CMAKE_GENERATOR}" == Ninja ]] ; then
        # Ninja buffers command output to avoid scrambling the output
        # of parallel jobs, which is awesome... except that it
        # interferes with the progress meter when testing.  Instead of
        # executing ninja directly, have it dump the commands it would
        # run, strip Ninja's progress prefix with sed, and tell the
        # shell to execute that.
        sh -c "$("${build_cmd[@]}" -n -v check-swift | sed -e 's/[^]]*] //')"
    else
        "${build_cmd[@]}" ${BUILD_TARGET_FLAG} check-swift
    fi
fi

# Run the Swift performance tests.
if [ \! "$SKIP_TEST_SWIFT_PERFORMANCE" ]; then
  # Currently we use the toolchain-specific Clang as our CC under test, because
  # our locally built one might not end up invoking an LD that supports
  # autolinking on older machines. We can reconsider this when it becomes useful
  # to have the C/C++ tests be running using the same LLVM basis as the Swift we
  # are testing.
  echo "--- Running Swift Performance Tests ---"
  export CLANG="$TOOLCHAIN/usr/bin/clang"
  export SWIFT="${SWIFT_BUILD_DIR}/bin/swift"
  if (cd "${SWIFT_BUILD_DIR}" &&
          "${LLVM_BUILD_DIR}/bin/llvm-lit" -v benchmark \
              -j1 --output benchmark/results.json); then
      PERFORMANCE_TESTS_PASSED=1
  else
      PERFORMANCE_TESTS_PASSED=0
  fi
  echo "--- Submitting Swift Performance Tests ---"
  swift_source_revision="$("$WORKSPACE/llvm/utils/GetSourceVersion" "$WORKSPACE/swift")"
  (cd "${SWIFT_BUILD_DIR}" &&
    "$WORKSPACE/swift/utils/submit-benchmark-results" benchmark/results.json \
        --output benchmark/lnt_results.json \
        --machine-name "matte.apple.com--${BUILD_TYPE}--x86_64--O3" \
        --run-order "$swift_source_revision" \
        --submit http://localhost:32169/submitRun) || exit 1

  # If the performance tests failed, fail the build.
  if [ "$PERFORMANCE_TESTS_PASSED" -ne 1 ]; then
      echo "*** ERROR: Swift Performance Tests failed ***"
      exit 1
  fi
fi

if [ "$PACKAGE" -a \! "$SKIP_PACKAGE_SWIFT" ]; then
  echo "--- Building Swift Package ---"
  "$CMAKE" --build "${SWIFT_BUILD_DIR}" -- ${BUILD_ARGS} ${BUILD_TARGET_FLAG} package

  saw_package=
  for package in "${SWIFT_BUILD_DIR}"/swift-*.tar.gz; do
    if [ "$saw_package" ]; then
      echo "More than one package file built!"
      exit 1
    fi
    saw_package=1

    echo "--- Testing $package ---"
    if ! "$INSTALL_TEST_SCRIPT" "$package"; then
      echo "$package failed test!"
      exit 1
    fi

    echo "--- Delivering $package ---"
    cp "$package" "$PACKAGE_PATH" || exit 1

    echo "--- Announcing $package ---"
    package_basename="$(basename "$package")"
    sendmail -r "$PACKAGE_ANNOUNCEMENT_ADDRESS" "$PACKAGE_ANNOUNCEMENT_ADDRESS" <<EOM
To: $PACKAGE_ANNOUNCEMENT_ADDRESS
Subject: Swift package $package_basename now available

A new Swift package is available at
You can download and install it using the command line:

          ~/Downloads
        sudo darwinup install ~/Downloads/$package_basename

where \$OD_USER is your Open Directory username.

We recommend uninstalling any previous Swift packages you have installed
before installing this package. Uninstall as follows:

        darwinup list
        sudo darwinup uninstall \$UUID_FROM_DARWINUP_LIST

When you find bugs in Swift, please report them using the 'Swift' Radar
component.

=== GETTING STARTED WITH SWIFT ===

Once installed, run 'swift' to bring up the interactive prompt:

        swift

Run Swift programs using the '-i' flag:

        swift -i /usr/share/swift/examples/hello.swift

Compile Swift programs to .o files using the '-c' flag. Currently they must
then be linked manually to the swift_stdlib_core library in
/usr/lib/swift/macosx using Clang:

        swift -c /usr/share/swift/examples/hello.swift -o hello.o
        clang -o hello hello.o -L/usr/lib/swift/macosx -lswift_stdlib_core
        ./hello

Language documentation and examples are installed under /usr/share/swift.

Have fun!

=== RECENT CHANGES ===

$(cat "$RELEASE_NOTES_TXT")

=== KNOWN ISSUES ===

The Swift compiler is under active development and has a number of known
problems. Here are some of the most commonly encountered issues:

* Spectacularly poor error messages: the compiler will often report the
  unhelpful errors "expression does not type-check" or "assignment does not
  type-check", preceded by a debugging dump.

* Run-time errors abort: run-time errors such as an out-of-bounds array access
  are detected by the standard library, which immediately aborts without
  reporting a problem. Moreover, such errors are not trapped in the REPL, and
  will cause the REPL itself to crash.

.
EOM
  done

  if [ \! "$saw_package" ]; then
    echo "No package file built!"
    exit 1
  fi
fi

# Run the SourceKit tests.
if [ \! "$SKIP_TEST_SOURCEKIT" ]; then
  echo "--- Running SourceKit Tests ---"

  build_cmd=("$CMAKE" --build "${SOURCEKIT_BUILD_DIR}" -- ${BUILD_ARGS})
  "${build_cmd[@]}" ${BUILD_TARGET_FLAG} check-sourcekit
fi

if [ "$PACKAGE" -a \! "$SKIP_PACKAGE_SOURCEKIT" ]; then
  echo "--- Building SourceKit Package ---"
  (cd "${SOURCEKIT_BUILD_DIR}" &&
    /bin/sh -ex "$WORKSPACE/SourceKit/utils/buildbot-package-sourcekit.sh") || exit 1

  saw_package=
  for package in "${SOURCEKIT_BUILD_DIR}"/SourceKit-*.tar.gz; do
    if [ "$saw_package" ]; then
      echo "More than one package file built!"
      exit 1
    fi
    saw_package=1

    echo "--- Delivering $package ---"
    cp "$package" "$SOURCEKIT_PACKAGE_PATH" || exit 1

    echo "--- Announcing $package ---"
    package_basename="$(basename "$package")"
    sendmail -r "$SOURCEKIT_PACKAGE_ANNOUNCEMENT_ADDRESS" "$SOURCEKIT_PACKAGE_ANNOUNCEMENT_ADDRESS" <<EOM
To: $SOURCEKIT_PACKAGE_ANNOUNCEMENT_ADDRESS
Subject: SourceKit package $package_basename now available

A new SourceKit package is available at
sftp://matte.apple.com$SOURCEKIT_PACKAGE_PATH/$package_basename .
You can download it using the command line:

        sftp \$OD_USER@matte.apple.com:$SOURCEKIT_PACKAGE_PATH/$package_basename \
          ~/Downloads

where \$OD_USER is your Open Directory username.

EOM
  done

  if [ \! "$saw_package" ]; then
    echo "No package file built!"
    exit 1
  fi
fi
