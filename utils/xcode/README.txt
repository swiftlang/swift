This contains EXPERIMENTAL Xcode support for Swift. To install:

1. Quit Xcode.
2. Run* install.sh.
3. Profit!

Once you've installed it, Xcode will recognize Swift source files as source 
files and add them to the appropriate part of your project. You will also be
able to use the following build settings:
  SWIFT_EXEC - Path to the Swift compiler.
  SWIFT_LIBRARY_PATH - Path to the directory containing libswift_stdlib_core.
  SWIFT_LIBRARIES_ONLY - Assume all inputs are libraries rather than scripts,
    i.e. automatically pass -parse-as-library to the Swift compiler. This is
    fine for applications with no main.swift, but will need to be turned off
    for CLI tools. You will then want to add -parse-as-library for each
    non-script file you are compiling.
  SWIFT_INCLUDE_PATHS - A list of paths to other directories containing Swift
    or Clang modules.
  SWIFT_OTHER_FLAGS - Any other flags to pass to the Swift compiler.

* install.sh has a few options:
  Pass --symlink to use a symbolic link to the repo's copy of Swift.xcplugin.
  Pass the path to your built Swift compiler to avoid having to specify
    SWIFT_EXEC and SWIFT_LIBRARY_PATH in every project with Swift source files.

