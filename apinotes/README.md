# API Notes README

API notes provide a mechanism by which Objective-C APIs can be
annotated with additional semantic information not present within the
original Objective-C headers. This semantic information can then be
used by the Swift compiler when importing the corresponding Objective-C
module to provide a better mapping of Objective-C APIs into Swift.

API notes are organized into a set of `.apinotes` files. Each
`.apinotes` file contains annotations for a single Objective-C module,
written in YAML (FIXME: to be) described below. These YAML sources
must be manually compiled into a binary representation (`.apinotesc`)
that the Swift compiler will lazily load when it builds code, also
described below. 

# API Notes YAML Format

TBD...

# Compiling API notes

The Swift compiler lazily loads API notes from compiled API notes
files (`.apinotesc` files) and uses these annotations to affect the
Swift signatures of imported Objective-C APIs. Compiled API notes
files reside in the Swift module directory, i.e., the same directory
where the `.swiftmodule` file would reside for the Swift overlay of
that module. For system modules, the path depends on the platform
and architecture.

Platform  | Path
:------------- | :-------------
  OSX | `$SWIFT_EXEC/lib/swift/macosx/`
  iOS (32-bit) | `$SWIFT_EXEC/lib/swift/iphoneos/32`
  iOS (64-bit) | `$SWIFT_EXEC/lib/swift/iphoneos`
  iOS Simulator (32-bit) | `$SWIFT_EXEC/lib/swift/iphonesimulator/32`
  iOS Simulator (64-bit) | `$SWIFT_EXEC/lib/swift/iphonesimulator`

where `$SWIFT_EXEC/bin/swift` is the path to the Swift compiler
executable.

When updating API notes for a system module, recompile the API notes
and place the result in the appropriate directories listed above. The
Swift compiler itself need not be recompiled except in rare cases
where the changes affect how the SDK overlays are built. To recompile
API notes for a given module `$MODULE` and place them into their 

### OS X
```
xcrun swift -apinotes -yaml-to-binary -target x86_64-apple-macosx10.10 -o $SWIFT_EXEC/lib/swift/macosx/$MODULE.apinotesc $MODULE.apinotes
```

### iOS (32-bit)
```
xcrun swift -apinotes -yaml-to-binary -target armv7-apple-ios7.0 -o $SWIFT_EXEC/lib/swift/iphoneos/32/$MODULE.apinotesc $MODULE.apinotes
```

### iOS (64-bit)
```
xcrun swift -apinotes -yaml-to-binary -target arm64-apple-ios7.0 -o $SWIFT_EXEC/lib/swift/iphoneos/$MODULE.apinotesc $MODULE.apinotes
```

### iOS Simulator (32-bit)
```
xcrun swift -apinotes -yaml-to-binary -target i386-apple-ios7.0 -o $SWIFT_EXEC/lib/swift/iphonesimulator/32/$MODULE.apinotesc $MODULE.apinotes
```

### iOS Simulator (64-bit)
```
xcrun swift -apinotes -yaml-to-binary -target x64_64-apple-ios7.0 -o $SWIFT_EXEC/lib/swift/iphonesimulator/$MODULE.apinotesc $MODULE.apinotes
```

To add API notes for a system module `$MODULE` that does not have them yet,
create a new source file `$MODULE.apinotes` and update CMakeLists.txt.
Updated API notes will be found by the build system during the next build.

Note that Swift provides decompilation of binary API notes files via
the `-apinotes -binary-to-yaml` option, which allows one to inspect
the information the compiler is using internally. The order of the
entities in the original YAML input is not preserved, so all entities
in the resulting YAML output are sorted alphabetically.
