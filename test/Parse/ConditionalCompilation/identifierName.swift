// RUN: %target-typecheck-verify-swift

// Ensure that the identifiers in compilation conditions don't reference
// any decls in the scope.
func f2(
  FOO: Int,
  swift: Int, _compiler_version: Int,
  os: Int, arch: Int, _endian: Int, _runtime: Int,
  targetEnvironment: Int,
  arm: Int, i386: Int, macOS: Int, OSX: Int, Linux: Int,
  big: Int, little: Int,
  _ObjC: Int, _Native: Int,
  simulator: Int
) {

#if FOO
  _ = FOO
#elseif os(macOS) && os(OSX) && os(Linux)
  _ = os + macOS + OSX + Linux
#elseif arch(i386) && arch(arm)
  _ = arch + i386 + arm
#elseif _endian(big) && _endian(little)
  _ = _endian + big + little
#elseif _runtime(_ObjC) && _runtime(_Native)
  _ = _runtime + _ObjC + _Native
#elseif targetEnvironment(simulator)
  _ = targetEnvironment + simulator
#elseif swift(>=1.0) && _compiler_version("4.*.0")
  _ = swift + _compiler_version
#endif

}

func f2() {
  let
    FOO = 1, swift = 1, _compiler_version = 1,
    os = 1, arch = 1, _endian = 1, _runtime = 1,
    targetEnvironment = 1,
    arm = 1, i386 = 1, macOS = 1, OSX = 1, Linux = 1,
    big = 1, little = 1,
    _ObjC = 1, _Native = 1,
    simulator = 1

#if FOO
  _ = FOO
#elseif os(macOS) && os(OSX) && os(Linux)
  _ = os + macOS + OSX + Linux
#elseif arch(i386) && arch(arm)
  _ = arch + i386 + arm
#elseif _endian(big) && _endian(little)
  _ = _endian + big + little
#elseif _runtime(_ObjC) && _runtime(_Native)
  _ = _runtime + _ObjC + _Native
#elseif targetEnvironment(simulator)
  _ = targetEnvironment + simulator
#elseif swift(>=1.0) && _compiler_version("4.*.0")
  _ = swift + _compiler_version
#endif

}

struct S {
  let
    FOO = 1, swift = 1, _compiler_version = 1,
    os = 1, arch = 1, _endian = 1, _runtime = 1,
    targetEnvironment = 1,
    arm = 1, i386 = 1, macOS = 1, OSX = 1, Linux = 1,
    big = 1, little = 1,
    _ObjC = 1, _Native = 1,
    simulator = 1

#if FOO
#elseif os(macOS) && os(OSX) && os(Linux)
#elseif arch(i386) && arch(arm)
#elseif _endian(big) && _endian(little)
#elseif _runtime(_ObjC) && _runtime(_Native)
#elseif targetEnvironment(simulator)
#elseif swift(>=1.0) && _compiler_version("4.*.0")
#endif

}

/// Ensure 'variable used within its own initial value' not to be emitted.
let BAR = { () -> Void in
#if BAR
#endif
}

