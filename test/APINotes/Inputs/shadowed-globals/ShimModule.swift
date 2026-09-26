import BaseSDK

// Shims of the kind a platform overlay writes when the underlying macros do
// not already import with the desired type. Once API notes give the macros a
// `Type:` these become redundant redeclarations of the same name.
internal var FILE_SHARE_READ: DWORD {
  DWORD(BaseSDK.FILE_SHARE_READ)
}

internal var FILE_SHARE_WRITE: DWORD {
  DWORD(BaseSDK.FILE_SHARE_WRITE)
}
