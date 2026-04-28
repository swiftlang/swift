# Swift/COM

Swift/COM bridges COM interfaces to Swift by generating Swift interfaces for the COM interface.

```swift
import SwiftCOM
import WinSDK

try! CoInitializeEx(COINIT_MULTITHREADED)

if let pFD = try? IFileDialog.CreateInstance(class: CLSID_FileOpenDialog) {
  try pFD.Show(nil)
}
```

The current approach is to manually construct high-level interfaces to a handful of COM+ interfaces to enable reflection into the system.  It would then allow for the generation of Swift bindings to the COM interface via C.  These interfaces can then be wrapped for more idiomatic access.
