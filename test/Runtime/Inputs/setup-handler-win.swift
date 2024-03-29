import WinSDK

func setupHandler() {
    SetUnhandledExceptionFilter { exception_pointers in
        guard let hSwiftCore = GetModuleHandleA("swiftCore.dll") else {
            return EXCEPTION_EXECUTE_HANDLER
        }
        guard let ppLastFatalErrorMessage = unsafeBitCast(
            GetProcAddress(hSwiftCore, "gLastFatalErrorMessage"),
            to: Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<UInt8>>>>.self) else {
            return EXCEPTION_EXECUTE_HANDLER
        }
        guard let pLastFatalErrorMessage = ppLastFatalErrorMessage.pointee else {
            return EXCEPTION_EXECUTE_HANDLER
        }
        let message = "Fatal error message: \(String(cString: pLastFatalErrorMessage))"
        message.utf8CString.withUnsafeBufferPointer { buf in
            let addr = buf.baseAddress!
            let count = DWORD(buf.count)
            let stderr = GetStdHandle(STD_ERROR_HANDLE)
            WriteFile(stderr, addr, count, nil, nil)
        }
        return EXCEPTION_EXECUTE_HANDLER
    }
}
