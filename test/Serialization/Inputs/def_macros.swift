@expression public macro publicStringify<T>(_ value: T) -> (T, String) = SomeModule.StringifyMacro

@expression macro internalStringify<T>(_ value: T) -> (T, String) = SomeModule.StringifyMacro

@attached(accessor) public macro myWrapper: Void = SomeModule.MyWrapperMacro
