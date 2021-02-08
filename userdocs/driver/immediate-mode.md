# Immediate Mode

Single-file Swift programs and scripts can be run immediately using `swift /path/to/file.swift`. Any arguments which come before the input file will be processed by `swift`, and any arguments which follow it will be processed as arguments of the program. For example, `swift -warnings-as-errors myscript.swift foo bar` will treat warnings as errors while interpreting `myscript.swift` with arguments `foo` and `bar`.

On Unix platforms, it's also possible to treat a `.swift` file as an executable which can be run directly. To do so, add a "shebang" line starting with `#!/usr/bin/swift` at the top of the file and grant it executable permissions.