# Profiling the Swift Compiler on Windows

This document describes Windows profiling tools from the angle of **profiling the Swift compiler itself**. The information is mostly general purpose, but specifics relevant to other applications aren't considered.

The recommended approach when you need detailed function and I/O operation-level profiling of the Swift compiler on Windows is to use **Event Tracing for Windows (ETW)**. This includes using **Windows Performance Recorder (WPR)** to collect profiling data in ETL format and then **Windows Performance Analyzer (WPA)** to analyze it. For specific analyses not available in WPA, you will also be able to use `xperf.exe` to convert a binary ETL file to a text format for analysis by another program. We also discuss a couple of alternatives which may in certain cases be easier to use or provide specific data of interest: **Visual Studio** (the full IDE, not VS Code) and **Intel VTune**.

The document is organized as follows:

1. [Recommended: Using Event Tracing for Windows (ETW) to profile Swift compiler runs](#recommended-using-event-tracing-for-windows-etw-to-profile-swift-compiler-runs)
2. [Alternative: Using Visual Studio to profile Swift compiler runs](#alternative-using-visual-studio-to-profile-swift-compiler-runs)
3. [Alternative: Using VTune to profile Swift compiler runs](#alternative-using-vtune-to-profile-swift-compiler-runs)
4. [Reference: Survey of Windows profiling tools](#reference-survey-of-windows-profiling-tools)
5. [Reference: Building the Swift toolchain for Windows with debugging symbols](#building-the-swift-toolchain-for-windows-with-debugging-symbols)

# Recommended: Using Event Tracing for Windows (ETW) to profile Swift compiler runs

1. Make sure you have a build of the Swift toolchain with PDB format debug symbols. One approach is to follow [Building the Swift toolchain for Windows with debugging symbols](#building-the-swift-toolchain-for-windows-with-debugging-symbols).
2. Use `wpr` (Windows Performance Recorder) or a wrapper script like [utils/windows-profiling-tools/RunProfiler.ps1](../utils/windows-profiling-tools/RunProfiler.ps1) to collect a `.etl` file (event trace log).
    1. If you are using a custom-built Swift toolchain, set environment variables as instructed in [WindowsQuickStart.md](https://github.com/compnerd/swift-build/blob/main/docs/WindowsQuickStart.md).
    2. Example usage of `RunProfiler.ps1`: `...\RunProfiler.ps1 -TracePath C:\Temp\program.etl -Target swiftc program.swift`. Here `program.swift` just serves as a workload for running the compiler. Different parts of the compiler will naturally be exercised more or less depending on the program.
    3. Instead of running `swiftc` directly, you can instead call a build system like `swift build` or `cmake` to build a larger program. Longer compilations will result in larger `.etl` files. Try a small program first, to see
    what the `.etl` file collection and analysis process is like (including the size of `.etl` files).
1. Run `wpa.exe <path\to\trace.etl>` to load the `.etl` file into WPA and analyze it.
    1. On the left, double click on the view you're interested in. Some useful ones include "Computation > CPU Usage (Sampled)," "System Activity > Processes," and "Storage > File I/O > Activity by Process, Thead, Type."
    2. Sort by Process name and select other related processes you may be interested in like `swift.exe`. Then right click on the selected processes and choose "Filter To Selection" to hide all other processes. Alternatively, right click on the table header, select "Edit Filters," and enter "Process Name" constraints.
    3. Go to Trace > Configure Symbol Paths. It will open a Configure Symbols dialog. Add `S:\Program Files\Swift\Runtimes\0.0.0\usr\bin` and `S:\Program Files\Swift\Toolchains\0.0.0+Asserts\usr\bin` the list in the Paths tab. This enables WPA to find PDB files for our EXEs and DLLs. You shouldn't have to reconfigure this every time you run WPA.
    4. Make sure Trace > Load Symbols is selected. If it's unselected, it will likely take some time to load symbols once you select it.
    5. The Stack column of the process list should now show function symbols as you drill down into execution times for various parts of the call stack. If you're in a view that doesn't include Stack or another column you want, right click on the table header and select "More Columns..." > the column view you want or right click the table header and select "Open View Editor..." for more detailed configuration.

# Alternative: Using Visual Studio to profile Swift compiler runs

Visual Studio (the full IDE, not VS Code) has a built-in profiler that can profile arbitrary executables without requiring a Visual Studio solution or project. This includes the Swift compiler, provided it is built with PDB format debug symbols. By default, Visual Studio's profiler only records profiling data for the directly invoked process, not child processes. This makes it a less than ideal choice for top level Swift builds that invoke many child processes, but can still be useful if you have a specific `swift-frontend.exe` command you would like to profile.

Assuming you want to profile `swiftc.exe`'s call to `swift-frontend.exe` run on an input source file `S:\Temp\hello.swift`:

1. On the command line, run `swiftc.exe -v hello.swift` (for example). This will give you a `swift-frontend.exe` command that `swiftc.exe` calls.
2. Open Visual Studio without any files/project.
3. Debug > Performance Profiler...
4. Change Target > Executable
    1. Path to executable: `S:\Program Files\Swift\Toolchains\0.0.0+Asserts\usr\bin\swift-frontend.exe`
    2. Command line options: (as output in step (1) above).
    3. Working directory: `S:\Temp`
    4. Environment Variables: see "Using the Toolchain" in [WindowsQuickStart.md](https://github.com/compnerd/swift-build/blob/main/docs/WindowsQuickStart.md).
5. Click Ok, select "CPU Usage," click Start.
6. When the run is done, Visual Studio will present you with a GUI with lists of the functions that take the most time as well as hot traces, which you can explore further.

# Alternative: Using VTune to profile Swift compiler runs

Intel VTune supports profiling using Intel processors' Performance Monitoring Unit (PMU), which provides hardware-level performance counters. It's available as an integrated GUI application as well as a standalone command line executable (`vtune.exe`) that can collect performance logs to be analyzed in the GUI. We'll discuss the GUI workflow here.

1. Download VTune for Windows from Intel and install it on a Windows computer with an Intel processor. The first time you launch VTune, a helpful walkthrough tutorial is available. After that:
2. On the Welcome tab, click "Configure Analysis..." and configure it as follows:
    1. Where: Local Host (default)
    2. What: Launch Application (default)
    3. Application: for example, `S:\Program Files\Swift\Toolchains\0.0.0+Asserts\usr\bin\swiftc.exe`
    4. Application parameters: for example, `hello.swift`
    5. Uncheck "Use application directory as working directory"
    6. Working directory: for example, `S:\Temp`
    7. Advanced >
        1. User-defined environment variables: see "Using the Toolchain" in [WindowsQuickStart.md](https://github.com/compnerd/swift-build/blob/main/docs/WindowsQuickStart.md).
        2. Make sure "Analyze child processes" is checked (it is by default)
        3. Duration time estimate: Under 1 minute
        4. Store result in (and create link file to) another directory: `C:\Users\%USER%\Documents\VTune\Projects\swift_hello` (note: manually fill in your `%USER%`)
    8. How: Select what you would like to profile. Hotspots will tell you where most of the time is spent in the code.
    9. Click the "Search Sources/Binaries" button (folder with a magnifying glass) and add the following search directories:
        - `S:\Program Files\Swift\Runtimes\0.0.0\usr\bin`
        - `S:\Program Files\Swift\Toolchains\0.0.0+Asserts\usr\bin`

    10. Click the "Start" button ("play" button style triangle above). If you would prefer to run `vtune.exe` from the command line, click the ">_" button seen above to get the command to run.
3. After profiling `swiftc.exe hello.swift` exits, you will be taken to an analysis tab to view the results, which include hot traces and time consuming functions.

# Reference: Survey of Windows profiling tools

This section describes Windows profiling tools that can be used with the Swift compiler and the relationships between them.

- **WPT** = Windows Performance Toolkit is a collection of tools & infrastructure for profiling from Microsoft. Includes the WP* and XPerf* tools below.
- **ETW** = Event Tracing for Windows is the OS-level feature for collecting application and kernel performance event traces. Microsoft/Windows Profiler tools drive ETW to collect events.
- Performance event recorders:
    - **WPR** = Windows Performance Recorder lets you start and stop an ETW session by running `wpr.exe`. Collects profiling data for all running processes and saves it to a `.etl` file. See [utils/windows-profiling-tools/RunProfiler.ps1](../utils/windows-profiling-tools/RunProfiler.ps1) for a PowerShell script that runs WPR for the duration of a specified command invocation.
    - **XPerf** is an older, lower level equivalent of WPR. It supports a few obscure ETW settings not exposed by WPR, but WPR is recommended if meets your needs.
    - **Visual Studio** also supports profiling `.exe`s that don’t have to be part of a Visual Studio solution/project, starting by opening VS without any files/project, then going to Debug > Performance Profiler... By default, it only profiles the initial process
    launched from the `.exe`, not child processes.
    - **Intel VTune** supports profiling using Intel processors’ Performance Monitoring Unit (PMU) rather than ETW. It’s available as an integrated GUI application as well as a standalone command line executable that can collect performance logs to be analyzed in the GUI.
- File formats:
    - `.etl` is the output binary event log file format produced by tools like WPR and XPerf. There are also a command line tools including `xperf.exe` and `tracerpt.exe` that can be used to convert an ETL file into multiple text-based formats.
    - `.wprp` is an XML configuration file format used to configure WPR’s operation. For example, [utils/windows-profiling-tools/CpuAndWaitsWithLargeBuffers.wprp](../utils/windows-profiling-tools/CpuAndWaitsWithLargeBuffers.wprp) increases the in-memory buffer size that WPR configures ETW to use so as not to drop events during recording.
- Analysis GUIs:
    - **WPA** = Windows Performance Analyzer is a GUI that lets you analyze various aspects of the performance trace data found in `.etl` files. It can be invoked on the command line by running `wpa.exe` with the `.etl` file as an argument. See [Using Event Tracing for Windows (ETW) to profile Swift compiler runs](#recommended-using-event-tracing-for-windows-etw-to-profile-swift-compiler-runs) for how to view program profiles with symbols.
    - **XPerfView** is an older GUI for analyzing `.etl` files that has been replaced by WPA.
    - **Visual Studio** directly displays profile results after recording a profile data from a run of a program. By default, it only shows profiles for the directly invoked program, not for subprocesses.
    - **Intel VTune** can either directly display profile results after recording them or display profile results from a command line run of `vtune.exe` (possibly run on a different host).
- Analysis CLIs:
    - `wpaexport.exe` takes a profile built in the WPA GUI and exports data from a `.etl` file according to that profile. Note that profiles that include hierarchical lists in the GUI may only export the top levels of the hierarchy. In this case, you can reconfigure the WPA GUI view to make the list flat and then export a new profile.
    - `xperf.exe` can be used to operate on `.etl` files recorded with either `wpr.exe` or `xperf.exe` itself. Examples:
        - Dump all events from a `.etl` file in text format: `xperf -i <INPUT>.etl -o <OUTPUT>.txt -symbols -target machine -a dumper -stacktimeshifting` where `<INPUT>` and `<OUTPUT>` are files of your choice with `_NT_SYMBOL_PATH` set in the environment to point to directories containing PDB files, e.g. in PowerShell:
            - `$env:_NT_SYMBOL_PATH="S:\b\5\bin\bin"` (list all directories containing relevant PDB files, separated by semicolons)
        - List execution times per function from a `.etl` file, *not including  the time taken by calls to other functions*: `xperf -i <INPUT>.etl -o <OUTPUT>.txt -symbols -a profile -detail`

# Building the Swift toolchain for Windows with debugging symbols

To build the Swift compiler with PDB format debug symbols that are used by profile analysis tools like WPA or Visual Studio, follow the https://github.com/compnerd/swift-build/blob/main/docs/WindowsQuickStart.md instructions, using the following `build.cmd` command line (or a variant on it based on your exact needs):

`S:\SourceCache\swift\utils\build.cmd -Windows -DebugInfo -SkipPackaging -WindowsSDKArchitectures x64 -CDebugFormat codeview -SwiftDebugFormat codeview`

If you need to build a package that you can install on a separate Windows host, either remove `-SkipPackaging` in the above command, or if you've already run the above build, substitute `-SkipBuild` in place of `-SkipPackaging`.
