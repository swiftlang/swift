# Swift 6.1 Build Hanging Fix for Ubuntu 24.04

This repository contains tools to address the Swift 6.1 build hanging issue on Ubuntu 24.04. When using Swift 6.1 on Ubuntu 24.04, the build process may hang indefinitely when the `.build` folder exists from a previous build.

## Files Included

1. **fix_swift_build_hang.sh** - Script to fix build hanging by cleaning problematic files while preserving dependencies
2. **check_swift_build_db.sh** - Utility to check and repair SwiftPM's SQLite database
3. **swift-build-ubuntu.sh** - Wrapper script that automatically detects and resolves hanging builds
4. **README_SWIFT_BUILD_HANG.md** - Detailed explanation of the issue and solutions

## Quick Start

The fastest way to fix your hanging build:

```bash
# Copy all scripts to your Swift package directory
cd your_swift_package
./fix_swift_build_hang.sh
swift build -c release
```

For automatic detection and fixing of hanging builds:

```bash
# Use the wrapper script instead of direct swift build
./swift-build-ubuntu.sh -c release
```

## Installation

```bash
# Clone or download this repository
git clone https://github.com/yourusername/swift-ubuntu-fixes.git

# Copy the scripts to your project directory
cp swift-ubuntu-fixes/*.sh your_project_directory/

# Make the scripts executable
chmod +x your_project_directory/*.sh

# Use the wrapper script or fix script as needed
cd your_project_directory
./swift-build-ubuntu.sh -c release
```

## Understanding the Issue

The most likely causes of the build hanging issue:

1. **SQLite Database Corruption**: SwiftPM uses SQLite for build state management
2. **Lock File Issues**: Stale lock files from interrupted builds
3. **Permissions Problems**: File permission issues in the `.build` directory
4. **Compatibility Issues**: Swift 6.1 may have specific issues with Ubuntu 24.04

See **README_SWIFT_BUILD_HANG.md** for more detailed information.

## Script Descriptions

### fix_swift_build_hang.sh

This script performs targeted cleaning of problematic files while preserving downloaded dependencies:

- Removes lock files
- Fixes permissions
- Cleans SQLite journal files
- Resets build state

### check_swift_build_db.sh

This utility focuses specifically on SwiftPM's SQLite database:

- Checks database integrity
- Removes journal files
- Attempts to repair corrupt databases

### swift-build-ubuntu.sh

This wrapper automates the entire process:

1. Attempts a normal build first
2. Watches for hanging (no output for 30+ seconds)
3. If hanging occurs, runs the database checker
4. Tries a selective clean build
5. If still hanging, falls back to full `.build` removal

## Reporting Issues

If these scripts don't resolve your issue, please report it to the Swift project:

1. Create a new issue at [Swift GitHub repository](https://github.com/apple/swift/issues)
2. Include Ubuntu version, Swift version, and exact steps to reproduce
3. Include output from `swift build -c release -v`

## License

These scripts are released under the MIT License. 