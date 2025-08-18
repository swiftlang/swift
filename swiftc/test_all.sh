#!/bin/bash

# Swift Compiler Complete Test Suite Runner
# This script runs all tests: unit tests, integration tests, and validation tests

set -e  # Exit on any error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="${SCRIPT_DIR}/build"
SOURCE_DIR="${SCRIPT_DIR}"

echo "========================================================================"
echo "                    Swift Compiler Test Suite"
echo "========================================================================"
echo "Source directory: ${SOURCE_DIR}"
echo "Build directory: ${BUILD_DIR}"
echo ""

# Function to print section headers
print_section() {
    echo ""
    echo "========================================================================"
    echo "$1"
    echo "========================================================================"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check prerequisites
print_section "Checking Prerequisites"

if ! command_exists cmake; then
    echo "❌ CMake not found! Please install CMake 3.20+."
    exit 1
fi

if ! command_exists make; then
    echo "❌ Make not found! Please install make."
    exit 1
fi

echo "✅ CMake found: $(cmake --version | head -n1)"
echo "✅ Make found: $(make --version | head -n1)"

# Check for GTest
if pkg-config --exists gtest; then
    echo "✅ GTest found: $(pkg-config --modversion gtest)"
else
    echo "⚠️  GTest not found via pkg-config, will try to find via CMake"
fi

# Build the project
print_section "Building Swift Compiler with Unit Tests"

mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Configure
echo "Configuring build..."
cmake -DCMAKE_BUILD_TYPE=Debug \
      -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      "${SOURCE_DIR}"

# Build
echo "Building..."
make -j$(nproc)

# Run unit tests if they were built
print_section "Running Unit Tests"

if [ -d "${BUILD_DIR}/unittests" ]; then
    echo "Unit tests found, running..."
    
    # Use Python runner if available
    if [ -f "${SOURCE_DIR}/run_unit_tests.py" ] && command_exists python3; then
        echo "Using Python test runner..."
        cd "${SOURCE_DIR}"
        python3 run_unit_tests.py --build-dir "${BUILD_DIR}" --no-build
    else
        echo "Using CTest..."
        cd "${BUILD_DIR}"
        ctest --output-on-failure
    fi
else
    echo "⚠️  Unit tests not built (GTest might not be available)"
fi

# Run integration tests
print_section "Running Integration Tests"

if [ -d "${SOURCE_DIR}/test" ]; then
    echo "Running integration tests..."
    cd "${SOURCE_DIR}"
    
    if [ -f "test/run_tests.py" ]; then
        python3 test/run_tests.py
    else
        echo "Integration test runner not found, skipping..."
    fi
else
    echo "Integration tests not found, skipping..."
fi

# Run compiler validation tests
print_section "Running Compiler Validation Tests"

if [ -f "${BUILD_DIR}/tools/swiftc-binary/swiftc-binary" ]; then
    SWIFTC="${BUILD_DIR}/tools/swiftc-binary/swiftc-binary"
    echo "Testing compiler binary: ${SWIFTC}"
    
    # Test version output
    echo "Testing version output..."
    if "${SWIFTC}" --version; then
        echo "✅ Version output works"
    else
        echo "❌ Version output failed"
    fi
    
    # Test help output
    echo "Testing help output..."
    if "${SWIFTC}" --help >/dev/null 2>&1; then
        echo "✅ Help output works"
    else
        echo "❌ Help output failed"
    fi
    
    # Test simple compilation
    echo "Testing simple compilation..."
    cat > "${BUILD_DIR}/test_simple.swift" << 'EOF'
let x = 42
print("Hello, World!")
EOF
    
    if "${SWIFTC}" -c -o "${BUILD_DIR}/test_simple.o" "${BUILD_DIR}/test_simple.swift"; then
        echo "✅ Simple compilation works"
        rm -f "${BUILD_DIR}/test_simple.o" "${BUILD_DIR}/test_simple.swift"
    else
        echo "❌ Simple compilation failed"
    fi
    
else
    echo "⚠️  Compiler binary not found, skipping validation tests"
fi

print_section "Test Suite Complete"

echo "All tests completed!"
echo ""
echo "Next steps:"
echo "  - Review any failed tests above"
echo "  - Run specific test suites with: python3 run_unit_tests.py --filter <TestName>"
echo "  - Run individual tests with: ./<TestExecutable> --gtest_filter=<TestCase>"
echo ""
echo "For more information, see the README.md file."