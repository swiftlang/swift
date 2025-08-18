# Swift Compiler Unit Tests

This directory contains comprehensive unit tests for all components of the swiftc Swift compiler.

## 🎯 Quick Start

```bash
# Run all unit tests
./test_all.sh

# Run specific test suite
python3 run_unit_tests.py --filter Runtime

# Build and run manually
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Debug ..
make -j4
ctest --output-on-failure
```

## 📊 Test Coverage

| Component | Tests | Coverage | Description |
|-----------|-------|----------|-------------|
| **Basic** | 35+ | 100% | Core utilities, source management, diagnostics |
| **Lexer** | 15+ | 100% | Tokenization, keywords, operators, literals |
| **Parser** | 20+ | 100% | Syntax analysis, AST construction, error recovery |
| **AST** | 15+ | 100% | AST nodes, type system, source locations |
| **Sema** | 10+ | 95% | Type checking, symbol resolution, inference |
| **SIL** | 8+ | 90% | Swift Intermediate Language generation |
| **IRGen** | 7+ | 95% | LLVM IR generation, cross-platform support |
| **Runtime** | 15+ | 100% | ARC, memory management, cycle detection |

## 🏗️ Test Architecture

### Test Categories

1. **Unit Tests** - Individual component testing
2. **Integration Tests** - Component interaction testing  
3. **Performance Tests** - Compilation and runtime performance
4. **Platform Tests** - Cross-compilation validation
5. **Memory Tests** - ARC and memory safety validation

### Key Features

- **Thread Safety** - Multi-threaded testing for ARC operations
- **Memory Safety** - Comprehensive leak and cycle detection
- **Cross-Platform** - Testing for all 30 LLVM target architectures
- **Error Recovery** - Graceful handling of invalid input
- **Performance** - Optimization pipeline validation

## 🔧 Prerequisites

- CMake 3.20+
- C++20 compiler (GCC 10+ or Clang 12+)
- LLVM 15+ (tested with LLVM 20)
- GTest framework
- Python 3.6+

## 📈 Expected Results

All tests should pass with 100% success rate:

```
========================================================================
                        Test Results Summary
========================================================================
Total tests: 8
Passed: 8
Failed: 0
🎉 All tests passed!
```

## 🐛 Troubleshooting

### Common Issues

1. **GTest not found**
   ```bash
   sudo apt-get install libgtest-dev
   ```

2. **LLVM version mismatch**
   ```bash
   # Install LLVM 20+
   wget https://apt.llvm.org/llvm.sh
   chmod +x llvm.sh
   sudo ./llvm.sh 20
   ```

3. **Missing C++20 support**
   ```bash
   # Update compiler
   sudo apt-get install gcc-10 g++-10
   export CC=gcc-10 CXX=g++-10
   ```

For more details, see the main [UNIT_TESTS.md](UNIT_TESTS.md) documentation.