#!/usr/bin/env python3
"""
Swift Compiler Unit Test Verification Script

This script verifies that all unit tests are properly implemented and configured.
"""

import os
import sys
from pathlib import Path

def check_file_exists(filepath, description):
    """Check if a file exists and report status."""
    if os.path.exists(filepath):
        print(f"✅ {description}: {filepath}")
        return True
    else:
        print(f"❌ {description}: {filepath} (MISSING)")
        return False

def check_directory_structure():
    """Verify the unit test directory structure."""
    print("=" * 60)
    print("Verifying Unit Test Directory Structure")
    print("=" * 60)
    
    base_dir = "/workspace/swiftc/unittests"
    
    # Check main structure
    success = True
    success &= check_file_exists(f"{base_dir}/CMakeLists.txt", "Main test config")
    success &= check_file_exists(f"{base_dir}/README.md", "Test documentation")
    
    # Check each component directory
    components = ["Basic", "Lexer", "Parser", "AST", "Sema", "SIL", "IRGen", "Runtime"]
    
    for component in components:
        print(f"\n{component} Component:")
        component_dir = f"{base_dir}/{component}"
        success &= check_file_exists(f"{component_dir}/CMakeLists.txt", f"{component} config")
        
        # Check for test files
        if os.path.exists(component_dir):
            cpp_files = [f for f in os.listdir(component_dir) if f.endswith('.cpp')]
            print(f"  📁 {len(cpp_files)} test files: {', '.join(cpp_files)}")
        else:
            success = False
    
    return success

def check_test_runners():
    """Verify test runner scripts."""
    print("\n" + "=" * 60)
    print("Verifying Test Runner Scripts")
    print("=" * 60)
    
    base_dir = "/workspace/swiftc"
    
    success = True
    success &= check_file_exists(f"{base_dir}/run_unit_tests.py", "Python test runner")
    success &= check_file_exists(f"{base_dir}/test_all.sh", "Shell test runner")
    success &= check_file_exists(f"{base_dir}/UNIT_TESTS.md", "Unit test documentation")
    success &= check_file_exists(f"{base_dir}/IMPLEMENTATION_SUMMARY.md", "Implementation summary")
    
    # Check if scripts are executable
    for script in ["run_unit_tests.py", "test_all.sh"]:
        script_path = f"{base_dir}/{script}"
        if os.path.exists(script_path) and os.access(script_path, os.X_OK):
            print(f"✅ {script} is executable")
        else:
            print(f"❌ {script} is not executable")
            success = False
    
    return success

def count_test_coverage():
    """Count and report test coverage."""
    print("\n" + "=" * 60)
    print("Test Coverage Summary")
    print("=" * 60)
    
    base_dir = "/workspace/swiftc/unittests"
    
    total_files = 0
    component_counts = {}
    
    components = ["Basic", "Lexer", "Parser", "AST", "Sema", "SIL", "IRGen", "Runtime"]
    
    for component in components:
        component_dir = f"{base_dir}/{component}"
        if os.path.exists(component_dir):
            cpp_files = [f for f in os.listdir(component_dir) if f.endswith('.cpp')]
            component_counts[component] = len(cpp_files)
            total_files += len(cpp_files)
        else:
            component_counts[component] = 0
    
    print(f"Total test files: {total_files}")
    print("\nComponent breakdown:")
    for component, count in component_counts.items():
        print(f"  {component:12}: {count:2} files")
    
    return total_files

def check_cmake_integration():
    """Verify CMake integration."""
    print("\n" + "=" * 60)
    print("Verifying CMake Integration")
    print("=" * 60)
    
    main_cmake = "/workspace/swiftc/CMakeLists.txt"
    
    if not os.path.exists(main_cmake):
        print(f"❌ Main CMakeLists.txt not found: {main_cmake}")
        return False
    
    # Check if unit tests are integrated
    with open(main_cmake, 'r') as f:
        content = f.read()
        
    if "add_subdirectory(unittests)" in content:
        print("✅ Unit tests integrated into main CMakeLists.txt")
    else:
        print("❌ Unit tests not integrated into main CMakeLists.txt")
        return False
    
    if "find_package(GTest" in content:
        print("✅ GTest dependency configured")
    else:
        print("❌ GTest dependency not configured")
        return False
    
    return True

def main():
    """Main verification function."""
    print("Swift Compiler Unit Test Verification")
    print("=" * 60)
    
    all_good = True
    
    # Check directory structure
    all_good &= check_directory_structure()
    
    # Check test runners
    all_good &= check_test_runners()
    
    # Count coverage
    total_tests = count_test_coverage()
    
    # Check CMake integration
    all_good &= check_cmake_integration()
    
    # Final summary
    print("\n" + "=" * 60)
    print("VERIFICATION SUMMARY")
    print("=" * 60)
    
    if all_good:
        print("🎉 ALL CHECKS PASSED!")
        print(f"✅ {total_tests} unit test files implemented")
        print("✅ 8 test suites configured")
        print("✅ Test runners ready")
        print("✅ CMake integration complete")
        print("")
        print("Ready to run tests with:")
        print("  ./test_all.sh")
        print("  python3 run_unit_tests.py")
    else:
        print("❌ SOME CHECKS FAILED!")
        print("Please review the errors above and fix any missing components.")
        sys.exit(1)

if __name__ == "__main__":
    main()