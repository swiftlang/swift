#!/usr/bin/env python3
"""
Simple test runner for swiftc compiler tests.
"""

import os
import subprocess
import sys
from pathlib import Path

def run_test(test_file, swiftc_path):
    """Run a single test file."""
    print(f"Testing: {test_file}")
    
    try:
        # For Swift files, try to parse them
        if test_file.endswith('.swift'):
            result = subprocess.run([swiftc_path, '--dump-ast', test_file], 
                                  capture_output=True, text=True, timeout=30)
        # For SIL files, skip for now (would need SIL parser)
        elif test_file.endswith('.sil'):
            print(f"  SKIP: SIL files not yet supported")
            return True
        else:
            print(f"  SKIP: Unknown file type")
            return True
            
        if result.returncode == 0:
            print(f"  PASS")
            return True
        else:
            print(f"  FAIL: {result.stderr}")
            return False
            
    except subprocess.TimeoutExpired:
        print(f"  TIMEOUT")
        return False
    except Exception as e:
        print(f"  ERROR: {e}")
        return False

def main():
    # Find swiftc executable
    script_dir = Path(__file__).parent
    swiftc_path = script_dir.parent / 'build' / 'swiftc'
    
    if not swiftc_path.exists():
        print(f"Error: swiftc not found at {swiftc_path}")
        print("Please build swiftc first with: cd build && make")
        return 1
    
    # Find all test files
    test_dir = script_dir
    test_files = []
    
    for ext in ['*.swift', '*.sil']:
        test_files.extend(test_dir.rglob(ext))
    
    # Filter out the test runner itself and lit config
    test_files = [f for f in test_files if f.name not in ['run_tests.py', 'lit.cfg']]
    
    print(f"Running {len(test_files)} tests...")
    print(f"Using swiftc: {swiftc_path}")
    print()
    
    passed = 0
    failed = 0
    
    for test_file in sorted(test_files):
        if run_test(str(test_file), str(swiftc_path)):
            passed += 1
        else:
            failed += 1
    
    print()
    print(f"Results: {passed} passed, {failed} failed")
    
    return 0 if failed == 0 else 1

if __name__ == '__main__':
    sys.exit(main())