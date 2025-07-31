#!/usr/bin/env python3
"""
Script to verify C family interop code snippets in markdown documentation.

This script parses markdown files, extracts test cases with HTML comment annotations,
generates test directories, runs swift-synthesize-interface, and compares results.
"""

import re
import shutil
import subprocess
import argparse
import difflib
from pathlib import Path
from typing import Dict, List, Tuple
from dataclasses import dataclass

@dataclass
class Test:
    name: str
    blocks: Dict[str, str]

class MarkdownParser:
    """Parser for extracting test cases from markdown files"""
    
    def __init__(self):
        self.test_block_pattern = re.compile(r'<!-- test-block:\s*([^>]+?)\s*-->')
        self.code_block_pattern = re.compile(r'```(\w+)?\n(.*?)```', re.DOTALL)
    
    def parse_file(self, file_path: Path) -> Test:
        """Parse a markdown file and extract a single test case using filename as test name"""
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
                
        lines = content.split('\n')
        block_type = None
        blocks = {}
        
        for i, line in enumerate(lines, 1):
            # Check for test block type
            test_block_match = self.test_block_pattern.search(line)
            if test_block_match:
                block_type = test_block_match.group(1)
                continue
            
            # Check for code block start
            if line.startswith('```') and block_type:
                # Find the end of the code block
                code_content = []
                j = i  # Start from the next line after ```
                while j < len(lines) and not lines[j].startswith('```'):
                    code_content.append(lines[j])
                    j += 1
                
                if j < len(lines):  # Found closing ```
                    content_str = '\n'.join(code_content)
                    assert block_type not in blocks
                    blocks[block_type] = content_str
                    block_type = None

        return Test(
            name=file_path.stem,
            blocks=blocks
        )


class TestGenerator:
    """Generates test files and directories"""
    
    def __init__(self, output_dir: Path):
        self.output_dir = output_dir
        self.output_dir.mkdir(parents=True, exist_ok=True)
    
    def generate_test(self, test: Test) -> Tuple[Path, List[Dict]]:
        """Generate test directory and files for a test case"""
        tests_dir = self.output_dir / test.name
        tests_dir.mkdir(parents=True, exist_ok=True)
        
        # Determine subtests
        subtests = self._determine_subtests(test)
        
        # Generate subtests
        for subtest in subtests:
            self._generate_subtest(test, tests_dir, subtest)
        
        return tests_dir, subtests
    
    def _determine_subtests(self, test: Test) -> List[Dict]:
        """Determine what subtests to generate based on available blocks"""
        subtests = []
        
        if "c-header" in test.blocks and "swift-interface-default" in test.blocks:
            subtests.append({
                "name": "default",
                "inputs": ["c-header"],
                "expected": "swift-interface-default"
            })

        if "c-header-annotated" in test.blocks and "swift-interface-refined" in test.blocks:
            subtests.append({
                "name": "attributes",
                "inputs": ["c-header-annotated"],
                "expected": "swift-interface-refined"
            })

        if ("c-header" in test.blocks and 
            "apinotes" in test.blocks and 
            "swift-interface-refined" in test.blocks):
            subtests.append({
                "name": "apinotes",
                "inputs": ["c-header", "apinotes"],
                "expected": "swift-interface-refined"
            })

        return subtests
    
    def _generate_subtest(self, test: Test, tests_dir: Path, subtest: Dict):
        """Generate files for a specific test subtest"""
        test_dir = tests_dir / subtest["name"]
        test_dir.mkdir(parents=True, exist_ok=True)
        
        # Generate example.h
        header_content = self._get_header_content(test, subtest["inputs"])
        f = test_dir / "example.h"
        f.write_text(header_content)

        # Generate module.modulemap
        modulemap_content = """module Example {
    header "example.h"
    export *
}
"""
        f = test_dir / "module.modulemap"
        f.write_text(modulemap_content)

        # Generate apinotes if needed
        if "apinotes" in subtest["inputs"] and "apinotes" in test.blocks:
            f = test_dir / "example.apinotes"
            f.write_text(test.blocks["apinotes"])

        # Save expected Swift interface
        expected_content = test.blocks[subtest["expected"]]
        f = test_dir / "expected.swift"
        f.write_text(expected_content)
    
    def _get_header_content(self, test: Test, inputs: List[str]) -> str:
        """Get the appropriate C header content based on inputs"""
        # Priority: annotated version over regular version
        if "c-header-annotated" in inputs:
            return test.blocks["c-header-annotated"]
        elif "c-header" in inputs:
            return test.blocks["c-header"]
        else:
            raise ValueError(f"No suitable header found in inputs: {inputs}")

class TestRunner:
    """Main test runner that coordinates all components"""
    def __init__(self, output_dir: Path = None):        
        self.parser = MarkdownParser()
        self.generator = TestGenerator(output_dir)
    
    def run_tests(self, files: List[Path]) -> bool:
        """Run tests for all markdown files"""
        passed = 0
        failed = 0
        
        for file in files:
            test = self.parser.parse_file(file)
            print(f"Running: {test.name}")
            tests_dir, subtests = self.generator.generate_test(test)
            # Run each subtest
            for subtest in subtests:
                subtest_name = subtest["name"]
                print(f"  Running: {subtest_name}", end="")

                error = None
                try:
                    test_dir = tests_dir / subtest_name
                    expected_file = test_dir / "expected.swift"
                    actual_file = self.swift_synthesize_interface(test_dir)
                    expected = expected_file.read_text()
                    actual = actual_file.read_text()
                    diff = self.verify_result(expected, actual)
                    if diff:
                        error = ''.join(diff)
                except Exception as e:
                    error = f"{e}"

                if error:
                    failed += 1
                    print(" ❌")
                    print(f"\n{error}\n")
                else:
                    passed += 1
                    print(" ✅")

        print("")
        total = passed + failed
        percent = passed/total*100
        if failed != 0:
            print(f"Failure: {passed}/{total} ({percent:.1f}%) tests pass")
        else:
            print(f"Success: {total} tests")

        return failed == 0

    def swift_synthesize_interface(self, test_dir: Path) -> Path:
        stdout = subprocess.check_output(
            [
                'swift-synthesize-interface',
                '-module-name', 'Example',
                '-I', '.',
                '-target', 'x86_64-apple-macos10.9'
            ],
            cwd=str(test_dir),
            text=True,
        )
        actual_file = test_dir / "actual.swift"
        actual_file.write_text(stdout)
        return actual_file

    def verify_result(self, expected: str, actual: str):
        def normalize(content: str) -> str:
            """Normalize content for comparison"""
            # Remove comments and empty lines
            lines = []
            for line in content.split('\n'):
                line = line.strip()
                if line and not line.startswith('//'):
                    lines.append(line)
            return '\n'.join(lines)

        return list(difflib.unified_diff(
            normalize(expected).splitlines(keepends=True),
            normalize(actual).splitlines(keepends=True),
            fromfile='Expected',
            tofile='Actual',
        ))

def find_markdown_files(paths: List[Path], recursive: bool = False) -> List[Path]:
    """Find markdown files from a list of file/directory paths"""
    markdown_files = []
    
    for path in paths:
        if path.is_file():
            if path.suffix.lower() == '.md':
                markdown_files.append(path)
            else:
                print(f"Warning: Skipping non-markdown file: {path}")
        elif path.is_dir():
            if recursive:
                # Recursively find all .md files
                markdown_files.extend(path.rglob('*.md'))
            else:
                # Only find .md files in the immediate directory
                markdown_files.extend(path.glob('*.md'))
        else:
            print(f"Warning: Path does not exist: {path}")
    
    return sorted(markdown_files)

def main():
    parser = argparse.ArgumentParser(description="Verify C family interop code snippets")
    parser.add_argument("files", nargs="+", help="Markdown files or directories to process")
    parser.add_argument("--output", "-o", default="tests", help="Output directory for test files")
    parser.add_argument("--keep-tests", action="store_true", help="Keep generated test files")
    parser.add_argument("--recursive", "-r", action="store_true", help="Recursively find markdown files in directories")
    
    args = parser.parse_args()
    
    input_paths = [Path(f) for f in args.files]
    output_dir = Path(args.output)
    
    # Find markdown files
    files = find_markdown_files(input_paths, args.recursive)
    if not files:
        print("Error: No markdown files found")
        return 1

    # Run tests
    runner = TestRunner(output_dir)
    result = runner.run_tests(files)
    
    # Clean up test files unless --keep-tests is specified
    if not args.keep_tests:
        print(f"\nCleaning up files in '{output_dir}'...")
        shutil.rmtree(output_dir, ignore_errors=True)

    return 0 if result else 1

if __name__ == "__main__":
    exit(main())
