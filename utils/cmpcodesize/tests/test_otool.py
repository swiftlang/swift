import subprocess
import unittest

from cmpcodesize import otool


# Store parameters passed to subprocess.check_output into
# this global variable.
_subprocess_check_output_arguments = []


# We'll monkey-patch subprocess.check_output with this stub
# function, which simply records whatever's passed to
# check_output into the global variables above.
def _stub_subprocess_check_output(arguments, *args, **kwargs):
    global _subprocess_check_output_arguments
    _subprocess_check_output_arguments = arguments


class OtoolTestCase(unittest.TestCase):
    def setUp(self):
        # Monkey-patch subprocess.check_output with our stub function.
        self._original_check_output = subprocess.check_output
        subprocess.check_output = _stub_subprocess_check_output

    def tearDown(self):
        # Undo the monkey-patching.
        subprocess.check_output = self._original_check_output

    def test_fat_headers(self):
        otool.fat_headers('/path/to/foo')
        self.assertEqual(_subprocess_check_output_arguments,
                         ['otool', '-V', '-f', '/path/to/foo'])

    def test_load_commands_with_no_architecture(self):
        otool.load_commands('/path/to/bar')
        self.assertEqual(_subprocess_check_output_arguments,
                         ['otool', '-l', '/path/to/bar'])

    def test_load_commands_with_architecture(self):
        otool.load_commands('/path/to/baz', architecture='arch-foo')
        self.assertEqual(
            _subprocess_check_output_arguments,
            ['otool', '-arch', 'arch-foo', '-l', '/path/to/baz'])

    def test_load_commands_no_architecture_but_including_text_sections(self):
        otool.load_commands(
            '/path/to/flim', include_text_sections=True)
        self.assertEqual(
            _subprocess_check_output_arguments,
            ['otool', '-l', '-v', '-t', '/path/to/flim'])

    def test_load_commands_with_architecture_and_including_text_sections(self):
        otool.load_commands(
            '/path/to/flam',
            architecture='arch-bar',
            include_text_sections=True)
        self.assertEqual(
            _subprocess_check_output_arguments,
            ['otool', '-arch', 'arch-bar', '-l', '-v', '-t', '/path/to/flam'])

    def test_text_sections_no_architecture(self):
        otool.text_sections('/path/to/fish')
        self.assertEqual(
            _subprocess_check_output_arguments,
            ['otool', '-v', '-s', '__TEXT', '__textcoal_nt', '/path/to/fish'])

    def test_text_sections_with_architecture(self):
        otool.text_sections('/path/to/frosh', architecture='arch-baz')
        self.assertEqual(
            _subprocess_check_output_arguments,
            ['otool', '-arch', 'arch-baz', '-v', '-s',
             '__TEXT', '__textcoal_nt', '/path/to/frosh'])
