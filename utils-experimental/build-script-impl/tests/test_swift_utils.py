
import unittest
import os
import os.path

from build_script import swift_utils


class SwiftUtilsTestCase(unittest.TestCase):

    def test_swift_utils(self):

        def isexec(path):
            return (
                os.path.exists(path) and
                not os.path.isdir(path) and
                os.access(path, os.X_OK))

        def isdir(path):
            return os.path.isdir(path)

        self.assertTrue(isexec(swift_utils('build-script')))
        self.assertTrue(isexec(swift_utils('recursive-lipo')))
        self.assertTrue(isexec(swift_utils('toolchain-codesign')))
        self.assertTrue(isexec(swift_utils('toolchain-installer')))
        self.assertTrue(isexec(swift_utils('swift-stdlib-tool-substitute')))
        self.assertTrue(isdir(swift_utils('darwin-installer-scripts')))

        # Returns path even if the file does not exist
        self.assertIsNotNone(swift_utils('not-exists-tool-name'))
