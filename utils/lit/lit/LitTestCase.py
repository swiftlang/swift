from __future__ import absolute_import
import unittest

import lit.Test

"""
TestCase adaptor for providing a 'unittest' compatible interface to 'lit' tests.
"""

class UnresolvedError(RuntimeError):
    pass
        
class LitTestCase(unittest.TestCase):
    def __init__(self, test, run):
        unittest.TestCase.__init__(self)
        self._test = test
        self._run = run

    def id(self):
        return self._test.getFullName()

    def shortDescription(self):
        return self._test.getFullName()

    def runTest(self):
        # Run the test.
        self._run.execute_test(self._test)

        # Adapt the result to unittest.
        result = self._test.result
        if result.code is lit.Test.UNRESOLVED:
            raise UnresolvedError(result.output)
        elif result.code.isFailure:
            self.fail(result.output)
