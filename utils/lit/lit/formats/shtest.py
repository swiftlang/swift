from __future__ import absolute_import

import lit.TestRunner
from .base import FileBasedTest

class ShTest(FileBasedTest):
    def __init__(self, execute_external = False):
        self.execute_external = execute_external

    def execute(self, test, litConfig):
        return lit.TestRunner.executeShTest(test, litConfig,
                                            self.execute_external)
