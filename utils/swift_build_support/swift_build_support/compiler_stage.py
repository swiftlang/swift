# ===--- compiler_stage.py -----------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https:#swift.org/LICENSE.txt for license information
# See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===#

class StageArgs(object):
    def __init__(self, stage, args):
        self.stage = stage
        self.args = args
        assert(not isinstance(self.args, StageArgs))

    def __getattr__(self, key):
        real_key = '{}{}'.format(key, self.stage.postfix)
        if not hasattr(self.args, real_key):
            return None
        return getattr(self.args, real_key)


class Stage(object):
    def __init__(self, identifier, postfix=""):
        self.identifier = identifier
        self.postfix = postfix


STAGE_1 = Stage(1, "")
STAGE_2 = Stage(2, "_stage2")
