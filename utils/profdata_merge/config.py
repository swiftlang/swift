# utils/profdata_merge/config.py
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# This file contains the data structure that transforms arguments into usable
# values

import tempfile
import os


class Config():
    """A class to store configuration information specified by command-line
    arguments."""
    def __init__(self, out_dir, no_remove_files):
        self.out_dir = out_dir
        self.tmp_dir = tempfile.mkdtemp()
        self.pid_file_path = os.path.join(self.out_dir,
                                          "profdata_merge_worker.pid")
        self.final_profdata_path = os.path.join(self.out_dir, "swift.profdata")
        self.remove_files = not no_remove_files
