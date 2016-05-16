# utils/profdata_merge/process.py
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# This file contains the worker processes that watch a file queue for files and
# merge profile data in parallel.

import logging
import os
import pipes
import sys

from multiprocessing import Process


# hack to import SwiftBuildSupport and swift_build_support
parent_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..')
sys.path.append(parent_dir)
support_dir = os.path.join(parent_dir, 'swift_build_support')
sys.path.append(support_dir)
from swift_build_support.toolchain import host_toolchain  # noqa (E402)
from SwiftBuildSupport import check_output, check_call  # noqa (E402)

toolchain = host_toolchain()
LLVM_PROFDATA_PATH = toolchain.llvm_profdata
_profdata_help = check_output([LLVM_PROFDATA_PATH, 'merge', '-help'])
LLVM_PROFDATA_SUPPORTS_SPARSE = 'sparse' in _profdata_help


class ProfdataMergerProcess(Process):
    def __init__(self, config, file_queue):
        super(ProfdataMergerProcess, self).__init__()
        self.config = config
        self.file_queue = file_queue
        self.filename_buffer = []
        self.profdata_path = os.path.join(config.tmp_dir,
                                          "%s.profdata" % self.name)
        self.profdata_tmp_path = self.profdata_path + ".copy"

    def report(self, msg, level=logging.INFO):
        """Convenience method for reporting status from the workers."""
        logging.log(level, "[%s]: %s" % (self.name, msg))

    def merge_file_buffer(self):
        """Merge all files in this worker's buffer and clear them.
        This method makes a copy of the working merge progress, then
        calls llvm-cov merge with up to 10 filenames, plus the current
        in-progress merge.
        """
        if not self.filename_buffer:
            self.report("no files to merge...")
            return
        if os.path.exists(self.profdata_path):
            os.rename(self.profdata_path, self.profdata_tmp_path)
            self.filename_buffer.append(self.profdata_tmp_path)
        cleaned_files = [pipes.quote(f) for f in self.filename_buffer]
        llvm_cmd = [LLVM_PROFDATA_PATH, "merge", "-o", self.profdata_path]
        if LLVM_PROFDATA_SUPPORTS_SPARSE:
            llvm_cmd.append("-sparse")
        llvm_cmd += cleaned_files
        self.report(llvm_cmd)
        ret = check_call(llvm_cmd)
        if ret != 0:
            self.report("llvm profdata command failed -- Exited with code %d"
                        % ret, level=logging.ERROR)
        if self.config.remove_files:
            for f in self.filename_buffer:
                if os.path.exists(f):
                    os.remove(f)
        self.filename_buffer = []

    def run(self):
        """Blocks and waits for the file queue so it can fill its buffer and
        execute merges. If it finds None in the queue, then it knows to stop
        waiting for the queue, merge its current buffer, and kill itself
        """
        while True:
            filename = self.file_queue.get()
            self.report("received filename: %s" % filename)
            if filename is None:
                self.report("received sentinel; merging...")
                self.merge_file_buffer()
                self.file_queue.task_done()
                break
            self.filename_buffer.append(filename)
            self.report("Adding %s to filename_buffer." % filename)
            if len(self.filename_buffer) >= 10:
                self.merge_file_buffer()
                self.file_queue.task_done()
