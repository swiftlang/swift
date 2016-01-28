#!/usr/bin/env python

# utils/profdata_merge_worker.py
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# This script is used to prevent profile data filling up available disk space
# by listening for profile data and merging them into a universal profdata
# file while tests are executing.

import os
import shutil
import pipes
import sys
import socket
import SocketServer
from multiprocessing import Lock, Process, Queue, JoinableQueue
import thread
import subprocess
import tempfile
import argparse

SERVER_ADDRESS = ('localhost', 12400)
TESTS_FINISHED_SENTINEL = "PROFDATA_MERGE_WORKER_TESTS_FINISHED_SENTINEL"
FILE_QUEUE = JoinableQueue()
FILES_MERGED = set()
CONFIG = None

class Config():
    """A class to store configuration information specified by command-line arguments.
    Used to encapsulate what would normally be global variables."""
    def __init__(self, debug, out_dir):
        self.debug = debug
        self.out_dir = out_dir
        self.tmp_dir = tempfile.mkdtemp()
        self.pid_file_path = os.path.join(self.out_dir, "profdata_merge_worker.pid")
        self.final_profdata_path = os.path.join(self.out_dir, "swift.profdata")

printlock = Lock()
def printsync(msg):
    if CONFIG and CONFIG.debug:
        with printlock:
            print >>sys.stderr, msg

class ProfdataTCPHandler(SocketServer.StreamRequestHandler):
    def report(self, msg):
        """Convenience method for reporting status from the workers."""
        printsync("\n===== ProfdataTCPHandler =====\n%s\n" % msg)

    def handle(self):
        """Receive a newline-separated list of filenames from a TCP connection
        and add them to the shared merge queue, where the workers will
        execute llvm-profdata merge commands."""
        data = self.rfile.read()
        self.report("received data (length %d): %s" % (len(data), data))

        # Stop once we receive the sentinel
        if data.startswith(TESTS_FINISHED_SENTINEL):
            self.report("received sentinel; killing server...")
            self.finish()
            self.connection.close()
            def kill_server(server):
                server.shutdown()
            # must be killed on another thread, or else deadlock
            thread.start_new_thread(kill_server, (self.server,))
        else:
            # Add all the files to the queue
            for f in data.splitlines():
                f = f.strip()
                if f in FILES_MERGED:
                    return
                FILES_MERGED.add(f)
                FILE_QUEUE.put(f)

class ProfdataMergerProcess(Process):
    def __init__(self):
        Process.__init__(self)
        self.filename_buffer = []
        self.profdata_path = os.path.join(CONFIG.tmp_dir, "%s.profdata" % self.name)
        self.profdata_tmp_path = self.profdata_path + ".copy"

    def report(self, msg):
        """Convenience method for reporting status from the workers."""
        printsync("\n===== %s =====\n%s\n" % (self.name, msg))

    def merge_file_buffer(self):
        """Merge all files in this worker's buffer and clear them.
        This method makes a copy of the working merge progress, then
        calls llvm-cov merge with up to 10 filenames, plus the current
        in-progress merge."""
        if not self.filename_buffer:
            self.report("no files to merge...")
            return
        if os.path.exists(self.profdata_path):
            os.rename(self.profdata_path, self.profdata_tmp_path)
            self.filename_buffer.append(self.profdata_tmp_path)
        cleaned_files = ' '.join(pipes.quote(f) for f in self.filename_buffer)
        # FIXME: This doesn't necessarily always line up with the version
        #        of clang++ used to build the binaries.
        llvm_cmd = ("xcrun llvm-profdata merge -o %s %s"
            % (self.profdata_path, cleaned_files))
        self.report(llvm_cmd)
        ret = subprocess.call(llvm_cmd, shell=True)
        if ret != 0:
            self.report("llvm profdata command failed -- Exited with code %d"
                % ret)
        for f in self.filename_buffer:
            if os.path.exists(f):
                os.remove(f)
        self.filename_buffer = []

    def run(self):
        """Blocks and waits for the file queue, so it can fill its buffer
        and execute merges. If it finds None in the queue, then it knows to
        stop waiting for the queue, merge its current buffer, and kill itself"""
        while True:
            filename = FILE_QUEUE.get()
            self.report("received filename: %s" % filename)
            if filename is None:
                self.report("received sentinel; merging...")
                self.merge_file_buffer()
                FILE_QUEUE.task_done()
                break
            self.filename_buffer.append(filename)
            self.report("Adding %s to filename_buffer." % filename)
            if len(self.filename_buffer) >= 10:
                self.merge_file_buffer()
                FILE_QUEUE.task_done()

def run_server():
    pid = os.getpid()
    if os.path.exists(CONFIG.pid_file_path):
        with open(CONFIG.pid_file_path) as pidfile:
            pid = pidfile.read()
            printsync(("existing process found with pid %s." +
                       "Ensure there are no other test runners running," +
                       "and delete the file at %s")
                       % (pid, CONFIG.pid_file_path))
        return

    with open(CONFIG.pid_file_path, "w") as pidfile:
        pidfile.write(str(pid))

    processes = [ProfdataMergerProcess() for _ in range(10)]
    for p in processes:
        p.start()

    server = SocketServer.TCPServer(SERVER_ADDRESS, ProfdataTCPHandler)
    server.serve_forever()

    for p in processes:
        # force each merge worker to gracefully exit
        FILE_QUEUE.put(None)

    for p in processes:
        printsync("waiting for %s to finish..." % p.name)
        p.join()

    # now that all workers have completed, merge all their files
    merge_final = ProfdataMergerProcess()
    merge_final.profdata_path = CONFIG.final_profdata_path
    for p in processes:
        if os.path.exists(p.profdata_path):
            printsync("merging " + p.profdata_path + "...")
            merge_final.filename_buffer.append(p.profdata_path)
    merge_final.merge_file_buffer()

def stop_server(args):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(SERVER_ADDRESS)
    sock.send(TESTS_FINISHED_SENTINEL)
    sock.close()

def start_server(args):
    global CONFIG
    CONFIG = Config(args.debug, args.output_dir)
    if not CONFIG.debug:
        pid = os.fork()
        if pid != 0:
            sys.exit(0) # kill the parent process we forked from.
    try:
        run_server()
    finally:
        if os.path.exists(CONFIG.pid_file_path):
            os.remove(CONFIG.pid_file_path)
        if os.path.exists(CONFIG.tmp_dir):
            shutil.rmtree(CONFIG.tmp_dir, ignore_errors=True)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    subparsers = parser.add_subparsers()

    start = subparsers.add_parser("start")
    start.add_argument("-d", "--debug",
        help="Run in foreground and report status.",
        action="store_true")
    start.add_argument("-o", "--output-dir",
        help="The directory to write the PID file and final profdata file.",
        default="/tmp")
    start.set_defaults(func=start_server)

    stop = subparsers.add_parser("stop")
    stop.set_defaults(func=stop_server)

    args = parser.parse_args()
    args.func(args)
