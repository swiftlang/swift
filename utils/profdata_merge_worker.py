#!/usr/bin/env python

from contextlib import closing
import os
import sys
import atexit
import socket
import select
import time
import SocketServer
from multiprocessing import Lock, Process, Queue, JoinableQueue
import thread
import subprocess

PID_FILE_PATH = os.path.join("/tmp", "profdata_merge_worker.pid")

FINAL_PROFDATA_PATH = os.path.join("/tmp", "swift.profdata")
SERVER_ADDRESS = ('localhost', 12400)

TESTS_FINISHED_SENTINEL = "PROFDATA_MERGE_WORKER_TESTS_FINISHED_SENTINEL"

FILE_QUEUE = JoinableQueue()
FILES_MERGED = set()

DEBUG = len(sys.argv) > 1 and sys.argv[1] == "--debug"

printlock = Lock()
def printsync(msg):
    if DEBUG:
        with printlock:
            print >>sys.stderr, msg

class ProfdataTCPHandler(SocketServer.StreamRequestHandler):
    def handle(self):
        """Receive a newline-separated list of filenames from a TCP connection
        and add them to the shared merge queue, where the workers will
        execute llvm-profdata merge commands."""
        data = self.rfile.read()
        printsync(data)

        # Stop once we receive the sentinel
        if data.startswith(TESTS_FINISHED_SENTINEL):
            printsync("received sentinel; killing server...")
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
        self.profdata_path = os.path.join("/tmp", "%s.profdata" % self.name)
        self.profdata_tmp_path = self.profdata_path + ".tmp"

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
        llvm_cmd = ("xcrun llvm-profdata merge %s -o %s" %
                    (" ".join(self.filename_buffer), self.profdata_path))
        self.report(llvm_cmd)
        subprocess.call(llvm_cmd, shell=True)
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

@atexit.register
def remove_pidfile():
    """Always remove the pid file when we exit."""
    if os.path.exists(PID_FILE_PATH):
        os.remove(PID_FILE_PATH)

def main():
    pid = os.getpid()
    if not DEBUG:
        pid = os.fork()
        if pid != 0:
            sys.exit(0) # kill the parent process we forked from.

    if os.path.exists(PID_FILE_PATH):
        with open(PID_FILE_PATH) as pidfile:
            pid = pidfile.read()
            printsync('existing process found with pid %s' % pid)
        return

    with open(PID_FILE_PATH, "w") as pidfile:
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
    merge_final.profdata_path = FINAL_PROFDATA_PATH
    for p in processes:
        if os.path.exists(p.profdata_path):
            printsync("merging " + p.profdata_path + "...")
            merge_final.filename_buffer.append(p.profdata_path)
    merge_final.merge_file_buffer()

if __name__ == "__main__":
    exit(main())
