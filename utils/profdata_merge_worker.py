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

PID_FILE_PATH = "/tmp/profdata_merge_worker.pid"

FINAL_PROFDATA_PATH = "/tmp/final.profdata"
SERVER_ADDRESS = ('localhost', 12400)

TESTS_FINISHED_SENTINEL = "PROFDATA_MERGE_WORKER_TESTS_FINISHED_SENTINEL"

FILE_QUEUE = JoinableQueue()
FILES_MERGED = set()

printlock = Lock()
def printsync(msg):
    pass
#    with printlock:
#        print >>sys.stderr, msg

class ProfdataTCPHandler(SocketServer.StreamRequestHandler):
    def handle(self):
        data = self.rfile.read()
        printsync(data)
        if data.startswith(TESTS_FINISHED_SENTINEL):
            printsync("received sentinel; killing server...")
            self.finish()
            self.connection.close()
            def kill_server(server):
                server.shutdown()
            # must be killed on another thread, or else deadlock
            thread.start_new_thread(kill_server, (self.server,))
        else:
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
        self.profdata_path = "/tmp/%s.profdata" % self.name
        self.profdata_tmp_path = self.profdata_path + ".tmp"

    def report(self, msg):
        printsync("\n===== %s =====\n%s\n" % (self.name, msg))

    def merge_file_buffer(self):
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

def main():
    fpid = os.fork()
    if fpid != 0:
        sys.exit(0)
    if os.path.exists(PID_FILE_PATH):
        pid = ""
        with open(PID_FILE_PATH) as pidfile:
            pid = pidfile.read()
        printsync('existing process found with pid %s' % pid)
        return

    pid = os.getpid()
    with open(PID_FILE_PATH, "w") as pidfile:
        pidfile.write(str(pid))

    def remove_pidfile():
        os.remove(PID_FILE_PATH)
    atexit.register(remove_pidfile)

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
    merge_final.profdata_path = "/tmp/swift.profdata"
    for p in processes:
        if os.path.exists(p.profdata_path):
            printsync("merging " + p.profdata_path + "...")
            merge_final.filename_buffer.append(p.profdata_path)
    merge_final.merge_file_buffer()

if __name__ == "__main__":
    exit(main())
