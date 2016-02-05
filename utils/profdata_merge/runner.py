# utils/profdata_merge/runner.py
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# This file contains the main subroutines that invoke or stop the merge worker.

import shutil
import os
import socket
import sys
import logging

from multiprocessing import JoinableQueue
from process import ProfdataMergerProcess
from server import ProfdataServer
from main import SERVER_ADDRESS, TESTS_FINISHED_SENTINEL
from config import Config


def run_server(config):
    pid = os.getpid()
    if os.path.exists(config.pid_file_path):
        with open(config.pid_file_path) as pidfile:
            pid = pidfile.read()
            logging.error(("existing process found with pid %s." +
                           "Ensure there are no other test runners running," +
                           "and delete the file at %s") %
                          (pid, config.pid_file_path))
        return

    with open(config.pid_file_path, "w") as pidfile:
        pidfile.write(str(pid))

    file_queue = JoinableQueue()

    processes = [ProfdataMergerProcess(config, file_queue) for _ in range(10)]
    for p in processes:
        p.start()

    server = ProfdataServer(file_queue)
    server.serve_forever()

    for p in processes:
        # force each merge worker to gracefully exit
        file_queue.put(None)

    for p in processes:
        logging.info("waiting for %s to finish..." % p.name)
        p.join()

    # now that all workers have completed, merge all their files
    merge_final = ProfdataMergerProcess(config, file_queue)
    merge_final.profdata_path = config.final_profdata_path
    for p in processes:
        if os.path.exists(p.profdata_path):
            logging.info("merging " + p.profdata_path + "...")
            merge_final.filename_buffer.append(p.profdata_path)
    merge_final.merge_file_buffer()


def stop_server(args):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(SERVER_ADDRESS)
    sock.send(TESTS_FINISHED_SENTINEL)
    sock.close()


def start_server(args):
    config = Config(args.output_dir, args.no_remove)
    if not args.debug:
        pid = os.fork()
        if pid != 0:
            # kill the parent process we forked from.
            sys.exit(0)
    try:
        run_server(config)
    finally:
        if os.path.exists(config.pid_file_path):
            os.remove(config.pid_file_path)
        if os.path.exists(config.tmp_dir):
            shutil.rmtree(config.tmp_dir, ignore_errors=True)
