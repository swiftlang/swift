import tempfile
import os

class Config():
    """A class to store configuration information specified by command-line arguments.
    Used to encapsulate what would normally be global variables."""
    def __init__(self, debug, out_dir, no_remove_files):
        self.debug = debug
        self.out_dir = out_dir
        self.tmp_dir = tempfile.mkdtemp()
        self.pid_file_path = os.path.join(self.out_dir, "profdata_merge_worker.pid")
        self.final_profdata_path = os.path.join(self.out_dir, "swift.profdata")
        self.remove_files = not no_remove_files

