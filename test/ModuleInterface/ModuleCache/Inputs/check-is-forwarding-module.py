import os
import platform
import sys

def extended(path):
    if platform.system() == 'Windows':
        path = os.path.abspath(path)
        if path.startswith('\\\\'):
            return "\\\\?\\UNC\\{0}".format(path[2:])
        return "\\\\?\\{0}".format(path)
    return path

for input_path in sys.argv[1:]:
    with open(extended(input_path), 'rb') as yaml_file:
        # Forwarding files are YAML files that start with '---'
        if yaml_file.read(3) != b'---':
            print("swiftmodule '%s' is not a forwarding module!" % input_path)
            sys.exit(1)
