import sys

with open(sys.argv[1], 'r') as yaml_file:
    # Forwarding files are YAML files that start with '---'
    if yaml_file.read(3) != '---':
        print("swiftmodule '%s' is not a forwarding module!")
        sys.exit(1)
