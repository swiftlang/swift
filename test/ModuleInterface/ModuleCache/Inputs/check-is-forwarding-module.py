import sys

for input_path in sys.argv[1:]:
    with open(input_path, 'rb') as yaml_file:
        # Forwarding files are YAML files that start with '---'
        if yaml_file.read(3) != b'---':
            print("swiftmodule '%s' is not a forwarding module!" % input_path)
            sys.exit(1)
