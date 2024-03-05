#!/usr/bin/env python3

import argparse
import datetime
import os
import re


def main():
    p = argparse.ArgumentParser()
    p.add_argument("name", help="The name of the new benchmark to be created")
    args = p.parse_args()

    # adds benchmark to `CMakeLists.txt`
    update_cmakelists(args.name)
    # create benchmark Swift file
    create_benchmark_file(args.name)
    # imports the benchmark module in `main.swift`
    add_import_benchmark(args.name)
    # registers the benchmark with the driver in `main.swift`
    add_register_benchmark(args.name)


def update_cmakelists(name):
    """Adds a new entry to the `CMakeLists.txt` file with the given
    benchmark name.
    """
    relative_path = create_relative_path("../CMakeLists.txt")

    file_contents = []
    with open(relative_path, "r") as f:
        file_contents = f.readlines()

    file_new_contents = insert_line_alphabetically(
        name,
        "    single-source/" + name + "\n",
        file_contents,
        r"    single-source\/([a-zA-Z]+)",
    )
    with open(relative_path, "w") as f:
        for line in file_new_contents:
            f.write(line)


def create_benchmark_file(name):
    """Creates a new Swift file with the given name based on the template
    and places it in the `single-source` directory.
    """

    file_text = ""
    template_path = create_relative_path("Template.swift")
    with open(template_path, "r") as f:
        file_text = "".join(f.readlines())

    # fill in missing template details
    file_text = file_text.format(
        name=name,
        padding="-" * (56 - len(name)),
        year=datetime.date.today().year
    )

    file_path_prefix = create_relative_path("../single-source/")
    file_path = os.path.join(file_path_prefix, name + ".swift")
    with open(file_path, "w") as f:
        f.write(file_text)


def add_import_benchmark(name):
    """Adds an `import` statement to the `main.swift` file for the new
    benchmark.
    """
    relative_path = create_relative_path("../utils/main.swift")

    # read current contents into an array
    file_contents = []
    with open(relative_path, "r") as f:
        file_contents = f.readlines()

    # the test dependencies are placed before all benchmarks, so we have to
    # insert the benchmark in the right alphabetical order after we have seen
    # all test dependencies.
    read_test_dependencies = False
    previous_benchmark_name = None
    file_new_contents = []
    for line in file_contents:
        # check if this line is a definition of a benchmark and get its name
        match = re.search(r"import ([a-zA-Z]+)", line)
        if match and match.group(1):
            benchmark_name = match.group(1)
            # find where to insert the new benchmark in the right alphabetical
            # order.
            if (
                name < benchmark_name
                and previous_benchmark_name is None
                or name < benchmark_name
                and name > previous_benchmark_name
            ):
                if read_test_dependencies:
                    file_new_contents.append("import " + name + "\n" + line)
                else:
                    # all test dependencies are first specified, so from now
                    # on we can look where to insert the new benchmark.
                    read_test_dependencies = True
                    file_new_contents.append(line)
            else:
                file_new_contents.append(line)
            previous_benchmark_name = benchmark_name
        else:
            file_new_contents.append(line)
    with open(relative_path, "w") as f:
        for line in file_new_contents:
            f.write(line)


def add_register_benchmark(name):
    """Adds an `import` statement to the `main.swift` file for the new
    benchmark.
    """
    relative_path = create_relative_path("../utils/main.swift")

    file_contents = []
    with open(relative_path, "r") as f:
        file_contents = f.readlines()

    file_new_contents = insert_line_alphabetically(
        name,
        "register(" + name + ".benchmarks)\n",
        file_contents,
        r"register\(([a-zA-Z]+)\.benchmarks\)",
    )
    with open(relative_path, "w") as f:
        for line in file_new_contents:
            f.write(line)


def insert_line_alphabetically(name, new_line, lines, regex):
    """Iterates through the given lines and executes the regex on each line to
    find where the new benchmark should be inserted with the given `new_line`.
    """
    # the name of the previous seen benchmark in order to insert the new
    # one at the correct position
    previous_benchmark_name = None
    # the new contents of the file
    updated_lines = []
    for line in lines:
        # apply regex and get name of benchmark on this line
        match = re.search(regex, line)
        if match and match.group(1):
            benchmark_name = match.group(1)
            # check if we're at the line where we have to insert the new
            # benchmark in the correct alphabetical order
            if (
                name < benchmark_name
                and previous_benchmark_name is None
                or name < benchmark_name
                and name > previous_benchmark_name
            ):
                updated_lines.append(new_line + line)
            else:
                updated_lines.append(line)
            previous_benchmark_name = benchmark_name
        else:
            updated_lines.append(line)
    return updated_lines


def create_relative_path(file_path):
    return os.path.join(os.path.dirname(__file__), file_path)


if __name__ == "__main__":
    main()
