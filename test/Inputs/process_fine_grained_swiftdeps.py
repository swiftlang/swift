
import subprocess
import sys

# Fine-grained swiftdeps files use multiple lines for each graph node.
# Compress such a file so that each entry is one line of the form:
# <kind> <aspect> <context> <name> <isProvides>
# Also sort for consistency, since the node order can vary.

output = subprocess.run(
    [
        sys.argv[1],
        "--to-yaml",
        "--input-filename={}".format(sys.argv[2]),
        "--output-filename=-",
    ],
    stdout=subprocess.PIPE,
)
entries = []
k = a = c = n = s = p = ""
for line in output.stdout.decode("utf-8").split("\n"):
    if "kind:" in line:
        k = line.split()[1]
    if "aspect:" in line:
        a = line.split()[1]
    if "context:" in line:
        c = line.split()[1]
    if "name:" in line:
        n = ' '.join(line.split()[1:])
    if "sequenceNumber:" in line:
        s = line.split()[1]
    if "isProvides:" in line:
        p = line.split()[1]
        entries.append(' '.join([k, a, c, n, p]))
entries.sort()
print("\n".join(entries))
