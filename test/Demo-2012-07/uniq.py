def uniq(path):
    # Open the input file.
    with open(path) as f:
        # Read the file contents.
        contents = f.read()

        # Count the unique lines
        counts = {}
        for ln in contents.split('\n'):
            counts[ln] = counts.get(ln, 0) + 1

    # Order the items by count.
    items = counts.items()
    items.sort(key = lambda (key, value): -value)

    # Print the result
    for ln,count in items:
        print "%d: %s" % (count, ln)
    
if __name__ == '__main__':
    import sys
    uniq(sys.argv[1])
