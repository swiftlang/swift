/*===--- swift_stats.d ----------------------------------------------------===//
 *
 * This source file is part of the Swift.org open source project
 *
 * Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
 * Licensed under Apache License v2.0 with Runtime Library Exception
 *
 * See https://swift.org/LICENSE.txt for license information
 * See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
 *
 *===----------------------------------------------------------------------===*/

pid$target:*:swift_retain:entry
{
        @counts["rr-opts"] = count();
}

pid$target:*:swift_release:entry
{
        @counts["rr-opts"] = count();
}

pid$target:*:swift_retain_n:entry
{
        @counts["rr-opts"] = count();
}

pid$target:*:swift_release_n:entry
{
        @counts["rr-opts"] = count();
}

END
{
        printf("\nDTRACE RESULTS\n");
        printa("%s,%@u\n", @counts)
}