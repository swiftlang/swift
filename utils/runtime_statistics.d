
swift*:::retain
{
        @counts["num retain calls"] = count();
}

swift*:::release
{
        @counts["num release calls"] = count();
}

swift*:::allocateObject
{
        @counts["num allocated objects"] = count();
}

swift*:::deallocateObject
{
        @counts["num deallocated objects"] = count();
}

swift*:::isUniquelyReferenced
{
        @counts["num calls to isUniquelyReferenced"] = count();
}

swift*:::isUniquelyReferencedOrPinned
{
        @counts["num calls to isUniquelyReferencedOrPinned"] = count();
}
