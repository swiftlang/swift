
pid$target:*:swift_retain:entry
{
        @counts["swift_retain"] = count();
}

pid$target:*:swift_release:entry
{
        @counts["swift_release"] = count();
}

pid$target:*:objc_retain:entry
{
        @counts["objc_retain"] = count();
}

pid$target:*:objc_release:entry
{
        @counts["objc_release"] = count();
}
