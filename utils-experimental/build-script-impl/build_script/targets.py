 
def split(target):
    res = target.split('-', 1)
    if len(res) != 2:
        res = (res[0], None)
    return res

def is_darwin_type(target):
    sys, _ = split(target)
    return sys in [
        'macosx',
        'iphoneos', 'iphonesimulator',
        'appletvos', 'appletvsimulator',
        'watchos', 'watchsimulator']

def is_osx(target):
    sys, _ = split(target)
    return sys == 'macosx'
