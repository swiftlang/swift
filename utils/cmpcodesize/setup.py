import os
import setuptools

import cmpcodesize

# setuptools expects to be invoked from within the directory of setup.py,
# but it is nice to allow `python path/to/setup.py install` to work
# (for scripts, etc.)
os.chdir(os.path.dirname(os.path.abspath(__file__)))

setuptools.setup(
    name="cmpcodesize",
    version=cmpcodesize.__version__,

    author=cmpcodesize.__author__,
    author_email=cmpcodesize.__email__,
    url='http://swift.org',
    license='Apache',

    description="A tool to compare the size of Swift compiler build products.",
    keywords='compare size swift',

    classifiers=[
        'Development Status :: 3 - Alpha',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: Apache Software License',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Topic :: Software Development :: Compilers',
    ],

    zip_safe=False,
    packages=setuptools.find_packages(),
    entry_points={
        'console_scripts': [
            'cmpcodesize = cmpcodesize:main',
        ],
    }
)
