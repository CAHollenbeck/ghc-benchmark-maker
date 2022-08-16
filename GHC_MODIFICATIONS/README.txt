For the paper Investigating Magic Numbers: Improving the Inlining Heuristic in the Glasgow Haskell Compiler, these were the files modified in GHC 8.10.3, commit 6db6db46af6f8e3e24d7d16b0b43a984a9a14677

To retrace our steps, build GHC, uncomment the "stage=2" line in build.mk, then copy theese modified files into their appropriate folders:
    compiler/coreSyn/CoreUnfold.hs
    compiler/main/DynFlags.hs
Then make again.

More precise steps:

In a new folder for the desired location of the GHC:

    $ git clone --recurse-submodules git@gitlab.haskell.org:ghc/ghc.git
    $ cd ghc
    $ git submodule update init
    $ git checkout 6db6db46af6f8e3e24d7d16b0b43a984a9a14677
    $ ./boot
    $ ./configure
    $ make

Then uncomment "stage=2" in mk/build.mk.sample.

    $ mv mk/build.mk.sample mk/build.mk
    $ cp <path_to_file>/CoreUnfold.hs compiler/coreSyn/CoreUnfold.hs
    $ cp <path_to_file>/DynFlags.hs compiler/main/DynFlags.hs
    $ cd compiler
    $ make

The compiler will be at inplace/bin/ghc-stage2
